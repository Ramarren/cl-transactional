(in-package :cl-transactional)

(defvar *top-level-transaction* t)

(defun call-with-mutexes-down (locks thunk)
  (if (null locks)
      (cons (funcall thunk) t)
      (with-mutex ((car locks) :wait-p nil)
	(call-with-mutexes-down (cdr locks) thunk))))

(defun call-with-mutexes-spinning (locks thunk)
  (iter (for down-lock next (call-with-mutexes-down locks thunk))
	(until (consp down-lock))
	(thread-yield)
	(finally (return (car down-lock)))))

(defmacro with-mutexes-spinning (locks &body body)
  `(call-with-mutexes-spinning ,locks
			       #'(lambda ()
				   ,@body)))

(defgeneric execute-transaction (transaction-log)
  (:method ((transaction-log hash-table))
    (let ((locks (iter (for (tvar nil) in-hashtable transaction-log)
		       (collecting (lock-of tvar)))))
      (with-mutexes-spinning locks
	(if (iter (for (tvar tvar-log) in-hashtable transaction-log)
		  (always (eql (read-value-of tvar-log)
			       (value-of tvar))))
	    (iter (for (tvar tvar-log) in-hashtable transaction-log)
		  (unless (eql (write-value-of tvar-log)
			       (value-of tvar))
		    (setf (value-of tvar) (write-value-of tvar-log))
		    (notify-waitees tvar))
		  (finally (return t))))))))

(defgeneric valid-transaction-p (transaction-log)
  (:method ((transaction-log hash-table))
    (let ((locks (mapcar #'lock-of (hash-table-keys transaction-log))))
      (with-mutexes-spinning locks
	(iter (for (tvar tvar-log) in-hashtable transaction-log)
	      (always (eql (read-value-of tvar-log)
			   (value-of tvar))))))))

(defcondition* transaction-fail ()
  ((read-vars nil)))

(defun retry-transaction ()
  (signal 'transaction-fail :read-vars *read-vars*))

(defgeneric suspend-thread-on-tvars (fail-tvars)
  (:method ((fail-tvars list))
    (unless (null fail-tvars)
     (let ((wait-mutex (make-mutex))
	   (wait-queue (make-waitqueue)))
       (with-mutex (wait-mutex)
	 (iter (for tvar in fail-tvars)
	       (with-mutex ((lock-of tvar))
		 (push (list wait-mutex wait-queue fail-tvars)
		       (waiting-of tvar))))
	 (condition-wait wait-queue wait-mutex))))
    (values nil nil)))

(defgeneric copy-log (old-log)
  (:method ((old-log hash-table))
    (let ((new-log (make-hash-table)))
      (iter (for (tvar tlog) in-hashtable old-log)
	    (setf (gethash tvar new-log)
		  (make-instance 'tvar-log
				 :tvar tvar
				 :read-value (read-value-of tlog)
				 :write-value (write-value-of tlog)))
	    (finally (return new-log))))))

(defgeneric merge-log-into (new-log old-log)
  (:method ((new-log hash-table) (old-log hash-table))
    (iter (for (tvar tlog) in-hashtable new-log)
	  (setf (gethash tvar old-log)
		tlog))))

(defun call-with-transaction (thunk)
  (if *top-level-transaction*
      (let ((*transaction-log* (make-hash-table))
	    (*wrapping-transaction-logs* nil)
	    (*read-vars* nil)
	    (*top-level-transaction* nil))
	(let ((transaction-result (funcall thunk)))
	  (if (execute-transaction *transaction-log*)
	      (values transaction-result t)
	      (signal 'transaction-fail))))
      (let ((*transaction-log* (copy-log *transaction-log*))
	    (*wrapping-transaction-logs* (cons *transaction-log* *wrapping-transaction-logs*)))
	(let ((transaction-result (funcall thunk)))
	  (if (valid-transaction-p *transaction-log*)
	      (multiple-value-prog1
		  (values transaction-result t)
		(merge-log-into *transaction-log* (car *wrapping-transaction-logs*)))
	      (signal 'transaction-fail))))))

(defmacro with-retry-transaction (&body body)
  (with-unique-names (thunk t-result t-success)
    `(let ((,thunk #'(lambda ()
		      ,@body)))
       (iter (for (values ,t-result ,t-success) next
		  (handler-case (call-with-transaction ,thunk)
		    (transaction-fail (fail-condition)
		      (aif (read-vars-of fail-condition)
			   (suspend-thread-on-tvars it)
			   (if *top-level-transaction*
			       (values nil nil)
			       (signal 'transaction-fail))))))
	     (until ,t-success)
	     (finally (return ,t-result))))))

(defun orelse-cycle (thunks)
  (let ((cycle-vars nil))
   (iter (for thunk in thunks)
	 (multiple-value-bind (t-result t-success)
	     (handler-case (call-with-transaction thunk)
	       (transaction-fail (fail-condition)
		 (aif (read-vars-of fail-condition)
		      (setf cycle-vars it))
		 (values nil nil)))
	   (awhen t-success
	     (leave (values t-result t))))
	 (finally (if cycle-vars
		      (return (suspend-thread-on-tvars cycle-vars))
		      (if *top-level-transaction*
			  (return (values nil nil))
			  (signal 'transaction-fail)))))))

(defmacro with-orelse-transaction (&body body)
  (with-unique-names (thunks t-result t-success)
    `(let ((,thunks (list ,@(iter (for th in body)
				  (collecting `#'(lambda ()
						   ,@th))))))
       (iter (for (values ,t-result ,t-success) next (orelse-cycle ,thunks))
	     (until ,t-success)
	     (finally (return ,t-result))))))
