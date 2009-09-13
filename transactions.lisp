(in-package :cl-transactional)

(defvar *top-level-transaction* t)

(defun call-with-mutexes-down (locks thunk)
  (if (null locks)
      (cons (funcall thunk) t)
      (sb-thread:with-mutex ((car locks) :wait-p nil)
	(call-with-mutexes-down (cdr locks) thunk))))

(defun call-with-mutexes-spinning (locks thunk)
  (iter (for down-lock next (call-with-mutexes-down locks thunk))
	(until (consp down-lock))
	(sb-thread:thread-yield)
	(finally (return (car down-lock)))))

(defmacro with-mutexes-spinning (locks &body body)
  `(call-with-mutexes-spinning ,locks
			       #'(lambda ()
				   ,@body)))

(defgeneric get-sorted-locks-from-log (transaction-log)
  (:method ((transaction-log hash-table))
    (mapcar #'cdr
            (sort (iter (for (tvar nil) in-hashtable transaction-log)
                        (collecting (cons (uid-of tvar) (lock-of tvar))))
                  #'<
                  :key #'car)))
  (:documentation "Get a list of locks in predicatable order for locking."))

(defgeneric valid-transaction-core-p (transaction-log)
  (:method ((transaction-log hash-table))
    (iter (for (tvar tvar-log) in-hashtable transaction-log)
          (always (eql (read-value-of tvar-log)
                       (value-of tvar)))))
  (:documentation "Check if transaction is valid without locking."))

(defgeneric execute-transaction (transaction-log)
  (:method ((transaction-log hash-table))
    (let ((locks (get-sorted-locks-from-log transaction-log)))
      (with-mutexes-spinning locks
	(when (valid-transaction-core-p transaction-log)
          (iter (for (tvar tvar-log) in-hashtable transaction-log)
                (unless (eql (write-value-of tvar-log)
                             (value-of tvar))
                  (setf (value-of tvar) (write-value-of tvar-log))
                  (notify-waitees tvar))
                (finally (return t))))))))

(defgeneric valid-transaction-p (transaction-log)
  (:method ((transaction-log hash-table))
    (let ((locks (get-sorted-locks-from-log transaction-log)))
      (with-mutexes-spinning locks
	(valid-transaction-core-p transaction-log))))
  (:documentation "Check if the transaction hasn't been invalidated."))

(define-condition transaction-fail ()
  ((read-vars :initform nil :initarg :read-vars :reader read-vars-of)))

(defun retry-transaction ()
  (signal 'transaction-fail :read-vars *read-vars*))

(defgeneric suspend-thread-on-tvars (fail-tvars)
  (:method ((fail-tvars list))
    (unless (null fail-tvars)
     (let ((wait-mutex (sb-thread:make-mutex))
	   (wait-queue (sb-thread:make-waitqueue)))
       (sb-thread:with-mutex (wait-mutex)
	 (iter (for (tvar . tvar-read-value) in fail-tvars)
	       (sb-thread:with-mutex ((lock-of tvar))
		 (unless (eql tvar-read-value (value-of tvar))
                   ;; if the value already changed just retry immediately
		   (return-from suspend-thread-on-tvars (values nil nil)))
		 (push (list wait-mutex wait-queue (mapcar #'car fail-tvars))
		       (waiting-of tvar))))
	 (sb-thread:condition-wait wait-queue wait-mutex))))
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

(defgeneric merge-log-into (merged merged-into)
  (:method ((merged hash-table) (merged-into hash-table))
    (iter (for (tvar tlog) in-hashtable merged)
	  (setf (gethash tvar merged-into)
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
          ;; mostly optimization, if valid-transaction-p is made wrong immediately it will be
          ;; caught upwards
	  (if (valid-transaction-p *transaction-log*)
	      (multiple-value-prog1
		  (values transaction-result t)
		(merge-log-into *transaction-log* (car *wrapping-transaction-logs*)))
	      (signal 'transaction-fail))))))

(defmacro with-retry-transaction (&body body)
  (with-unique-names (thunk t-result t-success fail-condition fail-vars)
    `(let ((,thunk #'(lambda ()
		      ,@body)))
       (iter (for (values ,t-result ,t-success) next
		  (handler-case (call-with-transaction ,thunk)
		    (transaction-fail (,fail-condition)
		      (if-let (,fail-vars (read-vars-of ,fail-condition))
                        (suspend-thread-on-tvars ,fail-vars)
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
                  (when-let (fail-vars (read-vars-of fail-condition))
                    (setf cycle-vars fail-vars))
                  (values nil nil)))
            (when t-success
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
