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
		    (setf (value-of tvar) (write-value-of tvar-log)))
		  (finally (return t))))))))

(defcondition* transaction-fail ()
  ((transaction-log nil)))

(defun retry-transaction ()
  (signal 'transaction-fail :transaction-log *transaction-log*))

(defun call-with-transaction (thunk)
  (let ((*transaction-log* (if *top-level-transaction*
			       (make-hash-table)
			       *transaction-log*))
	(*top-level-transaction* nil))
    (let ((transaction-result (funcall thunk)))
      (if (execute-transaction *transaction-log*)
	  (values transaction-result t)
	  (values transaction-result nil)))))

(defmacro with-retry-transaction (&body body)
  (with-unique-names (thunk t-result t-success)
    `(let ((thunk #'(lambda ()
		      ,@body)))
       (if (null *top-level-transaction*)
	   (funcall ,thunk)
	   (iter (for (values ,t-result ,t-success) next (call-with-transaction ,thunk))
		 (until ,t-success)
		 (finally (return ,t-result)))))))
