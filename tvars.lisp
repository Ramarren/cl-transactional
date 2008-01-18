(in-package :cl-transactional)

(defclass* tvar ()
  ((value nil)
   (lock (make-mutex))
   (waiting nil)))

(defclass* tvar-log ()
  ((tvar)
   (read-value)
   (write-value)))

(defmethod print-object ((object tvar) stream)
  (with-recursive-lock ((lock-of object))
   (print-unreadable-object (object stream :type t)
     (prin1 (value-of object) stream))))

(defun make-tvar (value)
  (make-instance 'tvar :value value))

;;; transaction-log is as hashtable mapping tvar to tvar-log objects, it is created when transaction is created
(defvar *transaction-log* nil)
(defvar *wrapping-transaction-logs* nil)
(defvar *read-vars* nil)

(defgeneric get-tvar-log (tvar transaction-log wrapping-logs)
  (:method ((tvar tvar) (transaction-log hash-table) (wrapping-logs list))
    (aif (gethash tvar transaction-log)
	 it
	 (aif (iter (for l in wrapping-logs)
		    (thereis (gethash tvar l)))
	      (setf (gethash tvar transaction-log)
		    (make-instance 'tvar-log
				   :tvar tvar
				   :read-value (read-value-of it)
				   :write-value (write-value-of it)))
	      (with-mutex ((lock-of tvar))
		(pushnew tvar *read-vars*)
		(setf (gethash tvar transaction-log)
		      (make-instance 'tvar-log
				     :tvar tvar
				     :read-value (value-of tvar)
				     :write-value (value-of tvar))))))))

(defgeneric get-tvar (tvar &optional transaction-log wrapping-logs)
  (:method ((tvar tvar) &optional (transaction-log *transaction-log*) (wrapping-logs *wrapping-transaction-logs*))
    (write-value-of (get-tvar-log tvar transaction-log wrapping-logs))))

(defgeneric put-tvar (tvar new-value &optional transaction-log wrapping-logs)
  (:method ((tvar tvar) new-value &optional (transaction-log *transaction-log*) (wrapping-logs *wrapping-transaction-logs*))
    (setf (write-value-of (get-tvar-log tvar transaction-log wrapping-logs)) new-value)))

(defgeneric notify-waitees (tvar)
  (:method ((tvar tvar))
    (when (waiting-of tvar)
      (iter (for (wait-mutex wait-queue other-tvars) in (waiting-of tvar))
	    (iter (for other-tvar in other-tvars)
		  (with-recursive-lock ((lock-of other-tvar))
		    (setf (waiting-of other-tvar)
			  (remove wait-mutex (waiting-of other-tvar) :count 1 :key #'car))))
	    (with-mutex (wait-mutex)
	      (condition-notify wait-queue))))))