(in-package :cl-transactional)

(defclass* tvar ()
  ((value nil)
   (lock (make-mutex))
   (waiting nil)))

(defclass* tvar-log ()
  ((tvar)
   (read-value)
   (write-value)))

;;; transaction-log is as hashtable mapping tvar to tvar-log objects, it is created when transaction is created
(defvar *transaction-log* nil)

(defgeneric get-tvar-log (tvar transaction-log)
  (:method ((tvar tvar) (transaction-log hash-table))
    (aif (gethash tvar transaction-log)
	 it
	 (with-mutex ((lock-of tvar))
	   (setf (gethash tvar transaction-log)
		 (make-instance 'tvar-log
				:tvar tvar
				:read-value (value-of tvar)
				:write-value (value-of tvar)))))))

(defgeneric get-tvar (tvar &optional transaction-log)
  (:method ((tvar tvar) &optional (transaction-log *transaction-log*))
    (assert *transaction-log*)
    (write-value-of (get-tvar-log tvar transaction-log))))

(defgeneric put-tvar (tvar new-value &optional transaction-log)
  (:method ((tvar tvar) new-value &optional (transaction-log *transaction-log*))
    (setf (write-value-of (get-tvar-log tvar transaction-log)) new-value)))