(in-package :cl-transactional)

(defvar *tvar-uid* 0)

(defclass tvar ()
  ((value   :accessor value-of   :initarg :value :initform nil)
   (lock    :accessor lock-of    :initarg :lock :initform (sb-thread:make-mutex :name "TVar lock"))
   (waiting :accessor waiting-of :initarg :waiting :initform nil)
   (uid     :accessor uid-of     :initarg :uid :initform (1- (incf *tvar-uid*)))))

(defclass tvar-log ()
  ((tvar        :accessor tvar-of        :initarg :tvar)
   (read-value  :accessor read-value-of  :initarg :read-value)
   (write-value :accessor write-value-of :initarg :write-value)))

(defmethod print-object ((object tvar) stream)
  (sb-thread:with-recursive-lock ((lock-of object))
    (print-unreadable-object (object stream :type t)
      (prin1 (value-of object) stream))))

(defun make-tvar (&optional (value nil value-p))
  (if value-p
      (make-instance 'tvar :value value)
      (make-instance 'tvar)))

;;; transaction-log is as hashtable mapping tvar to tvar-log objects, it is created when transaction is created
(defvar *transaction-log* nil)
(defvar *wrapping-transaction-logs* nil)
(defvar *read-vars* nil)

(defgeneric get-tvar-log (tvar transaction-log wrapping-logs)
  (:method ((tvar tvar) (transaction-log hash-table) (wrapping-logs list))
    (if-let (log (gethash tvar transaction-log))
      log
      (if-let (log (iter (for l in wrapping-logs)
                         (thereis (gethash tvar l))))
        (setf (gethash tvar transaction-log)
              (make-instance 'tvar-log
                             :tvar tvar
                             :read-value (read-value-of log)
                             :write-value (write-value-of log)))
        (sb-thread:with-recursive-lock ((lock-of tvar))
          (pushnew (cons tvar (value-of tvar)) *read-vars* :key #'car)
          (setf (gethash tvar transaction-log)
                (make-instance 'tvar-log
                               :tvar tvar
                               :read-value (value-of tvar)
                               :write-value (value-of tvar)))))))
  (:documentation "Find or create a log for tvar in a current transaction."))


(defgeneric get-tvar (tvar &optional transaction-log wrapping-logs)
  (:method ((tvar tvar) &optional (transaction-log *transaction-log*) (wrapping-logs *wrapping-transaction-logs*))
    (write-value-of (get-tvar-log tvar transaction-log wrapping-logs)))
  (:documentation "Get value of tvar in transaction."))

(defgeneric put-tvar (tvar new-value &optional transaction-log wrapping-logs)
  (:method ((tvar tvar) new-value &optional (transaction-log *transaction-log*) (wrapping-logs *wrapping-transaction-logs*))
    (setf (write-value-of (get-tvar-log tvar transaction-log wrapping-logs)) new-value))
  (:documentation "Set value of tvar in transaction"))

(defun tvar-ref (tvar)
  (get-tvar tvar))

(defun (setf tvar-ref) (new-value tvar)
  (put-tvar tvar new-value))

(defgeneric notify-waitees (tvar)
  (:method ((tvar tvar))
    (when (waiting-of tvar)
      (iter (for (wait-mutex wait-queue other-tvars) in (waiting-of tvar))
	    (iter (for other-tvar in other-tvars)
		  (sb-thread:with-recursive-lock ((lock-of other-tvar))
		    (setf (waiting-of other-tvar)
			  (remove wait-mutex (waiting-of other-tvar) :count 1 :key #'car))))
	    (sb-thread:with-mutex (wait-mutex)
	      (sb-thread:condition-notify wait-queue)))))
  (:documentation "Notify all threads waiting on tvar. Lock on tvar itself is alreade held."))
