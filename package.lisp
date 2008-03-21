(defpackage :cl-transactional
  (:nicknames :stm)
  (:export #:make-tvar #:get-tvar #:put-tvar
	   #:with-retry-transaction #:with-orelse-transaction
	   #:retry-transaction)
  (:use :cl :sb-thread :defclass-star :iterate)
  (:import-from :arnesi :curry :aif :it :with-unique-names :hash-table-keys :awhen))

(in-package :cl-transactional)