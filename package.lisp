(defpackage :cl-transactional
  (:nicknames :stm)
  (:export #:make-tvar #:get-tvar #:put-tvar
	   #:with-retry-transaction #:with-orelse-transaction
	   #:retry-transaction
           #:tvar-ref)
  (:use :cl :iterate :alexandria))

(in-package :cl-transactional)