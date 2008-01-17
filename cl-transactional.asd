(defpackage :cl-transactional-system (:use :cl :asdf))

(in-package :cl-transactional-system)

(defsystem :cl-transactional
  :components ((:file "package")
	       (:file "tvars" :depends-on ("package"))
	       (:file "transactions" :depends-on ("package" "tvars")))
  :depends-on (:arnesi :defclass-star :iterate))