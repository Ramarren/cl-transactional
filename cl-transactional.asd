(asdf:defsystem :cl-transactional
  :components ((:file "package")
	       (:file "tvars" :depends-on ("package"))
	       (:file "transactions" :depends-on ("package" "tvars")))
  :depends-on (:arnesi :defclass-star :iterate))