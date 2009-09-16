(asdf:defsystem :cl-transactional-tests
  :components ((:module "tests"
			:components ((:file "package")
				     (:file "trivial" :depends-on ("package"))
                                     (:file "tchan" :depends-on ("package")))))
  :depends-on (:cl-transactional :stefil :fset :iterate :alexandria))
