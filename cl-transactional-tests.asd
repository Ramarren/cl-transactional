(asdf:defsystem :cl-transactional-tests
  :components ((:module "tests"
			:components ((:file "package")
				     (:file "trivial"))))
  :depends-on (:cl-transactional :stefil :fset :iterate :alexandria))
