(defpackage :cl-transactional-tests
  (:use :cl :sb-thread :iterate :lift :cl-transactional :funds)
  (:import-from :arnesi :curry :rcurry)
  (:nicknames :tests))

(in-package :cl-transactional-tests)

(deftestsuite cl-transactional-tests ()
  ())
