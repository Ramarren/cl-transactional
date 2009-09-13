(defpackage :cl-transactional-tests
  (:use :cl :iterate :cl-transactional :alexandria :stefil)
  (:nicknames :tests))

(in-package :cl-transactional-tests)

(in-suite root-suite)

(defsuite cl-transactional-tests)
