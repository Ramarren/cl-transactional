(defpackage :cl-transactional
  (:nicknames :stm)
  (:use :cl :sb-thread :defclass-star :iterate)
  (:import-from :arnesi :curry :aif :it :with-unique-names))

(in-package :cl-transactional)