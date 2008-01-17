(defpackage :cl-transactional
  (:nicknames :stm)
  (:use :cl :sb-thread :defclass-star :iterate)
  (:import-from :arnesi :curry :aif :it))

(in-package :cl-transactional)