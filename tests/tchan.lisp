(in-package :cl-transactional-tests)

(defclass tchan ()
  ((tvar :initarg :tvar :initform (make-tvar (fset:empty-seq)) :accessor tvar-of)
   (length :initarg :length :initform (make-tvar 5) :accessor length-of)
   (count :initform (make-tvar 0) :accessor count-of))
  (:documentation "Transactional channel"))

(defgeneric channel-empty-p (channel)
  (:method ((channel tchan))
    (with-retry-transaction
      (zerop (tvar-ref (count-of channel))))))

(defgeneric channel-full-p (channel)
  (:method ((channel tchan))
    (with-retry-transaction
      (= (tvar-ref (length-of channel))
         (tvar-ref (count-of channel))))))

(defgeneric send-blocks-p (channel)
  (:method ((channel tchan)) (channel-full-p channel)))

(defgeneric recv-blocks-p (channel)
  (:method ((channel tchan)) (channel-empty-p channel)))

(defgeneric send (channel obj)
  (:method ((channel tchan) obj)
    (with-retry-transaction
      (if (>= (tvar-ref (count-of channel))
              (tvar-ref (length-of channel)))
          (retry-transaction)
          (setf (tvar-ref (tvar-of channel))
                (fset:with-last (tvar-ref (tvar-of channel)) obj)
                (tvar-ref (count-of channel))
                (1+ (tvar-ref (count-of channel))))))))

(defgeneric recv (channel)
  (:method ((channel tchan))
    (with-retry-transaction
      (if (zerop (tvar-ref (count-of channel)))
          (retry-transaction)
          (prog1
              (fset:first (tvar-ref (tvar-of channel)))
            (setf (tvar-ref (tvar-of channel))
                  (fset:less-first (tvar-ref (tvar-of channel)))
                  (tvar-ref (count-of channel))
                  (1- (tvar-ref (count-of channel)))))))))

(defun chan (&optional (buffer-size 1))
  (make-instance 'tchan :length (make-tvar buffer-size)))

;; The infamous parallel prime sieve from newsqueak
;; modified from Chanl

(defun counter (channel)
  (loop for i from 2 do (send channel i)))

(defun filter (prime in out)
  (loop for i = (recv in)
        when (plusp (mod i prime))
          do (send out i)))

(defun sieve ()
  (let* ((c (chan))
         (prime-chan (chan)))
    (sb-thread:make-thread #'(lambda ()
                               (counter c)))
    (sb-thread:make-thread
     #'(lambda ()
         (loop
           (let* ((prime (recv c))
                  (newc (chan)))
             (send prime-chan prime)
             (sb-thread:make-thread #'(lambda ()
                           (filter prime c newc)))
             (setf c newc)))))
    prime-chan))

(defun first-n-primes (n)
  (let* ((prime-chan (sieve))
         (threads (sb-thread:list-all-threads)))
    (prog1
        (loop repeat n collect (recv prime-chan))
      (map nil #'sb-thread:terminate-thread
           (set-difference (sb-thread:list-all-threads) threads)))))

(deftest test-tchan-sieve ()
  (is (equal (first-n-primes 10)
             '(2 3 5 7 11 13 17 19 23 29))))