(in-package :cl-transactional-tests)

;;; Test trivial functionality, no actual threads.

(in-suite cl-transactional-tests)

(defsuite* trivial-transactions)

(deftest test-trivial-transaction (subtest)
  (let ((tvar1 (make-tvar (fset:seq 'a 'b 'c 'd 'e 'f)))
        (tvar2 (make-tvar (fset:seq))))
    (funcall subtest tvar1 tvar2)
    (is (equal (gmap:gmap :list #'identity (:seq (cl-transactional::value-of tvar1))) nil))
    (is (equal (gmap:gmap :list #'identity (:seq (cl-transactional::value-of tvar2))) '(a b c d e f)))))

(deftest test-trivial-retry-transaction (tvar1 tvar2)
  (iter
    (while (with-retry-transaction
             (let ((q1 (get-tvar tvar1))
                   (q2 (get-tvar tvar2)))
               (unless (fset:empty? q1)
                 (let ((q (fset:first q1)))
                   (put-tvar tvar1 (fset:less-first q1))
                   (put-tvar tvar2 (fset:with-last q2 q))
                   t)))))))

(deftest test-trivial-orelse-transaction (tvar1 tvar2)
  (iter
    (while (with-orelse-transaction
             ((let ((q1 (get-tvar tvar1))
                    (q2 (get-tvar tvar2)))
                (unless (fset:empty? q1)
                  (let ((q (fset:first q1)))
                    (put-tvar tvar1 (fset:less-first q1))
                    (put-tvar tvar2 (fset:with-last q2 q))
                    t))))))))

(deftest test-trivial-transactions ()
  (test-trivial-transaction #'test-trivial-retry-transaction)
  (test-trivial-transaction #'test-trivial-orelse-transaction))

(deftest test-orelse-orelse ()
  (is (equal 'a (with-orelse-transaction
                  ((retry-transaction))
                  ('a)))))

;;; Test basic interleaving, retrying, orelsing

(in-suite cl-transactional-tests)

(defsuite* simple-transactions)

(deftest test-simple-transaction (subtest)
  (let ((tvar1 (make-tvar (fset:seq 'a 'b 'a 'b 'a 'b 'a 'b 'a 'b)))
        (tvar2 (make-tvar (fset:seq))))
    (funcall subtest tvar1 tvar2)
    (is (equal (gmap:gmap :list #'identity (:seq (cl-transactional::value-of tvar1))) nil))
    (is (equal (gmap:gmap :list #'identity (:seq (cl-transactional::value-of tvar2)))
               (list 'b 'a 'b 'a 'b 'a 'b 'a 'b 'a)))))

(deftest test-simple-retry-transaction (tvar1 tvar2)
  (let ((reverter (lambda ()
                    (iter
                      (while
                          (with-retry-transaction
                            (let ((q1 (get-tvar tvar1))
                                  (q2 (get-tvar tvar2)))
                              (unless (fset:empty? q1)
                                (let ((q (fset:first q1))
                                      (nq (fset:less-first q1)))
                                  (let ((qq (fset:first nq)))
                                    (put-tvar tvar2 (fset:with-last (fset:with-last q2 qq) q))
                                    (put-tvar tvar1 (fset:less-first nq))))
                                t))))
                      (sb-thread:thread-yield)))))
    (mapcar #'sb-thread:join-thread (list (sb-thread:make-thread reverter)
                                          (sb-thread:make-thread reverter)))))


(deftest test-simple-orelse-transaction (tvar1 tvar2)
  (let ((reverter (lambda ()
                    (iter
                      (while
                          (with-orelse-transaction
                            ((let ((q1 (get-tvar tvar1))
                                   (q2 (get-tvar tvar2)))
                               (unless (fset:empty? q1)
                                 (let ((q (fset:first q1))
                                       (nq (fset:less-first q1)))
                                   (let ((qq (fset:first nq)))
                                     (put-tvar tvar2 (fset:with-last (fset:with-last q2 qq) q))
                                     (put-tvar tvar1 (fset:less-first nq))))
                                 t)))))
                      (sb-thread:thread-yield)))))
    (mapcar #'sb-thread:join-thread (list (sb-thread:make-thread reverter)
                                          (sb-thread:make-thread reverter)))))

(deftest test-simple-transactions ()
  (test-simple-transaction #'test-simple-retry-transaction)
  (test-simple-transaction #'test-simple-orelse-transaction))

(deftest test-with-random-seq (subtest)
  (let ((random-list (iter (repeat 100)
                           (collect (random-elt
                                     '(a b c d e f g h i j k l m n o p r s))))))
    (let ((tvar1 (make-tvar (gmap:gmap :seq #'identity (:list random-list))))
          (tvar2 (make-tvar (fset:seq))))
      (is (equal (funcall subtest tvar1 tvar2) random-list))
      (is (equal (gmap:gmap :list #'identity (:seq (cl-transactional::value-of tvar1))) nil))
      (is (equal (gmap:gmap :list #'identity (:seq (cl-transactional::value-of tvar2))) nil)))))

(deftest test-simple-retry-and-wait (tvar1 tvar2)
  (let ((t1 (sb-thread:make-thread #'(lambda ()
                                       (iter
                                         (repeat 100)
                                         (let ((q (with-retry-transaction
                                                    (let ((q1 (get-tvar tvar2)))
                                                      (when (fset:empty? q1)
                                                        (retry-transaction))
                                                      (put-tvar tvar2 (fset:less-first q1))
                                                      (fset:first q1)))))
                                           (collect q))))
                                   :name "Collector"))
        (t2 (sb-thread:make-thread #'(lambda ()
                                       (iter
                                         (let ((q (with-retry-transaction
                                                    (let ((q1 (get-tvar tvar1)))
                                                      (unless (fset:empty? q1)
                                                        (put-tvar tvar2 (fset:with-last (get-tvar tvar2) (fset:first q1)))
                                                        (put-tvar tvar1 (fset:less-first q1))
                                                        (fset:first q1))))))
                                           (while q)
                                           (sb-thread:thread-yield))))
                                   :name "Mover")))
    (sb-thread:join-thread t2)
    (sb-thread:join-thread t1)))

(deftest test-random-simple-retry-and-wait ()
  (iter (repeat 100)
        (test-with-random-seq #'test-simple-retry-and-wait)))
