(in-package :cl-transactional-tests)

;;; Test trivial functionality, no actual threads.

(deftestsuite trivial-transactions (cl-transactional-tests)
  (ttvar1 ttvar2)
  (:setup (setf ttvar1 (make-tvar (make-queue :initial-contents (list 'a 'b 'c 'd 'e 'f))))
	  (setf ttvar2 (make-tvar (make-queue))))
  (:tests
   (retry-transaction
    (iter
      (while (with-retry-transaction
	       (let ((q1 (get-tvar ttvar1))
		     (q2 (get-tvar ttvar2)))
		 (unless (queue-empty-p q1)
		  (let ((q (queue-first q1)))
		    (put-tvar ttvar1 (dequeue q1))
		    (put-tvar ttvar2 (enqueue q2 q))
		    t))))))
    (ensure-same (queue-as-list (cl-transactional::value-of ttvar2))
		 (list 'a 'b 'c 'd 'e 'f)
		 :test #'equal))
   (orelse-transaction
    (iter
      (while (with-orelse-transaction
	       ((let ((q1 (get-tvar ttvar1))
		      (q2 (get-tvar ttvar2)))
		  (unless (queue-empty-p q1)
		    (let ((q (queue-first q1)))
		      (put-tvar ttvar1 (dequeue q1))
		      (put-tvar ttvar2 (enqueue q2 q))
		      t)))))))
    (ensure-same (queue-as-list (cl-transactional::value-of ttvar2))
		 (list 'a 'b 'c 'd 'e 'f)
		 :test #'equal))))
