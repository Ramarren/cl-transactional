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
		 :test #'equal))
   (orelse-orelse
    (ensure-same 'a (with-orelse-transaction
		      ((retry-transaction))
		      ('a))))))

;;; Test basic interleaving, retrying, orelsing

(defrandom-instance random-list simple-transactions
  (iter (repeat 100)
	(collect (random-element 'simple-transactions '(a b c d e f g h i j k l m n o p r s)))))

(deftestsuite simple-transactions (cl-transactional-tests)
  (ttvar1 ttvar2)
  (:setup (setf ttvar1 (make-tvar (make-queue :initial-contents (list 'a 'b 'a 'b 'a 'b 'a 'b 'a 'b))))
	  (setf ttvar2 (make-tvar (make-queue))))
  (:tests
   (retry-transactions
    (let ((reverter (lambda ()
		      (iter
			(while
			    (with-retry-transaction
			      (let ((q1 (get-tvar ttvar1))
				    (q2 (get-tvar ttvar2)))
				(unless (queue-empty-p q1)
				  (let ((q (queue-first q1))
					(nq (dequeue q1)))
				    (let ((qq (queue-first nq)))
				      (put-tvar ttvar2 (enqueue (enqueue q2 qq) q))
				      (put-tvar ttvar1 (dequeue nq))))
				  t))))
			(thread-yield)))))
      (mapcar #'join-thread (list (make-thread reverter)
				  (make-thread reverter)))
      (ensure (queue-empty-p (cl-transactional::value-of ttvar1)))
      (ensure-same (queue-as-list (cl-transactional::value-of ttvar2))
		   (list 'b 'a 'b 'a 'b 'a 'b 'a 'b 'a)
		   :test #'equal)))
   (orelse-transactions
    (let ((reverter (lambda ()
		      (iter
			(while
			    (with-orelse-transaction
			      ((let ((q1 (get-tvar ttvar1))
				     (q2 (get-tvar ttvar2)))
				 (unless (queue-empty-p q1)
				   (let ((q (queue-first q1))
					 (nq (dequeue q1)))
				     (let ((qq (queue-first nq)))
				       (put-tvar ttvar2 (enqueue (enqueue q2 qq) q))
				       (put-tvar ttvar1 (dequeue nq))))
				   t)))))
			(thread-yield)))))
      (mapcar #'join-thread (list (make-thread reverter)
				  (make-thread reverter)))
      (ensure (queue-empty-p (cl-transactional::value-of ttvar1)))
      (ensure-same (queue-as-list (cl-transactional::value-of ttvar2))
		   (list 'b 'a 'b 'a 'b 'a 'b 'a 'b 'a)
		   :test #'equal)))
   (retry-and-wait
    (ensure-random-cases 1000 ((init-list random-list))
      (setf ttvar1 (make-tvar (make-queue :initial-contents init-list))
	    ttvar2 (make-tvar (make-queue)))
      (let ((t1 (make-thread #'(lambda ()
				 (iter
				   (repeat 100)
				   (let ((q (with-retry-transaction
					      (let ((q1 (get-tvar ttvar2)))
						(when (queue-empty-p q1)
						  (retry-transaction))
						(put-tvar ttvar2 (dequeue q1))
						(queue-first q1)))))
				     (collect q))))
			     :name "Collector"))
	    (t2 (make-thread #'(lambda ()
				 (iter
				   (let ((q (with-retry-transaction
					      (let ((q1 (get-tvar ttvar1)))
						(unless (queue-empty-p q1)
						  (put-tvar ttvar2 (enqueue (get-tvar ttvar2) (queue-first q1)))
						  (put-tvar ttvar1 (dequeue q1))
						  (queue-first q1))))))
				     (while q)
				     (thread-yield))))
			     :name "Mover")))
	(join-thread t2)
	(ensure-same (join-thread t1) init-list
		     :test #'equal)
	(ensure (queue-empty-p (cl-transactional::value-of ttvar1)))
	(ensure (queue-empty-p (cl-transactional::value-of ttvar2))))))))

