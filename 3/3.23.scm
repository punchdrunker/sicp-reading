(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (list '() '()))

(define (make-item value)
  (cons value (cons '() '())))

(define (set-next-item! item next)
  (set-cdr! (cdr item) next))

(define (set-prev-item! item prev)
  (set-car! (cdr item) prev))

(define (next-item item)
  (cddr item))

(define (prev-item item)
  (cadr item))

(define (value-of-item item)
  (car item))



(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    ((value-of-item (front-ptr queue)))))

(define (rear-queue queue)
  (if (empty-queue? queue)
    (error "REAR called with an empty queue" queue)
    ((value-of-item (rear-ptr queue)))))

(define (front-insert-queue! queue value)
  (let ((new-item (make-item value)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-item)
           (set-rear-ptr! queue new-item)
           queue)
          (else
            (set-next-item! new-item (front-ptr queue))
            (set-front-ptr! queue new-item)
            queue))))

(define (rear-insert-queue! queue value)
  (let ((new-item (make-item value)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-item)
           (set-rear-ptr! queue new-item)
           queue)
          (else
            (set-prev-item! new-item (rear-ptr queue))
            (set-next-item! (rear-ptr queue) new-item)
            (set-rear-ptr! queue new-item)
            queue))))

(define (front-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (next-item (front-ptr queue)))
          queue)))

(define (rear-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-rear-ptr! queue (prev-item (rear-ptr queue)))
          queue)))

(define (print-queue queue)
  (define (print-iter q)
    (cond ((eq? q (rear-ptr queue))
           (display " ")
           (display (value-of-item q)))
          (else
            (begin (display " ")
                   (display (value-of-item q))
                   (print-iter (next-item q))))))
  (if (empty-queue? queue)
    (begin (display "empty")
           (newline))
    (begin (display "(")
           (print-iter (front-ptr queue))
           (display ")")))
  (newline))

(define q (make-queue))

(front-insert-queue! q 'a)
(print-queue q)
(front-insert-queue! q 'b)
(print-queue q)
(rear-insert-queue! q 'c)
(print-queue q)
(rear-insert-queue! q 'd)
(print-queue q)
(front-delete-queue! q)
(print-queue q)
(front-delete-queue! q)
(print-queue q)
(rear-delete-queue! q)
(print-queue q)

(newline)
