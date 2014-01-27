(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called to an empty queue")
        (car front-ptr)))
    (define (rear-queue)
      (if (empty-queue?)
        (error "REAR called to on empty queue")
        (car rear-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               front-ptr)
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)
                front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called to an empty queue"))
            (else
              (set-front-ptr! (cdr front-ptr)))))
    (define (print-queue)
      (newline)
      (display front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'front) front-queue)
            ((eq? m 'rear) rear-queue)
            ((eq? m 'print-queue) print-queue)
            (else
              (error "Undefined operation -- MAKE-QUEUE" m))))

    dispatch))


(define q (make-queue))
; それぞれの記述は局所状態を持つ手続きを返すので、
; 返ってきた手続きを実行するために、()で囲う
((q 'insert-queue!) 'a)
((q 'print-queue))
((q 'insert-queue!) 'b)
((q 'print-queue))
((q 'insert-queue!) 'c)
((q 'print-queue))

((q 'delete-queue!))
((q 'print-queue))
((q 'delete-queue!))
((q 'print-queue))
((q 'delete-queue!))
((q 'print-queue))
; 空のqueueにたいしてdeleteなので、エラー
((q 'delete-queue!))
;((q 'print-queue))

;((q 'hoge)) ; 存在しない手続きなので、エラー

(newline)
