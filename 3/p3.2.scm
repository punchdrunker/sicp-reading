(load "./helper.scm")


(define (make-monitored func)
  (define mf 0)
  (define (how-many-calls?) mf)
  (define (reset-count) (set! mf 0) mf)
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) (how-many-calls?))
          ((eq? m 'reset-count) (reset-count))
          (else (set! mf (+ 1 mf))
                (func m))))
  dispatch)

