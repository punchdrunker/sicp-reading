(load "./helper.scm")

(define (rand-update x)
  (remainder (+ x 1812433253) 4294967296))

(define rand
  (let ((x 1))
    (define (reset new-value)
      (set! x new-value) x)
    (define (generate)
      (set! x (rand-update x)) x)
    (define (dispatch m)
      (cond ((eq? m 'reset)
             reset)
            ((eq? m 'generate)
             (generate))
            (else
              (error "Unknown arg -- RAND" (list arg)))))
    dispatch))
