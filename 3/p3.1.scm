(load "./helper.scm")

(define (make-accumulator delta)
  (lambda (amount)
      (set! delta (+ delta amount))
          delta))

