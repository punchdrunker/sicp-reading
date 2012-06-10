(use gauche.test)

(define-syntax eqt
  (syntax-rules ()
      ((_ a b)
           (test* (quote b) a b))))

