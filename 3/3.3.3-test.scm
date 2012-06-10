(load "./helper.scm")
(load "./3.3.3.scm")

(define (install-deriv-package)
  ;; internal procedures
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (addend s) (car s))
  (define (augend s) (cadr s))

  (define (make-product m1 m2) (list '* m1 m2))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))

  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (define (deriv-product exp var)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand  exp) var))
      (make-product (deriv (multiplier  exp) var)
                    (multiplicand exp))))

  (put 'deriv '+ deriv-sum))
