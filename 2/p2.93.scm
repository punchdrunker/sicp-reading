(load "./helper.scm")
(load "./p2.92.scm")

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  ;; (分子 分母) となるリストにする
  (define (make-rat n d) (cons n d))

  ;; 多項式をたすきがけ
  (define (add-rat x y) (make-rat (add (mul (numer x) (denom y))
                                       (mul (numer y) (denom x)))
                                  (mul (denom x) (denom y))))
  (define (sub-rat x y) (make-rat (sub (mul (numer x) (denom y))
                                       (mul (numer y) (denom x)))
                                  (mul (denom x) (denom y))))
  (define (mul-rat x y) (make-rat (mul (numer x) (numer y))
                                  (mul (denom x) (denom y))))
  (define (div-rat x y) (make-rat (mul (numer x) (denom y))
                                  (mul (denom x) (numer y))))
  (define (equ-rat x y) (and (equ? (numer x) (numer y))
                             (equ? (denom x) (denom y))))
  (define (=zero-rat x) (=zero? (numer x)))
  (define (ratio r)
    (if (rational-function? r)
      (error "RATIO: scalar ratio of polynomials not supported" r)
      (/ (numer r) (denom r))))
  (define (rational-function? r)
    (or (eq? 'polynomial (type-tag (numer r)))
        (eq? 'polynomial (type-tag (denom r)))))
  (define (rational->real r) (make-real (exact->inexact (ratio r))))
  (define (project r) (make-integer (truncate (ratio r))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'make       'rational             (lambda (n d) (tag (make-rat n d))))
  (put 'add        '(rational rational)  (lambda (x y) (tag (add-rat x y))))
  (put 'sub        '(rational rational)  (lambda (x y) (tag (sub-rat x y))))
  (put 'mul        '(rational rational)  (lambda (x y) (tag (mul-rat x y))))
  (put 'div        '(rational rational)  (lambda (x y) (tag (div-rat x y))))
  (put 'equ?       '(rational rational)  (lambda (x y) (equ-rat x y)))
  (put '=zero?     '(rational)           (lambda (x)   (=zero-rat x)))
  (put 'raise      '(rational)           (lambda (x)   (rational->real x)))
  (put 'type-level '(rational)           (lambda (x)   2))
  (put 'project    '(rational)           (lambda (x)   (project x)))
  (put 'sq-root    '(rational)           (lambda (x)   (make-real (sqrt (ratio x)))))
  (put 'square     '(rational)           (lambda (x)   (tag (mul-rat x x))))
  (put 'sine       '(rational)           (lambda (x)   (make-real (sin (ratio x)))))
  (put 'cosine     '(rational)           (lambda (x)   (make-real (cos (ratio x)))))
  (put 'arctan     '(rational rational)  (lambda (x y) (make-real (atan (ratio x) (ratio y)))))
  (put 'negate     '(rational)           (lambda (x)   (sub (tag (make-rat 0 1)) x)))

  'done)



(install-rational-package)
