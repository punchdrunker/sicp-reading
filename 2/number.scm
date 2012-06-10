;; ======================================================================
;; The integer number package
;; ======================================================================
(define (install-integer-package)
  ;; internal procedures 

  (define (tag x) (attach-tag 'integer x))

  ; complex numbers can now have parts which are
  ; made from sub-types within our system and so
  ; it's now possible to call raise with an argument 
  ; that is a constant integer, rational or a real.
  (define (raise x)
    (cond ((integer? x)  (make-rational x 1))
          ((rational? x) (make-rational (numerator x) (denominator x)))
          ((real? x)     (raise (rationalize x 1/100000)))))

  ;; interface to rest of the system
  (put 'make       'integer           (lambda (x)   (tag x)))
  (put 'add        '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub        '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul        '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div        '(integer integer) (lambda (x y) (tag (/ x y))))
  (put 'equ?       '(integer integer) (lambda (x y) (= x y)))
  (put '=zero?     '(integer)         (lambda (x)   (zero? x)))
  (put 'raise      '(integer)         (lambda (x)   (make-rational x 1)))
  (put 'type-level '(integer)         (lambda (x)   1))
  ; does it make any sense to have sine, cosine, sq-root and arctan for integer?
  (put 'sq-root    '(integer)         (lambda (x)   (make-real (sqrt x))))
  (put 'square     '(integer)         (lambda (x)   (tag (* x x))))
  (put 'sine       '(integer)         (lambda (x)   (make-real(sin x))))
  (put 'cosine     '(integer)         (lambda (x)   (make-real(cos x))))
  (put 'arctan     '(integer integer) (lambda (x y) (make-real(atan x y))))
  (put 'negate     '(integer)         (lambda (x)   (sub (tag (make-integer 0)) x)))

  'done)
;; ======================================================================
;; The real number package
;; ======================================================================
(define (install-real-package)
  ;; internal procedures
  (define (tag x) (attach-tag 'real x))

  ; all complex types are created from lower types
  ; and so must be tagged using generic constructors
  (define (real->complex r)
    (make-complex-from-real-imag (make-real r) 
                                 (make-real 0)))

  (define (project r) 
    (cond ((integer? r) (make-rational r 1))
          (else (let ((rat (rationalize r 1/100000)))
                  (make-rational (numerator rat)
                                 (denominator rat))))))
  (define (make x)
    (let ((type (type-tag x))
          (val  (contents x)))
      (cond ((eq? type 'integer)  (tag x))
            ((eq? type 'rational) (tag (raise x)))
            ((eq? type 'real)     x)
            (else (error "MAKE-REAL : Bad type argument : " x)))))

  ;; interface to rest of the system
  (put 'make        'real        (lambda (x)   (make x)))
  (put 'add         '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub         '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul         '(real real) (lambda (x y) (tag (* x y))))
  (put 'div         '(real real) (lambda (x y) (tag (/ x y))))
  (put 'equ?        '(real real) (lambda (x y) (= x y)))
  (put '=zero?     '(real)       (lambda (x)   (zero? x)))
  (put 'raise      '(real)       (lambda (x)   (real->complex x)))
  (put 'type-level '(real)       (lambda (x)   3))
  (put 'project    '(real)       (lambda (x)   (project x)))
  (put 'sq-root    '(real)       (lambda (x)   (tag (sqrt x))))
  (put 'square     '(real)       (lambda (x)   (tag (* x x))))
  (put 'sine       '(real)       (lambda (x)   (tag (sin x))))
  (put 'cosine     '(real)       (lambda (x)   (tag (cos x))))
  (put 'arctan     '(real real)  (lambda (x y) (tag (atan x y))))
  (put 'negate     '(real)       (lambda (x)   (sub (tag (make 0)) x)))
  'done)

;; ======================================================================
;; The rectangular number package
;; ======================================================================
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  ; internally rectangular numbers are (real . imag) 
  ; and each part can be any of the lower types in the tower 
  ; so all calcuations must use generic functions
  (define (magnitude z)
    (sq-root (add (square (real-part z))
                  (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (mul r (cosine a)) (mul r (sine a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'make-from-real-imag 'rectangular   (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'rectangular   (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part           '(rectangular) (lambda (x)   (real-part x)))
  (put 'imag-part           '(rectangular) (lambda (x)   (imag-part x)))
  (put 'magnitude           '(rectangular) (lambda (x)   (magnitude x)))
  (put 'angle               '(rectangular) (lambda (x)   (angle x)))

  'done)

;; ======================================================================
;;
;; The polar number package
;;
;; ======================================================================
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))

  ; internally polar numbers are (mag . ang) 
  ; and each part can be any of the lower types in the tower 
  ; so all calcuations must use generic functions
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y) 
    (make-from-mag-ang (sq-root (add (square x) (square y)))
                       (arctan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'make-from-real-imag 'polar   (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'polar   (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part           '(polar) (lambda (x)   (real-part x)))
  (put 'imag-part           '(polar) (lambda (x)   (imag-part x)))
  (put 'magnitude           '(polar) (lambda (x)   (magnitude x)))
  (put 'angle               '(polar) (lambda (x)   (angle x)))

  'done)


;; ======================================================================
;;
;; The complex number package
;;
;; ======================================================================
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (tag z) (attach-tag 'complex z))
  (define (add-complex z1 z2) (make-from-real-imag (add (real-part z1) (real-part z2))
                                                   (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2) (make-from-real-imag (sub (real-part z1) (real-part z2))
                                                   (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2) (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                                                 (add (angle z1) (angle z2))))
  (define (div-complex z1 z2) (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                                                 (sub (angle z1) (angle z2))))
  (define (equ-complex z1 z2) (and (equ? (magnitude z1) (magnitude z2))
                                   (equ? (angle z1) (angle z2))))
  (define (=zero-complex z1) (zero? (magnitude z1)))
  (define (project z1)
    (make-real (real-part z1)))

  ;; interface to rest of the system
  (put 'make-from-real-imag 'complex           (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'complex           (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'add                 '(complex complex) (lambda (x y) (tag (add-complex x y))))
  (put 'sub                 '(complex complex) (lambda (x y) (tag (sub-complex x y))))
  (put 'mul                 '(complex complex) (lambda (x y) (tag (mul-complex x y))))
  (put 'div                 '(complex complex) (lambda (x y) (tag (div-complex x y))))
  (put 'equ?                '(complex complex) (lambda (x y) (equ-complex x y)))
  (put '=zero?              '(complex)         (lambda (x)   (=zero-complex x)))
  (put 'real-part           '(complex)         (lambda (x)   (real-part x)))
  (put 'imag-part           '(complex)         (lambda (x)   (imag-part x)))
  (put 'magnitude           '(complex)         (lambda (x)   (magnitude x)))
  (put 'angle               '(complex)         (lambda (x)   (angle x)))
  (put 'type-level          '(complex)         (lambda (x)   4))
  (put 'project             '(complex)         (lambda (x)   (project x)))
  (put 'negate              '(complex)         (lambda (x)   (sub (tag (make-from-real-imag 0 0)) x)))

  'done)
