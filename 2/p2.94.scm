(load "./helper.scm")
(load "./p2.92.scm")


;; ======================================================================
;;
;; The polynomial package
;;
;; ======================================================================
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (tag p) (attach-tag 'polynomial p))

  (define (make-dense-poly var terms)  ((get 'make-poly 'dense)  var terms))
  (define (make-sparse-poly var terms) ((get 'make-poly 'sparse) var terms))

  (define (make-poly var terms) (cons var terms))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define variable? symbol?)

  ; use alphabetic order of the variable as its ordering - ie a > b > c ...
  (define (var-order>? v1 v2)
    (string>? (symbol->string v1)
              (symbol->string v2)))

  ; coerce psrc into a poly of the same variable as ptarget
  ; both psrc and ptarget have had their type tags stripped
  ; and so contain only var and terms; in order to play nicely 
  ; with the rest of the system and work at the same level of abstraction
  ; psrc is de-constructed and re-constructed using the same
  ; type of term list as it originally had, although that's an arbitrary decision
  ; (choosing either sparse or dense as an executive decision would have worked equally well)
  (define (coerce-poly psrc ptarget)
    (let ((coerce-var (variable ptarget))
          (poly-constructor (if (eq? (type-tag (contents psrc)) 'dense)
                              make-dense-polynomial
                              make-sparse-polynomial))
          (zeroth-term (make-term 0 (tag psrc))))  
      (contents (poly-constructor coerce-var (list zeroth-term)))))

  ; assuming x is a higher order variable than y
  ; convert the polynomial Py into a polynomial in x and simply add it to Px
  ; Py = 0x^n + 0x^n-1 + .. + 0x + Py
  (define (add-poly p1 p2)
    (cond ((same-variable? (variable p1) (variable p2))
           (make-poly (variable p1)
                      (add (term-list p1)
                           (term-list p2))))
          ((var-order>? (variable p1) (variable p2))
           (add-poly p1 (coerce-poly p2 p1)))
          (else 
            (add-poly p2 (coerce-poly p1 p2)))))

  ; assuming x is a higher order variable than y
  ; convert the polynomial Py into a polynomial in x and simply multiply it by Px
  ; Py = 0x^n + 0x^n-1 + .. + 0x + Py
  (define (mul-poly p1 p2)
    (cond ((same-variable? (variable p1) (variable p2))
           (make-poly (variable p1)
                      (mul (term-list p1)
                           (term-list p2))))
          ((var-order>? (variable p1) (variable p2))
           (mul-poly p1 (coerce-poly p2 p1)))
          (else 
            (mul-poly p2 (coerce-poly p1 p2)))))

  (define (poly-zero? p)
    (=zero? (term-list p)))

  (define (negate-poly p)
    (make-poly (variable p) 
               (negate (term-list p))))

  (define (add-integer-poly i p)
    (make-poly (variable p)
               (add i (term-list p))))

  (define (mul-integer-poly i p)
    (make-poly (variable p)
               (mul i (term-list p))))

  ; Much nicer to define sub-poly in terms of add poly to reduce effort maintaining
  ; now the more complicated logic of havng polys in different variables is introduced
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  (define (sub-integer-poly i p)
    (make-poly (variable p)
               (sub i (term-list p))))

  (define (sub-poly-integer p i)
    (make-poly (variable p)
               (sub (term-list p) i)))

  (define (div-result var termlists)
    (list (tag (make-poly var (car termlists)))
          (tag (make-poly var (cadr termlists)))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (div-result (variable p1) (div (term-list p1) (term-list p2)))
      (error "Polys not in same var -- DIV-POLY"
             (list p1 p2))))

  (define (div-integer-poly i p)
    (div-result (variable p) (div i (term-list p))))

  (define (div-poly-integer p i)
    (div-result (variable p) (div (term-list p) i)))

  ;; added ======>
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (greatest-common-divisor (term-list p1)
                                          (term-list p2)))
      (error "Polys not in same var -- GCD-POLY"
             (list p1 p2))))
  ;; <====== added


  ;; interface to rest of the system
  (put 'make   'term                    (lambda (order coeff) (list order coeff)))
  (put 'make-dense-poly  'polynomial    (lambda (var terms)   (tag (make-dense-poly var terms))))
  (put 'make-sparse-poly 'polynomial    (lambda (var terms)   (tag (make-sparse-poly var terms))))
  (put 'add    '(polynomial polynomial) (lambda (p1 p2)       (tag (add-poly p1 p2))))
  (put 'add    '(integer polynomial)    (lambda (i p)         (tag (add-integer-poly i p))))
  (put 'add    '(polynomial integer)    (lambda (p i)         (tag (add-integer-poly i p))))
  (put 'mul    '(polynomial polynomial) (lambda (p1 p2)       (tag (mul-poly p1 p2))))
  (put 'mul    '(integer polynomial)    (lambda (i p)         (tag (mul-integer-poly i p))))
  (put 'mul    '(polynomial integer)    (lambda (p i)         (tag (mul-integer-poly i p))))
  (put '=zero? '(polynomial)            (lambda (p1)          (poly-zero? p1)))
  (put 'sub    '(polynomial polynomial) (lambda (p1 p2)       (tag (sub-poly p1 p2))))
  (put 'sub    '(integer polynomial)    (lambda (i p)         (tag (sub-integer-poly i p))))
  (put 'sub    '(polynomial integer)    (lambda (p i)         (tag (sub-poly-integer p i))))
  ; div doesn't return a poly - it returns a list of 2 polys
  (put 'div    '(polynomial polynomial) (lambda (p1 p2)       (div-poly p1 p2)))
  (put 'div    '(integer polynomial)    (lambda (i p)         (div-integer-poly i p)))
  (put 'div    '(polynomial integer)    (lambda (p i)         (div-poly-integer p i)))
  (put 'negate '(polynomial)            (lambda (p)           (tag (negate-poly p))))
  ;; added
  (put 'gcd    '(polynomial polynomial) (lambda (p1 p2)       (tag (gcd-poly p1 p2))))

  'done)


;; ======================================================================
;;
;; The dense terms package
;;
;; ======================================================================
(define (install-dense-terms-package)
  ;; internal procedures
  ;; representation of poly
  (define (tag terms) (attach-tag 'dense terms))

  ; Some items from the sparse package are needed to iterate sparse termslist.
  ; NB always use the published interface to interact with other types 
  (define (sparse-first-term terms)      ((get 'first-term 'sparse) terms))
  (define (sparse-rest-terms terms)      ((get 'rest-terms 'sparse) terms))
  (define (sparse-empty-termlist? terms) ((get 'empty-termlist? 'sparse) terms))
  (define (sparse-the-empty-termlist)    ((get 'the-empty-termlist 'sparse)))

  (define (sparse->dense terms)
    (define (sparse-iter L)
      (cond ((sparse-empty-termlist? L) (the-empty-termlist))
            ((=zero? (coeff (sparse-first-term L))) (sparse-iter (sparse-rest-terms L)))
            (else (adjoin-term (sparse-first-term L)
                               (sparse-iter (sparse-rest-terms L))))))
    (sparse-iter terms))

  (define (make-poly variable term-list)
    ; leading zeros are not desirable
    (define (shrink-termlist terms)
      (cond ((empty-termlist? terms) (the-empty-termlist))
            ((=zero? (coeff (first-term terms)))
             (shrink-termlist (rest-terms terms)))
            (else terms)))
    (cons variable (tag (shrink-termlist term-list))))

  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define variable? symbol?)

  (define (the-empty-termlist) nil)
  (define (first-term term-list) (make-term (- (length term-list) 1)
                                            (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (equal? term-list (the-empty-termlist)))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (make-zero-terms n)
    (if (= n 0)
      nil
      (cons 0 (make-zero-terms (- n 1)))))

  (define (zero-pad-terms max-order terms)
    (append (make-zero-terms (- max-order
                                (length terms)))

            terms))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons (coeff term)
            (zero-pad-terms (order term) term-list))))

  (define (terms-zero? terms)
    (if (empty-termlist? terms)
      true
      (and (=zero? (coeff (first-term terms)))
           (terms-zero? (rest-terms terms)))))

  (define (add-terms L1 L2) 
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))    
          (mul-term-by-all-terms t1 (rest-terms L))))))  

  (define (term-map f terms)
    (if (empty-termlist? terms) 
      (the-empty-termlist)
      (adjoin-term (f (first-term terms))
                   (term-map f (rest-terms terms)))))

  (define (negate-term term)
    (make-term (order term)
               (negate (coeff term))))

  (define (negate-terms p)
    (term-map negate-term p))

  (define (sub-terms L1 L2)
    (add-terms L1 (negate-terms L2)))

  (define (add-integer-terms i L)
    (add-terms (number->termlist i) L))

  (define (sub-integer-terms i L)
    (sub-terms (number->termlist i) L))

  (define (sub-terms-integer L i)
    (sub-terms  L (number->termlist i)))

  (define (mul-integer-terms i L)
    (mul-terms (number->termlist i) L))

  (define (number->termlist i)
    (adjoin-term (make-term 0 i) (the-empty-termlist)))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
          (list (the-empty-termlist) L1)
          (let* ((new-c (div (coeff t1) (coeff t2)))
                 (new-o (- (order t1) (order t2)))
                 (new-first-term (make-term new-o new-c)))
            (let ((rest-of-result (div-terms 
                                    (sub-terms 
                                      L1 
                                      (mul-term-by-all-terms new-first-term L2)) 
                                    L2)))
              (list (adjoin-term new-first-term
                                 (car rest-of-result))
                    (cadr rest-of-result))))))))

  (define (div-integer-terms i L)
    (div-terms (number->termlist i) L))

  (define (div-terms-integer L i)
    (div-terms L (number->termlist i)))

  ;; added ======>
  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))
  ;; <====== added 

  ;; interface to rest of the system
  (put 'make-poly           'dense          (lambda (var terms)   (make-poly var terms)))
  (put 'add                '(dense dense)   (lambda (t1 t2)       (tag (add-terms t1 t2))))
  (put 'add                '(dense sparse)  (lambda (t1 t2)       (tag (add-terms t1 (sparse->dense t2)))))
  (put 'add                '(integer dense) (lambda (t1 t2)       (tag (add-integer-terms t1 t2))))
  (put 'add                '(dense integer) (lambda (t1 t2)       (tag (add-integer-terms t2 t1))))
  (put 'mul                '(dense dense)   (lambda (t1 t2)       (tag (mul-terms t1 t2))))
  (put 'mul                '(dense sparse)  (lambda (t1 t2)       (tag (mul-terms t1 (sparse->dense t2)))))
  (put 'mul                '(integer dense) (lambda (i t)         (tag (mul-integer-terms i t))))
  (put 'mul                '(dense integer) (lambda (t i)         (tag (mul-integer-terms t i))))
  (put '=zero?             '(dense)         (lambda (t)           (terms-zero? t)))
  (put 'sub                '(dense dense)   (lambda (t1 t2)       (tag (sub-terms t1 t2))))
  (put 'sub                '(dense sparse)  (lambda (t1 t2)       (tag (sub-terms t1 (sparse->dense t2)))))
  (put 'sub                '(integer dense) (lambda (i t)         (tag (sub-integer-terms i t))))
  (put 'sub                '(dense integer) (lambda (t i)         (tag (sub-terms-integer t i))))
  (put 'div                '(dense dense)   (lambda (t1 t2)       (map tag (div-terms t1 t2))))
  (put 'div                '(dense sparse)  (lambda (t1 t2)       (map tag (div-terms t1 (sparse->dense t2)))))
  (put 'div                '(integer dense) (lambda (i t)         (map tag (div-integer-terms i t))))
  (put 'div                '(dense integer) (lambda (t i)         (map tag (div-terms-integer t i))))
  (put 'negate             '(dense)         (lambda (t)           (tag (negate-terms t))))
  (put 'first-term         'dense           (lambda (t)           (first-term t)))
  (put 'rest-terms         'dense           (lambda (t)           (rest-terms t)))
  (put 'the-empty-termlist 'dense           (lambda ()            the-empty-termlist))
  (put 'empty-termlist?    'dense           (lambda (t)           (empty-termlist? t)))
  ;; added 
  (put 'gcd                '(dense dense)   (lambda (t1 t2)       (tag (gcd-terms t1 t2))))
  (put 'gcd                '(dense sparse)  (lambda (t1 t2)       (tag (gcd-terms t1 (sparse->dense t2)))))

  'done)

;; ======================================================================
;;
;; The sparse terms package
;;
;; ======================================================================
(define (install-sparse-terms-package)
  ;; internal procedures
  ;; representation of poly
  (define (tag terms) (attach-tag 'sparse terms))

  ; Some items from the dense package are needed to iterate dense termslists.
  ; NB always use the published interface to interact with other types 
  (define (dense-first-term terms)      ((get 'first-term 'dense) terms))
  (define (dense-rest-terms terms)      ((get 'rest-terms 'dense) terms))
  (define (dense-empty-termlist? terms) ((get 'empty-termlist? 'dense) terms))
  (define (dense-the-empty-termlist)    ((get 'the-empty-termlist 'dense)))

  (define (dense->sparse terms)
    (define (dense-iter L)
      (cond ((dense-empty-termlist? L) (the-empty-termlist))
            ((=zero? (coeff (dense-first-term L))) (dense-iter (dense-rest-terms L)))
            (else (adjoin-term (dense-first-term L)
                               (dense-iter (dense-rest-terms L))))))
    (dense-iter terms))



  (define (make-poly variable term-list)
    (cons variable (tag term-list)))


  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define variable? symbol?)

  (define (the-empty-termlist) nil)
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (equal? term-list (the-empty-termlist)))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (terms-zero? terms)
    (if (empty-termlist? terms)
      true
      (and (=zero? (coeff (first-term terms)))
           (terms-zero? (rest-terms terms)))))

  (define (add-terms L1 L2) 
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))  

  (define (term-map f terms)
    (if (empty-termlist? terms) 
      (the-empty-termlist)
      (adjoin-term (f (first-term terms))
                   (term-map f (rest-terms terms)))))

  (define (negate-term term)
    (make-term (order term)
               (negate (coeff term))))

  (define (negate-terms p)
    (term-map negate-term p))

  (define (add-integer-terms i L)
    (add-terms (number->termlist i) L))

  (define (sub-terms L1 L2)
    (add-terms L1 (negate-terms L2)))

  (define (sub-integer-terms i L)
    (sub-terms L (number->termlist i)))

  (define (sub-terms-integer i L)
    (sub-terms (number->termlist i) L))

  (define (mul-integer-terms i L)
    (mul-terms (number->termlist i) L))

  (define (number->termlist i)
    (adjoin-term (make-term 0 i) (the-empty-termlist)))  

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
          (list (the-empty-termlist) L1)
          (let* ((new-c (div (coeff t1) (coeff t2)))
                 (new-o (- (order t1) (order t2)))
                 (new-first-term (make-term new-o new-c)))
            (let ((rest-of-result (div-terms 
                                    (sub-terms 
                                      L1 
                                      (mul-term-by-all-terms new-first-term L2)) 
                                    L2)))
              (list (adjoin-term new-first-term
                                 (car rest-of-result))
                    (cadr rest-of-result))))))))

  (define (div-integer-terms i L)
    (div-terms (number->termlist i) L))

  (define (div-terms-integer L i)
    (div-terms (number->termlist i) L))

  ;; added ======>
  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))
  ;; <====== added 


  ;; interface to rest of the system
  (put 'make-poly           'sparse          (lambda (var terms)   (make-poly var terms)))
  (put 'add                '(sparse sparse)  (lambda (t1 t2)       (tag (add-terms t1 t2))))
  (put 'add                '(sparse dense)   (lambda (t1 t2)       (tag (add-terms t1 (dense->sparse t2)))))
  (put 'add                '(integer sparse) (lambda (t1 t2)       (tag (add-integer-terms t1 t2))))
  (put 'add                '(sparse integer) (lambda (t1 t2)       (tag (add-integer-terms t2 t1))))
  (put 'mul                '(sparse sparse)  (lambda (t1 t2)       (tag (mul-terms t1 t2))))
  (put 'mul                '(sparse dense)   (lambda (t1 t2)       (tag (mul-terms t1 (dense->sparse t2)))))
  (put 'mul                '(integer sparse) (lambda (i t)         (tag (mul-integer-terms i t))))
  (put 'mul                '(sparse integer) (lambda (t i)         (tag (mul-integer-terms t i))))
  (put '=zero?             '(sparse)         (lambda (t)           (terms-zero? t)))
  (put 'sub                '(sparse sparse)  (lambda (t1 t2)       (tag (sub-terms t1 t2))))
  (put 'sub                '(sparse dense)   (lambda (t1 t2)       (tag (sub-terms t1 (dense->sparse t2)))))
  (put 'sub                '(integer sparse) (lambda (i t)         (tag (sub-integer-terms i t))))
  (put 'sub                '(sparse integer) (lambda (t i)         (tag (sub-terms-integer t i))))
  (put 'div                '(sparse sparse)  (lambda (t1 t2)       (map tag (div-terms t1 t2))))
  (put 'div                '(sparse dense)   (lambda (t1 t2)       (map tag (div-terms t1 (dense->sparse t2)))))
  (put 'div                '(integer sparse) (lambda (i t)         (map tag (div-integer-terms i t))))
  (put 'div                '(sparse integer) (lambda (t i)         (map tag (div-terms-integer t i))))
  (put 'negate             '(sparse)         (lambda (t)           (tag (negate-terms t))))
  (put 'first-term         'sparse           (lambda (t)           (first-term t)))
  (put 'rest-terms         'sparse           (lambda (t)           (rest-terms t)))
  (put 'the-empty-termlist 'sparse           (lambda ()            the-empty-termlist))
  (put 'empty-termlist?    'sparse           (lambda (t)           (empty-termlist? t)))
  (put 'gcd                '(sparse sparse)  (lambda (t1 t2)       (tag (gcd-terms t1 t2))))
  (put 'gcd                '(sparse dense)   (lambda (t1 t2)       (tag (gcd-terms t1 (dense->sparse t2)))))

  'done)



;; ======================================================================
;;
;; The integer number package
;;
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
  ;; added
  (put 'gcd        '(integer integer) (lambda (x y) (tag (gcd x y))))

  'done)


;; added
(define (greatest-common-divisor x y) (apply-generic 'gcd x y))


(install-integer-package)
(install-polynomial-package)
