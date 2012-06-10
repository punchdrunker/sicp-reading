;; ======================================================================
;; The polynomial package
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
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))

  (define (div-integer-poly i p)
    (div-result (variable p) (div i (term-list p))))

  (define (div-poly-integer p i)
    (div-result (variable p) (div (term-list p) i)))

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

  'done)


