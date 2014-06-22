(load "./4.1.2.scm")

(define true #t)
(define false #f)
(define (make-frame variables values)
  (cons variables values))
(define (first-frame env) (car env))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))
(define (tagged-list? exp tag)
  (if (pair? exp) 
        (eq? (car exp) tag)
              false))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'equal? equal?)
        ;        (list 'write-line write-line)
        (list '* *)
        (list '+ +)
        (list '- -)
        (list '= =)
        (list '/ /)
        ))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
      (define (scan vars vals)
            (cond ((null? vars)
                         (add-binding-to-frame! var val frame))
                        ((eq? var (car vars))
                                     (set-car! vals val))
                                    (else (scan (cdr vars) (cdr vals)))))
          (scan (frame-variables frame)
                    (frame-values frame))))

(define the-empty-environment '())
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define the-global-environment (setup-environment))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))


(define (primitive-implementation proc) (cadr proc))



(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")


(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))


(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))

(define the-global-environment (setup-environment))

(driver-loop)

;;; M-Eval input:
;;; (define (append x y)
;;;   (if (null? x)
;;;         y
;;;               (cons (car x)
;;;                           (append (cdr x) y))))
;;;                           ;;; M-Eval value:
;;;                           ok
;;;
;;;                           ;;; M-Eval input:
;;;                           (append '(a b c) '(d e f))
