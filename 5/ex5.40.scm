; 問題 5.40
;
; 翻訳系を修正し, 上述の翻訳時環境を維持するようにせよ.
; つまりcompileとそれぞれのコード生成器に翻訳時環境の引数を加え,
; compile-lambda-body で翻訳時環境を拡張せよ. 

(load "./ex5.39.scm")

(define (compile-lambda-body exp proc-entry ct-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
      (make-instruction-sequence '(env proc argl) '(env)
                                 `(,proc-entry
                                    (assign env (op compiled-procedure-env) (reg proc))
                                    (assign env
                                            (op extend-environment)
                                            (const ,formals)
                                            (reg argl)
                                            (reg env))))
      ; 引数と環境をconsしたものを追加する
      (compile-sequence (lambda-body exp) 'val 'return (cons formals ct-env)))))

(define (compile-sequence seq target linkage ct-env)
  (if (last-exp? seq)
    (compile (first-exp seq) target linkage ct-env)
    (preserving '(env continue)
                (compile (first-exp seq) target 'next ct-env)
                (compile-sequence (rest-exps seq) target linkage ct-env))))

; 引数と環境をconsしたものを追加(ct-env)
; あとはcompileを呼び出してるところを片っ端から引数追加していく
(define (compile exp target linkage ct-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage ct-env))
        ((definition? exp)
         (compile-definition exp target linkage ct-env))
        ((if? exp) (compile-if exp target linkage ct-env))
        ((lambda? exp) (compile-lambda exp target linkage ct-env))
        ((begin? exp)
         (compile-sequence (begin-action exp)
                           target
                           linkage
                           ct-env))
        ((cond? exp) (compile (cond->if exp) target linkage ct-env))
        ((application? exp)
         (compile-application exp target linkage ct-env))
        (else
          (error "Unknown expression type -- COMPILE" exp))))

(define (compile-application exp target linkage ct-env)
  (let ((proc-code (compile (operator exp) 'proc 'next ct-env))
        (operand-codes
          (map (lambda (operand) (compile operand 'val 'next ct-env))
               (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

(define (compile-assignment exp target linkage ct-env)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next ct-env)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op set-variable-value!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))

(define (compile-definition exp target linkage ct-env)
  (let ((var (definition-variable exp))
        (get-value-code
          (compile (definition-value exp) 'val 'next ct-env)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op define-variable!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))

(define (compile-lambda exp target linkage ct-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
            (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage lambda-linkage
                            (make-instruction-sequence '(env) (list target)
                                                       `((assign ,target
                                                                 (op make-compiled-procedure)
                                                                 (label ,proc-entry)
                                                                 (reg env)))))
          (compile-lambda-body exp proc-entry ct-env))
        after-lambda))))

(define (compile-if exp target linkage ct-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
            (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next ct-env))
            (c-code
              (compile
                (if-consequent exp) target consequent-linkage ct-env))
            (a-code
              (compile (if-alternative exp) target linkage ct-env)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                      (make-instruction-sequence '(val) '()
                                                 `((test (op false?) (reg val))
                                                   (branch (label ,f-branch))))
                      (parallel-instruction-sequences
                        (append-instruction-sequences t-branch c-code)
                        (append-instruction-sequences f-branch a-code))
                      after-if))))))
;(show-code
;  (compile
;    '(f 'x 'y)
;    'val
;    'next
;    '()
;    ))

;(show-code
;  (compile
;    '(set! x 4)
;    'val
;    'next
;    '()
;    ))

;(show-code
;  (compile
;    '(define (increment a)
;       (+ a 1))
;    'val
;    'next
;    '()
;    ))

;(show-code
;  (compile
;    '(if (= 1 1)
;       (1)
;       (0))
;    'val
;    'next
;    '()
;    ))
