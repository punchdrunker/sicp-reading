; 問題 5.49
; 積極制御評価器の読込み-評価-印字ループを使う代用として
; 読込み-評価-印字ループを実行するレジスタ計算機を設計せよ.
; つまり計算機は式を読み込み, それを翻訳し, 結果のコードをアセンブリして実行し,
; 結果を印字するループを走らせなければならない.
; これはわれわれのシミュレートした設定では,
; 手続きcompileとassembleを「レジスタ計算機の演算」として呼び出せるので,
; 走らせるのは簡単である. 

(load "./register_machine.scm")
(load "./compiler.scm")

(define (empty-arglist) '())

(define (last-operand? ops)
  (null? (cdr ops)))

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define the-global-environment (setup-environment))

(define (get-global-environment)
  the-global-environment)

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))

(define (compiled-procedure-entry c-proc)
  (cadr c-proc))

(define (compiled-procedure-env c-proc)
  (caddr c-proc))

(define (make-register-machine) register-machine)

(define operations
  (list (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'application? application?)
        ;;;
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        (list 'operands operands)
        (list 'operator operator)
        (list 'empty-arglist empty-arglist)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'last-operand? last-operand?)
        (list 'adjoin-arg adjoin-arg)
        (list 'rest-operands rest-operands)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'extend-environment extend-environment)
        (list 'procedure-body procedure-body)
        (list 'begin-actions begin-actions)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)
        (list 'if-predicate if-predicate)
        (list 'true? true?)
        (list 'if-alternative if-alternative)
        (list 'if-consequent if-consequent)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'make-compiled-procedure make-compiled-procedure)
        (list 'compiled-procedure? compiled-procedure?)
        (list 'compiled-procedure-entry compiled-procedure-entry)
        (list 'compiled-procedure-env compiled-procedure-env)
        (list 'list list)
        (list 'cons cons)
        (list 'false? false?)
        (list 'print-stack-statistics)
        (list 'make-register-machine make-register-machine)
        (list 'compile compile)
        (list 'statements statements)
        (list 'assemble assemble)
        ))

(define register-machine
  (make-machine
    '(exp env val proc argl continue unev compapp machine)
    operations
    '(
      (assign machine (op make-register-machine))
      read-eval-print-loop
      (perform (op initialize-stack))
      (perform
        (op prompt-for-input) (const ";;; RM-Eval input:"))
      (assign exp (op read))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (goto (label compile))
      print-result
      (perform (op print-stack-statistics))
      (perform
        (op announce-output) (const ";;; RM-Eval value:"))
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))
      compile
      (assign exp (op compile) (reg exp) (const val) (const return))
      (assign exp (op statements) (reg exp))
      (goto (label assemble))
      assemble
      (assign val (op assemble) (reg exp) (reg machine))
      (goto (reg val))
      )))

(define (start-register-machine)
  (set! the-global-environment (setup-environment))
  (set-register-contents! register-machine 'flag false)
  (start register-machine))

(start-register-machine)
