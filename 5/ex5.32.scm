(load "./5.5.scm")

(define eceval
  (make-machine
    '(exp env val proc argl continue unev)
    ev-application
    (save continue)
    (assign unev (op operands) (reg exp))
    (assign exp (op operator) (reg exp))
    (test (op symbol?) (reg exp))
    (branch (label ev-operand-symbol))
    (save env)
    (save unev)
    (assign continue (label ev-appl-did-operator-not-symbol))
    (goto (label eval-dispatch))
    ev-operand-symbol
    (save unev)
    (assign continue (label ev-appl-did-operator-symbol))
    (goto (label eval-dispatch))
    ev-appl-did-operator-not-symbol
    (restore unev)
    (restore env)
    (goto (label ev-appl-did-operator))
    ev-appl-did-operator-symbol
    (restore unev)
    (goto (label ev-appl-did-operator))
    ev-appl-did-operator
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)
    eceval-body))
