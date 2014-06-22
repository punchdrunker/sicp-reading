; 問題 5.43
;
; われわれは4.1.6節で, ブロック構造の内部定義は「真の」defineと考えるべきでないと論じた.
; むしろ手続き本体は, 定義している内部変数は,
; set!を使って正しい値に初期化する通常のlambda変数として組み込まれているように解釈すべきである.
; 4.1.6節と問題4.16は, 超循環評価器を修正し, 内部定義を掃き出してこれを実現する方法を示した.
; 翻訳系を修正し, 手続き本体を翻訳する前に, 同じ変換を実行するようにせよ. 

(load "./ex5.42.scm")
(define (scan-out-defines body)
  (define (iter exp vars sets exps)
    (if (null? exp)
      (list (reverse vars) (reverse sets) (reverse exps))
      (if (definition? (car exp))
        (iter
          (cdr exp)
          (cons (list (definition-variable (car exp)) ''*unassigned*) vars)
          (cons (list 'set! (definition-variable(car exp)) (definition-value (car exp))) sets)
          exps)
        (iter
          (cdr exp)
          vars
          sets
          (cons (car exp) exps)))))
  (define (include-define? exp)
    (if (null? exp)
      #f
      (if (definition? (car exp))
        #t
        (include-define? (cdr exp)))))
  (if (include-define? body)
    (let ((var-val-exp-list (iter body '() '() '())))
      (list (cons 'let (cons (car var-val-exp-list) (append (cadr var-val-exp-list) (caddr var-val-exp-list))))))
    body))


(define (compile-lambda-body exp proc-entry ct-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
      (make-instruction-sequence
        '(env proc argl)
        '(env)
        `(,proc-entry
           (assign env (op compiled-procedure-env) (reg proc))
           (assign env
                   (op extend-environment)
                   (const ,formals)
                   (reg argl)
                   (reg env))))
      (compile-sequence
        ; (lambda-body exp)
        (scan-out-defines (lambda-body exp))
        'val
        'return
        (cons formals ct-env)))))

(show-code
  (compile
    '(lambda (x y)
       (define u
         (+ u x))
       (define v
         (- v y))
       (* u v))
    'val
    'next
    '()))
