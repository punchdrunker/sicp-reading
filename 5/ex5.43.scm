; 問題 5.43
;
; われわれは4.1.6節で, ブロック構造の内部定義は「真の」defineと考えるべきでないと論じた.
; むしろ手続き本体は, 定義している内部変数は,
; set!を使って正しい値に初期化する通常のlambda変数として組み込まれているように解釈すべきである.
; 4.1.6節と問題4.16は, 超循環評価器を修正し, 内部定義を掃き出してこれを実現する方法を示した.
; 翻訳系を修正し, 手続き本体を翻訳する前に, 同じ変換を実行するようにせよ. 
;
; 4.1.6
; http://sicp.iijlab.net/fulltext/x416.html
; lambda式の本体を評価する前に本体にある内部定義を 「掃き出し」消してしまう.
; 内部で定義された変数はletで作り出し, 代入で値を設定する.

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
    ; 内部定義がなければ何もしない
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
        ; scan-out-definesを利用する
        ; (lambda-body exp)
        (scan-out-defines (lambda-body exp))
        'val
        'return
        (cons formals ct-env)))))

;(show-code
;  (compile
;    '(lambda (x y)
;       (define u
;         (+ u x))
;       (define v
;         (- v y))
;       (* u v))
;    'val
;    'next
;    '())
;  )
