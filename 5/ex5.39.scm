; 問題 5.39
;
; 新しい探索演算を実装する手続きlexical-address-lookupを書け.
; それは二つの引数---文面アドレスと実行時環境---をとり,
; 指定した文面アドレスに格納した変数の値を返す.
; lexical-address-lookupは変数の値が*unassigned*の記号ならエラーとする.
; また, 指定した文面アドレスにある変数の値を変更する演算を実装する手続きlexical-address-set!を書け. 

(load "./5.5.scm")

; lexical-addressはフレームと変位数のリスト
(define (make-lexical-address framenum displacement)
  (list framenum displacement))

(define (lexical-address-framenum lexical-address)
  (car lexical-address))

(define (lexical-address-displacement lexical-address)
  (cadr lexical-address))

; frameはリストの対: そのフレームで束縛されている変数のリストと, 対応づけられている値のリスト
(define (lexical-address-lookup lexical-address env)
  (let ((addr-frame (list-ref
                      env
                      (lexical-address-framenum lexical-address))))
    (let ((addr-val (list-ref
                      (frame-values addr-frame)
                      (lexical-address-displacement lexical-address))))
      (if (eq? addr-val '*unassigned*)
        (error "Var is unassigned"))
      (cons 'bound addr-val))))

(define (lexical-address-set! lexical-address env newval)
  (let ((addr-frame (list-ref
                      env
                      (lexical-address-framenum lexical-address))))
    (define (iter vals count)
      (cond ((null? vals)
             (error "Invalid lexical address - bad displacement"))
            ((= count 0)
             (set-car! vals newval))
            (else
              (iter (cdr vals) (+ 1 count)))))
    (iter
      (frame-values addr-frame)
      (lexical-address-displacement lexical-address))))
