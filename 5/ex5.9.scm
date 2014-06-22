;問題 5.9
;
; 上の機械演算の扱いは定数やレジスタの内容の他ラベルにも演算することを許す.
; 式の処理の手続きを修正し, 演算はレジスタと定数にだけ使えるという条件を強要するようにせよ. 

(load "./5.2.scm")
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
          (map (lambda (e)
                 ; ラベル表現の場合はエラーとする
                 (if (label-exp? e)
                   (error "演算はラベルには使えません.レジスタと定数を利用してください. -- ASSEMBLE" e)
                   (make-primitive-exp e machine labels)))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define test-machine
  (make-machine
    '()
    (list (list '= =))
    '(hoge
       (test (op =) (label hoge) (label hoge)))))
