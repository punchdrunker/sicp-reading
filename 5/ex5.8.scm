; 問題 5.8
;
; 次のレジスタ計算機の命令はラベルhereが複数回定義してあるので, 曖昧である:
; start
;   (goto (label here))
;   here
;     (assign a (const 3))
;     (goto (label there))
;   here
;     (assign a (const 4))
;     (goto (label there))
;   there
;
; これまでに書いたシミュレータでは, 制御がthereに達した時レジスタa の内容はどうなるか.
; extract-labels手続きを修正し, 同じラベル名が二つの異る場所を指すように使われたら, エラーとするようにせよ.
;
; 回答:
;  a には3が入っている

(load "./5.2.scm")
(load "./ex5.8-2.scm")

(define test-machine
  (make-machine
    '(a)
    '()
    '(start
       (goto (label here))
       here
       (assign a (const 3))
       (goto (label there))
       here
       (assign a (const 4))
       (goto (label there))
       there)))

(set-register-contents! test-machine 'a 0)
(start test-machine)
(show-register-contents test-machine 'a)

