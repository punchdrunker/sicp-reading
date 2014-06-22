;問題 5.11
;
; 5.1.4節でsaveとrestoreを紹介した時
; (save y)
; (save x)
; (restore y)
; のように最後に退避したのではないレジスタを回復しようとすると,
; 何が起きるかは規定しなかった.
; restoreの意味にはいくつかの正当な可能性がある. 
;
; b. (restore y)は, スタックに退避した最後の値を, それがyから退避された時だけ, yに置き, それ以外はエラーとする.
; シミュレータをこのように振舞うよう修正せよ.
; スタックに値と一緒にレジスタ名を置くよう, saveを変更しなければならない. 

(load "./5.2.scm")

;(define (make-save inst machine stack pc)
;  (let ((reg (get-register machine
;                           (stack-inst-reg-name inst))))
;    (lambda ()
;      (push stack (get-contents reg))
;      (advance-pc pc))))

(define (make-save inst machine stack pc)
  ; レジスタ名とその値をペアで持つ
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (push stack (cons reg-name (get-contents reg)))
        (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (let ((head-of-stack (pop stack)))
        ;;レジスタ名のチェック
          (if (eq? (car head-of-stack) reg-name)
            (set-contents! reg (cdr head-of-stack))
            (error "Wrong register name - RESTORE" reg-name)))
        (advance-pc pc)))))

(define test-machine
  (make-machine
    '(a b)
    '()
    '(start
       (assign a (const 1))
       (assign b (const 2))
       (save a)
       (save b)
       ; 逆にrestoreしてみる
       (restore a)
       (restore b)
       (goto (label done))
       done)))

(start test-machine)
(print "a is "
       (get-register-contents test-machine 'a))
(print "b is "
       (get-register-contents test-machine 'b))
