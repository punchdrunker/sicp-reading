; 問題 5.7
;
; このシミュレータを使い, 問題5.4で設計した計算機をテストせよ. 

(load "./5.2.scm")

;a. 再帰的べき乗:
;(define (expt b n)
;  (if (= n 0)
;    1
;    (* b (expt b (- n 1)))))
;b. 反復的べき乗:
;(define (expt b n)
;  (define (expt-iter counter product)
;    (if (= counter 0)
;      product
;      (expt-iter (- counter 1) (* b product))))
;  (expt-iter n 1))

(define expt-machine
  (make-machine
    ; 利用するレジスタ
    '(b n val continue)
    ; 利用する演算子
    (list (list '= =) (list '* *) (list '- -))
    ; 利用する命令
    '((assign continue (label expt-done))
      expt-loop
          (test (op =) (reg n) (const 0))
          (branch (label base-case))
          (save continue)
          (save n)
          (assign n (op -) (reg n) (const 1))
          (assign continue (label after-expt))
          (goto (label expt-loop))
      after-expt
          (restore n)
          (restore continue)
          (assign val (op *) (reg b) (reg val))
          (goto (reg continue))
      base-case
          (assign val (const 1))
          (goto (reg continue))
      expt-done)))

;(set-register-contents! expt-machine 'b 2)
;(set-register-contents! expt-machine 'n 8)
;(start expt-machine)
;(show-register-contents expt-machine 'val)

;b. 反復的べき乗:
;(define (expt b n)
;  (define (expt-iter counter product)
;    (if (= counter 0)
;      product
;      (expt-iter (- counter 1) (* b product))))
;  (expt-iter n 1))

(define expt-machine2
  (make-machine
    '(b n counter product)
    (list (list '= =) (list '* *) (list '- -))
    '((assign counter (reg n))
      (assign product (const 1))
      expt-loop
          (test (op =) (reg counter) (const 0))
          (branch (label expt-done))
          (assign counter (op -) (reg counter) (const 1))
          (assign product (op *) (reg b) (reg product))
          (goto (label expt-loop))
      expt-done)))

(set-register-contents! expt-machine2 'b 2)
(set-register-contents! expt-machine2 'n 8)
(start expt-machine2)
(show-register-contents expt-machine2 'product)
