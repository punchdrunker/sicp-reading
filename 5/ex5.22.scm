; 問題 5.22
;
; 3.3.1節の問題3.12は, 二つのリストを連接して新しいリストを形成するappend手続きと,
; 二つのリストを貼り合せるappend!手続きを示した.
; これらの手続きのそれぞれを実装するレジスタ計算機を設計せよ.
; リスト構造用メモリー演算は基本演算として使用可能とする. 

(load "./5.2.4.scm")

(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

(define append-machine
  (make-machine
    '(continue lis1 lis2 lis-result cons-tmp)
    (list (list 'null? null?) (list 'cons cons) (list 'car car) (list 'cdr cdr))
    '(start
       (assign continue (label append-done))
       append-loop
       (test (op null?) (reg lis1))
       (branch (label null))
       (save continue)
       (assign continue (label after-append))
       (save lis1)
       (assign lis1 (op cdr) (reg lis1))
       (goto (label append-loop))
       null
       (assign lis-result (reg lis2))
       (goto (reg continue))
       after-append
       (restore lis1)
       (restore continue)
       (assign cons-tmp (op car) (reg lis1))
       (assign lis-result (op cons) (reg cons-tmp) (reg lis-result))
       (goto (reg continue))
       append-done)))
(set-register-contents! append-machine 'lis1 '(a b))
(set-register-contents! append-machine 'lis2 '(c d))
(start append-machine)
(show-register-contents append-machine 'lis-result)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))
(define append!-machine
  (make-machine
    '(continue lis1 lis2 cdr-tmp)
    (list (list 'set-cdr! set-cdr!) (list 'null? null?) (list 'cdr cdr))
    '(start
       (assign continue (label append!-done))
       (save lis1)
       append!-loop
       (assign cdr-tmp (op cdr) (reg lis1))
       (test (op null?) (reg cdr-tmp))
       (branch (label null))
       (assign lis1 (op cdr) (reg lis1))
       (goto (label append!-loop))
       null
       (assign lis1 (op set-cdr!) (reg lis1) (reg lis2))
       (goto (reg continue))
       append!-done
       (restore lis1)
       )))

(set-register-contents! append!-machine 'lis1 '(a b))
(set-register-contents! append!-machine 'lis2 '(c d e))
(start append!-machine)
(show-register-contents append!-machine 'lis1)
