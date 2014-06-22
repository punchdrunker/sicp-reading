;
; 次の手続きのレジスタ計算機を実装せよ. リスト構造用のメモリー演算は, 計算機の基本演算として使用可能とする. 
;
; a. 再帰的count-leaves:
; (define (count-leaves tree)
;   (cond ((null? tree) 0)
;         ((not (pair? tree)) 1)
;         (else (+ (count-leaves (car tree))
;                  (count-leaves (cdr tree))))))
; b. カウンタを陽に持つ再帰的count-leaves:
; (define (count-leaves tree)
;   (define (count-iter tree n)
;     (cond ((null? tree) n)
;           ((not (pair? tree)) (+ n 1))
;           (else (count-iter (cdr tree)
;                             (count-iter (car tree) n)))))
;   (count-iter tree 0))
;
(load "./5.2.4.scm")

; a
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(define count-leaves-machine-a
  (make-machine
    '(continue tree val val-tmp tmp)
    (list (list '+ +) (list 'pair? pair?) (list 'null? null?) (list 'not not) (list 'car car) (list 'cdr cdr))
    '(start
       (assign continue (label count-leaves-done))
       (assign val (const 0))
       count-leaves-loop
       (test (op null?) (reg tree)) ;; tree が null
       (branch (label null))
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp)) ;; tree がペアでない
       (branch (label not-pair))
       ;; (count-leaves (car tree)) を実行するように設定
       (save continue)
       (assign continue (label count-leaves-with-car))
       (save tree) ;; tree を退避
       (assign tree (op car) (reg tree)) ;; tree を (car tree) に変える
       (goto (label count-leaves-loop)) ;; 再帰呼び出しを実行
       null
       (assign val (const 0))
       (goto (reg continue))
       not-pair
       (assign val (const 1))
       (goto (reg continue))
       count-leaves-with-car
       (restore tree)
       (restore continue)
       ;; (count-leaves (cdr tree)) を実行するように設定
       (assign tree (op cdr) (reg tree))
       (save continue)
       (assign continue (label count-leaves-with-cdr))
       (save val) ;; (count-leaves (car tree)) を退避
       (goto (label count-leaves-loop))
       count-leaves-with-cdr
       (assign val-tmp (reg val))
       (restore val)
       (restore continue)
       (assign val (op +) (reg val) (reg val-tmp))
       (goto (reg continue))
       count-leaves-done)))

(define x (cons (list 1 2) (list 3 4)))
(set-register-contents! count-leaves-machine-a 'tree (list x x))
(start count-leaves-machine-a)
(show-register-contents count-leaves-machine-a 'val)

; b
(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

(define count-leaves-machine-b
  (make-machine
    '(continue tree n val val-tmp tmp)
    (list (list '+ +) (list 'pair? pair?) (list 'null? null?) (list 'not not) (list 'car car) (list 'cdr cdr))
    '(start
       (assign continue (label count-leaves-done))
       (assign n (const 0))
       count-leaves-loop
       (test (op null?) (reg tree)) ;; tree が null
       (branch (label null))
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp)) ;; tree がペアでない
       (branch (label not-pair))
       (save continue)
       (assign continue (label count-iter-with-car))
       (save tree)
       (assign tree (op cdr) (reg tree))
       (goto (label count-leaves-loop))
       null
       (assign val (reg n))
       (goto (reg continue))
       not-pair
       (assign val (op +) (reg n) (const 1))
       (goto (reg continue))
       count-iter-with-car
       (restore tree)
       (restore continue)
       (assign tree (op car) (reg tree))
       (assign n (reg val))
       (goto (label count-leaves-loop))
       count-leaves-done)))

(define x (cons (list 1 2) (list 3 4)))
(set-register-contents! count-leaves-machine-b 'tree (list x x))
(start count-leaves-machine-b)
(show-register-contents count-leaves-machine-b 'val)
