;; 問題 5.13
;;
;; make-machineの引数としてレジスタリストを要求するのでなく,
;; 制御器の命令列を使って計算機の持つレジスタを決めるようにシミュレータを修正せよ.
;; make-machineであらかじめレジスタを割り当てる代りに,
;; 命令のアセンブリ中に, 初めて見る度に, レジスタを割り当てることが出来る. 
;;
;;
(load "./5.2.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
            (list
              (list 'initialize-stack
                    (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)
;      (define (lookup-register name)
;        (let ((val (assoc name register-table)))
;          (if val
;            (cadr val)
;            (error "Unknown register:" name))))
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (begin ; name がレジスタテーブルに登録されていない場合、登録を行う。
              (allocate-register name)
              (lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              ((instruction-execution-proc (car insts)))
              (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


(define expt-machine
  (make-machine
    ; 利用するレジスタ
    '()
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

(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 8)
(start expt-machine)
(show-register-contents expt-machine 'val)
