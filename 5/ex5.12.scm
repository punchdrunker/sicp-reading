; 問題 5.12
; 与えられた制御器で計算機を実装する時に必要なデータパスを決めるのを助けるため,
; シミュレータを使うことが出来る.
; アセンブラを拡張し, 次の情報を計算機モデルに格納するようにせよ:
;
; • (assign, gotoなどの)命令の型で, 格納されたすべての(異なる)命令のリスト;
; • 入り口を保持するのに使った(異る)レジスタのリスト (goto命令の参照するレジスタである.);
; • save, restoreされる(異る)レジスタのリスト;
; • 各レジスタに対し, (異る)代入元のリスト(例えば図5.11の階乗計算機で, レジスタvalの元は(const 1)と((op *) (reg n) (reg val))である.)
;
; 計算機のメッセージパッシングインターフェースを拡張し, これらの新しい情報にアクセス出来るようにせよ.
; 解析プログラムをテストするため, 図5.12のFibonacci計算機を定義し, 構成したリストを調べよ. 

(load "./5.2.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        ;; 解析器を追加
        (analyzer (make-analyzer))
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
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register:" name))))
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
              ;; メッセージ処理に解析器分を追加する
              ((eq? message 'analyzer) analyzer)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (make-analyzer)
  (let ((analyze-list '((assign) (test) (branch) (goto) (save) (restore) (perform))))
    (define (add-analyzer inst label)
      (let ((analyzer (assoc label analyze-list)))
        (if analyzer
          (if (not (member inst analyzer))
            (set-cdr! analyzer (cons inst (cdr analyzer)))))))
    (define (print-analyzer)
      (print analyze-list))
    (define (dispatch message)
      (cond ((eq? message 'add) add-analyzer)
            ((eq? message 'print) print-analyzer)
            (else (error "Unknown request -- ANALYZER" message))))
    dispatch))

(define (print-analyzed-result machine)
  (((machine 'analyzer) 'print)))

(define (add-analyzer inst machine label)
  (((machine 'analyzer) 'add) inst label))

;; 命令の実行手続きを生成する際に解析を行うように処理を追加する
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (add-analyzer inst machine 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (add-analyzer inst machine 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (add-analyzer inst machine 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (add-analyzer inst machine 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (add-analyzer inst machine 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (add-analyzer inst machine 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (add-analyzer inst machine 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))
(define fib-machine
  (make-machine
    '(continue val n)
    (list (list '< <) (list '- -) (list '+ +))
    '(start
       (assign continue (label fib-done))
       fib-loop
           (test (op <) (reg n) (const 2))
           (branch (label immediate-answer))
           (save continue)
           (assign continue (label afterfib-n-1))
           (save n)
           (assign n (op -) (reg n) (const 1))
           (goto (label fib-loop))
       afterfib-n-1
           (restore n)
           (restore continue)
           (assign n (op -) (reg n) (const 2))
           (save continue)
           (assign continue (label afterfib-n-2))
           (save val)
           (goto (label fib-loop))
       afterfib-n-2
           (assign n (reg val))
           (restore val)
           (restore continue)
           (assign val (op +) (reg val) (reg n))
           (goto (reg continue))
       immediate-answer
           (assign val (reg n))
           (goto (reg continue))
       fib-done)))

(print-analyzed-result fib-machine)
