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
; c. (restore y)は, yの後, 他のどのレジスタが退避され,
; 回復されていなくても, yから退避した最後の値をyに置く.
; シミュレータをこう振舞うように修正せよ.
; レジスタ毎に別のスタックを対応させなければならない.
; initialize-stack演算にすべてのレジスタスタックを初期化させなければならない. 
;

(load "./5.2.scm")

; レジスタ名毎にスタックを用意し、スタック操作はレジスタ名を引数にとってチェックする。
; スタックは連想リストを使ってレジスタ名と値のリストの形にする。
(define (make-stack)
  (let ((s '()))
    (define (push reg-name x)
      (let ((reg-stack (assoc reg-name s)))
        (if #?=reg-stack ;; リーダーマクロでスタックの中身を見る
          (set-cdr! reg-stack (cons x (cdr reg-stack)))
          (error "Wrong register -- PUSH" reg-name))))
    (define (pop reg-name)
      (let ((reg-stack (assoc reg-name s)))
        (if #?=reg-stack ;; リーダーマクロでスタックの中身を見る
          (if (null? (cdr reg-stack))
            (error "Empty stack of register -- POP" reg-name)
            (let ((top (cadr reg-stack)))
              (set-cdr! reg-stack (cddr reg-stack))
              top))
          (error "Wrong register -- POP" reg-name))))
    (define (add-register-to-stack reg-name)
      (if (assoc reg-name s)
        (error "Already registered -- STACK" reg-name)
        (set! s (cons (cons reg-name '()) s))))
    (define (initialize)
      (map (lambda (stack) (set-cdr! stack '())) s)
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) pop)
            ((eq? message 'add-register) add-register-to-stack)
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

(define (pop stack reg-name)
  ((stack 'pop) reg-name))

(define (push stack reg-name value)
  ((stack 'push) reg-name value))

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
      ; スタックにレジスタ名も登録する
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (begin
            (set! register-table
              (cons (list name (make-register name))
                    register-table))
            ((stack 'add-register) name)
            'register-allocated)))
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

(define (make-save inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (push stack reg-name (get-contents reg))
        (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (set-contents! reg (pop stack reg-name))
        (advance-pc pc)))))

(define test-machine
  (make-machine
    '(a b)
    '()
    '(start
       (assign a (const 1))
       (assign a (const 2))
       (assign b (const 2))
       (save a)
       (save b)
       (restore a)
       (restore a)
       (restore b)
       (goto (label done))
       done)))

(start test-machine)

(print "a is "
       (get-register-contents test-machine 'a))

(print "b is "
       (get-register-contents test-machine 'b))
