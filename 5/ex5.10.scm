;問題 5.10
;
; レジスタ計算機命令に新しい構文を設計し, シミュレータがその新しい構文を使えるように修正せよ.
; 本節の構文手続き以外のシミュレータ部分を変更せず, 新しい構文を実装出来るか. 

(load "./5.2.scm")
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        ; 追加
        ((eq? (car inst) 'increment) 
         (make-inc inst machine pc)) 
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (make-inc inst machine pc) 
  (let ((target  
          (get-register machine (inc-reg-name inst)))) 
    (lambda () 
      (set-contents! target (+ (get-contents target) 1)) 
      (advance-pc pc)))) 
(define (inc-reg-name inst) 
  (cadr inst)) 

(define test-machine
  (make-machine
    '(a)
    '()
    '(hoge
       (increment a)
       (increment a)
       )))
(set-register-contents! test-machine 'a 0)
(start test-machine)
(show-register-contents test-machine 'a)
