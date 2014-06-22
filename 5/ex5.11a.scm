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
; a. (restore y)は, どのレジスタから値が来たかに関係なく, スタックに退避した最後の値をyに置く.
; これはわれわれのシミュレータの振舞いである.
; この振舞いが5.1.4節(図5.12)のFibonacci計算から一命令を除去するのに使えることを示せ. 
;

(load "./5.2.scm")

(define fib-machine
  (make-machine
    ; 利用するレジスタ
    '(n val continue)
    ; 利用する演算子
    (list (list '< <) (list '+ +) (list '- -))
    ; 利用する命令
    '((assign continue (label fib-done))
      fib-loop
          (test (op <) (reg n) (const 2))
          (branch (label immediate-answer))
          ;; Fib(n-1)を計算するよう設定
          (save continue)
          (assign continue (label afterfib-n-1))
          (save n)                           ; nの昔の値を退避
          (assign n (op -) (reg n) (const 1)); nを n-1 に変える
          (goto (label fib-loop))            ; 再帰呼出しを実行
      afterfib-n-1                         ; 戻った時 Fib(n-1)はvalにある
          (restore n)
          (restore continue)
          ;; Fib(n-2)を計算するよう設定
          (assign n (op -) (reg n) (const 2))
          (save continue)
          (assign continue (label afterfib-n-2))
          (save val)                         ; Fib(n-1)を退避
          (goto (label fib-loop))
      afterfib-n-2                         ; 戻った時Fib(n-2)の値はvalにある
;          (assign n (reg val))               ; nにはFib(n-2)がある
;          (restore val)                      ; valにはFib(n-1)がある
          (restore n)

          (restore continue)
          (assign val                        ; Fib(n-1)+Fib(n-2)
                  (op +) (reg val) (reg n))
          (goto (reg continue))              ; 呼出し側に戻る. 答えはvalにある
      immediate-answer
          (assign val (reg n))               ; 基底の場合: Fib(n)=n
          (goto (reg continue))
      fib-done)
    ))

(set-register-contents! fib-machine 'n 7)
(start fib-machine)
(show-register-contents fib-machine 'val)

; nにはFib(n-1)が入っていて、valにはFib(n-2)が入ってることになる.
; どっちにしろ足してvalに入れるだけなので同じことになる.
