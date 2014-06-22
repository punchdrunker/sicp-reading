(load "./register_machine.scm")
(define fact-machine
  (make-machine
    '(continue val n)
    (list (list '= =) (list '- -) (list '* *))
    '(start
       (assign continue (label fact-done))
       fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
       after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))
       (goto (reg continue))
       base-case
       (assign val (const 1))
       (goto (reg continue))
       fact-done)))

(define (fact n)
  ((fact-machine 'stack) 'initialize)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  (format #t "fact:~2d => ~8d" n (get-register-contents fact-machine 'val))
  ((fact-machine 'stack) 'print-statistics)
  (newline))

(define (fact-iter n)
  (if (< n 10)
    (begin
      (fact n)
      (fact-iter (+ n 1)))))

(fact 1)
;(total-pushes = 0 maximum-depth = 0)
;(fact 3)
;(total-pushes = 4 maximum-depth = 4)
;(fact 5)
;(total-pushes = 8 maximum-depth = 8)
;(fact 10)
;(total-pushes = 18 maximum-depth = 18)
;(fact 100)
;(total-pushes = 198 maximum-depth = 198)
;(fact 1000)
;(total-pushes = 1998 maximum-depth = 1998)
