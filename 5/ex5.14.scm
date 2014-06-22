(load "./5.2.4.scm")

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

(fact-iter 4)
