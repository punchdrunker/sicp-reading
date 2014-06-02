(load "./5.2.scm")
(load "./ex5.8-2.scm")

(define test-machine
  (make-machine
    '(a)
    '()
    '(start
       (goto (label here))
       here
       (assign a (const 3))
       (goto (label there))
       here
       (assign a (const 4))
       (goto (label there))
       there)))

(set-register-contents! test-machine 'a 0)
(start test-machine)
(show-register-contents test-machine 'a)
