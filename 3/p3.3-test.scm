(load "./p3.3.scm")

(define acc (make-account 100 'test))

(eqt 160
     ((acc 'test 'deposit) 60))

(eqt 60
     ((acc 'test 'withdraw) 100))

(eqt (test-error <error>)
     ((acc 'foo 'deposit) 10))
