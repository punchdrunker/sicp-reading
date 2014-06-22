(load "./p3.4.scm")

(define acc (make-account 100 'test))

(eqt 40
     ((acc 'test 'withdraw) 60))

(eqt 70
     ((acc 'test 'deposit) 30))

(eqt (test-error <error>)
     ((acc 'foo 'withdraw) 70))

(eqt 20
     ((acc 'test 'withdraw) 50))

(eqt (test-error <error>)
     ((acc 'foo 'withdraw) 10))

(eqt (test-error <error>)
     ((acc 'foo 'withdraw) 20))
