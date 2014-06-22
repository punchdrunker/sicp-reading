(load "./p3.2.scm")

(define s (make-monitored sqrt))

(eqt 10
     (s 100))

(eqt 11
     (s 121))

(eqt 2
     (s 'how-many-calls?))

(eqt 0
     (s 'reset-count))

(eqt 0
     (s 'how-many-calls?))
