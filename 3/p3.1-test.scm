(load "./p3.1.scm")

(define A (make-accumulator 5))
(define B (make-accumulator 5))

(eqt 15 (A 10))
(eqt 25 (A 10))
(eqt 15 (B 10))
