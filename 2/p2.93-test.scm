(load "./p2.93.scm")

(install-number-packages)

(define ps1 (make-sparse-polynomial 'x '((2 1) (0 1))))
(define ps2 (make-sparse-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational ps2 ps1))

(eqt '(rational (polynomial x sparse (5 2) (3 2) (2 2) (0 2)) polynomial x sparse (4 1) (2 2) (0 1))
     (add rf rf))

(eqt true
     (=zero? (sub rf rf)))

(eqt false
     (=zero? (sub (add rf rf) rf)))
