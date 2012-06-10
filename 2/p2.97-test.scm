(load "./p2.97.scm")

(install-number-packages)

(define p1 (make-sparse-polynomial 'x '((1 1)(0 1))))
(define p2 (make-sparse-polynomial 'x '((3 1)(0 -1))))
(define p3 (make-sparse-polynomial 'x '((1 1))))
(define p4 (make-sparse-polynomial 'x '((2 1)(0 -1))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(eqt '(rational (polynomial x sparse (3 -1) (2 -2) (1 -3) (0 -1)) (polynomial x sparse (4 -1) (3 -1) (1 1) (0 1)))
     (add rf1 rf2))

