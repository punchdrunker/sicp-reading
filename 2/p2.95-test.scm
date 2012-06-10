(load "./p2.95.scm")

(install-number-packages)

(define p1 (make-sparse-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-sparse-polynomial 'x '((2 11) (0 7))))
(define p3 (make-sparse-polynomial 'x '((1 13) (0 5))))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

; p1になりそうだけど、全然違う結果になった
(eqt '(polynomial x sparse (2 1458/169) (1 -2916/169) (0 1458/169))
     (greatest-common-divisor q1 q2))

(define pd1 (make-dense-polynomial 'x '(1 -2 1)))
(define pd2 (make-dense-polynomial 'x '(11 0 7)))
(define pd3 (make-dense-polynomial 'x '(13 5)))
(define qd1 (mul pd1 pd2))
(define qd2 (mul pd1 pd3))

(eqt '(polynomial x dense 1458/169 -2916/169 1458/169)
     (greatest-common-divisor qd1 qd2))
