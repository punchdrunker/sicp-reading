(load "./p2.94.scm")

(install-number-packages)

(define (show x) (newline) (display x))

(define p1 (make-sparse-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-sparse-polynomial 'x '((3 1) (1 -1))))

(eqt '(polynomial x sparse (2 -1) (1 1))
     (greatest-common-divisor p1 p2))


;(define pd1 (make-dense-polynomial 'x '(1 -1 -2 2 0)))
;(define pd2 (make-dense-polynomial 'x '(1 0 -1 0)))
;(eqt '(polynomial x dense -1 1 0)
;     (greatest-common-divisor pd1 pd2))
