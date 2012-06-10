(load "./p2.96.scm")

(install-number-packages)

;; a
(define ps1 (make-sparse-polynomial 'x '((2 1) (1 -2) (0 1))))
(define ps2 (make-sparse-polynomial 'x '((2 11) (0 7))))
(define ps3 (make-sparse-polynomial 'x '((1 13) (0 5))))
(define qs1 (mul ps1 ps2))
(define qs2 (mul ps1 ps3))


; 今度は係数が整数になった
(eqt '(polynomial x sparse (2 1458) (1 -2916) (0 1458))
     (greatest-common-divisor qs1 qs2))


;(define pd1 (make-dense-polynomial 'x '(1 -2 1)))
;(define pd2 (make-dense-polynomial 'x '(11 0 7)))
;(define pd3 (make-dense-polynomial 'x '(13 5)))
;(define qd1 (mul pd1 pd2))
;(define qd2 (mul pd1 pd3))
;(eqt '(polynomial x dense 1458 -2916 1458)
;     (greatest-common-divisor qd1 qd2))


;; b

(define ps1 (make-sparse-polynomial 'x '((2 1) (1 -2) (0 1))))
(define ps2 (make-sparse-polynomial 'x '((2 11) (0 7))))
(define ps3 (make-sparse-polynomial 'x '((1 13) (0 5))))
(define qs1 (mul ps1 ps2))
(define qs2 (mul ps1 ps3))

(define pd1 (make-dense-polynomial 'x '(1 -2 1)))
(define pd2 (make-dense-polynomial 'x '(11 0 7)))
(define pd3 (make-dense-polynomial 'x '(13 5)))
(define qd1 (mul pd1 pd2))
(define qd2 (mul pd1 pd3))

; 今度は最大公約数がp1になった
(eqt ps1
     (greatest-common-divisor qs1 qs2))

;(eqt pd1
;     (greatest-common-divisor qd1 qd2))
