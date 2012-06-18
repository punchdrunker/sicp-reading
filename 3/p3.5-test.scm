(load "./p3.5.scm")

(display "中心(5, 7) 半径3 の円の場合")
(newline)
(define (p-test x y)
  (<= (+ (square (- x 5)) (square (- y 7))) (square 3)))


(pi-from-monte-carlo-simulation (estimate-integral p-test 2 8 4 10 100000.0) (square 3))
(pi-from-monte-carlo-simulation (estimate-integral p-test 2 8 4 10 100000.0) (square 3))
(pi-from-monte-carlo-simulation (estimate-integral p-test 2 8 4 10 100000.0) (square 3))

(display "中心(5, 5) 半径5 の円の場合")
(newline)
(define (p-test x y)
  (<= (+ (square (- x 5)) (square (- y 5))) (square 5)))

; 結果
(pi-from-monte-carlo-simulation (estimate-integral p-test 0 10 0 10 100000.0) (square 5))
(pi-from-monte-carlo-simulation (estimate-integral p-test 0 10 0 10 100000.0) (square 5))
(pi-from-monte-carlo-simulation (estimate-integral p-test 0 10 0 10 100000.0) (square 5))


