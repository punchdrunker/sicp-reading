(load "./helper.scm")

(define (square x) (* x x))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random-integer range))))

(define (monte-carlo trails experiment)
  (define (iter trails-remainding trails-passed)
    (cond ((= trails-remainding 0)
           (/ trails-passed trails))
          ((experiment)
           (iter (- trails-remainding 1) (+ trails-passed 1)))
          (else
            (iter (- trails-remainding 1) trails-passed))))
  (iter trails 0))

(define (estimate-integral p x1 x2 y1 y2 trails)
  (*
    (monte-carlo trails (lambda () (p (random-in-range x1 x2) (random-in-range y1 y2))))
    (* (- x2 x1) (- y2 y1))))

;; 面積と面積から算出した円周率piを表示する手続き
(define (pi-from-monte-carlo-simulation circle-area radius)
  (display circle-area)
  (newline)
  (/ circle-area radius))

