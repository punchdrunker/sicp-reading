(load "./5.5.ss")
(load "./5.5.7.ss")

(compile-and-go
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))
(factorial 5)
end
(start-eceval)
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
(factorial 5)
end


(compile-and-go
  '(define (fib n)
     (if (< n 2)
         n
         (+ (fib (- n 1)) (fib (- n 2)))))) 
(fib 5)
end
