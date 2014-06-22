(load "../5.4/5.4.ss")
(load "./5.5.ss")
(make-instruction-sequence '(env continue) '(val)
                           '((assign val
                                     (op lookup-variable-value) (const x) (reg env))
                             (goto (reg continue))))

(compile-linkage 'return)
(compile-linkage 'hoge)

(list-union '(a b c) '(b c))
(list-difference '(a b c) '(b c))

(quoted? 'a)

(compile
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n)))
  'val
  'next)

;ex5.33
(compile
  '(define (factorial-alt n)
     (if (= n 1)
         1
         (* n (factorial (- n 1)))))
  'val
  'next)




(print-after-compiler
  (compile
    '(define (factorial n)
       (if (= n 1)
           1
           (* (factorial (- n 1)) n)))
    'val
    'next))

(print-after-compiler
  (compile
    '(define (factorial-alt n)
       (if (= n 1)
           1
           (* n (factorial (- n 1)))))
    'val
    'next))

(print-after-compiler
  (compile
    '(define (factorial n)
       (define (iter product counter)
         (if (> counter n)
             product
             (iter (* counter product)
                   (+ counter 1))))
         (iter 1 1))
    'val
    'next))

;ex5.35
(print-after-compiler
  (compile
    '(define (f x)
       (+ x (g (+ x 2))))
    'val
    'next))
