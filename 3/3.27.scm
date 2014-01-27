(load "./helper.scm")
; 1.2.2
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else
          (+ (fib (- n 1))
             (fib (- n 2))))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else 
                     (+ (memo-fib (- n 1))
                        (memo-fib (- n 2)))))))) 

(define memo-fib2 
  (memoize fib))

(trace memo-fib)
(trace fib)

; A1. 
;(print "---------------------------------")
;(print "---(memo-fib 3)")
;(print (memo-fib 3))

; A2. ステップ数がnに比例するのは...
; 1度実行した memo-fib の値はテーブルに記録されるので、再帰呼び出しの内の一方はテーブルの参照で済む。
;(print "---------------------------------")
;(print "---memo-fib (1)")
;(print (memo-fib 5))
;(print "---memo-fib (2)")
;(print (memo-fib 5))
;(print "---memo-fib (3)")
;(print (memo-fib 4))
;(newline)

; A3. 
; 期待する答えは返すが、fibが再帰的に実行されるので、メモ化されない
;(print "---------------------------------")
;(newline)
;(print "---memo-fib2 (1)")
;(print (memo-fib2 5))
;(print "---memo-fib2 (2)")
;(print (memo-fib2 5))
;(print "---memo-fib2 (3)")
;(print (memo-fib2 4))
