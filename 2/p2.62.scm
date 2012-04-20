(use gauche.test)
(load "./2.3.3-2.scm")


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                      ((< x2 x1) (cons x2 (union-set set1 (cdr set2)))))))))

(trace union-set)
(test-start "問題2.62")
(test-section "ソートされたリストの union-set")
(test* "(adjoin-set '(1 2 3) '(2 3 4))" '(1 2 3 4) (union-set '(1 2 3) '(2 3 4)))
(test-end)
