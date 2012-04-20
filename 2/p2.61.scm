(use gauche.test)
(load "./2.3.3-2.scm")

; 集合setにオブジェクトxを追加した集合を返す
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(trace adjoin-set)
(test-start "問題2.61")
(test-section "ソートされたリストの adjoin-set")
(test* "(adjoin-set 1 '(1 2 3))" '(1 2 3) (adjoin-set 1 '(1 2 3)))
(test* "(adjoin-set 5 '(1 2 3))" '(1 2 3 5) (adjoin-set 5 '(1 2 3)))
(test* "(adjoin-set 9 '(1 2 3 4 5 6 7))" '(1 2 3 4 5 6 7 9) (adjoin-set 9 '(1 2 3 4 5 6 7)))
(test-end)
