(use gauche.test)
(load "./2.3.3-1.scm")

; 問題2.59

; 2つの集合の和集合(どちらかの集合に含まれている要素全ての集合)を返す
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ; (car set1)がset2に含まれていれば(cdr set1)とset2で再帰
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        ; (car set1)がset2に含まれていなければset2に(car set1)をaddして(cdr set1)とset2で再帰
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;(trace element-of-set?)
;(trace union-set)
(test-start "問題2.59")
(test-section " union-setを実装せよ")
(test* "(union-set '(a b c) '(b c d))" '(a b c d) (union-set '(a b c) '(b c d)))
(test* "(union-set '() '(b c d))" '(b c d) (union-set '() '(b c d)))
(test* "(union-set '(a b c) '())" '(a b c) (union-set '(a b c) '()))
(test-end)
