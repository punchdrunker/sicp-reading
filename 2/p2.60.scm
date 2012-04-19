(use gauche.test)
(load "./2.3.3-1.scm")

; 問題2.60

; 集合setにオブジェクトxが含まれているかを調べる
; (変更なし)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; 集合setにオブジェクトxを追加した集合を返す
; (重複チェックをしないようにする)
(define (adjoin-set x set) (cons x set))

; 2つの集合の積集合(どちらの集合にも含まれている要素の集合)を返す
; (変更なし)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


; 2つの集合の和集合(どちらかの集合に含まれている要素全ての集合)を返す
; (重複チェックをしないようにする)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ; set2に(car set1)をaddして(cdr set1)とset2で再帰
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define (union-set set1 set2)
    (append set1 set2))


(trace adjoin-set)
(trace union-set)
(test-start "問題2.60")
(test-section "重複の許される element-of-set?")
(test* "(element-of-set? 'a '(a b c))" true (element-of-set? 'a '(a b c)))
(test* "(element-of-set? 'z '(a b c))" false (element-of-set? 'z '(a b c)))

(test-section "重複の許される adjoin-set")
(test* "(adjoin-set 'z '(a b c))" '(z a b c) (adjoin-set 'z '(a b c)))
(test* "(adjoin-set 'a '(a b c))" '(a a b c) (adjoin-set 'a '(a b c)))

(test-section "重複の許される intersection-set")
(test* "(intersection-set '(a b c) '(b c d))" '(b c) (intersection-set '(a b c) '(b c d)))
(test* "(intersection-set '(a b c) '(hoge fuga))" '() (intersection-set '(a b c) '(hoge fuga)))

(test-section "重複の許される union-set")
(test* "(union-set '(a b c) '(b c d))" '(a b c b c d) (union-set '(a b c) '(b c d)))
(test* "(union-set '() '(b c d))" '(b c d) (union-set '() '(b c d)))
(test* "(union-set '(a b c) '())" '(a b c) (union-set '(a b c) '()))

(test-end)

; 1つ目のunion-set
; union-setだけのtraceだと一見して変わらないように見えるが、
; element-of-set?もtraceしてみると、重複のないadjoin-setとunion-setは
; O(n^2)で増加する事がわかる
; 重複を許すとO(n)で増加する
;
; 2つ目のunion-set
; ただのappendなので、ステップ数はO(1)
