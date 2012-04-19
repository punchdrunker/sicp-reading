(use slib)
(require 'trace)
(load "./bool.scm")

; テーマ
; 集合(互いに異なるオブジェクトの集まり)を表現する

; アプローチ
; 集合=どの要素も一度しか現れないリストとして考えてみる
; (集合の順序づけられないリスト表現)

; 集合setにオブジェクトxが含まれているかを調べる
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; 集合setにオブジェクトxを追加した集合を返す
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; 2つの集合の積集合(どちらの集合にも含まれている要素の集合)を返す
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

