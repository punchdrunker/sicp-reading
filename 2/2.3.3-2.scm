(use slib)
(require 'trace)
(load "./bool.scm")

; テーマ
; 集合演算を高速化してみる

; アプローチ
; 要素が大きくなる順にリスト表現を変更する

; 集合setにオブジェクトxが含まれているかを調べる
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

; リストがソート済みだと、element-of-set?を使わなくて良いので、
; O(n)で済む
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))
