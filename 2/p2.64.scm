(use gauche.test)
(load "./2.3.3-3.scm")

; 順序づけられたリストから
; 釣り合った二進木をつくる

; ソート済みリストを受け取り二進木を返す
(define (list->tree elements)
  (car (partial-tree elements (length elements))))


; ソート済みリストとその要素数を受け取り、
; 二進木を返す
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))


