(load "./p2.63.scm")

;(trace tree->list-1)
;(trace tree->list-2)
(define mytree
  (make-tree 4
             (make-tree 2
                        (make-tree 1 '() '())
                        (make-tree 3 '() '()))
             (make-tree 6
                        (make-tree 5 '() '())
                        (make-tree 7 '() '()))))

(test-start "問題2.63")
(test-section "tree->list-1")
(test* "(tree->list-1 mytree)" '(1 2 3 4 5 6 7) (tree->list-1 mytree))
(test-section "tree->list-2")
(test* "(tree->list-2 mytree)" '(1 2 3 4 5 6 7) (tree->list-2 mytree))
(test-end)

; a.
; 再帰と反復で異なるだけで、出力は同じだと思います
;
; b.
; あまり変わらないように見えるが、
; appendがある分、tree->list1の方がステップ数が増えるのでは?

