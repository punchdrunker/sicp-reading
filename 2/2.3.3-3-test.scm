(use gauche.test)
(load "./2.3.3-3.scm")

;(trace right-branch)
;(trace left-branch)
;(trace element-of-set?)
;(trace adjoin-set)

(define mytree
  (make-tree 4
             (make-tree 2
                        (make-tree 1 '() '())
                        (make-tree 3 '() '()))
             (make-tree 6
                        (make-tree 5 '() '())
                        (make-tree 7 '() '()))))
(print 'mytree:)
(print mytree)

(test-start "2.3.3 例:集合の表現")
(test-section "二進木としての集合")
(test-section "entry")
(test* "(entry '(1 2 3))" 1 (entry '(1 2 3)))

(test-section "left-branch")
(test* "(left-branch '(1 2 3))" 2 (left-branch '(1 2 3)))

(test-section "right-branch")
(test* "(right-branch '(1 2 3))" 3 (right-branch '(1 2 3)))

(test-section "make-tree")
(test* "(make-tree 1 2 3)" '(1 2 3) (make-tree 1 2 3))

(test-section "element-of-set?")
(test* "(element-of-set? 1 mytree)" true (element-of-set? 1 mytree))
(test* "(element-of-set? 9 mytree)" false (element-of-set? 9 mytree))

(test-section "adjoin-set")
(test* "(adjoin-set 9 mytree)" '(4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () (9 () ())))) (adjoin-set 9 mytree))
(test-end)

