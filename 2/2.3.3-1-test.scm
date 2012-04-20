(use gauche.test)
(load "./2.3.3-1.scm")

(trace element-of-set?)
;(trace adjoin-set)
(trace intersection-set)

(test-start "2.3.3 例:集合の表現")
(test-section "順序づけられないリストとしての集合")
(test-section "element-of-set?")
(test* "(element-of-set? '1 '(1 2 3))" true (element-of-set? '3 '(1 2 3)))
(test* "(element-of-set? '5 '(1 2 3))" false (element-of-set? 'a '(1 2 3)))

(test-section "adjoin-set")
(test* "(adjoin-set 4 '(1 2 3))" '(4 1 2 3) (adjoin-set '4 '(1 2 3)))
(test* "(adjoin-set 1 '(1 2 3))" '(1 2 3) (adjoin-set 1 '(1 2 3)))

(test-section "intersection-set")
(test* "(intersection-set '(0 1 2 3 4)) '(4 5 6 7 8)" '(4) (intersection-set '(0 1 2 3 4) '(4 5 6 7 8)))
(test-end)
