(load "./eqt.scm")
(load "./p2.67.scm")

(test-start "問題2.67")

(eqt '(A D A B B C A)
     (decode sample-message sample-tree))

(test-end)
