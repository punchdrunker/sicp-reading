(load "./eqt.scm")
(load "./p2.69.scm")

(test-start "問題2.69")

(eqt '((leaf A 8) ((leaf C 4) (leaf B 6) (C B) 10) (A C B) 18)
     (generate-huffman-tree '((A 8) (B 6) (C 4))))

(test-end)
