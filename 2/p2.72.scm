(load "./p2.69.scm")
(load "./p2.71.scm")

(trace successive-merge)
(trace make-code-tree)
(trace make-leaf-set)

(display (generate-huffman-tree (expt-pair set5)))
(display (generate-huffman-tree (expt-pair set10)))
