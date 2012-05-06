(load "./eqt.scm")
(load "./2.3.4-1.scm")

(test-start "2.3.4 例:Huffman符号化木")

(test-section "huffman木の表現")

(eqt '(leaf a 8)
     (make-leaf 'a 8))

(eqt true
     (leaf? (make-leaf 'a 8)))

(eqt 'a
     (symbol-leaf (make-leaf 'a 8)))

(eqt 8
     (weight-leaf (make-leaf 'a 8)))

(eqt '((leaf a 8) (leaf b 6) (a b) 14)
     (make-code-tree (make-leaf 'a 8) (make-leaf 'b 6)))

(eqt '(leaf a 8)
     (left-branch (make-code-tree (make-leaf 'a 8) (make-leaf 'b 6))))

(eqt '(leaf b 6)
     (right-branch (make-code-tree (make-leaf 'a 8) (make-leaf 'b 6))))

(eqt '(a b)
     (symbols (make-code-tree (make-leaf 'a 8) (make-leaf 'b 6))))

(eqt 14
     (weight (make-code-tree (make-leaf 'a 8) (make-leaf 'b 6))))

(test-section "復号化")

(eqt '(a b)
 (decode '(0 1) (make-code-tree (make-leaf 'a 8) (make-leaf 'b 6))))

(eqt '(leaf a 8)
 (choose-branch 0 (make-code-tree (make-leaf 'a 8) (make-leaf 'b 6))))

(test-section "重みつき要素の集合")

(eqt '((leaf c 4) (leaf a 8) (leaf b 6) (a b) 14)
 (adjoin-set '(leaf c 4) (make-code-tree (make-leaf 'a 8) (make-leaf 'b 6))))

(eqt '((leaf c 4) (leaf b 6) (leaf a 8))
 (make-leaf-set '((a 8) (b 6) (c 4))))

(test-end)
