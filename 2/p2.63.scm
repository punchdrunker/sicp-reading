(use gauche.test)
(load "./2.3.3-3.scm")

; 二進木からリストをつくる

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (trace copy-to-list)
  (copy-to-list tree '()))



(trace tree->list-1)
(trace tree->list-2)
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
; b.

