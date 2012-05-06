(use gauche.test)
(load "./p2.62.scm")
(load "./p2.63.scm")
(load "./p2.64.scm")

(define (union-set-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
          (list2 (tree->list-2 set2)))
      (list->tree (union-set list1 list2))))

(define (intersection-set-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
          (list2 (tree->list-2 set2)))
      (list->tree (intersection-set list1 list2))))


(define mytree1
  (make-tree 4
             (make-tree 2
                        (make-tree 1 '() '())
                        (make-tree 3 '() '()))
             (make-tree 6
                        (make-tree 5 '() '())
                        (make-tree 7 '() '()))))

(define mytree2
  (make-tree 7
             (make-tree 5
                        (make-tree 4 '() '())
                        (make-tree 6 '() '()))
             (make-tree 9
                        (make-tree 8 '() '())
                        (make-tree 10 '() '()))))

;(trace union-set-tree)
;(trace intersection-set-tree)

(test-start "問題2.65")
(test-section "union-set ")
(test* "(union-set-tree mytree1 mytree2)" '(5 (2 (1 () ()) (3 () (4 () ()))) (8 (6 () (7 () ())) (9 () (10 () ())))) (union-set-tree mytree1 mytree2))
;
;        5
;   2        8
; 1   3    6   9
;       4    7   10

(test-section "intersection-set-tree ")
(test* "(intersection-set-tree mytree1 mytree2)" '(5 (4 () ()) (6 () (7 () ()))) (intersection-set-tree mytree1 mytree2))
;    5
;  4   6
;       7

(test-end)
