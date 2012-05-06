(load "./2.3.4-1.scm")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge ordered-pairs)
  (cond ((null? ordered-pairs) ())
        ((null? (cdr ordered-pairs)) (car ordered-pairs))
        (else (successive-merge
                (adjoin-set
                  (make-code-tree (car ordered-pairs)
                                  (cadr ordered-pairs))
                  (cddr ordered-pairs))))))

