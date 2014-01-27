(load "./helper.scm")
(define (make-table)
  (let ((local-table '*table*))
    (define (key-tree tree)
      (car tree))
    (define (value-tree tree)
      (cadr tree))
    (define (left-branch tree)
      (caddr tree))
    (define (right-branch tree)
      (cadddr tree))
    (define (make-tree key value left right)
      (list key value left right))
    (define (set-value-tree! tree value)
      (set-car! (cdr tree) value))
    (define (set-left-branch-tree! tree left)
      (set-car! (cddr tree) left))
    (define (set-right-branch-tree! tree right)
      (set-car! (cdddr tree) right))

    (define (lookup key)
      (define (iter key tree)
        (cond ((null? key) #f)
              ((= key (key-tree tree)) (value-tree tree))
              ((< key (key-tree tree))
               (iter key (left-branch tree)))
              ((> key (key-tree tree))
               (iter key (right-branch tree)))))
      (trace iter)
      (iter key local-table))

    (define (insert! key value)
      (define (make-branch key value)
        (make-tree key value '() '()))
      (define (iter key value tree)
        (cond ((eq? tree '*table*)
               (set! local-table (make-branch key value)))
              ((= key (key-tree tree))
               (set-value-tree! tree value))
              ((< key (key-tree tree))
               (if (null? (left-branch tree))
                 (set-left-branch-tree! tree (make-branch key value))
                 (iter key value (left-branch tree))))
              ((> key (key-tree tree))
               (if (null? (right-branch tree))
                 (set-right-branch-tree! tree (make-branch key value))
                 (iter key value (right-branch tree))))))
      (iter key value local-table)
      'ok)

    (define (print-table)
      (display local-table)
      (newline))

    (define (dispatch m)
      (cond ((eq? m 'print-table) print-table)
            ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation TABLE" m))))
    dispatch))

(define tb (make-table))
(define lookup (tb 'lookup-proc))
(define insert! (tb 'insert-proc!))
(define print-table (tb 'print-table))

(insert! '1 'Bill)
(insert! '2 'Joe)
(insert! '3 'Frank)
(insert! '4 'John)

(print-table)

(trace lookup)

(lookup '4)

(newline)
