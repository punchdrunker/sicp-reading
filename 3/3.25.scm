(load "./helper.scm")

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (lookup-iter key-list local-table))
    (define (lookup-iter key-list local-table)
      (if (null? key-list)
        false
        (let ((subtable (assoc (car key-list) (cdr local-table))))
          (if subtable
            (if (null? (cdr key-list))
              (cdr subtable)
              (lookup-iter (cdr key-list) subtable))
            false ))))
    (define (insert! key-list value)
      (insert-iter! key-list value local-table))
    (define (insert-iter! key-list value local-table)
      (if (null? key-list)
        false
        (let ((subtable (assoc (car key-list) (cdr local-table))))
          (if subtable
            (if (null? (cdr key-list))
              (set-cdr! subtable value)
              (insert-iter! (cdr key-list) value subtable))
            (set-cdr! local-table
                      (cons (insert-iter key-list value)
                            (cdr local-table))))))
      'ok)
    (define (insert-iter key-list value)
      (if (null? (cdr key-list))
        (cons (car key-list) value)
        (list (car key-list) (insert-iter (cdr key-list) value))))
    (define (print-table)
      (display local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-table) print-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define tb (make-table))
(define lookup (tb 'lookup-proc))
(define insert! (tb 'insert-proc!))
(define print-table (tb 'print-table))

(insert! '(a1 b1 c1) 1)
(insert! '(a1 b2 c3) 2)
(insert! '(a2 b3 c1) 3)
(insert! '(a3 b4) 4)
(insert! '(a4) 5)
(display (lookup '(a1 b1 c1)))
(newline)
(display (lookup '(a1 b2 c3)))
(newline)
(display (lookup '(a3 b4)))
(newline)
(display (lookup '(a4)))
(newline)
(display (lookup '(a5)))
(newline)
