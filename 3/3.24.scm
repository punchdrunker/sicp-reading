(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (same-key? key-1 (cdr local-table))))
        (if subtable
          (let ((record (same-key? key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (same-key? key-1 (cdr local-table))))
        (if subtable
          (let ((record (same-key? key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (search key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        ((and (number? key) (< (abs (- key (caar records))) 0.5)) (car records))
        (else (search key (cdr records)))))

(define tb3 (make-table search))
(define insert! (tb3 'insert-proc!))
(define lookup (tb3 'lookup-proc))

(insert! 'a 1 'a1)
(insert! 'a 2 'a2)
(insert! 'b 1 'b1)
(insert! 'b 2 'b2)
(insert! 'b 3 'b3)
(display (lookup 'a 1))
(newline)
(display (lookup 'b 2))
(newline)
(display (lookup 'b 4))
(newline)
(newline)
