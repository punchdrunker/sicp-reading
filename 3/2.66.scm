(load "./helper.scm")
;レコードの集合がキーの数値で順序づけられている二進木で構造化されている場合のlookup手続きを実装せよ

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (key record) (car record))
(define (data record) (cdr record))
(define (make-record key data) (cons key data))

(define database
  (list (make-record 1 'Bill)
        (make-record 2 'Joe)
        (make-record 3 'Frank)
        (make-record 4 'John)))

(trace lookup)
(lookup 4 database)
