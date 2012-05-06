(load "./eqt.scm")
(load "./p2.67.scm")

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (enc-iter tree)
    (if (leaf? tree)
      '()
      (if (memq symbol (symbols (left-branch tree)))
        (cons 0 (enc-iter (left-branch tree)))
        (cons 1 (enc-iter (right-branch tree))))))
  (if (memq symbol (symbols tree))
    (enc-iter tree)
    (error "Not Found symbol of " symbol)))

(define sample-decoded-message '(A D A B B C A))

(define answer (encode sample-decoded-message sample-tree))


(test-start "問題2.68")
(eqt sample-message
     answer)   

(eqt '(A D A B B C A)
     (decode sample-message sample-tree))
(test-end)
