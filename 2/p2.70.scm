(load "./p2.68.scm")
(load "./p2.69.scm")

(define sample-pairs '((A 2)
                       (NA 16)
                       (BOOM 1)
                       (SHA 3)
                       (GET 2)
                       (YIP 9)
                       (JOB 2)
                       (WAH 1)))

(define tree (generate-huffman-tree sample-pairs))

(define song '(GET A JOB
                   SHA NA NA NA NA NA NA NA NA
                   GET A JOB
                   SHA NA NA NA NA NA NA NA NA
                   WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                   SHA BOOM))


(test-start "問題2.70")

(eqt '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
     (encode song tree))

(test-end)
