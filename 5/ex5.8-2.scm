; 修正版extract-labels
(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels (cdr text)
                    (lambda (insts labels)
                      (let ((next-inst (car text)))
                        (if (symbol? next-inst)
                          ; labelsの中にnext-instが既にないかチェックしている
                          (if (assoc next-inst labels)
                            (error "ラベルが重複しました: " next-inst)
                            (receive insts
                                     (cons (make-label-entry next-inst
                                                             insts)
                                           labels)))
                          (receive (cons (make-instruction next-inst)
                                         insts)
                                   labels)))))))

