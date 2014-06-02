;; 被演算子を左から右へ評価する list-of-values
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first-eval (eval (first-operand exps) env)))
      (cons first-eval
            (list-of-values (rest-operands exps) env)))))

;; 被演算子を右から左へ評価する list-of-values
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first-eval (list-of-values (rest-operands exps) env)))
      (cons (eval (first-operand exps) env)
            first-eval))))


(define val 10)

(define expression '((set! val (+ val 2)) (set! val (* val 2))))

(define (list-of-values-left-to-right exps)
  (if (null? exps)
        '()
              (let ((first-eval (eval (car exps) (interaction-environment))))
                         (cons first-eval
                                          (list-of-values-left-to-right (cdr exps))))))

(display (list-of-values-left-to-right expression))

(define (list-of-values-right-to-left exps)
  (if (null? exps)
        '()
              (let ((first-eval (list-of-values-right-to-left (cdr exps))))
                         (cons (eval (car exps) (interaction-environment))
                                          first-eval))))

(display (list-of-values-right-to-left expression))

;; 被演算子を左から右へ評価する list-of-values
(define (list-of-values exps env)
   (if (no-operands? exps)
     '()
     (let ((first-eval (eval (first-operand exps) env)))
       (cons first-eval
             (list-of-values (rest-operands exps) env)))))

;; 被演算子を右から左へ評価する list-of-values
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first-eval (list-of-values (rest-operands exps) env)))
      (cons (eval (first-operand exps) env)
            first-eval))))
