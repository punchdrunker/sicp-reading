(load "./5.2.4.scm")
(load "./ex5.16.scm")

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace? #f))
    (define (print-reg before-value value) ;; ex5.18
      (if trace?
        (print "reg-name: " name " before: " before-value " set: " value)))
    (define (dispatch message)
      (cond [(eq? message 'get) contents]
            [(eq? message 'set)
             (lambda (value) ;; ex5.18
               (let ((before-val contents))
                 (set! contents value)
                 (print-reg before-val value)))]
            [(eq? message 'trace-on) (set! trace? #t)]
            [(eq? message 'trace-off) (set! trace? #f)]
            [else
              (error "Unknown request -- REGISTER" message)]))
    dispatch))

(define (trace-on-register machine register-name) ;; ex5.18
  ((get-register machine register-name) 'trace-on))

(define (trace-off-register machine register-name) ;; ex5.18
  ((get-register machine register-name) 'trace-off))

(define expt-machine
  (make-machine
    '(b n val continue)
    (list (list '= =) (list '- -) (list '* *))
    '((assign continue (label expt-done))
      expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label return))
      (save continue)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-expt))
      (goto (label expt-loop))
      after-expt
      (restore continue)
      (assign val (op *) (reg b) (reg val))
      (goto (reg continue))
      return
      (assign val (const 1))
      (goto (reg continue))
      expt-done)))

(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 3)
(trace-on expt-machine)
(trace-on-register expt-machine 'val)
(trace-on-register expt-machine 'n)
(start expt-machine)
(get-register-contents expt-machine 'val)
