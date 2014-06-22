(load "./5.2.4.scm")
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-count 0) ;; exercise 5.15
        (trace #f)) ;; exercise 5.16
    (let ((the-operations
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply define register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register: " name))))
      (define (execute)
        (let ((instructions (get-contents pc)))
          (if (null? instructions)
            'done
            (begin
              (if trace
                (print "instruction: "(caar instructions))) ;; exercise 5.16
              ((instruction-execution-proc (car instructions)))
              (instruction-count-up) ;; exercise 5.15
              (execute)))))
      (define (print-stack-statistics) ;; exercise 5.14
        (stack 'print-statistics))
      (define (instruction-count-up) ;; exercise 5.15
        (set! instruction-count (+ instruction-count 1)))
      (define (print-inst-count) ;; exercise 5.15
        (print "instruction-count: "instruction-count)
        (set! instruction-count 0))
      (define (trace-on) ;; exercise 5.16
        (set! trace #t))
      (define (trace-off) ;; exercise 5.16
        (set! trace #f))
      (define (dispatch message)
        (cond [(eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute)]
              [(eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq))]
              [(eq? message 'allocate-register) allocate-register]
              [(eq? message 'get-register) lookup-register]
              [(eq? message 'install-operations)
               (lambda (operations) (set! the-operations
                                      (append the-operations operations)))]
              [(eq? message 'stack) stack]
              [(eq? message 'print-stack) (print-stack-statistics)] ;; exercise 5.14
              [(eq? message 'inst-count-init) (instruction-count-init)] ;; exercise 5.15
              [(eq? message 'print-inst-count) (print-inst-count)] ;; exercise 5.15
              [(eq? message 'trace-on) (trace-on)] ;; exercise 5.16
              [(eq? message 'trace-off) (trace-off)] ;; exercise 5.16
              [(eq? message 'operations) the-operations]
              [else
                (error "Unknown request -- MACHINE" message)]))
      dispatch)))

(define (trace-on machine) ;; exercise 5.16
  (machine 'trace-on))

(define (trace-off machine) ;; exercise 5.16
  (machine 'trace-off))
