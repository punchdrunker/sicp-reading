(load "./helper.scm")

(define (make-account balance password)
  (define misstype 0)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops) (error "[:-("))
  (define (dispatch pass m)
    (if (eq? pass password)
      (begin 
        (set! misstype 0)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      (begin
        (if (>= misstype 6)
          (call-the-cops)
          (begin
            (set! misstype (+ 1 misstype))
            (error "Incorrect password"))))))
  dispatch)
