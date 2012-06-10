(use slib)
(require 'trace)
(load "./bool.scm")
(load "./2.4.1-1.scm")

;;タグを付ける構成子
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

;;直交座標形式か極座標形式か
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;;attach-tagを元に実装し直す
;;関数の名前が衝突するのでその回避がちょっとあれ
;;;;;直交座標形式ベースの実装2
(define (real-part-rectangular z)
  (car z))

(define (imag-part-rectangular z)
  (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a))
                    (* r (sin a)))))

;;;;;;;;;極座標形式ベースの実装2
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z)
  (car z))

(define (angle-polar z)
  (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
              (cons r a)))


;;汎用的な手続きを実装
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))


;;コンストラクタでどちらの型を使うか決定
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;;
;;テスト
;;四則演算の実装は変更せずに直交座標形式と極座標形式が切り替わる
;;

(add-complex (make-from-real-imag 100 40)
             (make-from-real-imag 30 10))
;;CALL add-complex (rec...lar 100 . 40) (rectangular 30 . 10)
;;RETN add-complex (rectangular 130 . 50)
;;(rectangular 130 . 50)

(sub-complex (make-from-mag-ang 110 0.4)
             (make-from-real-imag 30 10))
;;CALL sub-complex (polar 110 . 0.4) (rectangular 30 . 10)
;;RETN sub-complex (rectangular 71.31670934031736 . 32.83601765395156)
;;(rectangular 71.31670934031736 . 32.83601765395156)

(mul-complex (make-from-mag-ang 110 0.4)
             (make-from-mag-ang 20  0.4))
;;CALL mul-complex (polar 110 . 0.4) (polar 20 . 0.4)
;;RETN mul-complex (polar 2200 . 0.8)
;;(polar 2200 . 0.8)

(div-complex (make-from-mag-ang 110 0.4)
             (make-from-real-imag 30 10))
;;CALL div-complex (polar 110 . 0.4) (rectangular 30 . 10)
;;RETN div-complex (polar 3.4785054261852175 . 0.07824944560335784)
;;(polar 3.4785054261852175 . 0.07824944560335784)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;名前空間の衝突をスマートに回避、汎用手続きをより抽象的にできるように
;;put / getの実装、とりあえず
(define complex-number-operater (make-hash-table))

(define (put op key item)
  (let ((keys (string->symbol (string-append (symbol->string op)
                                             (symbol->string (car key))))))
    (hash-table-put! complex-number-operater keys item)))

(define (get op key)
  (let ((keys (string->symbol (string-append (symbol->string op)
                                             (symbol->string (car key))))))
    (hash-table-get complex-number-operater keys)))

(put 'hello '(World) 123123)
(get 'hello '(World))

;;直交座標形式 package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z)
    (car z))
  (define (imag-part z)
    (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;極座標形式 package
(define (install-polar-package)
  ;; internal procedures  (define (magnitude z) (car z))
  (define (angle z)
    (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))

  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;keyから手続きを探し適用する
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))

(define (real-part z)
  (apply-generic 'real-part z))

(define (imag-part z)
  (apply-generic 'imag-part z))

(define (magnitude z)
  (apply-generic 'magnitude z))

(define (angle z)
  (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
