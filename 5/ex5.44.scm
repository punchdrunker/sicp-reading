; 問題 5.44
;
; 本節では翻訳時環境を使って文面アドレスを作ることに注目した.
; しかし翻訳時環境には他の使い方もある.
; 例えば問題5.38では翻訳したコードをオープンコード基本手続きによって効率を高めた.
; われわれの実装はオープンコード手続きの名前を予約語として扱った.
; プログラムがそういう名前を再束縛するなら, 問題5.38に述べた機構は,
; 新しい束縛を無視し, それを基本手続きとしてオープンコードのままとする.
; 例えばxとyの線形結合を計算する手続き
;
; (lambda (+ * a b x y)
;   (+ (* a x) (* b y)))
;
; を考えよう.
; 引数+matrix, *matrixと四つの行列を持ってこれを呼び出したいが,
; オープンコード翻訳系は(+ (* a x) (* b y))の+や*を基本手続きの+や*としてオープンコードのままとする.
; オープンコード翻訳系を修正し, 基本手続きの名前を含む式の正しいコードを翻訳するため,
; 翻訳時環境を調べるようにせよ.
; (プログラムがこれらの名前をdefineしたりset!したりしないうちは, コードは正しく働く.) 
;

(load "./ex5.43.scm")

(define (overwrite? operator ct-env)
  (let ((addr (find-variable operator ct-env)))
    (eq? addr 'not-found)))

(define (open-code-operator? exp ct-env)
  (and (memq (car exp) '(+ - * / =))
       (overwrite? (operator exp) ct-env)))

(define (spread-arguments a1 a2 ct-env)
  (let ((ca1 (compile a1 'arg1 'next ct-env))
        (ca2 (compile a2 'arg2 'next ct-env)))
    (list ca1 ca2)))

(define (compile-open-code exp target linkage ct-env)
  (if (= (length exp) 3)
    (let ((op (car exp)) ; 演算子
          (args (spread-arguments (cadr exp) (caddr exp) ct-env))) ; 被演算子をコンパイル
      (end-with-linkage linkage
                        (append-instruction-sequences
                          (car args)
                          (preserving '(arg1)
                                      (cadr args)
                                      (make-instruction-sequence '(arg1 arg2) (list target)
                                                                 `((assign ,target (op ,op) (reg arg1) (reg arg2))))))))
    (error "Require 3 elements -- COMPILE-OPEN-CODE" exp)))


(define (compile exp target linkage ct-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
;        ;; オープンコード判定
        ((open-code-operator? exp ct-env)
         (print exp)
         (compile-open-code exp target linkage ct-env))
        ((variable? exp)
         (compile-variable exp target linkage ct-env))
        ((assignment? exp)
         (compile-assignment exp target linkage ct-env))
        ((definition? exp)
         (compile-definition exp target linkage ct-env))
        ((if? exp) (compile-if exp target linkage ct-env))
        ((lambda? exp) (compile-lambda exp target linkage ct-env))
        ((begin? exp)
         (compile-sequence (begin-action exp)
                           target
                           linkage
                           ct-env))
        ((cond? exp) (compile (cond->if exp) target linkage ct-env))
        ((application? exp)
         (compile-application exp target linkage ct-env))
        (else
          (error "Unknown expression type -- COMPILE" exp))))

;(show-code
;  (compile
;    '(lambda (+ * a b x y)
;       (+ (* a x) (* b y)))
;    'val
;    'next
;    '())
;  )

;(show-code
  (compile
    '(+ (* a x) (* b y))
    'val
    'next
    '())
;  )
