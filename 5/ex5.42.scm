; 問題 5.42
;
; 問題5.41のfind-variableを使い,
; compile-variableとcompile-assignmentを書き直して文面アドレス命令を出力するようにせよ.
;
; find-variableがnot-foundを返す(つまり, その変数が翻訳時環境にない)場合は,
; コード生成器に以前のように束縛を探すため評価演算を使わせなければならない.
; (翻訳時に見つからない変数が出てくる唯一の場所は大域環境, つまり実行時環境の一部だが,
; 翻訳時環境の一部ではないところである.47 従って, 望むなら, envで見つかる全実行時環境を探させる代りに,
; 評価演算に演算(op~get-global-environment)で得られる大域環境を直接見させてもよい.)
;
; 修正した翻訳系を, 本節の最初の入れ子のlambda 組合せのようないくつかの単純な場合でテストせよ. 

(load "./ex5.40.scm")
(load "./ex5.41.scm")

; ct-envを受け取る
(define (compile-variable exp target linkage ct-env)
  (let ((address (find-variable exp ct-env)))
    ;(print "compile-variable: " address)
    (end-with-linkage
      linkage
      (make-instruction-sequence
        '(env) (list target)
        (if (eq? address 'not-found)
          `((assign ,target
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg env)))
          `((assign ,target
                    (op lexical-variable-value)
                    (const ,address)
                    (reg env))))))))

; ct-envを受け取る
(define (compile-assignment exp target linkage ct-env)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next ct-env)))
    (let ((address (find-variable exp ct-env)))
    ;(print "compile-assignment: " address)
      (end-with-linkage
        linkage
        (preserving
          '(env)
          get-value-code
          (make-instruction-sequence
            '(env val) (list target)
            (if (eq? address 'not-found)
              `((perform (op set-variable-value!)
                         (const ,var)
                         (reg val)
                         (reg env))
                (assign ,target (const ok)))
              `((perform (op lexical-address-set!)
                         (const ,address)
                         (reg val)
                         (reg env))
                (assign ,target (const ok))))
            ))))))

; ct-envを受け取る
(define (compile exp target linkage ct-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
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
;    '(lambda (x y)
;       (lambda (a b)
;         (+
;           (+ x a)
;           (* y b)
;           (set! x a)
;           (set! z b))))
;    'val
;    'next
;    '())
;  )
