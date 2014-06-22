; 問題 5.41
; 引数として変数と翻訳時環境をとり, その環境に対する変数の文面アドレスを返す手続きfind-variableを書け.
; 例えば上に示したプログラムの断片では, 式⟨e1⟩翻訳中の翻訳時環境は((y z) (a b c d e) (x y))である.
; find-variableは次のように出さなければならない.


(define (find-variable variable env) 
  ; 2階層目の探索
  (define (search-variable v l n) 
    (cond ((null? l) #f) 
          ((eq? v (car l)) n) 
          (else (search-variable v (cdr l) (+ n 1))))) 
  ; 1階層目の探索
  (define (search-frame frames f) 
    (if (null? frames) 
      'not-found 
      (let ((o (search-variable variable (car frames) 0))) 
        (if o 
        ;; 何番目のフレームの何番目の要素かを返す
          (cons f o) 
          (search-frame (cdr frames) (+ f 1)))))) 
  (search-frame env 0)) 

;(display (find-variable 'c '((y z) (a b c d e) (x y))))
;(newline)
;(display (find-variable 'x '((y z) (a b c d e) (x y))))
;(newline)
;(display (find-variable 'w '((y z) (a b c d e) (x y))))
