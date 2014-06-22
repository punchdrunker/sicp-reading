; 問題 5.46

; 問題5.45のような解析を行い, 図5.12の特殊目的Fibonacci計算機を使った有効性と比較して,
; 木構造再帰のFibonacci手続き

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))
; の翻訳の, 有効性を決めよ. (解釈する性能の計測には問題5.29を参照せよ.)
; Fibonacciでは使う時間資源はnに線形ではない.
; 従ってスタック演算の比はnと独立な極限値には近づかない. 

(load "./5.5.ss")
(load "./5.5.7.ss")

; 翻訳版
;(compile-and-go
;  '(define (fib n)
;     (if (< n 2)
;       n
;       (+ (fib (- n 1)) (fib (- n 2))))))

; 解釈版
;(start-eceval)
;
;(define (fib n)
;  (if (< n 2)
;    n
;    (+ (fib (- n 1)) (fib (- n 2)))))
;(fib 5)

; 特殊目的
; fact-machine.scm を実行
