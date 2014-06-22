(load "./5.5.scm")

; 5.5.7 翻訳したコードと評価器のインターフェース
;
;
;
; 翻訳したコードを評価器計算機にロードしたりそれを走らせたりする方法はまだ説明しなかった.
; 積極制御評価器計算機は脚注38で指定した追加の演算を持つ以外は5.4.4節で定義した通りと仮定しよう.
; Scheme式を翻訳し, 結果の目的コードを評価器計算機にロードし, 計算機に評価器の大域環境でコードを走らせ,
; 結果を印字し, 評価器の駆動ループに入る手続き compile-and-goを実装しよう.
; また評価器を修正し, 解釈される式が, 解釈されるものだけでなく, 翻訳したコードも呼び出せるようにしよう.
; そうすれば翻訳した手続きを計算機に置き, それを呼び出すのに評価器を使うことが出来る:

;(compile-and-go
;   '(define (factorial n)
;      (if (= n 1)
;        1
;        (* (factorial (- n 1)) n))))
;;; EC-Eval value:
;> ok

;;; EC-Eval input:
;(factorial 5)
;;; EC-Eval value:
;;> 120

; 評価器に翻訳したコードが扱える(例えば, 上のようにfactorial の呼出しを評価する)ようにするには,
; apply-kdispatch (5.4.1節)にあるコードを変更し,
; (合成手続きや基本手続きとも違う)翻訳した手続きを認識するようにし,
; 制御を翻訳したコードの入り口に直接飛び越すようにしなければならない:48


;apply-dispatch
;(test (op primitive-procedure?) (reg proc))
;(branch (label primitive-apply))
;(test (op compound-procedure?) (reg proc))  
;(branch (label compound-apply))
;(test (op compiled-procedure?) (reg proc))  
;(branch (label compiled-apply))
;(goto (label unknown-procedure-type))
;
;compiled-apply
;(restore continue)
;(assign val (op compiled-procedure-entry) (reg proc))
;(goto (reg val))

; compiled-applyでのcontinueの回復に注意しょう.
; 評価器ではapply-dispatchで継続がスタックの最上段にあるようになっていたことを思い出そう.
; 他方翻訳したコードの入り口は, 継続はcontinueにあると思っている.
; そこで翻訳したコードを実行する前にcontinueを回復しなければならない.
;
; 評価器計算機を起動した時, 翻訳したプログラムが走らせられるよう,
; 評価器計算機の先頭にbranch命令を追加し, flagレジスタが設定してあれば,
; 計算機に新しい入り口へ行かせる.

;(branch (label external-entry))      ; flagが設定してあれば分岐する
;read-eval-print-loop
;(perform (op initialize-stack))
;...

; external-entryは, 結果をvalに置き,
; (goto (reg continue))で終る命令列の場所をvalに入れて計算機を起動すると仮定する.
; この入り口での起動はvalの指示する場所へ飛び越す.
; しかしvalの値を印字し, 評価器の読込み-評価-印字ループの先頭へ行くprint-resultへ実行が戻るよう,
; まずcontinueに代入する.

;external-entry
;(perform (op initialize-stack))
;(assign env (op get-global-environment))
;(assign continue (label print-result))
;(goto (reg val))

; 手続き定義を翻訳し, 翻訳したコードを実行し,
; 読込み-評価-印字ループを走らせる次の手続きが使えるので, 手続きを試すことが出来る.
; 翻訳したコードは, 結果をvalに置き, continueにある場所へ戻って欲しいので,
; 式を標的valと接続returnで翻訳する.
; 翻訳で出来た目的コードを, 評価器レジスタ計算機で実行可能な命令に変換するには
; レジスタ計算機シミュレータ(5.2.2節)の手続きassembleを使う.
; 次にvalレジスタを初期化し, 命令のリストを指すようにし,
; 評価器が external-entryへ行くようflagを設定し, 評価器を起動する.


(define (compile-and-go expression)
  (let ((instructions
          (assemble (statements
                      (compile expression 'val 'return))
                    eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))
; 5.4.4節の最後のようにスタックの監視が設定してあれば,
; 翻訳したコードのスタック使用を調べることが出来る.

;(compile-and-go
;  '(define (factorial n)
;     (if (= n 1)
;       1
;       (* (factorial (- n 1)) n))))

;(total-pushes = 0 maximum-depth = 0)
;;;; EC-Eval value:
;ok
;
;;;; EC-Eval input:
;(factorial 5)
;(total-pushes = 31 maximum-depth = 14)
;;;; EC-Eval value:
;120

; この例を, 5.4.4節の最後に示した同じ手続きの解釈版を使った(factorial 5)の評価と比べよう.
; 解釈版はプッシュ144回と, 最大スタック深さ28を必要とする.
; これはわれわれの翻訳戦略の最適化の結果を示す.


; 解釈と翻訳
;
; 本節のプログラムを使えば, 解釈と翻訳の切替え実行戦略の実験が出来る.
; 解釈系は計算機を利用者のプログラムのレベルへ引き上げ, 翻訳系は利用者のプログラムを機械語レベルへ引き下げる.
; われわれはScheme言語(や他のプログラム言語)を, 機械語の上に作られた, 抽象の整合した一族と見ることが出来る.
; 解釈系は, プログラム実行のステップが, これらの抽象を使って組織化してあるので,
; 対話的プログラム開発と虫取りに優れており, 従ってプログラマにとりずっと理解し易い.
; 翻訳したコードは, プログラム実行のステップが機械語を使って組織化してあるので,
; ずっと高速に実行出来, 翻訳系は高レベルの抽象を切り割く最適化が楽に出来る.
;
; 解釈と翻訳の切替えは, 言語を新しい計算機へ 移植する別の戦略をもたらす.
; Lispを新しい計算機に実装したいとする.
; 戦略の一つは5.4節の積極制御評価器から始め, その命令を新しい計算機の命令へ翻訳する.
; 別の戦略は翻訳系から始め, 新しい計算機のコードを生成するように, コード生成器を変更する.
; この第二の戦略は, 元々のLispシステムで走っている翻訳系で任意のLispプログラムを最初に翻訳し,
; 実行時ライブラリの翻訳版と連結することで, それを新しい計算機で走らせるのを可能とする.
; もっとよいのは翻訳系自身を翻訳し, これを新しい計算機上で他のLispプログラムを翻訳するために走らせるのである.
; あるいは4.1節の解釈系の一つを翻訳し, 新しい計算機の上で走る解釈系を作ることも出来る. 
