(use gauche.test)
(load "./p2.64.scm")

(trace partial-tree)

(test-start "問題2.64")
(test-section "quotient n1 n2 :n1をn2で割った商")
(test* "(quotient 3 2)" 1 (quotient 3 2))
(test* "(quotient 3 2)" 3 (quotient 10 3))

(test-section "list->tree")
(test* "(list->tree '())" '() (list->tree '()))
(test* "(list->tree '(1 3 5 7 9 11))" '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))) (list->tree '(1 3 5 7 9 11)))
(test* "(list->tree '(1 3 5))" '(3 (1 () ()) (5 () ())) (list->tree '(1 3 5)))
(test-end)

; 一応、letについて
; (let (局所変数の定義領域) (局所変数を利用できる領域))
; (let ((var1 exp1) (var2 exp2) ...) (use var1 var2...))

; a.
; リストはソート済みなので、
; 1.中央の要素
; 2.中央の要素の左側のブランチ(値が小さい)
; 3.中央の要素の右側のブランチ(値が大きい)
; の3つに分割して、
; 再帰的にツリーを構築している
;
; 結果
; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

; b.
; 長さ6のリストを二進木に変換するステップ数と
; 長さ3のリストを二進木に変換するステップ数を比較すると、
; 下記の通り、リストの長さに比例するようなので、
; O(n)
; だと思います

; length: 6 => step: 26
;CALL partial-tree (1 3 5 7 9 11) 6
;CALL partial-tree (1 3 5 7 9 11) 2
;CALL partial-tree (1 3 5 7 9 11) 0
;RETN partial-tree (() 1 3 5 7 9 11)
;CALL partial-tree (3 5 7 9 11) 1
;CALL partial-tree (3 5 7 9 11) 0
;RETN partial-tree (() 3 5 7 9 11)
;CALL partial-tree (5 7 9 11) 0
;RETN partial-tree (() 5 7 9 11)
;RETN partial-tree ((3 ...) 5 7 9 11)
;RETN partial-tree ((1 ...) 5 7 9 11)
;CALL partial-tree (7 9 11) 3
;CALL partial-tree (7 9 11) 1
;CALL partial-tree (7 9 11) 0
;RETN partial-tree (() 7 9 11)
;CALL partial-tree (9 11) 0
;RETN partial-tree (() 9 11)
;RETN partial-tree ((7 () ()) 9 11)
;CALL partial-tree (11) 1
;CALL partial-tree (11) 0
;RETN partial-tree (() 11)
;CALL partial-tree () 0
;RETN partial-tree (())
;RETN partial-tree ((11 () ()))
;RETN partial-tree ((9 (7 () ()) (11 () ())))
;RETN partial-tree ((5 (1 () (3 () ())) (9 (7 () ...) (11 () ()))))


; length: 3 => step: 14
;CALL partial-tree (1 3 5) 3
;CALL partial-tree (1 3 5) 1
;CALL partial-tree (1 3 5) 0
;RETN partial-tree (() 1 3 5)
;CALL partial-tree (3 5) 0
;RETN partial-tree (() 3 5)
;RETN partial-tree ((1 () ()) 3 5)
;CALL partial-tree (5) 1
;CALL partial-tree (5) 0
;RETN partial-tree (() 5)
;CALL partial-tree () 0
;RETN partial-tree (())
;RETN partial-tree ((5 () ()))
;RETN partial-tree ((3 (1 () ()) (5 () ())))
