; 手続きmake-new-machineをまず呼び出す.
; make-new-machineで構成される基本的計算機モデルは,
; 本質的にいくつかのレジスタとスタックの容器と,
; 制御器の命令を一つずつ処理する実行機構である.
; 次にmake-machineはこの基本モデルを, (メッセージを送ることで)定義しようとする特定の計算機のレジスタ,
; 演算および制御器を含むように拡張する.
; まず渡されたレジスタ名のそれぞれに新しい計算機のレジスタを割り当て,
; 指示された演算を計算機に組み込む. 
; 次に(次の5.2.2節に述べる) アセンブラ(assembler)を使い,
; 制御器のリストを新しい計算機の命令に変換し,
; これを計算機の命令列として組み込む.
; make-machineは, 修正した計算機モデルをその値として返す.
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

; レジスタは, 局所状態を持つ手続きとして表す.
; 手続きmake-registerはアクセスしたり, 変更したり出来る値を保持するレジスタを作り出す:
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
              (error "Unknown request -- REGISTER" message))))
    dispatch))

; getter
(define (get-contents register)
  (register 'get))

; setter
(define (set-contents! register value)
  ((register 'set) value))

;スタックも局所状態を持つ手続きとして表す.
;手続きmake-stackはその局所状態が, スタックの項目のリストからなるスタックを作り出す.
;スタックは項目をスタックにpushし, 最上の項目をスタックから外して返してpopし, またスタックを空にinitializeする要求を受け入れる:
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
        (error "Empty stack -- POP")
        (let ((top (car s)))
          (set! s (cdr s))
          top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

; make-new-machine
; 以下の4つを構成する.
; 1.スタック
; 2.最初は空の命令列
; 3.最初はスタックを初期化する命令を含む演算のリスト
; 4.最初は flagとpc (「program counter」の意)という名の二つのレジスタを含む
;   レジスタ表 (register table)をその局所状態とするオブジェクト
;
; allocate-register : レジスタ表に新しい項目を追加
; lookup-register : 表中のレジスタを探す
; flagレジスタ : シミュレートされる計算機で分岐を制御するのに使う.
; test命令 : flagの内容をテストの結果(真または偽)に設定する.
; branch命令 : flagの内容を調べ, 分岐する, しないを決定する.
; pcレジスタ : 計算機が走る時の命令の進行を制御する. この進行は内部手続きexecuteが実装する.
;
; シミュレーションモデルでは, 各機械命令は, 命令実行手続き(instruction execution procedure)という,
; その手続きの呼出しがその命令の実行をシミュレートすることになる, 引数のない手続きを含むデータ構造である.
; シミュレーションが進むと, pcは実行すべき次の命令から始る命令列の場所を指す.
; executeは, その命令をとり, その命令実行手続きを呼び出してそれを実行し, このサイクルを実行する命令がなくなる(つまりpcが命令列の終りを指す)まで繰り返す. 
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
            (list
              (list 'initialize-stack
                    (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              ((instruction-execution-proc (car insts)))
              (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

; 各命令実行手続きは実行すべき次の命令を示すため, 演算の一部としてpcを修正する.
; branchとgoto命令は新しい行き先を指すようにpcを変更する.
; 他の命令はすべて列の中の次の命令を指すように単にpcを進める.
; executeの各呼出しがもう一度executeを呼び出すことに注意しよう.
; しかし命令実行手続きの実行はpcの内容を変更するので, 無限ループになることはない.
;
; make-new-machineは内部状態へのメッセージパッシングアクセスを実装するdispatch手続きを返す.
; 計算機の実行開始はpcを命令列の先頭に設定し, executeを呼び出すことで実現する.

; 便宜のため, 計算機のstart演算の手続きインターフェースをもう一つ用意する.
(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))


; 5.2.2
; アセンブラは, ある計算機の制御器の式の列を,
; それぞれが実行手続きを持つ対応する機械命令のリストに変換する.
; ---入力言語があり(今の場合はレジスタ計算機言語), 言語の式の型のそれぞれに適切な行動をとらなければならない.
; 各命令に実行手続きを作り出す技法は, 4.1.7節で解析を実行時の処理から分離して速度をあげるのに使ったのと同じである.
; 4章で見たように, Scheme式の有用な解析の多くは, 変数の実際の値の知識なしに行える. ここでも同様で,
; レジスタ計算機言語の式の有用な解析の多くは, 計算機レジスタの実際の内容の知識なしに行える.
; 例えばレジスタの参照を, レジスタオブジェクトへのポインタで取り替え,
; またラベルの参照を, ラベルの指示する命令列の中の場所へのポインタで取り替えることが出来る.
;
; 命令実行手続きを生成する前に, アセンブラはすべてのラベルが何を参照するか知らなければならない.
; そこでラベルを命令から分離するため, 制御器の文書の走査から始める.
; 文書を走査しながら, 命令のリストと, 各ラベルをリストの中へのポインタと対応づける表を構成する.
; 次にアセンブラは, 各命令の実行手続きを挿入して, 命令リストを拡張する.
;
; assemble手続きはアセンブラへの主要な入り口である.
; 引数として制御器の文書と計算機のモデルをとり, モデルに格納すべき命令列を返す.
; assembleはextract-labelsを呼び出し, 渡された制御器文書から, 最初の命令リストとラベル表を構築する.
; extract-labelsの第二引数は, これらの結果を処理するのに呼び出す手続きである:
;     この手続きはupdate-insts!を使い, 命令実行手続きを生成し,
;     それらを命令リストに挿入して, 修正したリストを返す.
;
;

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

; extract-labelsは引数としてリストtext(制御器の命令の式の列)と, receive手続きをとる.
; receiveは二つの値: (1)それぞれがtextの命令を含んでいる命令のデータ構造のリストinstsと(2)textの各ラベルを, リストinsts内のラベルが指示している位置と対応づけるlabelsという表で呼び出される.

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels (cdr text)
                    (lambda (insts labels)
                      (let ((next-inst (car text)))
                        (if (symbol? next-inst)
                          (receive insts
                                   (cons (make-label-entry next-inst
                                                           insts)
                                         labels))
                          (receive (cons (make-instruction next-inst)
                                         insts)
                                   labels)))))))

; extract-labelsはtextの要素を順に走査し, instsとlabelsを蓄積する.
; 要素が記号(つまりラベル)の時は, 適切な入り口を labels表に追加する.
; それ以外の要素はinstsリストに蓄積する

; update-insts!は, 最初命令の文書を持っていただけの命令リストを, 対応する実行手続きを含むように修正する:
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
      (lambda (inst)
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst) labels machine
            pc flag stack ops)))
      insts)))

; 機械命令データ構造は, 命令文書を対応する実行手続きと対にするだけである.
; 実行手続きは, extract-labelsが命令を構成した時にはまだ使用可能ではなく, 後にupdate-insts!によって挿入される.
(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

; われわれのシミュレータは命令文書を使わない.
; しかし虫とりのためには持ち回るのが便利である(問題5.16参照).

; ラベル表の要素は対である:
(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
      (cdr val)
      (error "Undefined label -- ASSEMBLE" label-name))))


; 5.2.3
; アセンブラは命令の実行手続きを生成すべくmake-execution-procedureを呼び出す.
; 4.1.7節の評価器のanalyze手続きと同様に, これは適切な実行手続きを生成するため, 命令の型に従って振分けを行う.
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

; レジスタ計算機言語の命令の型ごとに, 適切な実行手続きを構築する生成プログラムがある.
; これらの手続きの細部が, レジスタ計算機言語での各命令の構文と意味を決める.
; 4.1.2節の評価器でやったように, レジスタ計算機の式の細い構文を一般の実行機構から分離するため,
; データ抽象を使い構文手続きで命令の部分を取り出し, 分類する.

;; assign 命令
;; make-assign手続きはassign命令を扱う:
; make-assignは選択子 assign-reg-name, assign-value-exp を使い,
; assign命令から目標のレジスタ名(命令の第二要素)と値の式(命令を形成する式のリストの残り)を取り出す.
; レジスタ名は, 目標のレジスタオブジェクトを作るべくget-registerで探す.
; 値の式は値が演算の結果ならmake-operation-expに,
; それ以外なら make-primitive-expに渡す.
; (次に示す)これらの手続きは, 値の式を構文解析し, その値の実行手続きを作る.
; これは value-procという引数なしの手続きで,
; シミュレーション中にレジスタに代入する実際の値を得るために評価される.
; レジスタ名の探索と, 値の式の構文解析は, 命令がシミュレートされる度ではなく,
; アセンブリ時に一回だけ行うことに注意しよう.
; この省力は, 実行手続きを使う理由であり, 4.1.7節の評価器で,
; プログラム解析を実行から分離して得た省力と直接に対応する.
; make-assignが返す結果は, assign命令の実行手続きである.
; この手続きを(計算機モデルのexecute手続きで)呼び出すと,
; 目標のレジスタの内容を, value-procを評価して得られる結果に設定する.
; それからadvance-pcを走らせ, pcを次の命令へと進める.
; advance-pcはbranch とgotoを除き, すべての命令の通常の終了である.
;
(define (make-assign inst machine labels operations pc)
  (let ((target
          (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
            (if (operation-exp? value-exp)
              (make-operation-exp
                value-exp machine labels operations)
              (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda () ; assign の実行手続き
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; test命令
;; make-testはtest命令を同様に扱う.
;; テストすべき条件を規定する式を抜き出し, そのための実行手続きを生成する.
;; シミュレーション時には, 条件に対する手続きが呼び出され,
;; 結果がflagレジスタに代入され, pcが進められる:
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
      (let ((condition-proc
              (make-operation-exp
                condition machine labels operations)))
        (lambda ()
          (set-contents! flag (condition-proc))
          (advance-pc pc)))
      (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;; branch命令に対する実行手続きは, flagレジスタの内容を調べ,
;; pcの内容を(分岐する時)分岐目的地へ設定するか(分岐しない時) pcを進めるかする.
;; branch命令で指示される行き先はラベルでなければならず, make-branchはこれを強要することに注意しよう.
;; またラベルはアセンブリ時に探すので, branch命令をシミュレートする度でないことにも注意しよう.
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
      (let ((insts
              (lookup-label labels (label-exp-label dest))))
        (lambda ()
          (if (get-contents flag)
            (set-contents! pc insts)
            (advance-pc pc))))
      (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;; goto命令は, 行き先がラベルとしてかレジスタとして指定出来る点と,
;; 調べるべき条件がない---pcは常に新しい行き先へ設定する---点を除き, 分岐命令と似ている.
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                   (lookup-label labels
                                 (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                   (get-register machine
                                 (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; その他の命令
;; スタック命令saveとrestoreは
;; 指示されたレジスタでスタックを使い, pcを進めるだけである:
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

; make-performが扱う命令の最後の型は, 実行すべき働きに対して実行手続きを生成する.
; シミュレーション時に働きの手続きが実行され, pcが進められる:
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
      (let ((action-proc
              (make-operation-exp
                action machine labels operations)))
        (lambda ()
          (action-proc)
          (advance-pc pc)))
      (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

;; 部分式の実行手続き
;; reg, labelまたはconst式の値は,
;; レジスタへの代入(make-assign)や演算(次のmake-operation-exp)への入力で必要になる.
;; 次の手続きはシミュレーション時にこれらの式の値を作る実行手続きを生成する:
;; reg, labelおよびconst式の構文は↓の5つで決まる
;; register-exp?  register-exp-reg constant-exp?  constant-exp-value label-exp?

(define (label-exp-label exp) (cadr exp))
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                 (lookup-label labels
                               (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
          (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

; assign, performおよびtest命令は,
; (regおよびconst式の規定する)いくつかの被演算子に対する(op式の規定する)機械演算の作用を含むかも知れない.
; 次の手続きは「演算式」---演算と被演算子の式を含むリスト---に対する実行手続きを命令から作る.
;
; 演算式の構文は, operation-exp?, operation-exp-op, operation-exp-operands, で決る.
; 演算式の扱いは, 各被演算子について実行手続きを生成するという点で,
; 4.1.7節の評価器のanalyze-application手続きによる手続き作用の扱いと非常によく似ていることを見よう.
; シミュレーション時には, 被演算子手続きを呼び出し, その結果の値へ演算をシミュレートするScheme手続きを作用させる.
; シミュレーション手続きは, 計算機の演算表から演算名で探して見つける:
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
          (map (lambda (e)
                 (make-primitive-exp e machine labels))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
      (cadr val)
      (error "Unknown operation -- ASSEMBLE" symbol))))


;; 自分で追加したもの
(define (show-register-contents machine register-name)
  (display (string-append (symbol->string register-name) " : "))
  (display (get-contents (get-register machine register-name)))
  (newline)
  )
