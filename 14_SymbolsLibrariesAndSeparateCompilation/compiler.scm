(import (srfi 1))
(import (rnrs hashtables))

(load "test_cases.scm")

;;;; ユーティリティ関数

;;; アセンブリ・プログラム(アセンブリ表現)出力用手続き。書式と引数を取って表示し、改行を付け加えます。
;; 例: (emit "addi t0, t0, ~s" 1)
;; addi t0, t0, 1
;; が表示されます。
;; @param args
(define (emit . args)
  (apply format #t args)
  (newline))

;;; ユニーク・ラベル生成
;; 重複のない、ラベルを返します。
(define unique-label
  (let ((count 0))
    (lambda ()
      (let ((L (string->symbol (format "L_~s" count))))
	(set! count (+ count 1))
	L))))

;;; ユニーク・ラベルのリストを生成します。
;; @param vars ラベルの一部を形成する文字のリスト
(define (unique-labels vars)
  (map (lambda (var)
	 (format "~a_~s" (unique-label) var))
       vars))

;;; ユニーク変数生成
;; 重複のない、変数名を返します。
(define unique-name
  (let ((counts '()))  ; 既に出現した名前と、出現回数の連想リスト。例: ((x . 1) (a . 3) (cnt . 2))
    (lambda (name)
      (let ((entry (assv name counts)))
	(cond
	 (entry
	  (let* ((count (cdr entry))
		 (new-name (string->symbol (format "~s_~s" name count))))
	    (set-cdr! entry (+ count 1))
	    new-name))
	 (else
	  (set! counts (cons (cons name 1) counts))
	  name))))))

;;; 値比較のアセンブリ表現を出力します。
;;; a0と値を比較し、booleanオブジェクトをa0に設定します。
;; @param args 比較する即値、省略時はt0と比較
(define (emit-cmp-bool . args)
  (if (null? args)
      (emit "	sub a0, a0, t0")
      (emit "	addi a0, a0, ~s" (- (car args))))
  (emit "	seqz a0, a0")
  (emit "	slli a0, a0, ~s" bool_bit)
  (emit "	ori  a0, a0, ~s" bool_f))

;;; 式の書式判定ユーティリティ。
;; exprが(tag ....) の形式かどうかを判定します。
;; @param tag 判定したいタグ
;; @param 判定されるS式
(define (tagged-form? tag expr)
  (and (list? expr) (not (null? expr)) (eq? (car expr) tag)))

;;;; スタック関連

;;; a0レジスタの値をスタックに保存するアセンブリ表現を出力します。
;; @param si 保存先のスタック・インデックス
(define (emit-stack-save si)
  (emit "	sw a0, ~s(sp)" si))

;;; t0レジスタの値をスタックに保存するアセンブリ表現を出力します。
;; @param si 保存先のスタック・インデックス
(define (emit-stack-save-t0 si)
  (emit "	sw t0, ~s(sp)" si))

;;; a0レジスタにスタックの値を読み込むアセンブリ表現を出力します。
;; @param si 読み込み先のスタック・インデックス
(define (emit-stack-load si)
  (emit "	lw a0, ~s(sp)" si))

;;; t0レジスタにスタックの値を読み込むアセンブリ表現を出力します。
;; @param si 読み込み先のスタック・インデックス
(define (emit-stack-load-t0 si)
  (emit "	lw t0, ~s(sp)" si))

;;; 次のスタック・インデックスを返します。
;; @param si 現在のスタック・インデックス
(define (next-stack-index si)
  (- si wordsize))

;;; スタック・ポインタを移動させるアセンブリ表現を出力します。
;; @param si 移動先スタック・インデックス
(define (emit-adjust-base si)
  (emit "	addi sp, sp, ~s" si))

;;;; 環境関連(シンボルとその値の集合)

;;; 環境を生成します。
;; @param bindings 初期値。 '((a -8) (b -9))のような、シンボル、スタック・インデックスのリストのリスト。
;;    何もなければ空リストを渡す
(define (make-initial-env bindings)
  bindings)

;;; binding部分を生成します。
;; @param lhs シンボル(left hand side)。age = 20 の左辺に相当
;; @param rhs 値(right hand side)。age = 20 の右辺に相当
(define (bind lhs rhs)
  (list lhs rhs))

;;; bindingの束縛されるシンボル(left hand side)を返します
;; @param expr '(a -8)のような束縛式
(define (lhs expr)
  (car expr))

;;; bindingの値(right hand side)を返します
;; @param expr '(a -8)のような束縛式
(define (rhs expr)
  (cadr expr))

;;; 環境に変数を追加します。
;; @param var 変数のシンボル
;; @param si 値を示すスタック・インデックス
;; @param env 変数を追加する環境
(define (extend-env var si env)
  (cons (list var si) env))

;;; 環境に変数をまとめて追加します。
;; @param vars 変数のシンボルのリスト
;; @param vals 値のリスト
;; @param env 変数を追加する環境
(define (bulk-extend-env vars vals env)
  (append (map list vars vals) env))

;;; 環境から変数の値を検索します
;; @param var 検索した変数
;; @param env 環境
(define (lookup var env)
  (cond
   ((assv var env)
    (cadr (assv var env)))
   (else #f)))

;;;; オブジェクト型情報定義: タグ付きポインタ表現を使う

;;; 固定長整数: 下位2ビットが00、上位30ビットが符号付き整数となっている
(define fxshift 2)			; 整数変換シフト量
(define fxmask #x03)			; 整数判定ビットマスク(ANDを取って0なら整数オブジェクト)
(define fxtag #0x0)			;

;;; boolean:
(define bool_f #x2f)			; #fの数値表現
(define bool_t #x6f)			; #t
(define boolmask #xbf)			; boolean判定ビットマスク(ANDを取ってis_boolならbooleanオブジェクト)
(define is_bool #x2f)			;
(define bool_bit 6)			; booleanの判定用ビット位置

;;; 空リスト:
(define empty_list #x3f)		;
(define emptymask #xff)			;

;;; 文字: 下位8ビットが00001111。ASCIIコードを8ビットシフトして、0x0fとORを取る。
(define charmask #xff)			; char判定ビットマスク(ANDを取って、chartagならchar)
(define chartag #x0f)			; charタグ
(define charshift 8)			; char変換シフト量

(define wordsize 4)			; 32bit(4バイト)
(define wordshift 2)			; word数 -> バイト数 変換シフト量

(define fixnum-bits (- (* wordsize 8) fxshift)) ; 整数のビット数
(define fxlower (- (expt 2 (- fixnum-bits 1)))) ; 整数の下限値
(define fxupper (- (expt 2 (- fixnum-bits 1))	; 整数の上限値
		   1))

;;; 固定長整数かどうかを判定します。
;; @param x 判定対象の値
(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))


;;;; 即値関連関数

;;; 即値かどうかを返します。
;; @param x 判定対象
(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

;;; Schemeの即値から、アセンブリ言語でのオブジェクト表現を返します。
;; @param x Schemeの即値
(define (immediate-rep x)
  (cond
   ((fixnum? x) (ash x fxshift))
   ((eq? x #f) bool_f)
   ((eq? x #t) bool_t)
   ((char? x) (logior (ash (char->integer x) charshift) chartag))
   ((null? x) empty_list)
   (else (error "invalid immediate"))))

;;; 即値表現から、アセンブリ言語を出力します。
;; @param x 即値表現
(define (emit-immediate expr)
  (let ((imm (immediate-rep expr)))
    (when (>= imm 4096)
	  (emit "	lui a0, ~s" (ash imm -12)))
    (emit "	li a0, ~s" imm)))

;;;; グローバル・プロバティ(プリミティブの定義で使用する)

;;; プロパティ表現(Keyとその値の対応)から構成されるオブジェクトの、ハッシュテーブル。
(define *prop* (make-eq-hashtable)) 

;;; 指定されたオブジェクトのプロパティの値を返します。
;; @param x オブジェクト
;; @param property xオブジェクトのプロパティ名
(define (getprop x property)
  (let ((prop (hashtable-ref *prop* x #f)))
    (if prop
	(hashtable-ref prop property #f)
	#f)))

;;; 指定されたオブジェクトにプロパティの値を加えます
;; @param x オブジェクト
;; @param property xオブジェクトのプロパティ名
;; @param val プロパティの値
(define (putprop x property val)
  (let ((entry (hashtable-ref *prop* x #f)))
    (if entry
	(hashtable-set! entry property val)
	(hashtable-set! *prop*
			x
			(let ((entry (make-eq-hashtable)))
			  (hashtable-set! entry property val)
			  entry)))))

;;;; プリミティブ関連

;;; プリミティブを定義します
(define-syntax define-primitive
  (syntax-rules ()
    ;; @param prim-name
    ;; @param si 現在使えるスタック・インデックス
    ;; @param env 環境
    ;; @param arg* ... プリミティブの引数
    ;; @param body プリミティブのbody部分
    ;; @param body* ... bodyが複数の式から構成される場合のbodyリスト
    ((_ (prim-name si env arg* ...) body body* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
		(length '(arg* ...)))
       (putprop 'prim-name '*emmiter*
		(lambda (si env arg* ...)
		  body body* ...))))))

;;; ライブラリの定義保持用のグローバル変数
(define lib-primitives '())

;;; ライブラリのプリミティブを定義します。
(define-syntax define-lib-primitive
  (syntax-rules ()
    ((_ (prim-name arg* ...) body body* ...)
     (begin
       (set! lib-primitives (cons 'prim-name lib-primitives))
       (putprop 'prim-name '*is-lib-prim* #t)
       (putprop 'prim-name '*arg-count*
		(length '(arg* ...)))
       (putprop 'prim-name '*lib-code*
		(make-lambda '(arg* ...) (make-begin '(body body* ...))))))
    ((_ (prim-name . varargs) body body* ...)
     (begin
       (set! lib-primitives (cons 'prim-name lib-primitives))
       (putprop 'prim-name '*is-lib-prim* #t)
       (putprop 'prim-name '*arg-count* 0)
       (putprop 'prim-name '*vararg* #t)
       (putprop 'prim-name '*lib-code*
		(make-lambda 'varargs (make-begin '(body body* ...))))))))

;;; ライブラリをロード
(load "lib.scm")

;;; 引数が基本演算かどうかを返します。
;; @param x 判定対象。xは、add1のようにシンボルで、*is-prim*が#tにセットされている必要がある
(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

;;; プリミティブのemmiterを返します。
;; @param x プリミティブのインスタンス
(define (primitive-emitter x)
  (or (getprop x '*emmiter*) (error "primitive-emitter: not exist emmiter")))

;;; プリミティブ処理のアセンブリ表現を出力します。
(define (emit-primcall si env expr)
  (let ((prim (car expr))
	(args (cdr expr)))
    (apply (primitive-emitter prim) si env args)))

;;; ライブラリのプリミティブかどうかを判定します。
;; @param x 判定対象。
(define (lib-primitive? x)
  (and (symbol? x) (getprop x '*is-lib-prim*)))

;;; ライブラリのプリミティブのコード部分を返します。
;; @param x プリミティブ
(define (lib-primitive-code x)
  (or (getprop x '*lib-code*)
      (error "lib-primitive-code:" (format #t "primitive ~s has no lib code; ~s" x))))

;;;; 単項演算関連

;;; 単項演算呼び出し(単項演算処理)かどうかを返します。
;; @param expr 単項演算呼び出しは(op arg)の形式なので、最初はpairで、carがprimitive?がtrueを返すものでなければならない。
(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

;;; 引数に1を加えた値を返すプリミティブ
(define-primitive (fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "	addi a0, a0, ~s" (immediate-rep 1)))

;;; 引数から1を引いた値を返すプリミティブ
(define-primitive (fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "	addi a0, a0, ~s" (immediate-rep -1)))

;;; fixnumからcharに変換するプリミティブ
(define-primitive (fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "	slli a0, a0, ~s" (- charshift fxshift))
  (emit "	ori  a0, a0, ~s" chartag))

;;; charからfixnumに変換するプリミティブ
(define-primitive (char->fixnum si env arg)
  (emit-expr si env arg)
  (emit "	srli a0, a0, ~s" (- charshift fxshift)))

;;; fixnumかどうかを返すプリミティブ
(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit "	andi a0, a0, ~s" fxmask)
  (emit-cmp-bool fxtag))

;;; 空リストかどうかを返すプリミティブ
(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit "	andi a0, a0, ~s" emptymask)
  (emit-cmp-bool empty_list))

;;; booleanオブジェクトかどうかを返すプリミティブ
(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit "	andi a0, a0, ~s" boolmask)
  (emit-cmp-bool is_bool))

;;; 文字オブジェクトかどうかを返すプリミティブ
(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit "	andi a0, a0, ~s" charmask)
  (emit-cmp-bool chartag))

;;; #fなら#tを返し、それ以外は#fを返すプリミティブ
(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit-cmp-bool bool_f))

;;; ビット単位の否定のプリミティブ
(define-primitive (fxlognot si env arg)
  (emit-expr si env arg)
  (emit "	xori a0, a0, ~s" (immediate-rep -1)))

;;;; 二項基本演算

;;; 二項基本演算ユーティリティ
;; arg1、arg2を評価し、結果をそれぞれt0、a0レジスタに代入するアセンブリ表現を出力します。
(define (emit-binop si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-stack-save si)			; 結果をスタックに一時退避
  (emit-expr (next-stack-index si) env arg2)
  (emit-stack-load-t0 si))		      ; スタックに退避した値をt0に復元

;;; 整数加算のプリミティブ
(define-primitive (fx+ si env arg1 arg2)	; siは、stack indexの略。siが指す先は、空き領域にしてから呼び出す事
  (emit-binop si env arg1 arg2)    
  (emit "	add a0, a0, t0"))

;;; 整数減算のプリミティブ
(define-primitive (fx- si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "	sub a0, t0, a0"))

;;; 整数積のプリミティブ
(define-primitive (fx* si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "	sra a0, a0, ~s" fxshift)
  (emit "	mul a0, t0, a0"))

;;; 整数ビット論理積のプリミティブ
(define-primitive (fxlogand si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "	and a0, a0, t0"))

;;; 整数ビット論理和のプリミティブ
(define-primitive (fxlogor si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "	or a0, a0, t0"))

;;; 整数の等号のプリミティブ
(define-primitive (fx= si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit-cmp-bool))

;;; 整数のプリミティブ小なりのプリミティブ
(define-primitive (fx< si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "       slt a0, t0, a0")
  (emit "	slli a0, a0, ~s" bool_bit)
  (emit "	ori  a0, a0, ~s" bool_f))

;;; 整数以下のプリミティブ
(define-primitive (fx<= si env arg1 arg2)
  (emit-expr si env (list 'fx< arg2 arg1)) ; 大なりを判定して、あとで否定する
  (emit "	xori a0, a0, ~s" (ash 1 bool_bit)))

;;; 整数大なりのプリミティブ
(define-primitive (fx> si env arg1 arg2)
  (emit-expr si env (list 'fx< arg2 arg1)))	; 引数を逆にして、小なりを使う

;;; 整数以上のプリミティブ
(define-primitive (fx>= si env arg1 arg2)
  (emit-expr si env (list 'fx< arg1 arg2)) ; 小なりを判定して、あとで否定する
  (emit "	xori a0, a0, ~s" (ash 1 bool_bit)))

;;; 文字等号のプリミティブ
(define-primitive (char= si env arg1 arg2)
  (emit-binop si env arg1 arg2)	; 型判定をしていないので、fx=と同じ内容。eq?をこれにしてOKかも
  (emit-cmp-bool))

;;; 整数の小なりのプリミティブ
(define-primitive (char< si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "       slt a0, t0, a0")
  (emit "	slli a0, a0, ~s" bool_bit)
  (emit "	ori  a0, a0, ~s" bool_f))

;;; 整数の以下のプリミティブ
(define-primitive (char<= si env arg1 arg2)
  (emit-expr si env (list 'char< arg2 arg1)) ; 大なりを判定して、あとで否定する
  (emit "	xori a0, a0, ~s" (ash 1 bool_bit)))

;;; 整数の大なりのプリミティブ
(define-primitive (char> si env arg1 arg2)
  (emit-expr si env (list 'char< arg2 arg1)))	; 引数を逆にして、小なりを使う

;;; 整数の以上のプリミティブ
(define-primitive (char>= si env arg1 arg2)
  (emit-expr si env (list 'char< arg1 arg2)) ; 小なりを判定して、あとで否定する
  (emit "	xori a0, a0, ~s" (ash 1 bool_bit)))

;;;; 特殊形式関連

;;; 特殊形式、プリミティブのシンボルかどうかを判定します。
;; @param symbol 判定対象
(define (special? symbol)
  (or (member symbol '(if begin let lambda closure set! quote))
      (primitive? symbol)
      (lib-primitive? symbol)))

;;;; 条件式

;;; if形式関連

;;; if形式かどうかを返します
;; @param expr 判定対象のS式
(define (if? expr)
  (and (tagged-form? 'if expr)
       (or (= (length (cdr expr)) 3)
	   (error "if? " (format #t "malformed if ~s" expr)))))

;;; if形式のS式の述部(predicate、真偽を判定するS式)を取り出します。
;; @param expr ifのS式
(define (if-test expr)
  (cadr expr))

;;; if形式のS式の帰結部(consequent、述部が真の時の処理部)を取り出します。
;; @param expr ifのS式
(define (if-conseq expr)
  (caddr expr))

;;; if形式のS式の代替部(alternative、述部が偽の時の処理部)を取り出します。
;; @param expr ifのS式
(define (if-altern expr)
  (cadddr expr))

;;; if形式のS式のアセンブリ表現を出力します。
(define (emit-if si env tail expr)
  (let ((alt-label (unique-label))
	(end-label (unique-label)))
    (emit-expr si env (if-test expr))
    (emit "	addi a0, a0, ~s" (- bool_f))
    (emit "	beqz a0, ~a" alt-label)
    (emit-any-expr si env tail (if-conseq expr))
    (if (not tail) (emit "	j ~a" end-label))
    (emit "~a:" alt-label)
    (emit-any-expr si env tail (if-altern expr))
    (emit "~a:" end-label)))

;;; and形式関連

;;; and形式かどうかを判定します。
;; @param expr 判定対象のS式
(define (and? expr)
  (tagged-form? 'and expr))

;;; and形式のアセンブリ表現を出力します。
(define (emit-and si env expr)
  (let ((pred-len (length (cdr expr))))
    (cond
     ((= pred-len 0)
      (emit "	li a0, ~s" bool_t))	;引数なしなら常に真
     ((= pred-len 1)
      (emit-primcall si env (list 'not (cadr expr))) ; まず、(not test)の式に変換して評価する
      (emit "	xori a0, a0, ~s" (ash 1 bool_bit))) ; a0は偽かどうかの値なので、ビット反転でnotを演算する
     (else
      ;; (and test test* ...) => (if test (and test* ...) #f)と変換して処理
      (emit-if si env #f (list 'if (cadr expr)
		                 (cons 'and (cddr expr))
		                #f))))))

;;;; or形式関連

;;; or形式かどうかを判定します。
;; @param expr 判定対象のS式
(define (or? expr)
  (tagged-form? 'or expr))

;;; and形式のアセンブリ表現を出力します。
(define (emit-or si env expr)
  (let ((pred-len (length (cdr expr))))
    (cond
     ((= pred-len 0)
      (emit "	li a0, ~s" bool_f))	;引数なしなら常に偽
     ((= pred-len 1)
      (emit-primcall si env (list 'not (cadr expr))) ; まず、(not test)の式に変換して評価する
      (emit "	xori a0, a0, ~s" (ash 1 bool_bit))) ; a0は偽かどうかの値なので、ビット反転でnotを演算する
     (else
      ;; (or test test* ...) => (if test #t (or test* ...))と変換して処理
      (emit-if si env #f (list 'if (cadr expr)
		                 #t
		                 (cons 'or (cddr expr))))))))

;;;; let形式

;;; let形式かどうかを返します
;; @param expr 判定対象のS式
(define (let? expr)
  (tagged-form? 'let expr))

;;; letの形式の種類を返します。
;; @param expr 判定対象のS式
;; @return 'let 'let* 'let-recのいずれか
(define (let-kind expr)
  (car expr))

;;; let形式のbinding部分を返します。
;; @param expr let形式のS式
(define (let-bindings expr)
  (cadr expr))

;;; let形式を作成します。
;; @param let binding body... 
(define make-let list)

;;; let形式のbody部分が複数の式からなる時は、begin形式に変換してbody部分を1つの式にします。
;; @param expr let形式のS式
(define (let-body expr)
  (if (null? (cdddr expr))
      (caddr expr)
      (make-begin (cddr expr))))

;;; let形式のbodyの並びを返します。
;; @param expr let形式のS式
(define (let-body-seq expr)
  (cddr expr))

;;; let形式のアセンブリ表現を出力します。
(define (emit-let si env tail expr)
  (define (process-let bindings si new-env)
    (cond
     ((null? bindings)
      (emit-any-expr si new-env tail (let-body expr)))
     (else
      (let ((b (car bindings)))
	(emit-expr si env (rhs b))
	(emit-stack-save si)
	(process-let (cdr bindings)
		     (next-stack-index si)
		     (extend-env (lhs b) si new-env))))))
  (process-let (let-bindings expr) si env))

;;;; let*形式関連

;;; let*形式かどうかを返します。
;; @param expr 判定対象のS式
(define (let*? expr)
  (tagged-form? 'let* expr))

;;;; letrec形式

;;; letrec形式かどうかを返します。
;; @param expr 判定対象のS式
(define (letrec? expr)
  (or (tagged-form? 'letrec expr) (tagged-form? 'letrec* expr)))

;;; let系形式のどれかか、どうかを返します。
;; @param expr 判定対象のS式
(define (any-let? expr)
  (and (pair? expr)
       (member (car expr) '(let let* letrec))))

;;;; begin形式

;;; begin形式かどうかを返します。
;; @param expr 判定対象のS式
(define (begin? expr)
  (tagged-form? 'begin expr))

;;; begin形式のアセンブリ表現を出力します。
(define (emit-begin si env tail expr)
  (emit-seq si env tail (cdr expr)))

;;; 連続した式をのアセンブリ表現を出力します
;; @param seq 連続した式。例: ((set-car! some-pair 7) (set-cdr! come-pair 5) some-pair) 
(define (emit-seq si env tail seq)
  (cond
   ((null? seq) (error "empty seq"))
   ((null? (cdr seq))			; 連続式の末尾の場合
    (emit-any-expr si env tail (car seq)))
   (else				; 連続式の途中の場合
    (emit-expr si env (car seq))
    (emit-seq si env tail (cdr seq)))))

;;; begin形式を作成します。
;; @param lst begin形式にしたいS式のリスト
(define (make-begin lst)
  (if (null? (cdr lst))
      (car lst)
      (cons 'begin lst)))

;;; bodyの式のリストを一つのS式(begin形式)にします。
;; @param lst S式のリスト
(define (make-body lst)
  (make-begin lst))

;;;; 変数参照関連
;; 変数の表現書式は以下の3通りあります。
;; 数値: スタック上の変数
;; ('free offset): 自由変数。offsetは、クロージャ・オブジェクト内でのオフセット
;; シンボル: クロージャを指している

;;; 変数かどうかを返します。
;; @param expr 判定対象
(define (variable? expr)
  (symbol? expr))

;;; 自由変数を作成します。
;; @param offset クロージャ・オブジェクト内でのオフセット
(define (free-var offset)
  (list 'free (- offset closuretag)))

;;; 変数が自由変数かどうかを判定します。
;; @param var 判定対象変数
(define (free-var? var)
  (tagged-form? 'free var))

;;; 式の中から自由変数を取得します。
;; @param expr S式
(define (get-free-vars expr)
  (cond
   ((variable? expr)
    (list expr))
   ((lambda? expr)
    (filter (lambda (v)
	      (not (member v (lambda-vars expr))))
	    (get-free-vars (lambda-body expr))))
   ((let? expr)
    (append (append-map get-free-vars (map cadr (let-bindings expr)))
	    (filter (lambda (v)
		      (not (member v (map car (let-bindings expr)))))
		    (get-free-vars (let-body expr)))))
   ((tagged-form? 'primitive-ref expr) '())
   ((list? expr)
    (append-map get-free-vars (if (and (not (null? expr))
				       (special? (car expr)))
				  (cdr expr)
				  expr)))
   (else '())))

;;; 変数参照のアセンブリ表現を出力します。
(define (emit-variable-ref si env var)
  (cond
   ((lookup var env)
    (let ((v (lookup var env)))
      (cond
       ((free-var? v)
	(emit "	lw a0, ~s(a1)" (cadr v)))
       ((number? v)
	(emit-stack-load v))
       (else (error "emit-variable-ref. "
		    (format #t "looked up unknown value ~s for var ~s" v var))))))
   (else (error "emit-variable-ref. " (format "undefined variable ~s" var)))))

;;;; set!関連

;;; set!形式かどうかを返します
;; @param expr 判定対象のS式
(define (set? expr)
  (tagged-form? 'set! expr))

;;; set!形式を生成します。
;; @param lhs (left hand side)
;; @param rhs (right hand side)
(define (make-set! lhs rhs)
  (list 'set! lhs rhs))

;;; set!形式のS式の中の代入されるシンボルを返します
;; @param expr set!形式のS式
(define (set-lhs expr)
  (cadr expr))

;;; set!形式の代入する式を返します。
;; @param expr set!形式のS式
(define (set-rhs expr)
  (caddr expr))

;;;; lambda形式

;;; lamda形式かどうかを返します。
;; @param expr 判定対象のS式
(define (lambda? expr)
  (tagged-form? 'lambda expr))

;;; lambda形式の引数部分を返します。
;; @param expr lambda形式のS式
(define (lambda-formals expr)
  (cadr expr))

;;; lambda式の可変長引数の部分(ドット・リスト)をリストに変換します。
;; @param expr 引数
(define (formals-to-vars formals)
  (cond
   ((list? formals)
    formals)
   ((pair? formals)
    (cons (car formals) (formals-to-vars (cdr formals))))
   (else
    (list formals))))

;;; lambda式の可変長引数(ドット・リスト)のリストに変換して返します。
;; @param expr lambda形式のS式
;; @return 引数のリスト。元のlambdaのS式でない事に注意
(define (lambda-vars expr)
  (formals-to-vars (lambda-formals expr)))

;;; 引数リストに対して、fをmapします。ドット・リストにも対応します。
;; @param f 適用する手続き
;; @param formals 引数のリスト(ドット・リストも化)
(define (map-formals f formals)
  (cond
   ((list? formals)
    (map f formals))
   ((pair? formals)
    (cons (f (car formals)) (map-formals f (cdr formals))))
   (else
    (f formals))))

;;; lambda形式の本体部分を返します。
;; @param expr lambda形式のS式
(define (lambda-body expr)
  (make-body (cddr expr)))

;;; lambda形式を生成します。
;; @param formals 引数リスト
;; @param body 本体
(define (make-lambda formals body)
  (list 'lambda formals body))

;;;; code形式関連
;; code形式は、本コンパイラ内部専用の形式であり、Schemeには存在しない。

;;; code形式の引数部分を返します
;; @param expr code形式のS式
(define (code-formals expr)
  (cadr expr))

;;; code形式の引数の束縛を返します。ドット・リストは、リストに変換されます
;; @param expr code形式のS式
(define (code-bound-variables expr)
  (formals-to-vars (code-formals expr)))

;;; code形式の自由変数部分を返します
;; @param expr code形式のS式
(define (code-free-variables expr)
  (caddr expr))

;;; code形式の本体部分を返します。
;; @param expr code形式のS式
(define (code-body expr)
  (cadddr expr))

;;; 可変長引数形式のcode形式かどうかを返します
;; @param expr code形式のS式
(define (code-vararg? expr)
  (not (list? (code-formals expr))))

;;; code形式を生成します。
;; @param formals 引数リスト
;; @param free-vars 自由変数
;; @param body 本体
(define (make-code formals free-vars body)
  (list 'code formals free-vars body))

;;; code形式のアセンブリ表現を出力する手続きを返します
;; @param env
;; @param global? ファイル外に公開するか
;;
;; @return code形式の式と、そのlabelを引数に取ってアセンブリ表現を出力する手続き
(define (emit-code env global?)
  (lambda (expr label)
    ((if global? emit-function-header emit-label) label)
    (emit "	addi sp, sp, ~s" (- wordsize))
    (emit "	sw ra, 0(sp)")
    (let ((bound-variables (code-bound-variables expr))
	  (free-vars (code-free-variables expr))
	  (body (code-body expr)))
      (when (code-vararg? expr)
	    (let ((start-label (unique-label))
		  (fill-label (unique-label))
		  (loop-label (unique-label)))
	      (emit "	mv t0, a0")	; 引数の数(arity)はt0として扱う
	      (emit-immediate '())	; 残余引数のリストの末尾をa0にセット
	      (emit "	li t1, ~s" (- (length bound-variables) 1))
	      (emit "	ble t0, t1, ~s" fill-label) ; 残余引数がない場合へ(本来はt0<t1はエラー)
	      ;; 残余引数をリスト化する
	      (emit-label loop-label)
	      (emit "	li t1, ~s" (length bound-variables))
	      (emit "	blt t0, t1, ~s" start-label)
	      (emit "	slli t0, t0, ~s" wordshift) ; スタックポインタを一時的に引数の最上位に変更して作業したい
	      (emit "	sub sp, sp, t0")
	      (emit-stack-save (next-stack-index 0)) ; a0をスタックに積む(ループの最初は())
	      (emit-stack-save-t0 (next-stack-index (next-stack-index 0))) ; cons呼び出し前にt0をスタックに退避
	      (emit-cons 0)		; スタックの引数の最後尾2つをconsを使ってリスト化していく
	      (emit-stack-save 0)	; 引数はconsオブジェクトを参照するようにする
	      (emit-stack-load-t0 (next-stack-index (next-stack-index 0)))
	      (emit "	add sp, sp, t0") ;スタックポインタを元に戻す
	      (emit "	srai t0, t0, ~s" wordshift)
	      (emit "	addi t0, t0, -1")
	      (emit "	j ~s" loop-label)
	      ;; 残余引数を空リストにセットする 
	      (emit-label fill-label)
	      (emit "	addi t0, t0, 1")
	      (emit "	slli t0, t0, ~s" wordshift)
	      (emit "	sub sp, sp, t0")
	      (emit-stack-save 0)	; 既にa0に()がセットされている
	      (emit "	add sp, sp, t0")
	      ;; 手続き本体開始
	      (emit-label start-label)))
      (extend-env-with (- wordsize) env bound-variables
		       (lambda (si env)
			 (close-env-with wordsize env free-vars
					 (lambda (env)
					   (emit-tail-expr si env body))))))))

;;; 手続き呼び出しの引数で環境を拡張して、kを実行します。
;; @param lvars 引数のリスト
;; @param k 拡張された環境で実行したい手続き? thunk?
(define (extend-env-with si env lvars k)
  (if (null? lvars)
      (k si env)
      (extend-env-with (next-stack-index si)
		       (extend-env (car lvars) si env)
		       (cdr lvars)
		       k)))

;;; 自由変数で環境を拡張して、kを実行します。 (closure対応に必要)
;; @param offset クロージャ・オブジェクトの開始アドレスからのオフセット
;; @param lvars 自由変数のリスト
(define (close-env-with offset env lvars k)
  (if (null? lvars)
      (k env)
      (close-env-with (+ offset wordsize)
		      (extend-env (car lvars) (free-var offset) env)
		      (cdr lvars)
		      k)))


;;;; labels形式関連

;;; labels形式の本体部分を返します。
;; @param expr labels形式のS式
(define labels-body let-body)

;;; labels形式のアセンブリ表現を出力します。
;; @param labels-expr labels形式のS式
;; @param k S式 (expr) を環境 (env) を受け取って、アセンブリコードを出力する手続き
(define (emit-labels labels-expr k)
  (let* ((bindings (let-bindings labels-expr))
	 (labels (map car bindings))
	 (codes (map cadr bindings))
	 (env (make-initial-env '())))
    (for-each (emit-code env #f) codes labels)
    (k (labels-body labels-expr) env)))

;;;; app関連

;;; apply可能かどうか
(define (app? expr env)
  (and (list? expr) (not (null? expr))))

;;; applyの出力
;; si スタック・インデックス(stack index)
;; env 環境(environment)。変数や関数の名前と、アクセスするための位置情報のリスト
;; tail 式が手続きの末尾(tail)かどうか。
;; expr lambda式(expression)
(define (emit-app si env tail expr)
  ;;; 呼び出し先の引数をスタックに積む
  (define (emit-arguments si args)
    (unless (null? args)
      (emit-expr si env (car args))
      (emit-stack-save si)
      (emit-arguments (- si wordsize) (cdr args))))
  ;;; 末尾呼び出しの場合は、引数を自分の関数のスタックに移動する
  ;; delta 移動量
  (define (move-arguments si delta args)
    (unless (or (= delta 0) (null? args))
      (emit-stack-load si)
      (emit-stack-save (+ si delta))
      (move-arguments (- si wordsize) delta (cdr args))))
  (cond
   ((not tail)
    (emit-arguments (- si (* 2 wordsize)) (cdr expr))
    (emit-expr si env (car expr))
    (emit "	sw a1, ~s(sp)" si)
    (emit "	mv a1, a0")
    (emit-heap-load (- closuretag))
    (emit-adjust-base si)
    (emit "	mv t0, a0")
    (emit "	addi a0, a0, ~s" (length (cdr expr)))
    (emit "	jalr t0")
    (emit-adjust-base (- si))
    (emit "	lw a1, ~s(sp)" si))
   (else				; tail
    (emit-arguments si (cdr expr))
    (emit-expr (- si (* wordsize (length (cdr expr)))) env (car expr))
    (emit "	mv a1, a0")
    (move-arguments si (- (+ si wordsize)) (cdr expr))
    (emit "	mv a0, a1")
    (emit-heap-load (- closuretag))
    (emit "	mv t0, a0")
    (emit "	li a0, ~s" (length (cdr expr)))
    (emit "	lw ra, 0(sp)")
    (emit "	addi sp, sp, ~s" wordsize)
    (emit "	jr t0"))))

;;; Scheme手続きに対応する、アセンブリ言語のラベルを返します。見つからなかった場合は#fを返します。
(define (proc expr env)
  (and (variable? expr)
       (let ((val (lookup expr env)))
	 (and (symbol? val) val))))

;;;; ヒープ領域オブジェクト関連
(define objshift 2)			; ヒープ・オブジェクト・シフト量
(define objmask #x07)			; ヒープ・オブジェクト判定マスク(ANDを取って型タグの値と比較)

;;; ヒープメモリ確保時の最低サイズ(バイト)
(define heap-cell-size (ash 1 objshift))

;;; ヒープメモリを確保します。確保したアドレスはa0に設定
;; @param size 確保するバイト数
(define (emit-heap-alloc size)
  (let ((alloc-size (* (+ (div (- size 1) heap-cell-size) 1) heap-cell-size)))
    (emit "	mv a0, s0")
    (emit "	addi s0, s0, ~s" alloc-size)))

;;; 動的にヒープ・メモリを確保します。確保するバイト数はa0にセットして呼び出します。
(define (emit-heap-alloc-dynamic)
  (emit "	addi a0, a0, -1")
  (emit "	srai a0, a0, ~s" objshift)
  (emit "	addi a0, a0, 1")
  (emit "	slli a0, a0, ~s" objshift)
  (emit "	mv t0, a0")
  (emit "	mv a0, s0")
  (emit "	add s0, s0, t0"))

;;; スタックの値をヒープにコピーします。
;; @param si コピー元の値のスタックインデックス
;; @param offset a0+offset のアドレスに値をコピーします。
(define (emit-stack-to-heap si offset)
  (emit "	lw t0, ~s(sp)" si)
  (emit "	sw t0, ~s(a0)" offset))

;;; ヒープの値をa0に読み込みます。
;; @param offset a0+offset のアドレスの値を読み込みます
(define (emit-heap-load offset)
  (emit "	lw a0, ~s(a0)" offset))

;;; オブジェクトの型判定をします。
;; @param tag オブジェクトの型タグ
(define (emit-object? tag si env arg)
  (emit-expr si env arg)
  (emit "	andi a0, a0, ~s" objmask)
  (emit-cmp-bool tag))

;;;; eq?のプリミティブ
(define-primitive (eq? si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit-cmp-bool))

;;;; ペア関連
(define pairtag #b001)			; ペアのタグ
(define pairsize 8)			; ペアのメモリサイズ(バイト)
(define paircar 0)			; ペア中のcar部分のオフセット
(define paircdr 4)			; ペア中のcdr部分のオフセット

;;; consのプリミティブ
(define-primitive (cons si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit-stack-save (next-stack-index si))
  (emit-cons si))

;;; consのアセンブリ表現を出力します。consされるオブジェクトはスタックのsiと、(next-stack-index si)にある値
(define (emit-cons si)
  (emit-heap-alloc pairsize)
  (emit "	ori a0, a0, ~s" pairtag)
  (emit-stack-to-heap si (- paircar pairtag))
  (emit-stack-to-heap (next-stack-index si) (- paircdr pairtag)))

;;; pair?のプリミティブ
(define-primitive (pair? si env arg)
  (emit-object? pairtag si env arg))

;;; carのプリミティブ
(define-primitive (car si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- paircar pairtag)))

;;; cdrのプリミティブ
(define-primitive (cdr si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- paircdr pairtag)))

;;; set-car!のプリミティブ
(define-primitive (set-car! si env cell val)
  (emit-binop si env val cell)
  (emit-stack-to-heap si (- paircar pairtag)))

;;; set-cdr!のプリミティブ
(define-primitive (set-cdr! si env cell val)
  (emit-binop si env val cell)
  (emit-stack-to-heap si (- paircdr pairtag)))

;;;; ベクトル関連
(define vectortag #x05)			; ベクトルのタグ

;;; ベクトルを生成するプリミティブ
;; @param length ベクトルの要素数
(define-primitive (make-vector si env length)
  (emit-expr-save si env length)
  (emit-make-vector si))

;;; ベクトル生成のアセンブリ表現を出力します。
(define (emit-make-vector si)
  (emit "	addi a0, a0, ~s" (ash 1 fxshift)) ; 要素数+1のセルを確保する。+1はlengthデータ保存用
  (emit "	slli a0, a0, ~s" wordshift)	  ; 要素数 -> バイト数へ変換
  (emit-heap-alloc-dynamic)
  (emit-stack-to-heap si 0)
  (emit "	ori a0, a0, ~s" vectortag))

;;; ベクトルかどうかを返すプリミティブ
(define-primitive (vector? si env arg)
  (emit-object? vectortag si env arg))

;;; ベクトルの要素数を返すプリミティブ
(define-primitive (vector-length si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- vectortag)))	; タグの値の分だけアドレスをずらす

;;; ベクトルに値をセットするプリミティブ
;; @param vector セットされるベクトル
;; @param index セットする位置
;; @param value セットする値
(define-primitive (vector-set! si env vector index value)
  (emit-expr si env index)
  (emit "	addi a0, a0, ~s" (ash 1 fxshift)) ; index=0の位置には長さが入っているのでずれる。 
  (emit "	slli a0, a0, ~s" (- objshift fxshift))
  (emit-stack-save si)
  (emit-expr-save (next-stack-index si) env value)
  (emit-expr si env vector)
  (emit-stack-load-t0 si)
  (emit "	add a0, a0, t0")
  (emit-stack-to-heap (next-stack-index si) (- vectortag)))

;;; ベクトルの要素の値を取得します。
(define-primitive (vector-ref si env vector index)
  (emit-expr si env index)
  (emit "	addi a0, a0, ~s" (ash 1 fxshift)) ; index=0の位置には長さが入っているのでずれる。 
  (emit "	slli a0, a0, ~s" (- objshift fxshift))
  (emit-stack-save si)
  (emit-expr si env vector)
  (emit-stack-load-t0 si)
  (emit "	add a0, a0, t0")
  (emit-heap-load (- vectortag)))

;;;; 文字列関連
(define stringtag   #x06)		; 文字列の型タグ

;;; 文字列を作成するプリミティブ
(define-primitive (make-string si env length)
  (emit-expr-save si env length)
  (emit-make-string si))

;;; 文字列生成のアセンブリ表現を出力します。
(define (emit-make-string si)
  (emit "	srai a0, a0, ~s" fxshift)
  (emit "	addi a0, a0, ~s" wordsize)
  (emit-heap-alloc-dynamic)
  (emit-stack-to-heap si 0)
  (emit "	ori a0, a0, ~s" stringtag))

;;; 文字列かどうかを返すプリミティブ
(define-primitive (string? si env arg)
  (emit-object? stringtag si env arg))

;;; 文字列の長さを返すプリミティブ
(define-primitive (string-length si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- stringtag)))

;;; 文字列に文字をセットするプリミティブ
(define-primitive (string-set! si env string index value)
  (emit-expr si env index)
  (emit "	srai a0, a0, ~s" fxshift)
  (emit "	addi a0, a0, ~s" wordsize)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env value)
  (emit "	srai a0, a0, ~s" charshift)
  (emit-stack-save (next-stack-index si))
  (emit-expr si env string)
  (emit-stack-load-t0 si)
  (emit "	add a0, a0, t0")
  (emit-stack-load-t0 (next-stack-index si))
  (emit "	sb t0, ~s(a0)" (- stringtag)))

;;; 文字列の文字を参照するプリミティブ
(define-primitive (string-ref si env string index)
  (emit-expr si env index)
  (emit "	srai a0, a0, ~s" fxshift)
  (emit "	addi a0, a0, ~s" wordsize)
  (emit-stack-save si)
  (emit-expr si env string)
  (emit-stack-load-t0 si)
  (emit "	add a0, a0, t0")
  (emit "	lb a0, ~s(a0)" (- stringtag))
  (emit "	slli a0, a0, ~s" charshift)
  (emit "	ori a0, a0, ~s" chartag))

;;;; quote関連

;;; quote形式かどうかを返します。
(define (quote? expr)
  (tagged-form? 'quote expr))

;;; quoteされる中身を返します。
(define (quote-expr expr)
  (cadr expr))

;;; quoteのアセンブリ表現を出力します。
(define (emit-quote si env expr)
  (cond
   ((immediate? expr) (emit-immediate expr))
   ((pair? expr)
    (emit-quote si env (car expr))
    (emit-stack-save si)
    (emit-quote (next-stack-index si) env (cdr expr))
    (emit-stack-save (next-stack-index si))
    (emit-cons si))
   ((vector? expr)
    (emit-expr si env (vector-length expr))
    (emit-stack-save si)
    (emit-make-vector si)
    (emit-stack-save si)
    (let loop ((index 0))
      (unless (= index (vector-length expr))
	      (emit-quote (next-stack-index si) env (vector-ref expr index))
	      (emit-stack-save (next-stack-index si))
	      (emit-stack-load si)
	      (emit "	addi a0, a0, ~s" (* wordsize (+ index 1)))
	      (emit-stack-to-heap (next-stack-index si) (- vectortag))
	      (loop (+ index 1))))
    (emit-stack-load si))
   ((string? expr)
    (emit-expr si env (string-length expr))
    (emit-stack-save si)
    (emit-make-string si)
    (emit-stack-save si)
    (let loop ((index 0))
      (unless (= index (string-length expr))
	      (emit "	addi a0, a0, ~s" (if (= index 0) wordsize 1))
	      (emit "	li t0, ~s" (char->integer (string-ref expr index)))
	      (emit "	sb t0, ~s(a0)" (- stringtag))
	      (loop (+ index 1)))
      (emit-stack-load si)))
   (else (error "emit-quote: don't know how to quote" expr))))

;;;; クロージャ・オブジェクト関連
(define closuretag  #x02)		; クロージャ・オブジェクトタグ

;;; クロージャ特殊形式を作成します。
;; @param label
;; @param free-vars
(define (make-closure label free-vars)
  (cons 'closure (cons label free-vars)))

;;; クロージャかどうかを返します
;; @param expr 判定されるS式
(define (closure? expr)
  (tagged-form? 'closure expr))

;;; クロージャのアセンブリ表現を出力します。
(define (emit-closure si env expr)
  (let ((label (cadr expr))
	(free-vars (cddr expr)))
    (emit-heap-alloc (* (+ (length free-vars) 1) wordsize))
    (emit "	la t0, ~s" label)
    (emit "	sw t0, 0(a0)")
    (unless (null? free-vars)
	    (emit "	mv t0, a0")	; しばらくは、t0がクロージャ・オブジェクトの開始アドレスを指す
	    (let loop ((free-vars free-vars)
		       (count 1))
	      (unless (null? free-vars)	; 自由変数があれば、評価してヒープに保存
		      (emit-variable-ref si env (car free-vars))
		      (emit "	sw a0, ~s(t0)" (* count wordsize))
		      (loop (cdr free-vars) (+ count 1))))
	    (emit "	mv a0, t0"))	; クロージャ・オブジェクトの開始アドレスを戻す
    (emit "	ori a0, a0, ~s" closuretag)))

;;;; 前処理の変換

;;; マクロ変換します。
(define (macro-expand expr)
  (define (transform expr bound-vars)
    (cond
     ((set? expr)
      (make-set! (set-lhs expr) (transform (set-rhs expr) bound-vars)))
     ((lambda? expr)
      (make-lambda
       (lambda-formals expr)
       (transform (lambda-body expr)
		  (append (lambda-vars expr) bound-vars))))
     ((let? expr)
      (make-let
       (let-kind expr)
       (map (lambda (binding)
	      (bind (lhs binding) (transform (rhs binding) bound-vars)))
	    (let-bindings expr))
       (transform (let-body expr)
		  (append (map lhs (let-bindings expr)) bound-vars))))
     ((let*? expr)
      (transform
       (if (null? (let-bindings expr))
	   (let-body expr)
	   (make-let
	    'let
	    (list (car (let-bindings expr)))
	    (make-let
	     'let*
	     (cdr (let-bindings expr))
	     (let-body expr))))
       bound-vars))
     ((letrec? expr)
      (transform
       (make-let
	'let
	(map (lambda (binding)
	       (bind (lhs binding) '#f))
	     (let-bindings expr))
	(make-body
	 (append
	  (map (lambda (binding)
		 (make-set! (lhs binding) (rhs binding)))
	       (let-bindings expr))
	  (let-body-seq expr))))
       bound-vars))
     ((tagged-form? 'and expr)
      (cond
       ((null? (cdr expr))
	#t)
       ((null? (cddr expr))
	(transform (cadr expr) bound-vars))
       (else
	(transform
	 (list 'if (cadr expr)
	           (cons 'and (cddr expr))
		   #f)
	 bound-vars))))
     ((tagged-form? 'or expr)
      (cond
       ((null? (cdr expr))
	#f)
       ((null? (cddr expr))
	(transform (cadr expr) bound-vars))
       (else
	(transform
	 `(let ((one ,(cadr expr))
		(thunk (lambda () (or ,@(cddr expr)))))
	       (if one
		   one
		   (thunk)))
	 bound-vars))))
     ((tagged-form? 'when expr)
      (transform
       `(if ,(cadr expr)
	    ,(make-begin (cddr expr))
	    #f)
       bound-vars))
     ((tagged-form? 'unless expr)
      (transform
       (append `(when (not ,(cadr expr))
		      ,@(cddr expr)))
       bound-vars))
     ((tagged-form? 'cond expr)
      (transform
       (let* ((conditions (cdr expr))
	      (first-condition (car conditions))
	      (first-test (car first-condition))
	      (first-body (cdr first-condition))
	      (rest (if (null? (cdr conditions))
			#f
			(cons 'cond (cdr conditions)))))
	 (cond
	  ((and (eq? first-test 'else) (not (member 'else bound-vars)))
	   (make-begin first-body))
	  ((null? first-body)
	   (list 'or first-test rest))
	  (else
	   (list 'if first-test
		 (make-begin first-body)
		 rest))))
       bound-vars))
     ((list? expr)
      (map (lambda (e)
	     (transform e bound-vars))
	   expr))
     (else
      expr)))
  (transform expr '()))

;;; α変換(変数をユニークにする)
(define (alpha-conversion expr)
  (define (transform expr env)
    (cond
     ((variable? expr)
      (or (lookup expr env)
	  (error "alpha-conversion: undefined variable" expr)))
     ((lambda? expr)			; lamdaの引数名をユニークにする
      (let ((new-env (bulk-extend-env	; lambdaの引数に対応するユニークな名前を環境に追加
		      (lambda-vars expr)
		      (map unique-name (lambda-vars expr))
		      env)))
	(make-lambda			; ユニークな名前を使って、lambda式を作り直す
	 (map-formals (lambda (v)
			(lookup v new-env))
		      (lambda-formals expr))
	 (transform (lambda-body expr) new-env))))
     ((let? expr)			; letのbindされる変数名をユニークにする
      (let* ((lvars (map lhs (let-bindings expr)))
	     (new-env (bulk-extend-env
		       lvars
		       (map unique-name lvars)
		       env)))
	(make-let
	 'let
	 (map (lambda (binding)
		(bind (lookup (lhs binding) new-env)
		      (transform (rhs binding) env)))
	      (let-bindings expr))
	 (transform (let-body expr) new-env))))
     ((and (list? expr) (not (null? expr)) (special? (car expr)))
      (cons (car expr) (map (lambda (e)
			      (transform e env))
			    (cdr expr))))
     ((list? expr)
      (map (lambda (e)
	     (transform e env))
	   expr))
     (else
      expr)))
  (transform expr (make-initial-env '())))

;;; 代入処理の変換
;;; (set!の対象になる自由変数はxを (x . #f) のようにペアに変え、ヒープ領域で各クロージャから共有する)
(define (assignment-conversion expr)
  (let ((assigned '()))			; set!対象の変数名のリスト
    ;; set!対象リストに変数名を追加
    (define (set-variable-assigned! v)
      (unless (member v assigned)
	      (set! assigned (cons v assigned))))
    ;; set!対象リストに追加済みかどうか
    (define (variable-assigned v)
      (member v assigned))
    ;; 式の中のset!対象の変数を、対象リストに追加します。
    (define (mark expr)
      (when (set? expr)
	    (set-variable-assigned! (set-lhs expr)))
      (when (list? expr) (for-each mark expr)))
    (define (transform expr)
      (cond
       ((set? expr)
	(list 'set-car! (set-lhs expr) (transform (set-rhs expr))))
       ((lambda? expr)
	(let ((vars (filter variable-assigned (lambda-vars expr))))
	  (make-lambda
	   (lambda-formals expr)
	   (if (null? vars)
	       (transform (lambda-body expr))
	       (make-let
		'let
		(map (lambda (v)
		       (bind v (list 'cons v #f)))
		     vars)
		(transform (lambda-body expr)))))))
       ((let? expr)
	(make-let
	 'let
	 (map (lambda (binding)
		(let ((var (lhs binding))
		      (val (transform (rhs binding))))
		  (bind var
			(if (variable-assigned var)
			    (list 'cons val #f)
			    val))))
	      (let-bindings expr))
	 (transform (let-body expr))))
       ((list? expr) (map transform expr))
       ((and (variable? expr) (variable-assigned expr))
	(list 'car expr))
       (else
	expr)))
    (mark expr)
    (transform expr)))

;;; 定数定義をトップに繰り上げる
(define (lift-constants expr)
  (let ((constants '()))
    (define (transform expr)
      (cond
       ((and (quote? expr) (assoc expr constants))
	(cadr (assoc expr constants)))
       ((quote? expr)
	(set! constants (cons (list expr (unique-name 'c)) constants))
	(cadr (assoc expr constants)))
       ((string? expr)
	(transform `(quote ,expr)))
       ((list? expr)
	(map transform expr))
       (else expr)))
    (let ((t-expr (transform expr)))
      (if (null? constants)
	  expr
	  (make-let
	   'let
	   (map (lambda (val-cst)
		  (bind (cadr val-cst) (car val-cst)))
		constants)
	   t-expr)))))

;;; ライブラリのプリミティブ呼び出しである注釈(primitive-ref)を追加
(define (annotate-lib-primitives expr)
  (define (transform expr)
    (cond
     ((and (variable? expr) (lib-primitive?)) `(primitive-ref ,expr))
     ((list? expr) (map transform expr))
     (else expr)))
  (transform expr))

;;; ライブラリのプリミティブの呼び出し
;; @param label ライブラリのプリミティブの名前
(define-primitive (primitive-ref si env label)
  (let ((done-label (unique-label)))
    ;; ユーザー定義のプリミティブがあれば、labelアドレスの値は0ではない。
    ;; 0でない時はdone-labelへ行き、何もしない
    (emit "	la t0, ~s" label)
    (emit "	lw a0, 0(t0)")
    (emit "	bnez a0, ~s" done-label)
    (emit-adjust-base si)
    (emit-call (primitive-alloc label))
    (emit-adjust-base (- si))
    (emit "	la t0, ~s" label)	; ???
    (emit "	sw a0, 0 (t0)")		; ???
    (emit-label done-label)))

;;; ライブラリのプリミティブのラベル名を返します。
;; @param label プリミティブの名前
(define (primitive-alloc lable)
  (string->symbol (format "~a_alloc" lable)))

;;; code特殊形式を使った式に変換して、クロージャに対応します
;;; labels特殊形式と、top-envとのリストを返します。
(define (closure-convertion expr)
  (let ((labels '()))
    (define (transform expr . label)
      (cond
       ((lambda? expr)
	(let ((label (or (and (not (null? label)) (car label)) ; labelが指定されていなかったらlabelを生成
			 (unique-label)))
	      (free-vars (get-free-vars expr)))
	  (set! labels
		(cons (bind label (make-code (cadr expr)
					     free-vars
					     (transform (caddr expr))))
		      labels))
	  (make-closure label free-vars)))
       ((any-let? expr)
	(list (car expr)
	      (map (lambda (binding)
		     (list (car binding) (transform (cadr binding))))
		   (let-bindings expr))
	      (transform (let-body expr))))
       ((list? expr)
	(map transform expr))
       (else
	expr)))
    (let* ((body (if (letrec? expr)
		     (transform-letrec expr)
		     (transform expr))))
      (make-let 'labels labels body))))

;;; コンパイル前の変換処理
(define (all-conversions expr)
  (closure-convertion (annotate-lib-primitives (lift-constants (assignment-conversion (alpha-conversion (macro-expand (library-hack expr))))))))

(define (all-lib-conversions expr)
  (closure-convertion (annotate-lib-primitives (assignment-conversion (alpha-conversion (macro-expand expr))))))

;;;; コンパイラ・メイン処理

;;; 手続き内部の式のコンパイル

;;; ret 命令を出力します。
;; @param tail retを出力するかどうか
(define (emit-ret-if tail)
  (when tail
	(emit "	lw ra, 0(sp)")
	(emit "	addi sp, sp, ~s" wordsize)
	(emit "	ret")))

;;; 式のアセンブリ表現を出力します。
(define (emit-expr si env expr)
  (emit-any-expr si env #f expr))

;;; 式を評価してsiに保存する、アセンブリ表現を出力します。
(define (emit-expr-save si env arg)
  (emit-expr si env arg)
  (emit-stack-save si))

;;; 手続き末尾の式の、アセンブリ表現を出力します。
(define (emit-tail-expr si env expr)
  (emit-any-expr si env #t expr))

;;; 式のアセンブリ表現を出力します。
;; si スタック・インデックス(stack index)
;; env 環境(environment)。変数や関数の名前と、アクセスするための位置情報のリスト
;; tail 式が手続きの末尾(tail)かどうか。
;; expr コンパイルする式(expression)
(define (emit-any-expr si env tail expr)
  (cond
   ((immediate? expr) (emit-immediate expr) (emit-ret-if tail)) ; 即値の場合は、si、envを必要としない。
   ((variable? expr)  (emit-variable-ref si env expr) (emit-ret-if tail))
   ((closure? expr)   (emit-closure si env expr) (emit-ret-if tail))
   ((if? expr)        (emit-if si env tail expr))
   ((let? expr)       (emit-let si env tail expr))
   ((begin? expr)     (emit-begin si env tail expr))
   ((quote? expr)     (emit-quote si env (quote-expr expr)) (emit-ret-if tail))
   ((primcall? expr)  (emit-primcall si env expr) (emit-ret-if tail))
   ((app? expr env)   (emit-app si env tail expr))
   (else (error "imvalid expr: " expr))))

;;; アセンブリ言語のラベルを出力します。
(define (emit-label label)
  (emit "~a:" label))

;;; 関数ヘッダのアセンブリ表現を出力します。
(define (emit-function-header f)
  (emit "")
  (emit "	.text")
  (emit "	.globl ~a" f)
  (emit "	.type ~a, @function" f)
  (emit-label f))

;;; Schemeのエントリポイントのアセンブリ表現を出力します。
(define (emit-scheme-entry expr env)
  (emit-function-header "L_scheme_entry")
  (emit "	addi sp, sp, ~s" (- wordsize))
  (emit "	sw ra, 0(sp)")
  (emit-tail-expr (- wordsize) env expr))

;;; ライブラリのアセンブリ表現を出力します。
(define (emit-library)
  (define (emit-library-primitive prim-name)
    (let ((labels (all-lib-conversions (lib-primitive-code prim-name))))
      (emit-labels labels (lambda (expr env)
			    ((emit-code env #t) (make-code '() '() expr) (primitive-alloc prim-name))))
      ;; ユーザーが上書き定義をしなければ、bssセクションに変数を確保。0に初期化される。
      (emit ".global ~s" prim-name)
      (emit ".comm ~s, 4, 4" prim-name)))
  (for-each emit-library-primitive lib-primitives))

;;; プログラムのアセンブリ表現を出力します。
;; @param program プログラム(S式一つ)
(define (emit-program program)
  (emit-function-header "scheme_entry")
  (emit "	addi sp, sp, ~s" (- (* wordsize 3)))
  (emit "	sw ra, 0(sp)")
  (emit "	sw s0, ~s(sp)" wordsize)
  (emit "	sw a1, ~s(sp)" (* wordsize 2))
  (emit "	mv s0, a0")		; heapの空きアドレスは、s0レジスタに保存する。
  (emit "	call L_scheme_entry")
  (emit "	lw ra, 0(sp)")
  (emit "	lw s0, ~s(sp)" wordsize)
  (emit "	lw a1, ~s(sp)" (* wordsize 2))
  (emit "	addi sp, sp, ~s" (* wordsize 3))
  (emit "	ret")
  (emit-labels (all-conversions program) emit-scheme-entry))

;;;; 自動テスト関連

;;; Schemeプログラムのコンパイル
(define (compile-program expr)
  (with-output-to-file (path "stst.s")
    (lambda ()
	(emit-program expr))))

;;; ライブラリのコンパイル
(define (compile-library)
  (with-output-to-file (path "lib.s")
    (lambda ()
      (emit-library)))

;;; 実行ファイルの作成
(define (build)
  (unless (zero? (process-exit-wait (run-process "make stst --quiet")))
	  (error "Could not build target.")))

;;; テスト・プログラムの実行
(define (execute)
  (unless (zero? (process-exit-wait (run-process out-to: (path "./stst.out")
						 "spike pk ./stst > ./stst.out")))
	  (error "Produced program exited abnormally.")))

;;; テスト・プログラムの実行結果の検証
(define (validate expected-output)
  (let ((executed-output (path-data (path "stst.out"))))
    (unless (string=? expected-output executed-output)
	    (error "Output mismatch for expected ~s, got ~s."
		   expected-output executed-output))))

;;; 一つのテスト・ケースをテストします。
(define (test-one expr expected-output)
  (compile-program expr)
  (build)
  (execute)
  (validate expected-output))

;;; 全てのテストケースをテストします。
(define (test-all)
  (for-each (lambda (test-case)
	      (format #t "TestCase: ~a ..." (car test-case))
	      (flush-output-port)
	      (test-one (cadr test-case) (caddr test-case))
	      (format #t " ok.\n"))
	    test-cases))
