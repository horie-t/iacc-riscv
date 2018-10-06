(import (srfi 1))
(import (rnrs hashtables))

(load "test_cases.scm")

;;;; ユーティリティ関数

;;; 書式と引数を取って表示し、改行を付け加えます。
;; 例: (emit "addi t0, t0, ~s" 1)
;; addi t0, t0, 1
;; が表示されます。
(define (emit . args)
  (apply format #t args)
  (newline))

;;; ユニーク・ラベル生成
;; 重複のない、ラベルを返します。
(define unique-label
  (let ((count 0))
    (lambda ()
      (let ((L (string->symbol (format "L_~s" count)))) ; シンボルに変換する必要はある?
	(set! count (+ count 1))
	L))))

;;; ユニーク・ラベルのリストを生成します。
;; vars ラベルの一部を形成する文字のリスト
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

;;; 値比較ユーティリティ
;; a0と値を比較し、booleanオブジェクトをa0に設定します。
;; args 比較する即値、省略時はt0と比較
(define (emit-cmp-bool . args)
  (if (null? args)
      (emit "	sub a0, a0, t0")
      (emit "	addi a0, a0, ~s" (- (car args))))
  (emit "	seqz a0, a0")
  (emit "	slli a0, a0, ~s" bool_bit)
  (emit "	ori  a0, a0, ~s" bool_f))

;;; 式の書式判定ユーティリティ
;; (tag ....) の形式かどうかを判定します。
(define (tagged-form? tag expr)
  (and (list? expr) (not (null? expr)) (eq? (car expr) tag)))

;;;; スタック関連
;;; スタックに値を保存します。
;; si スタック・インデックス
(define (emit-stack-save si)
  (emit "	sw a0, ~s(sp)" si))

(define (emit-stack-load si)
  (emit "	lw a0, ~s(sp)" si))

(define (emit-stack-load-t0 si)
  (emit "	lw t0, ~s(sp)" si))

;;; 次のスタックインデックスを返します。
(define (next-stack-index si)
  (- si wordsize))

;;; スタック・ポインタを移動させます。
(define (emit-adjust-base si)
  (emit "	addi sp, sp, ~s" si))

;;;; 環境関連
;;; 環境を生成します。
;; bindings 初期値。 '((a -8) (b -9))のような、シンボル、スタック・インデックスのリストのリスト。
;;    何もなければ空リストを渡す
(define (make-initial-env bindings)
  bindings)

;;; binding部分を生成します。
(define (bind lhs rhs)
  (list lhs rhs))

;;; bindingの束縛されるシンボル(left hand side)を返します
(define (lhs expr)
  (car expr))

;;; bindingの値(right hand side)を返します
(define (rhs expr)
  (cadr expr))

;;; 環境に変数を追加します。
;; var 変数のシンボル
;; si スタック・インデックス
;; env 変数を追加する環境
(define (extend-env var si env)
  (cons (list var si) env))

;;; 環境に変数を追加します。
(define (bulk-extend-env vars vals env)
  (append (map list vars vals) env))

;;; 環境から変数の値を検索します
(define (lookup var env)
  (cond
   ((assv var env)
    (cadr (assv var env)))
   (else #f)))

;;;; オブジェクト型情報定義: タグ付きポインタ表現を使う

;;; 整数: 下位2ビットが00、上位30ビットが符号付き整数となっている
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

(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (- (expt 2 (- fixnum-bits 1))
		   1))
(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))


;;;; 即値関連関数

;;; 即値かどうかを返します。
(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

;;; Schemeの即値から、アセンブリ言語でのオブジェクト表現を返します。
(define (immediate-rep x)
  (cond
   ((fixnum? x) (ash x fxshift))
   ((eq? x #f) bool_f)
   ((eq? x #t) bool_t)
   ((char? x) (logior (ash (char->integer x) charshift) chartag))
   ((null? x) empty_list)
   (else (error "invalid immediate"))))

;;; 即値表現から、アセンブリ言語を出力します。
(define (emit-immediate expr)
  (let ((imm (immediate-rep expr)))
    (when (>= imm 4096)
	  (emit "	lui a0, ~s" (ash imm -12)))
    (emit "	li a0, ~s" imm)))

;;;; グローバル・プロバティ
(define *prop* (make-eq-hashtable))

(define (getprop x property)
  (let ((prop (hashtable-ref *prop* x #f)))
    (if prop
	(hashtable-ref prop property #f)
	#f)))

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

;;; プリミティブ定義(*porp*にプリミティブ関連の情報を追加)
;; 例: (define-primitive (fxadd1 arg)
;;       出力内容 ...)
(define-syntax define-primitive
  (syntax-rules ()
    ((_ (prime-name si env arg* ...) body body* ...)
     (begin
       (putprop 'prime-name '*is-prime* #t)
       (putprop 'prime-name '*arg-count
		(length '(arg* ...)))
       (putprop 'prime-name '*emmiter*
		(lambda (si env arg* ...)
		  body body* ...))))))

;;; 引数が基本演算かどうかを返します。
; xは、add1のようにシンボルで、*is-prime*が#tにセットされている必要がある
(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prime*)))

(define (primitive-emitter x)
  (or (getprop x '*emmiter*) (error "primitive-emitter: not exist emmiter")))

;;;; 単項演算関連

;;; 単項演算呼び出し(単項演算処理)かどうかを返します。
;; 単項演算呼び出しは(op arg)の形式なので、最初はpairで、carがprimitive?がtrueを返すものでなければならない。
(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (emit-primcall si env expr)
  (let ((prim (car expr))
	(args (cdr expr)))
    (apply (primitive-emitter prim) si env args)))

;;; 引数に1を加えた値を返します
(define-primitive (fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "	addi a0, a0, ~s" (immediate-rep 1)))

;;; 引数から1を引いた値を返します
(define-primitive (fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "	addi a0, a0, ~s" (immediate-rep -1)))

;;; fixnumからcharに変換します。
(define-primitive (fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "	slli a0, a0, ~s" (- charshift fxshift))
  (emit "	ori  a0, a0, ~s" chartag))

;;; charからfixnumに変換します。
(define-primitive (char->fixnum si env arg)
  (emit-expr si env arg)
  (emit "	srli a0, a0, ~s" (- charshift fxshift)))

;;; fixnumかどうかを返します
(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit "	andi a0, a0, ~s" fxmask)
  (emit-cmp-bool fxtag))

;;; 空リストかどうかを返します
(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit "	andi a0, a0, ~s" emptymask)
  (emit-cmp-bool empty_list))

;;; booleanオブジェクトかどうかを返します
(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit "	andi a0, a0, ~s" boolmask)
  (emit-cmp-bool is_bool))

;;; 文字オブジェクトかどうかを返します
(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit "	andi a0, a0, ~s" charmask)
  (emit-cmp-bool chartag))

;;; #fなら#tを返し、それ以外は#fを返します。
(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit-cmp-bool bool_f))

;;;
(define-primitive (fxlognot si env arg)
  (emit-expr si env arg)
  (emit "	xori a0, a0, ~s" (immediate-rep -1)))

;;;; 二項基本演算

;;; 二項基本演算ユーティリティ
;; arg1、arg2を評価し、結果をそれぞれt0、a0レジスタに代入します
(define (emit-binop si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-stack-save si)			; 結果をスタックに一時退避
  (emit-expr (next-stack-index si) env arg2)
  (emit-stack-load-t0 si))		      ; スタックに退避した値をt0に復元

;;; 整数加算
(define-primitive (fx+ si env arg1 arg2)	; siは、stack indexの略。siが指す先は、空き領域にしてから呼び出す事
  (emit-binop si env arg1 arg2)    
  (emit "	add a0, a0, t0"))

;;; 整数減算
(define-primitive (fx- si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "	sub a0, t0, a0"))

;;; 整数積
(define-primitive (fx* si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "	sra a0, a0, ~s" fxshift)
  (emit "	mul a0, t0, a0"))

;;; 整数ビット論理積
(define-primitive (fxlogand si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "	and a0, a0, t0"))

;;; 整数ビット論理和
(define-primitive (fxlogor si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "	or a0, a0, t0"))

;;; 整数等号
(define-primitive (fx= si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit-cmp-bool))

;;; 整数小なり
(define-primitive (fx< si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "       slt a0, t0, a0")
  (emit "	slli a0, a0, ~s" bool_bit)
  (emit "	ori  a0, a0, ~s" bool_f))

;;; 整数以下
(define-primitive (fx<= si env arg1 arg2)
  (emit-expr si env (list 'fx< arg2 arg1)) ; 大なりを判定して、あとで否定する
  (emit "	xori a0, a0, ~s" (ash 1 bool_bit)))

;;; 整数大なり
(define-primitive (fx> si env arg1 arg2)
  (emit-expr si env (list 'fx< arg2 arg1)))	; 引数を逆にして、小なりを使う

;;; 整数以上
(define-primitive (fx>= si env arg1 arg2)
  (emit-expr si env (list 'fx< arg1 arg2)) ; 小なりを判定して、あとで否定する
  (emit "	xori a0, a0, ~s" (ash 1 bool_bit)))

;;; 文字等号
(define-primitive (char= si env arg1 arg2)
  (emit-binop si env arg1 arg2)	; 型判定をしていないので、fx=と同じ内容。eq?をこれにしてOKかも
  (emit-cmp-bool))

;;; 整数小なり
(define-primitive (char< si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "       slt a0, t0, a0")
  (emit "	slli a0, a0, ~s" bool_bit)
  (emit "	ori  a0, a0, ~s" bool_f))

;;; 整数以下
(define-primitive (char<= si env arg1 arg2)
  (emit-expr si env (list 'char< arg2 arg1)) ; 大なりを判定して、あとで否定する
  (emit "	xori a0, a0, ~s" (ash 1 bool_bit)))

;;; 整数大なり
(define-primitive (char> si env arg1 arg2)
  (emit-expr si env (list 'char< arg2 arg1)))	; 引数を逆にして、小なりを使う

;;; 整数以上
(define-primitive (char>= si env arg1 arg2)
  (emit-expr si env (list 'char< arg1 arg2)) ; 小なりを判定して、あとで否定する
  (emit "	xori a0, a0, ~s" (ash 1 bool_bit)))

;;;; 特殊形式関連
;;; 特殊形式、プリミティブのシンボルかどうかを判定します。
(define (special? symbol)
  (or (member symbol '(if begin let lambda closure set!))
      (primitive? symbol)))

;;;; 条件式

;;; if形式
;;; if形式かどうかを返します
(define (if? expr)
  (and (tagged-form? 'if expr)
       (or (= (length (cdr expr)) 3)
	   (error "if? " (format #t "malformed if ~s" expr)))))

;;; if形式の述部(predicate)を取り出します。
(define (if-test expr)
  (cadr expr))

;;; if形式の帰結部(consequent)を取り出します。
(define (if-conseq expr)
  (caddr expr))

;;; if形式の代替部(alternative)を取り出します。
(define (if-altern expr)
  (cadddr expr))

;;; if形式の出力
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

;;; and形式
(define (and? expr)
  (tagged-form? 'and expr))

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

;;; or形式
(define (or? expr)
  (tagged-form? 'or expr))

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
(define (let? expr)
  (tagged-form? 'let expr))

;;; letの形式の詳細を返します。
(define (let-kind expr)
  (car expr))

;;; let形式のbinding部分を返します。
(define (let-bindings expr)
  (cadr expr))

(define make-let list)


;;; let形式のbody部分を返します。body部分が複数の式からなる時は、begin形式に変換します
(define (let-body expr)
  (if (null? (cdddr expr))
      (caddr expr)
      (make-begin (cddr expr))))

(define (let-body-seq expr)
  (cddr expr))

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

;;;; let*形式
;;; let*形式かどうかを返します。
(define (let*? expr)
  (tagged-form? 'let* expr))

;;;
;; let*は、letの入れ子に書き換えてしまう。
;; 例)
;; (let* ((x 1))                       (let ((x 1))
;;  (let* ((x (fx+ x 1))        =>       (let ((x (fx+ x 1)))
;;         (y (fx+ x 1)))                  (let ((y (fx+ x 1)))
;;     y))                                   y)))
(define (emit-let* si env tail expr)
  (emit-any-expr si env tail
		 (let ((bindings (let-bindings expr))
		       (body (let-body expr)))
		   (cond
		    ((<= (length bindings) 1)
		     (list 'let bindings
			   body))
		    (else
		     (list 'let (list (car bindings))
			   (list 'let* (cdr bindings)
				 body)))))))

;;;; letrec形式
;;; letrec形式かどうかを返します。
(define (letrec? expr)
  (or (tagged-form? 'letrec expr) (tagged-form? 'letrec* expr)))


;;; let系形式のどれかか、どうかを返します。
(define (any-let? expr)
  (and (pair? expr)
       (member (car expr) '(let let* letrec))))

;;;; begin形式
;;; begin形式かどうかを返します。
(define (begin? expr)
  (tagged-form? 'begin expr))

(define (emit-begin si env tail expr)
  (emit-seq si env tail (cdr expr)))

;; 連続した式を出力します
;; seq 連続した式。例: ((set-car! some-pair 7) (set-cdr! come-pair 5) some-pair) 
(define (emit-seq si env tail seq)
  (cond
   ((null? seq) (error "empty seq"))
   ((null? (cdr seq))			; 連続式の末尾の場合
    (emit-any-expr si env tail (car seq)))
   (else				; 連続式の途中の場合
    (emit-expr si env (car seq))
    (emit-seq si env tail (cdr seq)))))

(define (make-begin lst)
  (if (null? (cdr lst))
      (car lst)
      (cons 'begin lst)))

(define (make-body lst)
  (make-begin lst))

;;;; 変数参照関連
;; 変数の表現書式は以下の3通りあります。
;; 数値: スタック上の変数
;; ('free offset): 自由変数。offsetは、クロージャ・オブジェクト内でのオフセット
;; シンボル: クロージャを指している

;;; 変数かどうかを返します。
(define (variable? expr)
  (symbol? expr))

;;; 自由変数を作成します。
;; offset クロージャ・オブジェクト内でのオフセット
(define (free-var offset)
  (list 'free (- offset closuretag)))

;;; 変数が自由変数かどうかを判定します。
;; var 判定対象変数
(define (free-var? var)
  (tagged-form? 'free var))

;;; 式の中から自由変数を取得します。
(define (get-free-vars expr)
  (cond
   ((variable? expr)
    (list expr))
   ((lambda? expr)
    (filter (lambda (v)
	      (not (member v (cadr expr))))
	    (get-free-vars (caddr expr))))
   ((let? expr)
    (append (append-map get-free-vars (map cadr (let-bindings expr)))
	    (filter (lambda (v)
		      (not (member v (map car (let-bindings expr)))))
		    (get-free-vars (let-body expr)))))
   ((list? expr)
    (append-map get-free-vars (if (and (not (null? expr))
				       (special? (car expr)))
				  (cdr expr)
				  expr)))
   (else '())))

;;;
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
;;; set!特殊形式かどうかを返します
(define (set? expr)
  (tagged-form? 'set! expr))

;;; set!特殊形式を生成します。
;; lhs (left hand side)
;; rhs (right hand side)
(define (make-set! lhs rhs)
  (list 'set! lhs rhs))

;;; set!特殊形式の代入されるシンボルを返します
(define (set-lhs expr)
  (cadr expr))

;;; set!特殊形式の代入する式を返します。
(define (set-rhs expr)
  (caddr expr))

;;;; lambda特殊形式
;;; lamda特殊形式かどうかを返します。
(define (lambda? expr)
  (tagged-form? 'lambda expr))

;;; lambda特殊形式の引数部分を返します。
(define (lambda-formals expr)
  (cadr expr))

;;; lambda特殊形式の本体部分を返します。
(define (lambda-body expr)
  (make-body (cddr expr)))

;;; lambda特殊形式を生成します。
;; formals 引数リスト
;; body 本体
(define (make-lambda formals body)
  (list 'lambda formals body))

;;;; code特殊形式
(define (make-code formals free-vars body)
  (list 'code formals free-vars body))

(define (emit-code env)
  (lambda (expr label)
    (emit-function-header label)
    (emit "	addi sp, sp, ~s" (- wordsize))
    (emit "	sw ra, 0(sp)")
    (let ((formals (cadr expr))
	  (free-vars (caddr expr))
	  (body (cadddr expr)))
      (extend-env-with (- wordsize) env formals
		       (lambda (si env)
			 (close-env-with wordsize env free-vars
					 (lambda (env)
					   (emit-tail-expr si env body))))))))

;;; 手続き呼び出しの引数で環境を拡張して、kを実行します。
;; lvars 引数のリスト
;; k 拡張された環境で実行したい手続き? thunk?
(define (extend-env-with si env lvars k)
  (if (null? lvars)
      (k si env)
      (extend-env-with (next-stack-index si)
		       (extend-env (car lvars) si env)
		       (cdr lvars)
		       k)))

;;; 自由変数で環境を拡張して、kを実行します。 (closure対応に必要)
;; offset クロージャ・オブジェクトの開始アドレスからのオフセット
;; lvars 自由変数のリスト
(define (close-env-with offset env lvars k)
  (if (null? lvars)
      (k env)
      (close-env-with (+ offset wordsize)
		      (extend-env (car lvars) (free-var offset) env)
		      (cdr lvars)
		      k)))


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
    (emit-call)
    (emit-adjust-base (- si))
    (emit "	lw a1, ~s(sp)" si))
   (else				; tail
    (emit-arguments si (cdr expr))
    (emit-expr (- si (* wordsize (length (cdr expr)))) env (car expr))
    (emit "	mv a1, a0")
    (move-arguments si (- (+ si wordsize)) (cdr expr))
    (emit "	mv a0, a1")
    (emit-heap-load (- closuretag))
    (emit-jmp-tail))))

;;; Scheme手続きに対応する、アセンブリ言語のラベルを返します。見つからなかった場合は#fを返します。
(define (proc expr env)
  (and (variable? expr)
       (let ((val (lookup expr env)))
	 (and (symbol? val) val))))

;;;; ヒープ領域オブジェクト関連
(define objshift 2)
(define objmask #x07)

;;; ヒープメモリ確保時の最低サイズ(バイト)
(define heap-cell-size (ash 1 objshift))

;;; ヒープメモリを確保します。確保したアドレスはa0に設定
;; size 確保するバイト数
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
;; si コピー元の値のスタックインデックス
;; offset a0+offset のアドレスに値をコピーします。
(define (emit-stack-to-heap si offset)
  (emit "	lw t0, ~s(sp)" si)
  (emit "	sw t0, ~s(a0)" offset))

;;; ヒープの値をa0に読み込みます。
;; offset a0+offset のアドレスの値を読み込みます
(define (emit-heap-load offset)
  (emit "	lw a0, ~s(a0)" offset))

;;; オブジェクトの型判定をします。
;; tag オブジェクトの型タグ
(define (emit-object? tag si env arg)
  (emit-expr si env arg)
  (emit "	andi a0, a0, ~s" objmask)
  (emit-cmp-bool tag))

;;;; eq?
(define-primitive (eq? si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit-cmp-bool))

;;;; ペア関連
(define pairtag #b001)			; ペアのタグ
(define pairsize 8)			; ペアのメモリサイズ(バイト)
(define paircar 0)			; ペア中のcar部分のオフセット
(define paircdr 4)			; ペア中のcdr部分のオフセット

;;; cons
(define-primitive (cons si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit-stack-save (next-stack-index si))
  (emit-heap-alloc pairsize)
  (emit "	ori a0, a0, ~s" pairtag)
  (emit-stack-to-heap si (- paircar pairtag))
  (emit-stack-to-heap (next-stack-index si) (- paircdr pairtag)))

;;; pair?
(define-primitive (pair? si env arg)
  (emit-object? pairtag si env arg))

;;; car
(define-primitive (car si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- paircar pairtag)))

;;; cdr
(define-primitive (cdr si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- paircdr pairtag)))

;;; set-car!
(define-primitive (set-car! si env cell val)
  (emit-binop si env val cell)
  (emit-stack-to-heap si (- paircar pairtag)))

;;; set-cdr!
(define-primitive (set-cdr! si env cell val)
  (emit-binop si env val cell)
  (emit-stack-to-heap si (- paircdr pairtag)))

;;;; ベクトル関連
(define vectortag #x05)			; ベクトルのタグ

;;; ベクトルを作成します。
;; length ベクトルの要素数
(define-primitive (make-vector si env length)
  (emit-expr si env length)
  (emit-stack-save si)
  (emit "	addi a0, a0, ~s" (ash 1 fxshift)) ; 要素数+1のセルを確保する。+1はlengthデータ保存用
  (emit "	slli a0, a0, ~s" wordshift)	  ; 要素数 -> バイト数へ変換
  (emit-heap-alloc-dynamic)
  (emit-stack-to-heap si 0)
  (emit "	ori a0, a0, ~s" vectortag))

;;; ベクトルかどうかを返します。
(define-primitive (vector? si env arg)
  (emit-object? vectortag si env arg))

;;; ベクトルの要素数を返します。
(define-primitive (vector-length si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- vectortag)))	; タグの値の分だけアドレスをずらす

;;; ベクトルに値をセットします。
;; vector セットされるベクトル
;; index セットする位置
;; value セットする値
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
(define stringtag   #x06)

;;; 文字列を作成します。
(define-primitive (make-string si env length)
  (emit-expr-save si env length)
  (emit "	srai a0, a0, ~s" fxshift)
  (emit "	addi a0, a0, ~s" wordsize)
  (emit-heap-alloc-dynamic)
  (emit-stack-to-heap si 0)
  (emit "	ori a0, a0, ~s" stringtag))

;;; 文字列かどうかを返します。
(define-primitive (string? si env arg)
  (emit-object? stringtag si env arg))

;;; 文字列の長さを返します。
(define-primitive (string-length si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- stringtag)))

;;; 文字列に文字をセットします。
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

;;; 文字列の文字を参照します。
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

;;;; クロージャ・オブジェクト関連
(define closuretag  #x02)		; クロージャ・オブジェクトタグ

;;; クロージャ特殊形式を作成します。
(define (make-closure label free-vars)
  (cons 'closure (cons label free-vars)))

;;; クロージャかどうかを返します
(define (closure? expr)
  (tagged-form? 'closure expr))

;;; 
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

;;; マクロ変換
(define (macro-expand expr)
  (define (transform expr bound-vars)
    (cond
     ((set? expr)
      (make-set! (set-lhs expr) (transform (set-rhs expr) bound-vars)))
     ((lambda? expr)
      (make-lambda
       (lambda-formals expr)
       (transform (lambda-body expr)
		  (append (lambda-formals expr) bound-vars))))
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
       (list 'if (cadr expr)
	         (make-begin cddr expr)
	         #f)
       bound-vars))
     ((tagged-form? 'unless expr)
      (transform
       (append (list 'when ('not cadr expr))
	       (cddr expr))
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
	  (error "alpha-conversion: " (format #t "undefined variable ~s" expr))))
     ((lambda? expr)			; lamdaの引数名をユニークにする
      (let ((new-env (bulk-extend-env	; lambdaの引数に対応するユニークな名前を環境に追加
		      (lambda-formals expr)
		      (map unique-name (lambda-formals expr))
		      env)))
	(make-lambda			; ユニークな名前を使って、lambda式を作り直す
	 (map (lambda (v)
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
	(let ((vars (filter variable-assigned (lambda-formals expr))))
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
 (closure-convertion (assignment-conversion (alpha-conversion (macro-expand expr)))))

;;;; コンパイラ・メイン処理

;;; 手続き内部の式のコンパイル
(define (emit-ret-if tail)
  (when tail
	(emit "	lw ra, 0(sp)")
	(emit "	addi sp, sp, ~s" wordsize)
	(emit "	ret")))
  
(define (emit-expr si env expr)
  (emit-any-expr si env #f expr))

;;; 式を評価して、siに保存します。
(define (emit-expr-save si env arg)
  (emit-expr si env arg)
  (emit-stack-save si))

;;; 手続き末尾の式のコンパイル
(define (emit-tail-expr si env expr)
  (emit-any-expr si env #t expr))

;;; 式をコンパイルします。
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
   ((and? expr)       (emit-and si env expr) (emit-ret-if tail))
   ((or? expr)        (emit-or si env expr) (emit-ret-if tail))
   ((let? expr)       (emit-let si env tail expr))
   ((begin? expr)    (emit-begin si env tail expr))
   ((primcall? expr)  (emit-primcall si env expr) (emit-ret-if tail))
   ((app? expr env)   (emit-app si env tail expr))
   (else (error "imvalid expr: " expr))))

(define (emit-label label)
  (emit "~a:" label))

(define (emit-function-header f)
  (emit "")
  (emit "	.text")
  (emit "	.globl ~a" f)
  (emit "	.type ~a, @function" f)
  (emit-label f))

(define (emit-call . labels)
  (cond
   ((null? labels)
    (emit "	jalr a0"))
   (else
    (emit "	call ~a" (car labels)))))

;;; 末尾呼び出しの最後のジャンプ
(define (emit-jmp-tail . labels)
  (emit "	addi sp, sp, ~s" wordsize)
  (cond
   ((null? labels)
    (emit "	jr a0"))
   (else
    (emit "	j ~a" (car labels)))))

;;;;
(define (emit-scheme-entry expr env)
  (emit-function-header "L_scheme_entry")
  (emit "	addi sp, sp, ~s" (- wordsize))
  (emit "	sw ra, 0(sp)")
  (emit-tail-expr (- wordsize) env expr))

;;;;
(define (emit-labels labels-expr)
  (let* ((bindings (let-bindings labels-expr))
	 (labels (map car bindings))
	 (codes (map cadr bindings))
	 (env (make-initial-env '())))
    (for-each (emit-code env) codes labels)
    (emit-scheme-entry (caddr labels-expr) env)))

;;;; 
(define (emit-top top)
  (emit-labels (car top) (cadr top)))

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
  (emit-labels (all-conversions program)))

;;;; 自動テスト関連

;;; Schemeプログラムのコンパイル
(define (compile-program expr)
  (with-output-to-file (path "stst.s")
    (lambda ()
	(emit-program expr))))

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
