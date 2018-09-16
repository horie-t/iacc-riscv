(import (rnrs hashtables))

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
      (let ((L (format "L_~s" count)))
	(set! count (+ count 1))
	L))))

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
  (emit "	li a0, ~s" (immediate-rep expr)))

;;;; グローバル・プロバティ
(define *prop* (make-eq-hashtable))

(define (getprop x property)
  (hashtable-ref
   (hashtable-ref *prop* x #f)
   property #f))

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
    ((_ (prime-name si arg* ...) body body* ...)
     (begin
       (putprop 'prime-name '*is-prime* #t)
       (putprop 'prime-name '*arg-count
		(length '(arg* ...)))
       (putprop 'prime-name '*emmiter*
		(lambda (si arg* ...)
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

(define (emit-primcall si expr)
  (let ((prim (car expr))
	(args (cdr expr)))
    (apply (primitive-emitter prim) si args)))

;;; 引数に1を加えた値を返します
(define-primitive (fxadd1 arg)
  (emit-expr arg)
  (emit "	addi a0, a0, ~s" (immediate-rep 1)))

;;; 引数から1を引いた値を返します
(define-primitive (fxsub1 arg)
  (emit-expr arg)
  (emit "	addi a0, a0, ~s" (immediate-rep -1)))

;;; fixnumからcharに変換します。
(define-primitive (fixnum->char arg)
  (emit-expr arg)
  (emit "	slli a0, a0, ~s" (- charshift fxshift))
  (emit "	ori  a0, a0, ~s" chartag))

;;; charからfixnumに変換します。
(define-primitive (char->fixnum arg)
  (emit-expr arg)
  (emit "	srli a0, a0, ~s" (- charshift fxshift)))

;;; fixnumかどうかを返します
(define-primitive (fixnum? arg)
  (emit-expr arg)
  (emit "	andi a0, a0, ~s" fxmask)
  (emit "	addi a0, a0, ~s" (- fxtag))
  (emit "	seqz a0, a0")
  (emit "	slli a0, a0, ~s" bool_bit)
  (emit "	ori  a0, a0, ~s" bool_f))

;;; 空リストかどうかを返します
(define-primitive (null? arg)
  (emit-expr arg)
  (emit "	andi a0, a0, ~s" emptymask)
  (emit "	addi a0, a0, ~s" (- empty_list))
  (emit "	seqz a0, a0")
  (emit "	slli a0, a0, ~s" bool_bit)
  (emit "	ori  a0, a0, ~s" bool_f))

;;; booleanオブジェクトかどうかを返します
(define-primitive (boolean? arg)
  (emit-expr arg)
  (emit "	andi a0, a0, ~s" boolmask)
  (emit "	addi a0, a0, ~s" (- is_bool))
  (emit "	seqz a0, a0")
  (emit "	slli a0, a0, ~s" bool_bit)
  (emit "	ori  a0, a0, ~s" bool_f))

;;; 文字オブジェクトかどうかを返します
(define-primitive (char? arg)
  (emit-expr arg)
  (emit "	andi a0, a0, ~s" charmask)
  (emit "	addi a0, a0, ~s" (- chartag))
  (emit "	seqz a0, a0")
  (emit "	slli a0, a0, ~s" bool_bit)
  (emit "	ori  a0, a0, ~s" bool_f))

;;; #fなら#tを返し、それ以外は#fを返します。
(define-primitive (not arg)
  (emit-expr arg)
  (emit "	addi a0, a0, ~s" (- bool_f))
  (emit "	seqz a0, a0")
  (emit "	slli a0, a0, ~s" bool_bit)
  (emit "	ori  a0, a0, ~s" bool_f))

;;;
(define-primitive (fxlognot arg)
  (emit-expr arg)
  (emit "	xori a0, a0, ~s" (immediate-rep -1)))

;;;; 二項基本演算
(define-primitive (fx+ si arg1 arg2)	; siは、stack indexの略。siが指す先は、空き領域にしてから呼び出す事
  (emit-expr si arg1)
  (emit "	sw a0, ~s(sp)" si)	; 結果をスタックに一時退避
  (emit-expr (- si wordsize) arg2)
  (emit "	lw t0, ~s(sp)" si)	; スタックに退避した値をt0に復元
  (emit "	add a0, a0, t0"))
  

;;;; 条件式

;;; if形式
;;; if形式かどうかを返します
(define (if? expr)
  (and (pair? expr) (eq? (car expr) 'if)))

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
(define (emit-if si expr)
  (let ((alt-label (unique-label))
	(end-label (unique-label)))
    (emit-expr si (if-test expr))
    (emit "	addi a0, a0, ~s" (- bool_f))
    (emit "	beqz a0, ~a" alt-label)
    (emit-expr si (if-conseq expr))
    (emit "	j ~a" end-label)
    (emit "~a:" alt-label)
    (emit-expr si (if-altern expr))
    (emit "~a:" end-label)))

;;; and形式
(define (and? expr)
  (and (pair? expr) (eq? (car expr) 'and)))

(define (emit-and si expr)
  (let ((pred-len (length (cdr expr))))
    (cond
     ((= pred-len 0)
      (emit "	li a0, ~s" bool_t))	;引数なしなら常に真
     ((= pred-len 1)
      (emit-primcall si (list 'not (cadr expr))) ; まず、(not test)の式に変換して評価する
      (emit "	xori a0, a0, ~s" (ash 1 bool_bit))) ; a0は偽かどうかの値なので、ビット反転でnotを演算する
     (else
      ;; (and test test* ...) => (if test (and test* ...) #f)と変換して処理
      (emit-if si (list 'if (cadr expr)
		            (cons 'and (cddr expr))
		            #f))))))

;;; or形式
(define (or? expr)
  (and (pair? expr) (eq? (car expr) 'or)))

(define (emit-or si expr)
  (let ((pred-len (length (cdr expr))))
    (cond
     ((= pred-len 0)
      (emit "	li a0, ~s" bool_f))	;引数なしなら常に偽
     ((= pred-len 1)
      (emit-primcall si (list 'not (cadr expr))) ; まず、(not test)の式に変換して評価する
      (emit "	xori a0, a0, ~s" (ash 1 bool_bit))) ; a0は偽かどうかの値なので、ビット反転でnotを演算する
     (else
      ;; (or test test* ...) => (if test #t (or test* ...))と変換して処理
      (emit-if si (list 'if (cadr expr)
		            #t
		            (cons 'or (cddr expr))))))))

;;;; コンパイラ・メイン処理

(define (emit-expr si expr)
  (cond
   ((immediate? expr) (emit-immediate expr)) ; 即値の場合は、siを必要としない。他はsiを必要とする処理を呼び出す可能性がある
   ((if? expr)        (emit-if si expr))
   ((and? expr)       (emit-and si expr))
   ((or? expr)        (emit-or si expr))
   ((primcall? expr)  (emit-primcall si expr))
   (else (error "imvalid expr: ~a" expr))))

(define (emit-program expr)
  (emit "	.text")
  (emit "	.globl scheme_entry")
  (emit "	.type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit-expr (- wordsize) expr)
  (emit "	ret"))

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

;;(test-one '(fx+ 4 2) "6\n")

