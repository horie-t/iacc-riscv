;;;; ユーティリティ関数

;;; 書式と引数を取って表示し、改行を付け加えます。
;; 例: (emit "addi t0, t0, ~s" 1)
;; addi t0, t0, 1
;; が表示されます。
(define (emit . args)
  (apply format #t args)
  (newline))

;;;; オブジェクト型情報定義: タグ付きポインタ表現を使う

;;; 整数: 下位2ビットが00、上位30ビットが符号付き整数となっている
(define fxshift 2)			; 整数変換シフト量
(define fxmask #x03)			; 整数判定ビットマスク(ANDを取って0なら整数オブジェクト)

;;; boolean: 
(define bool_f #x2f)			; #fの数値表現 
(define bool_t #x6f)			; #t
(define boolmask #xbf)			; boolean判定ビットマスク(ANDを取ってis_boolならbooleanオブジェクト)
(define is_bool #x2f)			; 

;;; 空リスト:
(define empty_list #x3f)		; 

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

(define (immediate? x)
  (or (fixnum? x) (boolean? x)))

(define (immediate-rep x)
  (cond
   [(fixnum? x) (ash x fxshift)]
   [(eq? x #f) (bool_f)]
   [(eq? x #t) (bool_t)]
   [(char? x) (logior (ash (char->integer x) charshift) charmask)]
   [(null? x) ()]
   (else (error "invalid immediate"))))

(define (emit-program x)
  (unless (immediate? x) (error "not integer"))
  (emit "	.text")
  (emit "	.globl scheme_entry")
  (emit "	.type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "	li a0, ~s" (immediate-rep x))
  (emit "	ret"))

(emit-program 42)
