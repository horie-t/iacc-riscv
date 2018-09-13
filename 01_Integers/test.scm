(define (emit . args)
  (apply format #t args)
  (newline))

(define (emit-program x)
  (unless (integer? x) (error "not integer"))
  (emit "	.text")
  (emit "	.globl scheme_entry")
  (emit "	.type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "	li a0, ~s" x)
  (emit "	ret"))

(emit-program 42)
