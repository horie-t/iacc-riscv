(define test-cases
  '(
    ("imm int posi num"  1         "1\n")
    ("imm int zero num"  0         "0\n")
    ("imm int minus"    -1        "-1\n")
    ("imm int max"       536870911 "536870911\n")
    ("imm int min"       -536870912 "-536870912\n")
    ("imm false"         #f        "#f\n")
    ("imm true"          #t        "#t\n")
    ("imm emp list"      ()       "()\n")
    ("imm char a"        #\a       "a\n")
    ("imm char z"        #\z       "z\n")
    ("imm char A"        #\A       "A\n")
    ("imm char Z"        #\Z       "Z\n")
    ("imm char !"        #\!       "!\n")
    ("imm char ~"        #\~       "~\n")
    ("unary fxadd1"        (fxadd1 0)          "1\n")
    ("unary fxsub1"        (fxsub1 0)          "-1\n")
    ("unary char->fixnum"  (char->fixnum #\A)  "65\n")
    ("unary fixnum->char"  (fixnum->char 65)   "A\n")
    ("unary not #f"        (not #f)            "#t\n")
    ("unary not #\\a"      (not #\a)           "#f\n")
    ("unary null? ()"      (null? ())          "#t\n")
    ("unary null? #\\a"    (null? #\a)         "#f\n")
    ("unary fixnum? 42"    (fixnum? 42)        "#t\n")
    ("unary fixnum? #t"    (fixnum? #t)        "#f\n")
    ("unary boolean? #t"   (boolean? #t)       "#t\n")
    ("unary boolean? #t"   (boolean? #f)       "#t\n")
    ("unary boolean? #\\t" (boolean? #\t)      "#f\n")
    ("unary char? #\\f"    (char? #\f)         "#t\n")
    ("unary char? ()"      (char? ())          "#f\n")
    ("unary fxlognot"      (fxlognot 1)        "-2\n")
    ("cond if"             (if #t #f #t)           "#f\n")
    ("cond if"             (if (char? #\a) #t #f)  "#t\n")
    ("cond and"            (and 3 #\a ())          "()\n")
    ("cond or"             (or (fixnum? #t) #f 1) "1\n")
    ("binary fx+"  (fx+ 4 2)   "6\n")
    ("binary fx-"  (fx- 4 2)   "2\n")
    ("binary fx*"  (fx* 256999 2089)   "536870911\n")
    ("binary fx complex" (fx+ (fx- (fx- 30 3) 3) (fx- 6 5)) "25\n")
    ("binary fxlogand" (fxlogand #xa #x8) "8\n")
    ("binary fxlogor"  (fxlogor #xa #x5) "15\n")
    ("binary fx="  (fx= 7 7)  "#t\n")
    ("binary fx="  (fx= 9 3)  "#f\n")
    ("binary fx<"  (fx< 9 3)  "#f\n")
    ("binary fx<=" (fx<= 5 5) "#t\n")
    ("binary fx>"  (fx> 4 3)  "#t\n")
    ("binary fx>=" (fx>= 4 5) "#f\n")
    ("binary char="  (char= #\z #\z)  "#t\n")
    ("binary char="  (char= #\! #\@)  "#f\n")
    ("binary char<"  (char< #\Z #\Y)  "#f\n")
    ("binary char<=" (char<= #\A #\A) "#t\n")
    ("binary char>"  (char> #\b #\a)  "#t\n")
    ("binary char>=" (char>= #\b #\c) "#f\n")
    ("let" (let ((a (fx* 4 4))
		 (b (fx* 3 3)))
	     (fx+ a b)) "25\n")
    ("let*" (let* ((x 1))
	      (let* ((x (fx+ x 1))
		     (y (fx+ x 1)))
		y))    "3\n")
    ("letrec" (letrec ((fact (lambda (n)
			       (if (fx= n 1)
				   1
				   (fx* n (fact (fx- n 1)))))))
		(fact 5)) "120\n")
    ("heap car"    (car (cons 1 2)) "1\n")
    ("heap cdr"    (cdr (cons 1 2)) "2\n")
    ("heap pair?" (pair? (cons 1 2))   "#t\n")
    ("heap pair? not" (pair? 1)        "#f\n")
    ("begin" (begin (fx+ 1 2) (fx- 2 1)) "1\n")
    ("body seq" (let ((apair (cons 1 2)))
		  (cons 3 apair)
		  (car apair))     "1\n")
    ("set-car!" (let ((apair (cons 1 2)))
		  (set-car! apair 3)
		  (car apair))     "3\n")
    ("set-cdr!" (let ((apair (cons 1 2)))
		  (set-cdr! apair 5)
		  (cdr apair))     "5\n")
    ("eq? num"  (eq? 1 1)          "#t\n")
    ("eq? obj"  (let ((pair-a (cons 1 2))
		      (pair-b (cons 1 2)))
		  (eq? pair-a pair-b))          "#f\n")
    ("heap vector make"   (let ((vec (make-vector 3)))
			    (vector? vec))     "#t\n")
    ("heap vector length" (let ((vec (make-vector 2)))
			    (vector-length vec))  "2\n")
    ("heap vector set"    (let ((vec (make-vector 4)))
			    (vector-set! vec 0 #\a)
			    (vector-ref vec 0)) "a\n")
    ("heap string make" (let ((str (make-string 5)))
			  (string? str))              "#t\n")
    ("heap string?"     (string? ())                  "#f\n")
    ("heap string set!" (let ((str (make-string 4)))
			  (string-set! str 3 #\z)
			  (string-ref str 3))         "z\n")
    ("closure" (let ((n 12))
		 (let ((f (lambda (m) (fx+ n m))))
		   (f 100)))                          "112\n")
    ("set! " (let ((f (lambda (c)
			(cons (lambda (v) (set! c v))
			      (lambda () c)))))
	       (let ((lambda-pair (f 0)))
		 ((car lambda-pair) 12)
		 ((cdr lambda-pair))))                "12\n")
    ))
