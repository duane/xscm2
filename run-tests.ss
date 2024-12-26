(top-level-program
 (import (chezscheme)
	 (unit-test)
	 (arch aarch64 registers)
         (arch aarch64 literal)
	 (arch aarch64 operand-selector)
	 )

 (define (load-annotated path)
   (let ([buffer '()])
     (load path (lambda (expr) (set! buffer (cons expr buffer)))) (reverse buffer)))

 (define literals (apply vector (load-annotated "test/aarch64/literals.ss")))
  (define immediate (vector-ref literals 0))
  (define literal-symbol (vector-ref literals 1))
  (define reg-immediate (vector-ref literals 2))
  (define sp-immediate (vector-ref literals 3))
  (define reg-deref (vector-ref literals 4))
  (define reg-imm-deref (vector-ref literals 5))
  (define reg-prex-deref (vector-ref literals 6))
  (define reg-reg-deref (vector-ref literals 7))

 ((suite "operand selector"
	 (test "immediate"
	       (expect #t (select-operand 42 (parse-imm-selector 'uimm16))))
	 (test "register-arbitrary-w"
	       (expect #t (select-operand '%w14 (parse-reg-selector *w-or-sp*))))
	 (test "register-sp"
	       (expect #t (select-operand '%sp (parse-reg-selector *w-or-sp*)))))
  )
 )
