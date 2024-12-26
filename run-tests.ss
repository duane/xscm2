(top-level-program
 (import (chezscheme)
	 (unit-test)
	 (arch aarch64 registers)
         (arch aarch64 literal)
	 (arch aarch64 operand-selector)
	 )

 ;; (define (load-annotated path)
 ;;   (let ([buffer '()])
 ;;     (load path (lambda (expr) (set! buffer (cons expr buffer)))) (reverse buffer)))

 ;; (define literals (apply vector (load-annotated "test/aarch64/literals.ss")))
 ;; (define immediate (vector-ref literals 0))
 ;; (define literal-symbol (vector-ref literals 1))
 ;; (define reg-immediate (vector-ref literals 2))
 ;; (define sp-immediate (vector-ref literals 3))
 ;; (define reg-deref (vector-ref literals 4))
 ;; (define reg-imm-deref (vector-ref literals 5))
 ;; (define reg-prex-deref (vector-ref literals 6))
 ;; (define reg-reg-deref (vector-ref literals 7))

 ((suite "some test suite name"
	 (test "some test thing" (expect 0 0)))


  ;; 
  ;; 
					;(display "hello world")
  )
 )
