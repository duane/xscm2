(import (chezscheme)
	(unit-test)
	(arch aarch64 registers)
        (arch aarch64 literal)
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

(define-syntax an
  (syntax-rules ()
    [(_ expr) #'expr]))

((suite registers
	(test parse-xn (let ([register (parse-register! '%x17)])
			 (expect 64 (register-size register))
			 (expect '%x17 (register-name register))
			 (expect 17 (register-numeric register))))
	(test parse-wp (let ([register (parse-register! '%w17)])
			 (expect 32 (register-size register))
			 (expect '%w17 (register-name register))
			 (expect 17 (register-numeric register))))))


((suite literals
	(test parse-immediate
	      (let-values ([(type expr _) (parse-literal immediate)])
		(expect '(immediate 42) (list type expr))))
	(test parse-immediate-f-symbol (expect #f (parse-immediate literal-symbol)))
	(test parse-immediate-f-deref (expect #f (parse-immediate reg-deref)))
	(test parse-symbol
	      (let-values ([(type expr _) (parse-literal literal-symbol)])
		(expect '(symbol symbol) (list type expr))))
	(test parse-reg
	      (let-values ([(type expr _) (parse-literal reg-immediate)])
		(expect `(register ,(parse-register! '%w24)) (list type expr))))
	(test parse-deref
	      (let-values ([(type expr _) (parse-literal reg-deref)])
		(expect `(deref ,(parse-register! '%w2)) (list type expr))))
	))

;; (suite parse-literal
;;        (test parse-imm (parse-literal (an 42))))



