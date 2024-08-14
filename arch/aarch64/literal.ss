(library (arch aarch64 literal)
  (export parse-literal)
  (import (chezscheme)
	  (asm errors)
	  (arch aarch64 registers))

  (define ae! annotation-expression)
  (define-syntax with-annotation
    (syntax-rules ()
      [(_ (annotation) body ...)
       (guard (con [(not )]))]))
  
  (define (parse-immediate expr)
    (with-assembler-source (lit-expr expr)
      (if (number? lit-expr) (values 'immediate lit-expr expr) #f)))
  (define (parse-symbol expr)
    (with-assembler-source (lit-expr expr)
      (if (symbol? lit-expr) (values 'symbol lit-expr expr) #f)))
  (define (parse-reg expr)
    (with-assembler-source (reg-expr expr)
      (let ([result (parse-register reg-expr)])
	(if result (values 'register result expr) #f))))
  (define (parse-deref expr)
    (with-assembler-source (list-expr expr)
      (if (not (and (list? list-expr)
		    (null? (cdr list-expr)))) #f
	  (let ([child-annotation (car list-expr)]) 
	    (with-assembler-source (reg-expr child-annotation)
				   (assert (not (annotation? reg-expr)))
				   (let ([result (parse-register reg-expr)])
				     (if result (values 'deref result expr) #f)))
	    ))))
  
  (define (parse-literal annotated)
    (with-assembler-source (expr annotated)
      (cond [(number? expr) (values 'immediate expr annotated)]
	    [(symbol? expr)
	     (let ([reg-result (parse-register expr)])
	       (if reg-result
		   (values 'register reg-result annotated)
		   (values 'symbol expr annotated)))]
	    [(list? expr)
	     (let ([first-arg-annotated (car expr)])
	       (with-assembler-source (first-expr first-arg-annotated)
		 (let ([reg-result (parse-register first-expr)])
		   (if (and reg-result (null? (cdr expr)))
		       (values 'deref reg-result annotated)))))])))
  
  )
