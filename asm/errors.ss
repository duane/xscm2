(library (asm errors)
  (export raise-assembler-error with-assembler-source)
  (import (chezscheme))


  (define-condition-type
    &assembler-source
    &condition
    make-assembler-source-condition
    assembler-source-condition?
    (fields annotated-expr))


  (define-syntax raise-assembler-error
    (lambda (x)
      (syntax-case x ()
                    [(_ msg)
                     #'(raise (condition (make-error)
					 (make-message-condition msg)))]
                    [(_ msg #f)
                     #'(raise (condition (make-error)
					 (make-message-condition msg)))]
                    [(_ msg where)
                     #'(raise (condition (make-error)
					 (make-message-condition msg)
					 (make-assembler-source-condition where)))])
      )
    )

  (define-syntax with-assembler-source
    (syntax-rules ()
      [(_ (expr annotated-expr) body ...)
       (let* ([full-annotated-expr annotated-expr]
	      [expr (annotation-expression full-annotated-expr)])
	 (with-assembler-source annotated-expr body ...))]
      [(_ annotated-expr body ...)
       (guard (con
	       [(assembler-source-condition? con) (raise con)]
	       [(source-condition? con) (raise
					 (condition
					  con
					  (make-assembler-source-condition annotated-expr)))]
	       [else (raise
		      (condition
		       con
		       (make-assembler-source-condition annotated-expr)
		       (make-source-condition (annotation-source annotated-expr))))])
	 body ...)]))
  )
