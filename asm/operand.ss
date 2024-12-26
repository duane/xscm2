(library (asm build)
  (export read-operands)
  (import (chezscheme)
	  (asm errors)
	  (arch aarch64 registers))

  (define-record-type operand-type
    (fields
     check
     encode))

  ;; if the operands are legit, return a list of types and values
  ;; if the operands are not legit, return #f.

  ;; registers
  (define (read-r32 arg)
    (let ((register (parse-register arg)))
      (if (eq? 32 (register-size register)) register #f)
      )
    )

  (define (read-r64 arg)
    (let ((register (parse-register arg)))
      (if (eq? 64 (register-size register)) register #f)
      )
    )

  ;; binding
  (define (read-operands types args)
    (let* ((zipped (map list types args)))
      (fold-left
       (lambda (a e)
	 (if (eq? a #f) #f
	     (let ((read (caar zipped))
		   (val (cdar zipped)))
	       (let ((result (read val)))
		 (cond [(null? val) a]
		       [(not result) #f]
		       [else (cons result a)])
		 )
	       )
	     ))
       '()
       zipped)))
  )
