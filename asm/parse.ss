(library (asm parse)
  (export parse-imm bits-max)


  (define-syntax bits-max
    (lambda (bits-expr)
      (syntax-case bits-expr ()
	[(_ bits)
	 (let* [(bval (syntax->datum (syntax bits)))]
	   (and (number? bval) (> bval 0)))
	 #'(- (fxsll 1 bits) 1)])))

  (define-syntax parse-imm
    (lambda (imm-expr)
      (syntax-case imm-expr ()
	[(_ imm bits a-imm)
	 #'(begin
             (if (not (number? imm))
		 (raise-assembler-error "not a valid immediate literal" a-imm))
             (if (>= imm (bits-max bits))
		 (raise-assembler-error "immediate out of range" a-imm))
             imm
             )]
	)))
  
  )
