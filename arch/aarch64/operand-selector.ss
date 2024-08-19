(library (arch aarch64 operand-selector)
  (export operand-selector
	  parse-imm-selector make-imm-selector make-imm-select make-imm-encode
	  select-operand
	  select-operands)
  (import (chezscheme)
	  (arch aarch64 registers)
	  (util string))


  (define-record-type operand-selector
    (fields render select encode))
  
  (define (select-operand v t)
    (let* ([select (operand-selector-select t)]
	   [result (select v)])
      result))

  ;; (define (read-operand-list))


  
  ;; returns either #t
  ;; or (values reason #f)
  (define (select-operands vl tl)
    (letrec*
	([select-operands*
	  (lambda (vl tl s)
	    (let* ([vhead (car vl)]
		   [thead (car tl)]
		   [vshort? (null? vhead)]
		   [tshort? (null? thead)]
		   )
	      (if (and vshort? tshort?) s
		  (if vshort? #f
		      (if tshort? #f
			  (let* ([selector (operand-selector-select thead)]
				 [result (selector vhead)])
			    (select-operands* (cdr vl) (cdr tl) (cons result s))
			    ))))
	      )
	    )]
	 )
      (select-operands* vl tl '())
      )
    )

  (include "arch/aarch64/operand-types/imm-operand.ss")
  (include "arch/aarch64/operand-types/reg-operand.ss")

  )
