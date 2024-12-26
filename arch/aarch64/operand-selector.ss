(library (arch aarch64 operand-selector)
  (export operand-selector
	  parse-imm-selector make-imm-selector make-imm-select make-imm-encode
	  select-operand)
  (import (chezscheme)
	  (arch aarch64 registers)
	  (util string))


  (define-record-type operand-selector (fields render select encode))
  
  (define (select-operand v t)
    (let* ([select (operand-selector-select t)]
	   [result (select v)])
      result))


  ;; value-list is the domain value to be evaluated
  ;; type-list is the necessary domain-type for a given operand to be selected.
  ;; If the list of values conforms to the list of types, this function returns #t
  ;; If the list of values does not conform to the list of types, this function returns the reason why
  ;; returns either #t or a reason string.
  (define (select-operands value-list type-list)
    (letrec* ([*select-operands*
	       (lambda (value-list type-list s)
		 (let* ([value-head (car value-list)]
			[type-head (car type-list)]
			[value-short? (null? value-head)]
			[type-short? (null? type-head)]
			)
		   (if (and value-short? type-short?) s
		       (if value-short? "not enough values to satisfy type list"
			   (if type-short? "not enough types to satisfy value list"
			       ;; if the value and type lists are the same length,
			       ;; return the reason why this value does not match the type.
			       (let* ([selector (operand-selector-select type-head)]
				      [result (selector value-head)])
				 (*select-operands* (cdr value-list) (cdr type-list) (cons result s))
				 )
			       )
			   )
		       )
		   )
		 )
	       ]
	      )
      (*select-operands* value-list type-list '())
      )
    )

  (include "operand-types/imm-operand.ss")
  ;; (include "arch/aarch64/operand-types/reg-operand.ss")

  )
