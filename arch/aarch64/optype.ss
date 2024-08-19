(library (arch aarch64 optype)
  (export
   operand make-operand
   register-operand make-register-operand
   immediate-operand make-immediate-operand
   label-operand make-label-operand
   prex-operand make-prex-operand
   parse-annotated-reg
   parse-annotated-immn)
  (import (chezscheme)
	  (arch aarch64 registers)
	  (asm errors))

;;;; errors
  (define *expected-register* "expected register")
  (define *expected-imm* "expected immediate")
  (define *value-out-of-range* "value out of range for instruction")
  (define *value-unaligned* "value unaligned")

;;;; util
  (define (max-value-unsigned nbits) (- (fxsll 1 nbits) 1))
  (define (max-value-signed nbits) (max-value-unsigned (- nbits 1)))
  (define (min-value-signed nbits) (- -1 (max-value-signed nbits)))
  (define (max-value nbits signed) (if signed (max-value-signed nbits) (max-value-unsigned nbits)))
  (define (min-value nbits signed) (if signed (min-value-signed nbits) 0))

;;;; parsing

  (define (parse-annotated-reg annotated)
    (let ((expr (annotation-expression annotated)))
      (if (not (symbol? expr))
	  (raise-assembler-error *expected-register* annotated))
      (let ((reg (parse-register expr)))
	(if (not reg) (raise-assembler-error *expected-register* annotated))
	reg
	)
      )
    )

  (define (parse-annotated-immn annotated n-bits align signed)
    (let ((i (annotation-expression annotated)))
      (if (not (integer? i)) (raise-assembler-error *expected-imm* annotated))
      (let ((lower (* align (min-value n-bits signed)))
	    (upper (* align (max-value n-bits signed))))
	(if (or (> i upper) (< i lower)) (raise-assembler-error *value-out-of-range* annotated)))
      (let-values ([(q r) (fxdiv-and-mod i align)])
	(if (not (= 0 r)) (raise *value-unaligned*))
	q
	)
      )
    )
  
;;; types

  (define-record-type operand
    (fields parse))

  (define-record-type register-operand
    (parent operand)
    (fields size)
    (protocol
     (lambda (parent-new)
       (lambda (size)
	 (parent-new parse-annotated-reg size)
	 ))))
  
  (define-record-type immediate-operand
    (parent operand)
    (fields bits
	    align
	    signed?)
    (protocol
     (lambda (parent-new)
       (lambda (bits align signed?)
	 (let ((partial-parse (lambda (annotated) (parse-annotated-immn annotated bits align signed?))))
	   (parent-new partial-parse bits align signed?))
	 ))))

  (define-record-type label-operand
    (parent operand)
    (fields name))


  (define-record-type prex-operand
    (parent operand)
    (fields register
	    offset))



)
