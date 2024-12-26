(library (arch aarch64 register-set)
  (export register-set-lookup register-set-list register-class-contains?)
  (import (chezscheme)
	  (arch aarch64 registers))

  (define-record-type register-set (fields name registers))
  
  (define (set-contains-symbol? class sym)
    (let ([result (hashtable-ref class sym #f)])
      (if result #t #f)))

  (define-syntax hashtable-union
    (syntax-rules ()
      [(_ name ht ...)
       (let ([table (make-hashtable symbol-hash symbol=?)])
	 (vector-for-each
	  (lambda (reg)
	    (hashtable-set! table (register-name reg) reg))
	  (hashtable-values ht))
	 ...
	 (register-set 'name table))]))

  (define-syntax enumerated-register-set
    (syntax-rules ()
      [(_ reg-table body ...)
       (let ([table (make-hashtable symbol-hash symbol=?)])
	 (for-each
	  (lambda (reg)
	    (let ([reg-lookup-result (hashtable-ref reg-table reg #f)])
	      (assert reg-lookup-result)
	      (hashtable-set! table (register-name reg-lookup-result) reg-lookup-result)
	      )
	    )
	  (list 'body ...))
	 table)]))

  (define *sp* (enumerated-register-set
		*integer-register-by-symbol*
		%sp))
  (define *w*
    (enumerated-register-set
     *integer-register-by-symbol*
     %w0 %w1 %w2 %w3 %w4 %w5 %w6 %w7 %w8 %w9 %w10 %w11 %w12 %w13 %w14 %w15 %w16 %w17 %w18 %w19 %w20 %w21 %w22 %w23 %w24 %w25 %w26 %w27 %w28 %w29 %w30))
  (define *x*
    (enumerated-register-set
     *integer-register-by-symbol*
     %x0  %x1  %x2  %x3  %x4  %x5  %x6  %x7  %x8  %x9  %x10  %x11  %x12  %x13  %x14  %x15  %x16  %x17  %x18  %x19  %x20  %x21  %x22  %x23  %x24  %x25  %x26  %x27  %x28  %x29  %x30))
  (define *w-or-sp* (hashtable-union *sp* *w*))
  (define *x-or-sp* (hashtable-union *sp* *w*))
  )
