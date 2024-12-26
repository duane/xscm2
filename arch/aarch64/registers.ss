(library (arch aarch64 registers)
  (export *integer-register-by-symbol* register-name register-numeric parse-register! parse-register register-size *w-or-sp* set-contains-symbol?)
  (import (chezscheme)
	  (util string))

  (define-record-type register
    (fields name size numeric))

  (define (initialize-register-table . registers)
    (let [(table (make-hashtable symbol-hash symbol=?))]
      (letrec [(iterate
                (lambda (rest)
                  (cond [(not (null? rest))
                         (let* [(reg (car rest))
                                (next-rest (cdr rest))
                                (name (register-name reg))]
                           (hashtable-set! table name reg)
                           (iterate next-rest)
                           )]
                        )))]
        (iterate registers)
        )
      table
      )
    )

  (define-syntax define-register
    (lambda (x)
      (syntax-case x ()
        [(_ name size numeric)
         #'(begin
             (define name (make-register 'name size numeric))
             )
         ]
        )
      )
    )

  (define-syntax define-registers
    (lambda (x)
      (syntax-case x ()
        [(_ glob [name size numeric] ...)
         #'(begin
             (define-register name size numeric)
             ...
             (define glob (initialize-register-table name ...))
             )
         ])
      )
    )

  (define (set-contains-symbol? class sym)
    (let ([result (hashtable-ref class sym #f)])
      (if result #t #f)))

  (define-syntax hashtable-union
    (syntax-rules ()
      [(_ ht ...)
       (let ([table (make-hashtable symbol-hash symbol=?)])
	 (vector-for-each
	  (lambda (reg)
	    (hashtable-set! table (register-name reg) reg))
	  (hashtable-values ht))
	 ...
	 table)]))

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

  (define
    (lookup-register name size)
    (let ((reg (hashtable-ref *integer-register-by-symbol* name #f)))
      (if (and reg (= (register-size reg) size)) reg #f)
      )
    )

  (define-registers
    *integer-register-by-symbol*
    [%x0 64  0]
    [%x1 64  1]
    [%x2 64  2]
    [%x3 64  3]
    [%x4 64  4]
    [%x5 64  5]
    [%x6 64  6]
    [%x7 64  7]
    [%x8 64  8]
    [%x9 64  9]
    [%x10 64  10]
    [%x11 64  11]
    [%x12 64  12]
    [%x13 64  13]
    [%x14 64  14]
    [%x15 64  15]
    [%x16 64  16]
    [%x17 64  17]
    [%x18 64  18]
    [%x19 64  19]
    [%x20 64  20]
    [%x21 64  21]
    [%x22 64  22]
    [%x23 64  23]
    [%x24 64  24]
    [%x25 64  25]
    [%x26 64  26]
    [%x27 64  27]
    [%x28 64  28]
    [%x29 64  29]
    [%x30 64  30]

    [%w0 32 0]
    [%w1 32 1]
    [%w2 32 2]
    [%w3 32 3]
    [%w4 32 4]
    [%w5 32 5]
    [%w6 32 6]
    [%w7 32 7]
    [%w8 32 8]
    [%w9 32 9]
    [%w10 32 10]
    [%w11 32 11]
    [%w12 32 12]
    [%w13 32 13]
    [%w14 32 14]
    [%w15 32 15]
    [%w16 32 16]
    [%w17 32 17]
    [%w18 32 18]
    [%w19 32 19]
    [%w20 32 20]
    [%w21 32 21]
    [%w22 32 22]
    [%w23 32 23]
    [%w24 32 24]
    [%w25 32 25]
    [%w26 32 26]
    [%w27 32 27]
    [%w28 32 28]
    [%w29 32 29]
    [%w30 32 30]
    
    [%fp 64  29]
    [%lr 64  30]
    [%sp 64  31]    
    )

  (define (parse-register! sym)
    (let ([result (hashtable-ref *integer-register-by-symbol* sym #f)])
      (assert result)
      result))

  (define (parse-register sym)
    (let ([result (hashtable-ref *integer-register-by-symbol* sym #f)])
      (display result)      
      result))


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
