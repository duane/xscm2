(library (arch aarch64 registers)
					; register-description
					; register-abi-names
  ; *float-register-by-symbol*
  (export *integer-register-by-symbol* register-name register-numeric parse-register! parse-register register-size)
  (import (chezscheme))

  (define-record-type register
    (fields name size numeric))

  (define (XnOrSpScope reg) (= 64 (register-size reg)))


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
    (hashtable-ref *integer-register-by-symbol* sym #f)
    )
	
  )
