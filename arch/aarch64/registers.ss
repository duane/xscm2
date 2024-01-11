(library (arch aarch64 registers)
					; register-description
					; register-abi-names
  ; *float-register-by-symbol*
  (export *integer-register-by-symbol* register-name register-numeric)
  (import (chezscheme))

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
             (define name (make-register 'name numeric))
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
    [%fp 64  29]
    [%lr 64  30]
    [%sp 64  31]
    )
  )
