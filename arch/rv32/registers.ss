(library (arch rv32 registers)
  (export *float-register-by-symbol* *integer-register-by-symbol* register-name register-abi-names register-numeric register-description)
  (import (chezscheme))
  
  (define-record-type register
    (fields name abi-names numeric description))
  
  (define (initialize-register-table . registers)
    (let [(table (make-hashtable symbol-hash symbol=?))]
      (letrec [(iterate
                (lambda (rest)
                  (cond [(not (null? rest))
                         (let* [(reg (car rest))
                                (next-rest (cdr rest))
                                (name (register-name reg))
                                (abi-names (register-abi-names reg))]
                           (for-each
                            (lambda (key)
                              (hashtable-set! table key reg))
                            (cons name abi-names))
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
        [(_ name abi-name ... numeric description)
         #'(begin
             (define name (make-register 'name '(abi-name ...) numeric description))
             )
        ]
        )
      )
    )

  (define-syntax define-registers
    (lambda (x)
      (syntax-case x ()
        [(_ glob [name abi-name ... numeric description] ...)
         #'(begin
             (define-register name abi-name ... numeric description)
             ...
             (define glob (initialize-register-table name ...))
             )
         ])
      )
    )
  (define-registers
    *integer-register-by-symbol*
    [%x0   %zero   0  "Hard-wired zero"]
    [%x1   %ra     1  "Return address"]
    [%x2   %sp     2  "Stack pointer"]
    [%x3   %gp     3  "Global pointer"]
    [%x4   %tp     4  "Thread pointer"]
    [%x5   %t0     5  "Temporary register"]
    [%x6   %t1     6  "Temporary register"]
    [%x7   %t2     7  "Temporary register"]
    [%x8   %s0 %fp 8  "Saved register/frame pointer"]
    [%x9   %s1     9  "Saved register"]
    [%x10  %a0     10 "Function argument/return value"]
    [%x11  %a1     11 "Function argument/return value"]
    [%x12  %a2     12 "Function argument"]
    [%x13  %a3     13 "Function argument"]
    [%x14  %a4     14 "Function argument"]
    [%x15  %a5     15 "Function argument"]
    [%x16  %a6     16 "Function argument"]
    [%x17  %a7     17 "Function argument"]
    [%x18  %s2     18 "Saved register"]
    [%x19  %s3     19 "Saved register"]
    [%x20  %s4     20 "Saved register"]
    [%x21  %s5     21 "Saved register"]
    [%x22  %s6     22 "Saved register"]
    [%x23  %s7     23 "Saved register"]
    [%x24  %s8     24 "Saved register"]
    [%x25  %s9     25 "Saved register"]
    [%x26  %s10    26 "Saved register"]
    [%x27  %s11    27 "Saved register"]
    [%x28  %t3     28 "Temporary register"]
    [%x29  %t4     29 "Temporary register"]
    [%x30  %t5     30 "Temporary register"]
    [%x31  %t6     31 "Temporary register"]
    )

  (define-registers
    *float-register-by-symbol*
    [%f0 %ft0 0 "FP temporary register"]
    [%f1 %ft1 1 "FP temporary register"]
    [%f2 %ft2 2 "FP temporary register"]
    [%f3 %ft3 3 "FP temporary register"]
    [%f4 %ft4 4 "FP temporary register"]
    [%f5 %ft5 5 "FP temporary register"]
    [%f6 %ft6 6 "FP temporary register"]
    [%f7 %ft7 7 "FP temporary register"]
    [%f8 %fs0 8 "FP saved register"]
    [%f9 %fs1 9 "FP saved register"]
    [%f10 %fa0 10 "FP argument/return value"]
    [%f11 %fa1 11 "FP argument/return value"]
    [%f12 %fa2 12 "FP argument"]
    [%f13 %fa3 13 "FP argument"]
    [%f14 %fa4 14 "FP argument"]
    [%f15 %fa5 15 "FP argument"]
    [%f16 %fa6 16 "FP argument"]
    [%f17 %fa7 17 "FP argument"]
    [%f18 %fs2 18 "FP saved register"]
    [%f19 %fs3 19 "FP saved register"]
    [%f20 %fs4 20 "FP saved register"]
    [%f21 %fs5 21 "FP saved register"]
    [%f22 %fs6 22 "FP saved register"]
    [%f23 %fs7 23 "FP saved register"]
    [%f24 %fs8 24 "FP saved register"]
    [%f25 %fs9 25 "FP saved register"]
    [%f26 %fs10 26 "FP saved register"]
    [%f27 %fs11 27 "FP saved register"]
    [%f28 %ft8 28 "FP temporary register"]
    [%f29 %ft9 29 "FP temporary register"]
    [%f30 %ft10 30 "FP temporary register"]
    [%f31 %ft11 31 "FP temporary register"]
    )
  )
