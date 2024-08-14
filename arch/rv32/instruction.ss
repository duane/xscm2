(library (arch rv32 instruction)
  (export route-instruction)
  (import (chezscheme)
          (arch rv32 registers)
	  (asm parse)
          (asm errors))

(define-syntax get-ireg
  (lambda (x)
    (syntax-case x ()
      [(_ reg-sym a-reg)
       #'(let [(r (hashtable-ref
                   *integer-register-by-symbol*
                   'reg-sym
                   #f))]
           (if (not r) (set! r (hashtable-ref *integer-register-by-symbol* reg-sym #f)))
           (if r (register-numeric r) (raise-assembler-error "not a valid register" a-reg)))])))



(define-syntax define-u-instruction
  (lambda (x)
    (syntax-case x ()
      [(_ mnemonic opcode)
       #'(begin
           (define-syntax mnemonic
             (lambda (y)
               (with-syntax
                   [(generic-impl #`(fxior
                                     (fxsll imm 12)
                                     (fxsll rd 7)
                                     #,opcode))]
                    (syntax-case y ()
                      [(_ a-parent a-ops)
                       #'(begin
                           (if (null? a-ops)
                               (raise-assembler-error "not enough arguments" a-parent))
                           (let* [(a-rd (car a-ops))
                                  (op-rd (annotation-stripped a-rd))
                                  (next-lst (cdr a-ops))]
                             (if (null? next-lst)
                                 (raise-assembler-error "not enough arguments" a-parent))
                             (let* [(a-imm (car next-lst))
                                    (op-imm (annotation-stripped a-imm))]
                               (if (not (null? (cdr next-lst)))
                                        (raise-assembler-error "too many arguments" a-parent))
                               (let [(rd (get-ireg op-rd a-rd))
                                     (imm (parse-imm op-imm 20 a-imm))]
                                 generic-impl)
                               )
                             )
                           )]
                      )
                    )
               )
             )
           )
       ]
      )
    )
  )

(define-syntax define-instructions
  (lambda (x)
    (syntax-case x ()
      [(_
        router-procedure
        (u-type [mnemonic opcode] ...))
       #'(begin
           (define-u-instruction mnemonic opcode) ...
           )])))

(define (route-instruction a-mnemonic a-parent a-ops)
  (let [(mnemonic (annotation-stripped a-mnemonic))]
    (cond [(symbol=? mnemonic 'lui) (lui a-parent a-ops)]
          [(symbol=? mnemonic 'auipc) (auipc a-parent a-ops)]
          [else (raise-assembler-error "unknown instruction" a-mnemonic)])))


(define-u-instruction lui #b0110111)
                                        ;
(define-u-instruction auipc #b0010111)


)
