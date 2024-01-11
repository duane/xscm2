(library (arch aarch64 instruction)
  (export route-instruction)
  (import (chezscheme)
          (arch aarch64 registers)
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

(define-syntax bits-max
  (lambda (x)
    (syntax-case x ()
      [(_ bits)
       (let* [(bval (syntax->datum (syntax bits)))] (and (number? bval) (> bval 0)))
       #'(- (fxsll 1 bits) 1)])))

(define (route-instruction a-mnemonic a-parent a-ops)
  (let [(mnemonic (annotation-stripped a-mnemonic))]
    (cond [else (raise-assembler-error "unknown instruction" a-mnemonic)])))
)
