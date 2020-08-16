(library (asm errors)
  (export raise-assembler-error)
  (import (chezscheme))

  (define-condition-type &assembler-error &condition make-assembler-error assembler-error?
    (message assembler-error-message))

  ; single-point syntax error
  (define-condition-type
    &assembler-single-point-error
    &assembler-error
    make-assembler-single-point-error
    assembler-single-point-error?
    (where assembler-single-point-error-where))

  (define-syntax raise-assembler-error
    (lambda (x)
      (syntax-case x ()
                    [(_ msg)
                     #'(raise (make-assembler-error msg))]
                    [(_ msg #f)
                     #'(raise (make-assembler-error msg))]
                    [(_ msg where)
                     #'(raise (make-assembler-single-point-error msg where))])
      )
    )
  )
