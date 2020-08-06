(import (chezscheme))

(define args (command-line))

(cond [(or (null? args) (null? (cdr args)))
       (printf "No args, exiting....\n")
       (exit 0)]
      )

(define arg (cadr args))

(define (set-label-global obj symbol)
  
  )

(define (make-rv32-assembler)
  (let [(a 'a)]
    (lambda (value-or-annotation)
      (let [(datum (if (annotation? value-or-annotation) (annotation-stripped value-or-annotation) value-or-annotation))]
        (display (cond [(list? datum) (dispatch-directive-or-mnemonic)]
                       [(symbol? datum) 'symbol]
                       [(number? datum) 'number]))))
    )
  )

(load arg (make-rv32-assembler))
