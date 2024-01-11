(import (chezscheme)
        (asm errors)
        (arch aarch64 instruction))

(define args (command-line))

(cond [(or (null? args) (null? (cdr args)))
       (printf "No args, exiting....\n")
       (exit 0)]
      )

(define arg (cadr args))

(define (process-symbol symbol-datum annotated)
  (let* [(string (symbol->string symbol-datum))
         (len (string-length string))
         (label-len (- len 1))
         (last-char (string-ref string label-len))]
    (cond [(eq? last-char #\:)
           (let [(label-name (substring string 0 label-len))]
             (display "Encountered label: ")
             (display label-name)
             (newline))]
          [else (raise-assembler-error "not a label; labels must terminate with ':'" annotated)])
    )
  )
(define (process-list list-datum annotated)
  (if (null? list-datum) (raise-assembler-error "empty list" annotated))
  (let [(first-word (car list-datum))
        (a-expr (annotation-expression annotated))]
    (if (not (symbol? first-word))
        (raise-assembler-error "must be identifier" (car annotated))
        (route-instruction
         (car a-expr)
         annotated
         (cdr (a-expr))
         ))
    ;; look up instruction here
    (let [(operands (cdr list-datum))
          (operand-annotations (cdr a-expr))]
      #f
      )
    )
  )
(trace-define (process-other datum annotated) #f)

(define (make-aarch64-assembler)
  (let [(a 'a)]
    (lambda (value-or-annotation)
      (let [(annotation (if (annotation? value-or-annotation)
                            value-or-annotation
                            #f))
            (datum (if (annotation? value-or-annotation)
                       (annotation-stripped value-or-annotation)
                       value-or-annotation))]
        (apply (cond [(list? datum) process-list]
                       [(symbol? datum) process-symbol]
                       [else process-other])
               (list datum annotation))
        )
      )
    )
  )

(load arg (make-aarch64-assembler))
