(import (chezscheme))

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
          [else (raise "unknown use of symbol")])
    )
  )
(define (process-list list-datum annotated)
  (let [(first-word (car list-datum))]
    (if (null? first-word) (raise "found empty list at top-level"))
    (if (not (symbol? first-word)) (raise "the first datum in each expression should be a symbol"))
    (display "Encountered instruction or directive: ")
    (display first-word)
    (newline)
    )
  )
(trace-define (process-other datum annotated) #f)

(define (make-rv32-assembler)
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

(load arg (make-rv32-assembler))
