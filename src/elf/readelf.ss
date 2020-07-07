#!r6rs


(import (chezscheme)
        (elf)
        (elf-fmt))

(define args (command-line))
(cond [(or (null? args) (null? (cdr args)))
       (printf "No args, exiting....\n")
       (exit 0)]
      )
(define input-file-name (cadr args))
(define port (open-file-input-port input-file-name (file-options) 'block #f))
(define elf-file (read-elf-file port))
(define header (elf-get-header elf-file))
(define sections (elf-list-section-headers elf-file header))

(define (for-each-with-index procedure vector)
  (letrec* [(length (vector-length vector))
            (element-and-index-iterator
             (lambda (index)
               (if (< index length)
                   (let [(element (vector-ref vector index))]
                     (procedure element index)
                     (element-and-index-iterator (+ 1 index)))
                   )
               ))]
    (element-and-index-iterator 0)
    ))


;(dump-header header (current-output-port))

(for-each-with-index
 (lambda (section index)
   (display "section at index ")
   (display index)
   (display ": ")
   (dump-section-header elf-file header section)
   (newline)
   )
 sections
 )

(display (map utf8->string (read-string-table elf-file #xc4 #x2c)))
