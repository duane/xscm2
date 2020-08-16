(library (util in-memory-sfd)
  (export define-source-file)
  (import (chezscheme))

  (define-syntax define-source-file
    (lambda (x)
      (syntax-case x ()
        [(_ file string-or-expression)
         ])))
  )
