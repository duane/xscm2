(library (obj code-object)
  (export make-code-object)
  (import (chezscheme))
  (define-record-type code-object
    (fields
     (mutable sections))
    (protocol
     (lambda (make)
       (lambda ()
         (make '())
         )
       )
     )
    )
  )
