(library (obj string-table)
  (export make-string-table string-table-add-string string-table-lookup-string)
  (import (chezscheme))

  (define-record-type string-table
    (fields
     (mutable offset-by-string)
     (mutable current-offset)
     (mutable strings)
     )
    (protocol (lambda (make) (lambda () (make (make-hashtable string-hash string=?) 1 '()))))
    )

  (define (string-table-add-string st string)
    (let [(ht (string-table-offset-by-string st))
          (current-offset (string-table-current-offset st))]
      (cond
       [(not (hashtable-contains? ht string))
        (hashtable-set! ht string current-offset)
        (string-table-current-offset-set! st (+ 1 (string-length string) current-offset))
        (string-table-strings-set! st (cons string (string-table-strings st)))])
       )
    )
  (define (string-table-lookup-string st string) (hashtable-ref (string-table-offset-by-string st) string #f))
)
