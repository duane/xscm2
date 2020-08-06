#!r6rs

(define (by-mnemonic-table) (make-hashtable string-hash string=?))

(define (add-instruction instruction)
  (let* [(mnemonic (instruction-mnemonic instruction))
         (instructions (hashtable-ref by-mnemonic-table mnemonic '()))]
    (hashtable-set! by-mnemonic-table mnemonic (cons instructions instruction)))
  )

(define (fetch-instructions mnemonic) (hashtable-ref by-mnemonic-table mnemonic '()))
