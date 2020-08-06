;;; LABEL:
;;; SET - mark where the label offset/token is
;;; WRITE - write the calculated offset to the instruction
;;; TYPE - set the symbol as global or local

;;; VARIABLE:

;;; PATCH:
;;; bitfield
;;; token
;;; proc to evaluate offset literal

;;; STATE:
;;; *labels* - label by symbol
;;; *patches* - patch by symbol
;;; *variables* - TODO
;;; *here* - unsigned integer
;;; *output-port*
;;; *current-section*
;;; *symtab* - deduping string table
;;; *dynsym* - TODO

(import (chezscheme)
        (util hex))

(define-structure
  (label symbol offset type token))

(define-structure
  (patch start end token))

(define (lui rd imm)
  (bitwise-copy-bit-field
   (bitwise-copy-bit-field #b0110111 12 32 imm)
   7 12 rd))

(define (andi rd rs1 imm)
  (fxior
   (fxsll imm 20)
   (fxsll rs1 15)
   (fxsll #b111 12)
   (fxsll rd 7)
   #b010011)
  )

(define (addi rd rs1 imm)
  (fxior
   (fxsll imm 20)
   (fxsll rs1 15)
   (fxsll #b000 12)
   (fxsll rd 7)
   #b010011)
  )

;; (define (apply-directive args))
;; (define (apply-instruction args))
;; (define (define-label label))

;; (define (walk-objects))
;; (define (process-top-level-token))
;; (define (process-list))
;; (define (process-mnemonic))
;; (define (process-directive))

;; (define (assemble input-port output-port)
;;   (let [(obj-walk (lambda (atom)))
;;         (ident-lookup (lambda (token)))
;;         (directive-walk (lambda (token)))
;;         (label-define (lambda (token)))
;;         (label-lookup (lambda (token)))
;;         (label-stub (lambda (token)))
;;         (label-patch (lambda (toutken)))])
;;   )


(define-structure (section name type code))
(define-structure (string-table) (offset-by-string (make-hashtable string-hash string=?)))
(define-structure (compilation-target sections))

