(import
 (chezscheme)
 (util hex)
 (arch aarch64)
 (arch aarch64 optype)
 (asm build))


;; (define *buf* (load-buffer! "test/aarch64/hello.sas"))
;; (load "test/aarch64/hello.sas" (asm-load! *aarch64-assembler*))
;; (let ((*cb* (aarch64-assembler-code-block *aarch64-assembler*)))
;;   (cb-hex! *cb*))


;; instruction
					; mnemonic - symbol
					; seq - list of named and constant operands

;; optype
					; integer literal
					; register
;; sized
					; label

;; implications: lookup instruction by mnemonic and parsed arg types
;; lookup-by-mnemonic
;; parse-op


;; for each mn:
;; lookup mn---if not exist, bail with bad mnemonic
;;; for each in:
;;; run parse_operand
;;; if not false:
;;; store for encoding.
;; search mn for instruction fitting operands--if no mn, bail with bad mnemonic.
;; apply operands to mn

;; operand types:
;; immNN
;; <Wd|sp>
;; <Xd|sp>
;; sf
;; op
;; S
;; sh

;; mvp:
;; mnemonics
;; multi-instruction mnemonics
;; forward global label
;; backward global label
;; forward temp label
;; backward temp label

;; disassembly
