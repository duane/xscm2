#!/usr/bin/env scheme-script --program

(import (rnrs))

(load "aarch64.ss")
(load "util.ss")

(define (bv->oplist bv)
  (letrec* [(op-length (div (bytevector-length bv) 4))
         (traverse-bv (lambda (op-index)
                        (if (>= op-index op-length) '()
                            (let [(op (bytevector-u32-ref bv (* 4 op-index) 'little))]
                              (cons op (traverse-bv (+ 1 op-index)))
                              )
                         )
                        )
                      )
         (result (traverse-bv 0))]
    result
    )
  )

(set! hex-string-val "F3 03 00 91 B4 35 29 10 D5 B4 08 10 17 00 80 D2 58 35 09 10 B9 B3 08 10 8F B4 08 10 F0 03 0F AA F1 03 0F AA 2B 34 09 10 FA 03 13 AA FB 03 14 AA 09 00 80 D2 7D 5E 08 10 1C A1 FF 10 48 E5 00 4F E9 E5 00 4F 0A E4 01 4F 0B E6 01 4F EC E6 01 4F 2D E4 02 4F B6 86 40 F8 C0 02 40 F9 00 00 1F D6")

(set! machine-code-bytevector (hex->bv hex-string-val))


(define (sum-instruction-bits insns)
  (fold-left (lambda (count x) (+ count (cadr x))) 0 insns)
  )

(set! op-list (bv->oplist machine-code-bytevector))
(set! u32 (cadr op-list))
;(set! m (find-insn-bitfield-match u32 insns))
(print-decode machine-code-bytevector)

