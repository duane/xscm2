(library (arch rv32 instruction-table)
  (export instructions)
  (import (chezscheme)
          (util hex))

  (define (lui rd imm)
    (fxior
     (fxsll imm 12)
     (fxsll rd 7)
     #b0110111)
    )

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

  (define instructions (make-hashtable symbol-hash symbol=?))

  (hashtable-set! instructions 'addi addi)
  (hashtable-set! instructions 'lui lui)
  (hashtable-set! instructions 'andi andi)
  )
