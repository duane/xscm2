(library (arch aarch64 instruction)
  (export encode-adrp)
  (import (chezscheme)
          (arch aarch64 registers)
          (asm errors))


					; signatures-
					; 'r64
					; 'r32
					; 'ilit
					; 'addr/simple
					; 'addr/offset
					; 'addr/pre-indexed
					; 'addr-post-indexed
					; 'addr-scaled
					; 'addr-unscaled
					; 'addr/scaled-or-unscaled-register-idx


  (define (encode-adrp Rd-encoded imm20)
    (fxior #x90000000
	   (fxsll (fxand #b11 imm20) 29)
	   (fxsll (fxand #b11111111111111111 imm20) 3)
	   Rd-encoded
	   )
    ) 
)
