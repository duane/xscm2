					; simple - (x1)
					; offset - (x1 #12)
					; pre-indexed - (x1 #12)!
					; post-indexed - (x1), #12
					; scaled 12-bit unsigned
					; unscaled, 9-bit signed with pre- or post-
					; scaled or unscaled register index
(library (arch aarch64 addressing)
  (import (chezscheme)
	  (arch aarch64 registers)
	  (asm errors))


  (define-record-type simple-address (fields register))
  (define-record-type offset-address (fields register offset-immediate))
  (define-record-type pre-address (fields register offset-immediate))
  (define-record-type post-address (fields register offset-immediate))
  
  )
