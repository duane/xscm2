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

  (define (is-simple-address? expr))
  (define (is-offset-address? expr))
  (define (is-pre-address? expr))
  (define (is-post-address? expr))
  (define (encode-simple-address fields register))
  (define (encode-offset-address fields register offset-immediate))
  (define (encode-pre-address fields register offset-immediate))
  (define (encode-post-address fields register offset-immediate))
 
  )
