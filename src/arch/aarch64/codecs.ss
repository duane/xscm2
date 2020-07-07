#!r6rs

(load "arch/aarch64/structure.ss")
(import (util bit-range))

(define (wx-or-sp-codec suffix bits bit-range)
  (let [(document (lambda () (format "<~a~a|~a>"
                                     (cond [(= bits 32) #\W] [(= bits 64) #\X])
                                     suffix
                                     (cond [(= bits 32) "WSP"] [(= bits 64) "SP"]))))
        (render-as
         (lambda (u32)
           (let [(value (fetch-bit-range u32 bit-range))]
             (if (= value 31)
                 "sp"
                 (format "~a~d" (cond [(= bits 32) #\w] [(= bits 64) #\x]) value))
             )
           ))]
    (make-codec document render-as))
  )

(define (extend-codec-64 Rm-bit-range option-bit-range imm-bit-range)
  (let [(document (lambda () "<R><m>{, <extend> {#<amount>}}"))
        
        ]
    (make-codec document render-as)
    )
  )



;; tests
;; #b10001011001 00111 000 011 00101 00110
;; #b10001011001001110000110010100110
