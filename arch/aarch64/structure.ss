#!r6rs

(import (util bit-range))

(define-record-type instruction-type (fields mnemonic name opcode mask operands))

(define-record-type register-type (fields bitfields name bits-wide))

(define-record-type immediate-type (fields bitfields name bits-wide signed?))
(define-record-type label-type (fields bitfields bits-size))
(define-record-type pimm-type (fields bitfields register-type bits bits-size))
(define-record-type shift-zero-or-twelve-type (fields bitfields))
(define-record-type shift-type (fields shift-bitfields))
(define-record-type shift2-type (fields shift-bitfields amount-bitfields))
(define-record-type extend-type (fields Rm-bit-range option-bit-range imm-bit-range))
(define-record-type shifted-register-type (fields shift-bit-range imm-bit-range))
(define-record-type lsl-byte-shift-type (fields hw-bit-range))

(define (format-lsl-byte-shift-type shift-type)
  "{, lsl #<shift>}")

(define (render-lsl-byte-shift-type shift-type u32)
  (let [(shift-value (fetch-bit-range u32 (lsl-byte-shift-type-hw-bit-range shift-type)))]
    (if (= shift-value 0) #f (format "lsl #~d" shift-value)))
  )

(define-record-type codec (fields document render-as))


(set! add-extended-register-64
      (make-instruction-type
       'add 'add-extended-register-64 #xb2000000 #x7FE00000
       (list
        (make-register-type '((5 . 9)) 'Xn 64)
        (make-register-type '((0 . 4)) 'Xd 64)
        (make-extend-type '(16 . 20) '(13 . 15) '(10 . 12))
        )
       )
      )

; #x91402041
(set! add-immediate-64
      (make-instruction-type
       'add 'add-immediate-64 #x11000000 #x7F100000
       (list
        (make-register-type '((0 . 4)) 'Xd 64)
        (make-register-type '((5 . 9)) 'Xn 64)
        (make-immediate-type '((10 . 21)) 'imm 12 #f)
        (make-shift-zero-or-twelve-type '((22 . 22)))
        )))

                                        ; #x8b03b441
(set! add-shifted-register-64
      (make-instruction-type
       'add 'add-shifted-register-64 #b10001011000000000000000000000000
       #b01111111001000000000000000000000
       (list
        (make-register-type '((0 . 4)) 'Xd 64)
        (make-register-type '((5 . 9)) 'Xn 64)
        (make-register-type '((16 . 20)) 'Xm 64)
        (make-shifted-register-type '(22 . 23) '(10 . 15))
        )))

; #x70ff7fe1
(set! adr-64
      (make-instruction-type
       'adr 'adr-64 #b00010000000000000000000000000000 #b10011111000000000000000000000000
       (list
        (make-register-type '((0 . 4)) 'Xd 64)
        (make-label-type '((5 . 23) (29 . 30)) 21)
        )))

                                        ; #xd2a3f441
; #xd283f441
(set! movz-64
      (make-instruction-type
       'movz 'movz-64
       #b01010010100000000000000000000000
       #b01111111100000000000000000000000
       (list
        (make-register-type '((0 . 4)) 'Xd 64)
        (make-immediate-type '((5 . 20)) 'imm 16 #f)
        (make-lsl-byte-shift-type '(21 . 22))
        )))

(set! ldr-register-64
      (make-instruction-type
       'ldr 'ldr-register-64
       #b10111000011000000000100000000000
       #b10111111111000000000110000000000
       (list
        (make-register-type '((0 . 4)) )
        )))

(define (format-instruction-types)
  (for-each (lambda (instruction-type) (printf "~a\n" (format-instruction-type instruction-type))) instruction-types))

(define (format-register-type register-type)
  (format "~a" (register-type-name register-type)))

(define (render-register-type register-type u32)
  (let* [(bits (register-type-bits-wide register-type))
         (prefix (cond [(= bits 32) #\w] [(= bits 64) #\x]))
         (value (fetch-and-concat-bit-ranges u32 (register-type-bitfields register-type)))]
    (format "~a~d" prefix value)
    )
  )

(define (format-immediate-type immediate-type)
  (let* [(base-label (if (immediate-type-signed? immediate-type)
                         (format "simm~d" (immediate-type-bits immediate-type))
                         (format "imm~d" (immediate-type-bits immediate-type))))]
    (format "#<~a>" base-label))
  )

(define (render-immediate-type immediate-type u32)
  (format "#~d" (fetch-and-concat-bit-ranges u32 (immediate-type-bitfields immediate-type))))

(define (format-label-type label-type) (format "#<label~d>" (label-type-bits-size label-type)))

(define (render-label-type label-type u32)
  (format "#~d" (fetch-and-concat-bit-ranges u32 (label-type-bitfields label-type))))

(define (format-pimm-type pimm-type)
  (format "[~a{,#<pimm~d>}]" (format-register-type (pimm-type-register-type pimm-type)) (pimm-type-bits-size pimm-type)))

(define (render-pimm-type pimm-type u32)
  (let* [(value (fetch-and-concat-bit-ranges u32 (pimm-type-bitfields pimm-type)))
        (bits (pimm-type-bits pimm-type))
        (scale-value (cond [(= bits 32) 4] [(= bits 64) 8]))
        (scaled-value (* value scale-value))
        (rendered-register (render-register-type (pimm-type-register-type pimm-type) u32))]
    (if (= value 0)
        (format "[~a]" rendered-register)
        (format "[~a, #~d]" rendered-register scaled-value)
        )
    )
  )

(define (format-shifted-register-type shift-type)
  (format "{, <shift> #amount}"))

(define (render-shifted-register-type shift-type u32)
  (let* [(shift-value (fetch-bit-range u32 (shifted-register-type-shift-bit-range shift-type)))
         (shift-label (cond [(= shift-value 0) "lsl"]
                            [(= shift-value 1) "lsr"]
                            [(= shift-value 2) "asr"]))
         (imm (fetch-bit-range u32 (shifted-register-type-imm-bit-range shift-type)))
         ]
    (if (and (= imm 0) (= shift-value 0)) #f (format "~a #~d" shift-label imm))
    )
  )

(define (format-shift-zero-or-twelve-type shift-type)
  (format "{ lsl <#0|#12> }"))

(define (render-shift-zero-or-twelve-type shift-type u32)
  (let* [(bit-range (shift-zero-or-twelve-type-bitfields shift-type))
         (value (fetch-and-concat-bit-ranges u32 bit-range))]
    (if (= 0 value) #f "lsl #12")
    )
   )


(define (format-shift-type shift-type)
  (format "{ LSL #<shift> }"))

(define (render-shift-type shift-type u32)
  (let [(bit-range (shift-type-bitfields shift-type))
        (value (fetch-and-concat-bit-ranges u32 bit-range))]
    (cond [(= value 0) ""] [else (format "LSL #~d" value)]))
  )

(define (format-extend-type extend-type)
  (format "<R><m>{, <extend> {#<amount>}}"))

(define (render-extend-type extend-type u32)
  (let* [(Rm-value (fetch-bit-range u32 (extend-type-Rm-bit-range extend-type)))
         (option-value (fetch-bit-range u32 (extend-type-option-bit-range extend-type)))
         (extend (cond [(= option-value 0) "UXTB"]
                       [(= option-value 1) "UXTH"]
                       [(= option-value 2) "UXTW"]
                       [(= option-value 3) "LSL"]
                       [(= option-value 4) "SXTB"]
                       [(= option-value 5) "SXTH"]
                       [(= option-value 6) "SXTW"]
                       [(= option-value 7) "SXTX"]))
         (shift-value (fetch-bit-range u32 (extend-type-imm-bit-range extend-type)))
         (R-bits (if (= #b11 (bitwise-and option-value #b11)) 64 32))
         (R-prefix (if (= R-bits 32) #\w #\x))
         (R (cond [(and (= R-bits 32) (= Rm-value 31)) "wsp"]
                  [(and (= R-bits 64) (= Rm-value 32)) "sp"]
                  [else (format "~a~d" R-prefix Rm-value)]))
         ]
    (cond [(> shift-value 0) (format "~a, ~a #~d" R extend shift-value)]
          [(= 3 option-value)  (format "~a" R)]
          [else (format "~a, ~a" R extend)])
    )
  )

(define (format-operand-type operand-type)
  (cond [(register-type? operand-type) (format-register-type operand-type)]
        [(immediate-type? operand-type) (format-immediate-type operand-type)]
        [(label-type? operand-type) (format-label-type operand-type)]
        [(pimm-type? operand-type) (format-pimm-type operand-type)]
        [(shift-type? operand-type) (format-shift-type operand-type)]
        [(extend-type? operand-type) (format-extend-type operand-type)]
        [(shift-zero-or-twelve-type? operand-type) (format-shift-zero-or-twelve-type operand-type)]
        [(shifted-register-type? operand-type) (format-shifted-register-type operand-type)]
        [(lsl-byte-shift-type? operand-type) (format-lsl-byte-shift-type operand-type)]))

(define (render-operand-type operand-type u32)
  (cond [(register-type? operand-type) (render-register-type operand-type u32)]
        [(immediate-type? operand-type) (render-immediate-type operand-type u32)]
        [(label-type? operand-type) (render-label-type operand-type u32)]
        [(pimm-type? operand-type) (render-pimm-type operand-type u32)]
        [(shift-type? operand-type) (render-shift-type operand-type u32)]
        [(extend-type? operand-type) (render-extend-type operand-type u32)]
        [(shift-zero-or-twelve-type? operand-type) (render-shift-zero-or-twelve-type operand-type u32)]
        [(shifted-register-type? operand-type) (render-shifted-register-type operand-type u32)]
        [(lsl-byte-shift-type? operand-type) (render-lsl-byte-shift-type operand-type u32)])
  )

(define (format-instruction-type instruction-type)
  (let [(first? #t)
        (buffer (format "~a" (instruction-type-mnemonic instruction-type)))]
    (for-each
     (lambda (formatted-op-type)
       (if first?
           (set! buffer (format "~a " buffer))
           (set! buffer (format "~a, " buffer)))
       (set! first? #f)
       (set! buffer (format "~a~a" buffer formatted-op-type))
       )
     (map format-operand-type (instruction-type-operands instruction-type))
     )
    buffer
    ))

(define (render-instruction-type instruction-type u32)
  (let [(first? #t)
        (buffer (format "~a" (instruction-type-mnemonic instruction-type)))]
    (for-each
     (lambda (formatted-op-type)
       (if first?
           (set! buffer (format "~a " buffer))
           (set! buffer (format "~a, " buffer)))
       (set! first? #f)
       (set! buffer (format "~a~a" buffer formatted-op-type))
       )
     (filter string?
             (map (lambda (op) (render-operand-type op u32))
                  (instruction-type-operands instruction-type))
             )
     )
    buffer
    ))
