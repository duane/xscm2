#!r6rs

(import (util hex))

(define (format-insns2)
  (for-each (lambda (insn) (printf "~a\n" (format-insn2 insn))) insns2))

(define (format-register-type register-type)
  (format "~a" (register-type-name register-type)))


(define (render-register-type register-type u32)
  (let* [(bits (register-type-bits register-type))
         (prefix (cond [(= bits 32) #\w] [(= bits 64) #\x]))
         (value (fetch-bitfields u32 (register-type-bitfields register-type)))]
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
  (format "#~d" (fetch-bit-ranges u32 (immediate-type-bitfields immediate-type))))

(define (format-label-type label-type) (format "#<label~d>" (label-type-bits-size label-type)))

(define (render-label-type label-type u32)
  (format "#~d" (fetch-bit-ranges u32 (label-type-bitfields label-type))))

(define (format-pimm-type pimm-type)
  (format "[~a{,#<pimm~d>}]" (format-register-type (pimm-type-register-type pimm-type)) (pimm-type-bits-size pimm-type)))

(define (render-pimm-type pimm-type u32)
  (let* [(value (fetch-bit-ranges u32 (pimm-type-bitfields pimm-type)))
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

(define (format-shift-type shift-type)
  (format "{ LSL #<shift> }"))

(define (render-shift-type shift-type u32)
  (let [(bit-range (shift-type-bitfields shift-type))
        (value (fetch-bit-ranges u32 bit-range))]
    (cond [(= value 0) ""] [else (format "LSL #~d" value)]))
  )

(define (format-extend-type extend-type)
  (format "<extend>{, #<amount>}}"))

(define (render-extend-type extend-type u32)
        (let* [(option-bit-ranges (extend-type-option-bitfields extend-type))
               (option-value (fetch-bit-ranges u32 option-bit-ranges))
               (amount-bit-ranges (extend-type-amount-bitfields extend-type))
               (amount-value (fetch-bit-ranges u32 amount-bit-ranges))
               (extend-type-formatted (cond
                                       [(= option-value 0) "UXTB"]
                                       [(= option-value 1) "UXTH"]
                                       [(= option-value 2) "LSL"]
                                       [(= option-value 3) "UXTX"]
                                       [(= option-value 4) "SXTB"]
                                       [(= option-value 5) "SXTH"]
                                       [(= option-value 6) "SXTW"]
                                       [(= option-value 7) "SXTX"]))
               ]
          (if (= amount-value 0) extend-type-formatted (format "~a #~d" extend-type-formatted amount-value))
          )
        )


(define (format-operand-type operand-type)
  (cond [(register-type? operand-type) (format-register-type operand-type)]
        [(immediate-type? operand-type) (format-immediate-type operand-type)]
        [(label-type? operand-type) (format-label-type operand-type)]
        [(pimm-type? operand-type) (format-pimm-type operand-type)]
        [(shift-type? operand-type) (format-shift-type operand-type)]
        [(extend-type? operand-type) (format-extend-type operand-type)]))

(define (render-operand-type operand-type u32)
  (cond [(register-type? operand-type) (render-register-type operand-type u32)]
        [(immediate-type? operand-type) (render-immediate-type operand-type u32)]
        [(label-type? operand-type) (render-label-type operand-type u32)]
        [(pimm-type? operand-type) (render-pimm-type operand-type u32)]
        [(shift-type? operand-type) (render-shift-type operand-type u32)]
        [(extend-type? operand-type) (render-extend-type operand-type u32)])
  )

(define (format-insn2 insn2)
  (let [(first? #t)
        (buffer (format "~a" (insn2-mnemonic insn2)))]
    (for-each
     (lambda (formatted-op-type)
       (if first?
           (set! buffer (format "~a " buffer))
           (set! buffer (format "~a, " buffer)))
       (set! first? #f)
       (set! buffer (format "~a~a" buffer formatted-op-type))
       )
     (map format-operand-type (insn2-operands insn2))
     )
    buffer
    ))

(define (render-insn2 insn2 u32)
  (let [(first? #t)
        (buffer (format "~a" (insn2-mnemonic insn2)))]
    (for-each
     (lambda (formatted-op-type)
       (if first?
           (set! buffer (format "~a " buffer))
           (set! buffer (format "~a, " buffer)))
       (set! first? #f)
       (set! buffer (format "~a~a" buffer formatted-op-type))
       )
     (map (lambda (op) (render-operand-type op u32)) (insn2-operands insn2))
     )
    buffer
    ))

(module (print-decode)
        (define (print-decode bytevector)
          (bytevector-little-u32-iterate
           (lambda (offset u32)
             (let* [(m (find-insn-bitfield-match u32 insns))
                    (decoded (if (null? m)
                                 (format "~8,'0X" u32)
                                 ((insn-bitfield-format m) u32 m)))]
               (printf "~8,'0X ~a\n" offset decoded)
               )
             )
           bytevector)
          )

;;; insn querying

        (define (generate-mask-and-bits insns)
          (let [(bit 32)
                (mask 0)
                (bits 0)]
            (for-each
             (lambda (field)
               (let [(length (cadr field))]
                 (set! bit (- bit length))
                 (if (not (null? (cddr field)))
                     (let* [(these-bits (caddr field))
                            (this-mask (- (bitwise-arithmetic-shift-left 1 length) 1))
                            (shifted-bits (bitwise-arithmetic-shift-left these-bits bit))
                            (shifted-mask (bitwise-arithmetic-shift-left this-mask bit))]
                       (set! mask (bitwise-ior mask shifted-mask))
                       (set! bits (bitwise-ior bits shifted-bits)))
                     )
                 ))
             insns)
            (cons bits mask)
            )
          )

        (define (insn-bitfield-match? insn u32)
          (let* [(bits-and-mask (generate-mask-and-bits (insn-bitfield-fields insn)))
                 (bits (car bits-and-mask))
                 (mask (cdr bits-and-mask))
                 (masked (bitwise-and u32 mask))]
            (= bits masked)
            ))

        (define (find-insn-bitfield-match u32 insns)
          (fold-left (lambda (m insn)
                       (if (null? m)
                           (if (insn-bitfield-match? insn u32)
                               insn
                               '())
                           m
                           )
                       )
                     '()
                     insns))

;;; Formatting instructions with arguments.

         (define (insn-bitfield-fetch u32 insn name)
          (let [(fields (insn-bitfield-fields insn))
                (bit 32)]
            (fold-left (lambda (bits field)
                         (if (null? bits)
                             (let [(field-name (car field))
                                   (field-length (cadr field))]
                               (set! bit (- bit field-length))
                               (if (eq? name field-name)
                                   (let* [(mask (- (bitwise-arithmetic-shift-left 1 field-length) 1))
                                          (shifted-mask (bitwise-arithmetic-shift-left mask bit))
                                          (shifted-bits (bitwise-and u32 shifted-mask))
                                          (bits (bitwise-arithmetic-shift-right shifted-bits bit))]
                                     bits)
                                   '())
                               )
                             bits))
                       '()
                       fields))
          )

        (define (generic-format u32 insn) (format "~a" (insn-bitfield-name insn)))

        (define (format-adr u32 insn)
          (let* [(immlo (insn-bitfield-fetch u32 insn 'immlo))
                 (immhi (insn-bitfield-fetch u32 insn 'immhi))
                 (imm (bitwise-ior
                       (bitwise-arithmetic-shift-left immhi 2)
                       immlo))
                 (Rd (insn-bitfield-fetch u32 insn 'Rd))]
            (format "adr x~d, #~x" Rd imm)
            ))


;;; bulk disassembly

        (set! insns (list
                     (make-insn-bitfield 'movn
                                         '(
                                           (sf 1)
                                           (opc 2 #b00)
                                           (u0 6 #b100101)
                                           (hw 2)
                                           (imm16 16)
                                           (Rd 5)
                                           )
                                         generic-format
                                         )
                     (make-insn-bitfield 'add '(
                                       (sf 1)
                                       (op 1 0)
                                       (S 1 0)
                                       (u0 8 #b01011001)
                                       (Rm 5)
                                       (option 3)
                                       (imm3 3)
                                       (Rn 5)
                                       (Rd 5)
                                       )
                                generic-format)
                     (make-insn-bitfield 'add '(
                                       (sf 1)
                                       (op 1 0)
                                       (S 1 0)
                                       (u0 5 #b10001)
                                       (shift 2)
                                       (imm 12)
                                       (Rn 5)
                                       (Rd 5)
                                       )
                                generic-format
                                )
                                        ; add (shifted register)
                     (make-insn-bitfield 'add '(
                                                (sf 1)
                                                (op 1 0)
                                                (S 1 0)
                                                (u0 5 #b01010)
                                                (shift 2)
                                                (u1 1 0)
                                                (Rm 5)
                                                (imm6 6)
                                                (Rn 5)
                                                (Rd 5)
                                                )
                                         generic-format
                                         )
                                        ; adr
                     (make-insn-bitfield 'adr '(
                                       (sf 1 0)
                                       (immlo 2)
                                       (u0 5 #b10000)
                                       (immhi 19)
                                       (Rd 5)
                                       )
                                format-adr
                                )
                     (make-insn-bitfield 'adrp '(
                                        (sf 1 1)
                                        (immlo 2)
                                        (u0 5 #b10000)
                                        (immhi 19)
                                        (Rd 5)
                                        )
                                generic-format
                                )
                     (make-insn-bitfield 'movz '(
                                        (sf 1)
                                        (opc 2 #b10)
                                        (u0 6 #b100101)
                                        (hw 2)
                                        (imm16 16)
                                        (Rd 5) ) generic-format )
                     (make-insn-bitfield 'movi '(
                                        (u0 1 0)
                                        (Q 1)
                                        (op 1)
                                        (u1 10 #b0111100000)
                                        (a 1)
                                        (b 1)
                                        (c 1)
                                        (cmode 4)
                                        (u2 2 #b01)
                                        (d 1)
                                        (e 1)
                                        (f 1)
                                        (g 1)
                                        (h 1)
                                        (Rd 5)
                                        )
                                generic-format
                                )

                     (make-insn-bitfield 'br '(
                                      (u0 7 #b1101011)
                                      (Z 1 0)
                                      (u1 1 0)
                                      (op 2 #b00)
                                      (u2 9 #b111110000)
                                      (A 1 0)
                                      (M 1 0)
                                      (Rn 5)
                                      (Rm 5 #b00000)
                                      )
                                generic-format
                                )

                     (make-insn-bitfield
                      'ldr '((size 2)
                             (u0 6 #b111100)
                             (opc0 1)
                             (opc1 1 1)
                             (u1 1 0)
                             (imm9 9)
                             (u2 #b10)
                             (Rn 5)
                             (Rt 5))
                      generic-format
                      )
                     (make-insn-bitfield
                      'ldr '((size 2 #b11)
                             (u3 9 #b111000011)
                             (Rm 5)
                             (option 3)
                             (S 1)
                             (u4 2 #b10)
                             (Rn 5)
                             (Rt 5))
                      generic-format)

                     (make-insn-bitfield
                      'ldr ; ldr immediate post-index 64
                      '(
                        (opc 11 #b11111000010)
                        (imm9 9)
                        (opc1 2 #b01)
                        (Rn 5)
                        (Rt 5)
                        )
                      generic-format
                      )

                     (make-insn-bitfield
                      'ldr ; ldr immediate unsigned offset 64
                      '(
                        (opc 10 #b1111100101)
                        (imm12 12)
                        (Rn 5)
                        (Rt 5)
                        )
                      generic-format
                      )

                     (make-insn-bitfield
                      'orr '((sf 1)
                             (opc 2 #b01)
                             (u0 5 #b01010)
                             (shift 2)
                             (N 1 0)
                             (Rm 5)
                             (imm6 6)
                             (Rn 5)
                             (Rd 5)
                             )
                      generic-format)
                     )
              )
        )
