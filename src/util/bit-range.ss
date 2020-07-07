#!r6rs


(define (create-bit-range-bits bit-range bits) (bitwise-arithmetic-shift-left bits (car bit-range)))

(define (create-bit-range-mask bit-range)
  (let [(bit-range-size bit-range)
        (mask (- (bitwise-arithmetic-shift-left 1 (+ 1 bit-range-size)) 1))
        (shifted-mask (bitwise-arithmetic-shift-left mask (car bit-range)))]
    shifted-mask))

(define (fetch-bit-range u32 bit-range)
  (let* [(start (car bit-range))
        (end (cdr bit-range))
        (size (bit-range-size bit-range))
        (mask (- (bitwise-arithmetic-shift-left 1 size) 1))
        (shifted-mask (bitwise-arithmetic-shift-left mask start))
        (shifted-bits (bitwise-and shifted-mask u32))
        (bits (bitwise-arithmetic-shift-right shifted-bits start))]
    bits
    )
  )


; concat bit ranges
(define (fetch-and-concat-bit-ranges u32 bit-ranges)
  (fold-left (lambda (so-far bit-range)
               (let* [(fetched-segment (fetch-bit-range u32 bit-range))
                      (size (bit-range-size bit-range))
                      (shifted-so-far (bitwise-arithmetic-shift-left so-far size))
                      (combined (bitwise-ior shifted-so-far fetched-segment))]
                 combined)) 0 bit-ranges)
  )



(define (bit-range-size bit-range) (+ 1 (- (cdr bit-range) (car bit-range))))
