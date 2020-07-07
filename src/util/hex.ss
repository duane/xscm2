
(module
 (hex->bv bytevector-little-u32-iterate hex-dump)

 (define (bytevector-little-u32-iterate proc bytevector)
   (let [(max (div (bytevector-length bytevector) 4))]
     (letrec [(iterator
               (lambda (index)
                 (if (< index max)
                     (let* [(updated-index (+ 1 index))
                            (byte-offset (* index 4))
                            (u32 (bytevector-u32-ref bytevector byte-offset (endianness little)))]
                       (proc byte-offset u32)
                       (iterator (+ index 1))
                       ))))]
       (iterator 0)
       )
     )
   )

 (define (hex-dump bytevector)
   (bytevector-little-u32-iterate
    (lambda (offset u32)
      (printf "~8,'0X ~8,'0X\n" offset u32))
    machine-code-bytevector)
   )

 (define (parse-hex c)
   (let [(c (char-downcase c))]
     (cond
      [(and (char-ci>=? c #\a) (char-ci<=? c #\f))
       (+ #xa (- (char->integer c) (char->integer #\a)))]
      [(and (char-ci>=? c #\0) (char-ci<=? c #\9))
       (- (char->integer c) (char->integer #\0))]
      [else #f])
     )
   )

 (define (poke-bytevector-half-byte bv byte-offset hi-byte? half-byte)
   (let* [(byte (bytevector-u8-ref bv byte-offset))
          (masked-value (bitwise-and half-byte #xf))
          (shifted-value (if hi-byte? (bitwise-arithmetic-shift-left masked-value 4) masked-value))
          (poked-byte (bitwise-ior byte shifted-value))]
     (bytevector-u8-set! bv byte-offset poked-byte)
     )
   )

 (define (reverse-string-traverse index list)
   (if (null? list)
       (make-bytevector (ceiling (/ index 2)) 0)
       (let* [(c (car list))
              (parsed-result (parse-hex c))
              (updated-index (if (number? parsed-result) (+ 1 index) index))
              (buffer (reverse-string-traverse updated-index (cdr list)))]
         (if (number? parsed-result)
             (let [(char-offset (div index 2))
                   (hi-or-lo (mod index 2))]
               (poke-bytevector-half-byte buffer char-offset (= 0 hi-or-lo) parsed-result)
               buffer
               )
             buffer)
         )
       )
   )

 (define (hex->bv s)
   (reverse-string-traverse 0 (string->list s)))
 )
