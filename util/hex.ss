(library (util hex)
  (export hex->bytevector hex-dump bytevector->hex-string rand-hex-string u32->hex u32->le-hex)
  (import (chezscheme))

  (define (u32->hex u32)
    (string
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 28))))
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 24))))
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 20))))
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 16))))
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 12))))
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 8))))
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 4))))
     (integer->char (nibble->hex-char (fxand #xf u32)))
     )
    )

  (define (u32->le-hex u32)
    (string
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 4))))
     (integer->char (nibble->hex-char (fxand #xf u32)))
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 12))))
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 8))))
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 20))))
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 16))))
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 28))))
     (integer->char (nibble->hex-char (fxand #xf (fxsrl u32 24))))
     )
    )
  
  (define (bytevector-little-u32-iterate proc bytevector)
    (let [(max (div (bytevector-length bytevector) 4))]
      (letrec
          [(iterator
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

  (define (nibble->hex-char nibble)
    (if (< nibble #xA)
        (+ (char->integer #\0) nibble)
        (+ (char->integer #\A) (- nibble #xA)))
    )

  (define (bytevector->hex-string bytevector)
    (let [(hex-vector (make-bytevector (* 2 (bytevector-length bytevector))))]
      (letrec
          [(iterate
            (lambda (index)
              (cond [(< index (bytevector-length bytevector))
                     (let* [(byte (bytevector-u8-ref bytevector index))
                            (hi-nibble
                             (nibble->hex-char (fxbit-field byte 4 8)))
                            (lo-nibble
                            (nibble->hex-char (fxbit-field byte 0 4)))]
                       (bytevector-u8-set! hex-vector (* index 2) hi-nibble)
                       (bytevector-u8-set! hex-vector (+ 1 (* index 2)) lo-nibble)
                       )
                     (iterate (+ 1 index))
                     ])
              ))]
        (iterate 0)
        )
      (utf8->string hex-vector)
      )
    )

  (define (hex-dump bytevector)
    (bytevector-little-u32-iterate
     (lambda (offset u32)
       (printf "~8,'0X ~8,'0X\n" offset u32))
     bytevector)
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

  (define (rand-hex-string length)
    (let* [(random-port (open-file-input-port "/dev/random" (file-options) 'block #f))
           (random-string (get-bytevector-n random-port length))]
           (bytevector->hex-string random-string)
           )
      )

  (define (hex->bytevector s)
    (reverse-string-traverse 0 (string->list s)))
  )
