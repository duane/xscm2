#!r6rs

(import (rnrs)
        (struct pack))

(define elf-header-aarch64 #vu8(127 69 76 70 2 1 1 0 0 0 0 0 0 0 0 0 1 0 183 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(define-record-type string-table
  (fields
   (mutable length)
   (mutable strings)))

(define (new-string-table) (make-string-table 1 '()))

(define (add-string-to-table table string)
  (let* [(len (string-table-length table))
         (strings (string-table-strings table))
         (utf8-string (string->utf8 string))
         (this-string-length (bytevector-length utf8-string))
         (updated-length (+ len this-string-length 1))]
    (string-table-length-set! table updated-length)
    (string-table-strings-set! table (cons (cons len utf8-string) strings))
    (display string)
    (display ": ")
    (display len)
    (display " - ")
    (display this-string-length)
    (newline)
    len ; starting offset of string
    )
  )

(define (string-table->bytevector table)
  (let* [(len (string-table-length table))
         (strings (string-table-strings table))
         (buffer (make-bytevector len))]
    (letrec [(write-strings
              (lambda (lst)
                (cond [(null? lst)
                       (bytevector-u8-set! buffer 0 0)]
                      [else (let* [(offset-and-string-pair (car lst))
                                   (offset (car offset-and-string-pair))
                                   (string (cdr offset-and-string-pair))
                                   (rest (cdr lst))
                                   (str-len (bytevector-length string))]
                              (bytevector-u8-set! buffer (+ offset str-len) 0)
                              (bytevector-copy! string 0 buffer offset str-len)
                              (write-strings rest))])
                ))]
      (write-strings strings))
    buffer))


(define (pack-elf-header-aarch64 binary-output-port)
  (put-pack binary-output-port
            "<L 12C 2S L 3Q L 6S"
            #x464C457F          ; magic byte: 4 bytes
            2                   ; ei_class: 1 byte
            1                   ; ei_data: 1 byte
            1                   ; ei_version: 1 byte
            0                   ; ei_osabi: 1 byte
            0                   ; ei_abiversion: 1 byte
            0 0 0 0 0 0 0       ; ei_pad: 7 bytes
            1                   ; ei_type: et_rel: 1 byte
            #xB7                ; ei_machine: aarch64: 1 byte
            1                   ; ei_version: 1 byte
            0                   ; e_entry: 8 bytes
            0                   ; e_phoff: 8 bytes
            0                   ; e_shoff: 8 bytes
            0                   ; e_flags: 4 bytes
            0                   ; e_ehsize: 2 bytes
            0                   ; e_phentsize: 2 bytes
            0                   ; e_phnum: 2 bytes
            64                  ; e_shentsize: 2 bytes
            0                   ; e_shnum: 2 bytes
            0                   ; e_shstrndx: 2 bytes
            )
  )

(define-record-type elf-section-writer
  (fields
   sh_name
   sh_type
   sh_flags
   sh_addr
   sh_offset
   sh_size
   sh_link
   sh_info
   sh_addralign
   sh_entsize
   )
  )

(define (patch-section-header-offsets-and-shstrtab-index port offset count shstrtab-index)
  (let [(e_shoff-patch (make-bytevector 8))
        (e_shnum-patch (make-bytevector 2))]
    (set-port-position! port #x28)
    (bytevector-u64-set! e_shoff-patch 0 offset (endianness little))
    (put-bytevector port e_shoff-patch)
    (set-port-position! port #x3C)
    (bytevector-u16-set! e_shnum-patch 0 count (endianness little))
    (put-bytevector port e_shnum-patch)
    (put-pack port "<S" shstrtab-index)
    )
  )

(define (write-section-header-64 port writer)
  (let [(section-header-buffer (make-bytevector #x40))]
    (bytevector-u32-set! section-header-buffer 0 (elf-section-writer-sh_name writer) (endianness little))
    (bytevector-u32-set! section-header-buffer #x4 (elf-section-writer-sh_type writer) (endianness little))
    (bytevector-u64-set! section-header-buffer #x8 (elf-section-writer-sh_flags writer) (endianness little))
    (bytevector-u64-set! section-header-buffer #x10 (elf-section-writer-sh_addr writer) (endianness little))
    (bytevector-u64-set! section-header-buffer #x18 (elf-section-writer-sh_offset writer) (endianness little))
    (bytevector-u64-set! section-header-buffer #x20 (elf-section-writer-sh_size writer) (endianness little))
    (bytevector-u32-set! section-header-buffer #x28 (elf-section-writer-sh_link writer) (endianness little))
    (bytevector-u32-set! section-header-buffer #x2c (elf-section-writer-sh_info writer) (endianness little))
    (bytevector-u64-set! section-header-buffer #x30 (elf-section-writer-sh_addralign writer) (endianness little))
    (bytevector-u64-set! section-header-buffer #x38 (elf-section-writer-sh_entsize writer) (endianness little))
    (put-bytevector port section-header-buffer)
    )
  )

(define (write-section
         port
         section
         sh_name
         sh_type
         sh_flags
         sh_addr
         sh_link
         sh_info
         sh_addralign
         sh_entsize
         )
  (let [(len (bytevector-length section))
        (offset (port-position port))]
    (put-bytevector port section)
    (make-elf-section-writer
     sh_name
     sh_type
     sh_flags
     sh_addr
     offset
     len
     sh_link
     sh_info
     sh_addralign
     sh_entsize
     )
    )
  )

(define (write-elf-file
         text-bv
         data-bv
         bss-bv
         port)
                                        ;  (put-bytevector port elf-header-aarch64)
  (pack-elf-header-aarch64 port)
  (let* [(sh-string-table (new-string-table))
         (headers
          (vector
           (write-section ; NULL section
            port
            #vu8()
            #x0
            #x0
            #x0
            #x0
            #x0
            #x0
            #x0
            #x0
            )
           (write-section
            port
            text-bv
            (add-string-to-table sh-string-table ".text")
            #x1                       ; progbits
            #x6                       ; execinstr alloc
            #x0
            #x0
            #x0
            #x0
            #x0
            )
           (write-section
            port
            data-bv
            (add-string-to-table sh-string-table ".data")
            #x1  ; progbits
            #b11 ; alloc write
            #x0
            #x0
            #x0
            #x0
            #x0
            )
           (write-section
            port
            bss-bv
            (add-string-to-table sh-string-table ".bss")
            #x8 ; nobits
            #x0
            #x0
            #x0
            #x0
            #x0
            #x0)
           (write-section
            port
            (string-table->bytevector sh-string-table)
            (add-string-to-table sh-string-table ".shstrtab")
            #x3
            #x2
            #x0
            #x0
            #x0
            #x0
            #x0 )))
         ]
    (let [(section-offset (port-position port))]
      (display (string-table->bytevector sh-string-table))
      (vector-for-each
       (lambda (section-header) (write-section-header-64 port section-header))
       headers)
      (patch-section-header-offsets-and-shstrtab-index
       port
       section-offset
       (vector-length headers)
       (- (vector-length headers) 1))
      )
    )
  )

(define (with-binary-output-file procedure path)
  (let [(port (open-file-output-port path (file-options no-fail) 'block #f))]
    (let [(result (procedure port))]
      (close-port port)
      result
      )
    )
  )
