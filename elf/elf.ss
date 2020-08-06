#!r6rs

(library
    (elf)
  (export elf
          elf-file-header
          elf-section-header

          read-elf-file
          elf-list-section-headers
          elf-get-header
          elf-backing-buffer

          elf-file-header?
          make-elf-file-header
          elf-file-header-ei_class
          elf-file-header-ei_data
          elf-file-header-ei_version
          elf-file-header-ei_osabi
          elf-file-header-ei_abiversion
          elf-file-header-e_entry ; variable
          elf-file-header-e_phoff ; variable
          elf-file-header-e_shoff ; variable
          elf-file-header-e_flags
          elf-file-header-e_ehsize
          elf-file-header-e_phentsize
          elf-file-header-e_phnum
          elf-file-header-e_shentsize
          elf-file-header-e_shnum
          elf-file-header-e_shstrndx

          elf-section-header?
          make-elf-section-header
          elf-section-header-sh_name
          elf-section-header-sh_type
          elf-section-header-sh_flags
          elf-section-header-sh_addr
          elf-section-header-sh_offset
          elf-section-header-sh_size
          elf-section-header-sh_link
          elf-section-header-sh_info
          elf-section-header-sh_addralign
          elf-section-header-sh_entsize
          )
  (import (rnrs)
          (rnrs bytevectors)
          (rnrs records syntactic)
          (rnrs records inspection)
          (rnrs io ports)
          ) ; format

  (define-record-type elf (fields backing-buffer))
  (define-record-type elf-section-header
    (fields sh_name
            sh_type
            sh_flags
            sh_addr
            sh_offset
            sh_size
            sh_link
            sh_info
            sh_addralign
            sh_entsize))


  (define (elf-list-section-headers elf-file header)
    (letrec* [(initial-offset (elf-file-header-e_shoff header))
              (section-header-size (elf-file-header-e_shentsize header))
              (section-header-num (elf-file-header-e_shnum header))
              (out-vec (make-vector section-header-num))
              (iterator
               (lambda (index)
                 (if (< index (elf-file-header-e_shnum header))
                     (let* [(updated-index (+ 1 index))
                            (offset (+ initial-offset (* section-header-size index)))
                            (header (get-elf-section-header elf-file header offset))]
                       (vector-set! out-vec index header)
                       (iterator updated-index)
                       out-vec
                       )
                     )))]
      (iterator 0)
      ))

  (define (get-elf-section-header elf-file header offset)
    (let* [(index offset)
           (buffer (elf-backing-buffer elf-file))
           (sh_name (bytevector-u32-ref buffer (+ index SH_NAME-OFFSET) (endianness little)))
           (sh_type (bytevector-u32-ref buffer (+ index SH_TYPE-OFFSET) (endianness little)))
           ]
      (cond [(= 1 (elf-file-header-ei_class header))
             (let [(sh_flags (bytevector-u32-ref
                              buffer
                              (+ index SH_FLAGS-OFFSET)
                              (endianness little)))
                   (sh_addr (bytevector-u32-ref
                             buffer
                             (+ index SH_ADDR-OFFSET-32)
                             (endianness little)))
                   (sh_offset (bytevector-u32-ref
                               buffer
                               (+ index SH_OFFSET-OFFSET-32)
                               (endianness little)))
                   (sh_size (bytevector-u32-ref
                             buffer
                             (+ index SH_SIZE-OFFSET-32)
                             (endianness little)))
                   (sh_link (bytevector-u32-ref
                             buffer
                             (+ index SH_LINK-OFFSET-32)
                             (endianness little)))
                   (sh_info (bytevector-u32-ref
                             buffer
                             (+ index SH_INFO-OFFSET-32)
                             (endianness little)))
                   (sh_addralign (bytevector-u32-ref
                                  buffer
                                  (+ index SH_ADDRALIGN-OFFSET-32)
                                  (endianness little)))
                   (sh_entsize (bytevector-u32-ref
                                buffer
                                (+ index SH_ENTSIZE-OFFSET-32)
                                (endianness little)))]
               (make-elf-section-header sh_name
                                        sh_type
                                        sh_flags
                                        sh_addr
                                        sh_offset
                                        sh_size
                                        sh_link
                                        sh_info
                                        sh_addralign
                                        sh_entsize)
               )]
            [(= 2 (elf-file-header-ei_class header))
             (let [(sh_flags (bytevector-u64-ref
                              buffer
                              (+ index SH_FLAGS-OFFSET)
                              (endianness little)))
                   (sh_addr (bytevector-u64-ref
                             buffer
                             (+ index SH_ADDR-OFFSET-64)
                             (endianness little)))
                   (sh_offset (bytevector-u64-ref
                               buffer
                               (+ index SH_OFFSET-OFFSET-64)
                               (endianness little)))
                   (sh_size (bytevector-u64-ref
                             buffer
                             (+ index SH_SIZE-OFFSET-64)
                             (endianness little)))
                   (sh_link (bytevector-u32-ref
                             buffer
                             (+ index SH_LINK-OFFSET-64)
                             (endianness little)))
                   (sh_info (bytevector-u32-ref
                             buffer
                             (+ index SH_INFO-OFFSET-64)
                             (endianness little)))
                   (sh_addralign (bytevector-u64-ref
                                  buffer
                                  (+ index SH_ADDRALIGN-OFFSET-64)
                                  (endianness little)))
                   (sh_entsize (bytevector-u64-ref
                                buffer
                                (+ index SH_ENTSIZE-OFFSET-64)
                                (endianness little)))]
               (make-elf-section-header sh_name
                                        sh_type
                                        sh_flags
                                        sh_addr
                                        sh_offset
                                        sh_size
                                        sh_link
                                        sh_info
                                        sh_addralign
                                        sh_entsize)
               )])
      )
    )


  (define (read-elf-file port)
    (make-elf (get-bytevector-all port))
)

  (define-record-type elf-file-header
    (fields ei_class
            ei_data
            ei_version
            ei_osabi
            ei_abiversion
            e_entry ; variable
            e_phoff ; variable
            e_shoff ; variable
            e_flags
            e_ehsize
            e_phentsize
            e_phnum
            e_shentsize
            e_shnum
            e_shstrndx
            ))



  (define (elf-get-header elf-file)
    ;; read the first 16 bytes to confirm it's actually an elf header
    (let* [(buffer (elf-backing-buffer elf-file))
           (ei_class (bytevector-u8-ref buffer 4))
           (ei_data (bytevector-u8-ref buffer 5))
           (ei_version (bytevector-u8-ref buffer 6))
           (ei_osabi (bytevector-u8-ref buffer 7))
           (ei_abiversion (bytevector-u8-ref buffer 8))
           ]
      (cond
       [(= ei_class 2)
        (let [(e_entry (bytevector-u64-ref buffer E_ENTRY_OFFSET (endianness little)))
              (e_phoff (bytevector-u64-ref buffer E_PHOFF-OFFSET-64 (endianness little)))
              (e_shoff (bytevector-u64-ref buffer E_SHOFF-OFFSET-64 (endianness little)))
              (e_flags (bytevector-u32-ref buffer E_FLAGS-OFFSET-64 (endianness little)))
              (e_ehsize (bytevector-u16-ref buffer E_EHSIZE-OFFSET-64 (endianness little)))
              (e_phentsize (bytevector-u16-ref buffer E_PHENTSIZE-OFFSET-64 (endianness little)))
              (e_phnum (bytevector-u16-ref buffer E_PHNUM-OFFSET-64 (endianness little)))
              (e_shentsize (bytevector-u16-ref buffer E_SHENTSIZE-OFFSET-64 (endianness little)))
              (e_shnum (bytevector-u16-ref buffer E_SHNUM-OFFSET-64 (endianness little)))
              (e_shstrndx (bytevector-u16-ref buffer E_SHSTRNDX-OFFSET-64 (endianness little)))

              ]
          (make-elf-file-header ei_class
                                ei_data
                                ei_version
                                ei_osabi
                                ei_abiversion
                                e_entry
                                e_phoff
                                e_shoff
                                e_flags
                                e_ehsize
                                e_phentsize
                                e_phnum
                                e_shentsize
                                e_shnum
                                e_shstrndx
                                ))]
       [(= ei_class 1)
        (let [(e_entry (bytevector-u32-ref buffer E_ENTRY_OFFSET (endianness little)))
              (e_phoff (bytevector-u32-ref buffer E_PHOFF-OFFSET-32 (endianness little)))
              (e_shoff (bytevector-u32-ref buffer E_SHOFF-OFFSET-32 (endianness little)))
              (e_flags (bytevector-u32-ref buffer E_FLAGS-OFFSET-32 (endianness little)))
              (e_ehsize (bytevector-u16-ref buffer E_EHSIZE-OFFSET-32 (endianness little)))
              (e_phentsize (bytevector-u16-ref buffer E_PHENTSIZE-OFFSET-32 (endianness little)))
              (e_phnum (bytevector-u16-ref buffer E_PHNUM-OFFSET-32 (endianness little)))
              (e_shentsize (bytevector-u16-ref buffer E_SHENTSIZE-OFFSET-32 (endianness little)))
              (e_shnum (bytevector-u16-ref buffer E_SHNUM-OFFSET-32 (endianness little)))
              (e_shstrndx (bytevector-u16-ref buffer E_SHSTRNDX-OFFSET-32 (endianness little)))

              ]
          (make-elf-file-header ei_class
                                ei_data
                                ei_version
                                ei_osabi
                                ei_abiversion
                                e_entry
                                e_phoff
                                e_shoff
                                e_flags
                                e_ehsize
                                e_phentsize
                                e_phnum
                                e_shentsize
                                e_shnum
                                e_shstrndx
                                ))]
;       [else (errorf 'elf-header "foo")]
       )
      )
    )

  (define EI_MAG0 0)
  (define EI_MAG1 1)
  (define EI_MAG2 2)
  (define EI_MAG3 3)
  (define EI_CLASS 4)
  (define EI_DATA 5)
  (define EI_VERSION 6)
  (define EI_OSABI 7)
  (define EI_ABIVERSION 8)

  (define E_ENTRY_OFFSET #x18)

  (define E_PHOFF-OFFSET-32 #x1C)
  (define E_PHOFF-OFFSET-64 #x20)

  (define E_SHOFF-OFFSET-32 #x20)
  (define E_SHOFF-OFFSET-64 #x28)

  (define E_FLAGS-OFFSET-32 #x24)
  (define E_FLAGS-OFFSET-64 #x30)

  (define E_EHSIZE-OFFSET-32 #x28)
  (define E_EHSIZE-OFFSET-64 #x34)

  (define E_PHENTSIZE-OFFSET-32 #x2A)
  (define E_PHENTSIZE-OFFSET-64 #x36)

  (define E_PHNUM-OFFSET-32 #x2C)
  (define E_PHNUM-OFFSET-64 #x38)

  (define E_SHENTSIZE-OFFSET-32 #x2E)
  (define E_SHENTSIZE-OFFSET-64 #x3A)

  (define E_SHNUM-OFFSET-32 #x30)
  (define E_SHNUM-OFFSET-64 #x3C)

  (define E_SHSTRNDX-OFFSET-32 #x32)
  (define E_SHSTRNDX-OFFSET-64 #x3E)


  (define SH_NAME-OFFSET #x4)
  (define SH_TYPE-OFFSET #x4)
  (define SH_FLAGS-OFFSET #x8)

  (define SH_ADDR-OFFSET-32 #xc)
  (define SH_ADDR-OFFSET-64 #x10)

  (define SH_OFFSET-OFFSET-32 #x10)
  (define SH_OFFSET-OFFSET-64 #x18)

  (define SH_SIZE-OFFSET-32 #x14)
  (define SH_SIZE-OFFSET-64 #x20)

  (define SH_LINK-OFFSET-32 #x18)
  (define SH_LINK-OFFSET-64 #x28)

  (define SH_INFO-OFFSET-32 #x1C)
  (define SH_INFO-OFFSET-64 #x2C)

  (define SH_ADDRALIGN-OFFSET-32 #x20)
  (define SH_ADDRALIGN-OFFSET-64 #x30)

  (define SH_ENTSIZE-OFFSET-32 #x24)
  (define SH_ENTSIZE-OFFSET-64 #x38)
  
  )
