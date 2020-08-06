(library (elf-fmt)
  (export dump-header dump-section-header read-string-table)
  (import (chezscheme)
          (elf))

  (define (dump-header header port)
    (let* [(ei_class (elf-file-header-ei_class header))
           (arch-variable-formatter-hex (cond [(= ei_class 1) "~8,'0X"] [(= ei_class 2) "~16,'0X"]))]
      (display (format "ei_class: ~2,'0X" (elf-file-header-ei_class header)) port)
      (newline)
      (display (format "ei_data: ~2,'0X" (elf-file-header-ei_data header)) port)
      (newline)
      (display (format "ei_version: ~2,'0X" (elf-file-header-ei_version header)) port)
      (newline)
      (display (format "ei_osabi: ~2,'0X" (elf-file-header-ei_osabi header)) port)
      (newline)
      (display (format "ei_abiversion: ~2,'0X" (elf-file-header-ei_abiversion header)) port)
      (newline)
      (display "e_entry: ")
      (display (format arch-variable-formatter-hex (elf-file-header-e_entry header)))
      (newline)
      (display "e_phoff: ")
      (display (format arch-variable-formatter-hex (elf-file-header-e_phoff header)))
      (newline)
      (display "e_shoff: ")
      (display (format arch-variable-formatter-hex (elf-file-header-e_shoff header)))
      (newline)
      (display (format "e_flags: ~8,'0X\n" (elf-file-header-e_flags header)))
      (display (format "e_ehsize: ~4,'0X\n" (elf-file-header-e_ehsize header)))
      (display (format "e_phentsize: ~4,'0X\n" (elf-file-header-e_phentsize header)))
      (display (format "e_phnum: ~4,'0X\n" (elf-file-header-e_phnum header)))
      (display (format "e_shentsize: ~4,'0X\n" (elf-file-header-e_shentsize header)))
      (display (format "e_shnum: ~4,'0X\n" (elf-file-header-e_shnum header)))
      (display (format "e_shstrndx: ~4,'0X\n" (elf-file-header-e_shstrndx header)))
      )
    )

  (define (sh_type->string type)
    (cond [(= type 0) "NULL"]
          [(= type #x1) "PROGBITS"]
          [(= type #x2) "SYMTAB"]
          [(= type #x3) "STRTAB"]
          [(= type #x4) "RELA"]
          [(= type #x5) "HASH"]
          [(= type #x6) "DYNAMIC"]
          [(= type #x7) "NOTE"]
          [(= type #x8) "NOBITS"]
          [(= type #x9) "REL"]
          [(= type #xa) "SHLIB"]
          [(= type #xb) "DYNSYM"]
          [(= type #xe) "INIT_ARRAY"]
          [(= type #xf) "FINI_ARRAY"]
          [(= type #x10) "PREINIT_ARRAY"]
          [(= type #x11) "GROUP"]
          [(= type #x12) "SYMTAB_SHNDX"]
          [(= type #x13) "NUM"]
          [else "OTHER - UNKNOWN"])
    )

  (define (read-string-table elf offset size)
    (let [(backing-buffer (elf-backing-buffer elf))
          (str-list '())
          (string-start #f)]
      (letrec* [(iterator
                 (lambda (index)
                   (if (< index size)
                       (let* [(updated-index (+ 1 index))
                              (current-offset (+ index offset))
                              (u8 (bytevector-u8-ref backing-buffer current-offset))]
                         (cond [(and string-start (= u8 0))
                                (let* [(str-length (- index string-start))
                                      (out-str (make-bytevector str-length))
                                      ]
                                  (bytevector-copy!
                                       backing-buffer
                                       (+ offset string-start)
                                       out-str
                                       0
                                       str-length
                                       )
                                  (set! str-list (cons out-str str-list))
                                  (set! string-start #f)
                                  )]
                               [(and (not string-start) (not (= u8 0)))
                                (set! string-start index)])
                         (iterator updated-index)
                         )
                       )
                   ))]
        (iterator 0)
        str-list
        ))
    )

  (define (sh_flags->string flags)
    (let [(flags-list '())]
      (fold-left
       (lambda (flags-so-far flag)
         (let [(flag-value (car flag))]
           (if (fxbit-set? flags flag-value)
               (cons (cdr flag) flags-so-far)
               flags-so-far))
         )
       '()
       '((0 . "WRITE")
         (1 . "ALLOC")
         (2 . "EXECINSTR")
         (3 . "MERGE")
         (4 . "STRINGS")
         (5 . "INFO_LINK")
         (6 . "LINK_ORDER")
         (7 . "OS_NONCONFORMING")
         (8 . "GROUP")
         (9 . "TLS")
         (10 . "MASKOS")
         (11 . "MASKPROC")
         (12 . "ORDERED")
         (13 . "EXCLUDE"))))
    )

  (define (dump-section-header elf header section-header)
    (let* [(ei_class (elf-file-header-ei_class header))
           (arch-variable-formatter-hex (cond [(= ei_class 1) "~8,'0X"]
                                              [(= ei_class 2) "~16,'0X"]))]
      (display (format "sh_name: ~8,'0X" (elf-section-header-sh_name section-header)))
      (newline)
      (display "sh_type: ")
      (display (sh_type->string (elf-section-header-sh_type section-header)))
      (newline)
      
      (display "sh_flags: ")
      (display (sh_flags->string (elf-section-header-sh_flags section-header)))
      (newline)

      (display "sh_addr: ")
      (display (format arch-variable-formatter-hex (elf-section-header-sh_addr section-header)) )
      (newline)

      (display "sh_offset: ")
      (display (format arch-variable-formatter-hex (elf-section-header-sh_offset section-header)) )
      (newline)

      (display "sh_size: ")
      (display (format arch-variable-formatter-hex (elf-section-header-sh_size section-header)) )
      (newline)

      (display (format "sh_link: ~8,'0X" (elf-section-header-sh_link section-header)))
      (newline)
      (display (format "sh_info: ~8,'0X" (elf-section-header-sh_info section-header)))
      (newline)

      (display "sh_addralign: ")
      (display (format arch-variable-formatter-hex (elf-section-header-sh_addralign section-header)) )
      (newline)

      (display "sh_entsize: ")
      (display (format arch-variable-formatter-hex (elf-section-header-sh_entsize section-header)) )
      (newline)

      )
    )
  )

