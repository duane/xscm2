#!r6rs

(import (rnrs)
	(util hex)
	(struct pack))

(define LC_SYMTAB #x2)
(define LC_SEGMENT_64 #x19)
(define LC_BUILD_VERSION #x32)
(define LC_LINKER_OPTIMIZATION_HINT #x2E)
(define LC_DYSYMTAB #xb)

(define hello-world-c-string #vu8(#x68
				  #x65
				  #x6c
				  #x6c
				  #x6f
				  #x20
				  #x77
				  #x6f
				  #x72
				  #x6c
				  #x64
				  #x0))


(define-record-type mach-object-file-header
  (fields
   magic
   cpu-type
   sub-cpu-type
   filetype
   ncmds
   size-of-commands
   flags
   reserved))

(define (put-file-header binary-output-port header)
  (put-pack binary-output-port
	    "<8L"
	    (mach-object-file-header-magic header)
	    (mach-object-file-header-cpu-type header)
	    (mach-object-file-header-sub-cpu-type header)
	    (mach-object-file-header-filetype header)
	    (mach-object-file-header-ncmds header)
	    (mach-object-file-header-size-of-commands header)
	    (mach-object-file-header-flags header)
	    (mach-object-file-header-reserved header)))
	    

(define-record-type segment-command-64
  (fields
   cmd
   cmd-size
   seg-name
   vm-addr
   vm-size
   file-offset
   file-size
   max-prot
   init-prot
   n-sects
   flags))

(define seg-size-hack 312)

(define (n-zero-padded-str str n)
  (let [(padded (make-bytevector n 0))
	(utf8-str (string->utf8 str))]
    (bytevector-copy! utf8-str 0 padded 0 (min n (bytevector-length utf8-str)))
    padded))

(define (put-segment-command-64 binary-output-port cmd)
  (let [(seg-name-padded (n-zero-padded-str (segment-command-64-seg-name cmd) 16))]
    (put-pack binary-output-port "<2L" LC_SEGMENT_64 seg-size-hack)
    (put-bytevector binary-output-port seg-name-padded)
    (put-pack binary-output-port "<4Q 4L"
	      (segment-command-64-vm-addr cmd)
	      (segment-command-64-vm-size cmd)
	      (segment-command-64-file-offset cmd)
	      (segment-command-64-file-size cmd)
	      (segment-command-64-max-prot cmd)
	      (segment-command-64-init-prot cmd)
	      (segment-command-64-n-sects cmd)
	      (segment-command-64-flags cmd))
	      
    )
  )

(define-record-type section-64
  (fields
   sect-name
   seg-name
   addr
   size
   offset
   align
   reloff
   nreloc
   flags
   reserved1
   reserved2
   reserved3))

(define (put-section-64 binary-output-port section)
  (put-bytevector binary-output-port (n-zero-padded-str (section-64-sect-name section) 16))
  (put-bytevector binary-output-port (n-zero-padded-str (section-64-seg-name section) 16))
  (put-pack binary-output-port "<2Q 8L"
	    (section-64-addr section)
	    (section-64-size section)
	    (section-64-offset section)
	    (section-64-align section)
	    (section-64-reloff section)
	    (section-64-nreloc section)
	    (section-64-flags section)
	    (section-64-reserved1 section)
	    (section-64-reserved2 section)
	    (section-64-reserved3 section)
	    )
  )

(define-record-type build-version-command
  (fields
   cmd
   cmd-size
   platform
   minos
   sdk
   ntools))

(define (put-build-version-command binary-output-port)
  (put-pack binary-output-port "<6L"
	    LC_BUILD_VERSION
	    24
	    1
	    #x000E0000
	    #x000E0000
	    #x0))

(define (put-linker-optimization-hint binary-output-port)
  (put-pack binary-output-port "<4L"
	    LC_LINKER_OPTIMIZATION_HINT
	    16
	    600
	    8)
  )

(define-record-type linkedit-data-command
  (fields
   cmd
   cmd-size
   data-offset
   data-size))

(define-record-type symtab-command
  (fields
   cmd
   cmd-size
   sym-offset
   str-offset
   str-size))

(define (put-symtab-command binary-output-port)
  (put-pack binary-output-port "<6L"
	    LC_SYMTAB
	    24
	    608
	    6
	    704
	    40)
  )

(define-record-type dysym-tab-command
  (fields
   cmd
   cmd-size
   idx-local-symbols
   num-local-symbols
   idx-ext-symbols
   num-ext-symbols
   idx-undef-symbols
   num-undef-symbols
   toc-offset
   num-toc
   mod-tab-offset
   num-mod-tab
   extra-sym-offset
   num-extra-sym-offset
   indirect-sym-offset
   num-indirect-sym-offset
   extra-rel-offset
   num-extra-rel
   loc-rel-offset
   num-loc-rel
   ))

(define (put-dysymtab-command binary-output-port)
  (put-pack binary-output-port "<20L"
	    LC_DYSYMTAB
	    80
	    0
	    4
	    4
	    1
	    5
	    1
	    0
	    0
	    0
	    0
	    0
	    0
	    0
	    0
	    0
	    0
	    0
	    0))

(define (pack-macho-header-aarch64 binary-output-port)
  (put-pack binary-output-port
	    "<8L"
	    #xfeedfacf ; magic 4 byte header for arm64
	    #x100000C  ; cpu-type
	    #x0        ; sub cpu-type
	    #x1        ; filetype
	    #x0        ; ncmds
	    #x0        ; size_of_commands
	    #x0        ; flags
	    #x0        ; reserved
	    )
  )


(define text-bv (hex->bytevector "FD7BBFA9 FD030091 00000090 00000091 00000094 00008052 FD7BC1A8 C0035FD6"))

(define compact-unwind-bv #vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x20 #x00 #x00 #x00 #x00 #x00 #x00 #x04 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))

(define text-reloff #vu8(#x10 #x00 #x00 #x00 #x05 #x00 #x00 #x2D #x0C #x00 #x00 #x00 #x01 #x00 #X00 #x4C #x08 #x00 #x00 #x00 #x01 #x00 #x00 #x3D))

(define compact-unwind-reloff #vu8(#x00 #x00 #x00 #x00 #x01 #x00 #x00 #x06))

(define linker-optimization-data #vu8(#x07 #x02 #x08 #x0C #x00 #x00 #x00 #x00))

(define symtab-bin (hex->bytevector "20000000 0E010000 00000000 00000000 07000000 0E020000 20000000 00000000 1A000000 0E020000 20000000 00000000 14000000 0E030000 30000000 00000000 0E000000 0F010000 00000000 00000000 01000000 01000000 00000000 00000000 005F7075 7473006C 5F2E7374 72005F6D 61696E00 6C746D70 32006C74 6D703100 6C746D70 30000000"))

(define (write-test-file)
  (let [(output-port (open-file-output-port "test-macho.o" (file-options no-fail) 'block #f))
	(header (make-mach-object-file-header
		 #xfeedfacf
		 #x100000C
		 #x0
		 #x1
		 5 ; 5 commands
		 #x1c8 ; size of commands
		 #x2000 ; flags
		 #x0 ; reserved
		 ))
	(seg-64 (make-segment-command-64
		 #x19
		 312
		 ""
		 #x0
		 #x50
		 488
		 80
		 #x7
		 #x7
		 3
		 #x0))
	(text-section
	 (make-section-64
	  "__text"
	  "__TEXT"
	  #x0
	  #x20
	  488
	  2
	  568
	  3
	  #x80000400
	  0
	  0
	  0))
	(strings-section
	 (make-section-64
	  "__cstring"
	  "__TEXT"
	  #x20
	  #xc
	  520
	  0
	  0
	  0
	  #x02000000
	  0
	  0
	  0))
	(unwind-section
	 (make-section-64
	  "__compact_unwind"
	  "__LD"
	  #x30
	  #x20
	  536
	  3
	  592
	  1
	  #x02000000
	  0
	  0
	  0))]
    (put-file-header output-port header)
    (put-segment-command-64 output-port seg-64)
    (put-section-64 output-port text-section)
    (put-section-64 output-port strings-section)
    (put-section-64 output-port unwind-section)
    (put-build-version-command output-port)
    (put-linker-optimization-hint output-port)
    (put-symtab-command output-port)
    (put-dysymtab-command output-port)
    (put-bytevector output-port text-bv)
    (put-bytevector output-port hello-world-c-string)
    (put-bytevector output-port (make-bytevector 4 0)) ; padding - align to 8 bytes
    (put-bytevector output-port compact-unwind-bv)
    (put-bytevector output-port text-reloff)
    (put-bytevector output-port compact-unwind-reloff)
    (put-bytevector output-port linker-optimization-data)
    (put-bytevector output-port symtab-bin)
    (close-port output-port)
    )
  )

(write-test-file)
