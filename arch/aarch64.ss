(library (arch aarch64)
  (export lookup-instruction
	  named-operand-name named-operand-start named-operand-end
	  constant-operand-constant constant-operand-start constant-operand-end
	  instruction-name instruction-full-name instruction-variant instruction-prependage instruction-specific instruction-appendage instruction-seq
	  instruction-named-operands
	  *aarch64-assembler*
	  *instruction-table*
	  display-instruction display-instructions
	  read-operand
	  read-instruction
	  load-instructions
	  *op-types*
	  query-by-op
	  display-assembler
	  *op-type-histogram*
	  aarch64-assembler-labels
	  cb-hex!
	  aarch64-assembler-code-block
	  aarch64-tests
	  )
  (import (chezscheme)
	  (asm build)
	  (asm errors)
	  (arch aarch64 registers)
	  (util hex)
	  (unit-test))

  ;; operands
  
  (define-record-type named-operand
    (fields name start end))

  (define-record-type constant-operand
    (fields constant start end))

  (define (read-operand val start end)
    (if (symbol? val)
	(make-named-operand val start end)
	(make-constant-operand val start end)))

  ;; instruction
  
  (define-record-type instruction
    (fields name	     ; mnemonic
	    full-name	     ; name referring to a specific invocation
	    variant	     ; TODO: figure this out
	    prependage	     ; TODO: figure this out
	    specific	     ; TODO: figure this out
	    appendage	     ; TODO: figure this out
	    seq ; arguments of the bitfield ordered from most to least significant
	    ))

  ; read a line of the instruction definitions
  (define (read-instruction name full-name variant prependage specific appendage seq)
    (make-instruction
     (string->symbol name)
     (string->symbol full-name)
     variant
     prependage
     specific
     appendage
     (map (lambda (op) (apply read-operand op)) seq)
     )
    )

  ; read all instruction definitions from data file
  (define (load-instructions)
    (let ((instructions '()))
      (load "assets/instructions.ss"
	    (lambda (line)
	      (let* ((raw (if (annotation? line) (annotation-stripped line) line))
		     (parsed (apply read-instruction raw)))
		(set! instructions (cons parsed instructions))
		)
	      ))
      instructions
      )
    )

  ; an arbitrarily-ordered list of aarch64 instructions
  (define *instruction-table* (load-instructions))

  ; retrieve only the named operands of an instruction
  (define (instruction-named-operands insn)
    (fold-left (lambda (named arg) (if (named-operand? arg) (cons arg named) named))
	       '()
	       (instruction-seq insn)))

  ; display the instruction in a natural manner.
  (define (display-instruction insn)
    (display (instruction-full-name insn))
    (for-each (lambda (no)
		(display " ")
		(display (named-operand-name no))
		)
	      (instruction-named-operands insn))
    (newline)
    )
  

;; internal state
  

  (define-record-type code-block
    (fields (mutable buffer)
	    (mutable cursor)
	    (mutable length)
	    (mutable allocated))
    (protocol
     (lambda (make)
       (lambda arg
	 (let [(allocated (if (null? arg) 256 (car arg)))]
	   (make (make-bytevector allocated) 0 0 allocated)))
       )))

  (define (cb-hex! cb)
    (let ((only-used (make-bytevector (code-block-length cb))))
      (bytevector-copy! (code-block-buffer cb) 0 only-used 0 (code-block-length cb))
      (hex-dump only-used)
      )
    )
  
  (define (cb-extend! cb at-least)
    (let* [(new-allocated (* 2 (code-block-allocated cb)))
	   (new-bv (make-bytevector new-allocated))]
      (if (> at-least new-allocated) (raise "asking for too much!"))
      (bytevector-copy! (code-block-buffer cb) 0 new-bv 0 (code-block-length cb))
      (code-block-allocated-set! cb new-allocated)
      (code-block-buffer-set! cb new-bv))
    )
  
  (define (cb-write-aligned-u32! cb u32)
    (let* [(cursor (code-block-cursor cb))
	  (new-cursor (+ cursor 4))
	  (length (code-block-length cb))
	  (new-length (if (> new-cursor length) new-cursor length))]
      (if (< (code-block-allocated cb) new-length) (cb-extend! cb new-length))
      (bytevector-u32-set! (code-block-buffer cb) cursor u32 (endianness big))
      (code-block-cursor-set! cb new-cursor)
      (code-block-length-set! cb new-length)
      )
    )

  
  ;; annotated source stuff
  ;; errors

  (define *expected-register* "expected register")
  (define *wrong-register* "wrong register")
  (define *expected-integer* "expected register")
  (define *value-out-of-range* "value out of range for instruction")
  (define *value-unaligned* "value unaligned")
  (define *expected-pre* "expected preindex")

  (define *expected-arg-error* "expected argument")
  (define *unexpected-arg-error* "unexpected argument")
  (define *expected-immediate* "expected immediate")
  (define *no-such-instruction* "no such instruction")



  (define (max-value-unsigned nbits) (- (fxsll 1 nbits) 1))
  (define (max-value-signed nbits) (max-value-unsigned (- nbits 1)))
  (define (min-value-signed nbits) (- -1 (max-value-signed nbits)))
  (define (max-value nbits signed) (if signed (max-value-signed nbits) (max-value-unsigned nbits)))
  (define (min-value nbits signed) (if signed (min-value-signed nbits) 0))
  
  ;; (define (parse-annotated-r64 annotated)
  ;;   (let ((reg (parse-annotated-reg annotated)))
  ;;     (if (not (= 64 (register-size reg)))
  ;; 	  (raise-assembler-error *wrong-register* annotated))
  ;;     reg))
  (define (parse-annotated-r64 annotated) display)

  (define (parse-annotated-immn annotated n-bits align signed)
    (let ((i (annotation-expression annotated)))
      (if (not (integer? i)) (raise-assembler-error *expected-integer* annotated))
      (let ((lower (* align (min-value n-bits signed)))
	    (upper (* align (max-value n-bits signed))))
	(if (or (> i upper) (< i lower)) (raise-assembler-error *value-out-of-range* annotated)))
      (let-values ([(q r) (fxdiv-and-mod i align)])
	(if (not (= 0 r)) (raise *value-unaligned*))
	q
	)
      )
    )
  
  (define (parse-annotated-pre annotated n-bits align signed)
    (let ((arg (annotation-expression annotated)))
      (if (or (not (list? arg)) (null? arg))
	  (raise *expected-pre*))
      (let* ((annotated-bang (car arg))
	     (bang (annotation-expression annotated-bang))
	     (rest (cdr arg)))
	(if (not (equal? bang '!) ) (raise-assembler-error "expected !" annotated))
	(if (or (not (list? rest)) (null? rest))
	    (raise *expected-pre*))
	(let* ((annotated-Rn (cadr arg))
	      (Rn (annotation-expression annotated-Rn))
	      (rest (cddr arg)))
	  (if (not (symbol? Rn)) (raise-assembler-error *expected-register* annotated-Rn))
	  (if (or (not (list? rest)) (null? rest))
	      (raise *expected-pre*))
	  (let ((reg (parse-annotated-r64 annotated-Rn)))
	    (let* ((annotated-imm (caddr arg))
		   (imm (parse-annotated-immn annotated-imm n-bits align signed)))
	      (if (not (null? (cdddr arg))) (raise *expected-pre*))
	      
	      (cons reg imm)
	      ))
	  )
	))
    )

  

  (define (parse-annotated-stp args)
    (let* ((Rt (register-numeric (parse-annotated-r64 (car args))))
	   (Rt2 (register-numeric (parse-annotated-r64 (cadr args))))
	   (Rn-and-imm7 (parse-annotated-pre (caddr args) 7 8 #t))
	   (Rn (register-numeric (car Rn-and-imm7)))
	   (imm7 (cdr Rn-and-imm7)))
      (fxior 2843738112
	     (fxsll (fxand 127 imm7) 15)
	     (fxsll (fxand 31 Rt2) 10)
	     (fxsll (fxand 31 Rn) 5)
	     (fxsll (fxand 31 Rt) 0))
      )
    )

  (define *op-types*
    (let ((types (make-hashtable symbol-hash symbol=?)))
      (for-each (lambda (insn)
		  (for-each (lambda (op)
			      (if (named-operand? op) (hashtable-set! types (named-operand-name op) #t))
			      ) (instruction-seq insn)))
		*instruction-table*)
      types))

  (define *op-type-histogram*
    (let ((histogram (make-hashtable symbol-hash symbol=?)))
      (for-each
       (lambda (insn)
	 (for-each
	  (lambda (op)
	    (if (named-operand? op)
		(let* ((name (named-operand-name op))
		       (cur-count (hashtable-ref histogram name 0))
		       (new-count (+ 1 cur-count)))
		  (hashtable-set! histogram name new-count)))
	    )
	  (instruction-seq insn)
	  ))
       *instruction-table*)
      histogram
      ))

  (define *instruction-lookup*
    (let ((table (make-hashtable symbol-hash symbol=?)))
      (begin
	(for-each
	 (lambda (insn)
	   (hashtable-set! table (instruction-full-name insn) insn))
	 *instruction-table*)
	table
	)
      )
    )

  (define (lookup-instruction sym) (hashtable-ref *instruction-lookup* sym #f))

  
  (define-record-type instruction2
    (fields
     optypes))
  (define *mnemonic-dispatch*
    (let ((table (make-hashtable symbol-hash symbol=?)))
      (hashtable-set! table 'stp `(
				   ,(make-instruction2 '(parse-annotated-r64 parse-annotated-r64 parse-annotated-pre))))
      table
      ))
  
  (define (*aarch64-dispatch* asm annotated)
    (lambda (_x) #t)
    )

  ;; util
  (define (instruction-has-op insn op)
    (letrec ((seq-has-ops
	      (lambda (seq)
		(if (null? seq) #f
		    (let ((element (car seq)))
		      (if (and (named-operand? element)
			       (equal? (named-operand-name element) op))
			  #t
			  (seq-has-ops (cdr seq))
			  ))))))
      (seq-has-ops (instruction-seq insn))
      ))
  
  (define (query-by-op op)
    (fold-left
     (lambda (insns insn)
       (if (instruction-has-op insn op)
	   (cons insn insns)
	   insns))
     '()
     *instruction-table*
     ))

  (define (display-instructions insns)
    (for-each display-instruction insns))

  ;; errors  

  (define (validate-register arg)
    (if (not (and (symbol? arg) (parse-register arg))) (raise *expected-register*)))
  

  ;; (define (parse-add args)
  ;;   (let* ((Rd (register-numeric (parse-r64 (car args))))
  ;; 	   (Rn (register-numeric (parse-r64 (cadr args))))
  ;; 	   (imm12 (parse-immn (caddr args) 12 1 #f))
  ;; 	   (shift (if (not (null? (cdddr args))) (parse-immn (cadddr args) 1 1 #f)))
  ;; 	   )
  ;;     (fxior 2432696320
  ;; 	     (fxsll (fxand 1 shift) 22)
  ;; 	     (fxsll (fxand 4095 imm12) 10)
  ;; 	     (fxsll (fxand 31 Rn) 5)
  ;; 	     (fxsll (fxand 31 Rd) 0))
  ;;     ))


  (define (imm-label-size sym)
    (let* ((str (symbol->string sym))
	   (len (string-length str)))
      (string->number (substring str 3 len))
      )
    )

  (define-record-type aarch64-assembler
    (parent assembler)
    (fields
     (mutable mnemonics) ;; list of mnemonics that define the code block sorted 
     (mutable labels)	 ;; hashtable of offsets by label
     (mutable label-references) ;; hashtable of mnemonics indexed by the location in the mnemonics table above
     (mutable cursor) ;; location where next mnemonic might be emitted
     code-block
     )
    )

    ;; private methods on which common methods rely
  (define (*aarch64-validate-mnemonic* asm mnemonic) #f)

  ;; implementation of common interface methods
  (define (*aarch64-mnemonic-length* asm mnemonic)
    (if (*aarch64-validate-mnemonic* asm mnemonic) 4 #f))
  (define (*aarch64-emit-mnemonic* a64asm annotated-mnemonic)
    #f
    )
  (define (*aarch64-define-label!* a64-asm label)
    (hashtable-set!
     (aarch64-assembler-labels a64-asm)
     label
     (aarch64-assembler-cursor a64-asm)))

  (define *aarch64-assembler*
    (make-aarch64-assembler
       *aarch64-dispatch*
       *aarch64-emit-mnemonic*
       *aarch64-define-label!*
       *aarch64-mnemonic-length*
       '()				   ; mnemonics
       (make-hashtable symbol-hash symbol=?) ; labels
       (make-hashtable symbol-hash symbol=?) ; index
       0					   ; cursor
       (make-code-block)
       )
    )

  (define (display-assembler asm)
    (cb-hex! (aarch64-assembler-code-block asm))
    (let ((labels (aarch64-assembler-labels asm)))
      (vector-for-each
       (lambda (label)
	 (let ((pos (hashtable-ref labels label #f)))
	   
	   (display label)
	   (display ": ")
	   (display pos)
	   (newline))
	 )
       (hashtable-keys labels)
       ))
    )
)
;;  (test test-assembler-exists (if *aarch64-assembler* #t #f))


