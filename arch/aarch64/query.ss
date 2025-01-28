(library (arch aarch64 query)
  (export load-instructions
	  ;; *instruction-list*
	  *instructions*
	  instruction-named
	  instruction-constant
	  instruction-seq
	  instruction-name
	  instruction-full-name
	  instruction-variant
	  instruction-prependage
	  instruction-specific
	  instruction-appendage
	  instruction-seq
	  named-field-name
	  idb
	  list-instructions
	  list-field-names)
  (import (chezscheme)
	  (asm build)
	  (asm errors)
	  (arch aarch64 registers)
;	  (arch aarch64 instruction)
	  (util hex)
	  (unit-test))
  
  (define-record-type named-field
    (fields name start end))
  
  (define-record-type constant-field
    (fields constant start end))

  (define (instruction-named insn)
    (fold-left (lambda (accum val) (if (named-field? val) (cons val accum) accum))
	  '()
	  (instruction-seq insn)))
  
  (define (instruction-constant insn)
    (fold-left (lambda (accum val) (if (constant-field? val) (cons val accum) accum))
	       '()
	       (instruction-seq insn)))

  (define (read-field val start end)
    (if (symbol? val)
	(make-named-field val start end)
	(make-constant-field val start end)))

  (define-record-type instruction
    (fields name	     ; mnemonic
	    full-name	     ; name referring to a specific invocation
	    variant	     ; TODO: figure this out
	    prependage	     ; TODO: figure this out
	    specific	     ; TODO: figure this out
	    appendage	     ; TODO: figure this out
	    seq ; arguments of the bitfield ordered from most to least significant
	    ))

  ;; read a line of the instruction definitions
  (define (read-instruction name full-name variant prependage specific appendage seq)
    (make-instruction
     (string->symbol name)
     (string->symbol full-name)
     variant
     prependage
     specific
     appendage
     (map (lambda (op) (apply read-field op)) seq)
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

  ;; an arbitrarily-ordered list of aarch64 instructions
  (define *instruction-list* (load-instructions))

  (define (take lst n)
    (if (or (zero? n) (null? lst))
        '()
        (cons (car lst) (take (cdr lst) (- n 1)))))

  (define *instructions* (take *instruction-list* 10))
  (define (idb insn) 
    (display (instruction-full-name insn))
    (for-each
     (lambda (f)
       (display " ")
       (display (named-field-name f)))
     (instruction-named insn))
    (newline))

  (define (list-instructions)(for-each idb *instructions*))

  (define (list-field-names)
    (let* ([table (make-hashtable symbol-hash symbol=?)])
      (for-each
       (lambda (insn)
	 (for-each
	  (lambda (field)
	    (let* ([field-name (named-field-name field)]
		   [count (symbol-hashtable-ref table field-name #f)
			  ]
		   [new-count (+ (if count count 0) 1)])
	      (symbol-hashtable-set! table field-name new-count))
	    )
	  (instruction-named insn)))
       *instruction-list*)
      (let*-values ([(keys values) (hashtable-entries table)]
		    [ ( zipped) (vector->list (vector-map cons keys values))]
		    [(sorted) (sort (lambda (a b) (<= (cdr a) (cdr b))) zipped)])
		   sorted))
    )

  )
