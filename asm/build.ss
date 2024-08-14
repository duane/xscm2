(library (asm build)
  (export assembler
	  make-assembler
	  asm-emit-mnemonic!
	  asm-define-label!
	  asm-mnemonic-length
	  asm-load!
	  load-buffer!)
  (import (chezscheme)
	  (asm errors))
  
  (define-record-type assembler
    (fields
     ;; write the mnemonic to the code-block
     internal-dispatch-mnemonic!
     ;; add mnemonic to mnemonic list
     ;; advance cursor by mnemonic length
     internal-emit-mnemonic!
     ;; defines a label at current cursor point
     ;; TODO: throw error on re-definition
     internal-define-label!
     internal-mnemonic-length	  ; returns #f if not a valid mnemonic
     )
    )

  (define (asm-emit-mnemonic! asm mnemonic)
    ((assembler-internal-emit-mnemonic! asm) asm mnemonic))
  (define (asm-define-label! asm label)
    ((assembler-internal-define-label! asm) asm label))
  (define (asm-mnemonic-length asm mnemonic)
    ((assembler-internal-mnemonic-length asm) asm mnemonic))

  (define (assembler-load-symbol! asm annotated)
    (let*
	((str (symbol->string (annotation-expression annotated)))
	 (len (string-length str)))
      (if
       (equal? #\: (string-ref str (- len 1)))
       (let ((stripped (substring str 0 (- len 1))))
	 (asm-define-label! asm (string->symbol stripped))))))


  (define (asm-load! asm)
    (lambda (annotated)
      (let ((expr (annotation-expression annotated)))
	(cond
	 [(symbol? expr) (assembler-load-symbol! asm annotated)]
	 [(list? expr) ((assembler-internal-dispatch-mnemonic! asm) asm annotated)]
	 [else (raise-assembler-error "unknown expression" annotated)])
	)))

  (define (load-buffer! file)
    (let* ((buf '())
	   (eval-proc (lambda (annotation) (set! buf (cons annotation buf)))))
      (load file eval-proc)
      (reverse buf)))
  )
