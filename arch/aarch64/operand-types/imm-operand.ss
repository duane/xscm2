(define (render-imm-selector size align signed?)
  (if (= 1 align)
      (format "~Aimm~d" (if signed? "s" "u") size)
      (format "~Aimm~da~d" (if signed? "s" "u") size align)))

(define-syntax make-imm-encode
  (syntax-rules ()
    [(_ size align is-signed?)
     (let ([mask (- (fxsll 2 (- size 1)) 1)]
	   [align-factor (fxsll 2 2)])
       (lambda (val)
	 (display (format "val ~09b" val)) (newline)
	 (let-values ([(encoded rem) (fxdiv-and-mod val align-factor)])
	   (display (format "encoded ~08b rem ~03b" encoded rem)) (newline)
	   ;; assert inner bound
	   (assert (fxzero? rem))
	   (let ([val-mask (- (fxsll 2 (- size 1)) 1)])
	     (display (format "val-mask ~08b" val-mask)) (newline)
	     (display (format "encoded ^ val-mask ~08b" (fxxor encoded val-mask))) (newline)
	     ;; assert outer bound
	     (assert (fx<= (fxxor encoded val-mask) val-mask))
	     encoded))))]))

(define-syntax make-imm-select
  (syntax-rules ()
    [(_ size align is-signed?)
     (lambda (val)
       (let ([is-sign-valid? (if is-signed? #t (>= val 0))])
	 (if (not is-sign-valid?) '(sign-invalid #f)
	     (let* ([align-minus-one (- align 1)]
		    [alignment (fxsll 2 align-minus-one)]
		    [is-aligned? (fxzero? (mod val alignment))])
	       (display alignment) (newline)
	       (if (not is-aligned?) 'align-invalid
		   (let ([min-val (if is-signed? (- (fxsll 2 (+ (- align-minus-one 1) size)))
				      0)]
			 [max-val (if is-signed? (- (fxsll 2 (- (+ align-minus-one size) 1))
						    (fxsll 2 (- 2 align-minus-one)))
				      (- (fxsll 2 (+ align-minus-one size)) 1))])
		     (display min-val) (newline)
		     (display max-val) (newline)
		     (if (or (< val min-val) (> val max-val)) '(size-invalid #f) #t)
		     )))))
       )]))

(define-record-type imm-selector
  (parent operand-selector)
  (protocol
   (lambda (new)
     (lambda (size align is-signed?)
       (let ([render (render-imm-selector size align is-signed?)]
	     [select (make-imm-select size align is-signed?)]
	     [encode (make-imm-encode size align is-signed?)])
	 ((new render select encode))))
     )))

;; (s|u)imm<Size>[a<Align>]
(define (parse-imm-selector sym) (parse-imm-selector-str (symbol->string sym)))
(define (parse-imm-selector-str str)
  (let ([len (string-length str)])
    (assert (> len 4))
    (let ([first-char (string-ref str 0)]
	  [is-signed? (char=? #\s (string-ref str 0))])
      (assert (or (char=? first-char #\s) (char=? first-char #\u)))
      (assert (string=? "imm" (substring str 1 4)))
      (let* ([long-size? (and  (> len 5) (char-numeric? (string-ref str 5)))]
	     [total-len-size (if long-size? 6 5)]
	     [size-str (if long-size? (substring str 4 6) (substring str 4 5))]
	     [size (string->number size-str)])
	(assert size)
	(assert (> size 0))
	(assert (<= size 64))
	(let ([remaining (- len total-len-size)])
	  (assert (<= remaining 3))
	  (if (> remaining 0)
	      (let ([next-char (string-ref str total-len-size)])
		(assert (> remaining 1))
		(assert (char=? next-char #\a))
		(let* ([align-start (+ total-len-size 1)]
		       [align-end (+ align-start (- remaining 1))]
		       [align-str (substring str align-start align-end)]
		       [align (string->number align-str)])
		  (assert (> align 0))
		  (assert (<= align 64))
		  (make-imm-selector size align is-signed?))
		)
	      (make-imm-selector size 1 is-signed?)
	      ))
	)
      )
    )
  )


;; Wn|Wt|Wt2|Wd|Xn|Xt|Xt2|Sp
;; (define (parse-reg-selector str)
;;   )

(define (parse-operand-selector-specifier symbol)
  (let ([str (symbol->string symbol)])
    (cond [(or (string-starts-with? str "uimm")
	       (string-starts-with? str "simm"))
	   (parse-imm-selector str)])))


;; (assert (string=? "simm1" (render-imm-selector 1 1 #t)))
;; (assert (string=? "simm16" (render-imm-selector 16 1 #t)))
;; (assert (string=? "simm16a5" (render-imm-selector 16 5 #t)))
;; (assert (string=? "uimm1" (render-imm-selector 1 1 #f)))
;; (assert (string=? "uimm16" (render-imm-selector 16 1 #f)))
;; (assert (string=? "uimm16a5" (render-imm-selector 16 5 #f)))

