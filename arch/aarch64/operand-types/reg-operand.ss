(define-syntax make-reg-select
  (syntax-rules ()
    [(_ class)
     (lambda (val) (if (hashtable-contains? class val) #t #f))]))

(define-record-type reg-selector
  (parent operand-selector)
  (protocol
   (lambda (new)
     (lambda (class)
       (let ([render (render-reg-selector class)]
	     [select (make-reg-select class)]
	     [encode (make-reg-encode class)])
	 ((new render select encode))))
     )))
