(define-syntax make-reg-select
  (syntax-rules ()
    [(_ class)
     (lambda (val)
       (set-contains-symbol? class val))]))

(define-record-type reg-selector
  (parent operand-selector)
  (protocol
   (lambda (new)
     (lambda (class-name class)
       (let ([render class-name]
	     [select (lambda (val) (set-contains-symbol? class val))]
	     [encode (lambda (val) (register-numeric (parse-register! val)))]
	     )
	 ((new render select encode)))))))

(define-syntax parse-reg-selector
  (syntax-rules ()
    [(_ class-name)
     (make-reg-selector 'class-name class-name)]))
