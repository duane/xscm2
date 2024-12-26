;;; register selector
;;; ((X|W) class ...)

;; (define-syntax make-reg-render
;;   (syntax-rules ()
;;     [(_ class ...)
;;      (lambda (_) (format "~A" (list 'class ...)))]))

;; (define-syntax make-reg-select
;;   (syntax-rules ()
;;     [(_ class rest ...)
;;      (lambda (val)
;;        (or (set-contains-symbol? class val)
;; 	   ())
;;        )]))

;; Register class descriptor
;; (class ...)
(define-record-type reg-selector
  (parent operand-selector)
  (protocol
   (lambda (new)
     (lambda (first-class . more-classes)
       (let* ([classes (cons first-class more-classes)]
	      [render]
	      [select]
	      [encode])
	 ))
     )))
