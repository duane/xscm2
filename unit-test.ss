(library (unit-test)
  (export suite expect)
  (import (chezscheme))
  (define (run-tests-file tests-file)
    (parameterize ([compile-profile 'source])
      (load tests-file)))
  
  (define-condition-type
    &expectation-violation
    &assertion
    make-expectation-violation
    expectation-violation?
    (expected expectation-violation-expected)
    (actual expectation-violation-actual))

  (define (expect expected actual)
    (if (equal? expected actual) 'nil (raise
				       (condition
					(make-error)
					(make-expectation-violation expected actual)))))

  (define-syntax expect-values
    (syntax-rules ()
      [(_ (lh-value ...) rh-value)
       (call-with-values (lambda () rh-value) (lambda rh-values (expect (list lh-value ...) rh-values)))]))

  (define (render-expectation-violation ev)
    (format "expected\n\t~A\n   actual\n\t~A"
	    (expectation-violation-expected ev)
	    (expectation-violation-actual ev)))

  (define (render-who-condition con)
    (format "who ~A" (condition-who con)))

  (define (render-irritants-condition con)
    (format "irritants ~A" (condition-irritants con)))

  (define (render-message-condition con)
    (format "message ~A" (condition-message con)))

  (define render-condition-handlers
    `((,expectation-violation? . ,render-expectation-violation)
      (,irritants-condition? . ,render-irritants-condition)
      (,who-condition? . ,render-who-condition)
      (,message-condition? . ,render-message-condition)))

  (define (render-condition con)
    (let ([outputs '()]
	  [count 0]
	  [output ""])
      (for-each
       (lambda (handler)
	 (let ([predicate (car handler)]
	       [render-proc (cdr handler)])
	   (if (predicate con)
	       (let ([ord (+ count 1)]
		     [rendered (render-proc con)]
		     [pre-padding (if (equal? 0 count) "" "\n")])
		 (set! output (string-append output (format "~A\t\t~d. ~A" pre-padding ord rendered)))
		 (set! count (+ count 1))))))
       render-condition-handlers)
      output))

  ;; runtime stuff
  (define *should-reraise-test-exceptions* #t)
  
  ;; macro stuff
  (define-syntax make-test
    (syntax-rules ()
      [(_ name body ...)
       (lambda ()
	 (guard (con [else (display "\x1b;[31;20mf\x1b;[0m") (newline)
			   (display-condition con)
			   (newline)
			   (if *should-reraise-test-exceptions* (raise con))
			   #f])
	   (display #\tab)
	   (display 'name)
	   (display "...")
	   (begin body ...)
	   (display "\x1b;[32;20mp\x1b;[0m")
	   (newline)
	   #t))
       ]
      ))

  (define-syntax suite
    (syntax-rules (test)
      [(_ name (test test-name body ...) ...)
       (lambda ()
	 (display 'name) (newline)
	 (let* ([count 0]
		[passing 0])
	   (let ([result ((make-test test-name body ...))])
	     (set! count (+ count 1))
	     (if result (set! passing (+ passing 1)))
	     )
	   ...
	   (display (format "~d/~d passing\n" passing count))
	   ))]
      ))



  )
