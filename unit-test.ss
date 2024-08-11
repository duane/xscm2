(library (unit-test)
  (export unit-test-main)
  (import (chezscheme))

  (define-condition-type
    &expectation-violation
    &assertion
    make-expectation-violation
    expectation-violation?
    (expected expectation-violation-expected)
    (actual expectation-violation-actual))

  (define (expect expected actual)
    (if (equal? expected actual) 'nil (raise (make-expectation-violation expected actual))))

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
		 (set! output (string-append output (format "~A\t~d. ~A" pre-padding ord rendered)))
		 (set! count (+ count 1))))))
       render-condition-handlers)
      output))

  (define (run-test name proc)
    (guard (con [else (display 'fail) (newline)
		      (display (render-condition con))
		      #f])
      (proc)
      (display 'pass)
      #t))

  (define (run-test-suite suite)
    (let* ([tests-total (length suite)]
	   [tests-count 0]
	   [tests-passed 0])
      (for-each
       (lambda (test)
	 (let* ([name (car test)]
		[proc (cdr test)])
	   (display (format "~A... " name))
	   (let ([result (run-test name proc)])
	     (if result (set! tests-passed (+ tests-passed 1)))
	     (newline)
	   (set! tests-count (+ tests-count 1))	     
	   )
	   ))
       suite)
      (assert (equal? tests-total tests-count))
      (display (format "\t ~d/~d tests passed" tests-passed tests-total)) (newline)
      (>= tests-total tests-passed)))

  ;; runtime information

  (define *tests* '())
  (define (define-test name proc) (set! *tests* (cons (cons name proc) *tests*)))

  ;; macro stuff
  (define-syntax test
    (syntax-rules ()
      [(test name body1 ...) (define-test 'name (lambda () body1 ...))]))

  ;; driver stuff
  (define (run-tests) (run-test-suite *tests*))

  
  ;; tests and assertions

  (define *-example-expectation-violation-proc-* (lambda () (expect #t '(1 two (three)))))
  (define *-example-assertion-violation-proc-* (lambda () (assert #f)))
  (define *-example-who-condition-proc-* (lambda () (assertion-violation 'example-who "message")))
  
  (define *-example-expectation-violation-*
    (guard (con [else con]) (*-example-expectation-violation-proc-*)))
  (define *-example-assertion-violation-* (guard (con [else con]) (*-example-assertion-violation-proc-*)))
  (define *-example-who-condition-* (guard (con [else con]) (*-example-who-condition-proc-*)))
  
  (define *-example-failing-suite-*
    `(["Example assertion violation" . ,(lambda () (assert #f))]
      ["Example expectation violation" . ,(lambda () (expect #t '(1 two (three))))]
      ["Example who violation" . ,(lambda () (assertion-violation 'example-who "message"))]
      ["Example passing test" . ,(lambda () (display 'woo-hoo))]))

  (define *-example-passing-suite-*
    `(["Example assertion violation" . ,(lambda () (assert #t))]
      ["Example expectation violation" . ,(lambda () (expect #t #t))]
      ["Example passing test" . ,(lambda () (display 'woo-hoo))]))

  (define (verify-failure) (run-test-suite *-example-failing-suite-*))
  (define (verify-passing) (run-test-suite *-example-passing-suite-*))

  ;; at-runtime test definitions
  (define (unit-test-main)
      (test passing-test (assert #t))
      (run-tests)
      )
  )
