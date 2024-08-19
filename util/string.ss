(library (util string)
  (export string-ends-with? string-starts-with?)
  (import (chezscheme))

  (define (string-ends-with-char? str char)
    (let ((len (string-length str)))
      (equal? (string-ref str (- len 1)) char)))

  (define (string-ends-with? str pat)
    (let* ([pat-len (string-length pat)]
	   [str-len (string-length str)]
	   )
      (if (> pat-len str-len) #f
	  (let* ([str-end (substring str (- str-len pat-len) str-len)]
		 [result (string=? str-end pat)])
	    result))))

  (define (string-starts-with? str pat)
    (let* ([pat-len (string-length pat)]
	   [str-len (string-length str)])
      (if (> pat-len str-len) #f
	  (let* ([str-start (substring str 0 pat-len)]
		 [result (string=? str-start pat)])
	    result))))
  )

