(library (util file)
  (export with-temp-binary-output-file)
  (import (chezscheme)
          (util hex))
 
  (define (with-temp-binary-output-file target-path proc)
    (let* [(temp-file-name (format "/tmp/tmp-output-~a" (rand-hex-string 8)))
           (output-port (open-file-output-port
                         temp-file-name
                         (file-options)
                         'block
                         #f))]
      (with-exception-handler
          (lambda (e)
            (close-port output-port)
            (delete-file temp-file-name))
        (lambda ()
          (proc output-port)
          (close-port output-port)
          (rename-file temp-file-name target-path)
          )
        )
      )
    )
  )
