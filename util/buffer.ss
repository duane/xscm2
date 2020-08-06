(library (util buffer)
  (export make-buffer buffer-length buffer->bytevector buffer-binary-input/output-port)
  (import (chezscheme))

  (define-record-type buffer
    (fields
     (mutable length)
     (mutable backing-bytevector))
    (protocol
     (lambda (make)
       (lambda arg
         (let [(allocated (if (null? arg) 256 (car arg)))]
           (make 0 (make-bytevector allocated))))))
    )

  (define (buffer->bytevector buffer)
    (let* [(backing-bytevector (buffer-backing-bytevector buffer))
           (length (buffer-length buffer))
           (out-vector (make-bytevector length))]
      (bytevector-copy! backing-bytevector 0 out-vector 0 length)
      out-vector
      )
    )

  (define (reallocate-buffer buffer at-least-length)
    (let [(old-bytevector (buffer-backing-bytevector buffer))
          (new-bytevector (make-bytevector at-least-length))]
      (bytevector-copy! old-bytevector 0 new-bytevector 0 (bytevector-length old-bytevector))
      (buffer-backing-bytevector-set! buffer new-bytevector)
      new-bytevector
      )
    )

  (define (buffer-binary-input/output-port buffer . name)
    (let* [(current-port-position 0)
           (id (if (null? name) "buffer-binary-output-port" (car name)))
           (r! (lambda (bytevector start n)
                 (let* [(len (buffer-length buffer))
                        (backing-bytevector (buffer-backing-bytevector buffer))
                        (bytes-left-to-read (- len current-port-position))
                        (bytes-will-read (min n bytes-left-to-read))
                        (bytes-did-read
                         (cond [(<= bytes-left-to-read 0) 0]
                               [else
                                (bytevector-copy! backing-bytevector current-port-position bytevector start bytes-will-read)
                                bytes-will-read]))]
                   (set! current-port-position (+ current-port-position bytes-did-read))
                   bytes-did-read
                   )
                 ))
           (w! (lambda (bytevector start n)
                 (let [(backing-bytevector (buffer-backing-bytevector buffer))
                       (len (buffer-length buffer))
                       (new-size (+ current-port-position n))]
                   (cond [(> new-size (bytevector-length backing-bytevector))
                          (set! backing-bytevector (reallocate-buffer buffer (* 2 new-size)))])
                   (bytevector-copy! bytevector start backing-bytevector current-port-position n) ; actually write
                   (set! current-port-position new-size) ; update cursor
                   (if (> new-size len) (buffer-length-set! buffer new-size)) ; update length to reflect allocated file size.
                   n ; always write the entire bytevector.
                   )))
           (gp (lambda () current-port-position))
           (sp! (lambda (new-port-position) (set! current-port-position new-port-position)))
           (close #f)]
      (make-custom-binary-input/output-port id r! w! gp sp! close)
      ))
)
