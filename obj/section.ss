(library (obj section)
  (export obj-section-flags make-obj-section obj-section-name obj-section-type obj-section-buffer)
  (import (chezscheme)
          (util buffer))

  (define-enumeration
    obj-section-flag
    (bss no-load writable data read-only executable allocatable)
    obj-section-flags)

  (define-record-type obj-section
    (fields
     name
     type
     buffer))
  )
