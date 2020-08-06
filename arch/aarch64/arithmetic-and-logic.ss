#!r6rs

(set! insns2
      (list
       (make-insn2 'add 'add-extended-register #xb2000000 #x7FE00000
                   (list
                    (make-register-type '((16 . 20)) 'Xm 64)
                    (make-register-type '((5 . 9)) 'Xn 64)
                    (make-register-type '((0 . 4)) 'Xd 64)
                    (make-extend-type '((13 . 15)) '((10 . 12)))
                    )
                   )
       (make-insn2 'add 'add-immediate #x11000000 #x7F000000
                   (list
                    (make-register-type '((0 . 4)) 'Xd 64)
                    (make-register-type '((5 . 9)) 'Xn 64)
                    (make-immediate-type '((10 . 21)) 'imm 12 #f)
                    )
                   )
       (make-insn2 'add 'add-shifted-register #xb000000 #x7f200000
                   (list
                    (make-register-type '((0 . 4)) 'Xd 64)
                    (make-register-type '((5 . 9)) 'Xn 64)
                    (make-register-type '((16 . 20)) 'Xm 64)
                    (make-shift2-type '((22 . 23)) '((10 . 15)))
                    )
                   )
       (make-insn2 'adr 'adr #x10000000 #x9F000000
                   (list
                    (make-register-type '((0 . 4)) 'Xd 64)
                    (make-label-type '((5 . 23) (29 . 30)) 64)
                    )
                   )
       (make-insn2 'adrp 'adrp #x90000000 #x9F000000
                   (list
                    (make-register-type '((0 . 4)) 'Xd 64)
                    (make-label-type '((5 . 23) (29 . 30)) 64)
                    ))
       (make-insn2 'movn 'movn #x12800000 #x7F800000
                   (list
                    (make-register-type '((0 . 4)) 'Xd 64)
                    (make-immediate-type '((5 . 20)) 'imm 16 #f)
                    (make-shift-type '((21 . 22)))))
       ;; (make-insn2 'movi 'movi
       ;;             (list
       ;;              (make-register-type '(())')
       ;;              )
       (make-insn2 'movz 'movz
                   (list
                    (make-register-type '((0 . 4)) 'Xd 64))
                   )
        )
       )
