_main:
    (stp %fp %lr (! %sp -16))
    (add %fp %sp 0)
    (adrp %x0 l_.str@PAGE)
    (add %x0, %x0, l_.str@PAGEOFF)
    (bl _puts)
    (mov %w0 0)
    (ldp fp lr (> sp 16))
    (ret)

l_.str: (asciz "hello world")
