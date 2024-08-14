ARM64 SCHEME ASSEMBLER

# Syntax Equivalents

## Memory offsets

Register address

```
[Xn|sp]
```

becomes

```
(Xn|sp)
```

Signed immediate offset

```
[Xn|sp, +-imm9]
```

becomes

```
(Xn|sp +-imm9)
```

Unsigned immediate offset

```
[Xn|sp, imm12]
```

becomes

```
(Xn|sp imm12)
```

Pre-indexed immediate offset

```
[Xn|sp, +-imm9]!
```

becomes

```
(! Xn|sp +-imm9)
```

Post-indexed immediate offset

```
[Xn|sp], +-imm9
```

becomes

```
(Xn|sp) +-imm9
```

Register offset

```
[Xn|sp, Xm, (U|S)XTW]
```

becomes

```
(Xn|sp Xm <U|S>XTW)
```

Literal

```
<label>
```

becomes

```
<label>
```

Pseduo-load

```
=<immediate|symbol>
```

becomes

```
= <immediate|symbol>
```

# argument types

a
b
imm7
immhi/immlo - adrp only
