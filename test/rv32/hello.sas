_start:

(lui %t0 #x10010)

(andi %t1 %t1 0)
(addi %t1 %t1 72)
(sw %t1 (%t0))

(andi %t1 %t1 0)
(addi %t1 %t1 101)
(sw %t1 (%t0))

(andi %t1 %t1 0)
(addi %t1 %t1 108)
(sw %t1 (%t0))

(andi %t1 %t1 0)
(addi %t1 %t1 108)
(sw %t1 (%t0))

(andi %t1 %t1 0)
(addi %t1 %t1 111)
(sw %t1 (%t0))

(andi %t1 %t1 0)
(addi %t1 %t1 46)
(sw %t1 (%t0))

(andi %t1 %t1 0)
(addi %t1 %t1 10)
(sw %t1 (%t0))

finish:
(beq %t1 %t1 finish)
