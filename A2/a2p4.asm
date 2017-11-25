lis $4
.word 4 ;word size
lis $8
.word 1
;add $8, $2, $8
;mult $4, $8
;mflo $8
add $5, $1, $0 ;$5 will be the register we'll use to iterate
lw $3, 0($1) ;the max is intially the first element

loop: lw $6, 0($5)
slt $7, $3, $6 ; if $3 < $6, $6 is our max and $7 is 1. so when $7 is 0, we just move to next

beq $7, $0, increment
add $3, $6, $0

increment: add $5, $5, $4
decrement2: sub $2, $2, $8
bne $2, $0, loop
jr $31
