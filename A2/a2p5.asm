lis $4
.word 4 ;word size
lis $8
.word 1

lis $10
.word 0xffff000c
slt $7, $2, $8
beq $7, $8, return

add $5, $1, $0 ;$5 will be the register we'll use to iterate

loop: lw $6, 0($5)

bne $6, $0, printChar

lis $11
.word 32
add $6, $0, $11
sw $6, 0($10)
beq $0, $0, endLoop
printChar:
lis $11
.word 64
add $6, $6, $11
sw $6, 0($10)


endLoop:
increment: add $5, $5, $4
decrement2: sub $2, $2, $8

bne $2, $0, loop
return: jr $31
