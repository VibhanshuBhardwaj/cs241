start: .word start ;0

i1: .word i2 ; 8

i2: .word i1 ;4

anotherLabel: jr $2
i0: i3: i4: .word i4 ;should print 12

shudbe4: .word 2

b1: beq $0, $0, b2


blah: add $1, $2, $3

uh:

b2: beq $0, $0, b1

bne: bne $0, $0, beq
beq: beq $1, $2, bne
lol: LOLWA: 

branhBitch: onemore: anotherOne: beq $0, $0, afterwards

afterwards: .word 2 
