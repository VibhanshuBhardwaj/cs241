;adding prolog for isPrime
;adding prolog for wain
.import print
.import init
.import delete
.import new
lis $4
.word 4
lis $11
.word 1
beq $0, $0, Fwain
FisPrime:
; adding prolog for isPrime
lis $12
.word 12
sub $30, $30, $12
; prolog ends here for isPrime
lis $3
.word 1
sw $3, -4($29)
lis $3
.word 2
sw $3, -8($29)
; generating code for while 
sw1:
lw $3, -8($29)
sw $3, -4($30)
sub $30, $30, $4
; term -> term SLASH factor code starts
lw $3, 0($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 2
add $30, $30, $4
lw $5, -4($30)
div $5, $3
mflo $3
; term -> term SLASH factor code ends
add $30, $30, $4
lw $5, -4($30)
; LE code. inverting GT code
slt $3, $3, $5
sub $3, $11, $3
beq $3, $0, 1
beq $3, $11, 3
lis $6
.word ew1
jr $6
; generating for if
; term -> term PCT factor code starts
lw $3, 0($29)
sw $3, -4($30)
sub $30, $30, $4
lw $3, -8($29)
add $30, $30, $4
lw $5, -4($30)
div $5, $3
mfhi $3
; term -> term PCT factor code ends
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 0
add $30, $30, $4
lw $5, -4($30)
; EQ code. inverting NE
slt $6, $3, $5
slt $7, $5, $3
add $3, $6, $7
sub $3, $11, $3
bne $3, $0, sIF2
beq $0, $0, eIF2
sIF2:
lis $3
.word 0
sw $3, -4($29)
eIF2:
; expr -> expr PLUS term
lw $3, -8($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 1
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -8($29)
lis $6
.word sw1
jr $6
ew1:
lw $3, -4($29)
; epilog begins here for isPrime
add $30, $29, $4
jr $31
Fwain:
; adding prolog for wain
sub $29, $30, $4
lis $12
.word 24
sub $30, $30, $12
sw $1, 0($29)
sw $2, -4($29)
; prolog ends here for wain
; init
lis $10
.word init
sw $2, -4($30)
sub $30, $30, $4
add $2, $0, $0
sw $31, -4($30)
sub $30, $30, $4
jalr $10
add $30, $30, $4
lw $31, -4($30)
add $30, $30, $4
lw $2, -4($30)
add $3, $0, $11
sw $3, -8($29)
lis $3
.word 0
sw $3, -12($29)
lis $3
.word 2
sw $3, -16($29)
lis $3
.word 0
sw $3, -20($29)
lw $3, 0($29)
; gen code for new expr
sw $1, -4($30)
sub $30, $30, $4
sw $31, -4($30)
sub $30, $30, $4
add $1, $0, $3
lis $10
.word new
jalr $10
add $30, $30, $4
lw $31, -4($30)
bne $3, $0, newSuccess125
add $3, $11, $0
newSuccess125:
add $30, $30, $4
lw $1, -4($30)
sw $3, -8($29)
; generating code for while 
sw8:
lw $3, -16($29)
sw $3, -4($30)
sub $30, $30, $4
lw $3, 0($29)
add $30, $30, $4
lw $5, -4($30)
; LE code. inverting GT code
slt $3, $3, $5
sub $3, $11, $3
beq $3, $0, 1
beq $3, $11, 3
lis $6
.word ew8
jr $6
; generating for if
sw $29, -4($30)
sub $30, $30, $4
; starting to process
; storing 31 on stack
sw $31, -4($30)
sub $30, $30, $4
add $21, $30, $0
; processing 
lw $3, -16($29)
sw $3, -4($30)
sub $30, $30, $4
sub $29, $21, $4
add $30, $21, $0
lis $10
.word FisPrime
jalr $10
add $30, $30, $4
lw $31, -4($30)
add $30, $30, $4
lw $29, -4($30)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 1
add $30, $30, $4
lw $5, -4($30)
; EQ code. inverting NE
slt $6, $3, $5
slt $7, $5, $3
add $3, $6, $7
sub $3, $11, $3
bne $3, $0, sIF9
beq $0, $0, eIF9
sIF9:
lw $3, -16($29)
; de-ref a pointer and assign to it
sw $3, -4($30)
sub $30, $30, $4
; expr -> expr PLUS term
lw $3, -8($29)
sw $3, -4($30)
sub $30, $30, $4
lw $3, -12($29)
mult $3, $4
mflo $3
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
add $30, $30, $4
lw $5, -4($30)
sw $5, 0($3)
; expr -> expr PLUS term
lw $3, -12($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 1
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -12($29)
eIF9:
; expr -> expr PLUS term
lw $3, -16($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 1
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -16($29)
lis $6
.word sw8
jr $6
ew8:
; generating code for while 
sw21:
lw $3, -20($29)
sw $3, -4($30)
sub $30, $30, $4
lw $3, -12($29)
add $30, $30, $4
lw $5, -4($30)
; LT code
slt $3, $5, $3
beq $3, $0, 1
beq $3, $11, 3
lis $6
.word ew21
jr $6
; pointers! factor -> STAR factor
; expr -> expr PLUS term
lw $3, -8($29)
sw $3, -4($30)
sub $30, $30, $4
lw $3, -20($29)
mult $3, $4
mflo $3
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
lw $3, 0($3)
sw $1, -4($30)
sub $30, $30, $4
sw $31, -4($30)
sub $30, $30, $4
add $1, $3, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $31, -4($30)
add $30, $30, $4
lw $1, -4($30)
; expr -> expr PLUS term
lw $3, -20($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 1
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -20($29)
lis $6
.word sw21
jr $6
ew21:
lw $3, -12($29)
sw $1, -4($30)
sub $30, $30, $4
sw $31, -4($30)
sub $30, $30, $4
add $1, $3, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $31, -4($30)
add $30, $30, $4
lw $1, -4($30)
lw $3, -12($29)
; epilog begins here for wain
add $30, $29, $4
jr $31
