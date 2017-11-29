; ordered by usage 
available: 12
available: 8
available: 19
available: 23
available: 15
available: 9
available: 22
available: 26
available: 13
available: 24
available: 16
available: 10
available: 21
available: 17
available: 25
available: 14
available: 20
available: 27
available: 18
available: 28
; false  true
.import print
.import init
.import delete
.import new
lis $4
.word 4
lis $11
.word 1
beq $0, $0, Fwain
Fwain:
; adding prolog for wain
sub $29, $30, $4
add $28, $31, $0
lis $12
.word 20
sub $30, $30, $12
sw $1, 0($29)
sw $2, -4($29)
sw $31, -4($30)
sub $30, $30, $4
; prolog ends here for wain
; init
lis $10
.word init
sw $2, -4($30)
sub $30, $30, $4
add $2, $0, $0
jalr $10
add $30, $30, $4
lw $2, -4($30)
lis $3
.word 10
sw $3, -8($29)
lis $3
.word 6
sw $3, -12($29)
lis $3
.word 10
sw $3, -16($29)
; generating code for while 
sw1:
lw $3, -16($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 10
add $30, $30, $4
lw $5, -4($30)
; EQ code. inverting NE
slt $6, $3, $5
slt $7, $5, $3
add $3, $6, $7
sub $3, $11, $3
beq $3, $0, 1
beq $3, $11, 3
lis $6
.word ew1
jr $6
lw $3, -16($29)
sw $1, -4($30)
sub $30, $30, $4
add $1, $3, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
; expr -> expr PLUS term
lw $3, -16($29)
sw $3, -4($30)
sub $30, $30, $4
add $3, $0, $11
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -16($29)
lis $6
.word sw1
jr $6
ew1:
lis $3
.word 600
add $30, $30, $4
lw $31, -4($30)
add $30, $29, $4
add $31, $28, $0
jr $31
