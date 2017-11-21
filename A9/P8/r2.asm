.import print
lis $4
.word 4
lis $11
.word 1
sub $29, $30, $4
lis $12
.word 16
sub $30, $30, $12
sw $1, 0($29)
sw $2, -4($29)
sw $31, -12($29)
; prolog ends here
lis $3
.word 0
sw $3, -8($29)
; generating code for while 
sw1:
lw $3, -8($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 10
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
lw $3, -8($29)
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
; generating for if
; expr -> expr PLUS term
lis $3
.word 2
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 1
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 4
add $30, $30, $4
lw $5, -4($30)
; LT code
slt $3, $5, $3
bne $3, $0, sIF2
beq $0, $0, eIF2
sIF2:
lis $3
.word 99
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
; generating for if
lw $3, -8($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 5
add $30, $30, $4
lw $5, -4($30)
; LT code
slt $3, $5, $3
bne $3, $0, sIF4
lis $3
.word 666
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
beq $0, $0, eIF4
sIF4:
lw $3, -8($29)
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
eIF4:
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
lw $3, -8($29)
; epilog begins here
lw $31, -12($29)
add $30, $29, $4
jr $31
