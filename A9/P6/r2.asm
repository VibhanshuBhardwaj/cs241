.import print
lis $4
.word 4
lis $11
.word 1
sub $29, $30, $4
lis $12
.word 20
sub $30, $30, $12
sw $1, 0($29)
sw $2, -4($29)
sw $31, -16($29)
; prolog ends here
lis $3
.word 1
sw $3, -8($29)
lis $3
.word 1
sw $3, -12($29)
sw1:
lw $3, -8($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 5
add $30, $30, $4
lw $5, -4($30)
slt $3, $5, $3
beq $3, $0, 1
beq $3, $11, 3
lis $6
.word ew1
jr $6
lis $3
.word 1
sw $3, -12($29)
sw2:
lw $3, -12($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 3
add $30, $30, $4
lw $5, -4($30)
slt $3, $5, $3
beq $3, $0, 1
beq $3, $11, 3
lis $6
.word ew2
jr $6
lw $3, -12($29)
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
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
lis $6
.word sw2
jr $6
ew2:
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
; expr -> expr PLUS term
; expr -> expr PLUS term
; expr -> expr PLUS term
lw $3, 0($29)
sw $3, -4($30)
sub $30, $30, $4
lw $3, -4($29)
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -4($30)
sub $30, $30, $4
lw $3, -8($29)
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -4($30)
sub $30, $30, $4
lw $3, -12($29)
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
; epilog begins here
lw $31, -16($29)
add $30, $29, $4
jr $31
