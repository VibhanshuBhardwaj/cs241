.import print
.import init
.import delete
.import new
lis $4
.word 4
lis $11
.word 1
sub $29, $30, $4
lis $12
.word 24
sub $30, $30, $12
sw $1, 0($29)
sw $2, -4($29)
sw $31, -20($29)
; prolog ends here
; init
lis $10
.word init
jalr $10
add $3, $0, $11
sw $3, -8($29)
add $3, $0, $11
sw $3, -12($29)
lis $3
.word 0
sw $3, -16($29)
; expr -> expr PLUS term
lw $3, 0($29)
sw $3, -4($30)
sub $30, $30, $4
lw $3, -4($29)
mult $3, $4
mflo $3
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 1
mult $3, $4
mflo $3
add $30, $30, $4
lw $5, -4($30)
sub $3, $5, $3
sw $3, -12($29)
lw $3, 0($29)
sw $3, -8($29)
; generating code for while 
sw1:
lw $3, -8($29)
sw $3, -4($30)
sub $30, $30, $4
lw $3, -12($29)
add $30, $30, $4
lw $5, -4($30)
; LT code
sltu $3, $5, $3
beq $3, $0, 1
beq $3, $11, 3
lis $6
.word ew1
jr $6
; expr -> expr PLUS term
lw $3, -16($29)
sw $3, -4($30)
sub $30, $30, $4
; pointers! factor -> STAR factor
lw $3, 0($29)
lw $3, 0($3)
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -16($29)
; expr -> expr PLUS term
lw $3, -8($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 1
mult $3, $4
mflo $3
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -8($29)
lis $6
.word sw1
jr $6
ew1:
; pointers! factor -> STAR factor
lw $3, -8($29)
lw $3, 0($3)
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
lis $3
.word 0
; epilog begins here
lw $31, -20($29)
add $30, $29, $4
jr $31
