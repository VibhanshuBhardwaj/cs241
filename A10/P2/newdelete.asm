; now generating code for expr from statement lvalue BECOMES expr SEMI
; lex for lvalue is p
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
.word 16
sub $30, $30, $12
sw $1, 0($29)
sw $2, -4($29)
sw $31, -12($29)
; prolog ends here
; init
lis $10
.word init
sw $2, -4($30)
sub $30, $30, $4
add $2, $0, $0
jalr $10
add $30, $30, $4
lw $2, -4($30)
add $3, $0, $11
sw $3, -8($29)
lis $3
.word 5
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
lis $3
.word 8192
; gen code for new expr
sw $1, -4($30)
sub $30, $30, $4
add $1, $0, $3
lis $10
.word new
jalr $10
bne $3, $0, newSuccess353
add $3, $11, $0
newSuccess353:
add $30, $30, $4
lw $1, -4($30)
; de-ref a pointer and assign to it
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word -8
add $3, $3, $29
add $30, $30, $4
lw $5, -4($30)
sw $5, 0($3)
lis $3
.word 10
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
; generating statement -> DELETE
lw $3, -8($29)
beq $3, $11, skipDeleteBitch1
sw $1, -4($30)
sub $30, $30, $4
add $1, $0, $3
lis $10
.word delete
jalr $10
add $30, $30, $4
lw $1, -4($30)
skipDeleteBitch1:
lis $3
.word 69
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
; expr -> expr PLUS term
lw $3, 0($29)
sw $3, -4($30)
sub $30, $30, $4
lw $3, -4($29)
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
; epilog begins here
lw $31, -12($29)
add $30, $29, $4
jr $31
