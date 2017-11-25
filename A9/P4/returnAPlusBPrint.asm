.import print
lis $4
.word 4
sub $29, $30, $4
lis $12
.word 12
sub $30, $30, $12
sw $1, 0($29)
sw $2, -4($29)
sw $31, -8($29)
; prolog ends here
; expr -> expr PLUS term
lw $3, 0($29)
sw $3, -4($30)
sub $30, $30, $4
lw $3, -4($29)
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
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
lw $31, -8($29)
add $30, $29, $4
jr $31
