.import print
lis $4
.word 4
sub $29, $30, $4
lis $12
.word 20
sub $30, $30, $12
sw $1, 0($29)
sw $2, -4($29)
sw $31, -16($29)
; prolog ends here
lis $3
.word 5
sw $3, -8($29)
lis $3
.word 0
sw $3, -12($29)
lw $3, -8($29)
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
lw $3, -12($29)
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
lw $3, 0($29)
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
lw $3, -4($29)
; epilog begins here
lw $31, -16($29)
add $30, $29, $4
jr $31
