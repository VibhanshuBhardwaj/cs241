;adding prolog for foo
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
Ffoo:
; adding prolog for foo
lis $12
.word 4
sub $30, $30, $12
sw $31, -4($30)
sub $30, $30, $4
; prolog ends here for foo
lis $3
.word 5
sw $3, 0($29)
lw $3, 0($29)
; epilog begins here for foo
add $30, $30, $4
lw $31, -4($30)
add $30, $29, $4
jr $31
Fwain:
; adding prolog for wain
sub $29, $30, $4
add $28, $31, $0
lis $12
.word 12
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
.word 0
sw $3, -8($29)
sw $29, -4($30)
sub $30, $30, $4
sw $31, -4($30)
sub $30, $30, $4
lis $10
.word Ffoo
sub $29, $30, $4
jalr $10
add $30, $30, $4
lw $31, -4($30)
add $30, $30, $4
lw $29, -4($30)
sw $3, -8($29)
lis $3
.word 69
sw $1, -4($30)
sub $30, $30, $4
add $1, $3, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
lw $3, -8($29)
; epilog begins here for wain
add $30, $30, $4
lw $31, -4($30)
add $30, $29, $4
add $31, $28, $0
jr $31
