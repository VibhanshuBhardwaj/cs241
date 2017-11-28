; populating lvalue's children r
;function wain
;have used r
;have used p
;adding prolog for wain
; lvalue rulelvalue ID
; generating code for r
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
.word 0
sw $3, -8($29)
lis $3
.word 0
sw $3, -16($29)
; expr -> expr PLUS term
; expr -> expr PLUS term
lw $3, -8($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 42
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 79
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -16($29)
lw $3, -16($29)
; epilog begins here for wain
add $30, $30, $4
lw $31, -4($30)
add $30, $29, $4
add $31, $28, $0
jr $31
