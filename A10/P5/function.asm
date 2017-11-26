;have used print65
;have used print5
;adding prolog for print5
;adding prolog for print65
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
Fprint5:
; adding prolog for print5
sub $29, $30, $4
lis $12
.word 4
sub $30, $30, $12
sw $31, -0($29)
; prolog ends here for print5
lis $3
.word 5
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
lis $3
.word 5
; epilog begins here for print5
lw $31, -0($29)
add $30, $29, $4
jr $31
Fprint65:
; adding prolog for print65
sub $29, $30, $4
lis $12
.word 8
sub $30, $30, $12
sw $31, -4($29)
; prolog ends here for print65
lis $3
.word 0
sw $3, 0($29)
lis $3
.word 6
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
sw $29, -4($30)
sub $30, $30, $4
lis $10
.word Fprint5
jalr $10
add $30, $30, $4
lw $29, -4($30)
sw $3, 0($29)
lis $3
.word 6
; epilog begins here for print65
lw $31, -4($29)
add $30, $29, $4
jr $31
Fwain:
; adding prolog for wain
sub $29, $30, $4
lis $12
.word 16
sub $30, $30, $12
sw $1, 0($29)
sw $2, -4($29)
sw $31, -12($29)
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
lis $10
.word Fprint65
jalr $10
add $30, $30, $4
lw $29, -4($30)
sw $3, -8($29)
lis $3
.word 7
; epilog begins here for wain
lw $31, -12($29)
add $30, $29, $4
jr $31
