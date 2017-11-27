;have used p
;adding prolog for p
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
Fp:
; adding prolog for p
lis $12
.word 4
sub $30, $30, $12
; prolog ends here for p
lw $3, 0($29)
; epilog begins here for p
add $30, $29, $4
jr $31
Fwain:
; adding prolog for wain
sub $29, $30, $4
lis $12
.word 8
sub $30, $30, $12
sw $1, 0($29)
sw $2, -4($29)
; prolog ends here for wain
; init
lis $10
.word init
sw $2, -4($30)
sub $30, $30, $4
add $2, $0, $0
sw $31, -4($30)
sub $30, $30, $4
jalr $10
add $30, $30, $4
lw $31, -4($30)
add $30, $30, $4
lw $2, -4($30)
sw $29, -4($30)
sub $30, $30, $4
; starting to process
add $21, $30, $0
; processing 
lis $3
.word 10
sw $3, -4($30)
sub $30, $30, $4
sub $29, $21, $4
sw $31, -4($30)
sub $30, $30, $4
lis $10
.word Fp
jalr $10
lis $19
.word 8
add $30, $30, $4
lw $31, -4($30)
lis $19
.word 9
add $30, $30, $4
lw $29, -4($30)
lis $19
.word 10
; epilog begins here for wain
add $30, $29, $4
jr $31
