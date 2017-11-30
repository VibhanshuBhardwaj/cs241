;available: 12
;available: 8
;available: 19
;available: 23
;available: 15
;available: 9
;available: 22
;available: 26
;available: 13
;available: 24
;available: 16
;available: 10
;available: 21
;available: 17
;available: 25
;available: 14
;available: 20
;available: 27
;available: 18
;available: 28
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
.word 8
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
; term -> term SLASH factor code starts
lw $12, 0($29)
lw $8, -4($29)
div $12, $8
mflo $12
add $3, $0, $12
add $30, $30, $4
lw $31, -4($30)
add $30, $29, $4
add $31, $28, $0
jr $31
