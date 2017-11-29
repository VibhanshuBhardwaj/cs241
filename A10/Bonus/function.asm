; ordered by usage 
;function var wain q
; const value 6
;function var wain p
; const value 5
; false  true
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
add $7, $31, $0
lis $6
.word 20
sub $30, $30, $6
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
.word 10
sw $3, -16($29)
; generating code for while 
sw1:
lw $3, -16($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 10
add $30, $30, $4
lw $5, -4($30)
; EQ code. inverting NE
slt $6, $3, $5
slt $7, $5, $3
add $3, $6, $7
sub $3, $11, $3
beq $3, $0, 1
beq $3, $11, 3
lis $6
.word ew1
jr $6
lw $3, -16($29)
sw $1, -4($30)
sub $30, $30, $4
add $1, $3, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
; expr -> expr PLUS term
lw $3, -16($29)
sw $3, -4($30)
sub $30, $30, $4
add $3, $0, $11
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -16($29)
lis $6
.word sw1
jr $6
ew1:
lw $3, -16($29)
add $30, $30, $4
lw $31, -4($30)
add $30, $29, $4
add $31, $7, $0
jr $31
