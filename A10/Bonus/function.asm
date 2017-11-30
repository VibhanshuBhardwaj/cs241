;MappingToRegisters 0
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

; variable  + wain c is NOT mapped to a register
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
;if isConstantExpr results: exp1 false
;if isConstantExpr results: exp2 false
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
jalr $10
add $3, $0, $11
sw $3, -8($29)
; expr -> expr PLUS term
lw $12, 0($29)
add $8, $0, $11
mult $8, $4
mflo $8
add $12, $12, $8
sw $12, -8($29)
; generating for if
lw $12, 0($29)
lw $8, -8($29)
add $5, $12, $0
add $3, $8, $0
; LT code
sltu $3, $5, $3
bne $3, $0, sIF1
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF1
sIF1:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF1:
; generating for if
lw $12, 0($29)
lw $8, -8($29)
add $5, $12, $0
add $3, $8, $0
; GT code
sltu $3, $3, $5
bne $3, $0, sIF5
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF5
sIF5:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF5:
; generating for if
lw $12, -8($29)
lw $8, 0($29)
add $5, $12, $0
add $3, $8, $0
; LT code
sltu $3, $5, $3
bne $3, $0, sIF13
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF13
sIF13:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF13:
; generating for if
lw $12, 0($29)
lw $8, -8($29)
add $5, $12, $0
add $3, $8, $0
; LE code. inverting GT code
sltu $3, $3, $5
sub $3, $11, $3
bne $3, $0, sIF29
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF29
sIF29:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF29:
; generating for if
lw $12, 0($29)
lw $8, -8($29)
add $5, $12, $0
add $3, $8, $0
; GE code. inverting LT code
sltu $3, $5, $3
sub $3, $11, $3
bne $3, $0, sIF61
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF61
sIF61:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF61:
; generating for if
lw $12, -8($29)
lw $8, 0($29)
add $5, $12, $0
add $3, $8, $0
; LE code. inverting GT code
sltu $3, $3, $5
sub $3, $11, $3
bne $3, $0, sIF125
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF125
sIF125:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF125:
; generating for if
lw $12, -8($29)
lw $8, 0($29)
add $5, $12, $0
add $3, $8, $0
; GE code. inverting LT code
sltu $3, $5, $3
sub $3, $11, $3
bne $3, $0, sIF253
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF253
sIF253:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF253:
; generating for if
lw $12, 0($29)
lw $8, 0($29)
add $5, $12, $0
add $3, $8, $0
; EQ code. inverting NE
sltu $6, $3, $5
sltu $7, $5, $3
add $3, $6, $7
sub $3, $11, $3
bne $3, $0, sIF509
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF509
sIF509:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF509:
; generating for if
lw $12, -8($29)
lw $8, -8($29)
add $5, $12, $0
add $3, $8, $0
; NE code
sltu $6, $3, $5
sltu $7, $5, $3
add $3, $6, $7
bne $3, $0, sIF1021
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF1021
sIF1021:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF1021:
; generating for if
lw $12, 0($29)
lw $8, -8($29)
add $5, $12, $0
add $3, $8, $0
; EQ code. inverting NE
sltu $6, $3, $5
sltu $7, $5, $3
add $3, $6, $7
sub $3, $11, $3
bne $3, $0, sIF2045
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF2045
sIF2045:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF2045:
; generating for if
lw $12, 0($29)
lw $8, -8($29)
add $5, $12, $0
add $3, $8, $0
; NE code
sltu $6, $3, $5
sltu $7, $5, $3
add $3, $6, $7
bne $3, $0, sIF4093
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF4093
sIF4093:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF4093:
; generating for if
lw $12, 0($29)
lw $8, 0($29)
add $5, $12, $0
add $3, $8, $0
; LE code. inverting GT code
sltu $3, $3, $5
sub $3, $11, $3
bne $3, $0, sIF8189
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF8189
sIF8189:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF8189:
; generating for if
lw $12, -8($29)
lw $8, -8($29)
add $5, $12, $0
add $3, $8, $0
; GE code. inverting LT code
sltu $3, $5, $3
sub $3, $11, $3
bne $3, $0, sIF16381
add $12, $0, $0
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
beq $0, $0, eIF16381
sIF16381:
add $12, $0, $11
sw $1, -4($30)
sub $30, $30, $4
add $1, $12, $0
lis $10
.word print
jalr $10
add $30, $30, $4
lw $1, -4($30)
eIF16381:
add $12, $0, $0
add $3, $0, $12
add $30, $30, $4
lw $31, -4($30)
add $30, $29, $4
add $31, $28, $0
jr $31
