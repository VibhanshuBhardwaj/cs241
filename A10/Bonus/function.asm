; var  + type local int
; var  + type local int
; const wain local 5
; const bar local 8
; const foo local 99
; availabel for exp 19
; availabel for exp 23
; availabel for exp 15
; availabel for exp 9
; availabel for exp 22
; availabel for exp 26
; availabel for exp 13
; availabel for exp 24
; availabel for exp 16
; availabel for exp 21
; availabel for exp 17
; availabel for exp 25
; availabel for exp 14
; availabel for exp 20
; availabel for exp 27
; availabel for exp 18
;using a
; c is 19
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
.word 99
sw $3, 0($29)
; loading registers for foo
lis $19
.word 161
add $8, $0, $19
add $30, $30, $4
lw $31, -4($30)
add $30, $29, $4
jr $31
Fbar:
; adding prolog for bar
lis $12
.word 4
sub $30, $30, $12
sw $31, -4($30)
sub $30, $30, $4
; prolog ends here for bar
lis $3
.word 8
sw $3, 0($29)
; loading registers for bar
; expr -> expr MINUS term
lis $19
.word 8
sw $29, -4($30)
sub $30, $30, $4
sw $31, -4($30)
sub $30, $30, $4
; saving registers
sw $19, -4($30)
sub $30, $30, $4
lis $10
.word Ffoo
sub $29, $30, $4
jalr $10
; restoring registers
add $30, $30, $4
lw $19, -4($30)
add $30, $30, $4
lw $31, -4($30)
add $30, $30, $4
lw $29, -4($30)
; term -> factor r 8
sub $19, $19, $8
add $8, $0, $19
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
jalr $10
; loading registers for waian
; expr -> expr PLUS term
; expr -> expr PLUS term
;in expr -> term 
sw $29, -4($30)
sub $30, $30, $4
sw $31, -4($30)
sub $30, $30, $4
; saving registers
lis $10
.word Ffoo
sub $29, $30, $4
jalr $10
; restoring registers
add $30, $30, $4
lw $31, -4($30)
add $30, $30, $4
lw $29, -4($30)
; term -> factor r 8
; expr -> term in r 8
sw $29, -4($30)
sub $30, $30, $4
sw $31, -4($30)
sub $30, $30, $4
; saving registers
lis $10
.word Fbar
sub $29, $30, $4
jalr $10
; restoring registers
add $30, $30, $4
lw $31, -4($30)
add $30, $30, $4
lw $29, -4($30)
; term -> factor r 8
add $8, $8, $8
lis $19
.word 5
add $8, $8, $19
add $3, $0, $8
add $30, $30, $4
lw $31, -4($30)
add $30, $29, $4
add $31, $28, $0
jr $31
