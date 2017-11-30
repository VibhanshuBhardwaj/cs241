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
; const wain e 0

; variable  + wain c is NOT mapped to a register

;rule : lvalue STAR factor
;r: 12
; variable  + wain d is NOT mapped to a register
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
jalr $10
add $3, $0, $11
sw $3, -8($29)
add $3, $0, $11
sw $3, -12($29)
; expr -> expr PLUS term
;in expr -> term 
lw $12, 0($29)
; term -> factor r 12
; expr -> term in r 12
lw $8, -4($29)
; term -> factor r 8
mult $8, $4
mflo $8
add $12, $12, $8
sw $12, -8($29)
;in expr -> term 
;in expr -> term 
lw $12, -8($29)
; term -> factor r 12
; expr -> term in r 12
;s is 12
; ptr - int!
add $8, $0, $11
; t is 8
mult $8, $4
mflo $8
sub $12, $12, $8
; term -> factor r 12
; expr -> term in r 12
sw $12, -12($29)
; ptr subtraction!
;in expr -> term 
lw $12, -12($29)
; term -> factor r 12
; expr -> term in r 12
; s is 12
lw $8, 0($29)
; term -> factor r 8
; t is 8
sub $12, $12, $8
div $12, $4
mflo $12
add $3, $0, $12
add $30, $30, $4
lw $31, -4($30)
add $30, $29, $4
add $31, $28, $0
jr $31
