;have used foo
;have used bar
;adding prolog for bar
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
Fbar:
; adding prolog for bar
lis $12
.word 8
sub $30, $30, $12
sw $31, -4($29)
; prolog ends here for bar
add $3, $0, $11
sw $3, 0($29)
lis $3
.word 5
; gen code for new expr
sw $1, -4($30)
sub $30, $30, $4
add $1, $0, $3
lis $10
.word new
jalr $10
bne $3, $0, newSuccess929
add $3, $11, $0
newSuccess929:
add $30, $30, $4
lw $1, -4($30)
sw $3, 0($29)
lis $3
.word 69
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
lis $3
.word 2
; de-ref a pointer and assign to it
sw $3, -4($30)
sub $30, $30, $4
; expr -> expr PLUS term
lw $3, 0($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 3
mult $3, $4
mflo $3
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
add $30, $30, $4
lw $5, -4($30)
sw $5, 0($3)
; generating statement -> DELETE
lw $3, 0($29)
beq $3, $11, skipDeleteBitch1
sw $1, -4($30)
sub $30, $30, $4
add $1, $0, $3
lis $10
.word delete
jalr $10
add $30, $30, $4
lw $1, -4($30)
skipDeleteBitch1:
lis $3
.word 10
; epilog begins here for bar
lw $31, -4($29)
add $30, $29, $4
jr $31
Ffoo:
; adding prolog for foo
lis $12
.word 24
sub $30, $30, $12
sw $31, -20($29)
; prolog ends here for foo
lis $3
.word 5
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
sw $29, -4($30)
sub $30, $30, $4
sw $3, -4($30)
sub $30, $30, $4
sw $5, -4($30)
sub $30, $30, $4
lis $10
.word Fbar
jalr $10
add $30, $30, $4
lw $5, -4($30)
add $30, $30, $4
lw $3, -4($30)
add $30, $30, $4
lw $29, -4($30)
; de-ref a pointer and assign to it
sw $3, -4($30)
sub $30, $30, $4
; expr -> expr PLUS term
lw $3, -4($29)
sw $3, -4($30)
sub $30, $30, $4
sw $29, -4($30)
sub $30, $30, $4
sw $3, -4($30)
sub $30, $30, $4
sw $5, -4($30)
sub $30, $30, $4
lis $10
.word Fbar
jalr $10
add $30, $30, $4
lw $5, -4($30)
add $30, $30, $4
lw $3, -4($30)
add $30, $30, $4
lw $29, -4($30)
mult $3, $4
mflo $3
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
add $30, $30, $4
lw $5, -4($30)
sw $5, 0($3)
lis $3
.word 6
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
; expr -> expr PLUS term
; expr -> expr PLUS term
; expr -> expr PLUS term
; expr -> expr PLUS term
lw $3, 0($29)
sw $3, -4($30)
sub $30, $30, $4
; pointers! factor -> STAR factor
; expr -> expr PLUS term
lw $3, -4($29)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 10
mult $3, $4
mflo $3
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
lw $3, 0($3)
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -4($30)
sub $30, $30, $4
lw $3, -8($29)
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -4($30)
sub $30, $30, $4
lw $3, -12($29)
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
sw $3, -4($30)
sub $30, $30, $4
; pointers! factor -> STAR factor
lw $3, -16($29)
lw $3, 0($3)
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
; epilog begins here for foo
lw $31, -20($29)
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
add $3, $0, $11
sw $3, -8($29)
lis $3
.word 40
; gen code for new expr
sw $1, -4($30)
sub $30, $30, $4
add $1, $0, $3
lis $10
.word new
jalr $10
bne $3, $0, newSuccess426
add $3, $11, $0
newSuccess426:
add $30, $30, $4
lw $1, -4($30)
sw $3, -8($29)
sw $29, -4($30)
sub $30, $30, $4
; starting to process
add $21, $30, $0
; processing 
lw $3, 0($29)
sw $3, -4($30)
sub $30, $30, $4
; processing 
lw $3, -8($29)
sw $3, -4($30)
sub $30, $30, $4
; processing 
lw $3, 0($29)
sw $3, -4($30)
sub $30, $30, $4
; processing 
lw $3, -4($29)
sw $3, -4($30)
sub $30, $30, $4
; processing 
lis $3
.word 0
add $3, $3, $29
sw $3, -4($30)
sub $30, $30, $4
sub $29, $21, $4
lis $10
.word Ffoo
jalr $10
add $30, $30, $4
lw $29, -4($30)
; epilog begins here for wain
lw $31, -12($29)
add $30, $29, $4
jr $31
