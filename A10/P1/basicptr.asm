; now generating code for expr from statement lvalue BECOMES expr SEMI
; lex for lvalue is p
; now generating code for expr from statement lvalue BECOMES expr SEMI
; lex for lvalue is p
.import print
lis $4
.word 4
lis $11
.word 1
sub $29, $30, $4
lis $12
.word 20
sub $30, $30, $12
sw $1, 0($29)
sw $2, -4($29)
sw $31, -16($29)
; prolog ends here
lis $3
.word 5
sw $3, -8($29)
add $3, $0, $11
sw $3, -12($29)
lis $3
.word -8
add $3, $3, $29
; de-ref a pointer and assign to it
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word -12
add $3, $3, $29
add $30, $30, $4
lw $5, -4($30)
sw $5, 0($3)
lis $3
.word 666
; de-ref a pointer and assign to it
sw $3, -4($30)
sub $30, $30, $4
lw $3, -12($29)
add $30, $30, $4
lw $5, -4($30)
sw $5, 0($3)
lw $3, -8($29)
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
; expr -> expr PLUS term
; expr -> expr MINUS term
; pointers! factor -> STAR factor
lw $3, -12($29)
lw $3, 0($3)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 666
add $30, $30, $4
lw $5, -4($30)
sub $3, $5, $3
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 69
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
; epilog begins here
lw $31, -16($29)
add $30, $29, $4
jr $31
