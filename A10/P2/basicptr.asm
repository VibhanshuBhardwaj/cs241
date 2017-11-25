; now generating code for expr from statement lvalue BECOMES expr SEMI
; lex for lvalue is p
; now generating code for expr from statement lvalue BECOMES expr SEMI
; lex for lvalue is p
; now generating code for expr from statement lvalue BECOMES expr SEMI
; lex for lvalue is t
.import print
lis $4
.word 4
lis $11
.word 1
sub $29, $30, $4
lis $12
.word 24
sub $30, $30, $12
sw $1, 0($29)
sw $2, -4($29)
sw $31, -20($29)
; prolog ends here
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
.word 5
sw $3, -8($29)
add $3, $0, $11
sw $3, -12($29)
add $3, $0, $11
sw $3, -16($29)
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
; expr -> expr PLUS term
; pointers! factor -> STAR factor
add $3, $0, $11
lw $3, 0($3)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 4
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
; de-ref a pointer and assign to it
sw $3, -4($30)
sub $30, $30, $4
lw $3, -12($29)
add $30, $30, $4
lw $5, -4($30)
sw $5, 0($3)
lw $3, -12($29)
; de-ref a pointer and assign to it
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word -16
add $3, $3, $29
add $30, $30, $4
lw $5, -4($30)
sw $5, 0($3)
; pointers! factor -> STAR factor
lw $3, -16($29)
lw $3, 0($3)
add $1, $3, $0
lis $10
.word print
jalr $10
lw $1, 0($29)
; expr -> expr PLUS term
; pointers! factor -> STAR factor
lw $3, -12($29)
lw $3, 0($3)
sw $3, -4($30)
sub $30, $30, $4
lis $3
.word 1
add $30, $30, $4
lw $5, -4($30)
add $3, $5, $3
; epilog begins here
lw $31, -20($29)
add $30, $29, $4
jr $31
