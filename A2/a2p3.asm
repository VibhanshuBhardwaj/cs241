lis $4
.word 1
slt $5, $2, $4 #if $2 < $4, set $5 = 1. ie when array is empty
beq $5, $4, end
lis $4
.word 4
mult $2 $4
mflo $5
sub $5, $5, $4
add $5, $5, $1
lw $6, 0($5)
add $3, $0, $6
beq $0, $0, return

end: lis $4
.word -1
add $3, $0, $4
return: jr $31