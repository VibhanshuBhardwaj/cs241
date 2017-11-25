slt $4, $1, $2
bne $4, $0, end
add $3, $0, $1
beq $0, $0, 1
end: add $3, $0, $2
jr $31
