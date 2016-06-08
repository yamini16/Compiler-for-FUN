	.data 
NL:	.asciiz	"\n"
	.text 
main:
	addi 	$sp, $sp, -4	# $sp := $sp+-4
	sw 	$ra, 0($sp)	# [$sp+0] := $ra
	li 	$a0, 32000		# $a0 := 32000
	li 	$v0, 9		# $v0 := 9
	syscall 
	sw 	$v0, 0($gp)	# [$gp+0] := $v0
	jal 	_main		# call _main
	lw 	$ra, 0($sp)	# $ra := [$sp+0]
main.epilog:
	addi 	$sp, $sp, 4	# $sp := $sp+4
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
alloc:
	addi 	$sp, $sp, 0	# $sp := $sp+0
	lw 	$v0, 0($gp)	# $v0 := [$gp+0]
	add	$t0, $v0, $a0	# $t0 := $v0+$a0
	sw 	$t0, 0($gp)	# [$gp+0] := $t0
alloc.epilog:
	addi 	$sp, $sp, 0	# $sp := $sp+0
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_printint:
	addi 	$sp, $sp, 0	# $sp := $sp+0
	li 	$v0, 1		# $v0 := 1
	syscall 
	la 	$a0, NL	# $a0 := NL
	li 	$v0, 4		# $v0 := 4
	syscall 
_printint.epilog:
	addi 	$sp, $sp, 0	# $sp := $sp+0
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_fact:
	addi 	$sp, $sp, -40	# $sp := $sp+-40
	sw 	$s0, 4($sp)	# [$sp+4] := $s0
	sw 	$s1, 8($sp)	# [$sp+8] := $s1
	sw 	$s2, 12($sp)	# [$sp+12] := $s2
	sw 	$s3, 16($sp)	# [$sp+16] := $s3
	sw 	$s4, 20($sp)	# [$sp+20] := $s4
	sw 	$s5, 24($sp)	# [$sp+24] := $s5
	sw 	$s6, 28($sp)	# [$sp+28] := $s6
	sw 	$s7, 32($sp)	# [$sp+32] := $s7
	sw 	$ra, 0($sp)	# [$sp+0] := $ra
	sw 	$a0, 36($sp)	# [$sp+36] := $a0
	li 	$v0, 1		# $v0 := 1
	seq	$v0, $a0, $v0	# $v0 := $a0==$v0
	beqz	$v0, L1   	# if (signed) $v0 == 0 goto L1
	li 	$v0, 1		# $v0 := 1
				# $v0 := $v0
	j 	L2		# goto L2
L1:
	li 	$v0, 1		# $v0 := 1
	sub	$v0, $a0, $v0	# $v0 := $a0-$v0
	move 	$s0, $a0	# $s0 := $a0
	move 	$a0, $v0	# $a0 := $v0
	jal 	_fact		# call _fact
				# $v0 := $v0
	move 	$a0, $s0	# $a0 := $s0
	mulo	$v0, $a0, $v0	# $v0 := $a0*$v0
				# $v0 := $v0
L2:
				# $v0 := $v0
	lw 	$ra, 0($sp)	# $ra := [$sp+0]
	lw 	$s0, 4($sp)	# $s0 := [$sp+4]
	lw 	$s1, 8($sp)	# $s1 := [$sp+8]
	lw 	$s2, 12($sp)	# $s2 := [$sp+12]
	lw 	$s3, 16($sp)	# $s3 := [$sp+16]
	lw 	$s4, 20($sp)	# $s4 := [$sp+20]
	lw 	$s5, 24($sp)	# $s5 := [$sp+24]
	lw 	$s6, 28($sp)	# $s6 := [$sp+28]
	lw 	$s7, 32($sp)	# $s7 := [$sp+32]
	lw 	$a0, 36($sp)	# $a0 := [$sp+36]
_fact.epilog:
	addi 	$sp, $sp, 40	# $sp := $sp+40
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_main:
	addi 	$sp, $sp, -40	# $sp := $sp+-40
	sw 	$s0, 4($sp)	# [$sp+4] := $s0
	sw 	$s1, 8($sp)	# [$sp+8] := $s1
	sw 	$s2, 12($sp)	# [$sp+12] := $s2
	sw 	$s3, 16($sp)	# [$sp+16] := $s3
	sw 	$s4, 20($sp)	# [$sp+20] := $s4
	sw 	$s5, 24($sp)	# [$sp+24] := $s5
	sw 	$s6, 28($sp)	# [$sp+28] := $s6
	sw 	$s7, 32($sp)	# [$sp+32] := $s7
	sw 	$ra, 0($sp)	# [$sp+0] := $ra
	sw 	$a0, 36($sp)	# [$sp+36] := $a0
	li 	$v0, 4		# $v0 := 4
	move 	$s0, $a0	# $s0 := $a0
	move 	$a0, $v0	# $a0 := $v0
	jal 	_fact		# call _fact
	move 	$s1, $v0	# $s1 := $v0
	move 	$a0, $s0	# $a0 := $s0
	move 	$s0, $a0	# $s0 := $a0
	move 	$a0, $s1	# $a0 := $s1
	jal 	_printint		# call _printint
				# $v0 := $v0
	move 	$a0, $s0	# $a0 := $s0
	move 	$v0, $s1	# $v0 := $s1
	lw 	$ra, 0($sp)	# $ra := [$sp+0]
	lw 	$s0, 4($sp)	# $s0 := [$sp+4]
	lw 	$s1, 8($sp)	# $s1 := [$sp+8]
	lw 	$s2, 12($sp)	# $s2 := [$sp+12]
	lw 	$s3, 16($sp)	# $s3 := [$sp+16]
	lw 	$s4, 20($sp)	# $s4 := [$sp+20]
	lw 	$s5, 24($sp)	# $s5 := [$sp+24]
	lw 	$s6, 28($sp)	# $s6 := [$sp+28]
	lw 	$s7, 32($sp)	# $s7 := [$sp+32]
	lw 	$a0, 36($sp)	# $a0 := [$sp+36]
_main.epilog:
	addi 	$sp, $sp, 40	# $sp := $sp+40
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
