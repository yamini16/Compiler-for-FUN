	.data 
NL:	.asciiz	"\n"
	.text 
main:
	move 	$x0, $ra	# $x0 := $ra
	li 	$a0, 32000		# $a0 := 32000
	li 	$v0, 9		# $v0 := 9
	syscall 
	sw 	$v0, 0($gp)	# [$gp+0] := $v0
	jal 	_main		# call _main
	move 	$ra, $x0	# $ra := $x0
main.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
alloc:
	lw 	$v0, 0($gp)	# $v0 := [$gp+0]
	add	$t0, $v0, $a0	# $t0 := $v0+$a0
	sw 	$t0, 0($gp)	# [$gp+0] := $t0
alloc.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_printint:
	li 	$v0, 1		# $v0 := 1
	syscall 
	la 	$a0, NL	# $a0 := NL
	li 	$v0, 4		# $v0 := 4
	syscall 
_printint.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_fact:
	move 	$x2, $s0	# $x2 := $s0
	move 	$x3, $s1	# $x3 := $s1
	move 	$x4, $s2	# $x4 := $s2
	move 	$x5, $s3	# $x5 := $s3
	move 	$x6, $s4	# $x6 := $s4
	move 	$x7, $s5	# $x7 := $s5
	move 	$x8, $s6	# $x8 := $s6
	move 	$x9, $s7	# $x9 := $s7
	move 	$x1, $ra	# $x1 := $ra
	move 	$x10, $a0	# $x10 := $a0
	li 	$x13, 1		# $x13 := 1
	seq	$x14, $a0, $x13	# $x14 := $a0==$x13
	beqz	$x14, L1   	# if (signed) $x14 == 0 goto L1
	li 	$x16, 1		# $x16 := 1
	move 	$x15, $x16	# $x15 := $x16
	j 	L2		# goto L2
L1:
	li 	$x17, 1		# $x17 := 1
	sub	$x18, $a0, $x17	# $x18 := $a0-$x17
	move 	$x19, $a0	# $x19 := $a0
	move 	$a0, $x18	# $a0 := $x18
	jal 	_fact		# call _fact
	move 	$x20, $v0	# $x20 := $v0
	move 	$a0, $x19	# $a0 := $x19
	mulo	$x21, $a0, $x20	# $x21 := $a0*$x20
	move 	$x15, $x21	# $x15 := $x21
L2:
	move 	$v0, $x15	# $v0 := $x15
	move 	$ra, $x1	# $ra := $x1
	move 	$s0, $x2	# $s0 := $x2
	move 	$s1, $x3	# $s1 := $x3
	move 	$s2, $x4	# $s2 := $x4
	move 	$s3, $x5	# $s3 := $x5
	move 	$s4, $x6	# $s4 := $x6
	move 	$s5, $x7	# $s5 := $x7
	move 	$s6, $x8	# $s6 := $x8
	move 	$s7, $x9	# $s7 := $x9
	move 	$a0, $x10	# $a0 := $x10
_fact.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_main:
	move 	$x23, $s0	# $x23 := $s0
	move 	$x24, $s1	# $x24 := $s1
	move 	$x25, $s2	# $x25 := $s2
	move 	$x26, $s3	# $x26 := $s3
	move 	$x27, $s4	# $x27 := $s4
	move 	$x28, $s5	# $x28 := $s5
	move 	$x29, $s6	# $x29 := $s6
	move 	$x30, $s7	# $x30 := $s7
	move 	$x22, $ra	# $x22 := $ra
	move 	$x31, $a0	# $x31 := $a0
	li 	$x34, 4		# $x34 := 4
	move 	$x35, $a0	# $x35 := $a0
	move 	$a0, $x34	# $a0 := $x34
	jal 	_fact		# call _fact
	move 	$x36, $v0	# $x36 := $v0
	move 	$a0, $x35	# $a0 := $x35
	move 	$x37, $a0	# $x37 := $a0
	move 	$a0, $x36	# $a0 := $x36
	jal 	_printint		# call _printint
	move 	$x38, $v0	# $x38 := $v0
	move 	$a0, $x37	# $a0 := $x37
	move 	$v0, $x36	# $v0 := $x36
	move 	$ra, $x22	# $ra := $x22
	move 	$s0, $x23	# $s0 := $x23
	move 	$s1, $x24	# $s1 := $x24
	move 	$s2, $x25	# $s2 := $x25
	move 	$s3, $x26	# $s3 := $x26
	move 	$s4, $x27	# $s4 := $x27
	move 	$s5, $x28	# $s5 := $x28
	move 	$s6, $x29	# $s6 := $x29
	move 	$s7, $x30	# $s7 := $x30
	move 	$a0, $x31	# $a0 := $x31
_main.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
