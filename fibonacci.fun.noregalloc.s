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
_fib:
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
	li 	$x13, 0		# $x13 := 0
	seq	$x14, $a0, $x13	# $x14 := $a0==$x13
	beqz	$x14, L1   	# if (signed) $x14 == 0 goto L1
	li 	$x16, 0		# $x16 := 0
	move 	$x15, $x16	# $x15 := $x16
	j 	L2		# goto L2
L1:
	li 	$x17, 1		# $x17 := 1
	seq	$x18, $a0, $x17	# $x18 := $a0==$x17
	beqz	$x18, L3   	# if (signed) $x18 == 0 goto L3
	li 	$x20, 1		# $x20 := 1
	move 	$x19, $x20	# $x19 := $x20
	j 	L4		# goto L4
L3:
	li 	$x21, 1		# $x21 := 1
	sub	$x22, $a0, $x21	# $x22 := $a0-$x21
	move 	$x23, $a0	# $x23 := $a0
	move 	$a0, $x22	# $a0 := $x22
	jal 	_fib		# call _fib
	move 	$x24, $v0	# $x24 := $v0
	move 	$a0, $x23	# $a0 := $x23
	li 	$x25, 2		# $x25 := 2
	sub	$x26, $a0, $x25	# $x26 := $a0-$x25
	move 	$x27, $a0	# $x27 := $a0
	move 	$a0, $x26	# $a0 := $x26
	jal 	_fib		# call _fib
	move 	$x28, $v0	# $x28 := $v0
	move 	$a0, $x27	# $a0 := $x27
	add	$x29, $x24, $x28	# $x29 := $x24+$x28
	move 	$x19, $x29	# $x19 := $x29
L4:
	move 	$x15, $x19	# $x15 := $x19
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
_fib.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_main:
	move 	$x31, $s0	# $x31 := $s0
	move 	$x32, $s1	# $x32 := $s1
	move 	$x33, $s2	# $x33 := $s2
	move 	$x34, $s3	# $x34 := $s3
	move 	$x35, $s4	# $x35 := $s4
	move 	$x36, $s5	# $x36 := $s5
	move 	$x37, $s6	# $x37 := $s6
	move 	$x38, $s7	# $x38 := $s7
	move 	$x30, $ra	# $x30 := $ra
	move 	$x39, $a0	# $x39 := $a0
	li 	$x42, 4		# $x42 := 4
	move 	$x43, $a0	# $x43 := $a0
	move 	$a0, $x42	# $a0 := $x42
	jal 	_fib		# call _fib
	move 	$x44, $v0	# $x44 := $v0
	move 	$a0, $x43	# $a0 := $x43
	move 	$x45, $a0	# $x45 := $a0
	move 	$a0, $x44	# $a0 := $x44
	jal 	_printint		# call _printint
	move 	$x46, $v0	# $x46 := $v0
	move 	$a0, $x45	# $a0 := $x45
	move 	$v0, $x44	# $v0 := $x44
	move 	$ra, $x30	# $ra := $x30
	move 	$s0, $x31	# $s0 := $x31
	move 	$s1, $x32	# $s1 := $x32
	move 	$s2, $x33	# $s2 := $x33
	move 	$s3, $x34	# $s3 := $x34
	move 	$s4, $x35	# $s4 := $x35
	move 	$s5, $x36	# $s5 := $x36
	move 	$s6, $x37	# $s6 := $x37
	move 	$s7, $x38	# $s7 := $x38
	move 	$a0, $x39	# $a0 := $x39
_main.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
