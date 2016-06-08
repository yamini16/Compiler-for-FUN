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
_change:
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
	lw 	$x13, 0($a0)	# $x13 := [$a0+0]
	li 	$x14, 1		# $x14 := 1
	add	$x15, $x13, $x14	# $x15 := $x13+$x14
	sw 	$x15, 0($a0)	# [$a0+0] := $x15
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
_change.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_main:
	move 	$x17, $s0	# $x17 := $s0
	move 	$x18, $s1	# $x18 := $s1
	move 	$x19, $s2	# $x19 := $s2
	move 	$x20, $s3	# $x20 := $s3
	move 	$x21, $s4	# $x21 := $s4
	move 	$x22, $s5	# $x22 := $s5
	move 	$x23, $s6	# $x23 := $s6
	move 	$x24, $s7	# $x24 := $s7
	move 	$x16, $ra	# $x16 := $ra
	move 	$x25, $a0	# $x25 := $a0
	li 	$a0, 4		# $a0 := 4
	jal 	alloc		# call alloc
	move 	$x28, $v0	# $x28 := $v0
	move 	$x28, $v0	# $x28 := $v0
	li 	$x29, 5		# $x29 := 5
	sw 	$x29, 0($x28)	# [$x28+0] := $x29
	move 	$x30, $a0	# $x30 := $a0
	move 	$a0, $x28	# $a0 := $x28
	jal 	_change		# call _change
	move 	$x31, $v0	# $x31 := $v0
	move 	$a0, $x30	# $a0 := $x30
	lw 	$x32, 0($x28)	# $x32 := [$x28+0]
	move 	$x33, $a0	# $x33 := $a0
	move 	$a0, $x32	# $a0 := $x32
	jal 	_printint		# call _printint
	move 	$x34, $v0	# $x34 := $v0
	move 	$a0, $x33	# $a0 := $x33
	lw 	$x35, 0($x28)	# $x35 := [$x28+0]
	move 	$v0, $x35	# $v0 := $x35
	move 	$ra, $x16	# $ra := $x16
	move 	$s0, $x17	# $s0 := $x17
	move 	$s1, $x18	# $s1 := $x18
	move 	$s2, $x19	# $s2 := $x19
	move 	$s3, $x20	# $s3 := $x20
	move 	$s4, $x21	# $s4 := $x21
	move 	$s5, $x22	# $s5 := $x22
	move 	$s6, $x23	# $s6 := $x23
	move 	$s7, $x24	# $s7 := $x24
	move 	$a0, $x25	# $a0 := $x25
_main.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
