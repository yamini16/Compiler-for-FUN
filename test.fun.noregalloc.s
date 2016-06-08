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
_main:
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
	li 	$x13, 15		# $x13 := 15
	li 	$x14, 2		# $x14 := 2
	li 	$a0, 8		# $a0 := 8
	jal 	alloc		# call alloc
	move 	$x15, $v0	# $x15 := $v0
	sw 	$x13, 0($x15)	# [$x15+0] := $x13
	sw 	$x14, 4($x15)	# [$x15+4] := $x14
	li 	$x16, 5		# $x16 := 5
	lw 	$x17, 0($x15)	# $x17 := [$x15+0]
	lw 	$x18, 4($x15)	# $x18 := [$x15+4]
	add	$x19, $x17, $x18	# $x19 := $x17+$x18
	add	$x20, $x16, $x19	# $x20 := $x16+$x19
	move 	$x21, $a0	# $x21 := $a0
	move 	$a0, $x20	# $a0 := $x20
	jal 	_printint		# call _printint
	move 	$x22, $v0	# $x22 := $v0
	move 	$a0, $x21	# $a0 := $x21
	li 	$x23, 1		# $x23 := 1
	move 	$v0, $x23	# $v0 := $x23
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
_main.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
