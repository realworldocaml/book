	.data
	.globl	_camlCompare_mono__data_begin
_camlCompare_mono__data_begin:
	.text
	.globl	_camlCompare_mono__code_begin
_camlCompare_mono__code_begin:
	nop
	.data
	.quad	1024
	.globl	_camlCompare_mono
_camlCompare_mono:
	.space	8
	.data
	.quad	3319
_camlCompare_mono__1:
	.quad	_caml_curry2
	.quad	5
	.quad	_camlCompare_mono__cmp_1008
	.text
	.align	4
	.globl	_camlCompare_mono__cmp_1008
_camlCompare_mono__cmp_1008:
	.cfi_startproc
.L101:
	cmpq	%rbx, %rax
	jle	.L100
	ret
	.align	2
.L100:
	movq	%rbx, %rax
	ret
	.cfi_endproc
	.text
	.align	4
	.globl	_camlCompare_mono__entry
_camlCompare_mono__entry:
	.cfi_startproc
.L102:
	leaq	_camlCompare_mono__1(%rip), %rax
	movq	%rax, _camlCompare_mono(%rip)
	movq	$1, %rax
	ret
	.cfi_endproc
	.data
	.text
	nop
	.globl	_camlCompare_mono__code_end
_camlCompare_mono__code_end:
	.data
	.globl	_camlCompare_mono__data_end
_camlCompare_mono__data_end:
	.long	0
	.globl	_camlCompare_mono__frametable
_camlCompare_mono__frametable:
	.quad	0
