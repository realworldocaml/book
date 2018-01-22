	.file ""
	.data
	.globl	_camlCompare_mono__data_begin
_camlCompare_mono__data_begin:
	.text
	.globl	_camlCompare_mono__code_begin
_camlCompare_mono__code_begin:
	nop
	.data
	.quad	1792
	.globl	_camlCompare_mono
_camlCompare_mono:
	.quad	1
	.data
	.globl	_camlCompare_mono__gc_roots
_camlCompare_mono__gc_roots:
	.quad	_camlCompare_mono
	.quad	0
	.text
	.align	4
	.globl	_camlCompare_mono__cmp_1002
_camlCompare_mono__cmp_1002:
	.cfi_startproc
L101:
	cmpq	%rbx, %rax
	jle	L100
	ret
	.align	2
L100:
	movq	%rbx, %rax
	ret
	.cfi_endproc
	.data
	.quad	4087
_camlCompare_mono__1:
	.quad	_caml_curry2
	.quad	5
	.quad	_camlCompare_mono__cmp_1002
	.text
	.align	4
	.globl	_camlCompare_mono__entry
_camlCompare_mono__entry:
	.cfi_startproc
L102:
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
				/* relocation table start */
	.align	3
				/* relocation table end */
	.data
	.quad	0
	.globl	_camlCompare_mono__data_end
_camlCompare_mono__data_end:
	.quad	0
	.align	3
	.globl	_camlCompare_mono__frametable
_camlCompare_mono__frametable:
	.quad	0
