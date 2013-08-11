	.data
	.globl	camlCompare_mono__data_begin
camlCompare_mono__data_begin:
	.text
	.globl	camlCompare_mono__code_begin
camlCompare_mono__code_begin:
	.data
	.quad	1024
	.globl	camlCompare_mono
camlCompare_mono:
	.space	8
	.data
	.quad	3319
camlCompare_mono__1:
	.quad	caml_curry2
	.quad	5
	.quad	camlCompare_mono__cmp_1008
	.text
	.align	16
	.globl	camlCompare_mono__cmp_1008
camlCompare_mono__cmp_1008:
	.cfi_startproc
.L101:
	cmpq	%rbx, %rax
	jle	.L100
	ret
	.align	4
.L100:
	movq	%rbx, %rax
	ret
	.cfi_endproc
	.type	camlCompare_mono__cmp_1008,@function
	.size	camlCompare_mono__cmp_1008,.-camlCompare_mono__cmp_1008
	.text
	.align	16
	.globl	camlCompare_mono__entry
camlCompare_mono__entry:
	.cfi_startproc
.L102:
	leaq	camlCompare_mono__1(%rip), %rax
	movq	%rax, camlCompare_mono(%rip)
	movq	$1, %rax
	ret
	.cfi_endproc
	.type	camlCompare_mono__entry,@function
	.size	camlCompare_mono__entry,.-camlCompare_mono__entry
	.data
	.text
	.globl	camlCompare_mono__code_end
camlCompare_mono__code_end:
	.data
	.globl	camlCompare_mono__data_end
camlCompare_mono__data_end:
	.long	0
	.globl	camlCompare_mono__frametable
camlCompare_mono__frametable:
	.quad	0
	.section .note.GNU-stack,"",%progbits
