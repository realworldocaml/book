	.literal16
	.align	4
_caml_negf_mask:	.quad   0x8000000000000000, 0
	.align	4
_caml_absf_mask:	.quad   0x7FFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF
	.data
	.globl	_camlFast_match__data_begin
_camlFast_match__data_begin:
	.text
	.globl	_camlFast_match__code_begin
_camlFast_match__code_begin:
	nop
	.data
	.quad	1024
	.globl	_camlFast_match
_camlFast_match:
	.space	8
	.data
	.quad	2295
_camlFast_match__1:
	.quad	_camlFast_match__which_number_1008
	.quad	3
	.text
	.align	4
	.globl	_camlFast_match__which_number_1008
_camlFast_match__which_number_1008:
	.cfi_startproc
.L106:
	cmpq	$9, %rax
	jbe	.L105
	movq	$13, %rax
	ret
	.align	2
.L105:
	sarq	$1, %rax
	leaq	.L107(%rip), %rdx
	movslq	(%rdx, %rax, 4), %rax
	addq	%rax, %rdx
	jmp	*%rdx
	.const
	.align	2
.L107:	.long	.L104 - .L107
	.long	.L103 - .L107
	.long	.L102 - .L107
	.long	.L101 - .L107
	.long	.L100 - .L107
	.text
	.align	2
.L104:
	movq	$3, %rax
	ret
	.align	2
.L103:
	movq	$5, %rax
	ret
	.align	2
.L102:
	movq	$7, %rax
	ret
	.align	2
.L101:
	movq	$9, %rax
	ret
	.align	2
.L100:
	movq	$11, %rax
	ret
	.cfi_endproc
	.text
	.align	4
	.globl	_camlFast_match__entry
_camlFast_match__entry:
	.cfi_startproc
.L108:
	movq	_camlFast_match__1@GOTPCREL(%rip), %rax
	movq	_camlFast_match@GOTPCREL(%rip), %rbx
	movq	%rax, (%rbx)
	movq	$1, %rax
	ret
	.cfi_endproc
	.data
	.text
	nop
	.globl	_camlFast_match__code_end
_camlFast_match__code_end:
	.data
	.globl	_camlFast_match__data_end
_camlFast_match__data_end:
	.long	0
	.globl	_camlFast_match__frametable
_camlFast_match__frametable:
	.quad	0
