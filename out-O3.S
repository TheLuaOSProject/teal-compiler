	.text
	.p2align 4
	.globl _add
	.no_dead_strip _add
_add:
LFB0:
L2:
	leal	(%rdi,%rsi), %eax
	ret
LFE0:
	.p2align 4
	.globl _subtract
	.no_dead_strip _subtract
_subtract:
LFB1:
L5:
	movl	%edi, %eax
	subl	%esi, %eax
	ret
LFE1:
	.p2align 4
	.globl _multiply
	.no_dead_strip _multiply
_multiply:
LFB2:
L7:
	movl	%edi, %eax
	imull	%esi, %eax
	ret
LFE2:
	.p2align 4
	.globl _divide
	.no_dead_strip _divide
_divide:
LFB3:
L9:
	movl	%edi, %eax
	cltd
	idivl	%esi
	ret
LFE3:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0
	.byte	0x1
	.ascii "zR\0"
	.uleb128 0x1
	.sleb128 -8
	.byte	0x10
	.uleb128 0x1
	.byte	0x10
	.byte	0xc
	.uleb128 0x7
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x1
	.align 3
LECIE1:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB0-.
	.set L$set$2,LFE0-LFB0
	.quad L$set$2
	.uleb128 0
	.align 3
LEFDE1:
LSFDE3:
	.set L$set$3,LEFDE3-LASFDE3
	.long L$set$3
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB1-.
	.set L$set$4,LFE1-LFB1
	.quad L$set$4
	.uleb128 0
	.align 3
LEFDE3:
LSFDE5:
	.set L$set$5,LEFDE5-LASFDE5
	.long L$set$5
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB2-.
	.set L$set$6,LFE2-LFB2
	.quad L$set$6
	.uleb128 0
	.align 3
LEFDE5:
LSFDE7:
	.set L$set$7,LEFDE7-LASFDE7
	.long L$set$7
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB3-.
	.set L$set$8,LFE3-LFB3
	.quad L$set$8
	.uleb128 0
	.align 3
LEFDE7:
	.ident	"GCC: (Homebrew GCC 13.2.0) 13.2.0"
	.subsections_via_symbols
