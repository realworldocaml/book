/*
   Assembly version for the fast path of some functions in Z:
   - x86_64 target
   - Win64 ABI
   - GNU as

 
   This file is part of the Zarith library 
   http://forge.ocamlcore.org/projects/zarith .
   It is distributed under LGPL 2 licensing, with static linking exception.
   See the LICENSE file included in the distribution.
   
   Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
   Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
   a joint laboratory by:
   CNRS (Centre national de la recherche scientifique, France),
   ENS (École normale supérieure, Paris, France),
   INRIA Rocquencourt (Institut national de recherche en informatique, France).

 */

        
        /* helper functions */
        /* **************** */


#define SYMB(x)  x
#define FUNCTION_ALIGN 16

#define PROLOG(proc) \
        .text;   \
        .globl  SYMB(ml_as_z_##proc); \
        .align  FUNCTION_ALIGN; \
SYMB(ml_as_z_##proc):\

#define EPILOG(proc)

#define C_JMP(proc) \
        jmp     SYMB(ml_z_##proc)

	
        /* operation counter */
        
#ifndef Z_PERF_COUNTER
#define OP
#else
#define OP \
        mov     SYMB(ml_z_ops_as(%rip)), %rcx; \
        addq    $1, (%rcx)
#endif
        
        /* unary arithmetics */
        /* ***************** */

        /* neg */
        PROLOG(neg)
        test    $1, %rcx
        jz      .Lneg
        mov     %rcx, %rax
        not     %rax
        add     $3, %rax
        jo      .Lneg
        OP
        ret
.Lneg:
        C_JMP(neg)
        EPILOG(neg)

        
        /* abs */
        PROLOG(abs)
        test    $1, %rcx
        jz      .Labs
        mov     %rcx, %rax
        test    %rcx, %rcx
        jns     .Labs2
        not     %rax
        add     $3, %rax
        jo      .Lneg
.Labs2:
        OP
        ret
.Labs:
        C_JMP(abs)
        EPILOG(abs)

        
        /* succ */
        PROLOG(succ)
        test    $1, %rcx
        jz      .Lsucc
        mov     %rcx, %rax
        add     $2, %rax
        jo      .Lsucc
        OP
        ret
.Lsucc:
        C_JMP(succ)
        EPILOG(succ)

        
        /* pred */
        PROLOG(pred)
        test    $1, %rcx
        jz      .Lpred
        mov     %rcx, %rax
        sub     $2, %rax
        jo      .Lpred
        OP
        ret
.Lpred:
        C_JMP(pred)
        EPILOG(pred)



        
        /* binary arithmetics */
        /* ****************** */

        
        /* add */
        PROLOG(add)
        test    $1, %rcx
        jz      .Ladd
        test    $1, %rdx
        jz      .Ladd        
        lea     -1(%rcx), %rax
        add     %rdx, %rax
        jo      .Ladd
        OP
        ret
.Ladd:
        C_JMP(add)
        EPILOG(add)

        
        /* sub */
        PROLOG(sub)
        test    $1, %rcx
        jz      .Lsub
        test    $1, %rdx
        jz      .Lsub
        mov     %rcx, %rax
        sub     %rdx, %rax
        jo      .Lsub
        inc     %rax
        OP
        ret
.Lsub:
        C_JMP(sub)
        EPILOG(sub)

  
        /* mul */
        PROLOG(mul)
        test    $1, %rcx
        jz      .Lmul
        test    $1, %rdx
        jz      .Lmul
        lea     -1(%rdx), %rax
	mov	%rcx, %r8
        sar     %r8
        imul    %r8, %rax
        jo      .Lmul
        inc     %rax
        OP
        ret
.Lmul:
        C_JMP(mul)
        EPILOG(mul)

  
        /* div */
        PROLOG(div)
        test    $1, %rcx
        jz      .Ldiv
        test    $1, %rdx
        jz      .Ldiv
        mov     %rdx, %r8
        mov     %rcx, %rax
        sar     %r8
        jz      .Ldiv   /* division by zero */
        cmp     $-1, %r8
        je      .Ldivneg
        sar     %rax
        cqo
        idiv    %r8
        sal     %rax
        inc     %rax
        OP
        ret
.Ldivneg:
        /* division by -1, the only one that can overflow */
        not     %rax
        add     $3, %rax
        jo      .Ldiv
        OP
        ret
.Ldiv:
        C_JMP(div)
        EPILOG(div)

        
        /* divexact */
        PROLOG(divexact)
        test    $1, %rcx
        jz      .Ldivexact
        test    $1, %rdx
        jz      .Ldivexact
        mov     %rdx, %r8
        mov     %rcx, %rax
        sar     %r8
        jz      .Ldivexact   /* division by zero */
        cmp     $-1, %r8
        je      .Ldivexactneg
        sar     %rax
        cqo
        idiv    %r8
        sal     %rax
        inc     %rax
        OP
        ret
.Ldivexactneg:
        /* division by -1, the only one that can overflow */
        not     %rax
        add     $3, %rax
        jo      .Ldivexact
        OP
        ret
.Ldivexact:
        C_JMP(divexact)
        EPILOG(divexact)

        
        /* rem */
        PROLOG(rem)
        test    $1, %rcx
        jz      .Lrem
        test    $1, %rdx
        jz      .Lrem
        mov     %rdx, %r8
	mov	%rcx, %rax
        sar     %r8
        jz      .Lrem   /* division by zero */
        cmp     $-1, %r8
        je      .Lremneg
        sar     %rax
        cqo
        idiv    %r8
        sal     %rdx
        lea     1(%rdx), %rax
        OP
        ret
.Lremneg:
        /* division by -1 */
        mov     $1, %rax
        OP
        ret
.Lrem:
        C_JMP(rem)
        EPILOG(rem)

        
        /* bit operations */
        /* ************** */


        /* not */
        PROLOG(lognot)
        test    $1, %rcx
        jz      .Llognot
        lea     -1(%rcx), %rax
        not     %rax
        OP
        ret
.Llognot:
        C_JMP(lognot)
        EPILOG(lognot)

        
        /* and */
        PROLOG(logand)
        mov     %rcx, %rax
        and     %rdx, %rax
        test    $1, %rax
        jz      .Llogand
        OP
        ret
.Llogand:
        C_JMP(logand)
        EPILOG(logand)

        
         /* or */
        PROLOG(logor)
        test    $1, %rcx
        jz      .Llogor
        test    $1, %rdx
        jz      .Llogor
        mov     %rcx, %rax
        or      %rdx, %rax
        OP
        ret
.Llogor:
        C_JMP(logor)
        EPILOG(logor)

        
         /* xor */
        PROLOG(logxor)
        test    $1, %rcx
        jz      .Llogxor
        test    $1, %rdx
        jz      .Llogxor
        lea     -1(%rcx), %rax
        xor     %rdx, %rax
        OP
        ret
.Llogxor:
        C_JMP(logxor)
        EPILOG(logxor)


         /* shift_left */
        PROLOG(shift_left)
        test    $1, %rcx
        jz      .Lshift_left2
        lea     -1(%rcx), %rax
        mov     %rcx, %r9
        mov     %rdx, %r10
        sar     %rdx
        cmp     $63, %rdx
        jae     .Lshift_left
        mov     %rdx, %rcx
        mov     %rax, %r8
        sal     %cl, %rax
        mov     %rax, %rdx
        sar     %cl, %rdx
        cmp     %r8, %rdx
        jne     .Lshift_left /* overflow */
        inc     %rax
        OP
        ret
.Lshift_left:
        mov     %r9, %rcx
        mov     %r10, %rdx
.Lshift_left2:
       C_JMP(shift_left)
        EPILOG(shift_left)


        /* shift_right */
        PROLOG(shift_right)
        test    $1, %rcx
        jz      .Lshift_right
        mov     %rcx, %rax
        mov     %rdx, %rcx
        sar     %rcx
        js      .Lshift_right
        cmp     $63, %rcx
        jae     .Lshift_right2
        sar     %cl, %rax
        or      $1, %rax
        OP
        ret
.Lshift_right2:
        /* shift by 63 or more */
        test    %rax, %rax
        js      .Lshift_right3
        mov     $1, %rax
        OP
        ret
.Lshift_right3:
        mov     $-1, %rax
        OP
        ret
.Lshift_right:
        C_JMP(shift_right)
        EPILOG(shift_right)
        
