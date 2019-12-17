/*
   Assembly version for the fast path of some functions in Z:
   - x86 target
   - System 5 ABI and assembly syntax
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


        /* makes the stack non-executable. */
#ifdef Z_ELF
        .section .note.GNU-stack,"",@progbits
#endif

        /* helper functions */
        /* **************** */

        
        /* optional underscope prefix for symbols */
#ifdef Z_UNDERSCORE_PREFIX
#define SYMB(x) _##x
#else
#define SYMB(x)  x
#endif

	
        /* optional dot prefix for local labels */
#ifdef Z_DOT_LABEL_PREFIX
#define L(x) .L##x
#else
#define L(x)  L##x
#endif
	
        /* function prolog & epilog */

#if defined(Z_ELF) || defined(Z_COFF)
#define FUNCTION_ALIGN 16
#endif
#if defined(Z_MACOS)
#define FUNCTION_ALIGN 4
#endif

#if defined(Z_ELF)
#define PROLOG(proc) \
        .text;   \
        .globl  SYMB(ml_as_z_##proc); \
        .type   SYMB(ml_as_z_##proc), @function; \
        .align  FUNCTION_ALIGN; \
SYMB(ml_as_z_##proc):

#define EPILOG(proc) \
        .size   SYMB(ml_as_z_##proc), .-SYMB(ml_as_z_##proc)
#endif

#if defined(Z_COFF) || defined(Z_MACOS)
#define PROLOG(proc) \
        .text;   \
        .globl  SYMB(ml_as_z_##proc); \
        .align  FUNCTION_ALIGN; \
SYMB(ml_as_z_##proc):

#define EPILOG(proc)
#endif

        /* calling C functions */

#define C_JMP(proc) \
        jmp     SYMB(ml_z_##proc)


        /* operation counter */
        
#ifdef Z_PERF_COUNTER
#define OP \
        incl    SYMB(ml_z_ops_as)
#else
#define OP
#endif


        
        /* unary arithmetics */
        /* ***************** */

        /* neg */
        PROLOG(neg)
L(negenter):
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(neg)
        not     %eax
        add     $3, %eax
        jo      L(neg)
        OP
        ret
L(neg):
        C_JMP(neg)
        EPILOG(neg)


        /* abs */
        PROLOG(abs)
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(abs)
        test    %eax, %eax
        jns     L(abs2)
        not     %eax
        add     $3, %eax
        jo      L(abs)
L(abs2):
        OP
        ret
L(abs):
        C_JMP(abs)
        EPILOG(abs)

        
        /* succ */
        PROLOG(succ)
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(succ)
        add     $2, %eax
        jo      L(succ)
        OP
        ret
L(succ):
        C_JMP(succ)
        EPILOG(succ)


        /* pred */
        PROLOG(pred)
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(pred)
        sub     $2, %eax
        jo      L(pred)
        OP
        ret
L(pred):
        C_JMP(pred)
        EPILOG(pred)


        
        
        /* binary arithmetics */
        /* ****************** */

        /* add */
        PROLOG(add)
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(add)
        mov     8(%esp), %ecx
        test    $1, %cl
        jz      L(add)
        dec     %eax
        add     %ecx, %eax
        jo      L(add)
        OP
        ret
L(add):
        C_JMP(add)
        EPILOG(add)

 
        /* sub */
        PROLOG(sub)
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(sub)
        mov     8(%esp), %ecx
        test    $1, %cl
        jz      L(sub)
        sub     %ecx, %eax
        jo      L(sub)
        inc     %eax
        OP
        ret
L(sub):
        C_JMP(sub)
        EPILOG(sub)

        
        /* mul */
        PROLOG(mul)
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(mul)
        mov     8(%esp), %ecx
        sar     %ecx  /* sets CF to least significant bit */
        jnc     L(mul)
        dec     %eax
        imul    %ecx, %eax
        jo      L(mul)
        inc     %eax
        OP
        ret
L(mul):
        C_JMP(mul)
        EPILOG(mul)

        
        /* div */
        PROLOG(div)
        mov     8(%esp), %ecx
        sar     %ecx
        jnc     L(div)   /* not a 31-bit integer */
        jz      L(div)   /* division by zero */
        cmp     $-1, %ecx
	     /* division by -1, the only one that can overflow */
        je      L(negenter)
        mov     4(%esp), %eax
        sar     %eax
        jnc     L(div)   /* not a 31-bit integer */
        cdq
        idiv    %ecx
	lea	1(%eax, %eax), %eax
        OP
        ret
L(div):
        C_JMP(div)
        EPILOG(div)

        
        /* divexacty */
        PROLOG(divexact)
        mov     8(%esp), %ecx
        sar     %ecx
        jnc     L(divexact)   /* not a 31-bit integer */
        jz      L(divexact)   /* division by zero */
        cmp     $-1, %ecx
	     /* division by -1, the only one that can overflow */
        je      L(negenter)
        mov     4(%esp), %eax
        sar     %eax
        jnc     L(divexact)   /* not a 31-bit integer */
        cdq
        idiv    %ecx
	lea	1(%eax, %eax), %eax
        OP
        ret
L(divexact):
        C_JMP(divexact)
        EPILOG(divexact)

        
        /* rem */
        PROLOG(rem)
        mov     4(%esp), %eax
        sar     %eax
        jnc     L(rem)   /* not a 31-bit integer */
        mov     8(%esp), %ecx
        sar     %ecx
        jnc     L(rem)   /* not a 31-bit integer */
        jz      L(rem)   /* division by zero */
        cmp     $-1, %ecx
        je      L(remneg)
        cdq
        idiv    %ecx
	lea     1(%edx, %edx), %eax
        OP
        ret
L(remneg):
        /* division by -1 */
        mov     $1, %eax
        OP
        ret
L(rem):
        C_JMP(rem)
        EPILOG(rem)

        
        /* bit operations */
        /* ************** */

        
        /* not */
        PROLOG(lognot)
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(lognot)
        dec     %eax
        not     %eax
        ret
L(lognot):
        C_JMP(lognot)
        EPILOG(lognot)


        /* or */
        PROLOG(logor)
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(logor)
        mov     8(%esp), %ecx
        test    $1, %cl
        jz      L(logor)
        or      %ecx, %eax
        OP
        ret
L(logor):
        C_JMP(logor)
        EPILOG(logor)

        /* and */
        PROLOG(logand)
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(logand)
        mov     8(%esp), %ecx
        test    $1, %cl
        jz      L(logand)
        and     %ecx, %eax
        OP
        ret
L(logand):
        C_JMP(logand)
        EPILOG(logand)

        
        /* xor */
        PROLOG(logxor)
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(logxor)
        mov     8(%esp), %ecx
        test    $1, %cl
        jz      L(logxor)
        xor     %ecx, %eax
        inc     %eax
        OP
        ret
L(logxor):
        C_JMP(logxor)
        EPILOG(logxor)


        /* shift_left */
        PROLOG(shift_left)
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(shift_left)
        mov     8(%esp), %ecx
        sar     %ecx
        cmp     $31, %ecx
        jae     L(shift_left)
        dec     %eax
        sal     %cl, %eax
        mov     %eax, %edx
        sar     %cl, %edx
        inc     %edx
        cmp     4(%esp), %edx
        jne     L(shift_left) /* overflow */
        inc     %eax
        OP
        ret
L(shift_left):
        C_JMP(shift_left)
        EPILOG(shift_left)


        /* shift_right */
        PROLOG(shift_right)
        mov     4(%esp), %eax
        test    $1, %al
        jz      L(shift_right)
        mov     8(%esp), %ecx
        sar     %ecx
        js      L(shift_right)
        cmp     $31, %ecx
        jae     L(shift_right2)
        sar     %cl, %eax
        or      $1, %eax
        OP
        ret
L(shift_right2):
        /* shift by 31 or more */
        test    %eax, %eax
        js      L(shift_right3)
        mov     $1, %eax
        OP
        ret
L(shift_right3):
        mov     $-1, %eax
        OP
        ret
L(shift_right):
        C_JMP(shift_right)
        EPILOG(shift_right)
