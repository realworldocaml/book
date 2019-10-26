/*
   Assembly version for the fast path of some functions in Z:
   - x86_64 target
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

#if defined(Z_ELF)
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

#if defined(Z_MACOS)
#define PROLOG(proc) \
        .text;   \
        .globl  SYMB(ml_as_z_##proc); \
        .align  FUNCTION_ALIGN; \
SYMB(ml_as_z_##proc):

#define EPILOG(proc)

#endif

        /* calling C functions */

#if defined(Z_ELF)
#define C_JMP(proc) \
        jmp     SYMB(ml_z_##proc@PLT)
#endif

#if defined(Z_MACOS)
#define C_JMP(proc) \
        jmp     SYMB(ml_z_##proc)
#endif

        /* operation counter */
        
#ifndef Z_PERF_COUNTER
#define OP
#else
#if defined(Z_ELF) || defined(Z_MACOS)
#define OP \
        mov     SYMB(ml_z_ops_as@GOTPCREL(%rip)), %rcx; \
        addq    $1, (%rcx)
#endif
#endif
        
        /* unary arithmetics */
        /* ***************** */

        /* neg */
        PROLOG(neg)
L(negenter):	
        test    $1, %dil
        jz      L(neg)
        mov     %rdi, %rax
        not     %rax
        add     $3, %rax
        jo      L(neg)
        OP
        ret
L(neg):
        C_JMP(neg)
        EPILOG(neg)

        
        /* abs */
        PROLOG(abs)
        test    $1, %dil
        jz      L(abs)
        mov     %rdi, %rax
        test    %rdi, %rdi
        jns     L(abs2)
        not     %rax
        add     $3, %rax
        jo      L(neg)
L(abs2):
        OP
        ret
L(abs):
        C_JMP(abs)
        EPILOG(abs)

        
        /* succ */
        PROLOG(succ)
        test    $1, %dil
        jz      L(succ)
        mov     %rdi, %rax
        add     $2, %rax
        jo      L(succ)
        OP
        ret
L(succ):
        C_JMP(succ)
        EPILOG(succ)

        
        /* pred */
        PROLOG(pred)
        test    $1, %dil
        jz      L(pred)
        mov     %rdi, %rax
        sub     $2, %rax
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
        test    $1, %dil
        jz      L(add)
        test    $1, %sil
        jz      L(add)        
        lea     -1(%rdi), %rax
        add     %rsi, %rax
        jo      L(add)
        OP
        ret
L(add):
        C_JMP(add)
        EPILOG(add)

        
        /* sub */
        PROLOG(sub)
        test    $1, %dil
        jz      L(sub)
        test    $1, %sil
        jz      L(sub)
        mov     %rdi, %rax
        sub     %rsi, %rax
        jo      L(sub)
        inc     %rax
        OP
        ret
L(sub):
        C_JMP(sub)
        EPILOG(sub)

  
        /* mul */
        PROLOG(mul)
        test    $1, %dil
        jz      L(mul)
        mov     %rsi, %rcx
        sar     %rcx
        jnc     L(mul)
        lea     -1(%rdi), %rax
        imul    %rcx, %rax
        jo      L(mul)
        inc     %rax
        OP
        ret
L(mul):
        C_JMP(mul)
        EPILOG(mul)

  
        /* div */
        PROLOG(div)
        mov     %rsi, %rcx
        cmp     $-1, %rsi
                   /* division by -1, the only one that can overflow */
        je      L(negenter)
        sar     %rcx
        jnc     L(div)	
        jz      L(div)   /* division by zero */
        mov     %rdi, %rax
        sar     %rax
        jnc     L(div)
        cqo
        idiv    %rcx
        lea     1(%rax, %rax), %rax
        OP
        ret
L(div):
        C_JMP(div)
        EPILOG(div)

        
         /* divexact */
        PROLOG(divexact)
        mov     %rsi, %rcx
        cmp     $-1, %rsi
                   /* division by -1, the only one that can overflow */
        je      L(negenter)
        sar     %rcx
        jnc     L(divexact)	
        jz      L(divexact)   /* division by zero */
        mov     %rdi, %rax
        sar     %rax
        jnc     L(divexact)
        cqo
        idiv    %rcx
        lea     1(%rax, %rax), %rax
        OP
        ret
L(divexact):
        C_JMP(divexact)
        EPILOG(divexact)


        /* rem */
        PROLOG(rem)
        mov     %rdi, %rax
        sar     %rax
        jnc     L(rem)
        mov     %rsi, %rcx
        sar     %rcx
        jnc     L(rem)
        jz      L(rem)   /* division by zero */
        cmp     $-1, %rcx
        je      L(remneg)
        cqo
        idiv    %rcx
        lea     1(%rdx, %rdx), %rax
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
        test    $1, %dil
        jz      L(lognot)
        lea     -1(%rdi), %rax
        not     %rax
        OP
        ret
L(lognot):
        C_JMP(lognot)
        EPILOG(lognot)

        
        /* and */
        PROLOG(logand)
        mov     %rdi, %rax
        and     %rsi, %rax
        test    $1, %al
        jz      L(logand)
        OP
        ret
L(logand):
        C_JMP(logand)
        EPILOG(logand)

        
         /* or */
        PROLOG(logor)
        test    $1, %dil
        jz      L(logor)
        test    $1, %sil
        jz      L(logor)
        mov     %rdi, %rax
        or      %rsi, %rax
        OP
        ret
L(logor):
        C_JMP(logor)
        EPILOG(logor)

        
         /* xor */
        PROLOG(logxor)
        test    $1, %dil
        jz      L(logxor)
        test    $1, %sil
        jz      L(logxor)
        lea     -1(%rdi), %rax
        xor     %rsi, %rax
        OP
        ret
L(logxor):
        C_JMP(logxor)
        EPILOG(logxor)

        
         /* shift_left */
        PROLOG(shift_left)
        test    $1, %dil
        jz      L(shift_left)
        mov     %esi, %ecx
        sar     %ecx
        cmp     $127, %rsi /* 63 unboxed */
        jae     L(shift_left)
        lea     -1(%rdi), %rax
        mov     %rax, %r8
        sal     %cl, %rax
        mov     %rax, %rdx
        sar     %cl, %rdx
        cmp     %r8, %rdx
        jne     L(shift_left) /* overflow */
        inc     %rax
        OP
        ret
L(shift_left):
        C_JMP(shift_left)
        EPILOG(shift_left)


        /* shift_right */
        PROLOG(shift_right)
        test    $1, %dil
        jz      L(shift_right)
        mov     %rsi, %rcx
	mov     $63, %eax
        sar     %rcx
        js      L(shift_right)
        cmp     %rax, %rcx /* compare second argument to 63 */
        cmovae  %eax, %ecx /* if above or equal, then use 63 */
        mov     %rdi, %rax
        sar     %cl, %rax
        or      $1, %rax
        OP
        ret
L(shift_right):
        C_JMP(shift_right)
        EPILOG(shift_right)
        

