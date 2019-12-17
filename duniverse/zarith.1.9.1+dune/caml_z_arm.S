/*
   Assembly version for the fast path of some functions in Z:
   - ARM v5M and above target
   - System 5 ABI and assembly syntax
   - GNU as

 
   This file is part of the Zarith library 
   http://forge.ocamlcore.org/projects/zarith .
   It is distributed under LGPL 2 licensing, with static linking exception.
   See the LICENSE file included in the distribution.
   
   Copyright (c) 2013 Xavier Leroy, INRIA Paris-Rocquencourt,
   and Antoine Miné, Abstraction project.
   Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
   a joint laboratory by:
   CNRS (Centre national de la recherche scientifique, France),
   ENS (École normale supérieure, Paris, France),
   INRIA Rocquencourt (Institut national de recherche en informatique, France).

 */

        
        /* makes the stack non-executable. */
        .section .note.GNU-stack,"",%progbits


        /* helper functions */
        /* **************** */


        /* dot prefix for local labels */

#define L(x) .L##x
	
        /* function prolog & epilog */

#define PROLOG(proc) \
        .text;   \
        .global ml_as_z_##proc; \
        .type   ml_as_z_##proc, %function; \
ml_as_z_##proc:

#define EPILOG(proc) \
        .size   ml_as_z_##proc, . - ml_as_z_##proc

        /* calling C functions */

#define C_JMP(proc) \
        b     ml_z_##proc(PLT)

        /* operation counter (not implemented) */
        
#define OP
        
        /* unary arithmetics */
        /* ***************** */

        /* neg */
        PROLOG(neg)
L(negenter):	
	tst     r0, #1
        beq     L(neg)
        rsbs    r1, r0, #2      /* r1 = 2 - r0 */
        bvs     L(neg)
        mov     r0, r1
        OP
        bx      lr
L(neg):
        C_JMP(neg)
        EPILOG(neg)

        
        /* abs */
        PROLOG(abs)
	tst     r0, #1
        beq     L(abs)
        cmp     r0, #0
        bge     L(abs2)
        rsbs    r1, r0, #2      /* r1 = 2 - r0 */
        bvs     L(abs)
        mov     r0, r1
L(abs2):
        OP
        bx      lr
L(abs):
        C_JMP(abs)
        EPILOG(abs)

        
        /* succ */
        PROLOG(succ)
        tst     r0, #1
        beq     L(succ)
        adds    r1, r0, #2
        bvs     L(succ)
        mov     r0, r1
        OP
        bx	lr
L(succ):
        C_JMP(succ)
        EPILOG(succ)

        
        /* pred */
        PROLOG(pred)
        tst     r0, #1
        beq     L(pred)
        subs    r1, r0, #2
        bvs     L(pred)
        mov     r0, r1
        OP
        bx	lr
L(pred):
        C_JMP(pred)
        EPILOG(pred)



        
        /* binary arithmetics */
        /* ****************** */

        
        /* add */
        PROLOG(add)
	and     r2, r0, r1
        tst     r2, #1
        beq     L(add)
        sub     r2, r0, #1
        adds    r2, r2, r1
        bvs     L(add)
	mov     r0, r2
        OP
        bx	lr
L(add):
        C_JMP(add)
        EPILOG(add)

        
        /* sub */
        PROLOG(sub)
	and     r2, r0, r1
        tst     r2, #1
        beq     L(sub)
	subs    r2, r0, r1
        bvs     L(sub)
	add     r0, r2, #1
        OP
        bx	lr
L(sub):
        C_JMP(sub)
        EPILOG(sub)

  
        /* mul */
        PROLOG(mul)
	and     r2, r0, r1
        tst     r2, #1
        beq     L(mul)
        sub     r2, r0, #1
        mov     r3, r1, asr #1
        smull   r3, r12, r2, r3  /* r3 = low half of product, r12 = high half */
        cmp     r12, r3, asr #31 /* high half must equal sign-ext of low half */
        bne     L(mul)           /* otherwise, overflow occurred */
        add     r0, r3, #1
        OP
        bx	lr
L(mul):
        C_JMP(mul)
        EPILOG(mul)

        /* bit operations */
        /* ************** */


        /* not */
        PROLOG(lognot)
	tst     r0, #1
        beq     L(lognot)
        sub     r0, r0, #1
        mvn     r0, r0
        OP
        bx	lr
L(lognot):
        C_JMP(lognot)
        EPILOG(lognot)

        
        /* and */
        PROLOG(logand)
	and     r2, r0, r1
        tst     r2, #1
        beq     L(logand)
        mov     r0, r2
        OP
        bx	lr
L(logand):
        C_JMP(logand)
        EPILOG(logand)

        
         /* or */
        PROLOG(logor)
	and     r2, r0, r1
        tst     r2, #1
        beq     L(logor)
        orr     r0, r0, r1
        OP
        bx	lr
L(logor):
        C_JMP(logor)
        EPILOG(logor)

        
         /* xor */
        PROLOG(logxor)
	and     r2, r0, r1
        tst     r2, #1
        beq     L(logxor)
        eor     r0, r0, r1
        orr     r0, r0, #1
        OP
        bx	lr
L(logxor):
        C_JMP(logxor)
        EPILOG(logxor)

        
         /* shift_left */
        PROLOG(shift_left)
	tst     r0, #1
        beq     L(shift_left)
        cmp     r1, #63        /* 31 in 2n+1 encoding */
        bhs     L(shift_left)
        mov     r3, r1, asr #1
	sub     r2, r0, #1
        mov     r12, r2, lsl r3
        cmp     r2, r12, asr r3
        bne     L(shift_left)  /* overflow occurred */
        orr     r0, r12, #1
        OP
        bx	lr
L(shift_left):
        C_JMP(shift_left)
        EPILOG(shift_left)


        /* shift_right */
        PROLOG(shift_right)
        tst     r0, #1
        beq     L(shift_right)
        movs    r2, r1, asr #1
        bmi     L(shift_right) /* if shift amount < 0, go to slow path */
        cmp     r2, #31
        movlo   r0, r0, asr r2 /* if shift amount < 31, shift by this amount */
        movhs   r0, r0, asr #31 /* if shift amount >= 31, shift by 31 */
	orr     r0, r0, #1
        OP
        bx	lr
L(shift_right):
        C_JMP(shift_right)
        EPILOG(shift_right)
        

