/* This file is just preprocessed.  Lines of the form "OUT:XXX" are
   kept and replaced by XXX in the output to produce
   lib/config.h. */

#include <caml/config.h>

/* Defined in <caml/config.h> */
#if defined(ARCH_SIXTYFOUR)
"OUT:#define JSC_ARCH_SIXTYFOUR"
#else
"OUT:#undef JSC_ARCH_SIXTYFOUR"
#endif

/* Defined in <caml/config.h> */
#if defined(ARCH_BIG_ENDIAN)
"OUT:#define JSC_ARCH_BIG_ENDIAN"
#else
"OUT:#undef JSC_ARCH_BIG_ENDIAN"
#endif

#if defined(POSIX_TIMERS)
"OUT:#define JSC_POSIX_TIMERS"
#else
"OUT:#undef JSC_POSIX_TIMERS"
#endif
