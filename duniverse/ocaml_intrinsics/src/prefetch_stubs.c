#include "caml/config.h"
#include "caml/alloc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/bigarray.h"
#include "ext_pointer.h"

#if ((defined(__i386__) || defined(__x86_64__)) && defined(__GNUC__))

/* Anticipated operation  */
#define PREFETCH_FOR_READ 0
#define PREFETCH_FOR_WRITE 1

/* Mapping from user-level temporal locality hint to Intel specific values */
#define LOCALITY_NONE 0     // NTA hint to fill snoop filter
#define LOCALITY_LOW 1      // T2
#define LOCALITY_MODERATE 2 // T1
#define LOCALITY_HIGH 3     // T0
/*
 The meaning of Intel temporal locality hints

 T0 : temporal data
 prefetch data into all levels of the cache hierarchy.

 T1 : temporal data with respect to first level cache misses
 prefetch data into level 2 cache and higher.

 T2 : temporal data with respect to second level cache misses
 prefetch data into level 3 cache and higher, or an implementation-specific choice.

 NTA : non-temporal data with respect to all cache levels
 prefetch data into non-temporal cache structure and
 into a location close to the processor, minimizing cache pollution.

 For details, see Intel® 64 and IA-32 Architectures Optimization Reference Manual
 Section 9.3.2 Prefetch Instructions
 Table 9-1. Implementation Details of Prefetch Hint Instructions
*/

// Arguments rw and locality of __builtin_prefetch must be compile-time constants.
// Currently, some user-specified locality hints are ignored in prefetch for write.
static inline void prefetch_write_high(void *ptr)
{
  __builtin_prefetch(ptr, PREFETCH_FOR_WRITE, LOCALITY_HIGH);
}

static inline void prefetch_write_moderate(void *ptr)
{
  __builtin_prefetch(ptr, PREFETCH_FOR_WRITE, LOCALITY_MODERATE);
}

static inline void prefetch_write_low(void *ptr)
{
  __builtin_prefetch(ptr, PREFETCH_FOR_WRITE, LOCALITY_LOW);
}

static inline void prefetch_write_none(void *ptr)
{
  __builtin_prefetch(ptr, PREFETCH_FOR_WRITE, LOCALITY_NONE);
}

static inline void prefetch_read_high(void *ptr)
{
  __builtin_prefetch(ptr, PREFETCH_FOR_READ, LOCALITY_HIGH);
}

static inline void prefetch_read_moderate(void *ptr)
{
  __builtin_prefetch(ptr, PREFETCH_FOR_READ, LOCALITY_MODERATE);
}

static inline void prefetch_read_low(void *ptr)
{
  __builtin_prefetch(ptr, PREFETCH_FOR_READ, LOCALITY_LOW);
}

static inline void prefetch_read_none(void *ptr)
{
  __builtin_prefetch(ptr, PREFETCH_FOR_READ, LOCALITY_NONE);
}

#else
static inline void prefetch_write_high(__attribute__ ((unused)) void *ptr) {}
static inline void prefetch_write_moderate(__attribute__ ((unused)) void *ptr) {}
static inline void prefetch_write_low(__attribute__ ((unused)) void *ptr) {}
static inline void prefetch_write_none(__attribute__ ((unused)) void *ptr) {}
static inline void prefetch_read_high(__attribute__ ((unused)) void *ptr) {}
static inline void prefetch_read_moderate(__attribute__ ((unused)) void *ptr) {}
static inline void prefetch_read_low(__attribute__ ((unused)) void *ptr) {}
static inline void prefetch_read_none(__attribute__ ((unused)) void *ptr) {}
#endif

/* Native_pointer */

value caml_prefetch_write_high_native_pointer_unboxed(intnat ptr)
{
  prefetch_write_high((void *)ptr);
  return Val_unit;
}

value caml_prefetch_write_moderate_native_pointer_unboxed(intnat ptr)
{
  prefetch_write_moderate((void *)ptr);
  return Val_unit;
}

value caml_prefetch_write_low_native_pointer_unboxed(intnat ptr)
{
  prefetch_write_low((void *)ptr);
  return Val_unit;
}

value caml_prefetch_write_none_native_pointer_unboxed(intnat ptr)
{
  prefetch_write_none((void *)ptr);
  return Val_unit;
}

value caml_prefetch_read_high_native_pointer_unboxed(intnat ptr)
{
  prefetch_read_high((void *)ptr);
  return Val_unit;
}

value caml_prefetch_read_moderate_native_pointer_unboxed(value ptr)
{
  prefetch_read_moderate((void *)ptr);
  return Val_unit;
}

value caml_prefetch_read_low_native_pointer_unboxed(value ptr)
{
  prefetch_read_low((void *)ptr);
  return Val_unit;
}

value caml_prefetch_read_none_native_pointer_unboxed(value ptr)
{
  prefetch_read_none((void *)ptr);
  return Val_unit;
}

/* Ext_pointer */

value caml_prefetch_write_high_ext_pointer(value ptr)
{
  prefetch_write_high(caml_ext_pointer_decode(ptr));
  return Val_unit;
}

value caml_prefetch_write_moderate_ext_pointer(value ptr)
{
  prefetch_write_moderate(caml_ext_pointer_decode(ptr));
  return Val_unit;
}

value caml_prefetch_write_low_ext_pointer(value ptr)
{
  prefetch_write_low(caml_ext_pointer_decode(ptr));
  return Val_unit;
}

value caml_prefetch_write_none_ext_pointer(value ptr)
{
  prefetch_write_none(caml_ext_pointer_decode(ptr));
  return Val_unit;
}

value caml_prefetch_read_high_ext_pointer(value ptr)
{
  prefetch_read_high(caml_ext_pointer_decode(ptr));
  return Val_unit;
}

value caml_prefetch_read_moderate_ext_pointer(value ptr)
{
  prefetch_read_moderate(caml_ext_pointer_decode(ptr));
  return Val_unit;
}

value caml_prefetch_read_low_ext_pointer(value ptr)
{
  prefetch_read_low(caml_ext_pointer_decode(ptr));
  return Val_unit;
}

value caml_prefetch_read_none_ext_pointer(value ptr)
{
  prefetch_read_none(caml_ext_pointer_decode(ptr));
  return Val_unit;
}

/* Bigstring */

static char *bigstring_element_at_pos(value v_bstr, intnat pos)
{
  return ((char *)Caml_ba_data_val(v_bstr)) + pos;
}

value caml_prefetch_write_high_bigstring_untagged(value v_bstr, intnat pos)
{
  prefetch_write_high(bigstring_element_at_pos(v_bstr, pos));
  return Val_unit;
}

value caml_prefetch_write_moderate_bigstring_untagged(value v_bstr, intnat pos)
{
  prefetch_write_moderate(bigstring_element_at_pos(v_bstr, pos));
  return Val_unit;
}

value caml_prefetch_write_low_bigstring_untagged(value v_bstr, intnat pos)
{
  prefetch_write_low(bigstring_element_at_pos(v_bstr, pos));
  return Val_unit;
}

value caml_prefetch_write_none_bigstring_untagged(value v_bstr, intnat pos)
{
  prefetch_write_none(bigstring_element_at_pos(v_bstr, pos));
  return Val_unit;
}

value caml_prefetch_read_high_bigstring_untagged(value v_bstr, intnat pos)
{
  prefetch_read_high(bigstring_element_at_pos(v_bstr, pos));
  return Val_unit;
}

value caml_prefetch_read_moderate_bigstring_untagged(value v_bstr, intnat pos)
{
  prefetch_read_moderate(bigstring_element_at_pos(v_bstr, pos));
  return Val_unit;
}

value caml_prefetch_read_low_bigstring_untagged(value v_bstr, intnat pos)
{
  prefetch_read_low(bigstring_element_at_pos(v_bstr, pos));
  return Val_unit;
}

value caml_prefetch_read_none_bigstring_untagged(value v_bstr, intnat pos)
{
  prefetch_read_none(bigstring_element_at_pos(v_bstr, pos));
  return Val_unit;
}

/* Ignore in bytecode */

CAMLprim value caml_prefetch_ignore (__attribute__ ((unused)) value v)
{
  return Val_unit;
}

CAMLprim value caml_prefetch_ignore2 (__attribute__ ((unused)) value v_bstr,
                                      __attribute__ ((unused)) value v_pos)
{
  return Val_unit;
}

CAMLprim value caml_pause_hint (__attribute__ ((unused)) value unit)
{
#if ((defined(__i386__) || defined(__x86_64__)) && defined(__GNUC__))
  __builtin_ia32_pause();
#elif ((defined(__i386__) || defined(__x86_64__)) && defined(_MSC_VER))
#warning "Functionality on Windows has not been tested"
#include <intrin.h>
#pragma intrinsic(_mm_pause)
  _mm_pause();
#else
#warning "This target does not support PAUSE hints, emit NOP instead."
    /* do nothing */
#endif
  return Val_unit;
}
