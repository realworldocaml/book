#include "caml/config.h"
#include "caml/mlvalues.h"

#if defined(__GNUC__)
#ifdef  __ARM_FEATURE_CRC32
#include <arm_acle.h>
#endif
#ifdef ARCH_SIXTYFOUR
static inline uint64_t crc64(uint64_t initial, uint64_t data)
{
#ifdef  __ARM_FEATURE_CRC32
  return __crc32cd(initial, data);
#elif defined(__SSE4_2__)
  return __builtin_ia32_crc32di(initial, data);
#else
#error "Target not supported"
#endif
}
#else // not ARCH_SIXTYFOUR
static inline uint32_t crc32(uint32_t initial, uint32_t data)
{
#ifdef  __ARM_FEATURE_CRC32
  return __crc32cw(initial, data);
#elif defined(__SSE4_2__)
   return __builtin_ia32_crc32si(initial, data);
#else
#error "Target not supported"
#endif
}
#endif // ARCH_SIXTYFOUR
#elif defined(_MSC_VER)
#error "Functionality on Windows has not been tested"
#include <intrin.h>
#ifdef ARCH_SIXTYFOUR
static inline uint64_t crc64(uint64_t initial, uint64_t data)
{
   return _mm_crc32_u64(initial, data);
}
#else // not ARCH_SIXTYFOUR
static inline uint32_t crc32(uint32_t initial, uint32_t data)
{
   return _mm_crc32_u32(initial, data);
}
#endif // ARCH_SIXTYFOUR
#else
  #error "Target not supported"
  uint64_t crc64(uint64_t initial, uint64_t data);
  uint32_t crc32(uint32_t initial, uint32_t data);
#endif

intnat caml_int_crc_untagged(intnat initial, intnat data)
{
#ifdef ARCH_SIXTYFOUR
  return crc64(initial, (uint64_t) data);
#else
  return crc32(initial, (uint32_t) data);
#endif
}

#ifdef ARCH_SIXTYFOUR
intnat caml_int64_crc_unboxed(intnat initial, int64_t data)
{
  return crc64(initial, (uint64_t) data);
}
#else
intnat caml_int64_crc_unboxed(__attribute__ ((unused)) intnat initial,
                              __attribute__ ((unused)) int64_t data)
{
  // Instruction crc32q not available on 32-bit platforms
  abort();
}
#endif

CAMLprim value caml_int_crc(value v_initial, value v_data)
{
  return Val_long(caml_int_crc_untagged(Long_val(v_initial), Long_val(v_data)));
}

CAMLprim value caml_int64_crc(value v_initial, value v_data)
{
  return Val_long(caml_int64_crc_unboxed(Long_val(v_initial), Int64_val(v_data)));
}
