/* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. */

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

#if defined (__i386__) || defined (__x86_64__)
#define __x86__
#include <cpuid.h>
#endif /* __i386__ || __x86_64__ */

static inline int cpudetect (unsigned int res[9]) {
#if defined (__x86__)
  unsigned int __reg1, __reg2, __reg3, level = __get_cpuid_max (0x0, res);
  if (level < 1) return 0;
  __cpuid (1, res[1], __reg2, res[2], res[3]);
  if (level >= 7)
    __cpuid_count (7, 0, __reg1, res[4], res[5], __reg2);
  if (level >= 0xb)
    __cpuid_count (0xb, 1, __reg1, res[6], __reg2, __reg3);
  level = __get_cpuid_max (0x80000000, NULL);
  if (level >= 0x80000001)
    __cpuid (0x80000001, __reg1, __reg2, res[7], res[8]);
  return 1;
#else
  return 0;
#endif /* __x86__ */
}

CAMLprim value caml_cpuid_cpudetect (value unit) {
  CAMLparam1 (unit);
  CAMLlocal2 (opt, tup);
  unsigned int arr[9] = { 0 };
  if (cpudetect (arr)) {
    opt = caml_alloc (1, 0);
    Field (opt, 0) = tup = caml_alloc_tuple (9);
    for (int i = 0; i < 9; i++)
      Field (tup, i) = caml_copy_int32 (arr[i]);
  }
  CAMLreturn (opt);
}
