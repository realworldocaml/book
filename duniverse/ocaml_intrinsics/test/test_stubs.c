#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <stdio.h>

static inline value encode(void *ptr)
{
  return (value)((uintnat)ptr | 1);
}

static inline void *alloc_aligned(size_t size)
{
  void *ref = NULL;
  size_t align = size > sizeof(void *) ? size : sizeof(void *);
  if (posix_memalign(&ref, align, size)) {
    fprintf(stderr, "Error: cannot allocate aligned %zu bytes\n", size);
    exit(1);
  }
  return ref;
}

static inline intnat *make_untagged_int(value init)
{
  intnat *ref = alloc_aligned(sizeof(intnat));
  *ref = (intnat)(Long_val(init));
  return ref;
}

static inline intnat *make_immediate(value init)
{
  intnat *ref = alloc_aligned(sizeof(intnat));
  *ref = (intnat)init;
  return ref;
}

static inline double *make_unboxed_float(value init)
{
  double* ref = alloc_aligned(sizeof(double));
  *ref = Double_val(init);
  return ref;
}

static inline int64_t *make_unboxed_int64(value init)
{
  int64_t* ref = alloc_aligned(sizeof(int64_t));
  *ref = Int64_val(init);
  return ref;
}

static inline int32_t *make_unboxed_int32(value init)
{
  int32_t* ref = alloc_aligned(sizeof(int32_t));
  *ref = Int32_val(init);
  return ref;
}

static inline intnat *make_unboxed_nativeint(value init)
{
  intnat* ref = alloc_aligned(sizeof(intnat));
  *ref = Nativeint_val(init);
  return ref;
}

CAMLprim value external_untagged_int_ref(value init)
{
  return encode((void *)make_untagged_int(init));
}

CAMLprim value external_immediate_ref(value init)
{
  return encode((void *)make_immediate(init));
}

CAMLprim value external_unboxed_float_ref(value init)
{
  return encode((void *)make_unboxed_float(init));
}

CAMLprim value external_unboxed_int64_ref(value init)
{
  return encode((void *)make_unboxed_int64(init));
}

CAMLprim value external_unboxed_nativeint_ref(value init)
{
  return encode((void *)make_unboxed_nativeint(init));
}

CAMLprim value external_unboxed_int32_ref(value init)
{
  return encode((void *)make_unboxed_int32(init));
}

CAMLprim value external_untagged_int_ref_as_native_pointer(value init)
{
  return caml_copy_nativeint((uintnat)make_untagged_int(init));
}

CAMLprim value external_immediate_ref_as_native_pointer(value init)
{
  return caml_copy_nativeint((uintnat)make_immediate(init));
}

CAMLprim value external_unboxed_float_ref_as_native_pointer(value init)
{
  return caml_copy_nativeint((uintnat)make_unboxed_float(init));
}

CAMLprim value external_unboxed_int64_ref_as_native_pointer(value init)
{
  return caml_copy_nativeint((uintnat)make_unboxed_int64(init));
}

CAMLprim value external_unboxed_int32_ref_as_native_pointer(value init)
{
  return caml_copy_nativeint((uintnat)make_unboxed_int32(init));
}

CAMLprim value external_unboxed_nativeint_ref_as_native_pointer(value init)
{
  return caml_copy_nativeint((uintnat)make_unboxed_nativeint(init));
}
