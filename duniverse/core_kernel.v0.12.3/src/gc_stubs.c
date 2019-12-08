#include <caml/memory.h>

extern double caml_stat_minor_words;
extern double caml_stat_promoted_words;
extern double caml_stat_major_words;

extern char *caml_young_ptr;
extern char *caml_young_end;

extern uintnat caml_allocated_words;

extern intnat caml_stat_minor_collections;
extern intnat caml_stat_major_collections;
extern intnat caml_stat_heap_wsz;
extern intnat caml_stat_top_heap_wsz;
extern intnat caml_stat_compactions;
extern intnat caml_stat_heap_chunks;

static intnat minor_words(void)
{
  return (intnat) (caml_stat_minor_words +
            (double) Wsize_bsize (caml_young_end - caml_young_ptr));
}

CAMLprim value core_kernel_gc_minor_words(value unit __attribute__((unused)))
{
  return Val_long(minor_words());
}

static intnat major_words(void)
{
  return (intnat) (caml_stat_major_words + (double) caml_allocated_words);
}

CAMLprim value core_kernel_gc_major_words(value unit __attribute__((unused)))
{
  return Val_long(major_words());
}

CAMLprim value core_kernel_gc_promoted_words(value unit __attribute__((unused)))
{
  return Val_long((intnat) caml_stat_promoted_words);
}

CAMLprim value core_kernel_gc_minor_collections(value unit __attribute__((unused)))
{
  return Val_long(caml_stat_minor_collections);
}

CAMLprim value core_kernel_gc_major_collections(value unit __attribute__((unused)))
{
  return Val_long(caml_stat_major_collections);
}

CAMLprim value core_kernel_gc_heap_words(value unit __attribute__((unused)))
{
  return Val_long(caml_stat_heap_wsz);
}

CAMLprim value core_kernel_gc_heap_chunks(value unit __attribute__((unused)))
{
  return Val_long(caml_stat_heap_chunks);
}

CAMLprim value core_kernel_gc_compactions(value unit __attribute__((unused)))
{
  return Val_long(caml_stat_compactions);
}

CAMLprim value core_kernel_gc_top_heap_words(value unit __attribute__((unused)))
{
  return Val_long(caml_stat_top_heap_wsz);
}

CAMLprim value core_kernel_gc_major_plus_minor_words(value unit __attribute__((unused)))
{
  return Val_long(minor_words() + major_words());
}
