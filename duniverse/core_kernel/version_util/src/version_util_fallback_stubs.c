/* version_util.ml refers to this. This allows any library that depends on this one to be
   installed as a standalone library, e.g. for use in a toplevel. Our build rules, when
   building an executable, generate replacement functions with real information in
   *.build_info.c and *.hg_version.c. The latter ones have precedence as the ones in this
   file are defined as weak. And because all of this is C functinos, we can replace this
   information without doing a ton of ocaml compilation (essentially just relinking). */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

CAMLprim CAMLweakdef value generated_hg_version (value unit __attribute__ ((unused)))
{
  return(caml_copy_string(""));
}

CAMLprim CAMLweakdef value generated_build_info (value unit __attribute__ ((unused)))
{
  return(caml_copy_string(""));
}
