#ifndef ocaml_utils_macros_h
#define ocaml_utils_macros_h

#include <caml/mlvalues.h>
#include <caml/version.h>

#define UNUSED __attribute__((unused))

#define Is_string(v) (Is_block(v) && Tag_val(v) == String_tag)

#define Is_int_option(v) \
  (Is_none(v) \
    || (Is_block(v) && Tag_val(v) == 0 && Wosize_val(v) == 1 && Is_long(Field(v, 0))))

#define Is_string_option(v) \
  (Is_none(v) \
    || (Is_block(v) && Tag_val(v) == 0 && Wosize_val(v) == 1 && Is_string(Field(v, 0))))

#define Is_custom(v) (Is_block(v) && Tag_val(v) == Custom_tag)
#define Custom_block_name(v) (Custom_ops_val(v)->identifier)

#if OCAML_VERSION < 41200
#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)
#define Is_none(v) ((v) == Val_none)
#define Is_some(v) Is_block(v)
#endif

/* [strcmp] is defined as a macro in our current compilation environment.  We use
   [strcmp_not_a_macro] instead so that the text of this macro does not overflow the
   C89 limit on string literal length when used inside [assert]. */

/* defined in ocaml_utils_stubs.c */
extern int strcmp_not_a_macro(const char*, const char*);

#define Is_custom_named(v, name) \
  (Is_custom(v) && !strcmp_not_a_macro(name, Custom_block_name(v)))

#endif
