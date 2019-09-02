#include <caml/memory.h>
#include <caml/mlvalues.h>
#include "ctypes_raw_pointer.h"

/* 'a -> voidp */
value ctypes_caml_roots_create(value v)
{
  value *p = caml_stat_alloc(sizeof *p);
  *p = v;
  caml_register_generational_global_root(p);
  return CTYPES_FROM_PTR(p);
}

/* voidp -> 'a -> unit */
value ctypes_caml_roots_set(value p_, value v)
{
  value *p = CTYPES_TO_PTR(p_);
  caml_modify_generational_global_root(p, v);
  return Val_unit;
}

/* voidp -> 'a */
value ctypes_caml_roots_get(value p_)
{
  value *p = CTYPES_TO_PTR(p_);
  return *p;
}

/* voidp -> unit */
value ctypes_caml_roots_release(value p_)
{
  value *p = CTYPES_TO_PTR(p_);
  caml_remove_generational_global_root(p);
  caml_stat_free(p);
  return Val_unit;
}

/* 'a -> unit */
value ctypes_use(value v)
{
  return Val_unit;
}
