#ifndef MLRE2__UTIL_H
#define MLRE2__UTIL_H

/* must be #include'ed inside an extern "C" { } block */

#define Val_none (Val_int(0))

#define Regex_val(v) (*(RE2 **) Data_custom_val(v))
#define RegexSet_val(v) (*(RE2::Set **) Data_custom_val(v))
#define RegexOptions_val(v) (*(RE2::Options **) Data_custom_val(v))

#define PAIR(v_dst, v_a, v_b) do {                   \
    v_dst = caml_alloc_small(2, 0);                  \
    Field(v_dst, 0) = (v_a);                         \
    Field(v_dst, 1) = (v_b);                         \
  } while(false)

#define SOME(v_dst, v) do {                       \
    v_dst = caml_alloc_small(1, 0);               \
    Field(v_dst, 0) = v;                          \
  } while(false)

#endif /* MLRE2__UTIL_H */
