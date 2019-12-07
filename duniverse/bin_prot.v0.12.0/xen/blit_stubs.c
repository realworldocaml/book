/* Blitting between buffers/strings/float arrays */

#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/bigarray.h>
#include <caml/signals.h>

#if defined(__GNUC__) && __GNUC__ >= 3
# ifndef __likely
#   define likely(x) __builtin_expect (!!(x), 1)
# endif
# ifndef __unlikely
#   define unlikely(x) __builtin_expect (!!(x), 0)
# endif
#else
# ifndef  __likely
#   define likely(x) (x)
# endif
# ifndef  __unlikely
#   define unlikely(x) (x)
# endif
#endif

#ifdef __MINIOS__
#define unlikely(x)     __builtin_expect((x),0)
#endif

#define get_buf(v_buf, v_pos) (char *) Caml_ba_data_val(v_buf) + Long_val(v_pos)

/* Bytes_val is only available from 4.06 */
#ifndef Bytes_val
#define Bytes_val String_val
#endif

CAMLprim value bin_prot_blit_string_buf_stub(
  value v_src_pos, value v_str, value v_dst_pos, value v_buf, value v_len)
{
  const char *str = String_val(v_str) + Long_val(v_src_pos);
  char *buf = get_buf(v_buf, v_dst_pos);
  memcpy(buf, str, (size_t) Long_val(v_len));
  return Val_unit;
}

CAMLprim value bin_prot_blit_bytes_buf_stub(
  value v_src_pos, value v_str, value v_dst_pos, value v_buf, value v_len)
{
  unsigned char *str = Bytes_val(v_str) + Long_val(v_src_pos);
  char *buf = get_buf(v_buf, v_dst_pos);
  memcpy(buf, str, (size_t) Long_val(v_len));
  return Val_unit;
}

CAMLprim value bin_prot_blit_buf_bytes_stub(
  value v_src_pos, value v_buf, value v_dst_pos, value v_str, value v_len)
{
  char *buf = get_buf(v_buf, v_src_pos);
  unsigned char *str = Bytes_val(v_str) + Long_val(v_dst_pos);
  memcpy(str, buf, (size_t) Long_val(v_len));
  return Val_unit;
}

CAMLprim value bin_prot_blit_buf_stub(
  value v_src_pos, value v_src, value v_dst_pos, value v_dst, value v_len)
{
  struct caml_ba_array *ba_src = Caml_ba_array_val(v_src);
  struct caml_ba_array *ba_dst = Caml_ba_array_val(v_dst);
  char *src = (char *) ba_src->data + Long_val(v_src_pos);
  char *dst = (char *) ba_dst->data + Long_val(v_dst_pos);
  size_t len = (size_t) Long_val(v_len);
  if
    (
      unlikely(len > 65536)
      || unlikely(((ba_src->flags & CAML_BA_MAPPED_FILE) != 0))
      || unlikely(((ba_dst->flags & CAML_BA_MAPPED_FILE) != 0))
    )
  /* use [memmove] rather than [memcpy] because src and dst may overlap */
  {
    Begin_roots2(v_src, v_dst);
    caml_enter_blocking_section();
      memmove(dst, src, len);
    caml_leave_blocking_section();
    End_roots();
  }
  else memmove(dst, src, len);
  return Val_unit;
}

CAMLprim value bin_prot_blit_float_array_buf_stub(
  value v_src_pos, value v_arr, value v_dst_pos, value v_buf, value v_len)
{
  char *arr = (char*)v_arr + Long_val(v_src_pos) * sizeof(double);
  char *buf = get_buf(v_buf, v_dst_pos);
  memcpy(buf, arr, (size_t) (Long_val(v_len) * sizeof(double)));
  return Val_unit;
}

CAMLprim value bin_prot_blit_buf_float_array_stub(
  value v_src_pos, value v_buf, value v_dst_pos, value v_arr, value v_len)
{
  char *buf = get_buf(v_buf, v_src_pos);
  char *arr = (char*)v_arr + Long_val(v_dst_pos) * sizeof(double);
  memcpy(arr, buf, (size_t) (Long_val(v_len) * sizeof(double)));
  return Val_unit;
}
