/*
   PCRE-OCAML - Perl Compatibility Regular Expressions for OCaml

   Copyright (C) 1999-  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#if defined(_WIN32)
#  define snprintf _snprintf
#  if defined(_DLL)
#    define PCREextern __declspec(dllexport)
#  else
#    define PCREextern
#  endif
#endif

#if _WIN64
  typedef long long *caml_int_ptr;
#else
  typedef long *caml_int_ptr;
#endif

#if __GNUC__ >= 3
# define inline inline __attribute__ ((always_inline))
# define __unused __attribute__ ((unused))
#else
# define __unused
# define inline
#endif

#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>

#include <pcre.h>

/* Error codes as defined for pcre 7.9, undefined in pcre 4.5 */
#ifndef PCRE_ERROR_PARTIAL
#define PCRE_ERROR_PARTIAL        (-12)
#endif
#ifndef PCRE_ERROR_BADPARTIAL
#define PCRE_ERROR_BADPARTIAL     (-13)
#endif
#ifndef PCRE_ERROR_RECURSIONLIMIT
#define PCRE_ERROR_RECURSIONLIMIT (-21)
#endif

typedef const unsigned char *chartables;  /* Type of chartable sets */

/* Contents of callout data */
struct cod {
  long subj_start;        /* Start of subject string */
  value *v_substrings_p;  /* Pointer to substrings matched so far */
  value *v_cof_p;         /* Pointer to callout function */
  value v_exn;            /* Possible exception raised by callout function */
};

/* Cache for exceptions */
static const value *pcre_exc_Error     = NULL;  /* Exception [Error] */
static const value *pcre_exc_Backtrack = NULL;  /* Exception [Backtrack] */

/* Cache for polymorphic variants */
static value var_Start_only;   /* Variant [`Start_only] */
static value var_ANCHORED;     /* Variant [`ANCHORED] */
static value var_Char;         /* Variant [`Char char] */
static value var_Not_studied;  /* Variant [`Not_studied] */
static value var_Studied;      /* Variant [`Studied] */
static value var_Optimal;      /* Variant [`Optimal] */

static value None = Val_int(0);

/* Data associated with OCaml values of PCRE regular expression */
struct pcre_ocaml_regexp { pcre *rex; pcre_extra *extra; int studied; };

#define Pcre_ocaml_regexp_val(v) \
  ((struct pcre_ocaml_regexp *) Data_custom_val(v))

#define get_rex(v) Pcre_ocaml_regexp_val(v)->rex
#define get_extra(v) Pcre_ocaml_regexp_val(v)->extra
#define get_studied(v) Pcre_ocaml_regexp_val(v)->studied

#define set_rex(v, r) Pcre_ocaml_regexp_val(v)->rex = r
#define set_extra(v, e) Pcre_ocaml_regexp_val(v)->extra = e
#define set_studied(v, s) Pcre_ocaml_regexp_val(v)->studied = s

/* Data associated with OCaml values of PCRE tables */
struct pcre_ocaml_tables { chartables tables; };

#define Pcre_ocaml_tables_val(v) \
  ((struct pcre_ocaml_tables *) Data_custom_val(v))

#define get_tables(v) Pcre_ocaml_tables_val(v)->tables
#define set_tables(v, t) Pcre_ocaml_tables_val(v)->tables = t

/* Converts subject offsets from C-integers to OCaml-Integers.

   This is a bit tricky, because there are 32- and 64-bit platforms around
   and OCaml chooses the larger possibility for representing integers when
   available (also in arrays) - not so the PCRE!
*/
static inline void copy_ovector(
  long subj_start, const int *ovec_src, caml_int_ptr ovec_dst, int subgroups2)
{
  if (subj_start == 0)
    while (subgroups2--) {
      *ovec_dst = Val_int(*ovec_src);
      --ovec_src; --ovec_dst;
    }
  else
    while (subgroups2--) {
      *ovec_dst = Val_long(*ovec_src + subj_start);
      --ovec_src; --ovec_dst;
    }
}

/* Callout handler */
static int pcre_callout_handler(pcre_callout_block* cb)
{
  struct cod *cod = (struct cod *) cb->callout_data;

  if (cod != NULL) {
    /* Callout is available */
    value v_res;

    /* Set up parameter array */
    value v_callout_data = caml_alloc_small(8, 0);

    const value v_substrings = *cod->v_substrings_p;

    const int capture_top = cb->capture_top;
    int subgroups2 = capture_top << 1;
    const int subgroups2_1 = subgroups2 - 1;

    const int *ovec_src = cb->offset_vector + subgroups2_1;
    caml_int_ptr ovec_dst = &Field(Field(v_substrings, 1), 0) + subgroups2_1;
    long subj_start = cod->subj_start;

    copy_ovector(subj_start, ovec_src, ovec_dst, subgroups2);

    Field(v_callout_data, 0) = Val_int(cb->callout_number);
    Field(v_callout_data, 1) = v_substrings;
    Field(v_callout_data, 2) = Val_int(cb->start_match + subj_start);
    Field(v_callout_data, 3) = Val_int(cb->current_position + subj_start);
    Field(v_callout_data, 4) = Val_int(capture_top);
    Field(v_callout_data, 5) = Val_int(cb->capture_last);
    Field(v_callout_data, 6) = Val_int(cb->pattern_position);
    Field(v_callout_data, 7) = Val_int(cb->next_item_length);

    /* Perform callout */
    v_res = caml_callback_exn(*cod->v_cof_p, v_callout_data);

    if (Is_exception_result(v_res)) {
      /* Callout raised an exception */
      const value v_exn = Extract_exception(v_res);
      if (Field(v_exn, 0) == *pcre_exc_Backtrack) return 1;
      cod->v_exn = v_exn;
      return PCRE_ERROR_CALLOUT;
    }
  }

  return 0;
}

/* Fetchs the named OCaml-values + caches them and
   calculates + caches the variant hash values */
CAMLprim value pcre_ocaml_init(value __unused v_unit)
{
  pcre_exc_Error     = caml_named_value("Pcre.Error");
  pcre_exc_Backtrack = caml_named_value("Pcre.Backtrack");

  var_Start_only  = caml_hash_variant("Start_only");
  var_ANCHORED    = caml_hash_variant("ANCHORED");
  var_Char        = caml_hash_variant("Char");
  var_Not_studied = caml_hash_variant("Not_studied");
  var_Studied     = caml_hash_variant("Studied");
  var_Optimal     = caml_hash_variant("Optimal");

  pcre_callout = &pcre_callout_handler;

  return Val_unit;
}

/* Finalizing deallocation function for chartable sets */
static void pcre_dealloc_tables(value v_tables)
{ (pcre_free)((void *) get_tables(v_tables)); }

/* Finalizing deallocation function for compiled regular expressions */
static void pcre_dealloc_regexp(value v_rex)
{
  void *extra = get_extra(v_rex);
  if (extra != NULL)
#ifdef PCRE_STUDY_JIT_COMPILE
    pcre_free_study(extra);
#else
    pcre_free(extra);
#endif
  (pcre_free)(get_rex(v_rex));
}


/* Raising exceptions */

CAMLnoreturn_start
static inline void raise_pcre_error(value v_arg)
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_partial()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_bad_partial()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_bad_utf8()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_bad_utf8_offset()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_match_limit()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_recursion_limit()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_workspace_size()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_bad_pattern(const char *msg, int pos)
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_internal_error(char *msg)
CAMLnoreturn_end;

static inline void raise_pcre_error(value v_arg)
{ caml_raise_with_arg(*pcre_exc_Error, v_arg); }

static inline void raise_partial() { raise_pcre_error(Val_int(0)); }
static inline void raise_bad_partial() { raise_pcre_error(Val_int(1)); }
static inline void raise_bad_utf8() { raise_pcre_error(Val_int(2)); }
static inline void raise_bad_utf8_offset() { raise_pcre_error(Val_int(3)); }
static inline void raise_match_limit() { raise_pcre_error(Val_int(4)); }
static inline void raise_recursion_limit() { raise_pcre_error(Val_int(5)); }
static inline void raise_workspace_size() { raise_pcre_error(Val_int(6)); }

static inline void raise_bad_pattern(const char *msg, int pos)
{
  CAMLparam0();
  CAMLlocal1(v_msg);
  value v_arg;
  v_msg = caml_copy_string(msg);
  v_arg = caml_alloc_small(2, 0);
  Field(v_arg, 0) = v_msg;
  Field(v_arg, 1) = Val_int(pos);
  raise_pcre_error(v_arg);
  CAMLnoreturn;
}

static inline void raise_internal_error(char *msg)
{
  CAMLparam0();
  CAMLlocal1(v_msg);
  value v_arg;
  v_msg = caml_copy_string(msg);
  v_arg = caml_alloc_small(1, 1);
  Field(v_arg, 0) = v_msg;
  raise_pcre_error(v_arg);
  CAMLnoreturn;
}

/* PCRE pattern compilation */

static struct custom_operations regexp_ops = {
  "pcre_ocaml_regexp",
  pcre_dealloc_regexp,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* Makes compiled regular expression from compilation options, an optional
   value of chartables and the pattern string */

CAMLprim value pcre_compile_stub(intnat v_opt, value v_tables, value v_pat)
{
  value v_rex;  /* Final result -> value of type [regexp] */
  size_t regexp_size, ocaml_regexp_size = sizeof(struct pcre_ocaml_regexp);
  const char *error = NULL;  /* pointer to possible error message */
  int error_ofs = 0;  /* offset in the pattern at which error occurred */

  /* If v_tables = [None], then pointer to tables is NULL, otherwise
     set it to the appropriate value */
  chartables tables =
    (v_tables == None) ? NULL : get_tables(Field(v_tables, 0));

  /* Compiles the pattern */
  pcre *regexp = pcre_compile(String_val(v_pat), v_opt, &error,
                              &error_ofs, tables);

  /* Raises appropriate exception with [BadPattern] if the pattern
     could not be compiled */
  if (regexp == NULL) raise_bad_pattern(error, error_ofs);

  /* It's unknown at this point whether the user will study the pattern
     later (probably), or if JIT compilation is going to be used, but we
     have to decide on a size.  Tests with some simple patterns indicate a
     roughly 50% increase in size when studying without JIT.  A factor of
     two times hence seems like a reasonable bound to use here. */
  pcre_fullinfo(regexp, NULL, PCRE_INFO_SIZE, &regexp_size);
  v_rex = caml_alloc_custom_mem(&regexp_ops, ocaml_regexp_size, 2*regexp_size);

  set_rex(v_rex, regexp);
  set_extra(v_rex, NULL);
  set_studied(v_rex, 0);

  return v_rex;
}

CAMLprim value pcre_compile_stub_bc(value v_opt, value v_tables, value v_pat)
{
  return pcre_compile_stub(Int_val(v_opt), v_tables, v_pat);
}


/* Studies a regexp */
CAMLprim value pcre_study_stub(value v_rex)
{
  /* If it has not yet been studied */
  if (! get_studied(v_rex)) {
    const char *error = NULL;
    pcre_extra *extra = pcre_study(get_rex(v_rex), 0, &error);
    if (error != NULL) caml_invalid_argument((char *) error);
    set_extra(v_rex, extra);
    set_studied(v_rex, 1);
  }
  return v_rex;
}


/* Gets the match limit recursion of a regular expression if it exists */
CAMLprim value pcre_get_match_limit_recursion_stub(value v_rex)
{
  pcre_extra *extra = get_extra(v_rex);
  if (extra == NULL) return None;
  if (extra->flags & PCRE_EXTRA_MATCH_LIMIT_RECURSION) {
    value v_lim = Val_int(extra->match_limit_recursion);
    value v_res = caml_alloc_small(1, 0);
    Field(v_res, 0) = v_lim;
    return v_res;
  }
  return None;
}

/* Gets the match limit of a regular expression if it exists */
CAMLprim value pcre_get_match_limit_stub(value v_rex)
{
  pcre_extra *extra = get_extra(v_rex);
  if (extra == NULL) return None;
  if (extra->flags & PCRE_EXTRA_MATCH_LIMIT) {
    value v_lim = Val_int(extra->match_limit);
    value v_res = caml_alloc_small(1, 0);
    Field(v_res, 0) = v_lim;
    return v_res;
  }
  return None;
}


/* Sets a match limit for a regular expression imperatively */

CAMLprim value pcre_set_imp_match_limit_stub(value v_rex, intnat v_lim)
{
  pcre_extra *extra = get_extra(v_rex);
  if (extra == NULL) {
    extra = pcre_malloc(sizeof(pcre_extra));
    extra->flags = PCRE_EXTRA_MATCH_LIMIT;
    set_extra(v_rex, extra);
  } else {
    unsigned long *flags_ptr = &extra->flags;
    *flags_ptr = PCRE_EXTRA_MATCH_LIMIT | *flags_ptr;
  }
  extra->match_limit = v_lim;
  return v_rex;
}

CAMLprim value pcre_set_imp_match_limit_stub_bc(value v_rex, value v_lim)
{
  return pcre_set_imp_match_limit_stub(v_rex, Int_val(v_lim));
}


/* Sets a match limit recursion for a regular expression imperatively */

CAMLprim value pcre_set_imp_match_limit_recursion_stub(
    value v_rex, intnat v_lim)
{
  pcre_extra *extra = get_extra(v_rex);
  if (extra == NULL) {
    extra = pcre_malloc(sizeof(pcre_extra));
    extra->flags = PCRE_EXTRA_MATCH_LIMIT_RECURSION;
    set_extra(v_rex, extra);
  } else {
    unsigned long *flags_ptr = &extra->flags;
    *flags_ptr = PCRE_EXTRA_MATCH_LIMIT_RECURSION | *flags_ptr;
  }
  extra->match_limit_recursion = v_lim;
  return v_rex;
}

CAMLprim value pcre_set_imp_match_limit_recursion_stub_bc(
    value v_rex, value v_lim)
{
  return pcre_set_imp_match_limit_recursion_stub(v_rex, Int_val(v_lim));
}


/* Performs the call to the pcre_fullinfo function */
static inline int pcre_fullinfo_stub(value v_rex, int what, void *where)
{
  return pcre_fullinfo(get_rex(v_rex), get_extra(v_rex), what, where);
}

/* Some stubs for info-functions */

/* Generic macro for getting integer results from pcre_fullinfo */
#define make_intnat_info(tp, name, option) \
  CAMLprim intnat pcre_##name##_stub(value v_rex) \
  { \
    tp options; \
    const int ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_##option, &options); \
    if (ret != 0) raise_internal_error("pcre_##name##_stub"); \
    return options; \
  } \
  \
  CAMLprim value pcre_##name##_stub_bc(value v_rex) \
  { return Val_int(pcre_##name##_stub(v_rex)); }

make_intnat_info(unsigned long, options, OPTIONS)
make_intnat_info(size_t, size, SIZE)
make_intnat_info(size_t, studysize, STUDYSIZE)
make_intnat_info(int, capturecount, CAPTURECOUNT)
make_intnat_info(int, backrefmax, BACKREFMAX)
make_intnat_info(int, namecount, NAMECOUNT)
make_intnat_info(int, nameentrysize, NAMEENTRYSIZE)

CAMLprim value pcre_firstbyte_stub(value v_rex)
{
  int firstbyte;
  const int ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_FIRSTBYTE, &firstbyte);

  if (ret != 0) raise_internal_error("pcre_firstbyte_stub");

  switch (firstbyte) {
    case -1 : return var_Start_only; break;  /* [`Start_only] */
    case -2 : return var_ANCHORED; break;    /* [`ANCHORED] */
    default :
      if (firstbyte < 0 )  /* Should not happen */
        raise_internal_error("pcre_firstbyte_stub");
      else {
        value v_firstbyte;
        /* Allocates the non-constant constructor [`Char of char] and fills
           in the appropriate value */
        v_firstbyte = caml_alloc_small(2, 0);
        Field(v_firstbyte, 0) = var_Char;
        Field(v_firstbyte, 1) = Val_int(firstbyte);
        return v_firstbyte;
      }
  }
}

CAMLprim value pcre_firsttable_stub(value v_rex)
{
  const unsigned char *ftable;

  int ret =
    pcre_fullinfo_stub(v_rex, PCRE_INFO_FIRSTTABLE, (void *) &ftable);

  if (ret != 0) raise_internal_error("pcre_firsttable_stub");

  if (ftable == NULL) return None;
  else {
    value v_res, v_res_str;
    char *ptr;
    int i;

    Begin_roots1(v_rex);
      v_res_str = caml_alloc_string(32);
    End_roots();

    ptr = String_val(v_res_str);
    for (i = 0; i <= 31; ++i) { *ptr = *ftable; ++ptr; ++ftable; }

    Begin_roots1(v_res_str);
      /* Allocates [Some string] from firsttable */
      v_res = caml_alloc_small(1, 0);
    End_roots();

    Field(v_res, 0) = v_res_str;

    return v_res;
  }
}

CAMLprim value pcre_lastliteral_stub(value v_rex)
{
  int lastliteral;
  const int ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_LASTLITERAL,
                                        &lastliteral);

  if (ret != 0) raise_internal_error("pcre_lastliteral_stub");

  if (lastliteral == -1) return None;
  if (lastliteral < 0) raise_internal_error("pcre_lastliteral_stub");
  else {
    /* Allocates [Some char] */
    value v_res = caml_alloc_small(1, 0);
    Field(v_res, 0) = Val_int(lastliteral);
    return v_res;
  }
}

CAMLprim value pcre_study_stat_stub(value v_rex)
{
  /* Generates the appropriate constant constructor [`Optimal] or
     [`Studied] if regexp has already been studied */
  if (get_studied(v_rex))
    return (get_extra(v_rex) == NULL) ? var_Optimal : var_Studied;

  return var_Not_studied;  /* otherwise [`Not_studied] */
}

CAMLnoreturn_start
static inline void handle_exec_error(char *loc, const int ret)
CAMLnoreturn_end;

static inline void handle_exec_error(char *loc, const int ret)
{
  switch (ret) {
    /* Dedicated exceptions */
    case PCRE_ERROR_NOMATCH : caml_raise_not_found();
    case PCRE_ERROR_PARTIAL : raise_partial();
    case PCRE_ERROR_MATCHLIMIT : raise_match_limit();
    case PCRE_ERROR_BADPARTIAL : raise_bad_partial();
    case PCRE_ERROR_BADUTF8 : raise_bad_utf8();
    case PCRE_ERROR_BADUTF8_OFFSET : raise_bad_utf8_offset();
    case PCRE_ERROR_RECURSIONLIMIT : raise_recursion_limit();
    case PCRE_ERROR_DFA_WSSIZE : raise_workspace_size();
    /* Unknown error */
    default : {
      char err_buf[100];
      snprintf(err_buf, 100, "%s: unhandled PCRE error code: %d", loc, ret);
      raise_internal_error(err_buf);
    }
  }
}

static inline void handle_pcre_exec_result(
  int *ovec, value v_ovec, long ovec_len, long subj_start, int ret)
{
  caml_int_ptr ocaml_ovec = (caml_int_ptr) &Field(v_ovec, 0);
  const int subgroups2 = ret * 2;
  const int subgroups2_1 = subgroups2 - 1;
  const int *ovec_src = ovec + subgroups2_1;
  caml_int_ptr ovec_clear_stop = ocaml_ovec + (ovec_len * 2) / 3;
  caml_int_ptr ovec_dst = ocaml_ovec + subgroups2_1;
  copy_ovector(subj_start, ovec_src, ovec_dst, subgroups2);
  while (++ovec_dst < ovec_clear_stop) *ovec_dst = -1;
}

/* Executes a pattern match with runtime options, a regular expression, a
   matching position, the start of the the subject string, a subject string,
   a number of subgroup offsets, an offset vector and an optional callout
   function */

CAMLprim value pcre_exec_stub0(
    intnat v_opt, value v_rex, intnat v_pos, intnat v_subj_start, value v_subj,
    value v_ovec, value v_maybe_cof, value v_workspace)
{
  int ret;
  int is_dfa = v_workspace != (value) NULL;
  long
    pos = v_pos,
    len = caml_string_length(v_subj),
    subj_start = v_subj_start;
  long ovec_len = Wosize_val(v_ovec);

  if (pos > len || pos < subj_start)
    caml_invalid_argument("Pcre.pcre_exec_stub: illegal position");

  if (subj_start > len || subj_start < 0)
    caml_invalid_argument("Pcre.pcre_exec_stub: illegal subject start");

  pos -= subj_start;
  len -= subj_start;

  {
    const pcre *code = get_rex(v_rex);  /* Compiled pattern */
    const pcre_extra *extra = get_extra(v_rex);  /* Extra info */
    const char *ocaml_subj =
      String_val(v_subj) + subj_start;  /* Subject string */
    const int opt = v_opt;  /* Runtime options */

    /* Special case when no callout functions specified */
    if (v_maybe_cof == None) {
      int *ovec = (int *) &Field(v_ovec, 0);

      /* Performs the match */
      if (is_dfa)
        ret =
          pcre_dfa_exec(code, extra, ocaml_subj, len, pos, opt, ovec, ovec_len,
              (int *) &Field(v_workspace, 0), Wosize_val(v_workspace));
      else
        ret = pcre_exec(code, extra, ocaml_subj, len, pos, opt, ovec, ovec_len);

      if (ret < 0) handle_exec_error("pcre_exec_stub", ret);
      else handle_pcre_exec_result(ovec, v_ovec, ovec_len, subj_start, ret);
    }

    /* There are callout functions */
    else {
      value v_cof = Field(v_maybe_cof, 0);
      value v_substrings;
      char *subj = caml_stat_alloc(sizeof(char) * len);
      int *ovec = caml_stat_alloc(sizeof(int) * ovec_len);
      int workspace_len;
      int *workspace;
      struct cod cod = { 0, (value *) NULL, (value *) NULL, (value) NULL };
      struct pcre_extra new_extra =
#ifdef PCRE_EXTRA_MATCH_LIMIT_RECURSION
# ifdef PCRE_EXTRA_MARK
#  ifdef PCRE_EXTRA_EXECUTABLE_JIT
        { PCRE_EXTRA_CALLOUT_DATA, NULL, 0, NULL, NULL, 0, NULL, NULL };
#  else
        { PCRE_EXTRA_CALLOUT_DATA, NULL, 0, NULL, NULL, 0, NULL };
#  endif
# else
        { PCRE_EXTRA_CALLOUT_DATA, NULL, 0, NULL, NULL, 0 };
# endif
#else
        { PCRE_EXTRA_CALLOUT_DATA, NULL, 0, NULL, NULL };
#endif

      cod.subj_start = subj_start;
      memcpy(subj, ocaml_subj, len);

      Begin_roots4(v_rex, v_cof, v_substrings, v_ovec);
        Begin_roots1(v_subj);
          v_substrings = caml_alloc_small(2, 0);
        End_roots();

        Field(v_substrings, 0) = v_subj;
        Field(v_substrings, 1) = v_ovec;

        cod.v_substrings_p = &v_substrings;
        cod.v_cof_p = &v_cof;
        new_extra.callout_data = &cod;

        if (extra != NULL) {
          new_extra.flags = PCRE_EXTRA_CALLOUT_DATA | extra->flags;
          new_extra.study_data = extra->study_data;
          new_extra.match_limit = extra->match_limit;
          new_extra.tables = extra->tables;
#ifdef PCRE_EXTRA_MATCH_LIMIT_RECURSION
          new_extra.match_limit_recursion = extra->match_limit_recursion;
#endif
        }

        if (is_dfa) {
          workspace_len = Wosize_val(v_workspace);
          workspace = caml_stat_alloc(sizeof(int) * workspace_len);
          ret =
            pcre_dfa_exec(code, extra, subj, len, pos, opt, ovec, ovec_len,
                (int *) &Field(v_workspace, 0), workspace_len);
        } else
          ret =
            pcre_exec(code, &new_extra, subj, len, pos, opt, ovec, ovec_len);

        caml_stat_free(subj);
      End_roots();

      if (ret < 0) {
        if (is_dfa) caml_stat_free(workspace);
        caml_stat_free(ovec);
        if (ret == PCRE_ERROR_CALLOUT) caml_raise(cod.v_exn);
        else handle_exec_error("pcre_exec_stub(callout)", ret);
      } else {
        handle_pcre_exec_result(ovec, v_ovec, ovec_len, subj_start, ret);
        if (is_dfa) {
          caml_int_ptr ocaml_workspace_dst =
            (caml_int_ptr) &Field(v_workspace, 0);
          const int *workspace_src = workspace;
          const int *workspace_src_stop = workspace + workspace_len;
          while (workspace_src != workspace_src_stop) {
            *ocaml_workspace_dst = *workspace_src;
            ocaml_workspace_dst++;
            workspace_src++;
          }
          caml_stat_free(workspace);
        }
        caml_stat_free(ovec);
      }
    }
  }

  return Val_unit;
}

CAMLprim value pcre_exec_stub(
    intnat v_opt, value v_rex, intnat v_pos, intnat v_subj_start, value v_subj,
    value v_ovec, value v_maybe_cof)
{
  return pcre_exec_stub0(v_opt, v_rex, v_pos, v_subj_start, v_subj,
                         v_ovec, v_maybe_cof, (value) NULL);
}

/* Byte-code hook for pcre_exec_stub
   Needed, because there are more than 5 arguments */
CAMLprim value pcre_exec_stub_bc(value *argv, int __unused argn)
{
  return
    pcre_exec_stub0(
        Int_val(argv[0]), argv[1], Int_val(argv[2]), Int_val(argv[3]),
        argv[4], argv[5], argv[6], (value) NULL);
}

/* Byte-code hook for pcre_dfa_exec_stub
   Needed, because there are more than 5 arguments */
CAMLprim value pcre_dfa_exec_stub_bc(value *argv, int __unused argn)
{
  return
    pcre_exec_stub0(
        Int_val(argv[0]), argv[1], Int_val(argv[2]), Int_val(argv[3]),
        argv[4], argv[5], argv[6], argv[7]);
}

static struct custom_operations tables_ops = {
  "pcre_ocaml_tables",
  pcre_dealloc_tables,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* Generates a new set of chartables for the current locale (see man
   page of PCRE */
CAMLprim value pcre_maketables_stub(value __unused v_unit)
{
  /* According to testing with `malloc_size`, it seems that a typical set of
     tables will require about 1536 bytes of memory.  This may or may not
     be true on other platforms or for all versions of PCRE.  Since there
     is apparently no reliable way of finding out, 1536 is probably a good
     default value. */
  size_t tables_size = sizeof(struct pcre_ocaml_tables);
  const value v_tables = caml_alloc_custom_mem(&tables_ops, tables_size, 1536);
  set_tables(v_tables, pcre_maketables());
  return v_tables;
}

/* Wraps around the isspace-function */
CAMLprim value pcre_isspace_stub(value v_c)
{
  return Val_bool(isspace(Int_val(v_c)));
}


/* Returns number of substring associated with a name */

CAMLprim intnat pcre_get_stringnumber_stub(value v_rex, value v_name)
{
  const int ret = pcre_get_stringnumber(get_rex(v_rex), String_val(v_name));
  if (ret == PCRE_ERROR_NOSUBSTRING)
    caml_invalid_argument("Named string not found");

  return ret;
}

CAMLprim value pcre_get_stringnumber_stub_bc(value v_rex, value v_name)
{
  return Val_int(pcre_get_stringnumber_stub(v_rex, v_name));
}


/* Returns array of names of named substrings in a regexp */
CAMLprim value pcre_names_stub(value v_rex)
{
  CAMLparam0();
  CAMLlocal1(v_res);
  int name_count;
  int entry_size;
  const char *tbl_ptr;
  int i;

  int ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_NAMECOUNT, &name_count);
  if (ret != 0) raise_internal_error("pcre_names_stub: namecount");

  ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_NAMEENTRYSIZE, &entry_size);
  if (ret != 0) raise_internal_error("pcre_names_stub: nameentrysize");

  ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_NAMETABLE, &tbl_ptr);
  if (ret != 0) raise_internal_error("pcre_names_stub: nametable");

  v_res = caml_alloc(name_count, 0);

  for (i = 0; i < name_count; ++i) {
    value v_name = caml_copy_string(tbl_ptr + 2);
    Store_field(v_res, i, v_name);
    tbl_ptr += entry_size;
  }

  CAMLreturn(v_res);
}

/* Generic stub for getting integer results from pcre_config */
static inline int pcre_config_int(int what)
{
  int ret;
  pcre_config(what, (void *) &ret);
  return ret;
}

/* Generic stub for getting long integer results from pcre_config */
static inline int pcre_config_long(int what)
{
  long ret;
  pcre_config(what, (void *) &ret);
  return ret;
}


/* Some stubs for config-functions */

/* Makes OCaml-string from PCRE-version */
CAMLprim value pcre_version_stub(value __unused v_unit)
{
  return caml_copy_string((char *) pcre_version());
}

/* Returns boolean indicating UTF8-support */
CAMLprim value pcre_config_utf8_stub(value __unused v_unit)
{ return Val_bool(pcre_config_int(PCRE_CONFIG_UTF8)); }


/* Returns character used as newline */
CAMLprim value pcre_config_newline_stub(value __unused v_unit)
{ return Val_int(pcre_config_int(PCRE_CONFIG_NEWLINE)); }


/* Returns number of bytes used for internal linkage of regular expressions */

CAMLprim intnat pcre_config_link_size_stub(value __unused v_unit)
{ return pcre_config_int(PCRE_CONFIG_LINK_SIZE); }

CAMLprim value pcre_config_link_size_stub_bc(value v_unit)
{ return Val_int(pcre_config_link_size_stub(v_unit)); }


/* Returns default limit for calls to internal matching function */

CAMLprim intnat pcre_config_match_limit_stub(value __unused v_unit)
{ return pcre_config_long(PCRE_CONFIG_MATCH_LIMIT); }


CAMLprim value pcre_config_match_limit_stub_bc(value v_unit)
{ return Val_int(pcre_config_match_limit_stub(v_unit)); }


/* Returns default limit for recursive calls to internal matching function */

CAMLprim intnat pcre_config_match_limit_recursion_stub(value __unused v_unit)
{ return pcre_config_long(PCRE_CONFIG_MATCH_LIMIT_RECURSION); }

CAMLprim value pcre_config_match_limit_recursion_stub_bc(value v_unit)
{ return Val_int(pcre_config_match_limit_recursion_stub(v_unit)); }


/* Returns boolean indicating use of stack recursion */
CAMLprim value pcre_config_stackrecurse_stub(value __unused v_unit)
{ return Val_bool(pcre_config_int(PCRE_CONFIG_STACKRECURSE)); }
