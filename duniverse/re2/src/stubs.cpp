#include <vector>
#include "re2/re2.h"
#include "re2/set.h"
//#define DEBUG
#ifdef DEBUG
#include <iostream>
#endif
#include "stubs.h"

uint16_t bitfield_of_options(const RE2::Options& o) {
  uint16_t options = 0;
  int pos = 15;
#define PACK(V) if (V) { options |= (uint16_t) (1 << pos); } --pos;
#define X(_u,FIRST,REST,_uu) PACK(o.FIRST##REST())
#define X__MAXMEM(_u,_uu,_uuu,_uuuu)
#define X__ENCODING(_u,FIRST,REST,_uu,_uuu,TRANSLATION,_uuuu)  PACK(o.FIRST##REST() TRANSLATION)
#include "enum_x_macro.h"
#undef PACK
  return options;
}

void options_of_bitfield(uint16_t option_bits, RE2::Options& o) {
  int pos = 15;
#define UNPACK(SETTOR,TRANSLATED) SETTOR((option_bits & (0x1 << pos--)) TRANSLATED);
#define X(_u,FIRST,REST,_uu) UNPACK(o.set_##FIRST##REST,)
#define X__MAXMEM(_u,_uu,_uuu,_uuuu)
#define X__ENCODING(_u,FIRST,REST,_uu,_uuu,_uuuu,TRANSLATION) UNPACK(o.set_##FIRST##REST,TRANSLATION)
#include "enum_x_macro.h"
#undef UNPACK
}

template <typename T> inline int compare(T a, T b) {
  if (a == b) return 0;
  else return a < b ? -1 : 1;
}

int compare_options(const RE2::RE2::Options& A, const RE2::RE2::Options& B) {
  int retval = 0;
  uint16_t a = bitfield_of_options(A);
  uint16_t b = bitfield_of_options(B);

  if ((retval = compare<uint16_t>(a, b))) return retval;
  else return compare<int64_t>(A.max_mem(), B.max_mem());
}

using namespace re2;

extern "C" {

#include <limits.h>
#include <assert.h>
#include <arpa/inet.h>
#include <stdarg.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/misc.h>
#include <caml/intext.h>
#include <caml/custom.h>

#include "util.h"

  void mlre2__custom_regex_finalize(value v_obj) {
    delete Regex_val(v_obj);
  }

  int mlre2__custom_regex_compare(value v1, value v2) {
    int retval = 0;
    RE2 *re1 = Regex_val(v1), *re2 = Regex_val(v2);
#ifdef DEBUG
    assert(re1);
    assert(re2);
#endif
    if ((retval = re1->pattern().compare(re2->pattern()))) {
      return retval;
    } else return compare_options(re1->options(), re2->options());
  }

  long mlre2__custom_regex_hash(value v) {
    /* invariant: if compare(a, b) == 0 then hash(a) == hash(b)

       Since we ignore the options, two regexes with the same pattern
       and different options will deterministically collide in
       hash-space, which may (negligibly) decrease performance but
       will not affect correctness.

       This is hash_aux in hash.c, specialized for strings.
    */
#define Beta 19
    long hash = 0;
    const char *s = Regex_val(v)->pattern().c_str();
    const char *end = s + Regex_val(v)->pattern().length();
    for (/* empty */; s < end; ++s) hash = hash * Beta + *s;
    return hash;
#undef Beta
  }

  /* The serialization format:
     (4 bytes)   length of pattern, int
     (n bytes)   pattern, string
     (8 bytes)   max_mem, int64
     (2 bytes)   all other options, bit array
     where all other options has the format:
     (13 bits) bool_array_of_options, one bit per element
     (3  bits) zero
  */

  void mlre2__custom_regex_serialize(value v, unsigned long * wsize_32,
                                     unsigned long * wsize_64) {
    RE2 *re = Regex_val(v);
    size_t len = re->pattern().length() + 1;
    if (len > INT_MAX) {
      caml_failwith("cannot serialize regexes with patterns longer than INT_MAX");
    }
    caml_serialize_int_4((signed int) len);
    caml_serialize_block_1((char *) re->pattern().c_str(), len);
    caml_serialize_int_8(re->options().max_mem());
    caml_serialize_int_2(bitfield_of_options(re->options()));
#ifdef DEBUG
    std::cerr << "serialized regex /" << Regex_val(v)->pattern() << "/ (length "
      << len << ")" << std::endl;
#endif
    *wsize_32 = 4;
    *wsize_64 = 8;
  }

  unsigned long mlre2__custom_regex_deserialize(void * dst) {
    int len = caml_deserialize_sint_4();
    RE2::Options options;
    char * pattern = (char *) caml_stat_alloc(sizeof(*pattern) * (len));
    caml_deserialize_block_1(pattern, len);
    pattern[len - 1] = '\0';
    options.Copy(RE2::Quiet);
    options.set_max_mem(caml_deserialize_sint_8());
    options_of_bitfield((uint16_t) caml_deserialize_uint_2(), options);
#ifdef DEBUG
    std::cerr << "deserialized regex /" << pattern << "/" << std::endl;
#endif
    *(RE2 **) dst = new RE2(pattern, options);
    caml_stat_free(pattern);
    return sizeof(RE2 *);
  }

  /* 2017-04-01: It was a mistake to implement marshal. We don't use marshal
     internally, and will not add additional marshal support in the future. */

  struct custom_operations mlre2__custom_regex_ops = {
    (char *)"com.janestreet.re2-ocaml.regex-v20Mar2014",
    mlre2__custom_regex_finalize,
    mlre2__custom_regex_compare,
    mlre2__custom_regex_hash,
    mlre2__custom_regex_serialize,
    mlre2__custom_regex_deserialize,
#ifdef custom_compare_ext_default
    custom_compare_ext_default
#endif
  };

  void mlre2__custom_regex_multiple_finalize(value v_obj) {
    delete RegexSet_val(v_obj);
  }

  struct custom_operations mlre2__custom_regex_multiple_ops = {
    (char *)"com.janestreet.re2-ocaml.regex-set-v31Mar2017",
    mlre2__custom_regex_multiple_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
#ifdef custom_compare_ext_default
    custom_compare_ext_default
#endif
  };

  void mlre2__custom_regex_options_finalize(value v_obj) {
    delete RegexOptions_val(v_obj);
  }

  struct custom_operations mlre2__custom_regex_options_ops = {
    (char *)"com.janestreet.re2-ocaml.regex-options-v23Oct2018",
    mlre2__custom_regex_options_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
#ifdef custom_compare_ext_default
    custom_compare_ext_default
#endif
  };

  CAMLprim value mlre2__init(value unit) {
    (void) unit;
    caml_register_custom_operations(&mlre2__custom_regex_ops);
    caml_register_custom_operations(&mlre2__custom_regex_multiple_ops);
    caml_register_custom_operations(&mlre2__custom_regex_options_ops);
    return Val_unit;
  }

  static int new_pos(const char *input, StringPiece &remaining, int startpos, StringPiece &match) {
    if (remaining.length() < 0) {
      return -1;
    } else {
      /* casting these size_t's to int is safe because StringPiece's track
       * their lengths using ints */
      size_t first_unexamined = remaining.data() + startpos - input;
      size_t first_unmatched = match.data() - input + match.length();
      return (int) (first_unexamined > first_unmatched ? first_unexamined : first_unmatched);
    }
  }

  /* this is to ensure we don't return the same zero-length match forever */
  static int ensure_progress(StringPiece &str, const StringPiece &match) {
    static RE2 re(".");
    if (match.length() > 0) {
      return match.length();
    } else if (str.length() > 0) {
      StringPiece str_copy = str;
      /* Drops one character from the front of the StringPiece.
         Implemented using a regex call because that's the easiest way
         to handle multibyte Unicode characters. */
      RE2::Consume(&str_copy, re);
      return (int) (str_copy.data() - str.data());
    } else return 1; /* we halt on negative length strings, so this value is arbitrary */
  }

  static void assert_valid_sub(const RE2 *re, value v_sub) {
    CAMLparam1(v_sub);
    CAMLlocalN(error_args, 2);

    if (Int_val(v_sub) < 0 || re->NumberOfCapturingGroups() < Int_val(v_sub)) {
      error_args[0] = v_sub;
      error_args[1] = Val_int(re->NumberOfCapturingGroups() + 1);
      caml_raise_with_args(*caml_named_value("mlre2__Regex_no_such_subpattern"),
          2, error_args);
    }

    CAMLreturn0;
  }

  /* returns (cre2__obj_t * int * (string * int) list) where
   * - cre2__obj_t is the ML-side name for a custom_block with a struct regex *
   * - int is the number of submatches, including the whole match
   * - (string * int) list is the Map.to_alist of the submatch (name, index) Map.t
   */
  CAMLprim value mlre2__create_re(value v_options, value v_pattern) {
    value v_retval, v_compile_error;
    const char * c_pat = String_val(v_pattern);
    RE2* compiled = NULL;

    compiled = new RE2(c_pat, *RegexOptions_val(v_options));

    if (!compiled->ok()) {
      /* Warning
         from this point on it's no longer safe to access v_options or
         v_pattern as the GC might be invoked from caml_copy_string and
         move those values (as we haven't registered the paramters they
         wouldn't get updated).  This is fine because we don't access
         them before we call caml_raise_with_arg. */
      v_compile_error = caml_copy_string(compiled->error().c_str());
      delete compiled;
      compiled = NULL;
      caml_raise_with_arg(*caml_named_value("mlre2__Regex_compile_failed"),
          v_compile_error);
    }

    v_retval = caml_alloc_custom(&mlre2__custom_regex_ops, sizeof(compiled),
        1024*1024,      /* RE2 object uses ~1MB of memory outside the OCaml heap */
        500*1024*1024);  /* I'm okay with 500MB of RAM being wasted */

    Regex_val(v_retval) = compiled;

    return v_retval;
  }

  CAMLprim value mlre2__num_submatches(value v_regex) {
    return Val_int(Regex_val(v_regex)->NumberOfCapturingGroups() + 1);
  }

  CAMLprim value mlre2__submatch_index(value v_regex, value v_name) {
    std::map<string, int>::const_iterator it =
      Regex_val(v_regex)->NamedCapturingGroups().find(String_val(v_name));
    if (it == Regex_val(v_regex)->NamedCapturingGroups().end()) {
      return Val_int(-1);
    } else return Val_int(it->second);
  }

  CAMLprim value mlre2__pattern(value v_regex) {
    return caml_copy_string(Regex_val(v_regex)->pattern().c_str());
  }

  CAMLprim value mlre2__iter_next(value v_regex, value v_pos,
      value v_max_submatch, value v_input) {
    CAMLparam2(v_regex, v_input);
    CAMLlocal3(v_retval, v_match_array, v_match);
    /* [v_retval] is the return value.
     * [v_match_array] is the array used to return captured substrings
     * [v_match] is the substring captured by a submatch.
     */

    const RE2 * re = Regex_val(v_regex);
    const char * input = String_val(v_input);
    int startpos = Int_val(v_pos);
    int len = caml_string_length(v_input);
    StringPiece str = StringPiece(input, len);
    int max_submatch = Int_val(v_max_submatch) < 0
      ? re->NumberOfCapturingGroups()
      : Int_val(v_max_submatch);
    /* +1 for whole match ("subpattern zero") */
    int n = 1 + (max_submatch > 0 ? max_submatch : 0);
    StringPiece *submatches = new StringPiece[n];
    StringPiece *sub = submatches; /* extra pointer for iterating over [submatches] */

    if (str.length() < startpos
        || ! re->Match(str, startpos, str.length(), RE2::UNANCHORED, submatches, n)) {
      PAIR(v_retval, Val_int(-1), Val_none);
    } else {
      startpos += ensure_progress(str, submatches[0]);
      v_match_array = caml_alloc_tuple(n);
      for (int i = 0; i < n; ++i) {
        sub = submatches + i;
        if (sub->data()) {
          PAIR(v_retval, Val_int((int)(sub->data() - input)), Val_int(sub->length()));
          SOME(v_match, v_retval);
        } else v_match = Val_none;
        Store_field(v_match_array, i, v_match);
      }
      SOME(v_match, v_match_array);
      PAIR(v_retval, Val_int(new_pos(input, str, startpos, submatches[0])), v_match);
    }
    delete[] submatches;
    CAMLreturn(v_retval);
  }

  CAMLprim value mlre2__matches(value v_regex, value v_str) {
    StringPiece str(String_val(v_str), caml_string_length(v_str));
    return Val_int(Regex_val(v_regex)->Match(str, 0, str.length(),
                                             RE2::UNANCHORED, NULL, 0));
  }

  CAMLprim value mlre2__find_all(value v_regex, value v_sub, value v_str) {
    CAMLparam2(v_regex, v_str);
    CAMLlocal3(v_retval, v_car, v_cons);

    std::vector<StringPiece> results;

    const RE2 * re = Regex_val(v_regex);
    const char* input = String_val(v_str);
    int len = caml_string_length(v_str);
    StringPiece str = StringPiece(input, len);
    int n = Int_val(v_sub) + 1;
    int startpos = 0;
    StringPiece * matches = new StringPiece[n];
    StringPiece * sub = matches + Int_val(v_sub);

    assert_valid_sub(re, v_sub);

    while (str.length() > startpos
        && re->Match(str, startpos, str.length(), RE2::UNANCHORED, matches, n)) {
      startpos += ensure_progress(str, matches[0]);
      startpos = new_pos(input, str, startpos, matches[0]);
      /* push_back followed by back-to-front consing gives the correct final order */
      if (sub->data()) {
        results.push_back(*sub);
      }
    }

    if (results.size() <= 0) {
      delete[] matches;
      caml_raise_with_string(*caml_named_value("mlre2__Regex_match_failed"),
          re->pattern().c_str());
    }

    v_retval = Val_emptylist;
    for (std::vector<StringPiece>::reverse_iterator it = results.rbegin(); it != results.rend(); ++it) {
      /* We take a substring of an ocaml string, which caml_alloc_initialized_string
         doesn't handle, because the allocation can move the memory we're trying to copy
         (and wouldn't be enough here, as we do multiple allocations). So stick with
         alloc, compute pointer, blit. */
      v_car = caml_alloc_string(it->length());
      memcpy(Bytes_val(v_car), String_val(v_str) + (it->data() - input), it->length());
      v_cons = caml_alloc_small(2, Tag_cons);
      Field(v_cons, 0) = v_car;
      Field(v_cons, 1) = v_retval;
      v_retval = v_cons;
    }
    delete[] matches;
    CAMLreturn(v_retval);
  }

  CAMLprim value mlre2__find_first(value v_regex, value v_sub, value v_str) {
    CAMLparam2(v_regex, v_str);
    CAMLlocal1(v_retval);
    CAMLlocalN(error_args, 2);

    const RE2 * re = Regex_val(v_regex);
    const char* input = String_val(v_str);
    int len = caml_string_length(v_str);
    StringPiece str = StringPiece(input, len);
    int n = Int_val(v_sub) + 1;
    StringPiece * submatches = new StringPiece[n];

    assert_valid_sub(re, v_sub);

    if (! re->Match(str, 0, str.length(), RE2::UNANCHORED, submatches, n)) {
      delete[] submatches;
      caml_raise_with_string(*caml_named_value("mlre2__Regex_match_failed"),
        re->pattern().c_str());
    }

    StringPiece * sub = submatches + Int_val(v_sub);

    if (!sub->data()) {
      delete[] submatches;
      error_args[0] = caml_copy_string(re->pattern().c_str());
      error_args[1] = v_sub;
      caml_raise_with_args(*caml_named_value("mlre2__Regex_submatch_did_not_capture"),
          2, error_args);
    }

    /* see comment above about why caml_alloc_initialized_string doesn't work */
    v_retval = caml_alloc_string(sub->length());
    memcpy(Bytes_val(v_retval), String_val(v_str) + (sub->data() - input), sub->length());
    delete[] submatches;
    CAMLreturn(v_retval);
  }

  CAMLprim value mlre2__valid_rewrite_template(value v_regex, value v_template) {
    StringPiece rewrite(String_val(v_template), caml_string_length(v_template));
    string error;
    return Val_bool(Regex_val(v_regex)->CheckRewriteString(rewrite, &error));
  }

  CAMLprim value mlre2__rewrite_exn(value v_regex, value v_input, value v_rewrite) {
    CAMLparam3(v_regex, v_input, v_rewrite);
    CAMLlocalN(error_args, 2);

    /* string(const char*) makes a copy, so [tmp] is safe to modify */
    string tmp = String_val(v_input), error;
    const StringPiece rewrite(String_val(v_rewrite), caml_string_length(v_rewrite));
    int num_rewrites = 0;

    if (! Regex_val(v_regex)->CheckRewriteString(rewrite, &error)) {
      error_args[0] = v_rewrite;
      error_args[1] = caml_copy_string(error.c_str());
      caml_raise_with_args(*caml_named_value("mlre2__Regex_rewrite_template_invalid"),
                           2, error_args);
    }
    if ((num_rewrites = RE2::GlobalReplace(&tmp, *Regex_val(v_regex), rewrite))) {
      CAMLreturn(caml_copy_string(tmp.c_str()));
    } else CAMLreturn(v_input);
  }

  CAMLprim value mlre2__escape(value v_str) {
    CAMLparam1(v_str);
    StringPiece str(String_val(v_str), caml_string_length(v_str));
    CAMLreturn(caml_copy_string(RE2::QuoteMeta(str).c_str()));
  }


  CAMLprim value mlre2__multiple_create(value v_options) {
    CAMLparam1(v_options);
    CAMLlocal1(v_retval);

    RE2::Set* set = NULL;

    set = new RE2::Set(*RegexOptions_val(v_options), RE2::UNANCHORED);

    v_retval = caml_alloc_custom(&mlre2__custom_regex_multiple_ops, sizeof(set),
        1024*1024,      /* RE2 object uses ~1MB of memory outside the OCaml heap */
        500*1024*1024);  /* I'm okay with 500MB of RAM being wasted */

    RegexSet_val(v_retval) = set;

    CAMLreturn(v_retval);
  }

  CAMLprim value mlre2__multiple_add(value v_multiple, value v_pattern){
    CAMLparam2(v_multiple, v_pattern);
    CAMLlocal1(v_compile_error);

    const char *pattern_str = String_val(v_pattern);
    RE2::Set* set = RegexSet_val(v_multiple);
    string errstr;
    int idx = set->Add(pattern_str, &errstr);
    if(idx < 0){
      v_compile_error = caml_copy_string(errstr.c_str());
      caml_raise_with_arg(*caml_named_value("mlre2__Regex_compile_failed"),
			  v_compile_error);
    }
    CAMLreturn(Val_int(idx));
  }

  CAMLprim value mlre2__multiple_compile(value v_multiple){
    CAMLparam1(v_multiple);
    CAMLlocal1(v_compile_error);
    RE2::Set *set = RegexSet_val(v_multiple);
    if(!set->Compile()){
      v_compile_error = caml_copy_string("Unknown failure compiling Regex Set");
      caml_raise_with_arg(*caml_named_value("mlre2__Regex_compile_failed"),
			  v_compile_error);
    }
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__multiple_match(value v_multiple, value v_str){
    CAMLparam2(v_multiple, v_str);
    CAMLlocal1(res);
    const char *str = String_val(v_str);
    RE2::Set *set = RegexSet_val(v_multiple);
    std::vector<int> matches;

    set->Match(str, &matches);
    res = caml_alloc_tuple(matches.size());
    for(unsigned int i = 0; i < matches.size(); ++i){
      Store_field(res, i, Val_int(matches[i]));
    }
    CAMLreturn(res);
  }

  /*$ #use "options.cinaps";;
    List.iter all ~f:(fun { name; type_ = { value_of_c; value_to_c; _} } ->
    printf "\n  \
    CAMLprim value mlre2__options__%s(value v_options) {\n    \
    CAMLparam1(v_options);\n    \
    RE2::Options *options = RegexOptions_val(v_options);\n    \
    CAMLreturn(%s(options->%s()));\n  \
    }\n\
    \n  \
    CAMLprim value mlre2__options__set_%s(value v_options, value v_value) {\n    \
    CAMLparam2(v_options, v_value);\n    \
    RE2::Options *options = RegexOptions_val(v_options);\n    \
    options->set_%s(%s(v_value));\n    \
    CAMLreturn(Val_unit);\n  \
    }\n" name value_of_c name name name value_to_c); */
  CAMLprim value mlre2__options__case_sensitive(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_bool(options->case_sensitive()));
  }

  CAMLprim value mlre2__options__set_case_sensitive(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_case_sensitive(Bool_val(v_value));
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__options__dot_nl(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_bool(options->dot_nl()));
  }

  CAMLprim value mlre2__options__set_dot_nl(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_dot_nl(Bool_val(v_value));
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__options__encoding(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_int(options->encoding()));
  }

  CAMLprim value mlre2__options__set_encoding(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_encoding(static_cast<RE2::Options::Encoding>Int_val(v_value));
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__options__literal(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_bool(options->literal()));
  }

  CAMLprim value mlre2__options__set_literal(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_literal(Bool_val(v_value));
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__options__log_errors(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_bool(options->log_errors()));
  }

  CAMLprim value mlre2__options__set_log_errors(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_log_errors(Bool_val(v_value));
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__options__longest_match(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_bool(options->longest_match()));
  }

  CAMLprim value mlre2__options__set_longest_match(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_longest_match(Bool_val(v_value));
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__options__max_mem(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_int(options->max_mem()));
  }

  CAMLprim value mlre2__options__set_max_mem(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_max_mem(Int_val(v_value));
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__options__never_capture(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_bool(options->never_capture()));
  }

  CAMLprim value mlre2__options__set_never_capture(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_never_capture(Bool_val(v_value));
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__options__never_nl(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_bool(options->never_nl()));
  }

  CAMLprim value mlre2__options__set_never_nl(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_never_nl(Bool_val(v_value));
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__options__one_line(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_bool(options->one_line()));
  }

  CAMLprim value mlre2__options__set_one_line(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_one_line(Bool_val(v_value));
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__options__perl_classes(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_bool(options->perl_classes()));
  }

  CAMLprim value mlre2__options__set_perl_classes(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_perl_classes(Bool_val(v_value));
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__options__posix_syntax(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_bool(options->posix_syntax()));
  }

  CAMLprim value mlre2__options__set_posix_syntax(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_posix_syntax(Bool_val(v_value));
    CAMLreturn(Val_unit);
  }

  CAMLprim value mlre2__options__word_boundary(value v_options) {
    CAMLparam1(v_options);
    RE2::Options *options = RegexOptions_val(v_options);
    CAMLreturn(Val_bool(options->word_boundary()));
  }

  CAMLprim value mlre2__options__set_word_boundary(value v_options, value v_value) {
    CAMLparam2(v_options, v_value);
    RE2::Options *options = RegexOptions_val(v_options);
    options->set_word_boundary(Bool_val(v_value));
    CAMLreturn(Val_unit);
  }
/*$*/

  /* The caller is responsible for assigning the returned value into a variable
     registered with [CAMLlocal*]. */
  value mlre2__options_alloc_custom_block(void) {
    return caml_alloc_custom(&mlre2__custom_regex_options_ops, sizeof(RE2::Options *),
                      100,      /* uses ~100 bytes outside the OCaml heap */
                      1000 * 100);  /* I'm okay with 100k of RAM being wasted */
  }

  CAMLprim value mlre2__options__create_quiet(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(v_retval);
    v_retval = mlre2__options_alloc_custom_block();
    RegexOptions_val(v_retval) = new RE2::Options();
    RegexOptions_val(v_retval)->Copy(RE2::Quiet);
    CAMLreturn(v_retval);
  }

  CAMLprim value mlre2__options__encoding__get_latin1(value unit) {
    (void) unit;
    return Val_int(RE2::Options::EncodingLatin1);
  }

  CAMLprim value mlre2__options__encoding__get_utf8(value unit) {
    (void) unit;
    return Val_int(RE2::Options::EncodingUTF8);
  }

  CAMLprim value mlre2__options(value v_regex) {
    CAMLparam1(v_regex);
    CAMLlocal1(v_retval);
    RE2::Options *options = new RE2::Options();
    options->Copy(Regex_val(v_regex)->options());

    v_retval = mlre2__options_alloc_custom_block();
    RegexOptions_val(v_retval) = options;
    CAMLreturn(v_retval);
  }

} /* extern "C" */
