#ifndef MLRE2_STUBS_H
#define MLRE2_STUBS_H

extern "C" {

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <re2/re2.h>
  /* must be called before any other functions. */
  extern value mlre2__init(value unit);

  extern value mlre2__create_re(value v_options, value v_pattern);
  /** [mlre2__create_re(v_options, v_pattern)]
   * returns an opaque value [cre2__obj_t] (the C pointer to the RE2 object)
   */

  extern value mlre2__num_submatches(value v_regex);
  /** [mlre2__num_submatches(v_regex)] returns the number of submatches defined
   * by [v_regex].  N.B. since we are almost always interested in the whole
   * match as well as the submatches, this function returns
   * [RE2::NumberOfCapturingGroups() + 1] */

  extern value mlre2__submatch_index(value v_regex, value v_name);
  /** [mlre2__submatch_index(v_regex, v_name)] returns the submatch index
   * associated with the given name, or else returns -1 */

  extern value mlre2__pattern(value v_regex);
  /** [mlre2__pattern(v_regex)] returns [regex->pattern()] */

  extern value mlre2__iter_next(value v_regex, value v_pos,
      value v_n_submatches, value v_input);
  /** [mlre2__iter_next(v_regex, v_pos, v_n_submatches, v_input)] returns
   * [(int * (int * int) option array option) ] where first int is the new
   * [v_pos] and the nth element of the list is the start index and length of
   * the nth submatch, if the nth subpattern captured a string, or None
   * otherwise.
   *
   * The [input] parameter is the original string, and is used to calculate the
   * returned offsets.  So [input] must be _physically_ equal between calls.
   * A copy will not suffice.
   *
   * If [v_n_submatches == 0], Some [] means at least one (unspecified) match exists.
   * If [v_n_submatches < 0], all defined submatches are returned.
   *
   * If [v_pos] is negative, the entire string has been processed.
   */

  extern value mlre2__matches(value v_regex, value v_str);
  /** [mlre2__matches(v_regex, v_str)] return bool true or false, whether or
   * not at least one unanchored match is found in the given input. */

  extern value mlre2__find_all(value v_regex, value v_sub, value v_str);
  /** [mlre2__find_all(v_regex, v_sub, v_str)] returns a string list
   * containing the captures by the [v_sub]th submatch of [v_regex] on [v_str].
   *
   * If unspecified, [v_sub] defaults to zero, the whole match.
   *
   * May raise an exception.
   */

  extern value mlre2__find_first(value v_regex, value v_sub, value v_str);
  /** like mlre2__find_all but only returns a string, the first match */

  extern value mlre2__rewrite_exn(value _compiled, value _input, value _rewrite);
  /** [mlre2__rewrite(_compiled, _input, _rewrite)] returns a copy of the string
   * [_input], modified like this:
   * for each successive non-overlapping matche of the regex [_compiled],
   * first specialize the string [_rewrite] by replacing every occurrence of
   * "\\n" for n in [0-9] with the nth submatch,
   * then substitute the specialized template for the matched substring.
   *
   * The substitution may be of length from that of the match, since we use
   * (resizable) C++ strings internally.
   *
   * May raise [Regex_rewrite_template_invalid]
   */

  extern value mlre2__valid_rewrite_template(value v_regex, value v_template);
  /** [mlre2__valid_rewrite_template(v_regex, v_template)] calls
   *  RE2::CheckRewriteString()
   */

  extern value mlre2__escape(value _str);
  /** a thin wrapper around RE2::QuoteMeta */

  extern value mlre2__multiple_create(value v_options);
  extern value mlre2__multiple_add(value v_multiple, value v_pattern);
  extern value mlre2__multiple_compile(value v_multiple);
  extern value mlre2__multiple_match(value v_multiple, value v_str);
  /** These are thin wrappers around the similarly named RE2::Set methods. The naming is
      to avoid conflicts with [Set] on the OCaml side. */

  /** Some CINAPS templating to declare the getters and setters for options fields. */

  /*$ #use "options.cinaps";;
   List.iter all ~f:(fun { name; _} ->
   printf "\n  \
   extern value mlre2__options__%s(value v_options);\n  \
   extern value mlre2__options__set_%s(value v_options, value v_value);" name name);;
  */
  extern value mlre2__options__case_sensitive(value v_options);
  extern value mlre2__options__set_case_sensitive(value v_options, value v_value);
  extern value mlre2__options__dot_nl(value v_options);
  extern value mlre2__options__set_dot_nl(value v_options, value v_value);
  extern value mlre2__options__encoding(value v_options);
  extern value mlre2__options__set_encoding(value v_options, value v_value);
  extern value mlre2__options__literal(value v_options);
  extern value mlre2__options__set_literal(value v_options, value v_value);
  extern value mlre2__options__log_errors(value v_options);
  extern value mlre2__options__set_log_errors(value v_options, value v_value);
  extern value mlre2__options__longest_match(value v_options);
  extern value mlre2__options__set_longest_match(value v_options, value v_value);
  extern value mlre2__options__max_mem(value v_options);
  extern value mlre2__options__set_max_mem(value v_options, value v_value);
  extern value mlre2__options__never_capture(value v_options);
  extern value mlre2__options__set_never_capture(value v_options, value v_value);
  extern value mlre2__options__never_nl(value v_options);
  extern value mlre2__options__set_never_nl(value v_options, value v_value);
  extern value mlre2__options__one_line(value v_options);
  extern value mlre2__options__set_one_line(value v_options, value v_value);
  extern value mlre2__options__perl_classes(value v_options);
  extern value mlre2__options__set_perl_classes(value v_options, value v_value);
  extern value mlre2__options__posix_syntax(value v_options);
  extern value mlre2__options__set_posix_syntax(value v_options, value v_value);
  extern value mlre2__options__word_boundary(value v_options);
  extern value mlre2__options__set_word_boundary(value v_options, value v_value);/*$*/

  extern value mlre2__options__create_quiet(value unit);
  /** The quiet Options constructor, in a custom block */

  extern value mlre2__options__encoding__get_latin1(value unit);
  extern value mlre2__options__encoding__get_utf8(value unit);
  /** The values of RE2::Options::Encoding, as OCaml ints */

  extern value mlre2__options(value v_regex);
  /** returns a copy of the options used to create a regex. */

} /* extern "C" */

#endif /* MLRE2_STUBS_H */
