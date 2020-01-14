## v0.11

- Switched `Sexplib`'s sexp parser to use the `Parsexp` library.
  See https://github.com/janestreet/parsexp

- Remove the `location` field from the `parse_error` type.

- Dropped dependency on Base.

## v0.10

- Added `val Sexp.is_unit : t -> bool`

- Enabled `-safe-string`

## v0.9

## 113.43.00

- Sexps and EOF are a mess. Try to improve the situation somewhat!

  In particular, this feature makes improvements to when
  `Parsing_whitespace` sexp parser state is reported: it now
  distinguishes the case `Parsing_toplevel_whitespace` from
  `Parsing_nested_whitespace` and it's only a valid empty parse if
  `eof` happens in `Parsing_toplevel_whitespace`. See test diffs for
  examples of strings that were previously valid empty parses, but are
  now incomplete. One of the craziest is probably "(foo #| bar".

## 113.33.00

- Changes `Sexp.to_string` to escape all non-ASCII characters.

  Previously chars >= 127 are escaped or not depending on:
  1. other character in the string
  2. the system
  3. environment variable settings

  (2) and (3) are because `String.escaped` from the stdlib uses the C
  function `isprint` which is locale and OS dependent.

  This can cause invalid UTF-8 sequence to be printed by sexplib, which
  is annoying:

    https://github.com/janestreet/sexplib/issues/18

  Starting with this release, sexplib:
  1. copies the `String.escaped` function of OCaml 4.03 which escapes
     all non-ascii characters
  2. make sure we escape the string when it contains characters >= 127

- Clean up the documentation for sexplib, modernizing it to include
  `ppx_sexp_conv`, and breaking up the documentation between sexplib and
  `ppx_sexp_conv`.  Also changed the formatting to use org-mode, so it
  will render properly on github.  Markdown doesn't render well by
  default, unless you use quite different conventions about linebeaks.

- In sexp macro library, avoid returning success when there is any error
  reading a sexp. In particular, this prevents

    sexp resolve <(echo '(:use x)')

  from silently succeeding.

  Also, now we no longer read an included file multiple times.
  This lets even crazy stuff like this to work:

    $ echo 'hi ' | sexp resolve <(echo '((:include /dev/stdin) (:include /dev/stdin))')

## 113.24.00

- Switch code in `lib` subdir to ppx-style.

## 112.35.00

- Inline some calls that js_of_ocaml was unable to recognise as
  tail-recursive (cf. issue #14)

## 112.24.00

Minor update: documentation.

## 112.17.00

- Added `sexp_of_` support for GADTs, and remove the not-quite-working
  support for `of_sexp`.

## 112.06.00

- Improved the implementation of `Exn.sexp_of_t`, using the unique id in
  exceptions in OCaml 4.02.

    We use the identifier to map exception constructors to converters.

## 112.01.00

- Replaced occurrences of `Obj.magic 0` with `Obj.magic None`.

  With the former the compiler might think the destination type is
  always an integer and instruct the GC to ignore references to such
  values.  The latter doesn't have this problem as options are not
  always integers.

## 111.25.00

- Fix compatibility with OCaml 4.02

## 111.17.00

- Make the camlp4 dependency optional

## 111.13.00

- In `Sexplib.Std`, renamed `Macro` as `Sexp_macro`.

## 111.11.00

- Added error locations to `Macro`-expansion errors.

## 110.01.00

- Added `with sexp` support for mutually recursive types with common
  fields.

    For instance:

    ```ocaml
    type a = { fl : float } and b = { fl : int } with sexp
    ```

    Closes #3
- Fixed the behavior of sexplib on `private` types.

    sexplib used to ignore the `private` modifier, which means generated
    functions had the wrong type.  Now, it generates a function with the
    right type for `sexp_of` and refuses to generate anything for
    `of_sexp`.
- Added `Macro.expand_local_macros`, which macro expands sexps,
  failing on `:include` macros.
- Fixed `Macro`'s handling of nested `:include`'s which was broken
  with respect to paths.

    Prior to this fix, `:include`'s were broken with respect to the path
    used to resolve the filename.  Including a file outside the current
    directory which included another file relative to that one would
    break.

## 109.60.00

- Moved unix-specific code to a new object section, sexplib_unix

## 109.53.00

- Changed `sexp_of_float` to (usually) use as few digits as possible,
  without losing precision.
- Split the part of `Sexplib` that depends on `Num` into a separate
  library, `Sexplib_num`.

    This was done to eliminate the dependence of `Core_kernel` on `Num`,
    which is not usable on javascript.

## 109.52.00

- Added a `Macro` module, with `load_sexp*` functions that support
  file includes and templates.

    The following new syntaxes are supported:

    ```ocaml
    (:include filename)
    (:let f (arg1 ... argn) sexp1 ... sexpn)
    (:use f (arg1 valn) ... (argn valn))
    (:concat a1 ... an)
    ```
- Added support to `with sexp` for a subset of GADTs.

    The new support is for types that use existentially quantified
    variables or plain variants written with GADT syntax.

    Existentially quantified variables still have to be wrapped with
    `sexp_opaque` generate compiling code.
- Fixed a type error in the code generated by `with sexp` in some
  cases of variant inclusions.

## 109.20.00

- Renamed converter generated by `with sexp` for polymorphic variants so it is hidden from the toplevel.

    `of_sexp` created a value named `<type>_of_sexp__` to handle
    polymorphic variants.  To hide it from the toplevel, we renamed it as
    `__<type>_of_sexp__`.  We kept the `__` suffix to avoid any confusion
    with a type named `__<type>`.

## 109.12.00

- A tiny lexer improvement in `lexer.mll`.  Used
  `lexbuf.lex_{start|curr}_pos` instead of
  `lexbuf.lex_{start|curr}_p.pos_cnum` for computing the length of a
  lexeme since the difference is the same.

## 109.10.00

- Improved error messages in presence of GADTs.
- Made `with sexp` work with types containing `as` in signatures.

## 109.09.00

- Fixed an `unused rec` warning in the code generated by `pa_sexp` in
  rare cases.

## 2012-07-15

- Added support for S-expression default record fields.
- Added syntax for S-expression comments and for nested block comments.
- Fixed a few minor bugs and inconsistencies in the parsers and
  updated their whitespace handling to conform with the upcoming
  OCaml 4.00 compiler.  The parser specification now also supports
  Menhir.
- Rewrote README in Markdown and improved documentation.
- Minor bug fix in the preprocessing module.
- Eliminated new warnings available in OCaml 4.00.

## 2011-09-18

- Improved documentation.

## 2011-09-15

- Fixes to improve package dependency resolution.

## 2011-07-05

- Fixed a parser position bug.  Parser positions passed by the user
  were not updated correctly.
- Internal code beautification.

## 2011-07-04

- Internal updates to sync with Jane Street.

## 2011-01-30

- Fixed a code generation bug with toplevel entries.

    Thanks to Yong Lu <lyongu@gmail.com> for the report!

## 2010-12-27

- Added support for MoreLabels.Hashtbl and improved reporting of
  error locations with preprocessor.

## 2010-12-26

- Worked around a compiler bug that is expected to be fixed in
  OCaml 3.12.1.  This workaround temporarily removes the interface
  for module Conv (conv.mli), thus exposing the internals.
  This should not cause any problems for end users as long as
  they do not depend on the exported internal representations.
  The interface will become constrained again as soon as the
  fixed compiler is out.

## 2010-12-22

Major release.

- Merged with Jane Street version.  This has caused an API-change
  that requires "open Sexplib.Conv" at the top of files that use the
  syntax extension.

- Renamed functions:

    * sexp_of_lazy -> sexp_of_lazy_t
    * lazy_of_sexp -> lazy_t_of_sexp

- Some standard library modules are now re-exported with predefined
  S-expression converters in module Conv.

## 2010-09-25

- Fixed inferred types of generated functions when dealing with arrow
  types.

## 2010-08-26

- Fixed a lexer bug when parsing comments.

## 2010-05-21

- Added support for sexp_bool record field annotations.

## 2010-05-18

- Improved performance of converting S-expressions to strings.

## 2010-04-12

- Changed API of Of_sexp_error exception.

## 2010-04-07

- Added of_(big)string_conv_exn functions.

## 2010-04-01

- Merged with Jane Street version.

    Major new features (various functions):

    * Type-annotated parsing for better error messages
    * Greatly improved performance of exception converters

## 2009-12-21

- Improved saving of files.

## 2009-10-12

- Added sexp_array record field extension.

## 2009-09-19

- Added missing variant type cases.
- Fixed handling of variance annotations.

## 2009-09-15

- Internal cleanups.

## 2009-07-28

- Added better support for conversion of exception types.

## 2009-06-23

- Fixed build problem.

    Thanks to Sylvain Le Gall <gildor@ocamlcore.org> for
    the patch!

## 2009-05-08

- Fixed build problems on Windows and OCamlMakefile issues.

    Thanks to Sylvain Le Gall <gildor@ocamlcore.org> for
    the patch!

## 2009-04-22

- Added macro support for all types of vectors, matrices,
  and for bigstrings.

## 2009-04-21

- Merged with Jane Street version, no user-relevant changes.

## 2009-03-09

- Merged with Jane Street version, no user-relevant changes.

## 2009-03-01

- Fixed build problem on Mac OS X by updating OCamlMakefile.

## 2009-01-20

- Automatically add S-expression pretty-printers to toplevels.

## 2008-09-30

- Added a new feature: sexp_opaque.  It prevents the need for /
  use of type converters for a given type in a particular
  type context.

    Removed abstract types to unify them with this new concept.

## 2008-09-29

- Added a new feature: sexp_list.  This is similar to
  the handling of sexp_option.  By default an empty list is
  assumed for unspecified records using sexp_list as qualifier.
  Such record fields bound to empty lists will also not be
  printed anymore for better readability.

## 2008-09-23

- Added missing Not_found-exception to standard exception
  converters.

## 2008-08-20

- Removed dependency on threads.  Fixed build problems.

## 2008-08-08

- Nifty new feature: exceptions can now be converted to
  S-expressions, too!  The "with sexp" syntax extension can be
  used with exceptions, thus registering a conversion function.
  A global exception conversion function can then be called
  to convert an arbitrary exception into an S-expression,
  which can then be printed out.  This should greatly
  improve readability of uncaught exceptions while making
  life extremely easy for the developer.

- Renamed the ParseError exception to Parse_error to be more
  compliant with Jane Street naming conventions.

## 2008-07-25

- Added utilities for conversion error handling.  Minor fixes.

## 2008-04-24

- Made Sexp-interface manifest.

## 2008-03-20

- Fixed META-file (missing num dependency).

## 2008-03-17

- Improved META-file.

## 2008-03-13

- Fully allow function types in converters.  Raise runtime
  exceptions on converting from S-expressions instead when
  function type encountered.

## 2008-02-11

- Fixed code generation problems with variance annotations
  in signatures, and empty types.

## 2007-12-17

- Added support for generating signature entries for
  S-expression converters.  Thanks to Till Varoquaux
  <till.varoquaux@gmail.com> for the patch!

## 2007-11-29

- Added support for converting big_int, nat, num, and ratio.

## 2007-11-26

- Added support for parsing from bigstrings (char bigarrays).

## 2007-11-02

- Added syntax support for option types to use the ordinary
  sum type syntax.  This should improve readability.
  The old syntax will be accepted, too, if
  Conv.read_old_option_format is set to true (this is
  currently the default).  The old format will be used for
  writing if Conv.write_old_option_format is true (currently
  the default).  The old syntax is deprecated and will
  probably not be supported by default in the near future.
  Reading new-style option values will always succeed.

## 2007-09-14

- Fixed bug in S-expression preprocessor concerning record
  field names.

## 2007-08-06

- Added support for converting functions to S-expressions.

## 2007-07-20

- Fixed position information and improved speed of S-expression
  parser.  Fixed S-expression macro bug concerning contained
  polymorphic variants.

## 2007-06-28

- Improved Sexplib code generation.

## 2007-06-22

- Fixed escaping bug in S-expression parser.

## 2007-06-01

- Added correct handling of recursive types + test case.

## 2007-04-18

- Added missing conversion functions from S-expressions to
  pairs and triples.

## 2007-03-21

- Updated OCamlMakefile.

## 2007-03-02

- Improved error messages when parsing illegal type definitions.

## 2007-01-30

- Added triple conversions.

## 2006-11-22

- Updated OCamlMakefile.

## 2006-10-13

- Improved checking of records for extra or duplicate fields.

## 2006-09-06

- Added support for polymorphic record fields.

## 2006-09-05

- Added support for manifest types.

## 2006-08-16

- Improved error messages.

## 2006-07-28

- Added a new, hand-written S-expression parser that supports
  partial parsing and should be approx. 10x faster than the
  previous one.

## 2006-06-20

- Fixed a code generation problem leading to compilation
  errors concerning the use of type aliases within polymorphic
  variant type definitions.

    This fix also solves potential erroneous appearances of
    backtracking exceptions in user code.

## 2006-03-21

- Added -for-pack option to Makefile and cleaned up
  distribution for a new public release.

## 2006-03-13

- Sexplib now accepts capitalized booleans.

## 2006-03-03

- Added customizable indentation levels.
- Improved documentation.
- Fixed API-problem concerning backward compatibility.

## 2006-03-01

- Added a missing flush for string conversions with a buffer.

## 2006-02-08

- Eliminated unused variable warnings in Sexplib-generated code.

## 2006-01-11

- Added functions for pretty-printing to buffers.
- Improved performance of outputting S-expressions to channels.

## 2006-01-09

- Added functions load_sexp and load_sexps.

## 2006-01-04

- Changed float conversion from %E to %G (more readable).

## 2005-12-28

- Made machine representation for S-expressions more compact

## 2005-12-15

- Fixed a problem appearing with OCaml-release 3.08.4: CamlP4
  obviously performs more strict checking on some constructs
  now and crashed with an exception when generating
  S-expression code for records containing only one field
  ("singleton tuple problem").  This problem is fixed now.

## 2005-11-25

- Fixed problem with type variables that could not be generalized.

## 2005-11-23

- Added a missing case in type definitions (path alias)

## 2005-11-17

- Major release: 2.0

    Fixed a major design problem.  The user now has to pass lex
    buffers instead of channels to input-functions.  Reason:
    trailing characters in channels were lost due to ocamllex
    buffering them in the non-exposed lex buffer.  This lex
    buffer is now exposed.  The functions have been renamed
    ("input_X" -> "scan_X") to reflect this change.

## 2005-11-16

- Added label to conversion function "input_cnv_sexps".

## 2005-11-11

- Fixed a bug in the pretty-printer: strings in atoms were
  not escaped in the function "to_string_mach" (and therefore
  also "to_string").

## 2005-11-07

- Initial release.
