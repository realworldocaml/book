# Changes

## 2020/02/11

* Re-implement Menhir's default algorithm for constructing LR(1) automata,
  namely Pager's algorithm. This closes issue #21 (reported by Andrej Bauer),
  a bug that would sometimes cause unexplainable conflicts to appear, because
  states were merged too aggressively. This also removes an unreported bug
  that would cause the automaton to have too many states, because states were
  *not* merged aggressively enough. In summary, the old and new construction
  algorithms differ: in many cases, the resulting automaton is unchanged, but
  in some cases, the automaton produced by the new algorithm may have slightly
  more or slightly fewer states.

* Re-implement Menhir's algorithm for constructing automata in `--no-pager`
  mode. In this (undocumented) mode, Menhir does not merge any states, but
  allows itself to redirect a transition from a state `s` to a *larger* state
  `s'`. This method yields an automaton whose states form a subset of the
  states of the canonical LR(1) automaton. It usually has significantly fewer
  states than the canonical automaton, and significantly more states than the
  automaton produced by Pager's algorithm. The new construction method removes
  an unreported bug that would cause the automaton to have too many states.
  The automaton produced by the new algorithm will usually have significantly
  fewer states than the automaton produced by the previous algorithm.

* Re-implement Menhir's algorithms for constructing automata in `--lalr` and
  `--canonical` modes. The previous algorithms were correct, as far as we
  know, so the output of the new algorithms is the same, up to a possible
  renumbering of the states. The new algorithms are slightly faster.

* Increase the maximum length of a production, which used to be 127,
  up to 1023. Display a polite error message if this length is exceeded.
  (Problem reported by Andreas Abel.)

* The new switch `--timings-to <filename>` causes internal timing
  information to be written to the file `<filename>`.

* A version of the library `fix` is now vendored (included inside) Menhir.
  This should have no impact for end users, but implies that `dune` 2.2.0
  or later is required.

## 2020/01/21

* There used to be a distinction between two slightly different ways of
  installing Menhir, namely with and without `ocamlfind`. This distinction
  disappears. The command line switch `--suggest-ocamlfind` is deprecated
  and causes Menhir to print `false`.

* Menhir is now built and installed by `dune`. This should make life easier
  for Menhir's developers: in particular, `make test` and `make speed` can be
  run straight away and do not requiring installing Menhir first. This should
  also make compilation much faster on multi-core machines. (Contributed by
  Nicolás Ojeda Bär, to whom many thanks are due.)

## 2019/09/24

* Build Menhir's standard library into the Menhir executable instead of
  storing it in a separate file `standard.mly`. This removes the need to
  hardcode the path to this file into the Menhir executable. This also removes
  the need for the command line switch `--stdlib`, which remains supported but
  is now ignored, and for the environment variable `$MENHIR_STDLIB`, which is
  now ignored. A positive side effect of this change is that the full path of
  the file `standard.mly` no longer appears in generated parsers; this removes
  a source of spurious variation. (Suggested and implemented by Nicolás Ojeda
  Bär.)

## 2019/06/20

* When compiled with OCaml 4.02.3, Menhir could produce OCaml code
  containing invalid string literals. This was due to a problem in
  `String.escaped`. Fixed. (Reported by ELLIOTCABLE.)

## 2019/06/13

* Relax the syntax of point-free actions to allow `< >` (with arbitrary
  whitespace inside the angle brackets) instead of just `<>`.
  (Suggested by Lélio Brun.)

* When a cycle of `%inline` nonterminal symbols is encountered,
  the error message now shows the entire cycle,
  as opposed to just one symbol that participates in the cycle.

* Fix the treatment of the `error` token when printing the grammar for
  `ocamlyacc`. Its semantic value must not be referred to; a unit value
  must be used instead. The switch `--only-preprocess-for-ocamlyacc`
  remains undocumented. (Reported by kris.)

* Coq back-end: multiple changes to stay up-to-date with respect to
  coq-menhirlib. See [coq-menhirlib/CHANGES.md](coq-menhirlib/CHANGES.md).

* Coq back-end: the generated parser now contains a dedicated inductive
  type for tokens. This removes the need for `Obj.magic` in client code
  when the parser is used via extraction.

* Coq back-end: the generated parser checks that the version of
  MenhirLib matches. This check can be disabled with
  `--coq-no-version-check`.

* Coq back-end: the fuel parameter is now given as the *logarithm* of
  the maximum number of steps to perform. Therefore, using e.g., 50
  makes sure we will not run out of fuel in any reasonable
  computation time.

## 2018/11/13

* In `.mly` files, a new syntax for rules has been introduced, which is
  slightly more pleasant than the old syntax. (A rule is the definition of a
  nonterminal symbol.) The old syntax remains available; the user chooses
  between the two syntaxes on a per-rule basis. The new syntax is fully
  documented in the manual;
  [a brief summary of the differences](doc/new-rule-syntax-summary.md)
  with respect to the old syntax is also available.
  **The new syntax is considered experimental**
  and is subject to change in the near future.

* In the Coq back-end, avoid pattern-matching at type `int31`,
  which will disappear in future versions of Coq.
  Instead, convert `int31` to `Z`, and perform pattern matching in `Z`.
  (Reported by Vincent Laporte, implemented by Jacques-Henri Jourdan.)

* Implement a more economical renaming scheme for OCaml variables
  during the elimination of `%inline` symbols. This leads to slightly
  more readable code (more reasonable variables names, fewer `let`
  bindings).

* Another attempt at removing all trailing spaces in auto-generated
  `.messages` files. (I hope I got it right, this time.)

## 2018/10/26

* A new syntactic sugar facility, "token aliases", has been added.
  The declaration of a terminal symbol may also declare an alias,
  which takes the form of a name between double quotes, as in
  `%token PLUS "+"`.
  Thereafter, `"+"` may be used freely in place of `PLUS` throughout
  the grammar. This makes it slightly easier to read grammars.
  (Contributed by Perry E. Metzger.)

* Until today, the semicolon character `;` was insignificant: it was
  considered as whitespace by Menhir. It is now accepted only in a
  few specific places, namely: after a declaration; after a rule;
  after a producer. If Menhir suddenly complains about a semicolon,
  just remove it. This change is being made in preparation for
  further syntactic changes.

* New flag `--no-dollars`, which disallows the use of `$i` in semantic actions.
  The default behavior remains to allow the use of `$i`.

* When generating OCaml code, include all record fields in record patterns,
  even when bound to a wildcard pattern. Thus, avoid triggering OCaml's
  warning 9.

## 2018/10/06

* Standard library: add `rev`, `flatten`, `append`.
  Add a link from the manual to `standard.mly` in the repository.

* Update the manual to explain how to use `dune` and `menhir` together.

* Install `.cmxs` files for menhirLib and menhirSdk.

* Remove all references to `Pervasives` in the generated OCaml code.
  These references were infrequent anyway,
  and `Pervasives` is about to be deprecated in OCaml 4.08, it seems.

* In `--interpret` mode, print `Ready!` once ready to accept input.

* At verbosity level `-lg 2`, for each nonterminal symbol `N`, display
  a sentence (composed of terminal symbols) of minimal length generated
  by `N`.

* When writing a `.cmly` file, open it in binary mode. This should
  eliminate the failure that was observed under Windows:
  `output_value: not a binary channel`. (Reported by Bryan Phelps.
  A fix was present in the `mingw` patches for Menhir.)

* Change the logic used in the root `Makefile` to deal with Unix and Windows
  in a uniform way. (Also inspired by the `mingw` patches for Menhir.)

* Coq back-end: add a few newlines in the generated file for readability.
  (Suggested by Bernhard Schommer.)

* Remove the trailing space at the end of every sentence in
  auto-generated `.messages` files. (Suggested by Xavier Leroy.)

## 2018/09/05

* When `--explain` is enabled, always create a fresh `.conflicts` file
  (wiping out any pre-existing file), even if there are in fact no conflicts.
  This should avoid confusion with outdated `.conflicts` files.

* Fix several bugs in the treatment of `--strict`. No `.conflicts` file
  was created when `--strict` and `--explain` were both enabled. Also,
  some warnings were not properly turned into errors by `--strict`.

## 2018/07/04

* Update the `man` page, which was woefully out of date.

## 2018/07/03

* New location keywords.
  `$loc` is sugar for the pair `($startpos, $endpos)`.
  `$loc(x)` is sugar for the pair `($startpos(x), $endpos(x))`.
  `$sloc` is sugar for the pair `($symbolstartpos, $endpos)`.
  (Contributed by Nicolás Ojeda Bär.)

## 2018/06/08

* Add two new parameterized nonterminal symbols, `endrule(X)` and `midrule(X)`,
  to the standard library. These symbols have been available since 2015/02/11
  under the names `anonymous(X)` and `embedded(X)`, but were not yet documented.
  `endrule(X)` and `midrule(X)` are now documented,
  while `anonymous(X)` and `embedded(X)` remain present but are deprecated.

## 2018/05/30

* In `--coq` mode, Menhir now produces references to `MenhirLib.Grammar`
  instead of just `Grammar`, and similarly for all modules in Menhir's support
  library.

* New command line option `--coq-lib-no-path` to suppress the above behavior
  and retain the previous (now-deprecated) behavior, that is, produce
  unqualified references the modules in Menhir's support library.

* New command line option `--coq-lib-path <path>` to indicate under what name
  (or path) the support library has been installed. Its default value is
  `MenhirLib`.

## 2018/05/23

* New commands `--infer-write-query`, `--infer-read-reply`, and
  `--infer-protocol-supported`. These commands remove the need for Menhir to
  invoke `ocamlc` and `ocamldep` behind the scenes, and make it easier to
  write correct build rules for Menhir projects. The command line options
  `--infer`, `--raw-depend` and `--depend` remain supported, but are no longer
  preferred. (Suggested by Fabrice Le Fessant.)

* Remove the warning that was issued when `%inline` was used but `--infer` was
  turned off. Most people should use a build system that knows how to enable
  OCaml type inference, such as `ocamlbuild` or `dune`.

* New HTML rendering of the manual, available both online and as part of
  Menhir's installation. (Implemented by Gabriel Scherer.)

## 2017/12/22

* Add a flag `--unused-precedence-levels` to suppress all warnings about
  useless `%left`, `%right`, `%nonassoc` and `%prec` declarations.
  (Suggested by Zachary Tatlock.)

## 2017/12/06

* Fix the termination test that takes place before parameterized symbols are
  expanded away. The previous test was both unsound (it would accept grammars
  whose expansion did not terminate) and incomplete (it would reject grammars
  whose expansion did terminate). The new test is believed to be sound and
  complete. (Thanks to Martin Bodin for prompting us to look into this issue.)

## 2017/11/12

* Documentation: clarify the fact that `%type` declarations should carry types
  whose meaning does not depend on the headers `%{ ... %}`.

## 2017/10/13

* Remove the OCaml version check at installation time, for greater simplicity,
  and because for some reason it did not work properly under Cygwin.
  (Reported by Andrew Appel.)

## 2017/09/26

* `Makefile` fix: when determining whether the suffix `.exe` should be used,
  one should test whether the OS is Windows,
  not whether the compiler is MSVC.
  (Suggested by Jonathan Protzenko.)

## 2017/07/12

* Include the LaTeX sources of the manual in the official `.tar.gz` archive.
  This should allow the manual to be included as part of the Debian package.

* Mention [Obelisk](https://github.com/Lelio-Brun/Obelisk),
  a pretty-printer for `.mly` files, in the manual.

## 2017/06/07

* Removed an undeclared dependency of MenhirSdk on Unix. (Reported and fixed
  by Frédéric Bour.)

## 2017/05/09

* Menhir now always places OCaml line number directives in the generated `.ml`
  file. (Until now, this was done only when `--infer` was off.) Thus, if a
  semantic action contains an `assert` statement, the file name and line
  number information carried by the `Assert_failure` exception should now be
  correct. (Reported by Helmut Brandl.)

## 2017/04/18

* Changed Menhir's license from QPL to GPLv2.
  MenhirLib remains under LGPLv2, with a linking exception.

* Moved the repository to
  [gitlab.inria.fr](https://gitlab.inria.fr/fpottier/menhir/).

* Introduced a new command line switch, `--cmly`, which causes Menhir to
  create a `.cmly` file, containing a description of the grammar and
  automaton. (Suggested by Frédéric Bour.)

* Introduced a new library, MenhirSdk, which allows reading a `.cmly` file.
  The purpose of this library is to allow external tools to take advantage
  of the work performed by Menhir's front-end. (Suggested by Frédéric Bour.)

* Introduced new syntax for attributes in a `.mly` file. Attributes are
  ignored by Menhir's back-ends, but are written to `.cmly` files, thus
  can be exploited by external tools via MenhirSdk. (Suggested by Frédéric Bour.)

* The definition of a `%public` nonterminal symbol can now be split into several
  parts within a single `.mly` file. (This used to be permitted only over
  multiple `.mly` files.) (Suggested by Frédéric Bour.)

* New functions in the incremental API:
  `shifts`, `acceptable`, `current_state_number`.

* New functions in the incremental API and inspection API:
  `top`, `pop`, `pop_many`, `get`, `equal`,
  `force_reduction`, `feed`, `input_needed`,
  `state_has_default_reduction`,
  `production_index`, `find_production`.
  (Suggested by Frédéric Bour.)

* New module `MenhirLib.ErrorReports`. This module is supposed to offer
  auxiliary functions that help produce good syntax error messages.
  This module does not yet contain much functionality and is expected
  to evolve in the future.

* Incompatible change in the incremental API: the type `env` becomes `'a env`.

* Incompatible change in the incremental API: the function
  `has_default_reduction` is renamed `env_has_default_reduction`.

* The type `stack` and the function `stack` in the incremental API are
  deprecated. The new functions `top` and `pop` can be used instead to
  inspect the parser's stack. The module `MenhirLib.General` is deprecated
  as well. Deprecated functionality will be removed in the future.

* Incompatible change in the incremental API: the type of the function
  `print_stack` in the result signature of the functor
  `MenhirLib.Printers.Make` changes to `'a env -> unit`.
   (Anyway, as of now, `MenhirLib.Printers` remains undocumented.)

* Improved the syntax error message that is displayed when a `.mly` file
  is incorrect: the previous and next token are shown.

* Fixed a bug where the module name `Basics` was shadowed (that is, if the user's
  project happened to contain a toplevel module by this name, then it could not
  be referred to from a `.mly` file). (Reported by François Thiré.)

## 2017/01/01

* Add `$MENHIR_STDLIB` as a way of controlling where Menhir looks for the file
  `standard.mly`. This environment variable overrides the installation-time
  default setting, and is itself overridden by the `--stdlib` command line
  switch. (Requested by Jonathan Protzenko.)

* `Makefile` fix: filter out `'\r'` in the output of `menhir --suggest-ocamlfind`,
  so that the `Makefile` works when Menhir is compiled as a Windows executable.
  (Suggested by Jonathan Protzenko.)

## 2016/12/01

* Updated the Coq back-end for compatibility with Coq 8.6.
  (Jacques-Henri Jourdan.)

## 2016/11/15

* Fix in `--only-preprocess-for-ocamlyacc` mode: avoid printing newline characters
  inside a `%type` declaration, as this is forbidden by `ocamlyacc`. (Reported by
  Kenji Maillard.)
* Fix in `--only-preprocess-for-ocamlyacc` mode: avoid variable capture caused by
  `ocamlyacc` internally translating `$i` to `_i`. (Reported by Kenji Maillard.)

## 2016/09/01

* New command line switch `--only-preprocess-for-ocamlyacc`, supposed to print the
  grammar in a form that `ocamlyacc` can accept. As of now, this feature is
  incomplete (in particular, support for Menhir's position keywords is missing),
  untested, and undocumented. It could be removed in the future.

## 2016/08/26

* Fixes in the output of `--only-preprocess`:
  * The order of productions is now preserved.
      (It was not. This matters if there are reduce/reduce conflicts.)
  * `%parameter` directives are now printed. (They were not).
  * `%on_error_reduce` directives are now printed. (They were not.)

## 2016/08/25

* `Makefile` fix, undoing a change made on 2016/03/03, which caused installation
  to fail under (some versions of?) Windows where dynamic linking is not
  supported. (Reported by Andrew Appel.)

## 2016/08/05

* `%on_error_reduce` declarations now have implicit priority levels, so as to
  tell Menhir what to do when two such declarations are applicable.
  Also, the well-formedness checks on `%type` and `%on_error_reduce` declarations
  have been reinforced.

## 2016/06/23

* A small change in the generated code (both in the code and table back-ends) so
  as to avoid OCaml's warning 41. The warning would arise (when compiling a
  generated parser with OCaml 4.03) because Menhir's exception `Error` has the
  same name as the data constructor `Error` in OCaml's pervasive library.
  (Reported by Bernhard Schommer.)

## 2016/05/18

* Anonymous rules now work also when used inside a parameterized rule.
  (This did not work until now.) When an anonymous rule is hoisted out
  of a parameterized rule, it may itself become parameterized. Menhir
  parameterizes it only over the parameters that it actually needs.

## 2016/05/04

* In the Coq back-end, split the largest definitions into smaller
  ones. This circumvents a limitation of vm_compute on 32 bit
  machines. This also enables us to perform sharing between
  definitions, so that the generated files are much smaller.

## 2016/04/10

* When printing a grammar (which is done by the `--only-preprocess` options),
  remove the leading bar `|`, for compatibility with `yacc` and `bison`.

## 2016/03/11

* In the code back-end, generate type annotations when extracting a semantic
  value out of the stack. When working with a semantic value of some function
  type, OCaml would incorrectly warn that this function does not use its
  argument. This warning should now be gone.

## 2016/03/03

* Makefile changes, so as to support `ocamlbuild` 4.03, which seems to have
  stricter hygiene rules than previous versions.

## 2015/12/30

* Prevented an incorrect installation that would take place if `USE_OCAMLFIND`
  was given during `make all` but not during `make install`. Added a command
  line directive `--suggest-ocamlfind`.

## 2015/11/11

* Fixed a severe bug in Menhir 20151110 which (when using the code back-end)
  could cause a generated parser to crash. Thanks to ygrek for reporting the
  bug.

* The code produced by version `XXXXXXXX` of `menhir --table` can now be linked only
  against a matching version of MenhirLib. If an incorrect version of MenhirLib
  is installed, the OCaml compiler should complain that
  `MenhirLib.StaticVersion.require_XXXXXXXX` is undefined.

## 2015/11/10

* Optimized the computation of `$symbolstartpos`, based on a couple of assumptions
  about the lexer. (See the manual.)

## 2015/11/04

* Modified the treatment of `%inline` so that the positions that are computed are
  the same, regardless of whether `%inline` is used. This property did not hold
  until now. It now does. Of course, this means that the positions computed by
  the new Menhir are not the same as those computed by older versions of Menhir.

* Fixed a bug in the treatment of `%inline` that would lead to an incorrect
  position being computed when the caller and callee had a variable by the
  same name.

* Modified Menhir so as to compute the start and end positions in the exact same
  way as `ocamlyacc`. (There used to be a difference in the treatment of epsilon
  productions.) Of course, this means that the positions computed by the new
  Menhir are not the same as those computed by older versions of Menhir. Added
  the keyword `$symbolstartpos` so as to simulate `Parsing.symbol_start_pos()`
  in the `ocamlyacc` world. The keyword `$startpos` sometimes produces a position
  that is too far off to the left; `$symbolstartpos` produces a more accurate
  position.

* Incompatible change of the incremental API: instead of a unit argument, the
  entry points (which are named after the start symbols) now require an initial
  position, which typically should be `lexbuf.lex_curr_p`.

## 2015/11/03

* Fix-fix-and-re-fix the `Makefile` in an attempt to allow installation under
  opam/Windows. Thanks to Daniel Weil for patient explanations and testing.

## 2015/10/29

* MenhirLib is now installed in both binary and source forms.
  `menhir --suggest-menhirLib` reports where MenhirLib is installed.
  This can be used to retrieve a snapshot of MenhirLib in source form
  and include it in your project (if you wish to use `--table` mode, yet
  do not wish to have a dependency on MenhirLib).

## 2015/10/26

* Allow `--list-errors` to work on 32-bit machines (with low hard limits).
  This should fix a problem whereby the 2015/10/23 release could not
  bootstrap on a 32-bit machine.

## 2015/10/23

* New declaration `%on_error_reduce foo`, where `foo` is a nonterminal symbol.
  This modifies the automaton as follows. In every state where a production of
  the form `foo -> ...` is ready to be reduced, every error action is replaced
  with a reduction of this production. (If there is a conflict between several
  productions that could be reduced in this manner, nothing is done.) This does
  not affect the language that is accepted by the automaton, but delays the
  detection of an error: more reductions take place before the error is
  detected.

* Fixed a bug whereby Menhir would warn about a useless `%prec` declaration,
  even though it was useful. This would happen when the declaration was
  duplicated (by inlining or by macro-expansion) and some but not all of
  the copies were useful.

* Added `has_default_reduction` to the incremental API.

* Modified the meaning of `--canonical` to allow default reductions to take
  place. This implies no loss of precision in terms of lookahead sets,
  and should allow gaining more contextual information when a syntax
  error is encountered. (It should also lead to a smaller automaton.)

* A brand new set of tools to work on syntax errors.
* New command `--list-errors`, which produces a list of input sentences which
  are representative of all possible syntax errors. (Costly.)
* New command `--interpret-error`, which confirms that one particular input
  sentence ends in a syntax error, and prints the number of the state in
  which this error occurs.
* New command `--compile-errors`, which compiles a list of erroneous sentences
  (together with error messages) to OCaml code.
* New command `--compare-errors`, which compares two lists of erroneous sentences
  to check if they cover the same error states.
* New command `--update-errors`, which updates the auto-generated comments in
  a list of erroneous sentences.
* New command `--echo-errors`, which removes all comments and messages from
  a list of erroneous sentences, and echoes just the sentences.

## 2015/10/16

* Additions to the incremental API.
  * A `supplier` is a function that produces tokens on demand.
  * `lexer_lexbuf_to_supplier` turns a lexer and a lexbuf into a supplier.
  * `loop` is a ready-made made main parsing loop.
  * `loop_handle` is a variant that lets the user do her own error handling.
  * `loop_handle_undo` is a variant that additionally allows undoing the last
    few "spurious" reductions.
  * `number` maps a state of the LR(1) automaton to its number.

* Incompatible change of the incremental API: renamed the type `'a result`
  to `'a checkpoint`. This is a better name anyway, and should help avoid
  confusion with the type `'a result` introduced in OCaml 4.03.

## 2015/10/12

* Avoid using `$(shell pwd)` in `Makefile`, for better Windows compatibility.

## 2015/10/05

* Fixed a bug where inconsistent OCaml code was generated when `--table`
  and `--external-tokens` were used together. (Reported by Darin Morrison.)

* In `--infer` mode, leave the `.ml` file around (instead of removing it) if
  `ocamlc` fails, so we have a chance to understand what's wrong.

## 2015/09/21

* Re-established some error messages concerning the mis-use of `$i` which
  had disappeared on 2015/06/29.

## 2015/09/11

* Fixed the mysterious message that would appear when a nonterminal symbol
  begins with an uppercase letter and `--infer` is turned on. Clarified the
  documentation to indicate that a (non-start) nonterminal symbol can begin
  with an uppercase letter, but this is not recommended.

## 2015/08/27

* New option `--inspection` (added last January, documented only now). This
  generates an inspection API which allows inspecting the automaton's stack,
  among other things. This API can in principle be used to write custom code
  for error reporting, error recovery, etc. It is not yet mature and may
  change in the future.

## 2015/07/20

* Added the command line options `--unused-token <symbol>` and `--unused-tokens`.

## 2015/06/29

* Changed the treatment of the positional keywords `$i`. They are now
  rewritten into variables of the form `_i` where `i` is an integer.
  Users are advised not to use variables of this form inside semantic
  actions.

## 2015/02/11

* Added support for anonymous rules. This allows writing, e.g.,
  `list(e = expression SEMI { e })`
  whereas previously one should have written
  `list(terminated(e, SEMI))`.

## 2015/02/09

* Moved all of the demos to `ocamlbuild` (instead of `make`).

## 2015/01/18

* Incompatible change of the incremental API.
  The incremental API now exposes shift events too.

## 2015/01/16

* Fixed a couple bugs in `Makefile` and `src/Makefile` which would cause
  compilation and installation to fail with `TARGET=byte`. (Reported
  by Jérémie Courrèges-Anglas and Daniel Dickman.)

## 2015/01/01

* Incompatible change of the incremental API.
  The entry point `main_incremental` is now named `Incremental.main`.

## 2014/12/29

* Incompatible change of the incremental API.
  * The API now exposes reduction events.
  * The type `'a result` is now private.
  * The type `env` is no longer parameterized.
  * `handle` is renamed to `resume`.
  * `offer` and `resume` now expect a result, not an environment.

## 2014/12/22

* Documented the Coq back-end (designed and implemented by Jacques-Henri Jourdan).

## 2014/12/15

* New incremental API (in `--table` mode only), inspired by Frédéric Bour.

## 2014/12/11

* Menhir now reports an error if one of the start symbols produces
  either the empty language or the singleton language {epsilon}.

* Although some people out there actually define a start symbol that recognizes
  {epsilon} (and use it as a way of initializing or re-initializing some global
  state), this is considered bad style. Furthermore, by ruling out this case, we
  are able to simplify the table back-end a little bit.

## 2014/12/12

* A speed improvement in the code back-end.

## 2014/12/08

* Menhir now requires OCaml 4.02 (instead of 3.09).

## 2014/12/02

* Removed support for the `$previouserror` keyword.
* Removed support for `--error-recovery` mode.

## 2014/02/18

* In the Coq back-end, use `'` instead of `_` as separator in identifiers.
  Also, correct a serious bug that was inadvertently introduced on
  2013/03/01 (r319).

## 2014/02/14

* Lexer fix so as to support an open variant type `[> ...]` within
  a `%type<...>` declaration.

## 2013/12/16

* Updated the `Makefile` so that `install` no longer depends on `all`.

* Updated the demos so that the lexer does not invoke `exit 0`
  when encoutering `eof`. (This should be more intuitive.)

## 2013/09/11

* Fixed a newline conversion problem that would prevent Menhir from
  building on Windows when using ocaml 4.01.

## 2013/03/02

* Switched to ocamlbuild. Many thanks to Daniel Weil for offering
  very useful guidance.

## 2013/01/16

* `menhir --depend` was broken since someone added new whitespace in the output
  of `ocamldep`. Fixed.

## 2012/12/19

* Fixed a compilation problem that would arise when a file produced
  by Menhir on a 64-bit platform was compiled by ocaml on a 32-bit
  platform.

## 2012/08/25

* Performance improvements in the computation of various information
  about the automaton (module `Invariant`). The improvements will be
  noticeable only for very large automata.

## 2012/06/07

* The option `--log-grammar 3` (and above) now causes the `FOLLOW` sets for
  terminal symbols to be computed and displayed.

## 2012/05/25

* Added the flag `--canonical`, which causes Menhir to produce a canonical LR(1)
  automaton in the style of Knuth. This means that no merging of states takes
  place during the construction of the automaton, and that no default reductions
  are allowed.

## 2012/01/23

* Fixed a bug whereby a `%nonassoc` declaration was not respected. This
  declaration requests that a shift/reduce conflict be reduced in favor of
  neither shifting nor reducing, that is, a syntax error must occur. However,
  due to an unforeseen interaction with the default reduction mechanism, this
  declaration was sometimes ignored and reduction would take place.

## 2012/01/09

* Changes in the (undocumented) Coq back-end so as to match the ESOP 2012
  paper.

## 2011/10/19

* The `Makefile` now tests whether Unix or Windows is used (the test is performed
  by evaluating `Sys.os_type` under `ocaml`) and changes a couple settings accordingly:
  * the executable file name is either `menhir` or `menhir.exe`
  * the object file suffix is either `.o` or `.obj`

* Added `--strict`, which causes many warnings about the grammar and about the
  automaton to be considered errors.

* The `#` annotations that are inserted in the generated `.ml` file now retain their
  full path. (That is, we no longer use `Filename.basename`.) This implies that
  the `#` annotations depend on how Menhir is invoked
  -- e.g., `menhir foo/bar.mly` and `cd foo && menhir bar.mly` will produce different
  results. Nevertheless, this seems reasonable and useful (e.g., in conjunction
  with `ocamlbuild` and a hierarchy of files). Thanks to Daniel Weil.

## 2011/10/06

* With the `-lg 1` switch, Menhir now indicates whether the grammar is SLR(1).

## 2011/05/24

* Removed the lock in `ocamldep.wrapper`. It is the responsibility of the user
  to avoid interferences with other processes (or other instances of the script)
  that create and/or remove files.

## 2011/04/28

* The (internal) computation of the automaton's invariant was broken and has
  been fixed. Surprisingly, this does not seem to affect the generated code,
  (which was correct,) so no observable bug is fixed. Hopefully no bug is
  introduced!

## 2011/04/07

* The grammar description files (`.mly`) are now read in up front and stored in
  memory while they are parsed. This allows us to avoid the use of `pos_in` and
  `seek_in`, which do not work correctly when CRLF conversion is being performed.

## 2011/04/05

* Fixed a bug in the type inference module (for parameterized non-terminals)
  which would cause an infinite loop.

## 2011/01/24

* Fixed a bug that would cause an assertion failure in the generated parser
  in some situations where the input stream was incorrect and the grammar
  involved the error token. The fix might cause grammars that use the error
  token to behave differently (hopefully more accurately) as of now.

## 2009/06/18

* `Makefile` changes: build and install only the bytecode version of MenhirLib
  when `TARGET=byte` is set.

## 2009/02/06

* Fixed `ocamldep.wrapper` to avoid quoting the name of the `ocaml` command.
  This is hoped to fix a compilation problem under MinGW.

## 2009/02/04

* A `Makefile` fix to avoid a problem under Windows/Cygwin.
* Renamed the `ocaml-check-version` script so as to avoid a warning.

## 2008/09/05

* Ocaml summer project: added `--interpret`, `--table`, and `--suggest-*`.

## 2008/08/06

* Fixed a problem that would cause the code inliner to abort when a semantic
  value and a non-terminal symbol happened to have the same name.

* Removed code sharing.

## 2008/06/20

* Removed an incorrect assertion that caused failures (`lr1.ml`, line 134).

## 2007/12/05

* Disabled code sharing by default, as it is currently broken. (See Yann's
  message; assertion failure at runtime.)

## 2007/12/01

* Added an optimization to share code among states that have identical
  outgoing transition tables.

## 2007/08/30

* Small `Makefile` change: create an executable file for `check-ocaml-version` in
  order to work around the absence of dynamic loading on some platforms.

## 2007/05/20

* Made a fundamental change in the construction of the LR(1) automaton
  in order to eliminate a bug that could lead to spurious conflicts --
  thanks to Ketti for submitting a bug report.

## 2007/05/18

* Added `--follow-construction` to help understand the construction of the
  LR(1) automaton (very verbose).

## 2007/05/11

* Code generation: more explicit qualifications with `Pervasives` so as
  to avoid capture when the user redefines some of the built-in operators,
  such as `(+)`.
* Added a new demo (`calc-param`) that shows how to use `%parameter`.

## 2007/03/22

* `Makefile` improvements (check for `PREFIX`; bootstrap in bytecode now
  also available). Slight changes to `OMakefile.shared`.

## 2007/02/15

* Portability fix in `Makefile` and `Makefile.shared` (avoided `which`).

## 2006/12/15

* Portability fix in `Makefile.shared` (replaced `&>` with `2>&1 >`).

## 2006/06/23

* Made a slight restriction to Pager's criterion so as to never introduce
  fake conflict tokens (see `Lr0.compatible`). This might help make conflict
  explanations more accurate in the future.

## 2006/06/16

* Fixed bug that would cause positions to become invalid after inlining.

## 2006/06/15

* Fixed `--depend` to be more lenient when analyzing `ocamldep`'s output.
* Added `--raw-depend` which transmits `ocamldep`'s output unchanged (for
  use in conjunction with `omake`).

## 2006/06/12

* Fixed bug that would cause `--only-preprocess` to print `%token` declarations
  also for pseudo-tokens.
* Fixed bug that caused some precedence declarations to be incorrectly
  reported as useless.
* Improved things so that useless pseudo-tokens now also cause warnings.
* Fixed bug that would cause `%type` directives for terminal symbols to
  be incorrectly accepted.
* Fixed bug that would occur when a semantic action containing `$i` keywords
  was inlined.

## 2006/05/05

* Fixed problem that caused some end-of-stream conflicts not to be reported.
* Fixed Pager's compatibility criterion to avoid creating end-of-stream conflicts.

## 2006/04/21

* Fixed problem that allowed generating incorrect but apparently well-typed
  Objective Caml code when a semantic action was ill-typed and `--infer` was
  omitted.

## 2006/03/29

* Improved conflict reports by factoring out maximal common derivation contexts.

## 2006/03/28

* Fixed bug that could arise when explaining a conflict in a non-LALR(1) grammar.

## 2006/03/27

* Changed count of reduce/reduce conflicts to allow a comparison
  with `ocamlyacc`'s diagnostics.
* When refusing to resolve a conflict, report all diagnostics before dying.

## 2006/03/18

* Added display of `FOLLOW` sets when using `--log-grammar 2`.
* Added `--graph` option.
* Fixed behavior of `--depend` option.

## 2006/01/06

* Removed reversed lists from the standard library.
