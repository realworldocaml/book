## PCRE-OCaml - Perl Compatibility Regular Expressions for OCaml

This [OCaml](http://www.ocaml.org)-library interfaces the C-library
[PCRE](http://www.pcre.org) (Perl-compatibility Regular Expressions).  It can be
used for string matching with "PERL"-style regular expressions.

### Features

PCRE-OCaml offers the following functionality for operating on strings:

  * Searching for patterns
  * Extracting subpatterns
  * Splitting strings according to patterns
  * Pattern substitution

Other reasons to use PCRE-OCaml:

  * The PCRE-library by Philip Hazel has been under development for many
    years and is fairly advanced and stable.  It implements just about all
    of the functionality that can be found in PERL regular expressions.
    The higher-level functions written in OCaml (split, replace, etc.),
    too, are compatible with the corresponding PERL-functions to the extent
    that OCaml allows.  Most people find the syntax of PERL-style regular
    expressions more straightforward and powerful than the Emacs-style regular
    expressions used in the `Str`-module in the standard OCaml distribution.

  * PCRE-OCaml is reentrant and thus thread-safe, which is not the case
    for the `Str`-module in the OCaml standard library.  Using reentrant
    libraries also means more convenience for programmers.  They do not
    have to reason about states in which the library might be in.

  * The high-level functions for replacement and substitution, which are all
    implemented in OCaml, are much faster than the ones in the `Str`-module.
    In fact, when compiled to native code, they even seem to be significantly
    faster than those found in PERL (PERL is written in C).

  * You can rely on the data returned being unique.  In other terms: if
    the result of a function is a string, you can safely use destructive
    updates on it without having to fear side effects.

  * The interface to the library makes use of labels and default arguments
    to give you a high degree of programming comfort.

### Usage

Please consult the [API](https://mmottl.github.io/pcre-ocaml/api/pcre)
for details.

A general concept the user may need to understand is that most functions
allow for two different kinds of flags:

  1. "Convenience"-flags that make for readable and concise code, but which
     need to be translated to an internal representation on each call.
     Example:

     ```ocaml
     let rex = Pcre.regexp ~flags:[`ANCHORED; `CASELESS] "some pattern" in
     (* ... *)
     ```

     This makes it easy to pass flags on the fly.  They will be translated to
     the internal format automatically.  However, if this happens to be in a
     loop, this translation will occur on each iteration.  If you really need
     to save as much performance as possible, you should use the next approach.

  2. "Internal" flags that need to be defined and translated from
     "convenience"-flags before function calls, but which allow for optimum
     performance in loops.  Example:

     ```ocaml
     let iflags = Pcre.cflags [`ANCHORED; `CASELESS] in
     for i = 1 to 1000 do
       let rex = Pcre.regexp ~iflags "some pattern constructed at runtime" in
       (* ... *)
     done
     ```

      Factoring out the translation of flags for regular expressions may
      save some cycles, but don't expect too much.  You can save more CPU
      time when lifting the creation of regular expressions out of loops.
      Example for what not to do:

      ```ocaml
      for i = 1 to 1000 do
        let chunks = Pcre.split ~pat:"[ \t]+" "foo bar" in
        (* ... *)
      done
      ```

      Better:

      ```ocaml
      let rex = Pcre.regexp "[ \t]+" in
      for i = 1 to 1000 do
        let chunks = Pcre.split ~rex "foo bar" in
        (* ... *)
      done
      ```

The provided functions use optional arguments with intuitive defaults.  For
example, the `Pcre.split`-function will assume whitespace as pattern.  The
`examples`-directory contains a few example applications demonstrating the
functionality of PCRE-OCaml.

#### Restartable (partial) pattern matching

PCRE includes an "alternative" DFA match function that allows one to restart
a partial match with additional input.  This is exposed by `pcre-ocaml` via
the `pcre_dfa_exec` function.  While this cannot be used for "higher-level"
operations like extracting submatches or splitting subject strings, it can
be very useful in certain streaming and search use cases.

This `utop` interaction demonstrates the basic workflow of a partial match
that is then restarted multiple times before completing successfully:

```ocaml
utop # open Pcre;;
utop # let rex = regexp "12+3";;
val rex : regexp = <abstr>
utop # let workspace = Array.make 40 0;;
val workspace : int array =
  [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
    0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
utop # pcre_dfa_exec ~rex ~flags:[`PARTIAL] ~workspace "12222";;
Exception: Pcre.Error Partial.
utop # pcre_dfa_exec ~rex ~flags:[`PARTIAL; `DFA_RESTART] ~workspace "2222222";;
Exception: Pcre.Error Partial.
utop # pcre_dfa_exec ~rex ~flags:[`PARTIAL; `DFA_RESTART] ~workspace "2222222";;
Exception: Pcre.Error Partial.
utop # pcre_dfa_exec ~rex ~flags:[`PARTIAL; `DFA_RESTART] ~workspace "223xxxx";;
- : int array = [|0; 3; 0|]
```

Please refer to the documentation of `pcre_dfa_exec` and check out the
`dfa_restart` example for more info.

### Contact Information and Contributing

Please submit bugs reports, feature requests, contributions and similar to
the [GitHub issue tracker](https://github.com/mmottl/pcre-ocaml/issues).

Up-to-date information is available at: <https://mmottl.github.io/pcre-ocaml>
