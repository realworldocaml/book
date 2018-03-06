  $ jbuilder build exn_cost.exe
        ocamlc .exn_cost.eobjs/exn_cost.{cmi,cmo,cmt} (exit 2)
  (cd _build/default && /Users/thomas/git/rwo/book/_opam/bin/ocamlc.opt -w -40 -g -bin-annot -I /Users/thomas/git/rwo/book/_opam/lib/base -I /Users/thomas/git/rwo/book/_opam/lib/base/caml -I /Users/thomas/git/rwo/book/_opam/lib/base/md5 -I /Users/thomas/git/rwo/book/_opam/lib/base/shadow_stdlib -I /Users/thomas/git/rwo/book/_opam/lib/bin_prot -I /Users/thomas/git/rwo/book/_opam/lib/bin_prot/shape -I /Users/thomas/git/rwo/book/_opam/lib/core -I /Users/thomas/git/rwo/book/_opam/lib/core_bench -I /Users/thomas/git/rwo/book/_opam/lib/core_kernel -I /Users/thomas/git/rwo/book/_opam/lib/core_kernel/base_for_tests -I /Users/thomas/git/rwo/book/_opam/lib/fieldslib -I /Users/thomas/git/rwo/book/_opam/lib/jane-street-headers -I /Users/thomas/git/rwo/book/_opam/lib/ocaml/threads -I /Users/thomas/git/rwo/book/_opam/lib/ppx_assert/runtime-lib -I /Users/thomas/git/rwo/book/_opam/lib/ppx_bench/runtime-lib -I /Users/thomas/git/rwo/book/_opam/lib/ppx_compare/runtime-lib -I /Users/thomas/git/rwo/book/_opam/lib/ppx_expect/collector -I /Users/thomas/git/rwo/book/_opam/lib/ppx_expect/common -I /Users/thomas/git/rwo/book/_opam/lib/ppx_expect/config -I /Users/thomas/git/rwo/book/_opam/lib/ppx_hash/runtime-lib -I /Users/thomas/git/rwo/book/_opam/lib/ppx_inline_test/config -I /Users/thomas/git/rwo/book/_opam/lib/ppx_inline_test/runtime-lib -I /Users/thomas/git/rwo/book/_opam/lib/sexplib -I /Users/thomas/git/rwo/book/_opam/lib/sexplib/0 -I /Users/thomas/git/rwo/book/_opam/lib/sexplib/unix -I /Users/thomas/git/rwo/book/_opam/lib/spawn -I /Users/thomas/git/rwo/book/_opam/lib/stdio -I /Users/thomas/git/rwo/book/_opam/lib/textutils -I /Users/thomas/git/rwo/book/_opam/lib/textutils_kernel -I /Users/thomas/git/rwo/book/_opam/lib/typerep -I /Users/thomas/git/rwo/book/_opam/lib/variantslib -no-alias-deps -I .exn_cost.eobjs -o .exn_cost.eobjs/exn_cost.cmo -c -impl exn_cost.ml)
  [1mFile "[1mexn_cost.ml", line 10, characters 33-37[0m[0m:
  [1;35mWarning[0m 3: deprecated: Exit
  [2016-09] this element comes from the stdlib distributed with OCaml.
  Refering to the stdlib directly is discouraged by Base. You should either
  use the equivalent functionality offered by Base, or if you really want to
  refer to the stdlib, use Caml.Exit instead
  [1mFile "[1mexn_cost.ml", line 15, characters 10-14[0m[0m:
  [1;35mWarning[0m 3: deprecated: Exit
  [2016-09] this element comes from the stdlib distributed with OCaml.
  Refering to the stdlib directly is discouraged by Base. You should either
  use the equivalent functionality offered by Base, or if you really want to
  refer to the stdlib, use Caml.Exit instead
  [1mFile "[1mexn_cost.ml", line 16, characters 7-11[0m[0m:
  [1;35mWarning[0m 3: deprecated: Exit
  [2016-09] this element comes from the stdlib distributed with OCaml.
  Refering to the stdlib directly is discouraged by Base. You should either
  use the equivalent functionality offered by Base, or if you really want to
  refer to the stdlib, use Caml.Exit instead
  [1mFile "[1mexn_cost.ml", line 21, characters 32-36[0m[0m:
  [1;35mWarning[0m 3: deprecated: Exit
  [2016-09] this element comes from the stdlib distributed with OCaml.
  Refering to the stdlib directly is discouraged by Base. You should either
  use the equivalent functionality offered by Base, or if you really want to
  refer to the stdlib, use Caml.Exit instead
  [1mFile "[1mexn_cost.ml", line 22, characters 7-11[0m[0m:
  [1;35mWarning[0m 3: deprecated: Exit
  [2016-09] this element comes from the stdlib distributed with OCaml.
  Refering to the stdlib directly is discouraged by Base. You should either
  use the equivalent functionality offered by Base, or if you really want to
  refer to the stdlib, use Caml.Exit instead
  [1mFile "[1mexn_cost.ml", line 35, characters 5-16[0m[0m:
  [1;31mError[0m: Unbound module Command
  [1]
  $ ./_build/default/exn_cost.exe -ascii cycles -quota 1
  sh: ./_build/default/exn_cost.exe: No such file or directory
  [127]
