  $ jbuilder build echo.exe
        ocamlc .echo.eobjs/echo.{cmi,cmo,cmt} (exit 2)
  (cd _build/default && /Users/thomas/git/rwo/book/_opam/bin/ocamlc.opt -w -40 -g -bin-annot -I /Users/thomas/git/rwo/book/_opam/lib/async -I /Users/thomas/git/rwo/book/_opam/lib/async_extra -I /Users/thomas/git/rwo/book/_opam/lib/async_kernel -I /Users/thomas/git/rwo/book/_opam/lib/async_rpc_kernel -I /Users/thomas/git/rwo/book/_opam/lib/async_unix -I /Users/thomas/git/rwo/book/_opam/lib/base -I /Users/thomas/git/rwo/book/_opam/lib/base/caml -I /Users/thomas/git/rwo/book/_opam/lib/base/md5 -I /Users/thomas/git/rwo/book/_opam/lib/base/shadow_stdlib -I /Users/thomas/git/rwo/book/_opam/lib/bin_prot -I /Users/thomas/git/rwo/book/_opam/lib/bin_prot/shape -I /Users/thomas/git/rwo/book/_opam/lib/core -I /Users/thomas/git/rwo/book/_opam/lib/core_kernel -I /Users/thomas/git/rwo/book/_opam/lib/core_kernel/base_for_tests -I /Users/thomas/git/rwo/book/_opam/lib/fieldslib -I /Users/thomas/git/rwo/book/_opam/lib/jane-street-headers -I /Users/thomas/git/rwo/book/_opam/lib/ocaml/threads -I /Users/thomas/git/rwo/book/_opam/lib/ppx_assert/runtime-lib -I /Users/thomas/git/rwo/book/_opam/lib/ppx_bench/runtime-lib -I /Users/thomas/git/rwo/book/_opam/lib/ppx_compare/runtime-lib -I /Users/thomas/git/rwo/book/_opam/lib/ppx_expect/collector -I /Users/thomas/git/rwo/book/_opam/lib/ppx_expect/common -I /Users/thomas/git/rwo/book/_opam/lib/ppx_expect/config -I /Users/thomas/git/rwo/book/_opam/lib/ppx_hash/runtime-lib -I /Users/thomas/git/rwo/book/_opam/lib/ppx_inline_test/config -I /Users/thomas/git/rwo/book/_opam/lib/ppx_inline_test/runtime-lib -I /Users/thomas/git/rwo/book/_opam/lib/protocol_version_header -I /Users/thomas/git/rwo/book/_opam/lib/sexplib -I /Users/thomas/git/rwo/book/_opam/lib/sexplib/0 -I /Users/thomas/git/rwo/book/_opam/lib/sexplib/unix -I /Users/thomas/git/rwo/book/_opam/lib/spawn -I /Users/thomas/git/rwo/book/_opam/lib/stdio -I /Users/thomas/git/rwo/book/_opam/lib/typerep -I /Users/thomas/git/rwo/book/_opam/lib/variantslib -no-alias-deps -I .echo.eobjs -o .echo.eobjs/echo.cmo -c -impl echo.ml)
  [1mFile "[1mecho.ml", line 11, characters 19-25[0m[0m:
  [1;31mError[0m: This expression has type Core.Bytes.t = bytes
         but an expression was expected of type string
  [1]
  $ ./_build/default/echo.exe &
  $ sleep 1
  sh: ./_build/default/echo.exe: No such file or directory
  $ echo "This is an echo server" | nc 127.0.0.1 8765
  [1]
  $ echo "It repeats whatever I write" | nc 127.0.0.1 8765
  [1]
  $ killall -9 echo.exe
  No matching processes belonging to you were found
  [1]
