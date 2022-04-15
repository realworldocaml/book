open B0_kit.V000
open B00_std
open Result.Syntax

(* OCaml library names *)

let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"

let rresult = B0_ocaml.libname "rresult"
let rresult_top = B0_ocaml.libname "rresult.top"

(* Libraries *)

let result_lib =
  let srcs =
    Fpath.[ `File (v "src/rresult.mli"); `File (v "src/rresult.ml"); ]
  in
  let requires = [] in
  B0_ocaml.lib rresult ~doc:"The rresult library" ~srcs ~requires

let rresult_top =
  let srcs = Fpath.[ `File (v "src/rresult_top.ml") ] in
  let requires = [compiler_libs_toplevel] in
  B0_ocaml.lib rresult_top ~doc:"The rresult.top library" ~srcs ~requires

(* Tests *)

let test =
  let srcs = Fpath.[`File (v "test/test.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ rresult ] in
  B0_ocaml.exe "test" ~doc:"Test suite" ~srcs ~meta ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> tag B0_opam.tag
    |> add authors ["The rresult programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/rresult"
    |> add online_doc "https://erratique.ch/software/rresult/doc/Rresult"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/rresult.git"
    |> add issues "https://github.com/dbuenzli/rresult/issues"
    |> add description_tags ["result"; "error"; "org:erratique"]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
  in
  B0_pack.v "default" ~doc:"rresult package" ~meta ~locked:true @@
  B0_unit.list ()
