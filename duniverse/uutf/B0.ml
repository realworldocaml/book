open B0_kit.V000
open B00_std


(* OCaml library names *)

let uutf = B0_ocaml.libname "uutf"
let unix = B0_ocaml.libname "unix"
let cmdliner = B0_ocaml.libname "cmdliner"

(* Libraries *)

let uutf_lib =
  let srcs = Fpath.[`Dir (v "src")] in
  let requires = [] in
  B0_ocaml.lib uutf ~doc:"The uutf library" ~srcs ~requires

(* Tests *)

let test =
  let srcs = Fpath.[`File (v "test/test.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ uutf ] in
  B0_ocaml.exe "test" ~doc:"Test suite" ~srcs ~meta ~requires

let utftrip =
  let doc = "Recode UTF-{8,16,16LE,16BE} and latin1 from stdin to stdout" in
  let srcs = Fpath.[`File (v "test/utftrip.ml")] in
  let requires = [unix; uutf; cmdliner] in
  B0_ocaml.exe "utftrip" ~doc ~srcs ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> tag B0_opam.tag
    |> add authors ["The uutf programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/uutf"
    |> add online_doc "https://erratique.ch/software/uutf/doc/"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/uutf.git"
    |> add issues "https://github.com/dbuenzli/uutf/issues"
    |> add description_tags
      ["unicode"; "text"; "utf-8"; "utf-16"; "codec"; "org:erratique"]
    |> add B0_opam.Meta.depopts ["cmdliner", ""]
    |> add B0_opam.Meta.conflicts
      [ "cmdliner", {|< "0.9.8"|}]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.03.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-cmdliner" "%{cmdliner:installed}%"]]|}
  in
  B0_pack.v "default" ~doc:"uutf package" ~meta ~locked:true @@
  B0_unit.list ()
