open B0_kit.V000
open B00_std
open Result.Syntax

(* OCaml library names *)

let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"
let js_of_ocaml = B0_ocaml.libname "js_of_ocaml"

let mtime = B0_ocaml.libname "mtime"
let mtime_top = B0_ocaml.libname "mtime.top"
let mtime_clock_os = B0_ocaml.libname "mtime.clock.os"
let mtime_clock_jsoo = B0_ocaml.libname "mtime.clock.jsoo"

(* Libraries *)

let mtime_lib =
  let srcs = Fpath.[`File (v "src/mtime.mli"); `File (v "src/mtime.ml")] in
  let requires = [] in
  B0_ocaml.lib mtime ~doc:"The mtime library" ~srcs ~requires

let mtime_top =
  let srcs = Fpath.[`File (v "src/mtime_top.ml")] in
  let requires = [compiler_libs_toplevel] in
  B0_ocaml.lib mtime_top ~doc:"The mtime.top library" ~srcs ~requires

let mtime_clock_os_lib =
  let srcs = Fpath.[`Dir (v "src-os") ] in
  let requires = [mtime] in
  B0_ocaml.lib mtime_clock_os ~doc:"The mtime clock OS library" ~srcs ~requires

let mtime_clock_jsoo_lib =
  let srcs = Fpath.[`Dir (v "src-jsoo") ] in
  let requires = [mtime; js_of_ocaml] in
  let doc = "The mtime clock JSOO library" in
  B0_ocaml.lib mtime_clock_jsoo ~doc ~srcs ~requires

(* Tests *)

let test =
  let srcs = Fpath.[`File (v "test-os/test.ml");
                    `File (v "test-os/tests.ml")]
  in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ mtime; mtime_clock_os ] in
  B0_ocaml.exe "test" ~doc:"Test suite" ~srcs ~meta ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> tag B0_opam.tag
    |> add authors ["The mtime programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/mtime"
    |> add online_doc "https://erratique.ch/software/mtime/doc/"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/mtime.git"
    |> add issues "https://github.com/dbuenzli/mtime/issues"
    |> add description_tags
      ["time"; "monotonic"; "system"; "org:erratique"]
    |> add B0_opam.Meta.depopts ["js_of_ocaml", ""]
    |> add B0_opam.Meta.conflicts
      [ "js_of_ocaml", {|<= "3.3.0"|}]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.03.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-js_of_ocaml" "%{js_of_ocaml:installed}%"]]|}
  in
  B0_pack.v "default" ~doc:"mtime package" ~meta ~locked:true @@
  B0_unit.list ()
