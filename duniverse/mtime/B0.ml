open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"

let mtime = B0_ocaml.libname "mtime"
let mtime_top = B0_ocaml.libname "mtime.top"
let mtime_clock = B0_ocaml.libname "mtime.clock"
let mtime_clock_os = B0_ocaml.libname "mtime.clock.os"

(* Libraries *)

let mtime_lib =
  let srcs = Fpath.[`File (v "src/mtime.mli"); `File (v "src/mtime.ml")] in
  let requires = [] in
  B0_ocaml.lib mtime ~doc:"The mtime library" ~srcs ~requires

let mtime_top =
  let srcs = Fpath.[`File (v "src/mtime_top.ml")] in
  let requires = [compiler_libs_toplevel] in
  B0_ocaml.lib mtime_top ~doc:"The mtime.top library" ~srcs ~requires

let mtime_clock =
  let srcs = Fpath.[`File (v "src/mtime_clock.mli")] in
  let requires = [mtime] in
  let doc = "The mtime.clock interface library" in
  B0_ocaml.lib mtime_clock ~doc ~srcs ~requires

let mtime_clock_os_lib =
  let srcs = Fpath.[`Dir (v "src-clock") ] in
  let requires = [mtime] in
  let doc = "The mtime.clock library (including JavaScript support)" in
  B0_ocaml.lib mtime_clock_os ~doc ~srcs ~requires

(* Tests *)

let test =
  let srcs = Fpath.[`File (v "test/test.ml"); `File (v "test/tests.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ mtime; mtime_clock_os ] in
  B0_ocaml.exe "test" ~doc:"Test suite" ~srcs ~meta ~requires

let min_clock =
  let srcs = Fpath.[`File (v "test/min_clock.ml") ] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [mtime; mtime_clock_os] in
  let doc = "Minimal clock example" in
  B0_ocaml.exe "min-clock" ~doc ~srcs ~meta ~requires

(* FIXME b0 this forces the whole build to bytecode which is not
   what we want.
let min_clock_jsoo =
  let srcs = Fpath.[`File (v "test/min_clock.ml") ] in
  let meta = B0_meta.(empty |> tag test) in
  let meta = B0_jsoo.meta ~requires:[mtime; mtime_clock_os] ~meta () in
  let doc = "Minimal clock example" in
  B0_jsoo.web "min-clock-jsoo" ~doc ~srcs ~meta
*)

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
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build & != "0.9.0"|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.v "default" ~doc:"mtime package" ~meta ~locked:true @@
  B0_unit.list ()
