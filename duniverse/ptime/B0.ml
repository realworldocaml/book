open B0_kit.V000
open B00_std
open Result.Syntax

(* OCaml library names *)

let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"
let unix = B0_ocaml.libname "unix"

let ptime = B0_ocaml.libname "ptime"
let ptime_top = B0_ocaml.libname "ptime.top"
let ptime_clock = B0_ocaml.libname "ptime.clock"
let ptime_clock_os = B0_ocaml.libname "ptime.clock.os"

(* Libraries *)

let ptime_lib =
  let srcs = Fpath.[`File (v "src/ptime.mli"); `File (v "src/ptime.ml")] in
  let requires = [] in
  B0_ocaml.lib ptime ~doc:"The ptime library" ~srcs ~requires

let ptime_top =
  let srcs = Fpath.[`File (v "src/ptime_top.ml")] in
  let requires = [compiler_libs_toplevel] in
  B0_ocaml.lib ptime_top ~doc:"The ptime.top library" ~srcs ~requires

let ptime_clock =
  let srcs = Fpath.[`File (v "src/ptime_clock.mli")] in
  let requires = [ptime] in
  let doc = "The ptime.clock interface library" in
  B0_ocaml.lib ptime_clock ~doc ~srcs ~requires

let ptime_clock_os_lib =
  let srcs = Fpath.[`Dir (v "src-clock") ] in
  let requires = [ptime] in
  let doc = "The ptime.clock library (including JavaScript support)" in
  B0_ocaml.lib ptime_clock_os ~doc ~srcs ~requires

(* Tests *)

let in_test f = `File (Fpath.v ("test/" ^ f))

let basics =
  let srcs = [in_test "basics.ml"] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ptime] in
  B0_ocaml.exe "basics" ~doc:"Examples from the API docs" ~srcs ~meta ~requires

let test =
  let srcs =
    List.map in_test
      ["testing.mli"; "testing.ml"; "testing_ptime.ml"; "test_rand.ml";
       "test_span.ml"; "test_base.ml"; "test_date.ml";
       "test_date_time.ml"; "test_rfc3339.ml"; "test.ml" ]
  in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ ptime ] in
  B0_ocaml.exe "test" ~doc:"Test suite" ~srcs ~meta ~requires

let test_unix =
  let srcs = [in_test "testing.mli"; in_test "testing.ml";
              in_test "test_rand.ml"; in_test "testing_ptime.ml";
              in_test "test_unix.ml"]
  in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ptime; unix] in
  let doc = "Tests against Unix.gmtime" in
  B0_ocaml.exe "test-unix" ~doc ~srcs ~meta ~requires

let min_clock =
  let srcs = [in_test "min_clock.ml"] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ptime; ptime_clock_os] in
  let doc = "Minimal clock example" in
  B0_ocaml.exe "min-clock" ~doc ~srcs ~meta ~requires

(* FIXME b0 this forces the whole build to bytecode which is not
   what we want.
let min_clock_jsoo =
  let srcs = [in_test "min_clock.ml"] in
  let meta = B0_meta.(empty |> tag test) in
  let meta = B0_jsoo.meta ~requires:[ptime; ptime_clock_os] ~meta () in
  let doc = "Minimal clock example" in
  B0_jsoo.web "min-clock-jsoo" ~doc ~srcs ~meta
*)

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> tag B0_opam.tag
    |> add authors ["The ptime programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/ptime"
    |> add online_doc "https://erratique.ch/software/ptime/doc/"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/ptime.git"
    |> add issues "https://github.com/dbuenzli/ptime/issues"
    |> add description_tags
      ["time"; "posix"; "system"; "org:erratique"]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build & != "0.9.0"|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.v "default" ~doc:"ptime package" ~meta ~locked:true @@
  B0_unit.list ()
