open B0_kit.V000
open B00_std
open Result.Syntax

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"
let rresult = B0_ocaml.libname "rresult"
let rresult_top = B0_ocaml.libname "rresult.top"
let astring = B0_ocaml.libname "astring"
let astring_top = B0_ocaml.libname "astring.top"
let fpath = B0_ocaml.libname "fpath"
let fpath_top = B0_ocaml.libname "fpath.top"
let fmt = B0_ocaml.libname "fmt"
let fmt_top = B0_ocaml.libname "fmt.tty"
let fmt_tty = B0_ocaml.libname "fmt.tty"
let logs = B0_ocaml.libname "logs"
let logs_fmt = B0_ocaml.libname "logs.fmt"
let logs_top = B0_ocaml.libname "logs.top"
let mtime = B0_ocaml.libname "mtime"
let mtime_clock_os = B0_ocaml.libname "mtime.clock.os"

let bos = B0_ocaml.libname "bos"
let bos_setup = B0_ocaml.libname "bos.setup"
let bos_top = B0_ocaml.libname "bos.top"

(* Libraries *)

let bos_lib =
  let srcs =
    Fpath.[ `Dir (v "src");
            `X (v "src/bos_setup.ml");
            `X (v "src/bos_setup.mli");
            `X (v "src/bos_top.ml");
            `X (v "src/bos_top_init.ml") ]
  in
  let requires = [rresult; astring; fpath; fmt; unix; logs]
  in
  B0_ocaml.lib bos ~doc:"The bos library" ~srcs ~requires

let bos_setup_lib =
  let srcs = Fpath.[ `File (v "src/bos_setup.ml");
                     `File (v "src/bos_setup.mli") ]
  in
  let requires = [rresult; fmt_tty; logs_fmt; astring; fpath; logs; fmt; bos]
  in
  B0_ocaml.lib bos_setup ~doc:"The bos.setup library" ~srcs ~requires

let bos_top_lib =
  let srcs = Fpath.[ `File (v "src/bos_top.ml") ] in
  let requires =
    [ rresult_top; astring_top; fpath_top; fmt_top; logs_top;
      compiler_libs_toplevel]
  in
  B0_ocaml.lib bos_top ~doc:"The bos.top library" ~srcs ~requires

(* Tools *)

(* Tests *)

let test =
  let srcs =
    Fpath.[ `File (v "test/testing.mli");
            `File (v "test/testing.ml");
            `File (v "test/test.ml");
            `File (v "test/test_cmd.ml");
            `File (v "test/test_os_cmd.ml");
            `File (v "test/test_pat.ml"); ]
  in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ rresult; astring; fpath; logs_fmt; bos] in
  B0_ocaml.exe "test" ~doc:"Test suite" ~srcs ~meta ~requires

let test_arg =
  let srcs = Fpath.[ `File (v "test/test_arg.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ astring; fmt; fpath; logs_fmt; bos ] in
  B0_ocaml.exe "test-arg" ~doc:"Test argument parsing" ~srcs ~meta ~requires

let test_arg_pos =
  let srcs = Fpath.[ `File (v "test/test_arg_pos.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ fmt; logs_fmt; bos ] in
  B0_ocaml.exe "test-arg-pos" ~doc:"Test argument parsing" ~srcs ~meta ~requires

let watch =
  let srcs = Fpath.[`File (v "test/watch.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires =
    [ logs_fmt; fmt_tty; mtime; mtime_clock_os; rresult; fpath; bos; bos_setup ]
  in
  B0_ocaml.exe "watch" ~doc:"Watch files for changes." ~srcs ~meta ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The bos programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/bos"
    |> add online_doc "https://erratique.ch/software/bos/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/bos.git"
    |> add issues "https://github.com/dbuenzli/bos/issues"
    |> add description_tags
      ["os"; "system"; "cli"; "command"; "file"; "path"; "log"; "unix";
       "org:erratique"]
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "base-unix", "";
        "rresult", {|>= "0.7.0"|};
        "astring", "";
        "fpath", "";
        "fmt", {|>= "0.8.10"|};
        "logs", "";
        "mtime", {|test|};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.v "default" ~doc:"bos package" ~meta ~locked:true @@
  B0_unit.list ()
