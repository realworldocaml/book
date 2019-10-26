#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let jsoo = Conf.with_pkg "js_of_ocaml"
let cmdliner = Conf.with_pkg "cmdliner"
let fmt = Conf.with_pkg "fmt"
let lwt = Conf.with_pkg "lwt"
let threads = Conf.with_pkg "base-threads"

let () =
  Pkg.describe "logs" @@ fun c ->
  let jsoo = Conf.value c jsoo in
  let cmdliner = Conf.value c cmdliner in
  let fmt = Conf.value c fmt in
  let lwt = Conf.value c lwt in
  let threads = Conf.value c threads in
  Ok [ Pkg.mllib "src/logs.mllib";
       Pkg.mllib ~cond:fmt "src/logs_fmt.mllib";
       Pkg.mllib ~cond:jsoo "src/logs_browser.mllib";
       Pkg.mllib ~cond:cmdliner "src/logs_cli.mllib";
       Pkg.mllib ~cond:lwt "src/logs_lwt.mllib";
       Pkg.mllib ~cond:fmt ~api:[] "src/logs_top.mllib";
       Pkg.mllib ~cond:threads "src/logs_threaded.mllib";
       Pkg.lib "src/logs_top_init.ml";
       Pkg.lib "src/logs_fmt_top_init.ml";
       Pkg.doc "test/tool.ml";
       Pkg.doc "test/tags.ml";
       Pkg.test "test/test_fmt";
       Pkg.test "test/test_formatter";
       Pkg.test "test/tool";
       Pkg.test "test/tags";
       Pkg.test "test/test_multi";
(*       Pkg.test "test/test_browser.js";
         Pkg.test "test/test_browser.html"; *)
       Pkg.test "test/test_threaded";
       Pkg.test "test/test_lwt";
 ]
