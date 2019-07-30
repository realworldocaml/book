#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let jsoo = Conf.with_pkg "js_of_ocaml"

let () =
  Pkg.describe "mtime" @@ fun c ->
  let jsoo = Conf.value c jsoo in
  Ok [ Pkg.mllib "src/mtime.mllib";
       Pkg.mllib ~api:[] "src/mtime_top.mllib";
       Pkg.lib "src/mtime_top_init.ml";
       Pkg.mllib "src-os/mtime_clock.mllib" ~dst_dir:"os/";
       Pkg.clib "src-os/libmtime_clock_stubs.clib" ~lib_dst_dir:"os/";
       Pkg.mllib ~cond:jsoo "src-jsoo/mtime_clock.mllib" ~dst_dir:"jsoo";
(* Unable to find a way to convince ocamlbuild to make these work
   because of https://github.com/ocaml/ocamlbuild/issues/122

       Pkg.test "test-os/min_os";
       Pkg.test "test-os/test";
       Pkg.test ~run:false ~cond:jsoo ~auto:false "test-jsoo/test_jsoo.js";
       Pkg.test ~run:false ~cond:jsoo ~auto:false "test-jsoo/test_jsoo.html";
*)
       Pkg.doc "test-os/min_os.ml" ]
