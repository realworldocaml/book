#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mtime" @@ fun c ->
  Ok [ Pkg.mllib "src/mtime.mllib";
       Pkg.mllib ~api:[] "src/mtime_top.mllib" ~dst_dir:"top/";
       Pkg.lib "src/mtime_top_init.ml";
       Pkg.mllib "src-clock/mtime_clock.mllib" ~dst_dir:"clock/os/";
       Pkg.clib "src-clock/libmtime_clock_stubs.clib" ~lib_dst_dir:"clock/os/";
       Pkg.lib "src-clock/runtime.js" ~dst:"clock/os/";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "test/min_clock.ml"; ]
