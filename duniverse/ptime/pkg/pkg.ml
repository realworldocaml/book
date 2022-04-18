#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "ptime" @@ fun c ->
  Ok [ Pkg.mllib "src/ptime.mllib";
       Pkg.mllib ~api:[] "src/ptime_top.mllib" ~dst_dir:"top/";
       Pkg.lib "src/ptime_top_init.ml";
       Pkg.lib ~exts:Exts.interface "src/ptime_clock" ~dst:"clock/";
       Pkg.mllib "src-clock/ptime_clock.mllib" ~dst_dir:"clock/os/";
       Pkg.clib "src-clock/libptime_clock_stubs.clib" ~lib_dst_dir:"clock/os/";
       Pkg.lib "src-clock/runtime.js" ~dst:"clock/os/";
       Pkg.test "test/test";
       Pkg.test "test/test_unix";
       Pkg.test "test/basics";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "test/min_clock.ml"; ]
