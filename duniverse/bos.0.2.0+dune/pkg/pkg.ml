#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "bos" @@ fun c ->
  Ok [ Pkg.mllib ~api:["Bos"] "src/bos.mllib";
       Pkg.mllib "src/bos_setup.mllib";
       Pkg.mllib ~api:[] "src/bos_top.mllib";
       Pkg.lib "src/bos_top_init.ml";
       Pkg.test "test/test";
       Pkg.test ~run:false "test/test_arg";
       Pkg.test ~run:false "test/test_arg_pos";
       Pkg.test ~run:false "test/watch"; ]
