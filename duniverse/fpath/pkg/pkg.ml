#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "fpath" @@ fun c ->
  Ok [ Pkg.mllib ~api:["Fpath"] "src/fpath.mllib";
       Pkg.mllib ~api:[] "src/fpath_top.mllib";
       Pkg.lib "src/fpath_top_init.ml";
       Pkg.test "test/test"; ]
