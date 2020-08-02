#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "astring" @@ fun c ->
  Ok [ Pkg.mllib ~api:["Astring"] "src/astring.mllib";
       Pkg.mllib ~api:[] "src/astring_top.mllib";
       Pkg.lib "src/astring_top_init.ml";
       Pkg.doc "test/examples.ml";
       Pkg.test "test/test";
       Pkg.test "test/examples"; ]
