#!/usr/bin/env ocaml
#use "topfind";;
#require "topkg"
open Topkg

let () =
  Pkg.describe "rresult" @@ fun c ->
  Ok [ Pkg.mllib ~api:["Rresult"] "src/rresult.mllib";
       Pkg.mllib ~api:[] "src/rresult_top.mllib";
       Pkg.lib "src/rresult_top_init.ml";
       Pkg.test "test/test"; ]
