#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "jsonm" @@ fun c ->
  Ok [ Pkg.mllib "src/jsonm.mllib";
       Pkg.bin "test/jsontrip";
       Pkg.doc "test/examples.ml";
       Pkg.doc "test/jtree.ml";
       Pkg.test "test/test";
       Pkg.test ~run:false "test/examples";
       Pkg.test ~run:false "test/jtree"; ]
