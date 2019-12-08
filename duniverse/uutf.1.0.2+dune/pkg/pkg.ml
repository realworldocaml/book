#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let cmdliner = Conf.with_pkg "cmdliner"

let () =
  Pkg.describe "uutf" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib "src/uutf.mllib";
       Pkg.bin ~cond:cmdliner "test/utftrip";
       Pkg.test "test/examples";
       Pkg.test "test/test"; ]
