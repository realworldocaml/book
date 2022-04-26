#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let unix = Conf.with_pkg "base-unix"
let cmdliner = Conf.with_pkg "cmdliner"

let () =
  Pkg.describe "fmt" @@ fun c ->
  let unix = Conf.value c unix in
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib "src/fmt.mllib";
       Pkg.mllib ~cond:unix "src/fmt_tty.mllib";
       Pkg.mllib ~cond:cmdliner "src/fmt_cli.mllib";
       Pkg.mllib ~api:[] "src/fmt_top.mllib";
       Pkg.lib "src/fmt_tty_top_init.ml";
       Pkg.test "test/test";
       Pkg.test "test/styled_perf_bug";
     ]
