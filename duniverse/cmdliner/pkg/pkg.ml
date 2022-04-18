#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let test t = Pkg.flatten [ Pkg.test ~run:false t; Pkg.doc (t ^ ".ml")]

let distrib =
  let exclude_paths () = Ok [".git";".gitignore";".gitattributes";"_build"] in
  Pkg.distrib ~exclude_paths ()

let opams =
  [Pkg.opam_file "cmdliner.opam"]

let () =
  Pkg.describe ~distrib "cmdliner" ~opams @@ fun c ->
  Ok [ Pkg.mllib ~api:["Cmdliner"] "src/cmdliner.mllib";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld";
       Pkg.doc "doc/cli.mld" ~dst:"odoc-pages/cli.mld";
       Pkg.doc "doc/examples.mld" ~dst:"odoc-pages/examples.mld";
       Pkg.doc "doc/tool_man.mld" ~dst:"odoc-pages/tool_man.mld";
       test "test/chorus";
       test "test/cp_ex";
       test "test/darcs_ex";
       test "test/revolt";
       test "test/rm_ex";
       test "test/tail_ex";
       Pkg.test ~run:false "test/test_dupe_stdopts";
       Pkg.test ~run:false "test/test_nest";
       Pkg.test ~run:false "test/test_man";
       Pkg.test ~run:false "test/test_man_utf8";
       Pkg.test ~run:false "test/test_pos";
       Pkg.test ~run:false "test/test_pos_rev";
       Pkg.test ~run:false "test/test_pos_all";
       Pkg.test ~run:false "test/test_pos_left";
       Pkg.test ~run:false "test/test_pos_req";
       Pkg.test ~run:false "test/test_opt_req";
       Pkg.test ~run:false "test/test_term_dups";
       Pkg.test ~run:false "test/test_with_used_args"; ]
