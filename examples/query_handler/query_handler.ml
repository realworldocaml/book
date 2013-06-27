open Core.Std
open Async.Std
open Query_handler_core

let () =
  cli (build_dispatch_table [unique_instance; list_dir_instance])

(*
let load () =
  ignore (Ocaml_compiler.load_ocaml_src_files
            ["foo.ml"] : unit Or_error.t Deferred.t)
*)
