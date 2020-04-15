(** OCaml-Json code generator. *)

val make_ocaml_files
  :opens:string list
  -> with_typedefs:bool
  -> with_create:bool
  -> with_fundefs:bool
  -> all_rec:bool
  -> std:bool
  -> unknown_field_handler:string option
  -> pos_fname:string option
  -> pos_lnum:int option
  -> type_aliases:string option
  -> force_defaults:bool
  -> preprocess_input:string option
  -> ocaml_version:(int * int) option
  -> pp_convs:Ocaml.pp_convs
  -> string option
  -> Ox_emit.target
  -> unit
