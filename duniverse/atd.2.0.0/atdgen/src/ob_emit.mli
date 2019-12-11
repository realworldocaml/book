(** OCaml-Biniou code generator. *)

val make_ocaml_files
  : opens:string list
  -> with_typedefs:bool
  -> with_create:bool
  -> with_fundefs:bool
  -> all_rec:bool
  -> pos_fname:string option
  -> pos_lnum:int option
  -> type_aliases:string option
  -> force_defaults:_ (* not used *)
  -> ocaml_version:(int * int) option
  -> pp_convs:Ocaml.pp_convs
  -> string option -> Ox_emit.target -> unit
