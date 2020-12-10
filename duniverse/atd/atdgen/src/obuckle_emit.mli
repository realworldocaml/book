
val make_ocaml_files
  :  opens:string list
  -> with_typedefs:bool
  -> with_create:bool
  -> with_fundefs:bool
  -> all_rec:bool
  -> pos_fname:string option
  -> pos_lnum:int option
  -> type_aliases:string option
  -> force_defaults:'a
  -> ocaml_version:'b
  -> pp_convs:'c
  -> string option
  -> Ox_emit.target
  -> unit
