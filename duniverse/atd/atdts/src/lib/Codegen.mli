(*
   TypeScript code generation for JSON support (no biniou support)
*)

(** Take ATD type definitions and translate them to TypeScript, writing
    them out to a file which should have the '.ts' extension. *)
val run_file : string -> unit
