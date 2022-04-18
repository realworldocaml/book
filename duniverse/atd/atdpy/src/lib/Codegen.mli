(*
   Python code generation for JSON support (no biniou support)
*)

(** Take ATD type definitions and translate them to Python, writing
    them out to a file which should have the '.py' extension. *)
val run_file : string -> unit
