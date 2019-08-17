(* This modules re-export the stdlib functions we need in the generated code, as
   [Pervasives] might be shadowed by [Base] for instance. *)

(* It has to be defined as an external so that we get warning 5 properly. *)
external ignore : _ -> unit = "%ignore"
