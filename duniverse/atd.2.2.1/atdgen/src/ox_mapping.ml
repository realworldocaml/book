open! Atd.Import
open Atd.Ast

type analyze_field =
  { ocaml_default : string option
  ; unwrapped : bool
  }

let analyze_field target loc (f_kind : field_kind) annot =
  let ocaml_default, unwrapped =
    match f_kind, Ocaml.get_ocaml_default target annot with
      Required, None -> None, false
    | Optional, None -> Some "None", true
    | (Required | Optional), Some _ ->
        Error.error loc "Superfluous default OCaml value"
    | With_default, Some s -> Some s, false
    | With_default, None ->
        (* will try to determine implicit default value later *)
        None, false
  in
  { ocaml_default
  ; unwrapped
  }
