open! Import

type t =
  | Less
  | Equal
  | Greater
[@@deriving_inline compare, hash, enumerate, sexp, sexp_grammar]

let compare = (Ppx_compare_lib.polymorphic_compare : t -> t -> int)

let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
  (fun hsv arg ->
     match arg with
     | Less -> Ppx_hash_lib.Std.Hash.fold_int hsv 0
     | Equal -> Ppx_hash_lib.Std.Hash.fold_int hsv 1
     | Greater -> Ppx_hash_lib.Std.Hash.fold_int hsv 2
                  : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state)
;;

let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
  let func arg =
    Ppx_hash_lib.Std.Hash.get_hash_value
      (let hsv = Ppx_hash_lib.Std.Hash.create () in
       hash_fold_t hsv arg)
  in
  fun x -> func x
;;

let all = ([ Less; Equal; Greater ] : t list)

let t_of_sexp =
  (let error_source__005_ = "ordering.ml.t" in
   function
   | Sexplib0.Sexp.Atom ("less" | "Less") -> Less
   | Sexplib0.Sexp.Atom ("equal" | "Equal") -> Equal
   | Sexplib0.Sexp.Atom ("greater" | "Greater") -> Greater
   | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("less" | "Less") :: _) as sexp__006_ ->
     Sexplib0.Sexp_conv_error.stag_no_args error_source__005_ sexp__006_
   | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("equal" | "Equal") :: _) as sexp__006_ ->
     Sexplib0.Sexp_conv_error.stag_no_args error_source__005_ sexp__006_
   | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("greater" | "Greater") :: _) as sexp__006_ ->
     Sexplib0.Sexp_conv_error.stag_no_args error_source__005_ sexp__006_
   | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__004_ ->
     Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__005_ sexp__004_
   | Sexplib0.Sexp.List [] as sexp__004_ ->
     Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__005_ sexp__004_
   | sexp__004_ -> Sexplib0.Sexp_conv_error.unexpected_stag error_source__005_ sexp__004_
                   : Sexplib0.Sexp.t -> t)
;;

let sexp_of_t =
  (function
    | Less -> Sexplib0.Sexp.Atom "Less"
    | Equal -> Sexplib0.Sexp.Atom "Equal"
    | Greater -> Sexplib0.Sexp.Atom "Greater"
                 : t -> Sexplib0.Sexp.t)
;;

let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive_except_first_character
        ; clauses =
            [ No_tag { name = "Less"; clause_kind = Atom_clause }
            ; No_tag { name = "Equal"; clause_kind = Atom_clause }
            ; No_tag { name = "Greater"; clause_kind = Atom_clause }
            ]
        }
  }
;;

[@@@end]

let equal a b = compare a b = 0

module Export = struct
  type _ordering = t =
    | Less
    | Equal
    | Greater
end

let of_int n = if n < 0 then Less else if n = 0 then Equal else Greater

let to_int = function
  | Less -> -1
  | Equal -> 0
  | Greater -> 1
;;
