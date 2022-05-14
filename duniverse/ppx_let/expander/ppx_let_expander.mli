open Ppxlib

module Extension_kind : sig
  type t =
    { do_open : bool
    ; collapse_binds : bool
    }

  (* let%bind, let%map, etc. *)
  val default : t

  (* let%bind_open, let%map_open, etc. *)
  val default_open : t

  (* let%bindn, let%mapn, etc. *)
  val n : t

  (* let%bindn_open, let%mapn_open, etc. *)
  val n_open : t
end

type t

val ext_full_name : t -> Extension_kind.t -> label
val bind : t
val map : t
val sub : t
val arr : t

(* Bind each non-wildcard variable of each pattern to the expression which
   projects the bound expression to the variable's component. *)
val project_pattern_variables
  :  assume_exhaustive:bool
  -> modul:longident loc option
  -> with_location:bool
  -> value_binding list
  -> value_binding loc list

(* Produces a match-like expression which first matches on the index of the
   inhabited case, and then on the case itself. [switch] is used to create a
   match-like statement, and [destruct] is used to bind each of the variables
   in the case patterns so that they are available in the case bodies *)
val indexed_match
  :  loc:location
  -> modul:longident loc option
  -> destruct:
       (assume_exhaustive:bool
        -> loc:location
        -> modul:longident loc option
        -> lhs:pattern
        -> rhs:expression
        -> body:expression
        -> expression option)
  -> switch:
       (loc:location
        -> modul:longident loc option
        -> expression
        -> case list
        -> expression)
  -> expression
  -> case list
  -> expression

val qualified_return
  :  loc:location
  -> modul:longident loc option
  -> expression
  -> expression

val expand
  :  t
  -> Extension_kind.t
  -> modul:longident loc option
  -> expression
  -> expression
