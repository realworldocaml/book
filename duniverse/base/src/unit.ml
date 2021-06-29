open! Import

module T = struct
  type t = unit [@@deriving_inline enumerate, hash, sexp, sexp_grammar]

  let all = ([ () ] : t list)

  let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_unit

  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_unit in
    fun x -> func x
  ;;

  let t_of_sexp = (unit_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
  let sexp_of_t = (sexp_of_unit : t -> Ppx_sexp_conv_lib.Sexp.t)

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "unit" ]
      ; ggid = "\146e\023\249\235eE\139c\132W\195\137\129\235\025"
      ; types = [ "t", Implicit_var 0 ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ unit_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "unit.ml.T"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  [@@@end]

  let compare _ _ = 0

  let of_string = function
    | "()" -> ()
    | _ -> failwith "Base.Unit.of_string: () expected"
  ;;

  let to_string () = "()"
  let module_name = "Base.Unit"
end

include T
include Identifiable.Make (T)

let invariant () = ()
