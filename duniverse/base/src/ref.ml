open! Import

(* In the definition of [t], we do not have [[@@deriving compare, sexp]] because
   in general, syntax extensions tend to use the implementation when available rather than
   using the alias.  Here that would lead to use the record representation [ { mutable
   contents : 'a } ] which would result in different (and unwanted) behavior.  *)
type 'a t = 'a ref = { mutable contents : 'a }

include (
struct
  type 'a t = 'a ref [@@deriving_inline compare, equal, sexp, sexp_grammar]

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int = compare_ref
  let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool = equal_ref

  let t_of_sexp :
    'a. (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> Ppx_sexp_conv_lib.Sexp.t -> 'a t
    =
    ref_of_sexp
  ;;

  let sexp_of_t :
    'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t
    =
    sexp_of_ref
  ;;

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group)
      =
      { implicit_vars = [ "ref" ]
      ; ggid = "j\132);\135qH\158\135\222H\001\007\004\158\218"
      ; types =
          [ "t", Explicit_bind ([ "a" ], Apply (Implicit_var 0, [ Explicit_var 0 ])) ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ ref_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "ref.ml"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  [@@@end]
end :
sig
  type 'a t = 'a ref [@@deriving_inline compare, equal, sexp, sexp_grammar]

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t := 'a t

  val t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t

  [@@@end]
end
with type 'a t := 'a t)

external create : 'a -> 'a t = "%makemutable"
external ( ! ) : 'a t -> 'a = "%field0"
external ( := ) : 'a t -> 'a -> unit = "%setfield0"

let swap t1 t2 =
  let tmp = !t1 in
  t1 := !t2;
  t2 := tmp
;;

let replace t f = t := f !t

let set_temporarily t a ~f =
  let restore_to = !t in
  t := a;
  Exn.protect ~f ~finally:(fun () -> t := restore_to)
;;

module And_value = struct
  type t = T : 'a ref * 'a -> t [@@deriving sexp_of]

  let set (T (r, a)) = r := a
  let sets ts = List.iter ts ~f:set
  let snapshot (T (r, _)) = T (r, !r)
  let snapshots ts = List.map ts ~f:snapshot
end

let sets_temporarily and_values ~f =
  let restore_to = And_value.snapshots and_values in
  And_value.sets and_values;
  Exn.protect ~f ~finally:(fun () -> And_value.sets restore_to)
;;
