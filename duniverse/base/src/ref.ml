open! Import

include (
struct
  type 'a t = 'a ref [@@deriving_inline compare, equal, sexp, sexp_grammar]

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int = compare_ref
  let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool = equal_ref
  let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t = ref_of_sexp
  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t = sexp_of_ref

  let (t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t) =
    fun _'a_sexp_grammar -> ref_sexp_grammar _'a_sexp_grammar
  ;;

  [@@@end]
end :
sig
  type 'a t = 'a ref [@@deriving_inline compare, equal, sexp, sexp_grammar]

  include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t
  include Ppx_compare_lib.Equal.S1 with type 'a t := 'a t
  include Sexplib0.Sexpable.S1 with type 'a t := 'a t

  val t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t

  [@@@end]
end)

(* In the definition of [t], we do not have [[@@deriving compare, sexp]] because
   in general, syntax extensions tend to use the implementation when available rather than
   using the alias.  Here that would lead to use the record representation [ { mutable
   contents : 'a } ] which would result in different (and unwanted) behavior.  *)
type 'a t = 'a ref = { mutable contents : 'a }

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
