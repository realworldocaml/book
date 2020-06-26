open! Import

(* In the definition of [t], we do not have [[@@deriving_inline compare, sexp][@@@end]] because
   in general, syntax extensions tend to use the implementation when available rather than
   using the alias.  Here that would lead to use the record representation [ { mutable
   contents : 'a } ] which would result in different (and unwanted) behavior.  *)
type 'a t = 'a ref = { mutable contents : 'a }

include (
struct
  type 'a t = 'a ref [@@deriving_inline compare, equal, sexp]
  let compare : 'a . ('a -> 'a -> int) -> 'a t -> 'a t -> int = compare_ref
  let equal : 'a . ('a -> 'a -> bool) -> 'a t -> 'a t -> bool = equal_ref
  let t_of_sexp :
    'a . (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> Ppx_sexp_conv_lib.Sexp.t -> 'a t =
    ref_of_sexp
  let sexp_of_t :
    'a . ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t =
    sexp_of_ref
  [@@@end]
end :
sig
  type 'a t = 'a ref [@@deriving_inline compare, equal, sexp]
  include
    sig
      [@@@ocaml.warning "-32"]
      val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
      include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t :=  'a t
    end[@@ocaml.doc "@inline"]
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
