open! Core_kernel
open! Import

type ('a, 'permission) t = ('a, 'permission) Types.Bvar.t

type 'a repr = 'a Types.Bvar.repr =
  { mutable has_any_waiters : bool
  ; mutable ivar : 'a Ivar.t
  }
[@@deriving fields, sexp_of]

let invariant invariant_a _ t =
  let repr = Types.Bvar.to_repr t in
  Invariant.invariant [%here] repr [%sexp_of: _ repr] (fun () ->
    let check f = Invariant.check_field repr f in
    Fields_of_repr.iter
      ~has_any_waiters:
        (check (fun has_any_waiters ->
           if Ivar.has_handlers repr.ivar then assert has_any_waiters))
      ~ivar:
        (check (fun ivar ->
           Ivar.invariant invariant_a ivar;
           assert (Ivar.is_empty ivar))))
;;

let sexp_of_t _ _ t =
  let { has_any_waiters; ivar = _ } = Types.Bvar.to_repr t in
  (* We don't show [ivar] because it's always empty. *)
  [%message (has_any_waiters : bool)]
;;

include Scheduler1.Bvar

let broadcast t a =
  let repr = Types.Bvar.to_repr t in
  if repr.has_any_waiters
  then (
    repr.has_any_waiters <- false;
    Ivar.fill repr.ivar a;
    repr.ivar <- Ivar.create ())
;;

let wait t =
  let repr = Types.Bvar.to_repr t in
  repr.has_any_waiters <- true;
  Ivar.read repr.ivar
;;

let has_any_waiters t =
  let repr = Types.Bvar.to_repr t in
  repr.has_any_waiters
;;
