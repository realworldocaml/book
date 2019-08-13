open! Core_kernel
open! Import

type 'a u =
  | Empty of 'a Ivar.t
  | Full
[@@deriving sexp_of]

type 'a t = 'a u ref [@@deriving sexp_of]

let invariant _ t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    match !t with
    | Full -> ()
    | Empty ivar -> assert (Ivar.is_empty ivar))
;;

let create () =
  let ivar = Ivar.create () in
  let t = ref (Empty ivar) in
  t, Ivar.read ivar
;;

let is_empty t =
  match !t with
  | Empty _ -> true
  | Full -> false
;;

let fill t a =
  match !t with
  | Empty i ->
    t := Full;
    Ivar.fill i a
  | Full -> raise_s [%message "attempt to fill full ivar"]
;;
