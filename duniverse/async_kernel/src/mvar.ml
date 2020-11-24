open! Core_kernel
open! Import
open! Deferred_std

type ('a, 'phantom) t =
  { current_value : 'a Moption.t
  ; taken : (unit, read_write) Bvar.t
  ; mutable value_available : unit Ivar.t
  }
[@@deriving fields, sexp_of]

let value_available t = Ivar.read t.value_available
let is_empty t = Moption.is_none t.current_value

let invariant invariant_a _ (t : _ t) =
  Invariant.invariant [%here] t [%sexp_of: (_, _) t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~current_value:(check (Moption.invariant invariant_a))
      ~taken:(check (Bvar.invariant Unit.invariant ignore))
      ~value_available:
        (check (fun value_available ->
           [%test_result: bool]
             (Ivar.is_full value_available)
             ~expect:(Moption.is_some t.current_value))))
;;

let peek t = Moption.get t.current_value

let peek_exn t =
  if is_empty t then raise_s [%message "Mvar.peek_exn called on empty mvar"];
  Moption.get_some_exn t.current_value
;;

let sexp_of_t sexp_of_a _ t = [%sexp (peek t : a option)]

module Read_write = struct
  type nonrec 'a t = ('a, read_write) t [@@deriving sexp_of]

  let invariant invariant_a t = invariant invariant_a ignore t
end

module Read_only = struct
  type nonrec 'a t = ('a, read) t [@@deriving sexp_of]

  let invariant invariant_a t = invariant invariant_a ignore t
end

let read_only (t : ('a, [> read ]) t) = (t :> ('a, read) t)
let write_only (t : ('a, [> write ]) t) = (t :> ('a, write) t)

let create () =
  { current_value = Moption.create ()
  ; taken = Bvar.create ()
  ; value_available = Ivar.create ()
  }
;;

let take_nonempty t =
  assert (not (is_empty t));
  let r = Moption.get_some_exn t.current_value in
  Moption.set_none t.current_value;
  Bvar.broadcast t.taken ();
  t.value_available <- Ivar.create ();
  r
;;

let take_now_exn t =
  if is_empty t then raise_s [%message "Mvar.take_exn called on empty mvar"];
  take_nonempty t
;;

let take_now t = if not (is_empty t) then Some (take_nonempty t) else None

let rec take t =
  if not (is_empty t)
  then return (take_nonempty t)
  else (
    let%bind () = value_available t in
    take t)
;;

let set t v =
  Moption.set_some t.current_value v;
  Ivar.fill_if_empty t.value_available ()
;;

let update t ~f = set t (f (peek t))
let update_exn t ~f = set t (f (peek_exn t))
let taken t = Bvar.wait t.taken

let rec put t v =
  if is_empty t
  then (
    set t v;
    return ())
  else (
    let%bind () = taken t in
    put t v)
;;

let pipe_when_ready t =
  let r, w = Pipe.create () in
  let rec loop () =
    let%bind () = value_available t in
    if not (Pipe.is_closed w)
    then (
      match take_now t with
      | None -> loop ()
      | Some x ->
        let%bind () = Pipe.write w x in
        loop ())
    else return ()
  in
  don't_wait_for (loop ());
  r
;;
