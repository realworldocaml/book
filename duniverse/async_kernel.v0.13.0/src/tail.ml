open! Core_kernel
open! Import
module Deferred = Deferred1

module Stream = struct
  type 'a t = 'a next Deferred.t

  and 'a next = 'a Types.Stream.next =
    | Nil
    | Cons of 'a * 'a t

  let sexp_of_t sexp_of_a t =
    let rec loop d ac : Sexp.t =
      match Deferred.peek d with
      | None -> List (List.rev (Sexp.Atom "..." :: ac))
      | Some Nil -> List (List.rev ac)
      | Some (Cons (a, t)) -> loop t (sexp_of_a a :: ac)
    in
    loop t []
  ;;

  let next t = t
end

type 'a t = 'a Types.Tail.t =
  { (* [next] points at the tail of the stream *) mutable next : 'a Stream.next Ivar.t }
[@@deriving fields]

let sexp_of_t _ t : Sexp.t =
  Atom (if Ivar.is_empty t.next then "<open tail>" else "<closed tail>")
;;

let create () = { next = Ivar.create () }
let collect t = Ivar.read (next t)
let is_closed t = Ivar.is_full (next t)

let fill_exn t v =
  if is_closed t then raise_s [%message "stream is closed"] else Ivar.fill (next t) v
;;

let close_exn t = fill_exn t Nil
let close_if_open t = if not (is_closed t) then Ivar.fill (next t) Nil

let extend t v =
  let next = Ivar.create () in
  fill_exn t (Cons (v, Ivar.read next));
  t.next <- next
;;
