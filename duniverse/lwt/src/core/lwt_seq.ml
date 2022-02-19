(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Lwt.Syntax
open Lwt.Infix

type +'a node = Nil | Cons of 'a * 'a t

and 'a t = unit -> 'a node Lwt.t

let return_nil = Lwt.return Nil

let empty : 'a t = fun () -> return_nil

let return (x : 'a) : 'a t = fun () -> Lwt.return (Cons (x, empty))

let return_lwt (x : 'a Lwt.t) : 'a t = fun () ->
   let+ x = x in
   Cons (x, empty)

let cons x t () = Lwt.return (Cons (x, t))

let cons_lwt x t () =
   let+ x = x in
   Cons (x, t)

(* A note on recursing through the seqs:
   When traversing a seq, the first time we evaluate a suspended node we are
   on the left of the first bind (>>=). In that case, we use apply to capture
   exceptions into promise rejection.

   This is only needed on the first iteration because we are within a callback
   passed to Lwt on the right-hand side of a bind after that.

   Throughout this file we use the same code pattern to achieve this: we
   shadow the recursive traversal function with an identical-but-for-the-apply
   non-recursive copy. *)

let rec append seq1 seq2 () =
  seq1 () >>= function
  | Nil -> seq2 ()
  | Cons (x, next) -> Lwt.return (Cons (x, append next seq2))
let append seq1 seq2 () =
  Lwt.apply seq1 () >>= function
  | Nil -> seq2 ()
  | Cons (x, next) -> Lwt.return (Cons (x, append next seq2))

let rec map f seq () =
  seq () >|= function
  | Nil -> Nil
  | Cons (x, next) ->
      let x = f x in
      Cons (x, map f next)
let map f seq () =
  Lwt.apply seq () >|= function
  | Nil -> Nil
  | Cons (x, next) ->
      let x = f x in
      Cons (x, map f next)

let rec map_s f seq () =
  seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) ->
      let+ x = f x in
      Cons (x, map_s f next)
let map_s f seq () =
  Lwt.apply seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) ->
      let+ x = f x in
      Cons (x, map_s f next)

let rec filter_map f seq () =
  seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) -> (
      let x = f x in
      match x with
      | None -> filter_map f next ()
      | Some y -> Lwt.return (Cons (y, filter_map f next) ))
let filter_map f seq () =
  Lwt.apply seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) -> (
      let x = f x in
      match x with
      | None -> filter_map f next ()
      | Some y -> Lwt.return (Cons (y, filter_map f next) ))

let rec filter_map_s f seq () =
  seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) -> (
      let* x = f x in
      match x with
      | None -> filter_map_s f next ()
      | Some y -> Lwt.return (Cons (y, filter_map_s f next) ))
let filter_map_s f seq () =
  Lwt.apply seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) -> (
      let* x = f x in
      match x with
      | None -> filter_map_s f next ()
      | Some y -> Lwt.return (Cons (y, filter_map_s f next) ))

let rec filter f seq () =
  seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) ->
      let ok = f x in
      if ok then Lwt.return (Cons (x, filter f next)) else filter f next ()
let filter f seq () =
  Lwt.apply seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) ->
      let ok = f x in
      if ok then Lwt.return (Cons (x, filter f next)) else filter f next ()

let rec filter_s f seq () =
  seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) ->
      let* ok = f x in
      if ok then Lwt.return (Cons (x, filter_s f next)) else filter_s f next ()
let filter_s f seq () =
  Lwt.apply seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) ->
      let* ok = f x in
      if ok then Lwt.return (Cons (x, filter_s f next)) else filter_s f next ()

let rec flat_map f seq () =
  seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) ->
      flat_map_app f (f x) next ()

(* this is [append seq (flat_map f tail)] *)
and flat_map_app f seq tail () =
  seq () >>= function
  | Nil -> flat_map f tail ()
  | Cons (x, next) -> Lwt.return (Cons (x, flat_map_app f next tail))

let flat_map f seq () =
  Lwt.apply seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) ->
      flat_map_app f (f x) next ()

let fold_left f acc seq =
  let rec aux f acc seq =
    seq () >>= function
    | Nil -> Lwt.return acc
    | Cons (x, next) ->
        let acc = f acc x in
        aux f acc next
  in
  let aux f acc seq =
    Lwt.apply seq () >>= function
    | Nil -> Lwt.return acc
    | Cons (x, next) ->
        let acc = f acc x in
        aux f acc next
  in
  aux f acc seq

let fold_left_s f acc seq =
  let rec aux f acc seq =
    seq () >>= function
    | Nil -> Lwt.return acc
    | Cons (x, next) ->
        let* acc = f acc x in
        aux f acc next
  in
  let aux f acc seq =
    Lwt.apply seq () >>= function
    | Nil -> Lwt.return acc
    | Cons (x, next) ->
        let* acc = f acc x in
        aux f acc next
  in
  aux f acc seq

let iter f seq =
  let rec aux seq =
    seq () >>= function
    | Nil -> Lwt.return_unit
    | Cons (x, next) ->
        f x;
        aux next
  in
  let aux seq =
    Lwt.apply seq () >>= function
    | Nil -> Lwt.return_unit
    | Cons (x, next) ->
        f x;
        aux next
  in
  aux seq

let iter_s f seq =
  let rec aux seq =
    seq () >>= function
    | Nil -> Lwt.return_unit
    | Cons (x, next) ->
        let* () = f x in
        aux next
  in
  let aux seq =
    Lwt.apply seq () >>= function
    | Nil -> Lwt.return_unit
    | Cons (x, next) ->
        let* () = f x in
        aux next
  in
  aux seq

let iter_p f seq =
  let rec aux acc seq =
    seq () >>= function
    | Nil -> Lwt.join acc
    | Cons (x, next) ->
        let p = f x in
        aux (p::acc) next
  in
  let aux acc seq =
    Lwt.apply seq () >>= function
    | Nil -> Lwt.join acc
    | Cons (x, next) ->
        let p = f x in
        aux (p::acc) next
  in
  aux [] seq

let iter_n ?(max_concurrency = 1) f seq =
  begin
    if max_concurrency <= 0 then
      let message =
        Printf.sprintf
          "Lwt_seq.iter_n: max_concurrency must be > 0, %d given"
          max_concurrency
      in
      invalid_arg message
  end;
  let rec loop running available seq =
    begin
      if available > 0 then (
        Lwt.return (running, available)
      )
      else (
        Lwt.nchoose_split running >>= fun (complete, running) ->
        Lwt.return (running, available + List.length complete)
      )
    end >>= fun (running, available) ->
    seq () >>= function
    | Nil ->
      Lwt.join running
    | Cons (elt, seq) ->
      loop (f elt :: running) (pred available) seq
  in
  (* because the recursion is more complicated here, we apply the seq directly at
     the call-site instead *)
  loop [] max_concurrency (fun () -> Lwt.apply seq ())

let rec unfold f u () =
  match f u with
  | None -> return_nil
  | Some (x, u') -> Lwt.return (Cons (x, unfold f u'))
  | exception exc -> Lwt.fail exc

let rec unfold_lwt f u () =
  let* x = f u in
  match x with
  | None -> return_nil
  | Some (x, u') -> Lwt.return (Cons (x, unfold_lwt f u'))
let unfold_lwt f u () =
  let* x = Lwt.apply f u in
  match x with
  | None -> return_nil
  | Some (x, u') -> Lwt.return (Cons (x, unfold_lwt f u'))

let rec of_list = function
  | [] -> empty
  | h :: t -> cons h (of_list t)

let rec to_list seq =
  seq () >>= function
  | Nil -> Lwt.return_nil
  | Cons (x, next) ->
    let+ l = to_list next in
    x :: l
let to_list seq =
  Lwt.apply seq () >>= function
  | Nil -> Lwt.return_nil
  | Cons (x, next) ->
    let+ l = to_list next in
    x :: l

let rec of_seq seq () =
  match seq () with
  | Seq.Nil -> return_nil
  | Seq.Cons (x, next) ->
    Lwt.return (Cons (x, (of_seq next)))
  | exception exn -> Lwt.fail exn

let rec of_seq_lwt (seq: 'a Lwt.t Seq.t): 'a t = fun () ->
    match seq () with
    | Seq.Nil -> return_nil
    | Seq.Cons (x, next) ->
       let+ x = x in
       let next = of_seq_lwt next in
       Cons (x, next)
let of_seq_lwt (seq: 'a Lwt.t Seq.t): 'a t = fun () ->
    match seq () with
    | Seq.Nil -> return_nil
    | Seq.Cons (x, next) ->
       let+ x = x in
       let next = of_seq_lwt next in
       Cons (x, next)
    | exception exc -> Lwt.fail exc
