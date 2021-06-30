open! Core_kernel
open! Async_kernel
open! Import

module T = struct
  type +'a t = 'a Deferred.t [@@deriving sexp_of]

  let return = Deferred.return

  let bind t ~f =
    if Deferred.is_determined t then f (Deferred.value_exn t) else Deferred.bind t ~f
  ;;

  let map t ~f =
    if Deferred.is_determined t
    then return (f (Deferred.value_exn t))
    else Deferred.map t ~f
  ;;

  let map = `Custom map
end

include T
include Monad.Make (T)

let create = Deferred.create
let don't_wait_for = Deferred.don't_wait_for
let invariant = Deferred.invariant
let is_determined = Deferred.is_determined
let never = Deferred.never
let peek = Deferred.peek
let unit = Deferred.unit
let value_exn = Deferred.value_exn
let upon t f = if is_determined t then f (value_exn t) else Deferred.upon t f

let both t1 t2 =
  create (fun result ->
    upon t1 (fun a1 -> upon t2 (fun a2 -> Ivar.fill result (a1, a2))))
;;

let ok t = if is_determined t then return (Ok (value_exn t)) else Deferred.ok t
let ignore_m t = if is_determined t then unit else Deferred.ignore_m t

let any ts =
  match List.find ts ~f:is_determined with
  | Some x -> return (value_exn x)
  | None -> Deferred.any ts
;;

let any_unit ts =
  if List.exists ts ~f:(is_determined : unit t -> bool)
  then unit
  else Deferred.any_unit ts
;;

module Infix = struct
  include Monad_infix

  let ( >>> ) = upon
end

let repeat_until_finished state f =
  let open Infix in
  create (fun finished ->
    let rec loop state =
      f state
      >>> function
      | `Repeat state -> loop state
      | `Finished result -> Ivar.fill finished result
    in
    loop state)
;;

module List = struct
  open Infix
  open Let_syntax

  let foldi t ~init ~f =
    create (fun result ->
      let rec loop t i b =
        match t with
        | [] -> Ivar.fill result b
        | x :: xs -> f i b x >>> fun b -> loop xs (i + 1) b
      in
      loop t 0 init)
  ;;

  let fold t ~init ~f = foldi t ~init ~f:(fun _ a x -> f a x)

  let seqmapi t ~f =
    foldi t ~init:[] ~f:(fun i bs a ->
      let%map b = f i a in
      b :: bs)
    >>| List.rev
  ;;

  let all ds = seqmapi ds ~f:(fun _ x -> x)
  let all_unit ds = ignore_m (fold ds ~init:() ~f:(fun () d -> d) : unit T.t)

  let iteri ?(how = `Sequential) t ~f =
    match how with
    | (`Parallel | `Max_concurrent_jobs _) as how ->
      all_unit (List.mapi t ~f:(unstage (Throttle.monad_sequence_how2 ~how ~f)))
    | `Sequential -> foldi t ~init:() ~f:(fun i () x -> f i x)
  ;;

  let mapi ?(how = `Sequential) t ~f =
    match how with
    | (`Parallel | `Max_concurrent_jobs _) as how ->
      all (List.mapi t ~f:(unstage (Throttle.monad_sequence_how2 ~how ~f)))
    | `Sequential -> seqmapi t ~f
  ;;

  let filteri ?how t ~f =
    let%map bools = mapi t ?how ~f in
    List.rev
      (List.fold2_exn t bools ~init:[] ~f:(fun ac x b -> if b then x :: ac else ac))
  ;;

  let filter_mapi ?how t ~f = mapi t ?how ~f >>| List.filter_opt
  let concat_mapi ?how t ~f = mapi t ?how ~f >>| List.concat

  let find_mapi t ~f =
    let rec find_mapi t ~f i =
      match t with
      | [] -> return None
      | hd :: tl ->
        (match%bind f i hd with
         | None -> find_mapi tl ~f (i + 1)
         | Some _ as some -> return some)
    in
    find_mapi t ~f 0
  ;;

  let findi t ~f =
    find_mapi t ~f:(fun i elt ->
      let%map b = f i elt in
      if b then Some (i, elt) else None)
  ;;

  let find t ~f = find_mapi t ~f:(fun _ elt -> if%map f elt then Some elt else None)

  let existsi t ~f =
    match%map
      find_mapi t ~f:(fun i elt ->
        let%map b = f i elt in
        if b then Some () else None)
    with
    | Some () -> true
    | None -> false
  ;;

  let for_alli t ~f =
    match%map
      find_mapi t ~f:(fun i elt ->
        let%map b = f i elt in
        if not b then Some () else None)
    with
    | Some () -> false
    | None -> true
  ;;

  let iter ?how t ~f = iteri ?how t ~f:(fun _ a -> f a)
  let map ?how t ~f = mapi ?how t ~f:(fun _ a -> f a)
  let filter ?how t ~f = filteri ?how t ~f:(fun _ a -> f a)
  let filter_map ?how t ~f = filter_mapi ?how t ~f:(fun _ a -> f a)
  let concat_map ?how t ~f = concat_mapi ?how t ~f:(fun _ a -> f a)
  let find_map t ~f = find_mapi t ~f:(fun _ a -> f a)
  let exists t ~f = existsi t ~f:(fun _ a -> f a)
  let for_all t ~f = for_alli t ~f:(fun _ a -> f a)
  let init ?how n ~f = map ?how (List.init n ~f:Fn.id) ~f
end

let all_unit = List.all_unit
