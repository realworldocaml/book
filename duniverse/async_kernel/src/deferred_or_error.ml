open! Core_kernel
open! Import
module Deferred = Deferred1

module Monitor = struct
  let try_with = Monitor.try_with
end

(* Copied to [eager_deferred_or_error.ml].  There should be no diffs below this line. *)

include (Deferred_result : Monad.S2 with type ('a, 'b) t := ('a, 'b) Deferred_result.t)

type 'a t = 'a Or_error.t Deferred.t

include Applicative.Make (struct
    type nonrec 'a t = 'a t

    let return = return

    let apply f x =
      Deferred_result.combine
        f
        x
        ~ok:(fun f x -> f x)
        ~err:(fun e1 e2 -> Error.of_list [ e1; e2 ])
    ;;

    let map = `Custom map
  end)

module Let_syntax = struct
  let return = return

  include Monad_infix

  module Let_syntax = struct
    let return = return
    let map = map
    let bind = bind
    let both = both

    (* from Applicative.Make *)
    module Open_on_rhs = struct end
  end
end

open Let_syntax

let ignore = ignore_m
let fail error = Deferred.return (Result.fail error)
let ok_exn t = Deferred.map t ~f:Or_error.ok_exn
let of_exn exn = Deferred.return (Or_error.of_exn exn)
let of_exn_result t = Deferred.map t ~f:Or_error.of_exn_result
let error msg v sexp_of = Deferred.return (Or_error.error msg v sexp_of)
let error_s sexp = Deferred.return (Or_error.error_s sexp)
let error_string msg = Deferred.return (Or_error.error_string msg)
let errorf format = ksprintf error_string format
let tag t ~tag = Deferred.map t ~f:(Or_error.tag ~tag)
let tag_s t ~tag = Deferred.map t ~f:(Or_error.tag_s ~tag)

let tag_arg t message a sexp_of_a =
  Deferred.map t ~f:(fun t -> Or_error.tag_arg t message a sexp_of_a)
;;

let unimplemented msg = Deferred.return (Or_error.unimplemented msg)
let combine_errors l = Deferred.map (Deferred.all l) ~f:Or_error.combine_errors
let combine_errors_unit l = Deferred.map (Deferred.all l) ~f:Or_error.combine_errors_unit

let filter_ok_at_least_one l =
  Deferred.map (Deferred.all l) ~f:Or_error.filter_ok_at_least_one
;;

let find_map_ok l ~f =
  Deferred.repeat_until_finished (l, []) (fun (l, errors) ->
    match l with
    | [] ->
      let errors = Error.of_list (List.rev errors) in
      Deferred.return (`Finished (Error errors))
    | hd :: tl ->
      Deferred.map (f hd) ~f:(function
        | Error current_error -> `Repeat (tl, current_error :: errors)
        | Ok result -> `Finished (Ok result)))
;;

let ok_unit = return ()

let try_with ?extract_exn ?run ?here ?name f =
  Deferred.map (Monitor.try_with ?extract_exn ?run ?here ?name f) ~f:(function
    | Error exn -> Error (Error.of_exn exn)
    | Ok _ as ok -> ok)
;;

let try_with_join ?extract_exn ?run ?here ?name f =
  Deferred.map (try_with ?extract_exn ?run ?here ?name f) ~f:Or_error.join
;;

module List = struct
  let foldi list ~init:acc ~f =
    let rec loop i acc = function
      | [] -> return acc
      | hd :: tl ->
        let%bind acc = f i acc hd in
        loop (i + 1) acc tl
    in
    loop 0 acc list
  ;;

  let fold t ~init ~f = foldi t ~init ~f:(fun _ a x -> f a x)

  let seqmapi t ~f =
    foldi t ~init:[] ~f:(fun i bs a ->
      let%map b = f i a in
      b :: bs)
    >>| List.rev
  ;;

  let all = all
  let all_unit = all_unit

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

  let filter_mapi ?how t ~f = mapi t ?how ~f >>| List.filter_opt
  let concat_mapi ?how t ~f = mapi t ?how ~f >>| List.concat

  let filteri ?how t ~f =
    filter_mapi ?how t ~f:(fun i x ->
      let%map b = f i x in
      if b then Some x else None)
  ;;

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

  let find_map t ~f = find_mapi t ~f:(fun _ a -> f a)

  let findi t ~f =
    find_mapi t ~f:(fun i elt ->
      let%map b = f i elt in
      if b then Some (i, elt) else None)
  ;;

  let find t ~f =
    find_map t ~f:(fun elt ->
      let%map b = f elt in
      if b then Some elt else None)
  ;;

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

let rec repeat_until_finished state f =
  match%bind f state with
  | `Repeat state -> repeat_until_finished state f
  | `Finished state -> return state
;;
