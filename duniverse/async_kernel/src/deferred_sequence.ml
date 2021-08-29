open Core_kernel
open Deferred_std
module Deferred = Deferred1

(* [fold_mapi ?how t ~init ~mapi_f ~fold_f] is a more efficient version of:

   {[
     fold ~init ~f:(fun b a -> return (fold_f b a)) (mapi t ?how ~f:mapi_f) ]}

   It avoids creating the intermediate sequence that would result from [mapi], and
   allows the [fold] to proceed concurrently with the [mapi], so that one can accumulate
   the result as soon as possible, possibly avoiding creating an intermediate structure
   (e.g. [iteri] and [filter_map] uses [fold_mapi] to do this). *)
let fold_mapi
      (type a b c)
      ?(how = `Sequential)
      (t : a Sequence.t)
      ~(init : c)
      ~(mapi_f : int -> a -> b Deferred.t)
      ~(fold_f : c -> b -> c)
  : c Deferred.t
  =
  match how with
  | `Sequential ->
    let rec loop i t (c : c) =
      match Sequence.next t with
      | None -> return c
      | Some (a, t) ->
        let%bind b = mapi_f i a in
        loop (i + 1) t (fold_f c b)
    in
    loop 0 t init
  | `Parallel ->
    let rec loop i t (c : c Deferred.t) =
      match Sequence.next t with
      | None -> c
      | Some (a, t) ->
        loop
          (i + 1)
          t
          (let%bind b = mapi_f i a in
           let%map c = c in
           fold_f c b)
    in
    loop 0 t (return init)
  | `Max_concurrent_jobs max_concurrent_jobs ->
    let throttle = Throttle.create ~max_concurrent_jobs ~continue_on_error:false in
    (* [loop] forces the input sequence and enqueues a throttle job only if there is
       capacity available. *)
    let rec loop i t (c : c Deferred.t) =
      let%bind () = Throttle.capacity_available throttle in
      match Sequence.next t with
      | None -> c
      | Some (a, t) ->
        loop
          (i + 1)
          t
          (let%bind b = Throttle.enqueue throttle (fun () -> mapi_f i a) in
           let%map c = c in
           fold_f c b)
    in
    loop 0 t (return init)
;;

let foldi t ~init ~f =
  Sequence.delayed_fold
    t
    ~init:(0, init)
    ~f:(fun (i, b) a ~k ->
      let%bind b = f i b a in
      k (i + 1, b))
    ~finish:(fun (_, b) -> return b)
;;

(* [fold] is not implemented in terms of [foldi] to save the intermediate closure
   allocation. *)
let fold t ~init ~f =
  Sequence.delayed_fold t ~init ~f:(fun b a ~k -> f b a >>= k) ~finish:return
;;

let all t =
  let%map res =
    fold t ~init:[] ~f:(fun accum d ->
      let%map a = d in
      a :: accum)
  in
  Sequence.of_list (List.rev res)
;;

let all_unit t = fold t ~init:() ~f:(fun () v -> v)

let find_mapi t ~f =
  let rec find_mapi t ~f i =
    match Sequence.next t with
    | None -> return None
    | Some (v, rest) ->
      (match%bind f i v with
       | None -> find_mapi rest ~f (i + 1)
       | Some _ as some -> return some)
  in
  find_mapi t ~f 0
;;

let findi t ~f =
  find_mapi t ~f:(fun i elt ->
    let%map b = f i elt in
    if b then Some (i, elt) else None)
;;

let find t ~f =
  find_mapi t ~f:(fun _ elt ->
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

let iteri ?how t ~f : unit Deferred.t =
  fold_mapi ?how t ~mapi_f:f ~init:() ~fold_f:(fun () () -> ())
;;

let mapi ?how t ~f =
  let%map bs =
    fold_mapi ?how t ~mapi_f:(fun i a -> f i a) ~init:[] ~fold_f:(fun bs b -> b :: bs)
  in
  Sequence.of_list (List.rev bs)
;;

(* [filter_mapi] is implemented using [fold_mapi] rather than [map] so that we never need
   to keep a long stream of intermediate [None] results in the accumulator, only to later
   filter them all out. *)
let filter_mapi ?how t ~f =
  let%map bs =
    fold_mapi
      t
      ?how
      ~mapi_f:(fun i a -> f i a)
      ~init:[]
      ~fold_f:(fun bs maybe_v ->
        match maybe_v with
        | None -> bs
        | Some b -> b :: bs)
  in
  Sequence.of_list (List.rev bs)
;;

let concat_mapi ?how t ~f = mapi ?how t ~f >>| Sequence.concat

let filteri ?how t ~f =
  filter_mapi ?how t ~f:(fun i a ->
    match%map f i a with
    | true -> Some a
    | false -> None)
;;

let iter ?how t ~f = iteri ?how t ~f:(fun _ a -> f a)
let map ?how t ~f = mapi ?how t ~f:(fun _ a -> f a)
let filter ?how t ~f = filteri ?how t ~f:(fun _ a -> f a)
let filter_map ?how t ~f = filter_mapi ?how t ~f:(fun _ a -> f a)
let concat_map ?how t ~f = concat_mapi ?how t ~f:(fun _ a -> f a)
let find_map t ~f = find_mapi t ~f:(fun _ a -> f a)
let exists t ~f = existsi t ~f:(fun _ a -> f a)
let for_all t ~f = for_alli t ~f:(fun _ a -> f a)
let init ?how n ~f = map ?how (Sequence.init n ~f:Fn.id) ~f
