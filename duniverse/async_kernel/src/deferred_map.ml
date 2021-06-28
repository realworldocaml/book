open Core_kernel
open Deferred_std
module Deferred = Deferred1
module List = Deferred_list

type ('a, 'b, 'c) t = ('a, 'b, 'c) Map.t

let change t k ~f =
  let%map opt = f (Map.find t k) in
  Map.change t k ~f:(fun _ -> opt)
;;

let update t k ~f =
  let%map data = f (Map.find t k) in
  Map.set t ~key:k ~data
;;

let iter_keys ?how t ~f = List.iter ?how (Map.keys t) ~f
let iter ?how t ~f = List.iter ?how (Map.data t) ~f

let iteri ?how t ~f =
  List.iter ?how (Map.to_alist t) ~f:(fun (key, data) -> f ~key ~data)
;;

let fold t ~init ~f =
  let alist_in_increasing_key_order =
    Map.fold_right t ~init:[] ~f:(fun ~key ~data alist -> (key, data) :: alist)
  in
  List.fold alist_in_increasing_key_order ~init ~f:(fun ac (key, data) ->
    f ~key ~data ac)
;;

let fold_right t ~init ~f =
  let alist_in_decreasing_key_order =
    Map.fold t ~init:[] ~f:(fun ~key ~data alist -> (key, data) :: alist)
  in
  List.fold alist_in_decreasing_key_order ~init ~f:(fun ac (key, data) ->
    f ~key ~data ac)
;;

module Job = struct
  type ('a, 'b, 'c) t =
    { key : 'a
    ; data : 'b
    ; mutable result : 'c option
    }
  [@@deriving fields]
end

let filter_mapi ?how t ~f =
  let jobs = ref [] in
  let job_map =
    Map.mapi t ~f:(fun ~key ~data ->
      let job = { Job.key; data; result = None } in
      jobs := job :: !jobs;
      job)
  in
  let%map () =
    List.iter ?how !jobs ~f:(function { Job.key; data; result = _ } as job ->
      let%map x = f ~key ~data in
      job.result <- x)
  in
  Map.filter_map job_map ~f:Job.result
;;

let filter_map ?how t ~f = filter_mapi ?how t ~f:(fun ~key:_ ~data -> f data)

let filter_keys ?how t ~f =
  filter_mapi ?how t ~f:(fun ~key ~data ->
    let%map b = f key in
    if b then Some data else None)
;;

let filter ?how t ~f =
  filter_mapi ?how t ~f:(fun ~key:_ ~data ->
    let%map b = f data in
    if b then Some data else None)
;;

let filteri ?how t ~f =
  filter_mapi ?how t ~f:(fun ~key ~data ->
    let%map b = f ~key ~data in
    if b then Some data else None)
;;

let mapi ?how t ~f =
  filter_mapi ?how t ~f:(fun ~key ~data ->
    let%map z = f ~key ~data in
    Some z)
;;

let map ?how t ~f = mapi ?how t ~f:(fun ~key:_ ~data -> f data)

let merge ?how t1 t2 ~f =
  filter_map
    ?how
    (Map.merge t1 t2 ~f:(fun ~key z -> Some (fun () -> f ~key z)))
    ~f:(fun thunk -> thunk ())
;;

let all t = map t ~f:Fn.id
