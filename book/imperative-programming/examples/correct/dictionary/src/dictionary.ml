(* $MDX part-begin=1 *)
(* file: dictionary.ml *)
open Base

type ('a, 'b) t = { mutable length: int;
                    buckets: ('a * 'b) list array;
                    hash: 'a -> int;
                    equal: 'a -> 'a -> bool;
                  }
(* $MDX part-end *)

(* $MDX part-begin=2 *)
let num_buckets = 17

let hash_bucket t key = (t.hash key) % num_buckets

let create ~hash ~equal =
  { length = 0;
    buckets = Array.create ~len:num_buckets [];
    hash;
    equal;
  }

let length t = t.length

let find t key =
  List.find_map t.buckets.(hash_bucket t key)
    ~f:(fun (key',data) -> if t.equal key' key then Some data else None)
(* $MDX part-end *)

(* $MDX part-begin=3 *)
let iter t ~f =
  for i = 0 to Array.length t.buckets - 1 do
    List.iter t.buckets.(i) ~f:(fun (key, data) -> f ~key ~data)
  done
(* $MDX part-end *)

(* $MDX part-begin=4 *)
let bucket_has_key t i key =
  List.exists t.buckets.(i) ~f:(fun (key',_) -> t.equal key' key)

let add t ~key ~data =
  let i = hash_bucket t key in
  let replace = bucket_has_key t i key in
  let filtered_bucket =
    if replace then
      List.filter t.buckets.(i) ~f:(fun (key',_) -> not (t.equal key' key))
    else
      t.buckets.(i)
  in
  t.buckets.(i) <- (key, data) :: filtered_bucket;
  if not replace then t.length <- t.length + 1

let remove t key =
  let i = hash_bucket t key in
  if bucket_has_key t i key then (
    let filtered_bucket =
      List.filter t.buckets.(i) ~f:(fun (key',_) -> not (t.equal key' key))
    in
    t.buckets.(i) <- filtered_bucket;
    t.length <- t.length - 1
  )
(* $MDX part-end *)

let add_with_let_in t ~key ~data =
  let i = hash_bucket t key in
  let replace = bucket_has_key t i key in
  let filtered_bucket =
    if replace then
      List.filter t.buckets.(i) ~f:(fun (key',_) -> not (t.equal key' key))
    else
      t.buckets.(i)
  in
  (* $MDX part-begin=add-with-let-in *)
  let () = t.buckets.(i) <- (key, data) :: filtered_bucket in
  if not replace then t.length <- t.length + 1
  (* $MDX part-end *)
