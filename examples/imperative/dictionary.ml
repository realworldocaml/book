open Core.Std

type ('a, 'b) t = { mutable length: int;
                    buckets: ('a * 'b) list array;
                  }

let num_buckets = 17
let hash_bucket key = (Hashtbl.hash key) mod num_buckets

let create () =
  { length = 0;
    buckets = Array.create ~len:num_buckets [];
  }

let length t = t.length

let iter t ~f =
  for i = 0 to Array.length t.buckets - 1 do
    List.iter t.buckets.(i) ~f:(fun (key, data) -> f ~key ~data)
  done

let bucket_has_key t i key =
  List.exists t.buckets.(i) ~f:(fun (key',_) -> key' = key)

let add t ~key ~data =
  let i = hash_bucket key in
  let replace = bucket_has_key t i key in
  let filtered_bucket =
    if replace then List.filter t.buckets.(i) ~f:(fun (key',_) -> key' <> key)
    else t.buckets.(i)
  in
  t.buckets.(i) <- (key, data) :: filtered_bucket;
  if not replace then t.length <- t.length + 1

let find t key =
  List.find_map t.buckets.(hash_bucket key)
    ~f:(fun (key',data) -> if key' = key then Some data else None)

let remove t key =
  let rec find_bucket i =
    if i >= Array.length t.buckets then None
    else if List.exists t.buckets.(i) ~f:(fun (key',_) -> key' = key) then Some i
    else find_bucket (i+1)
  in
  match find_bucket 0 with
  | None -> () (* nothing to remove, so do nothing *)
  | Some i ->
    t.length <- t.length - 1;
    t.buckets.(i) <-
      List.filter t.buckets.(i) ~f:(fun (key',_) -> key' <> key)
