open! Import
include Linked_queue0

let enqueue t x = Linked_queue0.push x t
let dequeue t = if is_empty t then None else Some (Linked_queue0.pop t)
let dequeue_exn = Linked_queue0.pop
let peek t = if is_empty t then None else Some (Linked_queue0.peek t)
let peek_exn = Linked_queue0.peek

module C = Indexed_container.Make (struct
    type nonrec 'a t = 'a t

    let fold = fold
    let iter = `Custom iter
    let length = `Custom length
    let foldi = `Define_using_fold
    let iteri = `Define_using_fold
  end)

let count = C.count
let exists = C.exists
let find = C.find
let find_map = C.find_map
let fold_result = C.fold_result
let fold_until = C.fold_until
let for_all = C.for_all
let max_elt = C.max_elt
let mem = C.mem
let min_elt = C.min_elt
let sum = C.sum
let to_list = C.to_list
let counti = C.counti
let existsi = C.existsi
let find_mapi = C.find_mapi
let findi = C.findi
let foldi = C.foldi
let for_alli = C.for_alli
let iteri = C.iteri
let transfer ~src ~dst = Linked_queue0.transfer src dst

let concat_map t ~f =
  let res = create () in
  iter t ~f:(fun a -> List.iter (f a) ~f:(fun b -> enqueue res b));
  res
;;

let concat_mapi t ~f =
  let res = create () in
  iteri t ~f:(fun i a -> List.iter (f i a) ~f:(fun b -> enqueue res b));
  res
;;

let filter_map t ~f =
  let res = create () in
  iter t ~f:(fun a ->
    match f a with
    | None -> ()
    | Some b -> enqueue res b);
  res
;;

let filter_mapi t ~f =
  let res = create () in
  iteri t ~f:(fun i a ->
    match f i a with
    | None -> ()
    | Some b -> enqueue res b);
  res
;;

let filter t ~f =
  let res = create () in
  iter t ~f:(fun a -> if f a then enqueue res a);
  res
;;

let filteri t ~f =
  let res = create () in
  iteri t ~f:(fun i a -> if f i a then enqueue res a);
  res
;;

let map t ~f =
  let res = create () in
  iter t ~f:(fun a -> enqueue res (f a));
  res
;;

let mapi t ~f =
  let res = create () in
  iteri t ~f:(fun i a -> enqueue res (f i a));
  res
;;

let filter_inplace q ~f =
  let q' = filter q ~f in
  clear q;
  transfer ~src:q' ~dst:q
;;

let filteri_inplace q ~f =
  let q' = filteri q ~f in
  clear q;
  transfer ~src:q' ~dst:q
;;

let enqueue_all t list = List.iter list ~f:(fun x -> enqueue t x)

let of_list list =
  let t = create () in
  List.iter list ~f:(fun x -> enqueue t x);
  t
;;

let of_array array =
  let t = create () in
  Array.iter array ~f:(fun x -> enqueue t x);
  t
;;

let init len ~f =
  let t = create () in
  for i = 0 to len - 1 do
    enqueue t (f i)
  done;
  t
;;

let to_array t =
  match length t with
  | 0 -> [||]
  | len ->
    let arr = Array.create ~len (peek_exn t) in
    let i = ref 0 in
    iter t ~f:(fun v ->
      arr.(!i) <- v;
      incr i);
    arr
;;

let t_of_sexp a_of_sexp sexp = of_list (list_of_sexp a_of_sexp sexp)
let sexp_of_t sexp_of_a t = sexp_of_list sexp_of_a (to_list t)

let singleton a =
  let t = create () in
  enqueue t a;
  t
;;
