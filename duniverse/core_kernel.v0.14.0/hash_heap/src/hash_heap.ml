
open! Core_kernel
open! Import
open Hash_heap_intf

module Make (Key : Key) : S with module Key = Key = struct
  module Key = Key
  module Table = Hashtbl.Make_plain (Key)

  type 'a t =
    { heap : (Key.t * 'a) Heap.t
    ; cmp : 'a -> 'a -> int
    ; tbl : (Key.t * 'a) Heap.Elt.t Table.t
    }

  let create ?min_size cmp =
    let initial_tbl_size =
      match min_size with
      | None -> 50
      | Some s -> s
    in
    { heap = Heap.create ?min_size ~cmp:(fun (_, v1) (_, v2) -> cmp v1 v2) ()
    ; cmp
    ; tbl = Table.create ~size:initial_tbl_size ()
    }
  ;;

  (* [push_new_key] adds an entry to the heap without checking for duplicates.  Thus it
     should only be called when the key is known not to be present already. *)
  let push_new_key t ~key ~data =
    let el = Heap.add_removable t.heap (key, data) in
    Hashtbl.set t.tbl ~key ~data:el
  ;;

  let push t ~key ~data =
    match Hashtbl.find t.tbl key with
    | Some _ -> `Key_already_present
    | None ->
      push_new_key t ~key ~data;
      `Ok
  ;;

  exception Key_already_present of Key.t [@@deriving sexp]

  let push_exn t ~key ~data =
    match push t ~key ~data with
    | `Ok -> ()
    | `Key_already_present -> raise (Key_already_present key)
  ;;

  let replace t ~key ~data =
    match Hashtbl.find t.tbl key with
    | None -> push_exn t ~key ~data
    | Some el ->
      Heap.remove t.heap el;
      push_new_key t ~key ~data
  ;;

  let remove t key =
    match Hashtbl.find t.tbl key with
    | None -> ()
    | Some el ->
      Hashtbl.remove t.tbl key;
      Heap.remove t.heap el
  ;;

  let mem t key = Hashtbl.mem t.tbl key

  let top_with_key t =
    match Heap.top t.heap with
    | None -> None
    | Some (k, v) -> Some (k, v)
  ;;

  let top t =
    match top_with_key t with
    | None -> None
    | Some (_, v) -> Some v
  ;;

  let top_exn t = snd (Heap.top_exn t.heap)
  let top_with_key_exn t = Heap.top_exn t.heap

  let pop_with_key_exn t =
    let k, v = Heap.pop_exn t.heap in
    Hashtbl.remove t.tbl k;
    k, v
  ;;

  let pop_with_key t =
    try Some (pop_with_key_exn t) with
    | _ -> None
  ;;

  let pop t =
    match pop_with_key t with
    | None -> None
    | Some (_, v) -> Some v
  ;;

  let pop_exn t = snd (pop_with_key_exn t)

  let pop_if_with_key t f =
    match Heap.pop_if t.heap (fun (k, v) -> f ~key:k ~data:v) with
    | None -> None
    | Some (k, v) ->
      Hashtbl.remove t.tbl k;
      Some (k, v)
  ;;

  let pop_if t f =
    match pop_if_with_key t (fun ~key:_ ~data -> f data) with
    | None -> None
    | Some (_k, v) -> Some v
  ;;

  let find t key =
    match Hashtbl.find t.tbl key with
    | None -> None
    | Some el -> Some (snd (Heap.Elt.value_exn el))
  ;;

  exception Key_not_found of Key.t [@@deriving sexp]

  let find_exn t key =
    match find t key with
    | Some el -> el
    | None -> raise (Key_not_found key)
  ;;

  let find_pop t key =
    match Hashtbl.find t.tbl key with
    | None -> None
    | Some el ->
      let _k, v = Heap.Elt.value_exn el in
      Hashtbl.remove t.tbl key;
      Heap.remove t.heap el;
      Some v
  ;;

  let find_pop_exn t key =
    match find_pop t key with
    | Some el -> el
    | None -> raise (Key_not_found key)
  ;;

  let iteri t ~f = Heap.iter t.heap ~f:(fun (k, v) -> f ~key:k ~data:v)
  let iter t ~f = Heap.iter t.heap ~f:(fun (_k, v) -> f v)
  let iter_keys t ~f = Heap.iter t.heap ~f:(fun (k, _v) -> f k)
  let to_alist t = Heap.to_list t.heap

  let length t =
    assert (Hashtbl.length t.tbl = Heap.length t.heap);
    Hashtbl.length t.tbl
  ;;

  let copy t =
    let t' = create t.cmp in
    iteri t ~f:(fun ~key ~data -> push_exn t' ~key ~data);
    t'
  ;;
end
