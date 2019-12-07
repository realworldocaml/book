open! Import
open Hash_queue_intf

module type Key = Key
module type S = S

module Make_with_table (Key : Key) (Table : Hashtbl_intf.S_plain with type key = Key.t) :
  S with module Key = Key = struct
  module Key = Key
  module Table = Table

  module Key_value = struct
    module T = struct
      type 'a t =
        { key : Key.t
        ; mutable value : 'a
        }
    end

    include T

    let key t = t.key
    let value t = t.value
    let sexp_of_t sexp_of_a { key; value } = [%sexp_of: Key.t * a] (key, value)
  end

  open Key_value.T
  module Elt = Doubly_linked.Elt

  type 'a t =
    { mutable num_readers : int
    ; queue : 'a Key_value.t Doubly_linked.t
    ; table : 'a Key_value.t Elt.t Table.t
    }

  let sexp_of_t sexp_of_a t = [%sexp_of: a Key_value.t Doubly_linked.t] t.queue

  let invariant t =
    assert (Doubly_linked.length t.queue = Table.length t.table);
    (* Look at each element in the queue, checking:
     *   - every element in the queue is in the hash table
     *   - there are no duplicate keys
    *)
    let keys = Table.create ~size:(Table.length t.table) () in
    Doubly_linked.iter t.queue ~f:(fun kv ->
      let key = kv.key in
      match Table.find t.table key with
      | None -> assert false
      | Some _ ->
        assert (not (Table.mem keys key));
        Table.set keys ~key ~data:())
  ;;

  let create ?(growth_allowed = true) ?(size = 16) () =
    { num_readers = 0
    ; queue = Doubly_linked.create ()
    ; table = Table.create ~growth_allowed ~size ()
    }
  ;;

  let read t f =
    t.num_readers <- t.num_readers + 1;
    Exn.protect ~f ~finally:(fun () -> t.num_readers <- t.num_readers - 1)
  ;;

  let ensure_can_modify t =
    if t.num_readers > 0
    then failwith "It is an error to modify a Hash_queue.t while iterating over it."
  ;;

  let clear t =
    ensure_can_modify t;
    Doubly_linked.clear t.queue;
    Table.clear t.table
  ;;

  let length t = Table.length t.table
  let is_empty t = length t = 0

  let lookup t k =
    match Table.find t.table k with
    | None -> None
    | Some elt -> Some (Elt.value elt).value
  ;;

  let lookup_exn t k = (Elt.value (Table.find_exn t.table k)).value
  let mem t k = Table.mem t.table k

  (* Note that this is the tail-recursive Core_list.map *)
  let to_list t = List.map (Doubly_linked.to_list t.queue) ~f:Key_value.value
  let to_array t = Array.map (Doubly_linked.to_array t.queue) ~f:Key_value.value

  let for_all t ~f =
    read t (fun () -> Doubly_linked.for_all t.queue ~f:(fun kv -> f kv.value))
  ;;

  let exists t ~f =
    read t (fun () -> Doubly_linked.exists t.queue ~f:(fun kv -> f kv.value))
  ;;

  let find_map t ~f =
    read t (fun () -> Doubly_linked.find_map t.queue ~f:(fun kv -> f kv.value))
  ;;

  let find t ~f =
    read t (fun () ->
      Option.map
        (Doubly_linked.find t.queue ~f:(fun kv -> f kv.value))
        ~f:Key_value.value)
  ;;

  let enqueue t back_or_front key value =
    ensure_can_modify t;
    if Table.mem t.table key
    then `Key_already_present
    else (
      let contents = { Key_value.key; value } in
      let elt =
        match back_or_front with
        | `back -> Doubly_linked.insert_last t.queue contents
        | `front -> Doubly_linked.insert_first t.queue contents
      in
      Table.set t.table ~key ~data:elt;
      `Ok)
  ;;

  let enqueue_back t = enqueue t `back
  let enqueue_front t = enqueue t `front

  exception Enqueue_duplicate_key of Key.t [@@deriving sexp]

  let enqueue_exn t back_or_front key value =
    match enqueue t back_or_front key value with
    | `Key_already_present -> raise (Enqueue_duplicate_key key)
    | `Ok -> ()
  ;;

  let enqueue_back_exn t = enqueue_exn t `back
  let enqueue_front_exn t = enqueue_exn t `front

  (* Performance hack: we implement this version separately to avoid allocation from the
     option. *)
  let lookup_and_move_to_back_exn t key =
    ensure_can_modify t;
    let elt = Table.find_exn t.table key in
    Doubly_linked.move_to_back t.queue elt;
    Key_value.value (Elt.value elt)
  ;;

  let lookup_and_move_to_back t key =
    let open Option.Let_syntax in
    ensure_can_modify t;
    let%map elt = Table.find t.table key in
    Doubly_linked.move_to_back t.queue elt;
    Key_value.value (Elt.value elt)
  ;;

  let lookup_and_move_to_front_exn t key =
    ensure_can_modify t;
    let elt = Table.find_exn t.table key in
    Doubly_linked.move_to_front t.queue elt;
    Key_value.value (Elt.value elt)
  ;;

  let lookup_and_move_to_front t key =
    let open Option.Let_syntax in
    ensure_can_modify t;
    let%map elt = Table.find t.table key in
    Doubly_linked.move_to_front t.queue elt;
    Key_value.value (Elt.value elt)
  ;;

  let dequeue_with_key t back_or_front =
    ensure_can_modify t;
    let maybe_kv =
      match back_or_front with
      | `back -> Doubly_linked.remove_last t.queue
      | `front -> Doubly_linked.remove_first t.queue
    in
    match maybe_kv with
    | None -> None
    | Some kv ->
      Table.remove t.table kv.key;
      Some (kv.key, kv.value)
  ;;

  exception Dequeue_with_key_empty [@@deriving sexp]

  let dequeue_with_key_exn t back_or_front =
    match dequeue_with_key t back_or_front with
    | None -> raise Dequeue_with_key_empty
    | Some (k, v) -> k, v
  ;;

  let dequeue_back_with_key t = dequeue_with_key t `back
  let dequeue_back_with_key_exn t = dequeue_with_key_exn t `back
  let dequeue_front_with_key t = dequeue_with_key t `front
  let dequeue_front_with_key_exn t = dequeue_with_key_exn t `front

  let dequeue t back_or_front =
    match dequeue_with_key t back_or_front with
    | None -> None
    | Some (_, v) -> Some v
  ;;

  let dequeue_back t = dequeue t `back
  let dequeue_front t = dequeue t `front

  let first_with_key t =
    match Doubly_linked.first t.queue with
    | None -> None
    | Some { key; value } -> Some (key, value)
  ;;

  let first t =
    match Doubly_linked.first t.queue with
    | None -> None
    | Some kv -> Some kv.value
  ;;

  exception Dequeue_empty [@@deriving sexp]

  let dequeue_exn t back_or_front =
    match dequeue t back_or_front with
    | None -> raise Dequeue_empty
    | Some v -> v
  ;;

  let dequeue_back_exn t = dequeue_exn t `back
  let dequeue_front_exn t = dequeue_exn t `front

  let keys t =
    (* Return the keys in the order of the queue. *)
    List.map (Doubly_linked.to_list t.queue) ~f:Key_value.key
  ;;

  let iteri t ~f =
    read t (fun () ->
      Doubly_linked.iter t.queue ~f:(fun kv -> f ~key:kv.key ~data:kv.value))
  ;;

  let iter t ~f = iteri t ~f:(fun ~key:_ ~data -> f data)

  let foldi t ~init ~f =
    read t (fun () ->
      Doubly_linked.fold t.queue ~init ~f:(fun ac kv -> f ac ~key:kv.key ~data:kv.value))
  ;;

  let fold t ~init ~f = foldi t ~init ~f:(fun ac ~key:_ ~data -> f ac data)
  let count t ~f = Container.count ~fold t ~f
  let sum m t ~f = Container.sum m ~fold t ~f
  let min_elt t ~compare = Container.min_elt ~fold t ~compare
  let max_elt t ~compare = Container.max_elt ~fold t ~compare
  let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
  let fold_until t ~init ~f = Container.fold_until ~fold ~init ~f t

  let dequeue_all t ~f =
    let rec loop () =
      match dequeue_front t with
      | None -> ()
      | Some v ->
        f v;
        loop ()
    in
    loop ()
  ;;

  let remove t k =
    ensure_can_modify t;
    match Table.find t.table k with
    | None -> `No_such_key
    | Some elt ->
      Doubly_linked.remove t.queue elt;
      Table.remove t.table (Elt.value elt).key;
      `Ok
  ;;

  exception Remove_unknown_key of Key.t [@@deriving sexp]

  let remove_exn t k =
    ensure_can_modify t;
    match remove t k with
    | `No_such_key -> raise (Remove_unknown_key k)
    | `Ok -> ()
  ;;

  let replace t k v =
    ensure_can_modify t;
    match Table.find t.table k with
    | None -> `No_such_key
    | Some elt ->
      (Elt.value elt).value <- v;
      `Ok
  ;;

  exception Replace_unknown_key of Key.t [@@deriving sexp]

  let replace_exn t k v =
    ensure_can_modify t;
    match replace t k v with
    | `No_such_key -> raise (Replace_unknown_key k)
    | `Ok -> ()
  ;;

  let drop ?(n = 1) t back_or_front =
    if n >= length t
    then clear t
    else
      for _ = 1 to n do
        ignore (dequeue_with_key t back_or_front : _ option)
      done
  ;;

  let drop_back ?n t = drop ?n t `back
  let drop_front ?n t = drop ?n t `front
end

module Make (Key : Key) : S with module Key = Key =
  Make_with_table (Key) (Hashtbl.Make_plain (Key))
