open! Core_kernel

module Entry = struct
  type ('key, 'data) t =
    { (* the int is fixed, but the 'key can change *)
      mutable key : 'key
    ; mutable data : 'data
    (* The index in [defined_entries] where this [Entry.t] is placed. *)
    ; mutable defined_entries_index : int
    }
  [@@deriving fields, sexp_of]
end

type ('key, 'data) t_detailed =
  { num_keys : int
  ; sexp_of_key : ('key -> Sexp.t) option
  ; key_to_int : 'key -> int
  (* The number of entries in the table, not the length of the arrays below. *)
  ; mutable length : int
  (* [(key, data)] is in the table iff
     {[
       entries_by_key.( key_to_int key ) = Some { key; data; _ }
     ]}
  *)
  ; entries_by_key : ('key, 'data) Entry.t option array
  (* The first [length] elements of [defined_entries] hold the data in the table.  This is
     an optimization for fold, to keep us from wasting iterations when the array is
     sparse. *)
  ; defined_entries : ('key, 'data) Entry.t option array
  }
[@@deriving fields, sexp_of]

type ('a, 'b) t = ('a, 'b) t_detailed
type ('a, 'b) table = ('a, 'b) t

let sexp_of_key t =
  match t.sexp_of_key with
  | Some f -> f
  | None -> fun key -> Int.sexp_of_t (t.key_to_int key)
;;

let invariant invariant_key invariant_data t =
  try
    let num_keys = t.num_keys in
    assert (num_keys = Array.length t.entries_by_key);
    assert (num_keys = Array.length t.defined_entries);
    assert (0 <= t.length && t.length <= num_keys);
    Array.iteri t.entries_by_key ~f:(fun i ->
      function
      | None -> ()
      | Some entry ->
        invariant_key entry.Entry.key;
        invariant_data entry.data;
        assert (i = t.key_to_int entry.key);
        (match t.defined_entries.(entry.defined_entries_index) with
         | None -> assert false
         | Some entry' -> assert (phys_equal entry entry')));
    Array.iteri t.defined_entries ~f:(fun i entry_opt ->
      match i < t.length, entry_opt with
      | false, None -> ()
      | true, Some entry -> assert (i = entry.Entry.defined_entries_index)
      | _ -> assert false);
    let get_entries array =
      let a = Array.filter_opt array in
      Array.sort a ~compare:(fun entry entry' ->
        Int.compare (t.key_to_int entry.Entry.key) (t.key_to_int entry'.key));
      a
    in
    let entries = get_entries t.entries_by_key in
    let entries' = get_entries t.defined_entries in
    assert (t.length = Array.length entries);
    assert (Array.equal phys_equal entries entries')
  with
  | exn ->
    let sexp_of_key = sexp_of_key t in
    failwiths
      ~here:[%here]
      "invariant failed"
      (exn, t)
      [%sexp_of: exn * (key, _) t_detailed]
;;

let debug = ref false
let check_invariant t = if !debug then invariant ignore ignore t
let is_empty t = length t = 0

let create ?sexp_of_key ~num_keys ~key_to_int () =
  if num_keys < 0
  then failwiths ~here:[%here] "num_keys must be nonnegative" num_keys [%sexp_of: int];
  let t =
    { num_keys
    ; sexp_of_key
    ; key_to_int
    ; length = 0
    ; entries_by_key = Array.create ~len:num_keys None
    ; defined_entries = Array.create ~len:num_keys None
    }
  in
  check_invariant t;
  t
;;

let create_like
      { num_keys
      ; sexp_of_key
      ; key_to_int
      ; length = _
      ; entries_by_key = _
      ; defined_entries = _
      }
  =
  create ~num_keys ?sexp_of_key ~key_to_int ()
;;

let fold t ~init ~f =
  let rec loop i ac =
    if i = t.length
    then ac
    else (
      match t.defined_entries.(i) with
      | None -> assert false
      | Some entry -> loop (i + 1) (f ~key:entry.key ~data:entry.data ac))
  in
  loop 0 init
;;

let iteri t ~f = fold t ~init:() ~f:(fun ~key ~data () -> f ~key ~data)
let iter t ~f = iteri t ~f:(fun ~key:_ ~data -> f data)
let iter_keys t ~f = iteri t ~f:(fun ~key ~data:_ -> f key)
let map_entries t ~f = fold t ~init:[] ~f:(fun ~key ~data ac -> f ~key ~data :: ac)
let to_alist t = map_entries t ~f:(fun ~key ~data -> key, data)

let clear t =
  for i = 0 to t.length - 1 do
    match t.defined_entries.(i) with
    | None -> assert false
    | Some entry ->
      t.defined_entries.(i) <- None;
      t.entries_by_key.(t.key_to_int entry.key) <- None
  done;
  t.length <- 0
;;

module Serialized = struct
  type ('key, 'data) t =
    { num_keys : int
    ; alist : ('key * 'data) list
    }
  [@@deriving bin_io, sexp]
end

let to_serialized t = { Serialized.num_keys = t.num_keys; alist = to_alist t }

let sexp_of_t sexp_of_key sexp_of_data t =
  Serialized.sexp_of_t sexp_of_key sexp_of_data (to_serialized t)
;;

let keys t = map_entries t ~f:(fun ~key ~data:_ -> key)
let data t = map_entries t ~f:(fun ~key:_ ~data -> data)

let entry_opt t key =
  let index = t.key_to_int key in
  try t.entries_by_key.(index) with
  | _ ->
    let sexp_of_key = sexp_of_key t in
    failwiths
      ~here:[%here]
      "key's index out of range"
      (key, index, `Should_be_between_0_and (t.num_keys - 1))
      [%sexp_of: key * int * [ `Should_be_between_0_and of int ]]
;;

let find t key =
  match entry_opt t key with
  | None -> None
  | Some e -> Some (Entry.data e)
;;

let find_exn t key =
  match entry_opt t key with
  | Some entry -> Entry.data entry
  | None ->
    let sexp_of_key = sexp_of_key t in
    failwiths
      ~here:[%here]
      "Bounded_int_table.find_exn got unknown key"
      (key, t)
      [%sexp_of: key * (key, _) t]
;;

let mem t key = is_some (entry_opt t key)

let add_assuming_not_there t ~key ~data =
  let defined_entries_index = t.length in
  let entry_opt = Some { Entry.key; data; defined_entries_index } in
  t.entries_by_key.(t.key_to_int key) <- entry_opt;
  t.defined_entries.(defined_entries_index) <- entry_opt;
  t.length <- t.length + 1;
  check_invariant t
;;

let find_or_add t key ~default =
  match entry_opt t key with
  | Some e -> Entry.data e
  | None ->
    let data = default () in
    add_assuming_not_there t ~key ~data;
    data
;;

let set t ~key ~data =
  match entry_opt t key with
  | None -> add_assuming_not_there t ~key ~data
  | Some entry ->
    entry.key <- key;
    (* we update the key because we want the latest key in the table *)
    entry.data <- data
;;

let add t ~key ~data =
  match entry_opt t key with
  | Some entry -> `Duplicate entry.Entry.data
  | None ->
    add_assuming_not_there t ~key ~data;
    `Ok
;;

let add_exn t ~key ~data =
  match add t ~key ~data with
  | `Ok -> ()
  | `Duplicate _ ->
    let sexp_of_key = sexp_of_key t in
    failwiths
      ~here:[%here]
      "Bounded_int_table.add_exn of key whose index is already present"
      (key, t.key_to_int key)
      [%sexp_of: key * int]
;;

let remove t key =
  (match entry_opt t key with
   | None -> ()
   | Some entry ->
     t.length <- t.length - 1;
     t.entries_by_key.(t.key_to_int key) <- None;
     let hole = entry.defined_entries_index in
     let last = t.length in
     if hole < last
     then (
       match t.defined_entries.(last) with
       | None ->
         let sexp_of_key = sexp_of_key t in
         failwiths
           ~here:[%here]
           "Bounded_int_table.remove bug"
           (key, last, t)
           [%sexp_of: key * int * (key, _) t_detailed]
       | Some entry_to_put_in_hole as entry_to_put_in_hole_opt ->
         t.defined_entries.(hole) <- entry_to_put_in_hole_opt;
         entry_to_put_in_hole.defined_entries_index <- hole);
     t.defined_entries.(last) <- None);
  check_invariant t
;;

let existsi t ~f =
  with_return (fun r ->
    iteri t ~f:(fun ~key ~data -> if f ~key ~data then r.return true);
    false)
;;

let exists t ~f = existsi t ~f:(fun ~key:_ ~data -> f data)
let for_alli t ~f = not (existsi t ~f:(fun ~key ~data -> not (f ~key ~data)))
let for_all t ~f = for_alli t ~f:(fun ~key:_ ~data -> f data)

let equal key_equal data_equal t1 t2 =
  length t1 = length t2
  && for_alli t1 ~f:(fun ~key ~data ->
    match entry_opt t2 key with
    | None -> false
    | Some entry ->
      key_equal key entry.Entry.key && data_equal data entry.Entry.data)
;;

module With_key (Key : sig
    type t [@@deriving bin_io, sexp]

    val to_int : t -> int
  end) =
struct
  type 'data t = (Key.t, 'data) table
  type 'data table = 'data t

  let create ~num_keys =
    create ~sexp_of_key:Key.sexp_of_t ~num_keys ~key_to_int:Key.to_int ()
  ;;

  let of_alist_exn alist =
    let max_key =
      List.fold alist ~init:(-1) ~f:(fun max (key, _) -> Int.max max (Key.to_int key))
    in
    let t = create ~num_keys:(max_key + 1) in
    List.iter alist ~f:(fun (key, data) -> add_exn t ~key ~data);
    t
  ;;

  let of_alist alist = Or_error.try_with (fun () -> of_alist_exn alist)
  let sexp_of_t sexp_of_data = sexp_of_t Key.sexp_of_t sexp_of_data

  let of_serialized { Serialized.num_keys; alist } =
    let t = create ~num_keys in
    List.iter alist ~f:(fun (key, data) -> add_exn t ~key ~data);
    t
  ;;

  let t_of_sexp data_of_sexp sexp =
    of_serialized (Serialized.t_of_sexp Key.t_of_sexp data_of_sexp sexp)
  ;;

  include Binable.Of_binable1_without_uuid [@alert "-legacy"]
      (struct
        type 'data t = (Key.t, 'data) Serialized.t [@@deriving bin_io]
      end)
      (struct
        type 'data t = 'data table

        let to_binable = to_serialized
        let of_binable = of_serialized
      end)
end

let filter_mapi t ~f =
  let result = create_like t in
  iteri t ~f:(fun ~key ~data ->
    match f ~key ~data with
    | None -> ()
    | Some data -> add_exn result ~key ~data);
  result
;;

let ignore_key f ~key:_ ~data = f data
let ignore_data f ~key ~data:_ = f key
let filter_map t ~f = filter_mapi t ~f:(ignore_key f)

let filteri t ~f =
  filter_mapi t ~f:(fun ~key ~data -> if f ~key ~data then Some data else None)
;;

let filter t ~f = filteri t ~f:(ignore_key f)
let filter_keys t ~f = filteri t ~f:(ignore_data f)
let mapi t ~f = filter_mapi t ~f:(fun ~key ~data -> Some (f ~key ~data))
let map t ~f = mapi t ~f:(ignore_key f)
