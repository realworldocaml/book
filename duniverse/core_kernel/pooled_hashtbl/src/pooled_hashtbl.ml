open! Core_kernel
open! Import
open Hashtbl_intf
open With_return

let failwiths = Error.failwiths

module Hashable = Hashtbl_intf.Hashable
module Merge_into_action = Hashtbl_intf.Merge_into_action
module Pool = Tuple_pool

let hash_param = Hashable.hash_param
let hash = Hashable.hash

module Entry : sig
  module Pool : sig
    type ('k, 'd) t [@@deriving sexp_of]

    val invariant : ('k, 'd) t -> unit
    val create : capacity:int -> (_, _) t
    val grow : ?capacity:int -> ('k, 'd) t -> ('k, 'd) t
    val max_capacity : int
  end

  type ('k, 'd) t = private int [@@deriving sexp_of]

  val null : unit -> (_, _) t
  val is_null : (_, _) t -> bool
  val create : ('k, 'd) Pool.t -> next:('k, 'd) t -> key:'k -> data:'d -> ('k, 'd) t
  val free : ('k, 'd) Pool.t -> ('k, 'd) t -> unit
  val next : ('k, 'd) Pool.t -> ('k, 'd) t -> ('k, 'd) t
  val key : ('k, 'd) Pool.t -> ('k, 'd) t -> 'k
  val data : ('k, 'd) Pool.t -> ('k, 'd) t -> 'd
  val set_next : ('k, 'd) Pool.t -> ('k, 'd) t -> ('k, 'd) t -> unit
  val set_data : ('k, 'd) Pool.t -> ('k, 'd) t -> 'd -> unit
end = struct
  (* It is OK to use [Pool.Unsafe] because entries are never exposed to user code.  Thus,
     we can convince ourselves solely from looking at the implementation of
     [Pooled_hashtbl] that an entry is never used after it is freed. *)
  module Unsafe = Pool.Unsafe
  module Pointer = Unsafe.Pointer

  type ('k, 'd) fields = (('k, 'd) fields Pointer.t, 'k, 'd) Unsafe.Slots.t3
  [@@deriving sexp_of]

  type ('k, 'd) t = ('k, 'd) fields Pointer.t [@@deriving sexp_of]

  let create pool ~next ~key ~data = Unsafe.new3 pool next key data
  let free = Unsafe.free
  let next p t = Unsafe.get p t Unsafe.Slot.t0
  let key p t = Unsafe.get p t Unsafe.Slot.t1
  let data p t = Unsafe.get p t Unsafe.Slot.t2
  let set_next p t x = Unsafe.set p t Unsafe.Slot.t0 x
  let set_data p t x = Unsafe.set p t Unsafe.Slot.t2 x

  module Pool = struct
    type ('k, 'd) t = ('k, 'd) fields Unsafe.t [@@deriving sexp_of]

    let invariant t = Unsafe.invariant ignore t
    let create ~capacity = Unsafe.create Unsafe.Slots.t3 ~capacity

    let max_capacity =
      Unsafe.max_capacity ~slots_per_tuple:(Unsafe.Slots.slots_per_tuple Unsafe.Slots.t3)
    ;;

    let grow = Unsafe.grow
  end

  let null = Pointer.null
  let is_null = Pointer.is_null
end

type ('k, 'd) hashtbl =
  { hashable : 'k Hashable.t
  ; growth_allowed : bool
  ; mutable length : int
  ; mutable capacity : int
  ; mutable entries : ('k, 'd) Entry.Pool.t
  ; mutable table : ('k, 'd) Entry.t array
  ; mutable n_entries : int
  ; mutable mutation_allowed : bool
  }

type ('k, 'd) t = ('k, 'd) hashtbl
type 'a key = 'a

module type S_plain = S_plain with type ('a, 'b) hashtbl = ('a, 'b) t
module type S = S with type ('a, 'b) hashtbl = ('a, 'b) t
module type S_binable = S_binable with type ('a, 'b) hashtbl = ('a, 'b) t

let sexp_of_key t = t.hashable.Hashable.sexp_of_t

let ensure_mutation_allowed t =
  if not t.mutation_allowed
  then failwith "Hashtbl: mutation not allowed during iteration"
;;

let without_mutating t f v =
  if t.mutation_allowed
  then (
    t.mutation_allowed <- false;
    match f v with
    | x ->
      t.mutation_allowed <- true;
      x
    | exception exn ->
      t.mutation_allowed <- true;
      raise exn)
  else f v
;;

(* We match want to match Core's interface completely, so you can't change the load
   factor. If we care, we can add a new create function, put it back in the record, and
   plumb it through functions like map which call create. *)
let load_factor = 0.85
let max_table_length = Int.floor_pow2 Sys.max_array_length

let calculate_table_size size =
  (* Ensure we can fit size elements in the table. *)
  let size = Int.min size Sys.max_array_length in
  let capacity = Int.ceil_pow2 size in
  let n_entries = int_of_float (Float.round_up (float capacity *. load_factor)) in
  let n_entries = Int.max size n_entries in
  let n_entries = Int.min n_entries Entry.Pool.max_capacity in
  capacity, n_entries
;;

let create ?(growth_allowed = true) ?(size = 128) ~hashable () =
  let size = Int.min (Int.max 1 size) max_table_length in
  let capacity, n_entries = calculate_table_size size in
  let table = Array.create ~len:capacity (Entry.null ()) in
  let entries = Entry.Pool.create ~capacity:n_entries in
  { hashable
  ; growth_allowed
  ; length = 0
  ; capacity
  ; table
  ; entries
  ; n_entries
  ; mutation_allowed = true
  }
;;

let table_get (t : ('k, 'd) Entry.t array) h = Array.unsafe_get t h

let table_set (t : ('k, 'd) Entry.t array) h (e : ('k, 'd) Entry.t) =
  Array.unsafe_set t h e
;;

let hash_key t key = t.hashable.hash key
let compare_key t k1 k2 = t.hashable.compare k1 k2
let hashable t = t.hashable
let hashable_s t = Hashable.to_key t.hashable
let slot t key = hash_key t key land (t.capacity - 1)
let length t = t.length
let is_empty t = t.length = 0

let clear =
  let rec free_loop t e =
    let next = Entry.next t.entries e in
    Entry.free t.entries e;
    if not (Entry.is_null next) then free_loop t next
  in
  fun t ->
    ensure_mutation_allowed t;
    for i = 0 to t.capacity - 1 do
      let e = table_get t.table i in
      if not (Entry.is_null e)
      then (
        free_loop t e;
        table_set t.table i (Entry.null ()))
    done;
    t.length <- 0
;;

let on_grow = ref (fun () -> Staged.stage (fun ~old_capacity:_ ~new_capacity:_ -> ()))

let resize =
  let rec copy_entries t e =
    if not (Entry.is_null e)
    then (
      let key = Entry.key t.entries e in
      let next_e = Entry.next t.entries e in
      let index = slot t key in
      let next = table_get t.table index in
      Entry.set_next t.entries e next;
      table_set t.table index e;
      copy_entries t next_e)
  in
  fun t size ->
    if t.growth_allowed
    then (
      if size > t.capacity
      then (
        let new_capacity, new_n_entries = calculate_table_size size in
        let old_table, old_capacity = t.table, t.capacity in
        let after_grow = Staged.unstage (!on_grow ()) in
        t.entries <- Entry.Pool.grow t.entries ~capacity:new_n_entries;
        t.table <- Array.create ~len:new_capacity (Entry.null ());
        t.capacity <- new_capacity;
        t.n_entries <- new_n_entries;
        for i = 0 to old_capacity - 1 do
          copy_entries t (table_get old_table i)
        done;
        after_grow ~old_capacity ~new_capacity))
    else (
      t.entries <- Entry.Pool.grow t.entries ~capacity:(2 * t.n_entries);
      t.n_entries <- 2 * t.n_entries)
;;

let on_grow ~before ~after =
  let old_before = !on_grow in
  on_grow
  := fun () ->
    let old_after = Staged.unstage (old_before ()) in
    let v = before () in
    Staged.stage (fun ~old_capacity ~new_capacity ->
      old_after ~old_capacity ~new_capacity;
      after v ~old_capacity ~new_capacity)
;;

let rec find_entry t ~key ~it =
  if Entry.is_null it
  then it
  else (
    let curr_key = Entry.key t.entries it in
    if compare_key t curr_key key = 0
    then it
    else find_entry t ~key ~it:(Entry.next t.entries it))
;;

let mem t key =
  let index = slot t key in
  let it = table_get t.table index in
  let e = find_entry t ~key ~it in
  not (Entry.is_null e)
;;

(* we assume here that [Entry.create] will succeed *)
let insert_link_pool_not_full t ~index ~key ~data ~it =
  (* New entry adds to the begining of the list, which is t.table.(index) or `it`. *)
  let e = Entry.create t.entries ~next:it ~key ~data in
  table_set t.table index e;
  t.length <- t.length + 1
;;

let insert_link t ~index ~key ~data ~it =
  if t.length < t.n_entries
  then insert_link_pool_not_full t ~index ~key ~data ~it
  else (
    resize t (t.capacity + 1);
    let index = slot t key in
    let it = table_get t.table index in
    insert_link_pool_not_full t ~index ~key ~data ~it)
;;

let delete_link t ~index ~prev ~e =
  let next = Entry.next t.entries e in
  if Entry.is_null prev
  then table_set t.table index next
  else Entry.set_next t.entries prev next;
  Entry.free t.entries e;
  t.length <- t.length - 1
;;

(** If key is already in t, return the entry it was found at. Otherwise, create an entry,
    set it to data and return the empty entry. *)
let set_or_entry t ~key ~data =
  let index = slot t key in
  let it = table_get t.table index in
  let e = find_entry t ~key ~it in
  if Entry.is_null e then insert_link t ~index ~key ~data ~it;
  e
;;

let set t ~key ~data =
  ensure_mutation_allowed t;
  let e = set_or_entry t ~key ~data in
  if not (Entry.is_null e) then Entry.set_data t.entries e data
;;

let replace = set

let add t ~key ~data =
  ensure_mutation_allowed t;
  let e = set_or_entry t ~key ~data in
  if Entry.is_null e then `Ok else `Duplicate
;;

let add_exn t ~key ~data =
  match add t ~key ~data with
  | `Ok -> ()
  | `Duplicate ->
    let sexp_of_key = sexp_of_key t in
    let error =
      Error.create "Pooled_hashtbl.add_exn got key already present" key sexp_of_key
    in
    Error.raise error
;;

let[@inline always] find_or_add_impl t key ~without_mutating_make_default ~default =
  ensure_mutation_allowed t;
  let index = slot t key in
  let it = table_get t.table index in
  let e = find_entry t ~key ~it in
  if not (Entry.is_null e)
  then Entry.data t.entries e
  else (
    let data = without_mutating_make_default t default key in
    insert_link t ~index ~key ~data ~it;
    data)
;;

let findi_or_add =
  let without_mutating_make_default t default key = without_mutating t default key in
  fun t key ~default -> find_or_add_impl t key ~without_mutating_make_default ~default
;;

let find_or_add =
  let without_mutating_make_default t default _key = without_mutating t default () in
  fun t key ~default -> find_or_add_impl t key ~without_mutating_make_default ~default
;;

let find t key =
  let index = slot t key in
  let it = table_get t.table index in
  let e = find_entry t ~key ~it in
  if Entry.is_null e then None else Some (Entry.data t.entries e)
;;

let find_exn t key =
  (* We could call find here, but that returns a boxed option. *)
  let index = slot t key in
  let it = table_get t.table index in
  let e = find_entry t ~key ~it in
  if not (Entry.is_null e)
  then Entry.data t.entries e
  else  raise Caml.Not_found
;;

let[@inline always] find_and_call_impl
                      t
                      key
                      ~call_if_found
                      ~call_if_not_found
                      ~if_found
                      ~if_not_found
                      arg1
                      arg2
  =
  let index = slot t key in
  let it = table_get t.table index in
  let e = find_entry t ~key ~it in
  if not (Entry.is_null e)
  then
    call_if_found
      ~if_found
      ~key:(Entry.key t.entries e)
      ~data:(Entry.data t.entries e)
      arg1
      arg2
  else call_if_not_found ~if_not_found key arg1 arg2
;;

let find_and_call =
  let call_if_found ~if_found ~key:_ ~data () () = if_found data in
  let call_if_not_found ~if_not_found key () () = if_not_found key in
  fun t key ~if_found ~if_not_found ->
    find_and_call_impl
      t
      key
      ()
      ()
      ~call_if_found
      ~call_if_not_found
      ~if_found
      ~if_not_found
;;

let findi_and_call =
  let call_if_found ~if_found ~key ~data () () = if_found ~key ~data in
  let call_if_not_found ~if_not_found key () () = if_not_found key in
  fun t key ~if_found ~if_not_found ->
    find_and_call_impl
      t
      key
      ()
      ()
      ~call_if_found
      ~call_if_not_found
      ~if_found
      ~if_not_found
;;

let find_and_call1 =
  let call_if_found ~if_found ~key:_ ~data a () = if_found data a in
  let call_if_not_found ~if_not_found key a () = if_not_found key a in
  fun t key ~a ~if_found ~if_not_found ->
    find_and_call_impl
      t
      key
      ~call_if_found
      ~call_if_not_found
      ~if_found
      ~if_not_found
      a
      ()
;;

let findi_and_call1 =
  let call_if_found ~if_found ~key ~data a () = if_found ~key ~data a in
  let call_if_not_found ~if_not_found key a () = if_not_found key a in
  fun t key ~a ~if_found ~if_not_found ->
    find_and_call_impl
      t
      key
      ~call_if_found
      ~call_if_not_found
      ~if_found
      ~if_not_found
      a
      ()
;;

let find_and_call2 =
  let call_if_found ~if_found ~key:_ ~data a b = if_found data a b in
  let call_if_not_found ~if_not_found key a b = if_not_found key a b in
  fun t key ~a ~b ~if_found ~if_not_found ->
    find_and_call_impl
      t
      key
      ~call_if_found
      ~call_if_not_found
      ~if_found
      ~if_not_found
      a
      b
;;

let findi_and_call2 =
  let call_if_found ~if_found ~key ~data a b = if_found ~key ~data a b in
  let call_if_not_found ~if_not_found key a b = if_not_found key a b in
  fun t key ~a ~b ~if_found ~if_not_found ->
    find_and_call_impl
      t
      key
      ~call_if_found
      ~call_if_not_found
      ~if_found
      ~if_not_found
      a
      b
;;

(* This is split in a rather odd way so as to make find_and_remove for a single entry
   chain able to be inlined. *)
let rec remove_key_r t index key e prev =
  if compare_key t (Entry.key t.entries e) key = 0
  then (
    let data = Entry.data t.entries e in
    delete_link t ~index ~prev ~e;
    Some data)
  else (
    let next = Entry.next t.entries e in
    if Entry.is_null next then None else remove_key_r t index key next e)
;;

let find_and_remove t key =
  ensure_mutation_allowed t;
  let index = slot t key in
  let e = table_get t.table index in
  (* can't reuse find_entry given that we require the prev pointer *)
  if not (Entry.is_null e)
  then
    if compare_key t (Entry.key t.entries e) key = 0
    then (
      let data = Entry.data t.entries e in
      delete_link t ~index ~prev:(Entry.null ()) ~e;
      Some data)
    else (
      let next = Entry.next t.entries e in
      if Entry.is_null next then None else remove_key_r t index key next e)
  else None
;;

let change =
  let call t f x = without_mutating t (fun () -> f x) () in
  let rec change_key t key f index e prev =
    if Entry.is_null e
    then `Not_found
    else (
      let curr_key = Entry.key t.entries e in
      if compare_key t curr_key key = 0
      then (
        (match call t f (Some (Entry.data t.entries e)) with
         | Some data -> Entry.set_data t.entries e data
         | None -> delete_link t ~index ~prev ~e);
        `Changed)
      else change_key t key f index (Entry.next t.entries e) e)
  in
  fun t key ~f ->
    ensure_mutation_allowed t;
    let index = slot t key in
    let it = table_get t.table index in
    match change_key t key f index it (Entry.null ()) with
    | `Changed -> ()
    | `Not_found ->
      (* New entry is inserted in the begining of the list (it) *)
      (match call t f None with
       | None -> ()
       | Some data -> insert_link t ~index ~key ~data ~it)
;;

let incr_by ~remove_if_zero t key by =
  if remove_if_zero
  then
    change t key ~f:(fun opt ->
      match by + Option.value opt ~default:0 with
      | 0 -> None
      | n -> Some n)
  else (
    ensure_mutation_allowed t;
    let e = set_or_entry t ~key ~data:by in
    if not (Entry.is_null e)
    then (
      let data = Entry.data t.entries e in
      Entry.set_data t.entries e (data + by)))
;;

let incr ?(by = 1) ?(remove_if_zero = false) t key = incr_by ~remove_if_zero t key by
let decr ?(by = 1) ?(remove_if_zero = false) t key = incr_by ~remove_if_zero t key (-by)
let update t key ~f = change t key ~f:(fun data -> Some (f data))

(* Split similar to find and removed. Code duplicated to avoid allocation and
   unroll/inline the single entry case *)
let rec remove_key_r t index key e prev =
  if compare_key t (Entry.key t.entries e) key = 0
  then delete_link t ~index ~prev ~e
  else (
    let next = Entry.next t.entries e in
    if not (Entry.is_null next) then remove_key_r t index key next e)
;;

let remove t key =
  ensure_mutation_allowed t;
  let index = slot t key in
  let e = table_get t.table index in
  (* can't reuse find_entry given that we require the prev pointer *)
  if not (Entry.is_null e)
  then
    if compare_key t (Entry.key t.entries e) key = 0
    then delete_link t ~index ~prev:(Entry.null ()) ~e
    else (
      let next = Entry.next t.entries e in
      if not (Entry.is_null next) then remove_key_r t index key next e)
;;

(* TODO: If we care, these can be optimized to avoid option boxes, allocating closures,
   etc. These are largely copied from core_hashtbl.ml. If we do care about performance
   here, we should, at the least, allow you to determine, given an entry, whether it has
   a key. Then we could just iterate over the Entry_pool and get better cache behavior. *)

let add_multi t ~key ~data =
  match find t key with
  | None -> replace t ~key ~data:[ data ]
  | Some l -> replace t ~key ~data:(data :: l)
;;

let find_multi t key =
  match find t key with
  | None -> []
  | Some l -> l
;;

let remove_multi t key =
  match find t key with
  | None -> ()
  | Some [] | Some [ _ ] -> remove t key
  | Some (_ :: tl) -> replace t ~key ~data:tl
;;

let iteri =
  let rec loop t f e =
    if not (Entry.is_null e)
    then (
      f ~key:(Entry.key t.entries e) ~data:(Entry.data t.entries e);
      loop t f (Entry.next t.entries e))
  in
  fun t ~f ->
    if t.length = 0
    then ()
    else (
      let m = t.mutation_allowed in
      match
        t.mutation_allowed <- false;
        for i = 0 to t.capacity - 1 do
          loop t f (table_get t.table i)
        done
      with
      | () -> t.mutation_allowed <- m
      | exception exn ->
        t.mutation_allowed <- m;
        raise exn)
;;

let iter t ~f = iteri t ~f:(fun ~key:_ ~data -> f data)
let iter_keys t ~f = iteri t ~f:(fun ~key ~data:_ -> f key)

let rec choose_nonempty t i =
  let entry = table_get t.table i in
  if Entry.is_null entry
  then choose_nonempty t (i + 1)
  else Entry.key t.entries entry, Entry.data t.entries entry
;;

let choose t = if t.length = 0 then None else Some (choose_nonempty t 0)

let choose_exn t =
  if t.length = 0 then raise_s [%message "[Pooled_hashtbl.choose_exn] of empty hashtbl"];
  choose_nonempty t 0
;;

let fold =
  let rec fold_entries t e acc f =
    if Entry.is_null e
    then acc
    else
      fold_entries
        t
        (Entry.next t.entries e)
        (f ~key:(Entry.key t.entries e) ~data:(Entry.data t.entries e) acc)
        f
  in
  fun t ~init ~f ->
    if length t = 0
    then init
    else (
      let acc = ref init in
      let m = t.mutation_allowed in
      match
        t.mutation_allowed <- false;
        for i = 0 to t.capacity - 1 do
          let e = table_get t.table i in
          if not (Entry.is_null e) then acc := fold_entries t e !acc f
        done
      with
      | () ->
        t.mutation_allowed <- m;
        !acc
      | exception exn ->
        t.mutation_allowed <- m;
        raise exn)
;;

let invariant invariant_key invariant_data t =
  let n = Array.length t.table in
  for i = 0 to n - 1 do
    let e = table_get t.table i in
    assert (Entry.is_null e || i = slot t (Entry.key t.entries e))
  done;
  Entry.Pool.invariant t.entries;
  let real_len =
    fold t ~init:0 ~f:(fun ~key ~data i ->
      invariant_key key;
      invariant_data data;
      i + 1)
  in
  assert (real_len = t.length);
  assert (t.length <= t.n_entries)
;;

let sexp_of_t sexp_of_k sexp_of_d t =
  let coll ~key:k ~data:v acc = Sexp.List [ sexp_of_k k; sexp_of_d v ] :: acc in
  Sexp.List (fold ~f:coll t ~init:[])
;;

let existsi t ~f =
  with_return (fun r ->
    iteri t ~f:(fun ~key ~data -> if f ~key ~data then r.return true);
    false)
;;

let exists t ~f = existsi t ~f:(fun ~key:_ ~data -> f data)
let for_alli t ~f = not (existsi t ~f:(fun ~key ~data -> not (f ~key ~data)))
let for_all t ~f = not (existsi t ~f:(fun ~key:_ ~data -> not (f data)))

let counti t ~f =
  fold t ~init:0 ~f:(fun ~key ~data acc -> if f ~key ~data then acc + 1 else acc)
;;

let count t ~f =
  fold t ~init:0 ~f:(fun ~key:_ ~data acc -> if f data then acc + 1 else acc)
;;

let mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed ~hashable:t.hashable ~size:t.length ()
  in
  iteri t ~f:(fun ~key ~data -> replace new_t ~key ~data:(f ~key ~data));
  new_t
;;

let map t ~f = mapi t ~f:(fun ~key:_ ~data -> f data)

let filter_mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed ~hashable:t.hashable ~size:t.length ()
  in
  iteri t ~f:(fun ~key ~data ->
    match f ~key ~data with
    | Some new_data -> replace new_t ~key ~data:new_data
    | None -> ());
  new_t
;;

let filter_map t ~f = filter_mapi t ~f:(fun ~key:_ ~data -> f data)

let filteri t ~f =
  filter_mapi t ~f:(fun ~key ~data -> if f ~key ~data then Some data else None)
;;

let filter t ~f = filteri t ~f:(fun ~key:_ ~data -> f data)
let filter_keys t ~f = filteri t ~f:(fun ~key ~data:_ -> f key)

let partition_mapi t ~f =
  let t0 =
    create ~growth_allowed:t.growth_allowed ~hashable:t.hashable ~size:t.length ()
  in
  let t1 =
    create ~growth_allowed:t.growth_allowed ~hashable:t.hashable ~size:t.length ()
  in
  iteri t ~f:(fun ~key ~data ->
    match f ~key ~data with
    | First new_data -> replace t0 ~key ~data:new_data
    | Second new_data -> replace t1 ~key ~data:new_data);
  t0, t1
;;

let partition_map t ~f = partition_mapi t ~f:(fun ~key:_ ~data -> f data)

let partitioni_tf t ~f =
  partition_mapi t ~f:(fun ~key ~data ->
    if f ~key ~data then First data else Second data)
;;

let partition_tf t ~f = partitioni_tf t ~f:(fun ~key:_ ~data -> f data)

let create_mapped ?growth_allowed ?size ~hashable ~get_key ~get_data rows =
  let size =
    match size with
    | Some s -> s
    | None -> List.length rows
  in
  let res = create ?growth_allowed ~hashable ~size () in
  let dupes = ref [] in
  List.iter rows ~f:(fun r ->
    let key = get_key r in
    let data = get_data r in
    if mem res key then dupes := key :: !dupes else replace res ~key ~data);
  match !dupes with
  | [] -> `Ok res
  | keys -> `Duplicate_keys (List.dedup_and_sort ~compare:hashable.Hashable.compare keys)
;;

let create_mapped_multi ?growth_allowed ?size ~hashable ~get_key ~get_data rows =
  let size =
    match size with
    | Some s -> s
    | None -> List.length rows
  in
  let res = create ?growth_allowed ~size ~hashable () in
  List.iter rows ~f:(fun r ->
    let key = get_key r in
    let data = get_data r in
    add_multi res ~key ~data);
  res
;;

let of_alist ?growth_allowed ?size ~hashable lst =
  match create_mapped ?growth_allowed ?size ~hashable ~get_key:fst ~get_data:snd lst with
  | `Ok t -> `Ok t
  | `Duplicate_keys k -> `Duplicate_key (List.hd_exn k)
;;

let of_alist_report_all_dups ?growth_allowed ?size ~hashable lst =
  create_mapped ?growth_allowed ?size ~hashable ~get_key:fst ~get_data:snd lst
;;

let of_alist_or_error ?growth_allowed ?size ~hashable lst =
  match of_alist ?growth_allowed ?size ~hashable lst with
  | `Ok v -> Result.Ok v
  | `Duplicate_key key ->
    let sexp_of_key = hashable.Hashable.sexp_of_t in
    Or_error.error "Pooled_hashtbl.of_alist_exn: duplicate key" key [%sexp_of: key]
;;

let of_alist_exn ?growth_allowed ?size ~hashable lst =
  match of_alist_or_error ?growth_allowed ?size ~hashable lst with
  | Result.Ok v -> v
  | Result.Error e -> Error.raise e
;;

let of_alist_multi ?growth_allowed ?size ~hashable lst =
  create_mapped_multi ?growth_allowed ?size ~hashable ~get_key:fst ~get_data:snd lst
;;

let to_alist t = fold ~f:(fun ~key ~data list -> (key, data) :: list) ~init:[] t
let validate ~name f t = Validate.alist ~name f (to_alist t)
let keys t = fold t ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc)
let data t = fold ~f:(fun ~key:_ ~data list -> data :: list) ~init:[] t

let add_to_groups groups ~get_key ~get_data ~combine ~rows =
  List.iter rows ~f:(fun row ->
    let key = get_key row in
    let data = get_data row in
    let data =
      match find groups key with
      | None -> data
      | Some old -> combine old data
    in
    replace groups ~key ~data)
;;

let group ?growth_allowed ?size ~hashable ~get_key ~get_data ~combine rows =
  let res = create ?growth_allowed ?size ~hashable () in
  add_to_groups res ~get_key ~get_data ~combine ~rows;
  res
;;

let create_with_key ?growth_allowed ?size ~hashable ~get_key rows =
  create_mapped ?growth_allowed ?size ~hashable ~get_key ~get_data:(fun x -> x) rows
;;

let create_with_key_or_error ?growth_allowed ?size ~hashable ~get_key rows =
  match create_with_key ?growth_allowed ?size ~hashable ~get_key rows with
  | `Ok t -> Result.Ok t
  | `Duplicate_keys keys ->
    let sexp_of_key = hashable.Hashable.sexp_of_t in
    Or_error.error
      "Pooled_hashtbl.create_with_key: duplicate keys"
      keys
      [%sexp_of: key list]
;;

let create_with_key_exn ?growth_allowed ?size ~hashable ~get_key rows =
  Or_error.ok_exn
    (create_with_key_or_error ?growth_allowed ?size ~hashable ~get_key rows)
;;

let merge =
  let maybe_set t ~key ~f d =
    match f ~key d with
    | None -> ()
    | Some v -> set t ~key ~data:v
  in
  fun t_left t_right ~f ->
    if not (Hashable.equal t_left.hashable t_right.hashable)
    then invalid_arg "Pooled_hashtbl.merge: different 'hashable' values";
    let new_t =
      create
        ~growth_allowed:t_left.growth_allowed
        ~hashable:t_left.hashable
        ~size:t_left.length
        ()
    in
    without_mutating
      t_left
      (fun () ->
         without_mutating
           t_right
           (fun () ->
              iteri t_left ~f:(fun ~key ~data:left ->
                match find t_right key with
                | None -> maybe_set new_t ~key ~f (`Left left)
                | Some right -> maybe_set new_t ~key ~f (`Both (left, right)));
              iteri t_right ~f:(fun ~key ~data:right ->
                match find t_left key with
                | None -> maybe_set new_t ~key ~f (`Right right)
                | Some _ -> ()
                (* already done above *)))
           ())
      ();
    new_t
;;

let merge_into ~src ~dst ~f =
  iteri src ~f:(fun ~key ~data ->
    let dst_data = find dst key in
    let action = without_mutating dst (fun () -> f ~key data dst_data) () in
    match (action : _ Merge_into_action.t) with
    | Remove -> remove dst key
    | Set_to data ->
      (match dst_data with
       | None -> replace dst ~key ~data
       | Some dst_data -> if not (phys_equal dst_data data) then replace dst ~key ~data))
;;

let filteri_inplace t ~f =
  let to_remove =
    fold t ~init:[] ~f:(fun ~key ~data ac -> if f ~key ~data then ac else key :: ac)
  in
  List.iter to_remove ~f:(fun key -> remove t key)
;;

let filter_inplace t ~f = filteri_inplace t ~f:(fun ~key:_ ~data -> f data)
let filter_keys_inplace t ~f = filteri_inplace t ~f:(fun ~key ~data:_ -> f key)

let filter_mapi_inplace t ~f =
  let map_results =
    fold t ~init:[] ~f:(fun ~key ~data ac -> (key, f ~key ~data) :: ac)
  in
  List.iter map_results ~f:(fun (key, result) ->
    match result with
    | None -> remove t key
    | Some data -> set t ~key ~data)
;;

let filter_map_inplace t ~f = filter_mapi_inplace t ~f:(fun ~key:_ ~data -> f data)

let mapi_inplace t ~f =
  let map_results =
    fold t ~init:[] ~f:(fun ~key ~data ac -> (key, f ~key ~data) :: ac)
  in
  List.iter map_results ~f:(fun (key, data) -> set t ~key ~data)
;;

let map_inplace t ~f = mapi_inplace t ~f:(fun ~key:_ ~data -> f data)

let equal equal t t' =
  length t = length t'
  && with_return (fun r ->
    iteri t ~f:(fun ~key ~data ->
      match find t' key with
      | None -> r.return false
      | Some data' ->
        if not (without_mutating t' (fun () -> equal data data') ())
        then r.return false);
    true)
;;

let similar = equal

let copy t =
  let table = Array.create ~len:t.capacity (Entry.null ()) in
  let entries = Entry.Pool.create ~capacity:t.n_entries in
  let copy =
    { hashable = t.hashable
    ; growth_allowed = t.growth_allowed
    ; length = 0
    ; capacity = t.capacity
    ; table
    ; entries
    ; n_entries = t.n_entries
    ; mutation_allowed = true
    }
  in
  iteri t ~f:(fun ~key ~data -> add_exn copy ~key ~data);
  copy
;;

module Accessors = struct
  let choose = choose
  let choose_exn = choose_exn
  let clear = clear
  let copy = copy
  let remove = remove
  let replace = replace
  let set = set
  let add = add
  let add_exn = add_exn
  let change = change
  let update = update
  let add_multi = add_multi
  let remove_multi = remove_multi
  let find_multi = find_multi
  let mem = mem
  let iter_keys = iter_keys
  let iter = iter
  let iteri = iteri
  let exists = exists
  let existsi = existsi
  let for_all = for_all
  let for_alli = for_alli
  let count = count
  let counti = counti
  let fold = fold
  let length = length
  let is_empty = is_empty
  let map = map
  let mapi = mapi
  let filter_map = filter_map
  let filter_mapi = filter_mapi
  let filter_keys = filter_keys
  let filter = filter
  let filteri = filteri
  let partition_map = partition_map
  let partition_mapi = partition_mapi
  let partition_tf = partition_tf
  let partitioni_tf = partitioni_tf
  let find_or_add = find_or_add
  let findi_or_add = findi_or_add
  let find = find
  let find_exn = find_exn
  let find_and_call = find_and_call
  let findi_and_call = findi_and_call
  let find_and_call1 = find_and_call1
  let findi_and_call1 = findi_and_call1
  let find_and_call2 = find_and_call2
  let findi_and_call2 = findi_and_call2
  let find_and_remove = find_and_remove
  let to_alist = to_alist
  let validate = validate
  let merge = merge
  let merge_into = merge_into
  let keys = keys
  let data = data
  let filter_keys_inplace = filter_keys_inplace
  let filter_inplace = filter_inplace
  let filteri_inplace = filteri_inplace
  let map_inplace = map_inplace
  let mapi_inplace = mapi_inplace
  let filter_map_inplace = filter_map_inplace
  let filter_mapi_inplace = filter_mapi_inplace
  let equal = equal
  let similar = similar
  let incr = incr
  let decr = decr
  let sexp_of_key = sexp_of_key
end

module type Key_plain = Key_plain
module type Key = Key
module type Key_binable = Key_binable
module type For_deriving = For_deriving

module Creators (Key : sig
    type 'a t

    val hashable : 'a t Hashable.t
  end) : sig
  type ('a, 'b) t_ = ('a Key.t, 'b) t

  val t_of_sexp : (Sexp.t -> 'a Key.t) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t_

  include
    Creators
    with type ('a, 'b) t := ('a, 'b) t_
    with type 'a key := 'a Key.t
    with type ('key, 'data, 'a) create_options :=
      ('key, 'data, 'a) create_options_without_hashable
end = struct
  let hashable = Key.hashable

  type ('a, 'b) t_ = ('a Key.t, 'b) t

  let create ?growth_allowed ?size () = create ?growth_allowed ?size ~hashable ()
  let of_alist ?growth_allowed ?size l = of_alist ?growth_allowed ~hashable ?size l

  let of_alist_report_all_dups ?growth_allowed ?size l =
    of_alist_report_all_dups ?growth_allowed ~hashable ?size l
  ;;

  let of_alist_or_error ?growth_allowed ?size l =
    of_alist_or_error ?growth_allowed ~hashable ?size l
  ;;

  let of_alist_exn ?growth_allowed ?size l =
    of_alist_exn ?growth_allowed ~hashable ?size l
  ;;

  let t_of_sexp k_of_sexp d_of_sexp sexp =
    let alist = [%of_sexp: (k * d) list] sexp in
    of_alist_exn alist ~size:(List.length alist)
  ;;

  let of_alist_multi ?growth_allowed ?size l =
    of_alist_multi ?growth_allowed ~hashable ?size l
  ;;

  let create_mapped ?growth_allowed ?size ~get_key ~get_data l =
    create_mapped ?growth_allowed ~hashable ?size ~get_key ~get_data l
  ;;

  let create_with_key ?growth_allowed ?size ~get_key l =
    create_with_key ?growth_allowed ~hashable ?size ~get_key l
  ;;

  let create_with_key_or_error ?growth_allowed ?size ~get_key l =
    create_with_key_or_error ?growth_allowed ~hashable ?size ~get_key l
  ;;

  let create_with_key_exn ?growth_allowed ?size ~get_key l =
    create_with_key_exn ?growth_allowed ~hashable ?size ~get_key l
  ;;

  let group ?growth_allowed ?size ~get_key ~get_data ~combine l =
    group ?growth_allowed ~hashable ?size ~get_key ~get_data ~combine l
  ;;
end

module Poly = struct
  type ('a, 'b) t = ('a, 'b) hashtbl
  type 'a key = 'a

  let hashable = Hashable.poly
  let invariant = invariant

  include Creators (struct
      type 'a t = 'a

      let hashable = hashable
    end)

  include Accessors

  let sexp_of_t = sexp_of_t

  include Bin_prot.Utils.Make_iterable_binable2 (struct
      type ('a, 'b) z = ('a, 'b) t
      type ('a, 'b) t = ('a, 'b) z
      type ('a, 'b) el = 'a * 'b [@@deriving bin_io]

      let caller_identity =
        Bin_prot.Shape.Uuid.of_string "a9b0d5e8-4992-11e6-a717-dfe192342aee"
      ;;

      let module_name = Some "Pooled_hashtbl"
      let length = length
      let iter t ~f = iteri t ~f:(fun ~key ~data -> f (key, data))

      let init ~len ~next =
        let t = create ~size:len () in
        for _i = 0 to len - 1 do
          let key, data = next () in
          match find t key with
          | None -> replace t ~key ~data
          | Some _ -> failwith "Pooled_hashtbl.bin_read_t_: duplicate key"
        done;
        t
      ;;
    end)
end

module Make_plain (Key : Key_plain) = struct
  let hashable =
    { Hashable.hash = Key.hash; compare = Key.compare; sexp_of_t = Key.sexp_of_t }
  ;;

  type key = Key.t [@@deriving sexp_of]
  type ('a, 'b) hashtbl = ('a, 'b) t
  type 'a t = (key, 'a) hashtbl
  type 'a key_ = key

  let invariant invariant_data t = invariant ignore invariant_data t

  include Creators (struct
      type 'a t = Key.t

      let hashable = hashable
    end)

  include Accessors

  let sexp_of_t sexp_of_v t = Poly.sexp_of_t Key.sexp_of_t sexp_of_v t

  module Provide_of_sexp
      (X : sig
         type t [@@deriving of_sexp]
       end
       with type t := key) =
  struct
    let t_of_sexp v_of_sexp sexp = t_of_sexp X.t_of_sexp v_of_sexp sexp
  end

  module Provide_bin_io
      (X : sig
         type t [@@deriving bin_io]
       end
       with type t := key) =
    Bin_prot.Utils.Make_iterable_binable1 (struct
      module Key = struct
        include Key
        include X
      end

      type nonrec 'a t = 'a t
      type 'a el = Key.t * 'a [@@deriving bin_io]

      let caller_identity =
        Bin_prot.Shape.Uuid.of_string "aa942e1a-4992-11e6-8f73-876922b0953c"
      ;;

      let module_name = Some "Pooled_hashtbl"
      let length = length
      let iter t ~f = iteri t ~f:(fun ~key ~data -> f (key, data))

      let init ~len ~next =
        let t = create ~size:len () in
        for _i = 0 to len - 1 do
          let key, data = next () in
          match find t key with
          | None -> replace t ~key ~data
          | Some _ ->
            failwiths
              ~here:[%here]
              "Pooled_hashtbl.bin_read_t: duplicate key"
              key
              [%sexp_of: Key.t]
        done;
        t
      ;;
    end)
end

module Make (Key : Key) = struct
  include Make_plain (Key)
  include Provide_of_sexp (Key)
end

module Make_binable (Key : sig
    include Key
    include Binable.S with type t := t
  end) =
struct
  include Make (Key)
  include Provide_bin_io (Key)
end

module M (K : T) = struct
  type nonrec 'v t = (K.t, 'v) t
end

module type Sexp_of_m = sig
  type t [@@deriving sexp_of]
end

module type M_of_sexp = sig
  type t [@@deriving of_sexp]

  include Key with type t := t
end

let t_of_sexp ~hashable k_of_sexp d_of_sexp sexp =
  let alist = list_of_sexp (pair_of_sexp k_of_sexp d_of_sexp) sexp in
  of_alist_exn ~hashable alist ~size:(List.length alist)
;;

let sexp_of_m__t (type k) (module K : Sexp_of_m with type t = k) sexp_of_v t =
  sexp_of_t K.sexp_of_t sexp_of_v t
;;

let m__t_of_sexp (type k) (module K : M_of_sexp with type t = k) v_of_sexp s =
  t_of_sexp ~hashable:(Hashable.of_key (module K)) K.t_of_sexp v_of_sexp s
;;

module Using_hashable = struct
  type nonrec ('a, 'b) t = ('a, 'b) t [@@deriving sexp_of]

  let create = create
  let of_alist = of_alist
  let of_alist_report_all_dups = of_alist_report_all_dups
  let of_alist_or_error = of_alist_or_error
  let of_alist_exn = of_alist_exn
  let of_alist_multi = of_alist_multi
  let create_mapped = create_mapped
  let create_with_key = create_with_key
  let create_with_key_or_error = create_with_key_or_error
  let create_with_key_exn = create_with_key_exn
  let group = group
end

let create ?growth_allowed ?size m =
  create ~hashable:(Hashable.of_key m) ?growth_allowed ?size ()
;;

let of_alist ?growth_allowed ?size m l =
  of_alist ~hashable:(Hashable.of_key m) ?growth_allowed ?size l
;;

let of_alist_report_all_dups ?growth_allowed ?size m l =
  of_alist_report_all_dups ~hashable:(Hashable.of_key m) ?growth_allowed ?size l
;;

let of_alist_or_error ?growth_allowed ?size m l =
  of_alist_or_error ~hashable:(Hashable.of_key m) ?growth_allowed ?size l
;;

let of_alist_exn ?growth_allowed ?size m l =
  of_alist_exn ~hashable:(Hashable.of_key m) ?growth_allowed ?size l
;;

let of_alist_multi ?growth_allowed ?size m l =
  of_alist_multi ~hashable:(Hashable.of_key m) ?growth_allowed ?size l
;;

let create_mapped ?growth_allowed ?size m ~get_key ~get_data l =
  create_mapped ~hashable:(Hashable.of_key m) ?growth_allowed ?size ~get_key ~get_data l
;;

let create_with_key ?growth_allowed ?size m ~get_key l =
  create_with_key ~hashable:(Hashable.of_key m) ?growth_allowed ?size ~get_key l
;;

let create_with_key_or_error ?growth_allowed ?size m ~get_key l =
  create_with_key_or_error ~hashable:(Hashable.of_key m) ?growth_allowed ?size ~get_key l
;;

let create_with_key_exn ?growth_allowed ?size m ~get_key l =
  create_with_key_exn ~hashable:(Hashable.of_key m) ?growth_allowed ?size ~get_key l
;;

let group ?growth_allowed ?size m ~get_key ~get_data ~combine l =
  group ~hashable:(Hashable.of_key m) ?growth_allowed ?size ~get_key ~get_data ~combine l
;;
