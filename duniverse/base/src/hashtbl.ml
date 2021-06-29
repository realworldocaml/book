open! Import
include Hashtbl_intf

module type Key = Key.S

let with_return = With_return.with_return
let hash_param = Hashable.hash_param
let hash = Hashable.hash
let raise_s = Error.raise_s

type ('k, 'v) t =
  { mutable table : ('k, 'v) Avltree.t array
  ; mutable length : int
  (* [recently_added] is the reference passed to [Avltree.add]. We put it in the hash
     table to avoid allocating it at every [set]. *)
  ; recently_added : bool ref
  ; growth_allowed : bool
  ; hashable : 'k Hashable.t
  ; mutable mutation_allowed : bool (* Set during all iteration operations *)
  }

type 'a key = 'a

let sexp_of_key t = t.hashable.Hashable.sexp_of_t
let compare_key t = t.hashable.Hashable.compare

let ensure_mutation_allowed t =
  if not t.mutation_allowed
  then failwith "Hashtbl: mutation not allowed during iteration"
;;

let without_mutating t f =
  if t.mutation_allowed
  then (
    t.mutation_allowed <- false;
    match f () with
    | x ->
      t.mutation_allowed <- true;
      x
    | exception exn ->
      t.mutation_allowed <- true;
      raise exn)
  else f ()
;;

(** Internally use a maximum size that is a power of 2. Reverses the above to find the
    floor power of 2 below the system max array length *)
let max_table_length = Int.floor_pow2 Array.max_length

(* The default size is chosen to be 0 (as opposed to 128 as it was before) because:
   - 128 can create substantial memory overhead (x10) when creating many tables, most
     of which are not big (say, if you have a hashtbl of hashtbl). And memory overhead is
     not that easy to profile.
   - if a hashtbl is going to grow, it's not clear why 128 is markedly better than other
     sizes (if you going to stick 1000 elements, you're going to grow the hashtable once
     or twice anyway)
   - in other languages (like rust, python, and apparently go), the default is also a
     small size. *)
let create ?(growth_allowed = true) ?(size = 0) ~hashable () =
  let size = Int.min (Int.max 1 size) max_table_length in
  let size = Int.ceil_pow2 size in
  { table = Array.create ~len:size Avltree.empty
  ; length = 0
  ; growth_allowed
  ; recently_added = ref false
  ; hashable
  ; mutation_allowed = true
  }
;;

(** Supplemental hash. This may not be necessary, it is intended as a defense against poor
    hash functions, for which the power of 2 sized table will be especially sensitive.
    With some testing we may choose to add it, but this table is designed to be robust to
    collisions, and in most of my testing this degrades performance. *)
let _supplemental_hash h =
  let h = h lxor ((h lsr 20) lxor (h lsr 12)) in
  h lxor (h lsr 7) lxor (h lsr 4)
;;

let slot t key =
  let hash = t.hashable.Hashable.hash key in
  (* this is always non-negative because we do [land] with non-negative number *)
  hash land (Array.length t.table - 1)
;;

let add_worker t ~replace ~key ~data =
  let i = slot t key in
  let root = t.table.(i) in
  let added = t.recently_added in
  added := false;
  let new_root =
    (* The avl tree might replace the value [replace=true] or do nothing [replace=false]
       to the entry, in that case the table did not get bigger, so we should not
       increment length, we pass in the bool ref t.added so that it can tell us whether
       it added or replaced. We do it this way to avoid extra allocation. Since the bool
       is an immediate it does not go through the write barrier. *)
    Avltree.add ~replace root ~compare:(compare_key t) ~added ~key ~data
  in
  if !added then t.length <- t.length + 1;
  (* This little optimization saves a caml_modify when the tree
     hasn't been rebalanced. *)
  if not (phys_equal new_root root) then t.table.(i) <- new_root
;;

let maybe_resize_table t =
  let len = Array.length t.table in
  let should_grow = t.length > len in
  if should_grow && t.growth_allowed
  then (
    let new_array_length = Int.min (len * 2) max_table_length in
    if new_array_length > len
    then (
      let new_table = Array.create ~len:new_array_length Avltree.empty in
      let old_table = t.table in
      t.table <- new_table;
      t.length <- 0;
      let f ~key ~data = add_worker ~replace:true t ~key ~data in
      for i = 0 to Array.length old_table - 1 do
        Avltree.iter old_table.(i) ~f
      done))
;;

let set t ~key ~data =
  ensure_mutation_allowed t;
  add_worker ~replace:true t ~key ~data;
  maybe_resize_table t
;;

let add t ~key ~data =
  ensure_mutation_allowed t;
  add_worker ~replace:false t ~key ~data;
  if !(t.recently_added)
  then (
    maybe_resize_table t;
    `Ok)
  else `Duplicate
;;

let add_exn t ~key ~data =
  match add t ~key ~data with
  | `Ok -> ()
  | `Duplicate ->
    let sexp_of_key = sexp_of_key t in
    let error = Error.create "Hashtbl.add_exn got key already present" key sexp_of_key in
    Error.raise error
;;

let clear t =
  ensure_mutation_allowed t;
  for i = 0 to Array.length t.table - 1 do
    t.table.(i) <- Avltree.empty
  done;
  t.length <- 0
;;

let find_and_call t key ~if_found ~if_not_found =
  (* with a good hash function these first two cases will be the overwhelming majority,
     and Avltree.find is recursive, so it can't be inlined, so doing this avoids a
     function call in most cases. *)
  match t.table.(slot t key) with
  | Avltree.Empty -> if_not_found key
  | Avltree.Leaf { key = k; value = v } ->
    if compare_key t k key = 0 then if_found v else if_not_found key
  | tree ->
    Avltree.find_and_call tree ~compare:(compare_key t) key ~if_found ~if_not_found
;;

let find_and_call1 t key ~a ~if_found ~if_not_found =
  match t.table.(slot t key) with
  | Avltree.Empty -> if_not_found key a
  | Avltree.Leaf { key = k; value = v } ->
    if compare_key t k key = 0 then if_found v a else if_not_found key a
  | tree ->
    Avltree.find_and_call1 tree ~compare:(compare_key t) key ~a ~if_found ~if_not_found
;;

let find_and_call2 t key ~a ~b ~if_found ~if_not_found =
  match t.table.(slot t key) with
  | Avltree.Empty -> if_not_found key a b
  | Avltree.Leaf { key = k; value = v } ->
    if compare_key t k key = 0 then if_found v a b else if_not_found key a b
  | tree ->
    Avltree.find_and_call2
      tree
      ~compare:(compare_key t)
      key
      ~a
      ~b
      ~if_found
      ~if_not_found
;;

let findi_and_call t key ~if_found ~if_not_found =
  (* with a good hash function these first two cases will be the overwhelming majority,
     and Avltree.find is recursive, so it can't be inlined, so doing this avoids a
     function call in most cases. *)
  match t.table.(slot t key) with
  | Avltree.Empty -> if_not_found key
  | Avltree.Leaf { key = k; value = v } ->
    if compare_key t k key = 0 then if_found ~key:k ~data:v else if_not_found key
  | tree ->
    Avltree.findi_and_call tree ~compare:(compare_key t) key ~if_found ~if_not_found
;;

let findi_and_call1 t key ~a ~if_found ~if_not_found =
  match t.table.(slot t key) with
  | Avltree.Empty -> if_not_found key a
  | Avltree.Leaf { key = k; value = v } ->
    if compare_key t k key = 0 then if_found ~key:k ~data:v a else if_not_found key a
  | tree ->
    Avltree.findi_and_call1 tree ~compare:(compare_key t) key ~a ~if_found ~if_not_found
;;

let findi_and_call2 t key ~a ~b ~if_found ~if_not_found =
  match t.table.(slot t key) with
  | Avltree.Empty -> if_not_found key a b
  | Avltree.Leaf { key = k; value = v } ->
    if compare_key t k key = 0 then if_found ~key:k ~data:v a b else if_not_found key a b
  | tree ->
    Avltree.findi_and_call2
      tree
      ~compare:(compare_key t)
      key
      ~a
      ~b
      ~if_found
      ~if_not_found
;;

let find =
  let if_found v = Some v in
  let if_not_found _ = None in
  fun t key -> find_and_call t key ~if_found ~if_not_found
;;

let mem t key =
  match t.table.(slot t key) with
  | Avltree.Empty -> false
  | Avltree.Leaf { key = k; value = _ } -> compare_key t k key = 0
  | tree -> Avltree.mem tree ~compare:(compare_key t) key
;;

let remove t key =
  ensure_mutation_allowed t;
  let i = slot t key in
  let root = t.table.(i) in
  let added_or_removed = t.recently_added in
  added_or_removed := false;
  let new_root =
    Avltree.remove root ~removed:added_or_removed ~compare:(compare_key t) key
  in
  if not (phys_equal root new_root) then t.table.(i) <- new_root;
  if !added_or_removed then t.length <- t.length - 1
;;

let length t = t.length
let is_empty t = length t = 0

let fold t ~init ~f =
  if length t = 0
  then init
  else (
    let n = Array.length t.table in
    let acc = ref init in
    let m = t.mutation_allowed in
    match
      t.mutation_allowed <- false;
      for i = 0 to n - 1 do
        match Array.unsafe_get t.table i with
        | Avltree.Empty -> ()
        | Avltree.Leaf { key; value = data } -> acc := f ~key ~data !acc
        | bucket -> acc := Avltree.fold bucket ~init:!acc ~f
      done
    with
    | () ->
      t.mutation_allowed <- m;
      !acc
    | exception exn ->
      t.mutation_allowed <- m;
      raise exn)
;;

let iteri t ~f =
  if t.length = 0
  then ()
  else (
    let n = Array.length t.table in
    let m = t.mutation_allowed in
    match
      t.mutation_allowed <- false;
      for i = 0 to n - 1 do
        match Array.unsafe_get t.table i with
        | Avltree.Empty -> ()
        | Avltree.Leaf { key; value = data } -> f ~key ~data
        | bucket -> Avltree.iter bucket ~f
      done
    with
    | () -> t.mutation_allowed <- m
    | exception exn ->
      t.mutation_allowed <- m;
      raise exn)
;;

let iter t ~f = iteri t ~f:(fun ~key:_ ~data -> f data)
let iter_keys t ~f = iteri t ~f:(fun ~key ~data:_ -> f key)

let rec choose_nonempty table i =
  let avltree = table.(i) in
  if Avltree.is_empty avltree
  then choose_nonempty table (i + 1)
  else Avltree.choose_exn avltree
;;

let choose_exn t =
  if t.length = 0 then raise_s (Sexp.message "[Hashtbl.choose_exn] of empty hashtbl" []);
  choose_nonempty t.table 0
;;

let choose t = if is_empty t then None else Some (choose_nonempty t.table 0)

let invariant invariant_key invariant_data t =
  for i = 0 to Array.length t.table - 1 do
    Avltree.invariant t.table.(i) ~compare:(compare_key t)
  done;
  let real_len =
    fold t ~init:0 ~f:(fun ~key ~data i ->
      invariant_key key;
      invariant_data data;
      i + 1)
  in
  assert (real_len = t.length)
;;

let find_exn =
  let if_found v _ = v in
  let if_not_found k t =
    raise
      (Not_found_s (List [ Atom "Hashtbl.find_exn: not found"; t.hashable.sexp_of_t k ]))
  in
  let find_exn t key = find_and_call1 t key ~a:t ~if_found ~if_not_found in
  (* named to preserve symbol in compiled binary *)
  find_exn
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
  iteri t ~f:(fun ~key ~data -> set new_t ~key ~data:(f ~key ~data));
  new_t
;;

let map t ~f = mapi t ~f:(fun ~key:_ ~data -> f data)
let copy t = map t ~f:Fn.id

let filter_mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed ~hashable:t.hashable ~size:t.length ()
  in
  iteri t ~f:(fun ~key ~data ->
    match f ~key ~data with
    | Some new_data -> set new_t ~key ~data:new_data
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
    match (f ~key ~data : _ Either.t) with
    | First new_data -> set t0 ~key ~data:new_data
    | Second new_data -> set t1 ~key ~data:new_data);
  t0, t1
;;

let partition_map t ~f = partition_mapi t ~f:(fun ~key:_ ~data -> f data)

let partitioni_tf t ~f =
  partition_mapi t ~f:(fun ~key ~data ->
    if f ~key ~data then First data else Second data)
;;

let partition_tf t ~f = partitioni_tf t ~f:(fun ~key:_ ~data -> f data)

let find_or_add t id ~default =
  match find t id with
  | Some x -> x
  | None ->
    let default = default () in
    set t ~key:id ~data:default;
    default
;;

let findi_or_add t id ~default =
  match find t id with
  | Some x -> x
  | None ->
    let default = default id in
    set t ~key:id ~data:default;
    default
;;

(* Some hashtbl implementations may be able to perform this more efficiently than two
   separate lookups *)
let find_and_remove t id =
  let result = find t id in
  if Option.is_some result then remove t id;
  result
;;


let change t id ~f =
  match f (find t id) with
  | None -> remove t id
  | Some data -> set t ~key:id ~data
;;

let update t id ~f = set t ~key:id ~data:(f (find t id))

let incr_by ~remove_if_zero t key by =
  if remove_if_zero
  then
    change t key ~f:(fun opt ->
      match by + Option.value opt ~default:0 with
      | 0 -> None
      | n -> Some n)
  else
    update t key ~f:(function
      | None -> by
      | Some i -> by + i)
;;

let incr ?(by = 1) ?(remove_if_zero = false) t key = incr_by ~remove_if_zero t key by
let decr ?(by = 1) ?(remove_if_zero = false) t key = incr_by ~remove_if_zero t key (-by)

let add_multi t ~key ~data =
  update t key ~f:(function
    | None -> [ data ]
    | Some l -> data :: l)
;;

let remove_multi t key =
  match find t key with
  | None -> ()
  | Some [] | Some [ _ ] -> remove t key
  | Some (_ :: tl) -> set t ~key ~data:tl
;;

let find_multi t key =
  match find t key with
  | None -> []
  | Some l -> l
;;

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
    if mem res key then dupes := key :: !dupes else set res ~key ~data);
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
    Or_error.error "Hashtbl.of_alist_exn: duplicate key" key sexp_of_key
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

let sexp_of_t sexp_of_key sexp_of_data t =
  t
  |> to_alist
  |> List.sort ~compare:(fun (k1, _) (k2, _) -> t.hashable.compare k1 k2)
  |> sexp_of_list (sexp_of_pair sexp_of_key sexp_of_data)
;;

let t_of_sexp ~hashable k_of_sexp d_of_sexp sexp =
  let alist = list_of_sexp (pair_of_sexp k_of_sexp d_of_sexp) sexp in
  match of_alist ~hashable alist ~size:(List.length alist) with
  | `Ok v -> v
  | `Duplicate_key k ->
    (* find the sexp of a duplicate key, so the error is narrowed to a key and not
       the whole map *)
    let alist_sexps = list_of_sexp (pair_of_sexp Fn.id Fn.id) sexp in
    let found_first_k = ref false in
    List.iter2_exn alist alist_sexps ~f:(fun (k2, _) (k2_sexp, _) ->
      if hashable.compare k k2 = 0
      then
        if !found_first_k
        then of_sexp_error "Hashtbl.t_of_sexp: duplicate key" k2_sexp
        else found_first_k := true);
    assert false
;;

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
    set groups ~key ~data)
;;

let group ?growth_allowed ?size ~hashable ~get_key ~get_data ~combine rows =
  let res = create ?growth_allowed ?size ~hashable () in
  add_to_groups res ~get_key ~get_data ~combine ~rows;
  res
;;

let create_with_key ?growth_allowed ?size ~hashable ~get_key rows =
  create_mapped ?growth_allowed ?size ~hashable ~get_key ~get_data:Fn.id rows
;;

let create_with_key_or_error ?growth_allowed ?size ~hashable ~get_key rows =
  match create_with_key ?growth_allowed ?size ~hashable ~get_key rows with
  | `Ok t -> Result.Ok t
  | `Duplicate_keys keys ->
    let sexp_of_key = hashable.Hashable.sexp_of_t in
    Or_error.error_s
      (Sexp.message
         "Hashtbl.create_with_key: duplicate keys"
         [ "keys", sexp_of_list sexp_of_key keys ])
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
    then invalid_arg "Hashtbl.merge: different 'hashable' values";
    let new_t =
      create
        ~growth_allowed:t_left.growth_allowed
        ~hashable:t_left.hashable
        ~size:t_left.length
        ()
    in
    without_mutating t_left (fun () ->
      without_mutating t_right (fun () ->
        iteri t_left ~f:(fun ~key ~data:left ->
          match find t_right key with
          | None -> maybe_set new_t ~key ~f (`Left left)
          | Some right -> maybe_set new_t ~key ~f (`Both (left, right)));
        iteri t_right ~f:(fun ~key ~data:right ->
          match find t_left key with
          | None -> maybe_set new_t ~key ~f (`Right right)
          | Some _ -> ()
          (* already done above *))));
    new_t
;;

let merge_into ~src ~dst ~f =
  iteri src ~f:(fun ~key ~data ->
    let dst_data = find dst key in
    let action = without_mutating dst (fun () -> f ~key data dst_data) in
    match (action : _ Merge_into_action.t) with
    | Remove -> remove dst key
    | Set_to data ->
      (match dst_data with
       | None -> set dst ~key ~data
       | Some dst_data -> if not (phys_equal dst_data data) then set dst ~key ~data))
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
  ensure_mutation_allowed t;
  without_mutating t (fun () -> Array.iter t.table ~f:(Avltree.mapi_inplace ~f))
;;

let map_inplace t ~f = mapi_inplace t ~f:(fun ~key:_ ~data -> f data)

let equal equal t t' =
  length t = length t'
  && with_return (fun r ->
    without_mutating t' (fun () ->
      iteri t ~f:(fun ~key ~data ->
        match find t' key with
        | None -> r.return false
        | Some data' -> if not (equal data data') then r.return false));
    true)
;;

let similar = equal

module Accessors = struct
  let invariant = invariant
  let choose = choose
  let choose_exn = choose_exn
  let clear = clear
  let copy = copy
  let remove = remove
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
  let find_and_call1 = find_and_call1
  let find_and_call2 = find_and_call2
  let findi_and_call = findi_and_call
  let findi_and_call1 = findi_and_call1
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

module Creators (Key : sig
    type 'a t

    val hashable : 'a t Hashable.t
  end) : sig
  type ('a, 'b) t_ = ('a Key.t, 'b) t

  val t_of_sexp : (Sexp.t -> 'a Key.t) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t_

  include
    Creators_generic
    with type ('a, 'b) t := ('a, 'b) t_
    with type 'a key := 'a Key.t
    with type ('key, 'data, 'a) create_options :=
      ('key, 'data, 'a) create_options_without_first_class_module
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

  let t_of_sexp k_of_sexp d_of_sexp sexp = t_of_sexp ~hashable k_of_sexp d_of_sexp sexp

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
  type nonrec ('a, 'b) t = ('a, 'b) t
  type 'a key = 'a

  let hashable = Hashable.poly

  include Creators (struct
      type 'a t = 'a

      let hashable = hashable
    end)

  include Accessors

  let sexp_of_t = sexp_of_t
end

module Private = struct
  module type Creators_generic = Creators_generic
  module type Hashable = Hashable.Hashable

  type nonrec ('key, 'data, 'z) create_options_without_first_class_module =
    ('key, 'data, 'z) create_options_without_first_class_module

  let hashable t = t.hashable
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

let hashable_s t = Hashable.to_key t.hashable

module M (K : T.T) = struct
  type nonrec 'v t = (K.t, 'v) t
end

module type Sexp_of_m = sig
  type t [@@deriving_inline sexp_of]

  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

  [@@@end]
end

module type M_of_sexp = sig
  type t [@@deriving_inline of_sexp]

  val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t

  [@@@end]

  include Key.S with type t := t
end

let sexp_of_m__t (type k) (module K : Sexp_of_m with type t = k) sexp_of_v t =
  sexp_of_t K.sexp_of_t sexp_of_v t
;;

let m__t_of_sexp (type k) (module K : M_of_sexp with type t = k) v_of_sexp sexp =
  t_of_sexp ~hashable:(Hashable.of_key (module K)) K.t_of_sexp v_of_sexp sexp
;;

(* typechecking this code is a compile-time test that [Creators] is a specialization of
   [Creators_generic].  *)
module Check : sig end = struct
  module Make_creators_check
      (Type : T.T2)
      (Key : T.T1)
      (Options : T.T3)
      (M : Creators_generic
       with type ('a, 'b) t := ('a, 'b) Type.t
       with type 'a key := 'a Key.t
       with type ('a, 'b, 'z) create_options := ('a, 'b, 'z) Options.t) =
  struct end

  module Check_creators_is_specialization_of_creators_generic (M : Creators) =
    Make_creators_check
      (struct
        type ('a, 'b) t = ('a, 'b) M.t
      end)
      (struct
        type 'a t = 'a
      end)
      (struct
        type ('a, 'b, 'z) t = ('a, 'b, 'z) create_options
      end)
      (struct
        include M

        let create ?growth_allowed ?size m () = create ?growth_allowed ?size m
      end)
end
