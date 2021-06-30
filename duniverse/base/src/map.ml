(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Apache 2.0 license. See ../THIRD-PARTY.txt  *)
(*  for details.                                                       *)
(*                                                                     *)
(***********************************************************************)

open! Import
module List = List0

include (
  Map_intf :
  sig
    module Or_duplicate = Map_intf.Or_duplicate
    module Continue_or_stop = Map_intf.Continue_or_stop
    module With_comparator = Map_intf.With_comparator
    module With_first_class_module = Map_intf.With_first_class_module
    module Without_comparator = Map_intf.Without_comparator

    (* The module susbstitutions below are needed for older versions of OCaml
       (before 4.07), because back then [module type of] did not keep module
       aliases. *)

    include module type of struct
      include Map_intf
    end
    with module Finished_or_unfinished := Map_intf.Finished_or_unfinished
     and module Or_duplicate := Or_duplicate
     and module Continue_or_stop := Continue_or_stop
     and module With_comparator := With_comparator
     and module With_first_class_module := With_first_class_module
     and module Without_comparator := Without_comparator
  end)

module Finished_or_unfinished = struct
  include Map_intf.Finished_or_unfinished

  (* These two functions are tested in [test_map.ml] to make sure our use of
     [Caml.Obj.magic] is correct and safe. *)
  let of_continue_or_stop : Continue_or_stop.t -> t = Caml.Obj.magic
  let to_continue_or_stop : t -> Continue_or_stop.t = Caml.Obj.magic
end

let with_return = With_return.with_return

exception Duplicate [@@deriving_inline sexp]

let () =
  Ppx_sexp_conv_lib.Conv.Exn_converter.add [%extension_constructor Duplicate] (function
    | Duplicate -> Ppx_sexp_conv_lib.Sexp.Atom "map.ml.Duplicate"
    | _ -> assert false)
;;

[@@@end]

module Tree0 = struct
  type ('k, 'v) t =
    | Empty
    | Leaf of 'k * 'v
    | Node of ('k, 'v) t * 'k * 'v * ('k, 'v) t * int

  type ('k, 'v) tree = ('k, 'v) t

  let height = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node (_, _, _, _, h) -> h
  ;;

  let invariants =
    let in_range lower upper compare_key k =
      (match lower with
       | None -> true
       | Some lower -> compare_key lower k < 0)
      &&
      match upper with
      | None -> true
      | Some upper -> compare_key k upper < 0
    in
    let rec loop lower upper compare_key t =
      match t with
      | Empty -> true
      | Leaf (k, _) -> in_range lower upper compare_key k
      | Node (l, k, _, r, h) ->
        let hl = height l
        and hr = height r in
        abs (hl - hr) <= 2
        && h = max hl hr + 1
        && in_range lower upper compare_key k
        && loop lower (Some k) compare_key l
        && loop (Some k) upper compare_key r
    in
    fun t ~compare_key -> loop None None compare_key t
  ;;

  (* precondition: |height(l) - height(r)| <= 2 *)
  let create l x d r =
    let hl = height l
    and hr = height r in
    if hl = 0 && hr = 0
    then Leaf (x, d)
    else Node (l, x, d, r, if hl >= hr then hl + 1 else hr + 1)
  ;;

  let singleton key data = Leaf (key, data)

  (* We must call [f] with increasing indexes, because the bin_prot reader in
     Core_kernel.Map needs it. *)
  let of_increasing_iterator_unchecked ~len ~f =
    let rec loop n ~f i : (_, _) t =
      match n with
      | 0 -> Empty
      | 1 ->
        let k, v = f i in
        Leaf (k, v)
      | 2 ->
        let kl, vl = f i in
        let k, v = f (i + 1) in
        Node (Leaf (kl, vl), k, v, Empty, 2)
      | 3 ->
        let kl, vl = f i in
        let k, v = f (i + 1) in
        let kr, vr = f (i + 2) in
        Node (Leaf (kl, vl), k, v, Leaf (kr, vr), 2)
      | n ->
        let left_length = n lsr 1 in
        let right_length = n - left_length - 1 in
        let left = loop left_length ~f i in
        let k, v = f (i + left_length) in
        let right = loop right_length ~f (i + left_length + 1) in
        create left k v right
    in
    loop len ~f 0
  ;;

  let of_sorted_array_unchecked array ~compare_key =
    let array_length = Array.length array in
    let next =
      if array_length < 2
         ||
         let k0, _ = array.(0) in
         let k1, _ = array.(1) in
         compare_key k0 k1 < 0
      then fun i -> array.(i)
      else fun i -> array.(array_length - 1 - i)
    in
    of_increasing_iterator_unchecked ~len:array_length ~f:next, array_length
  ;;

  let of_sorted_array array ~compare_key =
    match array with
    | [||] | [| _ |] -> Result.Ok (of_sorted_array_unchecked array ~compare_key)
    | _ ->
      with_return (fun r ->
        let increasing =
          match compare_key (fst array.(0)) (fst array.(1)) with
          | 0 ->
            r.return (Or_error.error_string "of_sorted_array: duplicated elements")
          | i -> i < 0
        in
        for i = 1 to Array.length array - 2 do
          match compare_key (fst array.(i)) (fst array.(i + 1)) with
          | 0 ->
            r.return (Or_error.error_string "of_sorted_array: duplicated elements")
          | i ->
            if Poly.( <> ) (i < 0) increasing
            then
              r.return
                (Or_error.error_string "of_sorted_array: elements are not ordered")
        done;
        Result.Ok (of_sorted_array_unchecked array ~compare_key))
  ;;

  (* precondition: |height(l) - height(r)| <= 3 *)
  let bal l x d r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2
    then (
      match l with
      | Empty -> invalid_arg "Map.bal"
      | Leaf _ -> assert false (* height(Leaf) = 1 && 1 is not larger than hr + 2 *)
      | Node (ll, lv, ld, lr, _) ->
        if height ll >= height lr
        then create ll lv ld (create lr x d r)
        else (
          match lr with
          | Empty -> invalid_arg "Map.bal"
          | Leaf (lrv, lrd) ->
            create (create ll lv ld Empty) lrv lrd (create Empty x d r)
          | Node (lrl, lrv, lrd, lrr, _) ->
            create (create ll lv ld lrl) lrv lrd (create lrr x d r)))
    else if hr > hl + 2
    then (
      match r with
      | Empty -> invalid_arg "Map.bal"
      | Leaf _ -> assert false (* height(Leaf) = 1 && 1 is not larger than hl + 2 *)
      | Node (rl, rv, rd, rr, _) ->
        if height rr >= height rl
        then create (create l x d rl) rv rd rr
        else (
          match rl with
          | Empty -> invalid_arg "Map.bal"
          | Leaf (rlv, rld) ->
            create (create l x d Empty) rlv rld (create Empty rv rd rr)
          | Node (rll, rlv, rld, rlr, _) ->
            create (create l x d rll) rlv rld (create rlr rv rd rr)))
    else create l x d r
  ;;

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let raise_key_already_present ~key ~sexp_of_key =
    Error.raise_s
      (Sexp.message "[Map.add_exn] got key already present" [ "key", key |> sexp_of_key ])
  ;;

  module Add_or_set = struct
    type t =
      | Add_exn_internal
      | Add_exn
      | Set
  end


  let rec find_and_add_or_set
            t
            ~length
            ~key:x
            ~data
            ~compare_key
            ~sexp_of_key
            ~(add_or_set : Add_or_set.t)
    =
    match t with
    | Empty -> Leaf (x, data), length + 1
    | Leaf (v, d) ->
      let c = compare_key x v in
      if c = 0
      then (
        match add_or_set with
        | Add_exn_internal -> Exn.raise_without_backtrace Duplicate
        | Add_exn -> raise_key_already_present ~key:x ~sexp_of_key
        | Set -> Leaf (x, data), length)
      else if c < 0
      then Node (Leaf (x, data), v, d, Empty, 2), length + 1
      else Node (Empty, v, d, Leaf (x, data), 2), length + 1
    | Node (l, v, d, r, h) ->
      let c = compare_key x v in
      if c = 0
      then (
        match add_or_set with
        | Add_exn_internal -> Exn.raise_without_backtrace Duplicate
        | Add_exn -> raise_key_already_present ~key:x ~sexp_of_key
        | Set -> Node (l, x, data, r, h), length)
      else if c < 0
      then (
        let l, length =
          find_and_add_or_set
            ~length
            ~key:x
            ~data
            l
            ~compare_key
            ~sexp_of_key
            ~add_or_set
        in
        bal l v d r, length)
      else (
        let r, length =
          find_and_add_or_set
            ~length
            ~key:x
            ~data
            r
            ~compare_key
            ~sexp_of_key
            ~add_or_set
        in
        bal l v d r, length)
  ;;

  let add_exn t ~length ~key ~data ~compare_key ~sexp_of_key =
    find_and_add_or_set
      t
      ~length
      ~key
      ~data
      ~compare_key
      ~sexp_of_key
      ~add_or_set:Add_exn
  ;;

  let add_exn_internal t ~length ~key ~data ~compare_key ~sexp_of_key =
    find_and_add_or_set
      t
      ~length
      ~key
      ~data
      ~compare_key
      ~sexp_of_key
      ~add_or_set:Add_exn_internal
  ;;

  let set t ~length ~key ~data ~compare_key =
    find_and_add_or_set
      t
      ~length
      ~key
      ~data
      ~compare_key
      ~sexp_of_key:(fun _ -> List [])
      ~add_or_set:Set
  ;;

  let set' t key data ~compare_key = fst (set t ~length:0 ~key ~data ~compare_key)

  module Build_increasing = struct
    module Fragment = struct
      type nonrec ('k, 'v) t =
        { left_subtree : ('k, 'v) t
        ; key : 'k
        ; data : 'v
        }

      let singleton_to_tree_exn = function
        | { left_subtree = Empty; key; data } -> singleton key data
        | _ -> failwith "Map.singleton_to_tree_exn: not a singleton"
      ;;

      let singleton ~key ~data = { left_subtree = Empty; key; data }

      (* precondition: |height(l.left_subtree) - height(r)| <= 2,
         max_key(l) < min_key(r)
      *)
      let collapse l r = create l.left_subtree l.key l.data r

      (* precondition: |height(l.left_subtree) - height(r.left_subtree)| <= 2,
         max_key(l) < min_key(r)
      *)
      let join l r = { r with left_subtree = collapse l r.left_subtree }
      let max_key t = t.key
    end

    (** Build trees from singletons in a balanced way by using skew binary encoding.
        Each level contains trees of the same height, consecutive levels have consecutive
        heights. There are no gaps. The first level are single keys.
    *)
    type ('k, 'v) t =
      | Zero of unit
      (* [unit] to make pattern matching faster *)
      | One of ('k, 'v) t * ('k, 'v) Fragment.t
      | Two of ('k, 'v) t * ('k, 'v) Fragment.t * ('k, 'v) Fragment.t

    let empty = Zero ()

    let add_unchecked =
      let rec go t x =
        match t with
        | Zero () -> One (t, x)
        | One (t, y) -> Two (t, y, x)
        | Two (t, z, y) -> One (go t (Fragment.join z y), x)
      in
      fun t ~key ~data -> go t (Fragment.singleton ~key ~data)
    ;;

    let to_tree =
      let rec go t r =
        match t with
        | Zero () -> r
        | One (t, l) -> go t (Fragment.collapse l r)
        | Two (t, ll, l) -> go t (Fragment.collapse (Fragment.join ll l) r)
      in
      function
      | Zero () -> Empty
      | One (t, r) -> go t (Fragment.singleton_to_tree_exn r)
      | Two (t, l, r) -> go (One (t, l)) (Fragment.singleton_to_tree_exn r)
    ;;

    let max_key = function
      | Zero () -> None
      | One (_, r) | Two (_, _, r) -> Some (Fragment.max_key r)
    ;;
  end

  let of_increasing_sequence seq ~compare_key =
    with_return (fun { return } ->
      let builder, length =
        Sequence.fold
          seq
          ~init:(Build_increasing.empty, 0)
          ~f:(fun (builder, length) (key, data) ->
            match Build_increasing.max_key builder with
            | Some prev_key when compare_key prev_key key >= 0 ->
              return
                (Or_error.error_string "of_increasing_sequence: non-increasing key")
            | _ -> Build_increasing.add_unchecked builder ~key ~data, length + 1)
      in
      Ok (Build_increasing.to_tree builder, length))
  ;;

  (* Like [bal] but allows any difference in height between [l] and [r].

     O(|height l - height r|) *)
  let rec join l k d r ~compare_key =
    match l, r with
    | Empty, _ -> set' r k d ~compare_key
    | _, Empty -> set' l k d ~compare_key
    | Leaf (lk, ld), _ -> set' (set' r k d ~compare_key) lk ld ~compare_key
    | _, Leaf (rk, rd) -> set' (set' l k d ~compare_key) rk rd ~compare_key
    | Node (ll, lk, ld, lr, lh), Node (rl, rk, rd, rr, rh) ->
      (* [bal] requires height difference <= 3. *)
      if lh > rh + 3
      (* [height lr >= height r],
         therefore [height (join lr k d r ...)] is [height rl + 1] or [height rl]
         therefore the height difference with [ll] will be <= 3 *)
      then bal ll lk ld (join lr k d r ~compare_key)
      else if rh > lh + 3
      then bal (join l k d rl ~compare_key) rk rd rr
      else bal l k d r
  ;;

  let rec split t x ~compare_key =
    match t with
    | Empty -> Empty, None, Empty
    | Leaf (k, d) ->
      let cmp = compare_key x k in
      if cmp = 0
      then Empty, Some (k, d), Empty
      else if cmp < 0
      then Empty, None, t
      else t, None, Empty
    | Node (l, k, d, r, _) ->
      let cmp = compare_key x k in
      if cmp = 0
      then l, Some (k, d), r
      else if cmp < 0
      then (
        let ll, maybe, lr = split l x ~compare_key in
        ll, maybe, join lr k d r ~compare_key)
      else (
        let rl, maybe, rr = split r x ~compare_key in
        join l k d rl ~compare_key, maybe, rr)
  ;;

  let split_and_reinsert_boundary t ~into x ~compare_key =
    let left, boundary_opt, right = split t x ~compare_key in
    match boundary_opt with
    | None -> left, right
    | Some (key, data) ->
      let insert_into tree = fst (set tree ~key ~data ~length:0 ~compare_key) in
      (match into with
       | `Left -> insert_into left, right
       | `Right -> left, insert_into right)
  ;;

  let split_range
        t
        ~(lower_bound : 'a Maybe_bound.t)
        ~(upper_bound : 'a Maybe_bound.t)
        ~compare_key
    =
    if Maybe_bound.bounds_crossed
         ~compare:compare_key
         ~lower:lower_bound
         ~upper:upper_bound
    then empty, empty, empty
    else (
      let left, mid_and_right =
        match lower_bound with
        | Unbounded -> empty, t
        | Incl lb -> split_and_reinsert_boundary ~into:`Right t lb ~compare_key
        | Excl lb -> split_and_reinsert_boundary ~into:`Left t lb ~compare_key
      in
      let mid, right =
        match upper_bound with
        | Unbounded -> mid_and_right, empty
        | Incl lb ->
          split_and_reinsert_boundary ~into:`Left mid_and_right lb ~compare_key
        | Excl lb ->
          split_and_reinsert_boundary ~into:`Right mid_and_right lb ~compare_key
      in
      left, mid, right)
  ;;

  let rec find t x ~compare_key =
    match t with
    | Empty -> None
    | Leaf (v, d) -> if compare_key x v = 0 then Some d else None
    | Node (l, v, d, r, _) ->
      let c = compare_key x v in
      if c = 0 then Some d else find (if c < 0 then l else r) x ~compare_key
  ;;

  let add_multi t ~length ~key ~data ~compare_key =
    let data = data :: Option.value (find t key ~compare_key) ~default:[] in
    set ~length ~key ~data t ~compare_key
  ;;

  let find_multi t x ~compare_key =
    match find t x ~compare_key with
    | None -> []
    | Some l -> l
  ;;

  let find_exn =
    let if_not_found key ~sexp_of_key =
      raise (Not_found_s (List [ Atom "Map.find_exn: not found"; sexp_of_key key ]))
    in
    let rec find_exn t x ~compare_key ~sexp_of_key =
      match t with
      | Empty -> if_not_found x ~sexp_of_key
      | Leaf (v, d) -> if compare_key x v = 0 then d else if_not_found x ~sexp_of_key
      | Node (l, v, d, r, _) ->
        let c = compare_key x v in
        if c = 0
        then d
        else find_exn (if c < 0 then l else r) x ~compare_key ~sexp_of_key
    in
    (* named to preserve symbol in compiled binary *)
    find_exn
  ;;

  let mem t x ~compare_key = Option.is_some (find t x ~compare_key)

  let rec min_elt = function
    | Empty -> None
    | Leaf (k, d) -> Some (k, d)
    | Node (Empty, k, d, _, _) -> Some (k, d)
    | Node (l, _, _, _, _) -> min_elt l
  ;;

  exception Map_min_elt_exn_of_empty_map [@@deriving_inline sexp]

  let () =
    Ppx_sexp_conv_lib.Conv.Exn_converter.add
      [%extension_constructor Map_min_elt_exn_of_empty_map]
      (function
        | Map_min_elt_exn_of_empty_map ->
          Ppx_sexp_conv_lib.Sexp.Atom "map.ml.Tree0.Map_min_elt_exn_of_empty_map"
        | _ -> assert false)
  ;;

  [@@@end]

  exception Map_max_elt_exn_of_empty_map [@@deriving_inline sexp]

  let () =
    Ppx_sexp_conv_lib.Conv.Exn_converter.add
      [%extension_constructor Map_max_elt_exn_of_empty_map]
      (function
        | Map_max_elt_exn_of_empty_map ->
          Ppx_sexp_conv_lib.Sexp.Atom "map.ml.Tree0.Map_max_elt_exn_of_empty_map"
        | _ -> assert false)
  ;;

  [@@@end]

  let min_elt_exn t =
    match min_elt t with
    | None -> raise Map_min_elt_exn_of_empty_map
    | Some v -> v
  ;;

  let rec max_elt = function
    | Empty -> None
    | Leaf (k, d) -> Some (k, d)
    | Node (_, k, d, Empty, _) -> Some (k, d)
    | Node (_, _, _, r, _) -> max_elt r
  ;;

  let max_elt_exn t =
    match max_elt t with
    | None -> raise Map_max_elt_exn_of_empty_map
    | Some v -> v
  ;;

  let rec remove_min_elt t =
    match t with
    | Empty -> invalid_arg "Map.remove_min_elt"
    | Leaf _ -> Empty
    | Node (Empty, _, _, r, _) -> r
    | Node (l, x, d, r, _) -> bal (remove_min_elt l) x d r
  ;;

  let append ~lower_part ~upper_part ~compare_key =
    match max_elt lower_part, min_elt upper_part with
    | None, _ -> `Ok upper_part
    | _, None -> `Ok lower_part
    | Some (max_lower, _), Some (min_upper, v) when compare_key max_lower min_upper < 0
      ->
      let upper_part_without_min = remove_min_elt upper_part in
      `Ok (join ~compare_key lower_part min_upper v upper_part_without_min)
    | _ -> `Overlapping_key_ranges
  ;;

  let fold_range_inclusive =
    (* This assumes that min <= max, which is checked by the outer function. *)
    let rec go t ~min ~max ~init ~f ~compare_key =
      match t with
      | Empty -> init
      | Leaf (k, d) ->
        if compare_key k min < 0 || compare_key k max > 0
        then (* k < min || k > max *)
          init
        else f ~key:k ~data:d init
      | Node (l, k, d, r, _) ->
        let c_min = compare_key k min in
        if c_min < 0
        then
          (* if k < min, then this node and its left branch are outside our range *)
          go r ~min ~max ~init ~f ~compare_key
        else if c_min = 0
        then
          (* if k = min, then this node's left branch is outside our range *)
          go r ~min ~max ~init:(f ~key:k ~data:d init) ~f ~compare_key
        else (
          (* k > min *)
          let z = go l ~min ~max ~init ~f ~compare_key in
          let c_max = compare_key k max in
          (* if k > max, we're done *)
          if c_max > 0
          then z
          else (
            let z = f ~key:k ~data:d z in
            (* if k = max, then we fold in this one last value and we're done *)
            if c_max = 0 then z else go r ~min ~max ~init:z ~f ~compare_key))
    in
    fun t ~min ~max ~init ~f ~compare_key ->
      if compare_key min max <= 0 then go t ~min ~max ~init ~f ~compare_key else init
  ;;

  let range_to_alist t ~min ~max ~compare_key =
    List.rev
      (fold_range_inclusive
         t
         ~min
         ~max
         ~init:[]
         ~f:(fun ~key ~data l -> (key, data) :: l)
         ~compare_key)
  ;;

  let concat_unchecked t1 t2 =
    match t1, t2 with
    | Empty, t -> t
    | t, Empty -> t
    | _, _ ->
      let x, d = min_elt_exn t2 in
      bal t1 x d (remove_min_elt t2)
  ;;

  let rec remove t x ~length ~compare_key =
    match t with
    | Empty -> Empty, length
    | Leaf (v, _) -> if compare_key x v = 0 then Empty, length - 1 else t, length
    | Node (l, v, d, r, _) ->
      let c = compare_key x v in
      if c = 0
      then concat_unchecked l r, length - 1
      else if c < 0
      then (
        let l, length = remove l x ~length ~compare_key in
        bal l v d r, length)
      else (
        let r, length = remove r x ~length ~compare_key in
        bal l v d r, length)
  ;;

  (* Use exception to avoid tree-rebuild in no-op case *)
  exception Change_no_op

  let change t key ~f ~length ~compare_key =
    let rec change_core t key f =
      match t with
      | Empty ->
        (match f None with
         | None -> raise Change_no_op (* equivalent to returning: Empty *)
         | Some data -> Leaf (key, data), length + 1)
      | Leaf (v, d) ->
        let c = compare_key key v in
        if c = 0
        then (
          match f (Some d) with
          | None -> Empty, length - 1
          | Some d' -> Leaf (v, d'), length)
        else if c < 0
        then (
          let l, length = change_core Empty key f in
          bal l v d Empty, length)
        else (
          let r, length = change_core Empty key f in
          bal Empty v d r, length)
      | Node (l, v, d, r, h) ->
        let c = compare_key key v in
        if c = 0
        then (
          match f (Some d) with
          | None -> concat_unchecked l r, length - 1
          | Some data -> Node (l, key, data, r, h), length)
        else if c < 0
        then (
          let l, length = change_core l key f in
          bal l v d r, length)
        else (
          let r, length = change_core r key f in
          bal l v d r, length)
    in
    try change_core t key f with
    | Change_no_op -> t, length
  ;;

  let update t key ~f ~length ~compare_key =
    let rec update_core t key f =
      match t with
      | Empty ->
        let data = f None in
        Leaf (key, data), length + 1
      | Leaf (v, d) ->
        let c = compare_key key v in
        if c = 0
        then (
          let d' = f (Some d) in
          Leaf (v, d'), length)
        else if c < 0
        then (
          let l, length = update_core Empty key f in
          bal l v d Empty, length)
        else (
          let r, length = update_core Empty key f in
          bal Empty v d r, length)
      | Node (l, v, d, r, h) ->
        let c = compare_key key v in
        if c = 0
        then (
          let data = f (Some d) in
          Node (l, key, data, r, h), length)
        else if c < 0
        then (
          let l, length = update_core l key f in
          bal l v d r, length)
        else (
          let r, length = update_core r key f in
          bal l v d r, length)
    in
    update_core t key f
  ;;

  let remove_multi t key ~length ~compare_key =
    change t key ~length ~compare_key ~f:(function
      | None | Some ([] | [ _ ]) -> None
      | Some (_ :: (_ :: _ as non_empty_tail)) -> Some non_empty_tail)
  ;;

  let rec iter_keys t ~f =
    match t with
    | Empty -> ()
    | Leaf (v, _) -> f v
    | Node (l, v, _, r, _) ->
      iter_keys ~f l;
      f v;
      iter_keys ~f r
  ;;

  let rec iter t ~f =
    match t with
    | Empty -> ()
    | Leaf (_, d) -> f d
    | Node (l, _, d, r, _) ->
      iter ~f l;
      f d;
      iter ~f r
  ;;

  let rec iteri t ~f =
    match t with
    | Empty -> ()
    | Leaf (v, d) -> f ~key:v ~data:d
    | Node (l, v, d, r, _) ->
      iteri ~f l;
      f ~key:v ~data:d;
      iteri ~f r
  ;;

  let iteri_until =
    let rec iteri_until_loop t ~f : Continue_or_stop.t =
      match t with
      | Empty -> Continue
      | Leaf (v, d) -> f ~key:v ~data:d
      | Node (l, v, d, r, _) ->
        (match iteri_until_loop ~f l with
         | Stop -> Stop
         | Continue ->
           (match f ~key:v ~data:d with
            | Stop -> Stop
            | Continue -> iteri_until_loop ~f r))
    in
    fun t ~f -> Finished_or_unfinished.of_continue_or_stop (iteri_until_loop t ~f)
  ;;

  let rec map t ~f =
    match t with
    | Empty -> Empty
    | Leaf (v, d) -> Leaf (v, f d)
    | Node (l, v, d, r, h) ->
      let l' = map ~f l in
      let d' = f d in
      let r' = map ~f r in
      Node (l', v, d', r', h)
  ;;

  let rec mapi t ~f =
    match t with
    | Empty -> Empty
    | Leaf (v, d) -> Leaf (v, f ~key:v ~data:d)
    | Node (l, v, d, r, h) ->
      let l' = mapi ~f l in
      let d' = f ~key:v ~data:d in
      let r' = mapi ~f r in
      Node (l', v, d', r', h)
  ;;

  let rec fold t ~init:accu ~f =
    match t with
    | Empty -> accu
    | Leaf (v, d) -> f ~key:v ~data:d accu
    | Node (l, v, d, r, _) -> fold ~f r ~init:(f ~key:v ~data:d (fold ~f l ~init:accu))
  ;;

  let rec fold_right t ~init:accu ~f =
    match t with
    | Empty -> accu
    | Leaf (v, d) -> f ~key:v ~data:d accu
    | Node (l, v, d, r, _) ->
      fold_right ~f l ~init:(f ~key:v ~data:d (fold_right ~f r ~init:accu))
  ;;

  let filter_keys t ~f ~compare_key =
    fold ~init:(Empty, 0) t ~f:(fun ~key ~data (accu, length) ->
      if f key then set ~length ~key ~data accu ~compare_key else accu, length)
  ;;


  let filter t ~f ~compare_key =
    fold ~init:(Empty, 0) t ~f:(fun ~key ~data (accu, length) ->
      if f data then set ~length ~key ~data accu ~compare_key else accu, length)
  ;;

  let filteri t ~f ~compare_key =
    fold ~init:(Empty, 0) t ~f:(fun ~key ~data (accu, length) ->
      if f ~key ~data then set ~length ~key ~data accu ~compare_key else accu, length)
  ;;

  let filter_map t ~f ~compare_key =
    fold ~init:(Empty, 0) t ~f:(fun ~key ~data (accu, length) ->
      match f data with
      | None -> accu, length
      | Some b -> set ~length ~key ~data:b accu ~compare_key)
  ;;

  let filter_mapi t ~f ~compare_key =
    fold ~init:(Empty, 0) t ~f:(fun ~key ~data (accu, length) ->
      match f ~key ~data with
      | None -> accu, length
      | Some b -> set ~length ~key ~data:b accu ~compare_key)
  ;;

  let partition_mapi t ~f ~compare_key =
    fold
      t
      ~init:((Empty, 0), (Empty, 0))
      ~f:(fun ~key ~data (pair1, pair2) ->
        match (f ~key ~data : _ Either.t) with
        | First x ->
          let t, length = pair1 in
          set t ~key ~data:x ~compare_key ~length, pair2
        | Second y ->
          let t, length = pair2 in
          pair1, set t ~key ~data:y ~compare_key ~length)
  ;;

  let partition_map t ~f ~compare_key =
    partition_mapi t ~compare_key ~f:(fun ~key:_ ~data -> f data)
  ;;

  let partitioni_tf t ~f ~compare_key =
    partition_mapi t ~compare_key ~f:(fun ~key ~data ->
      if f ~key ~data then First data else Second data)
  ;;

  let partition_tf t ~f ~compare_key =
    partition_mapi t ~compare_key ~f:(fun ~key:_ ~data ->
      if f data then First data else Second data)
  ;;

  module Enum = struct
    type increasing
    type decreasing

    type ('k, 'v, 'direction) t =
      | End
      | More of 'k * 'v * ('k, 'v) tree * ('k, 'v, 'direction) t

    let rec cons t (e : (_, _, increasing) t) : (_, _, increasing) t =
      match t with
      | Empty -> e
      | Leaf (v, d) -> More (v, d, Empty, e)
      | Node (l, v, d, r, _) -> cons l (More (v, d, r, e))
    ;;

    let rec cons_right t (e : (_, _, decreasing) t) : (_, _, decreasing) t =
      match t with
      | Empty -> e
      | Leaf (v, d) -> More (v, d, Empty, e)
      | Node (l, v, d, r, _) -> cons_right r (More (v, d, l, e))
    ;;

    let of_tree tree : (_, _, increasing) t = cons tree End
    let of_tree_right tree : (_, _, decreasing) t = cons_right tree End

    let starting_at_increasing t key compare : (_, _, increasing) t =
      let rec loop t e =
        match t with
        | Empty -> e
        | Leaf (v, d) -> loop (Node (Empty, v, d, Empty, 1)) e
        | Node (_, v, _, r, _) when compare v key < 0 -> loop r e
        | Node (l, v, d, r, _) -> loop l (More (v, d, r, e))
      in
      loop t End
    ;;

    let starting_at_decreasing t key compare : (_, _, decreasing) t =
      let rec loop t e =
        match t with
        | Empty -> e
        | Leaf (v, d) -> loop (Node (Empty, v, d, Empty, 1)) e
        | Node (l, v, _, _, _) when compare v key > 0 -> loop l e
        | Node (l, v, d, r, _) -> loop r (More (v, d, l, e))
      in
      loop t End
    ;;

    let compare compare_key compare_data t1 t2 =
      let rec loop t1 t2 =
        match t1, t2 with
        | End, End -> 0
        | End, _ -> -1
        | _, End -> 1
        | More (v1, d1, r1, e1), More (v2, d2, r2, e2) ->
          let c = compare_key v1 v2 in
          if c <> 0
          then c
          else (
            let c = compare_data d1 d2 in
            if c <> 0
            then c
            else if phys_equal r1 r2
            then loop e1 e2
            else loop (cons r1 e1) (cons r2 e2))
      in
      loop t1 t2
    ;;

    let equal compare_key data_equal t1 t2 =
      let rec loop t1 t2 =
        match t1, t2 with
        | End, End -> true
        | End, _ | _, End -> false
        | More (v1, d1, r1, e1), More (v2, d2, r2, e2) ->
          compare_key v1 v2 = 0
          && data_equal d1 d2
          && if phys_equal r1 r2 then loop e1 e2 else loop (cons r1 e1) (cons r2 e2)
      in
      loop t1 t2
    ;;

    let rec fold ~init ~f = function
      | End -> init
      | More (key, data, tree, enum) ->
        let next = f ~key ~data init in
        fold (cons tree enum) ~init:next ~f
    ;;

    let fold2 compare_key t1 t2 ~init ~f =
      let rec loop t1 t2 curr =
        match t1, t2 with
        | End, End -> curr
        | End, _ ->
          fold t2 ~init:curr ~f:(fun ~key ~data acc -> f ~key ~data:(`Right data) acc)
        | _, End ->
          fold t1 ~init:curr ~f:(fun ~key ~data acc -> f ~key ~data:(`Left data) acc)
        | More (k1, v1, tree1, enum1), More (k2, v2, tree2, enum2) ->
          let compare_result = compare_key k1 k2 in
          if compare_result = 0
          then (
            let next = f ~key:k1 ~data:(`Both (v1, v2)) curr in
            loop (cons tree1 enum1) (cons tree2 enum2) next)
          else if compare_result < 0
          then (
            let next = f ~key:k1 ~data:(`Left v1) curr in
            loop (cons tree1 enum1) t2 next)
          else (
            let next = f ~key:k2 ~data:(`Right v2) curr in
            loop t1 (cons tree2 enum2) next)
      in
      loop t1 t2 init
    ;;

    let symmetric_diff t1 t2 ~compare_key ~data_equal =
      let step state =
        match state with
        | End, End -> Sequence.Step.Done
        | End, More (key, data, tree, enum) ->
          Sequence.Step.Yield ((key, `Right data), (End, cons tree enum))
        | More (key, data, tree, enum), End ->
          Sequence.Step.Yield ((key, `Left data), (cons tree enum, End))
        | (More (k1, v1, tree1, enum1) as left), (More (k2, v2, tree2, enum2) as right)
          ->
          let compare_result = compare_key k1 k2 in
          if compare_result = 0
          then (
            let next_state =
              if phys_equal tree1 tree2
              then enum1, enum2
              else cons tree1 enum1, cons tree2 enum2
            in
            if data_equal v1 v2
            then Sequence.Step.Skip next_state
            else Sequence.Step.Yield ((k1, `Unequal (v1, v2)), next_state))
          else if compare_result < 0
          then Sequence.Step.Yield ((k1, `Left v1), (cons tree1 enum1, right))
          else Sequence.Step.Yield ((k2, `Right v2), (left, cons tree2 enum2))
      in
      Sequence.unfold_step ~init:(of_tree t1, of_tree t2) ~f:step
    ;;

    let fold_symmetric_diff t1 t2 ~compare_key ~data_equal ~init ~f =
      let add acc k v = f acc (k, `Right v) in
      let remove acc k v = f acc (k, `Left v) in
      let rec loop left right acc =
        match left, right with
        | End, enum -> fold enum ~init:acc ~f:(fun ~key ~data acc -> add acc key data)
        | enum, End -> fold enum ~init:acc ~f:(fun ~key ~data acc -> remove acc key data)
        | (More (k1, v1, tree1, enum1) as left), (More (k2, v2, tree2, enum2) as right)
          ->
          let compare_result = compare_key k1 k2 in
          if compare_result = 0
          then (
            let acc = if data_equal v1 v2 then acc else f acc (k1, `Unequal (v1, v2)) in
            if phys_equal tree1 tree2
            then loop enum1 enum2 acc
            else loop (cons tree1 enum1) (cons tree2 enum2) acc)
          else if compare_result < 0
          then (
            let acc = remove acc k1 v1 in
            loop (cons tree1 enum1) right acc)
          else (
            let acc = add acc k2 v2 in
            loop left (cons tree2 enum2) acc)
      in
      loop (of_tree t1) (of_tree t2) init
    ;;
  end

  let to_sequence_increasing comparator ~from_key t =
    let next enum =
      match enum with
      | Enum.End -> Sequence.Step.Done
      | Enum.More (k, v, t, e) -> Sequence.Step.Yield ((k, v), Enum.cons t e)
    in
    let init =
      match from_key with
      | None -> Enum.of_tree t
      | Some key -> Enum.starting_at_increasing t key comparator.Comparator.compare
    in
    Sequence.unfold_step ~init ~f:next
  ;;

  let to_sequence_decreasing comparator ~from_key t =
    let next enum =
      match enum with
      | Enum.End -> Sequence.Step.Done
      | Enum.More (k, v, t, e) -> Sequence.Step.Yield ((k, v), Enum.cons_right t e)
    in
    let init =
      match from_key with
      | None -> Enum.of_tree_right t
      | Some key -> Enum.starting_at_decreasing t key comparator.Comparator.compare
    in
    Sequence.unfold_step ~init ~f:next
  ;;

  let to_sequence
        comparator
        ?(order = `Increasing_key)
        ?keys_greater_or_equal_to
        ?keys_less_or_equal_to
        t
    =
    let inclusive_bound side t bound =
      let compare_key = comparator.Comparator.compare in
      let l, maybe, r = split t bound ~compare_key in
      let t = side (l, r) in
      match maybe with
      | None -> t
      | Some (key, data) -> set' t key data ~compare_key
    in
    match order with
    | `Increasing_key ->
      let t = Option.fold keys_less_or_equal_to ~init:t ~f:(inclusive_bound fst) in
      to_sequence_increasing comparator ~from_key:keys_greater_or_equal_to t
    | `Decreasing_key ->
      let t = Option.fold keys_greater_or_equal_to ~init:t ~f:(inclusive_bound snd) in
      to_sequence_decreasing comparator ~from_key:keys_less_or_equal_to t
  ;;

  let compare compare_key compare_data t1 t2 =
    Enum.compare compare_key compare_data (Enum.of_tree t1) (Enum.of_tree t2)
  ;;

  let equal compare_key compare_data t1 t2 =
    Enum.equal compare_key compare_data (Enum.of_tree t1) (Enum.of_tree t2)
  ;;

  let iter2 t1 t2 ~f ~compare_key =
    Enum.fold2
      compare_key
      (Enum.of_tree t1)
      (Enum.of_tree t2)
      ~init:()
      ~f:(fun ~key ~data () -> f ~key ~data)
  ;;

  let fold2 t1 t2 ~init ~f ~compare_key =
    Enum.fold2 compare_key (Enum.of_tree t1) (Enum.of_tree t2) ~f ~init
  ;;

  let symmetric_diff = Enum.symmetric_diff

  let fold_symmetric_diff t1 t2 ~compare_key ~data_equal ~init ~f =
    (* [Enum.fold_diffs] is a correct implementation of this function, but is considerably
       slower, as we have to allocate quite a lot of state to track enumeration of a tree.
       Avoid if we can.
    *)
    let slow x y ~init =
      Enum.fold_symmetric_diff x y ~compare_key ~data_equal ~f ~init
    in
    let add acc k v = f acc (k, `Right v) in
    let remove acc k v = f acc (k, `Left v) in
    let delta acc k v v' =
      if data_equal v v' then acc else f acc (k, `Unequal (v, v'))
    in
    (* If two trees have the same structure at the root (and the same key, if they're
       [Node]s) we can trivially diff each subpart in obvious ways. *)
    let rec loop t t' acc =
      if phys_equal t t'
      then acc
      else (
        match t, t' with
        | Empty, new_vals ->
          fold new_vals ~init:acc ~f:(fun ~key ~data acc -> add acc key data)
        | old_vals, Empty ->
          fold old_vals ~init:acc ~f:(fun ~key ~data acc -> remove acc key data)
        | Leaf (k, v), Leaf (k', v') ->
          (match compare_key k k' with
           | x when x = 0 -> delta acc k v v'
           | x when x < 0 ->
             let acc = remove acc k v in
             add acc k' v'
           | _ (* when x > 0 *) ->
             let acc = add acc k' v' in
             remove acc k v)
        | Node (l, k, v, r, _), Node (l', k', v', r', _) when compare_key k k' = 0 ->
          let acc = loop l l' acc in
          let acc = delta acc k v v' in
          loop r r' acc
        (* Our roots aren't the same key. Fallback to the slow mode. Trees with small
           diffs will only do this on very small parts of the tree (hopefully - if the
           overall root is rebalanced, we'll eat the whole cost, unfortunately.) *)
        | Node _, Node _ | Node _, Leaf _ | Leaf _, Node _ -> slow t t' ~init:acc)
    in
    loop t1 t2 init
  ;;

  let rec length = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node (l, _, _, r, _) -> length l + length r + 1
  ;;

  let hash_fold_t_ignoring_structure hash_fold_key hash_fold_data state t =
    fold
      t
      ~init:(hash_fold_int state (length t))
      ~f:(fun ~key ~data state -> hash_fold_data (hash_fold_key state key) data)
  ;;

  let keys t = fold_right ~f:(fun ~key ~data:_ list -> key :: list) t ~init:[]
  let data t = fold_right ~f:(fun ~key:_ ~data list -> data :: list) t ~init:[]

  module type Foldable = sig
    val name : string

    type 'a t

    val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  end

  module Of_foldable (M : Foldable) = struct
    let of_foldable_fold foldable ~init ~f ~compare_key =
      M.fold foldable ~init:(empty, 0) ~f:(fun (accum, length) (key, data) ->
        let prev_data =
          match find accum key ~compare_key with
          | None -> init
          | Some prev -> prev
        in
        let data = f prev_data data in
        set accum ~length ~key ~data ~compare_key)
    ;;

    let of_foldable_reduce foldable ~f ~compare_key =
      M.fold foldable ~init:(empty, 0) ~f:(fun (accum, length) (key, data) ->
        let new_data =
          match find accum key ~compare_key with
          | None -> data
          | Some prev -> f prev data
        in
        set accum ~length ~key ~data:new_data ~compare_key)
    ;;

    let of_foldable foldable ~compare_key =
      with_return (fun r ->
        let map =
          M.fold foldable ~init:(empty, 0) ~f:(fun (t, length) (key, data) ->
            let ((_, length') as acc) = set ~length ~key ~data t ~compare_key in
            if length = length' then r.return (`Duplicate_key key) else acc)
        in
        `Ok map)
    ;;

    let of_foldable_or_error foldable ~comparator =
      match of_foldable foldable ~compare_key:comparator.Comparator.compare with
      | `Ok x -> Result.Ok x
      | `Duplicate_key key ->
        Or_error.error
          ("Map.of_" ^ M.name ^ "_or_error: duplicate key")
          key
          comparator.sexp_of_t
    ;;

    let of_foldable_exn foldable ~comparator =
      match of_foldable foldable ~compare_key:comparator.Comparator.compare with
      | `Ok x -> x
      | `Duplicate_key key ->
        Error.create
          ("Map.of_" ^ M.name ^ "_exn: duplicate key")
          key
          comparator.sexp_of_t
        |> Error.raise
    ;;
  end

  module Of_alist = Of_foldable (struct
      let name = "alist"

      type 'a t = 'a list

      let fold = List.fold
    end)

  let of_alist_fold = Of_alist.of_foldable_fold
  let of_alist_reduce = Of_alist.of_foldable_reduce
  let of_alist = Of_alist.of_foldable
  let of_alist_or_error = Of_alist.of_foldable_or_error
  let of_alist_exn = Of_alist.of_foldable_exn

  (* Reverse the input, then fold from left to right. The resulting map uses the first
     instance of each key from the input list. The relative ordering of elements in each
     output list is the same as in the input list. *)
  let of_foldable_multi foldable ~fold ~compare_key =
    let alist = fold foldable ~init:[] ~f:(fun l x -> x :: l) in
    of_alist_fold alist ~init:[] ~f:(fun l x -> x :: l) ~compare_key
  ;;

  let of_alist_multi alist ~compare_key =
    of_foldable_multi alist ~fold:List.fold ~compare_key
  ;;

  module Of_sequence = Of_foldable (struct
      let name = "sequence"

      type 'a t = 'a Sequence.t

      let fold = Sequence.fold
    end)

  let of_sequence_fold = Of_sequence.of_foldable_fold
  let of_sequence_reduce = Of_sequence.of_foldable_reduce
  let of_sequence = Of_sequence.of_foldable
  let of_sequence_or_error = Of_sequence.of_foldable_or_error
  let of_sequence_exn = Of_sequence.of_foldable_exn

  let of_sequence_multi sequence ~compare_key =
    of_foldable_multi sequence ~fold:Sequence.fold ~compare_key
  ;;

  let for_all t ~f =
    with_return (fun r ->
      iter t ~f:(fun data -> if not (f data) then r.return false);
      true)
  ;;

  let for_alli t ~f =
    with_return (fun r ->
      iteri t ~f:(fun ~key ~data -> if not (f ~key ~data) then r.return false);
      true)
  ;;

  let exists t ~f =
    with_return (fun r ->
      iter t ~f:(fun data -> if f data then r.return true);
      false)
  ;;

  let existsi t ~f =
    with_return (fun r ->
      iteri t ~f:(fun ~key ~data -> if f ~key ~data then r.return true);
      false)
  ;;

  let count t ~f =
    fold t ~init:0 ~f:(fun ~key:_ ~data acc -> if f data then acc + 1 else acc)
  ;;

  let counti t ~f =
    fold t ~init:0 ~f:(fun ~key ~data acc -> if f ~key ~data then acc + 1 else acc)
  ;;

  let to_alist ?(key_order = `Increasing) t =
    match key_order with
    | `Increasing -> fold_right t ~init:[] ~f:(fun ~key ~data x -> (key, data) :: x)
    | `Decreasing -> fold t ~init:[] ~f:(fun ~key ~data x -> (key, data) :: x)
  ;;

  let merge t1 t2 ~f ~compare_key =
    let elts = Uniform_array.unsafe_create_uninitialized ~len:(length t1 + length t2) in
    let i = ref 0 in
    iter2 t1 t2 ~compare_key ~f:(fun ~key ~data:values ->
      match f ~key values with
      | Some value ->
        Uniform_array.set elts !i (key, value);
        incr i
      | None -> ());
    let len = !i in
    let get i = Uniform_array.get elts i in
    let tree = of_increasing_iterator_unchecked ~len ~f:get in
    tree, len
  ;;

  module Closest_key_impl = struct
    (* [marker] and [repackage] allow us to create "logical" options without actually
       allocating any options. Passing [Found key value] to a function is equivalent to
       passing [Some (key, value)]; passing [Missing () ()] is equivalent to passing
       [None]. *)
    type ('k, 'v, 'k_opt, 'v_opt) marker =
      | Missing : ('k, 'v, unit, unit) marker
      | Found : ('k, 'v, 'k, 'v) marker

    let repackage
          (type k v k_opt v_opt)
          (marker : (k, v, k_opt, v_opt) marker)
          (k : k_opt)
          (v : v_opt)
      : (k * v) option
      =
      match marker with
      | Missing -> None
      | Found -> Some (k, v)
    ;;

    (* The type signature is explicit here to allow polymorphic recursion. *)
    let rec loop :
      'k 'v 'k_opt 'v_opt. ('k, 'v) tree
      -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
      -> 'k -> compare_key:('k -> 'k -> int) -> ('k, 'v, 'k_opt, 'v_opt) marker
      -> 'k_opt -> 'v_opt -> ('k * 'v) option
      =
      fun t dir k ~compare_key found_marker found_key found_value ->
        match t with
        | Empty -> repackage found_marker found_key found_value
        | Leaf (k', v') ->
          let c = compare_key k' k in
          if match dir with
            | `Greater_or_equal_to -> c >= 0
            | `Greater_than -> c > 0
            | `Less_or_equal_to -> c <= 0
            | `Less_than -> c < 0
          then Some (k', v')
          else repackage found_marker found_key found_value
        | Node (l, k', v', r, _) ->
          let c = compare_key k' k in
          if c = 0
          then (
            (* This is a base case (no recursive call). *)
            match dir with
            | `Greater_or_equal_to | `Less_or_equal_to -> Some (k', v')
            | `Greater_than ->
              if is_empty r
              then repackage found_marker found_key found_value
              else min_elt r
            | `Less_than ->
              if is_empty l
              then repackage found_marker found_key found_value
              else max_elt l)
          else (
            (* We are guaranteed here that k' <> k. *)
            (* This is the only recursive case. *)
            match dir with
            | `Greater_or_equal_to | `Greater_than ->
              if c > 0
              then loop l dir k ~compare_key Found k' v'
              else loop r dir k ~compare_key found_marker found_key found_value
            | `Less_or_equal_to | `Less_than ->
              if c < 0
              then loop r dir k ~compare_key Found k' v'
              else loop l dir k ~compare_key found_marker found_key found_value)
    ;;

    let closest_key t dir k ~compare_key = loop t dir k ~compare_key Missing () ()
  end

  let closest_key = Closest_key_impl.closest_key

  let rec rank t k ~compare_key =
    match t with
    | Empty -> None
    | Leaf (k', _) -> if compare_key k' k = 0 then Some 0 else None
    | Node (l, k', _, r, _) ->
      let c = compare_key k' k in
      if c = 0
      then Some (length l)
      else if c > 0
      then rank l k ~compare_key
      else Option.map (rank r k ~compare_key) ~f:(fun rank -> rank + 1 + length l)
  ;;

  (* this could be implemented using [Sequence] interface but the following implementation
     allocates only 2 words and doesn't require write-barrier *)
  let rec nth' num_to_search = function
    | Empty -> None
    | Leaf (k, v) ->
      if !num_to_search = 0
      then Some (k, v)
      else (
        decr num_to_search;
        None)
    | Node (l, k, v, r, _) ->
      (match nth' num_to_search l with
       | Some _ as some -> some
       | None ->
         if !num_to_search = 0
         then Some (k, v)
         else (
           decr num_to_search;
           nth' num_to_search r))
  ;;

  let nth t n = nth' (ref n) t


  let rec find_first_satisfying t ~f =
    match t with
    | Empty -> None
    | Leaf (k, v) -> if f ~key:k ~data:v then Some (k, v) else None
    | Node (l, k, v, r, _) ->
      if f ~key:k ~data:v
      then (
        match find_first_satisfying l ~f with
        | None -> Some (k, v)
        | Some _ as x -> x)
      else find_first_satisfying r ~f
  ;;

  let rec find_last_satisfying t ~f =
    match t with
    | Empty -> None
    | Leaf (k, v) -> if f ~key:k ~data:v then Some (k, v) else None
    | Node (l, k, v, r, _) ->
      if f ~key:k ~data:v
      then (
        match find_last_satisfying r ~f with
        | None -> Some (k, v)
        | Some _ as x -> x)
      else find_last_satisfying l ~f
  ;;

  let binary_search t ~compare how v =
    match how with
    | `Last_strictly_less_than ->
      find_last_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v < 0)
    | `Last_less_than_or_equal_to ->
      find_last_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v <= 0)
    | `First_equal_to ->
      (match
         find_first_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v >= 0)
       with
       | Some (key, data) as pair when compare ~key ~data v = 0 -> pair
       | None | Some _ -> None)
    | `Last_equal_to ->
      (match find_last_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v <= 0) with
       | Some (key, data) as pair when compare ~key ~data v = 0 -> pair
       | None | Some _ -> None)
    | `First_greater_than_or_equal_to ->
      find_first_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v >= 0)
    | `First_strictly_greater_than ->
      find_first_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v > 0)
  ;;

  let binary_search_segmented t ~segment_of how =
    let is_left ~key ~data =
      match segment_of ~key ~data with
      | `Left -> true
      | `Right -> false
    in
    let is_right ~key ~data = not (is_left ~key ~data) in
    match how with
    | `Last_on_left -> find_last_satisfying t ~f:is_left
    | `First_on_right -> find_first_satisfying t ~f:is_right
  ;;

  type ('k, 'v) acc =
    { mutable bad_key : 'k option
    ; mutable map_length : ('k, 'v) t * int
    }

  let of_iteri ~iteri ~compare_key =
    let acc = { bad_key = None; map_length = empty, 0 } in
    iteri ~f:(fun ~key ~data ->
      let map, length = acc.map_length in
      let ((_, length') as pair) = set ~length ~key ~data map ~compare_key in
      if length = length' && Option.is_none acc.bad_key
      then acc.bad_key <- Some key
      else acc.map_length <- pair);
    match acc.bad_key with
    | None -> `Ok acc.map_length
    | Some key -> `Duplicate_key key
  ;;

  let t_of_sexp_direct key_of_sexp value_of_sexp sexp ~(comparator : _ Comparator.t) =
    let alist = list_of_sexp (pair_of_sexp key_of_sexp value_of_sexp) sexp in
    let compare_key = comparator.compare in
    match of_alist alist ~compare_key with
    | `Ok v -> v
    | `Duplicate_key k ->
      (* find the sexp of a duplicate key, so the error is narrowed to a key and not
         the whole map *)
      let alist_sexps = list_of_sexp (pair_of_sexp Fn.id Fn.id) sexp in
      let found_first_k = ref false in
      List.iter2_ok alist alist_sexps ~f:(fun (k2, _) (k2_sexp, _) ->
        if compare_key k k2 = 0
        then
          if !found_first_k
          then of_sexp_error "Map.t_of_sexp_direct: duplicate key" k2_sexp
          else found_first_k := true);
      assert false
  ;;

  let sexp_of_t sexp_of_key sexp_of_value t =
    let f ~key ~data acc = Sexp.List [ sexp_of_key key; sexp_of_value data ] :: acc in
    Sexp.List (fold_right ~f t ~init:[])
  ;;

  let combine_errors t ~compare_key ~sexp_of_key =
    let oks, (error_tree, _) = partition_map t ~compare_key ~f:Result.to_either in
    if is_empty error_tree
    then Ok oks
    else Or_error.error_s (sexp_of_t sexp_of_key Error.sexp_of_t error_tree)
  ;;
end

type ('k, 'v, 'comparator) t =
  { (* [comparator] is the first field so that polymorphic equality fails on a map due
       to the functional value in the comparator.
       Note that this does not affect polymorphic [compare]: that still produces
       nonsense. *)
    comparator : ('k, 'comparator) Comparator.t
  ; tree : ('k, 'v) Tree0.t
  ; length : int
  }

type ('k, 'v, 'comparator) tree = ('k, 'v) Tree0.t

let compare_key t = t.comparator.Comparator.compare


let like { tree = _; length = _; comparator } (tree, length) =
  { tree; length; comparator }
;;

let like2 x (y, z) = like x y, like x z
let with_same_length { tree = _; comparator; length } tree = { tree; comparator; length }
let of_tree ~comparator tree = { tree; comparator; length = Tree0.length tree }

(* Exposing this function would make it very easy for the invariants
   of this module to be broken. *)
let of_tree_unsafe ~comparator ~length tree = { tree; comparator; length }

module Accessors = struct
  let comparator t = t.comparator
  let to_tree t = t.tree
  let invariants t = Tree0.invariants t.tree ~compare_key:(compare_key t)
  let is_empty t = Tree0.is_empty t.tree
  let length t = t.length

  let set t ~key ~data =
    like t (Tree0.set t.tree ~length:t.length ~key ~data ~compare_key:(compare_key t))
  ;;

  let add_exn t ~key ~data =
    like
      t
      (Tree0.add_exn
         t.tree
         ~length:t.length
         ~key
         ~data
         ~compare_key:(compare_key t)
         ~sexp_of_key:t.comparator.sexp_of_t)
  ;;

  let add_exn_internal t ~key ~data =
    like
      t
      (Tree0.add_exn_internal
         t.tree
         ~length:t.length
         ~key
         ~data
         ~compare_key:(compare_key t)
         ~sexp_of_key:t.comparator.sexp_of_t)
  ;;

  let add t ~key ~data =
    match add_exn_internal t ~key ~data with
    | result -> `Ok result
    | exception Duplicate -> `Duplicate
  ;;

  let add_multi t ~key ~data =
    like
      t
      (Tree0.add_multi t.tree ~length:t.length ~key ~data ~compare_key:(compare_key t))
  ;;

  let remove_multi t key =
    like t (Tree0.remove_multi t.tree ~length:t.length key ~compare_key:(compare_key t))
  ;;

  let find_multi t key = Tree0.find_multi t.tree key ~compare_key:(compare_key t)

  let change t key ~f =
    like t (Tree0.change t.tree key ~f ~length:t.length ~compare_key:(compare_key t))
  ;;

  let update t key ~f =
    like t (Tree0.update t.tree key ~f ~length:t.length ~compare_key:(compare_key t))
  ;;

  let find_exn t key =
    Tree0.find_exn
      t.tree
      key
      ~compare_key:(compare_key t)
      ~sexp_of_key:t.comparator.sexp_of_t
  ;;

  let find t key = Tree0.find t.tree key ~compare_key:(compare_key t)

  let remove t key =
    like t (Tree0.remove t.tree key ~length:t.length ~compare_key:(compare_key t))
  ;;

  let mem t key = Tree0.mem t.tree key ~compare_key:(compare_key t)
  let iter_keys t ~f = Tree0.iter_keys t.tree ~f
  let iter t ~f = Tree0.iter t.tree ~f
  let iteri t ~f = Tree0.iteri t.tree ~f
  let iteri_until t ~f = Tree0.iteri_until t.tree ~f
  let iter2 t1 t2 ~f = Tree0.iter2 t1.tree t2.tree ~f ~compare_key:(compare_key t1)
  let map t ~f = with_same_length t (Tree0.map t.tree ~f)
  let mapi t ~f = with_same_length t (Tree0.mapi t.tree ~f)
  let fold t ~init ~f = Tree0.fold t.tree ~f ~init
  let fold_right t ~init ~f = Tree0.fold_right t.tree ~f ~init

  let fold2 t1 t2 ~init ~f =
    Tree0.fold2 t1.tree t2.tree ~init ~f ~compare_key:(compare_key t1)
  ;;

  let filter_keys t ~f =
    like t (Tree0.filter_keys t.tree ~f ~compare_key:(compare_key t))
  ;;

  let filter t ~f = like t (Tree0.filter t.tree ~f ~compare_key:(compare_key t))
  let filteri t ~f = like t (Tree0.filteri t.tree ~f ~compare_key:(compare_key t))
  let filter_map t ~f = like t (Tree0.filter_map t.tree ~f ~compare_key:(compare_key t))

  let filter_mapi t ~f =
    like t (Tree0.filter_mapi t.tree ~f ~compare_key:(compare_key t))
  ;;

  let partition_mapi t ~f =
    like2 t (Tree0.partition_mapi t.tree ~f ~compare_key:(compare_key t))
  ;;

  let partition_map t ~f =
    like2 t (Tree0.partition_map t.tree ~f ~compare_key:(compare_key t))
  ;;

  let partitioni_tf t ~f =
    like2 t (Tree0.partitioni_tf t.tree ~f ~compare_key:(compare_key t))
  ;;

  let partition_tf t ~f =
    like2 t (Tree0.partition_tf t.tree ~f ~compare_key:(compare_key t))
  ;;

  let combine_errors t =
    Or_error.map
      ~f:(like t)
      (Tree0.combine_errors
         t.tree
         ~compare_key:(compare_key t)
         ~sexp_of_key:t.comparator.sexp_of_t)
  ;;

  let compare_direct compare_data t1 t2 =
    Tree0.compare (compare_key t1) compare_data t1.tree t2.tree
  ;;

  let equal compare_data t1 t2 =
    Tree0.equal (compare_key t1) compare_data t1.tree t2.tree
  ;;

  let keys t = Tree0.keys t.tree
  let data t = Tree0.data t.tree
  let to_alist ?key_order t = Tree0.to_alist ?key_order t.tree
  let validate ~name f t = Validate.alist ~name f (to_alist t)
  let validatei ~name f t = Validate.list ~name:(Fn.compose name fst) f (to_alist t)

  let symmetric_diff t1 t2 ~data_equal =
    Tree0.symmetric_diff t1.tree t2.tree ~compare_key:(compare_key t1) ~data_equal
  ;;

  let fold_symmetric_diff t1 t2 ~data_equal ~init ~f =
    Tree0.fold_symmetric_diff
      t1.tree
      t2.tree
      ~compare_key:(compare_key t1)
      ~data_equal
      ~init
      ~f
  ;;

  let merge t1 t2 ~f =
    like t1 (Tree0.merge t1.tree t2.tree ~f ~compare_key:(compare_key t1))
  ;;

  let min_elt t = Tree0.min_elt t.tree
  let min_elt_exn t = Tree0.min_elt_exn t.tree
  let max_elt t = Tree0.max_elt t.tree
  let max_elt_exn t = Tree0.max_elt_exn t.tree
  let for_all t ~f = Tree0.for_all t.tree ~f
  let for_alli t ~f = Tree0.for_alli t.tree ~f
  let exists t ~f = Tree0.exists t.tree ~f
  let existsi t ~f = Tree0.existsi t.tree ~f
  let count t ~f = Tree0.count t.tree ~f
  let counti t ~f = Tree0.counti t.tree ~f

  let split t k =
    let l, maybe, r = Tree0.split t.tree k ~compare_key:(compare_key t) in
    let comparator = comparator t in
    (* Try to traverse the least amount possible to calculate the length,
       using height as a heuristic. *)
    let both_len = if Option.is_some maybe then t.length - 1 else t.length in
    if Tree0.height l < Tree0.height r
    then (
      let l = of_tree l ~comparator in
      l, maybe, of_tree_unsafe r ~comparator ~length:(both_len - length l))
    else (
      let r = of_tree r ~comparator in
      of_tree_unsafe l ~comparator ~length:(both_len - length r), maybe, r)
  ;;

  let subrange t ~lower_bound ~upper_bound =
    let left, mid, right =
      Tree0.split_range t.tree ~lower_bound ~upper_bound ~compare_key:(compare_key t)
    in
    (* Try to traverse the least amount possible to calculate the length,
       using height as a heuristic. *)
    let outer_joined_height =
      let h_l = Tree0.height left
      and h_r = Tree0.height right in
      if h_l = h_r then h_l + 1 else max h_l h_r
    in
    if outer_joined_height < Tree0.height mid
    then (
      let mid_length = t.length - (Tree0.length left + Tree0.length right) in
      of_tree_unsafe mid ~comparator:(comparator t) ~length:mid_length)
    else of_tree mid ~comparator:(comparator t)
  ;;

  let append ~lower_part ~upper_part =
    match
      Tree0.append
        ~compare_key:(compare_key lower_part)
        ~lower_part:lower_part.tree
        ~upper_part:upper_part.tree
    with
    | `Ok tree ->
      `Ok
        (of_tree_unsafe
           tree
           ~comparator:(comparator lower_part)
           ~length:(lower_part.length + upper_part.length))
    | `Overlapping_key_ranges -> `Overlapping_key_ranges
  ;;

  let fold_range_inclusive t ~min ~max ~init ~f =
    Tree0.fold_range_inclusive t.tree ~min ~max ~init ~f ~compare_key:(compare_key t)
  ;;

  let range_to_alist t ~min ~max =
    Tree0.range_to_alist t.tree ~min ~max ~compare_key:(compare_key t)
  ;;

  let closest_key t dir key =
    Tree0.closest_key t.tree dir key ~compare_key:(compare_key t)
  ;;

  let nth t n = Tree0.nth t.tree n
  let nth_exn t n = Option.value_exn (nth t n)
  let rank t key = Tree0.rank t.tree key ~compare_key:(compare_key t)
  let sexp_of_t sexp_of_k sexp_of_v _ t = Tree0.sexp_of_t sexp_of_k sexp_of_v t.tree

  let to_sequence ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t =
    Tree0.to_sequence
      t.comparator
      ?order
      ?keys_greater_or_equal_to
      ?keys_less_or_equal_to
      t.tree
  ;;

  let binary_search t ~compare how v = Tree0.binary_search t.tree ~compare how v

  let binary_search_segmented t ~segment_of how =
    Tree0.binary_search_segmented t.tree ~segment_of how
  ;;

  let hash_fold_direct hash_fold_key hash_fold_data state t =
    Tree0.hash_fold_t_ignoring_structure hash_fold_key hash_fold_data state t.tree
  ;;
end

(* [0] is used as the [length] argument everywhere in this module, since trees do not
   have their lengths stored at the root, unlike maps. The values are discarded always. *)
module Tree = struct
  type ('k, 'v, 'comparator) t = ('k, 'v, 'comparator) tree

  let empty_without_value_restriction = Tree0.empty
  let empty ~comparator:_ = empty_without_value_restriction
  let of_tree ~comparator:_ tree = tree
  let singleton ~comparator:_ k v = Tree0.singleton k v

  let of_sorted_array_unchecked ~comparator array =
    fst
      (Tree0.of_sorted_array_unchecked array ~compare_key:comparator.Comparator.compare)
  ;;

  let of_sorted_array ~comparator array =
    Tree0.of_sorted_array array ~compare_key:comparator.Comparator.compare
    |> Or_error.map ~f:fst
  ;;

  let of_alist ~comparator alist =
    match Tree0.of_alist alist ~compare_key:comparator.Comparator.compare with
    | `Duplicate_key _ as d -> d
    | `Ok (tree, _size) -> `Ok tree
  ;;

  let of_alist_or_error ~comparator alist =
    Tree0.of_alist_or_error alist ~comparator |> Or_error.map ~f:fst
  ;;

  let of_alist_exn ~comparator alist = fst (Tree0.of_alist_exn alist ~comparator)

  let of_alist_multi ~comparator alist =
    fst (Tree0.of_alist_multi alist ~compare_key:comparator.Comparator.compare)
  ;;

  let of_alist_fold ~comparator alist ~init ~f =
    fst (Tree0.of_alist_fold alist ~init ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let of_alist_reduce ~comparator alist ~f =
    fst (Tree0.of_alist_reduce alist ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let of_iteri ~comparator ~iteri =
    match Tree0.of_iteri ~iteri ~compare_key:comparator.Comparator.compare with
    | `Ok (tree, _size) -> `Ok tree
    | `Duplicate_key _ as d -> d
  ;;

  let of_increasing_iterator_unchecked ~comparator:_required_by_intf ~len ~f =
    Tree0.of_increasing_iterator_unchecked ~len ~f
  ;;

  let of_increasing_sequence ~comparator seq =
    Or_error.map
      ~f:fst
      (Tree0.of_increasing_sequence seq ~compare_key:comparator.Comparator.compare)
  ;;

  let of_sequence ~comparator seq =
    match Tree0.of_sequence seq ~compare_key:comparator.Comparator.compare with
    | `Duplicate_key _ as d -> d
    | `Ok (tree, _size) -> `Ok tree
  ;;

  let of_sequence_or_error ~comparator seq =
    Tree0.of_sequence_or_error seq ~comparator |> Or_error.map ~f:fst
  ;;

  let of_sequence_exn ~comparator seq = fst (Tree0.of_sequence_exn seq ~comparator)

  let of_sequence_multi ~comparator seq =
    fst (Tree0.of_sequence_multi seq ~compare_key:comparator.Comparator.compare)
  ;;

  let of_sequence_fold ~comparator seq ~init ~f =
    fst (Tree0.of_sequence_fold seq ~init ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let of_sequence_reduce ~comparator seq ~f =
    fst (Tree0.of_sequence_reduce seq ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let to_tree t = t

  let invariants ~comparator t =
    Tree0.invariants t ~compare_key:comparator.Comparator.compare
  ;;

  let is_empty t = Tree0.is_empty t
  let length t = Tree0.length t

  let set ~comparator t ~key ~data =
    fst (Tree0.set t ~key ~data ~length:0 ~compare_key:comparator.Comparator.compare)
  ;;

  let add_exn ~comparator t ~key ~data =
    fst
      (Tree0.add_exn
         t
         ~key
         ~data
         ~length:0
         ~compare_key:comparator.Comparator.compare
         ~sexp_of_key:comparator.sexp_of_t)
  ;;

  let add ~comparator t ~key ~data =
    try `Ok (add_exn t ~comparator ~key ~data) with
    | _ -> `Duplicate
  ;;

  let add_multi ~comparator t ~key ~data =
    Tree0.add_multi t ~key ~data ~length:0 ~compare_key:comparator.Comparator.compare
    |> fst
  ;;

  let remove_multi ~comparator t key =
    Tree0.remove_multi t key ~length:0 ~compare_key:comparator.Comparator.compare |> fst
  ;;

  let find_multi ~comparator t key =
    Tree0.find_multi t key ~compare_key:comparator.Comparator.compare
  ;;

  let change ~comparator t key ~f =
    fst (Tree0.change t key ~f ~length:0 ~compare_key:comparator.Comparator.compare)
  ;;

  let update ~comparator t key ~f =
    change ~comparator t key ~f:(fun data -> Some (f data))
  ;;

  let find_exn ~comparator t key =
    Tree0.find_exn
      t
      key
      ~compare_key:comparator.Comparator.compare
      ~sexp_of_key:comparator.Comparator.sexp_of_t
  ;;

  let find ~comparator t key =
    Tree0.find t key ~compare_key:comparator.Comparator.compare
  ;;

  let remove ~comparator t key =
    fst (Tree0.remove t key ~length:0 ~compare_key:comparator.Comparator.compare)
  ;;

  let mem ~comparator t key = Tree0.mem t key ~compare_key:comparator.Comparator.compare
  let iter_keys t ~f = Tree0.iter_keys t ~f
  let iter t ~f = Tree0.iter t ~f
  let iteri t ~f = Tree0.iteri t ~f
  let iteri_until t ~f = Tree0.iteri_until t ~f

  let iter2 ~comparator t1 t2 ~f =
    Tree0.iter2 t1 t2 ~f ~compare_key:comparator.Comparator.compare
  ;;

  let map t ~f = Tree0.map t ~f
  let mapi t ~f = Tree0.mapi t ~f
  let fold t ~init ~f = Tree0.fold t ~f ~init
  let fold_right t ~init ~f = Tree0.fold_right t ~f ~init

  let fold2 ~comparator t1 t2 ~init ~f =
    Tree0.fold2 t1 t2 ~init ~f ~compare_key:comparator.Comparator.compare
  ;;

  let filter_keys ~comparator t ~f =
    fst (Tree0.filter_keys t ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let filter ~comparator t ~f =
    fst (Tree0.filter t ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let filteri ~comparator t ~f =
    fst (Tree0.filteri t ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let filter_map ~comparator t ~f =
    fst (Tree0.filter_map t ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let filter_mapi ~comparator t ~f =
    fst (Tree0.filter_mapi t ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let partition_mapi ~comparator t ~f =
    let (a, _), (b, _) =
      Tree0.partition_mapi t ~f ~compare_key:comparator.Comparator.compare
    in
    a, b
  ;;

  let partition_map ~comparator t ~f =
    let (a, _), (b, _) =
      Tree0.partition_map t ~f ~compare_key:comparator.Comparator.compare
    in
    a, b
  ;;

  let partitioni_tf ~comparator t ~f =
    let (a, _), (b, _) =
      Tree0.partitioni_tf t ~f ~compare_key:comparator.Comparator.compare
    in
    a, b
  ;;

  let partition_tf ~comparator t ~f =
    let (a, _), (b, _) =
      Tree0.partition_tf t ~f ~compare_key:comparator.Comparator.compare
    in
    a, b
  ;;

  let combine_errors ~comparator t =
    Or_error.map
      ~f:fst
      (Tree0.combine_errors
         t
         ~compare_key:comparator.Comparator.compare
         ~sexp_of_key:comparator.Comparator.sexp_of_t)
  ;;

  let compare_direct ~comparator compare_data t1 t2 =
    Tree0.compare comparator.Comparator.compare compare_data t1 t2
  ;;

  let equal ~comparator compare_data t1 t2 =
    Tree0.equal comparator.Comparator.compare compare_data t1 t2
  ;;

  let keys t = Tree0.keys t
  let data t = Tree0.data t
  let to_alist ?key_order t = Tree0.to_alist ?key_order t
  let validate ~name f t = Validate.alist ~name f (to_alist t)
  let validatei ~name f t = Validate.list ~name:(Fn.compose name fst) f (to_alist t)

  let symmetric_diff ~comparator t1 t2 ~data_equal =
    Tree0.symmetric_diff t1 t2 ~compare_key:comparator.Comparator.compare ~data_equal
  ;;

  let fold_symmetric_diff ~comparator t1 t2 ~data_equal ~init ~f =
    Tree0.fold_symmetric_diff
      t1
      t2
      ~compare_key:comparator.Comparator.compare
      ~data_equal
      ~init
      ~f
  ;;

  let merge ~comparator t1 t2 ~f =
    fst (Tree0.merge t1 t2 ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let min_elt t = Tree0.min_elt t
  let min_elt_exn t = Tree0.min_elt_exn t
  let max_elt t = Tree0.max_elt t
  let max_elt_exn t = Tree0.max_elt_exn t
  let for_all t ~f = Tree0.for_all t ~f
  let for_alli t ~f = Tree0.for_alli t ~f
  let exists t ~f = Tree0.exists t ~f
  let existsi t ~f = Tree0.existsi t ~f
  let count t ~f = Tree0.count t ~f
  let counti t ~f = Tree0.counti t ~f
  let split ~comparator t k = Tree0.split t k ~compare_key:comparator.Comparator.compare

  let append ~comparator ~lower_part ~upper_part =
    Tree0.append ~lower_part ~upper_part ~compare_key:comparator.Comparator.compare
  ;;

  let subrange ~comparator t ~lower_bound ~upper_bound =
    let _, ret, _ =
      Tree0.split_range
        t
        ~lower_bound
        ~upper_bound
        ~compare_key:comparator.Comparator.compare
    in
    ret
  ;;

  let fold_range_inclusive ~comparator t ~min ~max ~init ~f =
    Tree0.fold_range_inclusive
      t
      ~min
      ~max
      ~init
      ~f
      ~compare_key:comparator.Comparator.compare
  ;;

  let range_to_alist ~comparator t ~min ~max =
    Tree0.range_to_alist t ~min ~max ~compare_key:comparator.Comparator.compare
  ;;

  let closest_key ~comparator t dir key =
    Tree0.closest_key t dir key ~compare_key:comparator.Comparator.compare
  ;;

  let nth ~comparator:_ t n = Tree0.nth t n
  let nth_exn ~comparator t n = Option.value_exn (nth ~comparator t n)

  let rank ~comparator t key =
    Tree0.rank t key ~compare_key:comparator.Comparator.compare
  ;;

  let sexp_of_t sexp_of_k sexp_of_v _ t = Tree0.sexp_of_t sexp_of_k sexp_of_v t

  let t_of_sexp_direct ~comparator k_of_sexp v_of_sexp sexp =
    fst (Tree0.t_of_sexp_direct k_of_sexp v_of_sexp sexp ~comparator)
  ;;

  let to_sequence ~comparator ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t =
    Tree0.to_sequence
      comparator
      ?order
      ?keys_greater_or_equal_to
      ?keys_less_or_equal_to
      t
  ;;

  let binary_search ~comparator:_ t ~compare how v = Tree0.binary_search t ~compare how v

  let binary_search_segmented ~comparator:_ t ~segment_of how =
    Tree0.binary_search_segmented t ~segment_of how
  ;;
end

module Using_comparator = struct
  type nonrec ('k, 'v, 'cmp) t = ('k, 'v, 'cmp) t

  include Accessors

  let empty ~comparator = { tree = Tree0.empty; comparator; length = 0 }
  let singleton ~comparator k v = { comparator; tree = Tree0.singleton k v; length = 1 }
  let of_tree0 ~comparator (tree, length) = { comparator; tree; length }
  let of_tree ~comparator tree = of_tree0 ~comparator (tree, Tree0.length tree)
  let to_tree = to_tree

  let of_sorted_array_unchecked ~comparator array =
    of_tree0
      ~comparator
      (Tree0.of_sorted_array_unchecked array ~compare_key:comparator.Comparator.compare)
  ;;

  let of_sorted_array ~comparator array =
    Or_error.map
      (Tree0.of_sorted_array array ~compare_key:comparator.Comparator.compare)
      ~f:(fun tree -> of_tree0 ~comparator tree)
  ;;

  let of_alist ~comparator alist =
    match Tree0.of_alist alist ~compare_key:comparator.Comparator.compare with
    | `Ok (tree, length) -> `Ok { comparator; tree; length }
    | `Duplicate_key _ as z -> z
  ;;

  let of_alist_or_error ~comparator alist =
    Result.map (Tree0.of_alist_or_error alist ~comparator) ~f:(fun tree ->
      of_tree0 ~comparator tree)
  ;;

  let of_alist_exn ~comparator alist =
    of_tree0 ~comparator (Tree0.of_alist_exn alist ~comparator)
  ;;

  let of_alist_multi ~comparator alist =
    of_tree0
      ~comparator
      (Tree0.of_alist_multi alist ~compare_key:comparator.Comparator.compare)
  ;;

  let of_alist_fold ~comparator alist ~init ~f =
    of_tree0
      ~comparator
      (Tree0.of_alist_fold alist ~init ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let of_alist_reduce ~comparator alist ~f =
    of_tree0
      ~comparator
      (Tree0.of_alist_reduce alist ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let of_iteri ~comparator ~iteri =
    match Tree0.of_iteri ~compare_key:comparator.Comparator.compare ~iteri with
    | `Ok tree_length -> `Ok (of_tree0 ~comparator tree_length)
    | `Duplicate_key _ as z -> z
  ;;

  let of_increasing_iterator_unchecked ~comparator ~len ~f =
    of_tree0 ~comparator (Tree0.of_increasing_iterator_unchecked ~len ~f, len)
  ;;

  let of_increasing_sequence ~comparator seq =
    Or_error.map
      ~f:(of_tree0 ~comparator)
      (Tree0.of_increasing_sequence seq ~compare_key:comparator.Comparator.compare)
  ;;

  let of_sequence ~comparator seq =
    match Tree0.of_sequence seq ~compare_key:comparator.Comparator.compare with
    | `Ok (tree, length) -> `Ok { comparator; tree; length }
    | `Duplicate_key _ as z -> z
  ;;

  let of_sequence_or_error ~comparator seq =
    Result.map (Tree0.of_sequence_or_error seq ~comparator) ~f:(fun tree ->
      of_tree0 ~comparator tree)
  ;;

  let of_sequence_exn ~comparator seq =
    of_tree0 ~comparator (Tree0.of_sequence_exn seq ~comparator)
  ;;

  let of_sequence_multi ~comparator seq =
    of_tree0
      ~comparator
      (Tree0.of_sequence_multi seq ~compare_key:comparator.Comparator.compare)
  ;;

  let of_sequence_fold ~comparator seq ~init ~f =
    of_tree0
      ~comparator
      (Tree0.of_sequence_fold seq ~init ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let of_sequence_reduce ~comparator seq ~f =
    of_tree0
      ~comparator
      (Tree0.of_sequence_reduce seq ~f ~compare_key:comparator.Comparator.compare)
  ;;

  let t_of_sexp_direct ~comparator k_of_sexp v_of_sexp sexp =
    of_tree0 ~comparator (Tree0.t_of_sexp_direct k_of_sexp v_of_sexp sexp ~comparator)
  ;;

  module Empty_without_value_restriction (K : Comparator.S1) = struct
    let empty = { tree = Tree0.empty; comparator = K.comparator; length = 0 }
  end

  module Tree = Tree
end

include Accessors

type ('k, 'cmp) comparator =
  (module Comparator.S with type t = 'k and type comparator_witness = 'cmp)

let comparator_s (type k cmp) t : (k, cmp) comparator =
  (module struct
    type t = k
    type comparator_witness = cmp

    let comparator = t.comparator
  end)
;;

let to_comparator (type k cmp) ((module M) : (k, cmp) comparator) = M.comparator
let empty m = Using_comparator.empty ~comparator:(to_comparator m)
let singleton m a = Using_comparator.singleton ~comparator:(to_comparator m) a
let of_alist m a = Using_comparator.of_alist ~comparator:(to_comparator m) a

let of_alist_or_error m a =
  Using_comparator.of_alist_or_error ~comparator:(to_comparator m) a
;;

let of_alist_exn m a = Using_comparator.of_alist_exn ~comparator:(to_comparator m) a
let of_alist_multi m a = Using_comparator.of_alist_multi ~comparator:(to_comparator m) a

let of_alist_fold m a ~init ~f =
  Using_comparator.of_alist_fold ~comparator:(to_comparator m) a ~init ~f
;;

let of_alist_reduce m a ~f =
  Using_comparator.of_alist_reduce ~comparator:(to_comparator m) a ~f
;;

let of_sorted_array_unchecked m a =
  Using_comparator.of_sorted_array_unchecked ~comparator:(to_comparator m) a
;;

let of_sorted_array m a =
  Using_comparator.of_sorted_array ~comparator:(to_comparator m) a
;;

let of_iteri m ~iteri = Using_comparator.of_iteri ~iteri ~comparator:(to_comparator m)

let of_increasing_iterator_unchecked m ~len ~f =
  Using_comparator.of_increasing_iterator_unchecked ~len ~f ~comparator:(to_comparator m)
;;

let of_increasing_sequence m seq =
  Using_comparator.of_increasing_sequence ~comparator:(to_comparator m) seq
;;

let of_sequence m s = Using_comparator.of_sequence ~comparator:(to_comparator m) s

let of_sequence_or_error m s =
  Using_comparator.of_sequence_or_error ~comparator:(to_comparator m) s
;;

let of_sequence_exn m s =
  Using_comparator.of_sequence_exn ~comparator:(to_comparator m) s
;;

let of_sequence_multi m s =
  Using_comparator.of_sequence_multi ~comparator:(to_comparator m) s
;;

let of_sequence_fold m s ~init ~f =
  Using_comparator.of_sequence_fold ~comparator:(to_comparator m) s ~init ~f
;;

let of_sequence_reduce m s ~f =
  Using_comparator.of_sequence_reduce ~comparator:(to_comparator m) s ~f
;;

module M (K : sig
    type t
    type comparator_witness
  end) =
struct
  type nonrec 'v t = (K.t, 'v, K.comparator_witness) t
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

  include Comparator.S with type t := t
end

module type Compare_m = sig end
module type Equal_m = sig end
module type Hash_fold_m = Hasher.S

let sexp_of_m__t (type k) (module K : Sexp_of_m with type t = k) sexp_of_v t =
  sexp_of_t K.sexp_of_t sexp_of_v (fun _ -> Sexp.Atom "_") t
;;

let m__t_of_sexp
      (type k cmp)
      (module K : M_of_sexp with type t = k and type comparator_witness = cmp)
      v_of_sexp
      sexp
  =
  Using_comparator.t_of_sexp_direct ~comparator:K.comparator K.t_of_sexp v_of_sexp sexp
;;

let m__t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t =
  Inline
    (Explicit_bind
       ( [ "'k"; "'v" ]
       , Apply
           ( Grammar list_sexp_grammar
           , [ Apply
                 ( Grammar Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.tuple2_sexp_grammar
                 , [ Explicit_var 0; Explicit_var 1 ] )
             ] ) ))
;;

let compare_m__t (module K : Compare_m) compare_v t1 t2 = compare_direct compare_v t1 t2
let equal_m__t (module K : Equal_m) equal_v t1 t2 = equal equal_v t1 t2

let hash_fold_m__t (type k) (module K : Hash_fold_m with type t = k) hash_fold_v state =
  hash_fold_direct K.hash_fold_t hash_fold_v state
;;

let merge_skewed t1 t2 ~combine =
  let t1, t2, combine =
    if length t2 <= length t1
    then t1, t2, combine
    else t2, t1, fun ~key v1 v2 -> combine ~key v2 v1
  in
  fold t2 ~init:t1 ~f:(fun ~key ~data:v2 t1 ->
    change t1 key ~f:(function
      | None -> Some v2
      | Some v1 -> Some (combine ~key v1 v2)))
;;

module Poly = struct
  type nonrec ('k, 'v) t = ('k, 'v, Comparator.Poly.comparator_witness) t
  type nonrec ('k, 'v) tree = ('k, 'v) Tree0.t
  type comparator_witness = Comparator.Poly.comparator_witness

  include Accessors

  let comparator = Comparator.Poly.comparator
  let of_tree tree = { tree; comparator; length = Tree0.length tree }

  include Using_comparator.Empty_without_value_restriction (Comparator.Poly)

  let singleton a = Using_comparator.singleton ~comparator a
  let of_alist a = Using_comparator.of_alist ~comparator a
  let of_alist_or_error a = Using_comparator.of_alist_or_error ~comparator a
  let of_alist_exn a = Using_comparator.of_alist_exn ~comparator a
  let of_alist_multi a = Using_comparator.of_alist_multi ~comparator a
  let of_alist_fold a ~init ~f = Using_comparator.of_alist_fold ~comparator a ~init ~f
  let of_alist_reduce a ~f = Using_comparator.of_alist_reduce ~comparator a ~f

  let of_sorted_array_unchecked a =
    Using_comparator.of_sorted_array_unchecked ~comparator a
  ;;

  let of_sorted_array a = Using_comparator.of_sorted_array ~comparator a
  let of_iteri ~iteri = Using_comparator.of_iteri ~iteri ~comparator

  let of_increasing_iterator_unchecked ~len ~f =
    Using_comparator.of_increasing_iterator_unchecked ~len ~f ~comparator
  ;;

  let of_increasing_sequence seq =
    Using_comparator.of_increasing_sequence ~comparator seq
  ;;

  let of_sequence s = Using_comparator.of_sequence ~comparator s
  let of_sequence_or_error s = Using_comparator.of_sequence_or_error ~comparator s
  let of_sequence_exn s = Using_comparator.of_sequence_exn ~comparator s
  let of_sequence_multi s = Using_comparator.of_sequence_multi ~comparator s

  let of_sequence_fold s ~init ~f =
    Using_comparator.of_sequence_fold ~comparator s ~init ~f
  ;;

  let of_sequence_reduce s ~f = Using_comparator.of_sequence_reduce ~comparator s ~f
end
