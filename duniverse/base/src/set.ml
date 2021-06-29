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

(* Sets over ordered types *)

open! Import
include Set_intf

let with_return = With_return.with_return


module Tree0 = struct
  type 'a t =
    | Empty
    (* (Leaf x) is the same as (Node (Empty, x, Empty, 1, 1)) but uses less space. *)
    | Leaf of 'a
    (* first int is height, second is sub-tree size *)
    | Node of 'a t * 'a * 'a t * int * int

  type 'a tree = 'a t

  (* Sets are represented by balanced binary trees (the heights of the children differ by
     at most 2. *)
  let height = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node (_, _, _, h, _) -> h
  ;;

  let length = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node (_, _, _, _, s) -> s
  ;;

  let invariants =
    let in_range lower upper compare_elt v =
      (match lower with
       | None -> true
       | Some lower -> compare_elt lower v < 0)
      &&
      match upper with
      | None -> true
      | Some upper -> compare_elt v upper < 0
    in
    let rec loop lower upper compare_elt t =
      match t with
      | Empty -> true
      | Leaf v -> in_range lower upper compare_elt v
      | Node (l, v, r, h, n) ->
        let hl = height l
        and hr = height r in
        abs (hl - hr) <= 2
        && h = max hl hr + 1
        && n = length l + length r + 1
        && in_range lower upper compare_elt v
        && loop lower (Some v) compare_elt l
        && loop (Some v) upper compare_elt r
    in
    fun t ~compare_elt -> loop None None compare_elt t
  ;;

  let is_empty = function
    | Empty -> true
    | Leaf _ | Node _ -> false
  ;;

  (* Creates a new node with left son l, value v and right son r.
     We must have all elements of l < v < all elements of r.
     l and r must be balanced and | height l - height r | <= 2.
     Inline expansion of height for better speed. *)

  let create l v r =
    let hl =
      match l with
      | Empty -> 0
      | Leaf _ -> 1
      | Node (_, _, _, h, _) -> h
    in
    let hr =
      match r with
      | Empty -> 0
      | Leaf _ -> 1
      | Node (_, _, _, h, _) -> h
    in
    let h = if hl >= hr then hl + 1 else hr + 1 in
    if h = 1
    then Leaf v
    else (
      let sl =
        match l with
        | Empty -> 0
        | Leaf _ -> 1
        | Node (_, _, _, _, s) -> s
      in
      let sr =
        match r with
        | Empty -> 0
        | Leaf _ -> 1
        | Node (_, _, _, _, s) -> s
      in
      Node (l, v, r, h, sl + sr + 1))
  ;;

  (* We must call [f] with increasing indexes, because the bin_prot reader in
     Core_kernel.Set needs it. *)
  let of_increasing_iterator_unchecked ~len ~f =
    let rec loop n ~f i =
      match n with
      | 0 -> Empty
      | 1 ->
        let k = f i in
        Leaf k
      | 2 ->
        let kl = f i in
        let k = f (i + 1) in
        create (Leaf kl) k Empty
      | 3 ->
        let kl = f i in
        let k = f (i + 1) in
        let kr = f (i + 2) in
        create (Leaf kl) k (Leaf kr)
      | n ->
        let left_length = n lsr 1 in
        let right_length = n - left_length - 1 in
        let left = loop left_length ~f i in
        let k = f (i + left_length) in
        let right = loop right_length ~f (i + left_length + 1) in
        create left k right
    in
    loop len ~f 0
  ;;

  let of_sorted_array_unchecked array ~compare_elt =
    let array_length = Array.length array in
    let next =
      (* We don't check if the array is sorted or keys are duplicated, because that
         checking is slower than the whole [of_sorted_array] function *)
      if array_length < 2 || compare_elt array.(0) array.(1) < 0
      then fun i -> array.(i)
      else fun i -> array.(array_length - 1 - i)
    in
    of_increasing_iterator_unchecked ~len:array_length ~f:next
  ;;

  let of_sorted_array array ~compare_elt =
    match array with
    | [||] | [| _ |] -> Result.Ok (of_sorted_array_unchecked array ~compare_elt)
    | _ ->
      with_return (fun r ->
        let increasing =
          match compare_elt array.(0) array.(1) with
          | 0 ->
            r.return (Or_error.error_string "of_sorted_array: duplicated elements")
          | i -> i < 0
        in
        for i = 1 to Array.length array - 2 do
          match compare_elt array.(i) array.(i + 1) with
          | 0 ->
            r.return (Or_error.error_string "of_sorted_array: duplicated elements")
          | i ->
            if Poly.( <> ) (i < 0) increasing
            then
              r.return
                (Or_error.error_string "of_sorted_array: elements are not ordered")
        done;
        Result.Ok (of_sorted_array_unchecked array ~compare_elt))
  ;;

  (* Same as create, but performs one step of rebalancing if necessary.
     Assumes l and r balanced and | height l - height r | <= 3.
     Inline expansion of create for better speed in the most frequent case
     where no rebalancing is required. *)

  let bal l v r =
    let hl =
      match l with
      | Empty -> 0
      | Leaf _ -> 1
      | Node (_, _, _, h, _) -> h
    in
    let hr =
      match r with
      | Empty -> 0
      | Leaf _ -> 1
      | Node (_, _, _, h, _) -> h
    in
    if hl > hr + 2
    then (
      match l with
      | Empty -> assert false
      | Leaf _ -> assert false (* because h(l)>h(r)+2 and h(leaf)=1 *)
      | Node (ll, lv, lr, _, _) ->
        if height ll >= height lr
        then create ll lv (create lr v r)
        else (
          match lr with
          | Empty -> assert false
          | Leaf lrv ->
            assert (is_empty ll);
            create (create ll lv Empty) lrv (create Empty v r)
          | Node (lrl, lrv, lrr, _, _) -> create (create ll lv lrl) lrv (create lrr v r)))
    else if hr > hl + 2
    then (
      match r with
      | Empty -> assert false
      | Leaf rv -> create (create l v Empty) rv Empty
      | Node (rl, rv, rr, _, _) ->
        if height rr >= height rl
        then create (create l v rl) rv rr
        else (
          match rl with
          | Empty -> assert false
          | Leaf rlv ->
            assert (is_empty rr);
            create (create l v Empty) rlv (create Empty rv rr)
          | Node (rll, rlv, rlr, _, _) -> create (create l v rll) rlv (create rlr rv rr)))
    else (
      let h = if hl >= hr then hl + 1 else hr + 1 in
      let sl =
        match l with
        | Empty -> 0
        | Leaf _ -> 1
        | Node (_, _, _, _, s) -> s
      in
      let sr =
        match r with
        | Empty -> 0
        | Leaf _ -> 1
        | Node (_, _, _, _, s) -> s
      in
      if h = 1 then Leaf v else Node (l, v, r, h, sl + sr + 1))
  ;;

  (* Insertion of one element *)

  exception Same

  let add t x ~compare_elt =
    let rec aux = function
      | Empty -> Leaf x
      | Leaf v ->
        let c = compare_elt x v in
        if c = 0
        then raise Same
        else if c < 0
        then bal (Leaf x) v Empty
        else bal Empty v (Leaf x)
      | Node (l, v, r, _, _) ->
        let c = compare_elt x v in
        if c = 0 then raise Same else if c < 0 then bal (aux l) v r else bal l v (aux r)
    in
    try aux t with
    | Same -> t
  ;;

  (* Same as create and bal, but no assumptions are made on the relative heights of l and
     r. *)
  let rec join l v r ~compare_elt =
    match l, r with
    | Empty, _ -> add r v ~compare_elt
    | _, Empty -> add l v ~compare_elt
    | Leaf lv, _ -> add (add r v ~compare_elt) lv ~compare_elt
    | _, Leaf rv -> add (add l v ~compare_elt) rv ~compare_elt
    | Node (ll, lv, lr, lh, _), Node (rl, rv, rr, rh, _) ->
      if lh > rh + 2
      then bal ll lv (join lr v r ~compare_elt)
      else if rh > lh + 2
      then bal (join l v rl ~compare_elt) rv rr
      else create l v r
  ;;

  (* Smallest and greatest element of a set *)
  let rec min_elt = function
    | Empty -> None
    | Leaf v | Node (Empty, v, _, _, _) -> Some v
    | Node (l, _, _, _, _) -> min_elt l
  ;;

  exception Set_min_elt_exn_of_empty_set [@@deriving_inline sexp]

  let () =
    Ppx_sexp_conv_lib.Conv.Exn_converter.add
      [%extension_constructor Set_min_elt_exn_of_empty_set]
      (function
        | Set_min_elt_exn_of_empty_set ->
          Ppx_sexp_conv_lib.Sexp.Atom "set.ml.Tree0.Set_min_elt_exn_of_empty_set"
        | _ -> assert false)
  ;;

  [@@@end]

  exception Set_max_elt_exn_of_empty_set [@@deriving_inline sexp]

  let () =
    Ppx_sexp_conv_lib.Conv.Exn_converter.add
      [%extension_constructor Set_max_elt_exn_of_empty_set]
      (function
        | Set_max_elt_exn_of_empty_set ->
          Ppx_sexp_conv_lib.Sexp.Atom "set.ml.Tree0.Set_max_elt_exn_of_empty_set"
        | _ -> assert false)
  ;;

  [@@@end]

  let min_elt_exn t =
    match min_elt t with
    | None -> raise Set_min_elt_exn_of_empty_set
    | Some v -> v
  ;;

  let fold_until t ~init ~f ~finish =
    let rec fold_until_helper ~f t acc =
      match t with
      | Empty -> Continue_or_stop.Continue acc
      | Leaf value -> f acc value
      | Node (left, value, right, _, _) ->
        (match fold_until_helper ~f left acc with
         | Stop _a as x -> x
         | Continue acc ->
           (match f acc value with
            | Stop _a as x -> x
            | Continue a -> fold_until_helper ~f right a))
    in
    match fold_until_helper ~f t init with
    | Continue x -> finish x
    | Stop x -> x
  ;;

  let rec max_elt = function
    | Empty -> None
    | Leaf v | Node (_, v, Empty, _, _) -> Some v
    | Node (_, _, r, _, _) -> max_elt r
  ;;

  let max_elt_exn t =
    match max_elt t with
    | None -> raise Set_max_elt_exn_of_empty_set
    | Some v -> v
  ;;

  (* Remove the smallest element of the given set *)

  let rec remove_min_elt = function
    | Empty -> invalid_arg "Set.remove_min_elt"
    | Leaf _ -> Empty
    | Node (Empty, _, r, _, _) -> r
    | Node (l, v, r, _, _) -> bal (remove_min_elt l) v r
  ;;

  (* Merge two trees l and r into one.  All elements of l must precede the elements of r.
     Assume | height l - height r | <= 2. *)
  let merge t1 t2 =
    match t1, t2 with
    | Empty, t -> t
    | t, Empty -> t
    | _, _ -> bal t1 (min_elt_exn t2) (remove_min_elt t2)
  ;;

  (* Merge two trees l and r into one.  All elements of l must precede the elements of r.
     No assumption on the heights of l and r. *)
  let concat t1 t2 ~compare_elt =
    match t1, t2 with
    | Empty, t | t, Empty -> t
    | _, _ -> join t1 (min_elt_exn t2) (remove_min_elt t2) ~compare_elt
  ;;

  let split t x ~compare_elt =
    let rec split t =
      match t with
      | Empty -> Empty, None, Empty
      | Leaf v ->
        let c = compare_elt x v in
        if c = 0
        then Empty, Some v, Empty
        else if c < 0
        then Empty, None, Leaf v
        else Leaf v, None, Empty
      | Node (l, v, r, _, _) ->
        let c = compare_elt x v in
        if c = 0
        then l, Some v, r
        else if c < 0
        then (
          let ll, maybe_elt, rl = split l in
          ll, maybe_elt, join rl v r ~compare_elt)
        else (
          let lr, maybe_elt, rr = split r in
          join l v lr ~compare_elt, maybe_elt, rr)
    in
    split t
  ;;

  (* Implementation of the set operations *)

  let empty = Empty

  let rec mem t x ~compare_elt =
    match t with
    | Empty -> false
    | Leaf v ->
      let c = compare_elt x v in
      c = 0
    | Node (l, v, r, _, _) ->
      let c = compare_elt x v in
      c = 0 || mem (if c < 0 then l else r) x ~compare_elt
  ;;

  let singleton x = Leaf x

  let remove t x ~compare_elt =
    let rec aux t =
      match t with
      | Empty -> raise Same
      | Leaf v -> if compare_elt x v = 0 then Empty else raise Same
      | Node (l, v, r, _, _) ->
        let c = compare_elt x v in
        if c = 0 then merge l r else if c < 0 then bal (aux l) v r else bal l v (aux r)
    in
    try aux t with
    | Same -> t
  ;;

  let remove_index t i ~compare_elt:_ =
    let rec aux t i =
      match t with
      | Empty -> raise Same
      | Leaf _ -> if i = 0 then Empty else raise Same
      | Node (l, v, r, _, _) ->
        let l_size = length l in
        let c = Poly.compare i l_size in
        if c = 0
        then merge l r
        else if c < 0
        then bal (aux l i) v r
        else bal l v (aux r (i - l_size - 1))
    in
    try aux t i with
    | Same -> t
  ;;

  let union s1 s2 ~compare_elt =
    let rec union s1 s2 =
      if phys_equal s1 s2
      then s1
      else (
        match s1, s2 with
        | Empty, t | t, Empty -> t
        | Leaf v1, _ -> union (Node (Empty, v1, Empty, 1, 1)) s2
        | _, Leaf v2 -> union s1 (Node (Empty, v2, Empty, 1, 1))
        | Node (l1, v1, r1, h1, _), Node (l2, v2, r2, h2, _) ->
          if h1 >= h2
          then
            if h2 = 1
            then add s1 v2 ~compare_elt
            else (
              let l2, _, r2 = split s2 v1 ~compare_elt in
              join (union l1 l2) v1 (union r1 r2) ~compare_elt)
          else if h1 = 1
          then add s2 v1 ~compare_elt
          else (
            let l1, _, r1 = split s1 v2 ~compare_elt in
            join (union l1 l2) v2 (union r1 r2) ~compare_elt))
    in
    union s1 s2
  ;;

  let union_list ~comparator ~to_tree xs =
    let compare_elt = comparator.Comparator.compare in
    List.fold xs ~init:empty ~f:(fun ac x -> union ac (to_tree x) ~compare_elt)
  ;;

  let inter s1 s2 ~compare_elt =
    let rec inter s1 s2 =
      if phys_equal s1 s2
      then s1
      else (
        match s1, s2 with
        | Empty, _ | _, Empty -> Empty
        | (Leaf elt as singleton), other_set | other_set, (Leaf elt as singleton) ->
          if mem other_set elt ~compare_elt then singleton else Empty
        | Node (l1, v1, r1, _, _), t2 ->
          (match split t2 v1 ~compare_elt with
           | l2, None, r2 -> concat (inter l1 l2) (inter r1 r2) ~compare_elt
           | l2, Some v1, r2 -> join (inter l1 l2) v1 (inter r1 r2) ~compare_elt))
    in
    inter s1 s2
  ;;

  let diff s1 s2 ~compare_elt =
    let rec diff s1 s2 =
      if phys_equal s1 s2
      then Empty
      else (
        match s1, s2 with
        | Empty, _ -> Empty
        | t1, Empty -> t1
        | Leaf v1, t2 -> diff (Node (Empty, v1, Empty, 1, 1)) t2
        | Node (l1, v1, r1, _, _), t2 ->
          (match split t2 v1 ~compare_elt with
           | l2, None, r2 -> join (diff l1 l2) v1 (diff r1 r2) ~compare_elt
           | l2, Some _, r2 -> concat (diff l1 l2) (diff r1 r2) ~compare_elt))
    in
    diff s1 s2
  ;;

  module Enum = struct
    type increasing
    type decreasing

    type ('a, 'direction) t =
      | End
      | More of 'a * 'a tree * ('a, 'direction) t

    let rec cons s (e : (_, increasing) t) : (_, increasing) t =
      match s with
      | Empty -> e
      | Leaf v -> More (v, Empty, e)
      | Node (l, v, r, _, _) -> cons l (More (v, r, e))
    ;;

    let rec cons_right s (e : (_, decreasing) t) : (_, decreasing) t =
      match s with
      | Empty -> e
      | Leaf v -> More (v, Empty, e)
      | Node (l, v, r, _, _) -> cons_right r (More (v, l, e))
    ;;

    let of_set s : (_, increasing) t = cons s End
    let of_set_right s : (_, decreasing) t = cons_right s End

    let starting_at_increasing t key compare : (_, increasing) t =
      let rec loop t e =
        match t with
        | Empty -> e
        | Leaf v -> loop (Node (Empty, v, Empty, 1, 1)) e
        | Node (_, v, r, _, _) when compare v key < 0 -> loop r e
        | Node (l, v, r, _, _) -> loop l (More (v, r, e))
      in
      loop t End
    ;;

    let starting_at_decreasing t key compare : (_, decreasing) t =
      let rec loop t e =
        match t with
        | Empty -> e
        | Leaf v -> loop (Node (Empty, v, Empty, 1, 1)) e
        | Node (l, v, _, _, _) when compare v key > 0 -> loop l e
        | Node (l, v, r, _, _) -> loop r (More (v, l, e))
      in
      loop t End
    ;;

    let compare compare_elt e1 e2 =
      let rec loop e1 e2 =
        match e1, e2 with
        | End, End -> 0
        | End, _ -> -1
        | _, End -> 1
        | More (v1, r1, e1), More (v2, r2, e2) ->
          let c = compare_elt v1 v2 in
          if c <> 0
          then c
          else if phys_equal r1 r2
          then loop e1 e2
          else loop (cons r1 e1) (cons r2 e2)
      in
      loop e1 e2
    ;;

    let rec iter ~f = function
      | End -> ()
      | More (a, tree, enum) ->
        f a;
        iter (cons tree enum) ~f
    ;;

    let iter2 compare_elt t1 t2 ~f =
      let rec loop t1 t2 =
        match t1, t2 with
        | End, End -> ()
        | End, _ -> iter t2 ~f:(fun a -> f (`Right a))
        | _, End -> iter t1 ~f:(fun a -> f (`Left a))
        | More (a1, tree1, enum1), More (a2, tree2, enum2) ->
          let compare_result = compare_elt a1 a2 in
          if compare_result = 0
          then (
            f (`Both (a1, a2));
            loop (cons tree1 enum1) (cons tree2 enum2))
          else if compare_result < 0
          then (
            f (`Left a1);
            loop (cons tree1 enum1) t2)
          else (
            f (`Right a2);
            loop t1 (cons tree2 enum2))
      in
      loop t1 t2
    ;;

    let symmetric_diff t1 t2 ~compare_elt =
      let step state : ((_, _) Either.t, _) Sequence.Step.t =
        match state with
        | End, End -> Done
        | End, More (elt, tree, enum) -> Yield (Second elt, (End, cons tree enum))
        | More (elt, tree, enum), End -> Yield (First elt, (cons tree enum, End))
        | (More (a1, tree1, enum1) as left), (More (a2, tree2, enum2) as right) ->
          let compare_result = compare_elt a1 a2 in
          if compare_result = 0
          then (
            let next_state =
              if phys_equal tree1 tree2
              then enum1, enum2
              else cons tree1 enum1, cons tree2 enum2
            in
            Skip next_state)
          else if compare_result < 0
          then Yield (First a1, (cons tree1 enum1, right))
          else Yield (Second a2, (left, cons tree2 enum2))
      in
      Sequence.unfold_step ~init:(of_set t1, of_set t2) ~f:step
    ;;
  end

  let to_sequence_increasing comparator ~from_elt t =
    let next enum =
      match enum with
      | Enum.End -> Sequence.Step.Done
      | Enum.More (k, t, e) -> Sequence.Step.Yield (k, Enum.cons t e)
    in
    let init =
      match from_elt with
      | None -> Enum.of_set t
      | Some key -> Enum.starting_at_increasing t key comparator.Comparator.compare
    in
    Sequence.unfold_step ~init ~f:next
  ;;

  let to_sequence_decreasing comparator ~from_elt t =
    let next enum =
      match enum with
      | Enum.End -> Sequence.Step.Done
      | Enum.More (k, t, e) -> Sequence.Step.Yield (k, Enum.cons_right t e)
    in
    let init =
      match from_elt with
      | None -> Enum.of_set_right t
      | Some key -> Enum.starting_at_decreasing t key comparator.Comparator.compare
    in
    Sequence.unfold_step ~init ~f:next
  ;;

  let to_sequence
        comparator
        ?(order = `Increasing)
        ?greater_or_equal_to
        ?less_or_equal_to
        t
    =
    let inclusive_bound side t bound =
      let compare_elt = comparator.Comparator.compare in
      let l, maybe, r = split t bound ~compare_elt in
      let t = side (l, r) in
      match maybe with
      | None -> t
      | Some elt -> add t elt ~compare_elt
    in
    match order with
    | `Increasing ->
      let t = Option.fold less_or_equal_to ~init:t ~f:(inclusive_bound fst) in
      to_sequence_increasing comparator ~from_elt:greater_or_equal_to t
    | `Decreasing ->
      let t = Option.fold greater_or_equal_to ~init:t ~f:(inclusive_bound snd) in
      to_sequence_decreasing comparator ~from_elt:less_or_equal_to t
  ;;


  let rec find_first_satisfying t ~f =
    match t with
    | Empty -> None
    | Leaf v -> if f v then Some v else None
    | Node (l, v, r, _, _) ->
      if f v
      then (
        match find_first_satisfying l ~f with
        | None -> Some v
        | Some _ as x -> x)
      else find_first_satisfying r ~f
  ;;

  let rec find_last_satisfying t ~f =
    match t with
    | Empty -> None
    | Leaf v -> if f v then Some v else None
    | Node (l, v, r, _, _) ->
      if f v
      then (
        match find_last_satisfying r ~f with
        | None -> Some v
        | Some _ as x -> x)
      else find_last_satisfying l ~f
  ;;

  let binary_search t ~compare how v =
    match how with
    | `Last_strictly_less_than -> find_last_satisfying t ~f:(fun x -> compare x v < 0)
    | `Last_less_than_or_equal_to ->
      find_last_satisfying t ~f:(fun x -> compare x v <= 0)
    | `First_equal_to ->
      (match find_first_satisfying t ~f:(fun x -> compare x v >= 0) with
       | Some x as elt when compare x v = 0 -> elt
       | None | Some _ -> None)
    | `Last_equal_to ->
      (match find_last_satisfying t ~f:(fun x -> compare x v <= 0) with
       | Some x as elt when compare x v = 0 -> elt
       | None | Some _ -> None)
    | `First_greater_than_or_equal_to ->
      find_first_satisfying t ~f:(fun x -> compare x v >= 0)
    | `First_strictly_greater_than ->
      find_first_satisfying t ~f:(fun x -> compare x v > 0)
  ;;

  let binary_search_segmented t ~segment_of how =
    let is_left x =
      match segment_of x with
      | `Left -> true
      | `Right -> false
    in
    let is_right x = not (is_left x) in
    match how with
    | `Last_on_left -> find_last_satisfying t ~f:is_left
    | `First_on_right -> find_first_satisfying t ~f:is_right
  ;;

  let merge_to_sequence
        comparator
        ?(order = `Increasing)
        ?greater_or_equal_to
        ?less_or_equal_to
        t
        t'
    =
    Sequence.merge_with_duplicates
      (to_sequence comparator ~order ?greater_or_equal_to ?less_or_equal_to t)
      (to_sequence comparator ~order ?greater_or_equal_to ?less_or_equal_to t')
      ~compare:
        (match order with
         | `Increasing -> comparator.compare
         | `Decreasing -> Fn.flip comparator.compare)
  ;;

  let compare compare_elt s1 s2 =
    Enum.compare compare_elt (Enum.of_set s1) (Enum.of_set s2)
  ;;

  let iter2 s1 s2 ~compare_elt = Enum.iter2 compare_elt (Enum.of_set s1) (Enum.of_set s2)
  let equal s1 s2 ~compare_elt = compare compare_elt s1 s2 = 0

  let is_subset s1 ~of_:s2 ~compare_elt =
    let rec is_subset s1 ~of_:s2 =
      match s1, s2 with
      | Empty, _ -> true
      | _, Empty -> false
      | Leaf v1, t2 -> mem t2 v1 ~compare_elt
      | Node (l1, v1, r1, _, _), Leaf v2 ->
        (match l1, r1 with
         | Empty, Empty ->
           (* This case shouldn't occur in practice because we should have constructed
              a Leaf rather than a Node with two Empty subtrees *)
           compare_elt v1 v2 = 0
         | _, _ -> false)
      | Node (l1, v1, r1, _, _), (Node (l2, v2, r2, _, _) as t2) ->
        let c = compare_elt v1 v2 in
        if c = 0
        then
          phys_equal s1 s2 || (is_subset l1 ~of_:l2 && is_subset r1 ~of_:r2)
          (* Note that height and size don't matter here. *)
        else if c < 0
        then is_subset (Node (l1, v1, Empty, 0, 0)) ~of_:l2 && is_subset r1 ~of_:t2
        else is_subset (Node (Empty, v1, r1, 0, 0)) ~of_:r2 && is_subset l1 ~of_:t2
    in
    is_subset s1 ~of_:s2
  ;;

  let rec are_disjoint s1 s2 ~compare_elt =
    match s1, s2 with
    | Empty, _ | _, Empty -> true
    | Leaf elt, other_set | other_set, Leaf elt -> not (mem other_set elt ~compare_elt)
    | Node (l1, v1, r1, _, _), t2 ->
      if phys_equal s1 s2
      then false
      else (
        match split t2 v1 ~compare_elt with
        | l2, None, r2 ->
          are_disjoint l1 l2 ~compare_elt && are_disjoint r1 r2 ~compare_elt
        | _, Some _, _ -> false)
  ;;

  let iter t ~f =
    let rec iter = function
      | Empty -> ()
      | Leaf v -> f v
      | Node (l, v, r, _, _) ->
        iter l;
        f v;
        iter r
    in
    iter t
  ;;

  let symmetric_diff = Enum.symmetric_diff

  let rec fold s ~init:accu ~f =
    match s with
    | Empty -> accu
    | Leaf v -> f accu v
    | Node (l, v, r, _, _) -> fold ~f r ~init:(f (fold ~f l ~init:accu) v)
  ;;

  let hash_fold_t_ignoring_structure hash_fold_elem state t =
    fold t ~init:(hash_fold_int state (length t)) ~f:hash_fold_elem
  ;;

  let count t ~f = Container.count ~fold t ~f
  let sum m t ~f = Container.sum ~fold m t ~f

  let rec fold_right s ~init:accu ~f =
    match s with
    | Empty -> accu
    | Leaf v -> f v accu
    | Node (l, v, r, _, _) -> fold_right ~f l ~init:(f v (fold_right ~f r ~init:accu))
  ;;

  let rec for_all t ~f:p =
    match t with
    | Empty -> true
    | Leaf v -> p v
    | Node (l, v, r, _, _) -> p v && for_all ~f:p l && for_all ~f:p r
  ;;

  let rec exists t ~f:p =
    match t with
    | Empty -> false
    | Leaf v -> p v
    | Node (l, v, r, _, _) -> p v || exists ~f:p l || exists ~f:p r
  ;;

  let filter s ~f:p ~compare_elt =
    let rec filt accu = function
      | Empty -> accu
      | Leaf v -> if p v then add accu v ~compare_elt else accu
      | Node (l, v, r, _, _) ->
        filt (filt (if p v then add accu v ~compare_elt else accu) l) r
    in
    filt Empty s
  ;;

  let filter_map s ~f:p ~compare_elt =
    let rec filt accu = function
      | Empty -> accu
      | Leaf v ->
        (match p v with
         | None -> accu
         | Some v -> add accu v ~compare_elt)
      | Node (l, v, r, _, _) ->
        filt
          (filt
             (match p v with
              | None -> accu
              | Some v -> add accu v ~compare_elt)
             l)
          r
    in
    filt Empty s
  ;;

  let partition_tf s ~f:p ~compare_elt =
    let rec part ((t, f) as accu) = function
      | Empty -> accu
      | Leaf v -> if p v then add t v ~compare_elt, f else t, add f v ~compare_elt
      | Node (l, v, r, _, _) ->
        part
          (part (if p v then add t v ~compare_elt, f else t, add f v ~compare_elt) l)
          r
    in
    part (Empty, Empty) s
  ;;

  let rec elements_aux accu = function
    | Empty -> accu
    | Leaf v -> v :: accu
    | Node (l, v, r, _, _) -> elements_aux (v :: elements_aux accu r) l
  ;;

  let elements s = elements_aux [] s

  let choose t =
    match t with
    | Empty -> None
    | Leaf v -> Some v
    | Node (_, v, _, _, _) -> Some v
  ;;

  let choose_exn =
    let not_found = Not_found_s (Atom "Set.choose_exn: empty set") in
    let choose_exn t =
      match choose t with
      | None -> raise not_found
      | Some v -> v
    in
    (* named to preserve symbol in compiled binary *)
    choose_exn
  ;;

  let of_list lst ~compare_elt =
    List.fold lst ~init:empty ~f:(fun t x -> add t x ~compare_elt)
  ;;

  let to_list s = elements s

  let of_array a ~compare_elt =
    Array.fold a ~init:empty ~f:(fun t x -> add t x ~compare_elt)
  ;;

  (* faster but equivalent to [Array.of_list (to_list t)] *)
  let to_array = function
    | Empty -> [||]
    | Leaf v -> [| v |]
    | Node (l, v, r, _, s) ->
      let res = Array.create ~len:s v in
      let pos_ref = ref 0 in
      let rec loop = function
        (* Invariant: on entry and on exit to [loop], !pos_ref is the next
           available cell in the array. *)
        | Empty -> ()
        | Leaf v ->
          res.(!pos_ref) <- v;
          incr pos_ref
        | Node (l, v, r, _, _) ->
          loop l;
          res.(!pos_ref) <- v;
          incr pos_ref;
          loop r
      in
      loop l;
      (* res.(!pos_ref) is already initialized (by Array.create ~len:above). *)
      incr pos_ref;
      loop r;
      res
  ;;

  let map t ~f ~compare_elt = fold t ~init:empty ~f:(fun t x -> add t (f x) ~compare_elt)

  let group_by set ~equiv ~compare_elt =
    let rec loop set equiv_classes =
      if is_empty set
      then equiv_classes
      else (
        let x = choose_exn set in
        let equiv_x, not_equiv_x =
          partition_tf set ~f:(fun elt -> phys_equal x elt || equiv x elt) ~compare_elt
        in
        loop not_equiv_x (equiv_x :: equiv_classes))
    in
    loop set []
  ;;

  let rec find t ~f =
    match t with
    | Empty -> None
    | Leaf v -> if f v then Some v else None
    | Node (l, v, r, _, _) ->
      if f v
      then Some v
      else (
        match find l ~f with
        | None -> find r ~f
        | Some _ as r -> r)
  ;;

  let rec find_map t ~f =
    match t with
    | Empty -> None
    | Leaf v -> f v
    | Node (l, v, r, _, _) ->
      (match f v with
       | Some _ as r -> r
       | None ->
         (match find_map l ~f with
          | None -> find_map r ~f
          | Some _ as r -> r))
  ;;

  let find_exn t ~f =
    match find t ~f with
    | None -> failwith "Set.find_exn failed to find a matching element"
    | Some e -> e
  ;;

  let rec nth t i =
    match t with
    | Empty -> None
    | Leaf v -> if i = 0 then Some v else None
    | Node (l, v, r, _, s) ->
      if i >= s
      then None
      else (
        let l_size = length l in
        let c = Poly.compare i l_size in
        if c < 0 then nth l i else if c = 0 then Some v else nth r (i - l_size - 1))
  ;;

  let stable_dedup_list xs ~compare_elt =
    let rec loop xs leftovers already_seen =
      match xs with
      | [] -> List.rev leftovers
      | hd :: tl ->
        if mem already_seen hd ~compare_elt
        then loop tl leftovers already_seen
        else loop tl (hd :: leftovers) (add already_seen hd ~compare_elt)
    in
    loop xs [] empty
  ;;

  let t_of_sexp_direct a_of_sexp sexp ~compare_elt =
    match sexp with
    | Sexp.List lst ->
      let elt_lst = List.map lst ~f:a_of_sexp in
      let set = of_list elt_lst ~compare_elt in
      if length set = List.length lst
      then set
      else (
        let set = ref empty in
        List.iter2_exn lst elt_lst ~f:(fun el_sexp el ->
          if mem !set el ~compare_elt
          then of_sexp_error "Set.t_of_sexp: duplicate element in set" el_sexp
          else set := add !set el ~compare_elt);
        assert false)
    | sexp -> of_sexp_error "Set.t_of_sexp: list needed" sexp
  ;;

  let sexp_of_t sexp_of_a t =
    Sexp.List (fold_right t ~init:[] ~f:(fun el acc -> sexp_of_a el :: acc))
  ;;

  module Named = struct
    type nonrec ('a, 'cmp) t =
      { tree : 'a t
      ; name : string
      }

    let is_subset (subset : _ t) ~of_:(superset : _ t) ~sexp_of_elt ~compare_elt =
      let invalid_elements = diff subset.tree superset.tree ~compare_elt in
      if is_empty invalid_elements
      then Ok ()
      else (
        let invalid_elements_sexp = sexp_of_t sexp_of_elt invalid_elements in
        Or_error.error_s
          (Sexp.message
             (subset.name ^ " is not a subset of " ^ superset.name)
             [ "invalid_elements", invalid_elements_sexp ]))
    ;;

    let equal s1 s2 ~sexp_of_elt ~compare_elt =
      Or_error.combine_errors_unit
        [ is_subset s1 ~of_:s2 ~sexp_of_elt ~compare_elt
        ; is_subset s2 ~of_:s1 ~sexp_of_elt ~compare_elt
        ]
    ;;
  end
end

type ('a, 'comparator) t =
  { (* [comparator] is the first field so that polymorphic equality fails on a map due
       to the functional value in the comparator.
       Note that this does not affect polymorphic [compare]: that still produces
       nonsense. *)
    comparator : ('a, 'comparator) Comparator.t
  ; tree : 'a Tree0.t
  }

type ('a, 'comparator) tree = 'a Tree0.t

let like { tree = _; comparator } tree = { tree; comparator }
let compare_elt t = t.comparator.Comparator.compare

module Accessors = struct
  let comparator t = t.comparator
  let invariants t = Tree0.invariants t.tree ~compare_elt:(compare_elt t)
  let length t = Tree0.length t.tree
  let is_empty t = Tree0.is_empty t.tree
  let elements t = Tree0.elements t.tree
  let min_elt t = Tree0.min_elt t.tree
  let min_elt_exn t = Tree0.min_elt_exn t.tree
  let max_elt t = Tree0.max_elt t.tree
  let max_elt_exn t = Tree0.max_elt_exn t.tree
  let choose t = Tree0.choose t.tree
  let choose_exn t = Tree0.choose_exn t.tree
  let to_list t = Tree0.to_list t.tree
  let to_array t = Tree0.to_array t.tree
  let fold t ~init ~f = Tree0.fold t.tree ~init ~f
  let fold_until t ~init ~f = Tree0.fold_until t.tree ~init ~f
  let fold_right t ~init ~f = Tree0.fold_right t.tree ~init ~f
  let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
  let iter t ~f = Tree0.iter t.tree ~f
  let iter2 a b ~f = Tree0.iter2 a.tree b.tree ~f ~compare_elt:(compare_elt a)
  let exists t ~f = Tree0.exists t.tree ~f
  let for_all t ~f = Tree0.for_all t.tree ~f
  let count t ~f = Tree0.count t.tree ~f
  let sum m t ~f = Tree0.sum m t.tree ~f
  let find t ~f = Tree0.find t.tree ~f
  let find_exn t ~f = Tree0.find_exn t.tree ~f
  let find_map t ~f = Tree0.find_map t.tree ~f
  let mem t a = Tree0.mem t.tree a ~compare_elt:(compare_elt t)
  let filter t ~f = like t (Tree0.filter t.tree ~f ~compare_elt:(compare_elt t))
  let add t a = like t (Tree0.add t.tree a ~compare_elt:(compare_elt t))
  let remove t a = like t (Tree0.remove t.tree a ~compare_elt:(compare_elt t))
  let union t1 t2 = like t1 (Tree0.union t1.tree t2.tree ~compare_elt:(compare_elt t1))
  let inter t1 t2 = like t1 (Tree0.inter t1.tree t2.tree ~compare_elt:(compare_elt t1))
  let diff t1 t2 = like t1 (Tree0.diff t1.tree t2.tree ~compare_elt:(compare_elt t1))

  let symmetric_diff t1 t2 =
    Tree0.symmetric_diff t1.tree t2.tree ~compare_elt:(compare_elt t1)
  ;;

  let compare_direct t1 t2 = Tree0.compare (compare_elt t1) t1.tree t2.tree
  let equal t1 t2 = Tree0.equal t1.tree t2.tree ~compare_elt:(compare_elt t1)

  let is_subset t ~of_ =
    Tree0.is_subset t.tree ~of_:of_.tree ~compare_elt:(compare_elt t)
  ;;

  let are_disjoint t1 t2 =
    Tree0.are_disjoint t1.tree t2.tree ~compare_elt:(compare_elt t1)
  ;;

  module Named = struct
    type nonrec ('a, 'cmp) t =
      { set : ('a, 'cmp) t
      ; name : string
      }

    let to_named_tree { set; name } = { Tree0.Named.tree = set.tree; name }

    let is_subset (subset : (_, _) t) ~of_:(superset : (_, _) t) =
      Tree0.Named.is_subset
        (to_named_tree subset)
        ~of_:(to_named_tree superset)
        ~compare_elt:(compare_elt subset.set)
        ~sexp_of_elt:subset.set.comparator.sexp_of_t
    ;;

    let equal t1 t2 =
      Or_error.combine_errors_unit [ is_subset t1 ~of_:t2; is_subset t2 ~of_:t1 ]
    ;;
  end

  let partition_tf t ~f =
    let tree_t, tree_f = Tree0.partition_tf t.tree ~f ~compare_elt:(compare_elt t) in
    like t tree_t, like t tree_f
  ;;

  let split t a =
    let tree1, b, tree2 = Tree0.split t.tree a ~compare_elt:(compare_elt t) in
    like t tree1, b, like t tree2
  ;;

  let group_by t ~equiv =
    List.map (Tree0.group_by t.tree ~equiv ~compare_elt:(compare_elt t)) ~f:(like t)
  ;;

  let nth t i = Tree0.nth t.tree i

  let remove_index t i =
    like t (Tree0.remove_index t.tree i ~compare_elt:(compare_elt t))
  ;;

  let sexp_of_t sexp_of_a _ t = Tree0.sexp_of_t sexp_of_a t.tree

  let to_sequence ?order ?greater_or_equal_to ?less_or_equal_to t =
    Tree0.to_sequence t.comparator ?order ?greater_or_equal_to ?less_or_equal_to t.tree
  ;;

  let binary_search t ~compare how v = Tree0.binary_search t.tree ~compare how v

  let binary_search_segmented t ~segment_of how =
    Tree0.binary_search_segmented t.tree ~segment_of how
  ;;

  let merge_to_sequence ?order ?greater_or_equal_to ?less_or_equal_to t t' =
    Tree0.merge_to_sequence
      t.comparator
      ?order
      ?greater_or_equal_to
      ?less_or_equal_to
      t.tree
      t'.tree
  ;;

  let hash_fold_direct hash_fold_key state t =
    Tree0.hash_fold_t_ignoring_structure hash_fold_key state t.tree
  ;;
end

include Accessors

let compare _ _ t1 t2 = compare_direct t1 t2

module Tree = struct
  type ('a, 'comparator) t = ('a, 'comparator) tree

  let ce comparator = comparator.Comparator.compare

  let t_of_sexp_direct ~comparator a_of_sexp sexp =
    Tree0.t_of_sexp_direct ~compare_elt:(ce comparator) a_of_sexp sexp
  ;;

  let empty_without_value_restriction = Tree0.empty
  let empty ~comparator:_ = empty_without_value_restriction
  let singleton ~comparator:_ e = Tree0.singleton e
  let length t = Tree0.length t
  let invariants ~comparator t = Tree0.invariants t ~compare_elt:(ce comparator)
  let is_empty t = Tree0.is_empty t
  let elements t = Tree0.elements t
  let min_elt t = Tree0.min_elt t
  let min_elt_exn t = Tree0.min_elt_exn t
  let max_elt t = Tree0.max_elt t
  let max_elt_exn t = Tree0.max_elt_exn t
  let choose t = Tree0.choose t
  let choose_exn t = Tree0.choose_exn t
  let to_list t = Tree0.to_list t
  let to_array t = Tree0.to_array t
  let iter t ~f = Tree0.iter t ~f
  let exists t ~f = Tree0.exists t ~f
  let for_all t ~f = Tree0.for_all t ~f
  let count t ~f = Tree0.count t ~f
  let sum m t ~f = Tree0.sum m t ~f
  let find t ~f = Tree0.find t ~f
  let find_exn t ~f = Tree0.find_exn t ~f
  let find_map t ~f = Tree0.find_map t ~f
  let fold t ~init ~f = Tree0.fold t ~init ~f
  let fold_until t ~init ~f = Tree0.fold_until t ~init ~f
  let fold_right t ~init ~f = Tree0.fold_right t ~init ~f
  let map ~comparator t ~f = Tree0.map t ~f ~compare_elt:(ce comparator)
  let filter ~comparator t ~f = Tree0.filter t ~f ~compare_elt:(ce comparator)
  let filter_map ~comparator t ~f = Tree0.filter_map t ~f ~compare_elt:(ce comparator)

  let partition_tf ~comparator t ~f =
    Tree0.partition_tf t ~f ~compare_elt:(ce comparator)
  ;;

  let iter2 ~comparator a b ~f = Tree0.iter2 a b ~f ~compare_elt:(ce comparator)
  let mem ~comparator t a = Tree0.mem t a ~compare_elt:(ce comparator)
  let add ~comparator t a = Tree0.add t a ~compare_elt:(ce comparator)
  let remove ~comparator t a = Tree0.remove t a ~compare_elt:(ce comparator)
  let union ~comparator t1 t2 = Tree0.union t1 t2 ~compare_elt:(ce comparator)
  let inter ~comparator t1 t2 = Tree0.inter t1 t2 ~compare_elt:(ce comparator)
  let diff ~comparator t1 t2 = Tree0.diff t1 t2 ~compare_elt:(ce comparator)

  let symmetric_diff ~comparator t1 t2 =
    Tree0.symmetric_diff t1 t2 ~compare_elt:(ce comparator)
  ;;

  let compare_direct ~comparator t1 t2 = Tree0.compare (ce comparator) t1 t2
  let equal ~comparator t1 t2 = Tree0.equal t1 t2 ~compare_elt:(ce comparator)
  let is_subset ~comparator t ~of_ = Tree0.is_subset t ~of_ ~compare_elt:(ce comparator)

  let are_disjoint ~comparator t1 t2 =
    Tree0.are_disjoint t1 t2 ~compare_elt:(ce comparator)
  ;;

  let of_list ~comparator l = Tree0.of_list l ~compare_elt:(ce comparator)
  let of_array ~comparator a = Tree0.of_array a ~compare_elt:(ce comparator)

  let of_sorted_array_unchecked ~comparator a =
    Tree0.of_sorted_array_unchecked a ~compare_elt:(ce comparator)
  ;;

  let of_increasing_iterator_unchecked ~comparator:_ ~len ~f =
    Tree0.of_increasing_iterator_unchecked ~len ~f
  ;;

  let of_sorted_array ~comparator a =
    Tree0.of_sorted_array a ~compare_elt:(ce comparator)
  ;;

  let union_list ~comparator l = Tree0.union_list l ~to_tree:Fn.id ~comparator

  let stable_dedup_list ~comparator xs =
    Tree0.stable_dedup_list xs ~compare_elt:(ce comparator)
  ;;

  let group_by ~comparator t ~equiv =
    Tree0.group_by t ~equiv ~compare_elt:(ce comparator)
  ;;

  let split ~comparator t a = Tree0.split t a ~compare_elt:(ce comparator)
  let nth t i = Tree0.nth t i
  let remove_index ~comparator t i = Tree0.remove_index t i ~compare_elt:(ce comparator)
  let sexp_of_t sexp_of_a _ t = Tree0.sexp_of_t sexp_of_a t
  let to_tree t = t
  let of_tree ~comparator:_ t = t

  let to_sequence ~comparator ?order ?greater_or_equal_to ?less_or_equal_to t =
    Tree0.to_sequence comparator ?order ?greater_or_equal_to ?less_or_equal_to t
  ;;

  let binary_search ~comparator:_ t ~compare how v = Tree0.binary_search t ~compare how v

  let binary_search_segmented ~comparator:_ t ~segment_of how =
    Tree0.binary_search_segmented t ~segment_of how
  ;;

  let merge_to_sequence ~comparator ?order ?greater_or_equal_to ?less_or_equal_to t t' =
    Tree0.merge_to_sequence comparator ?order ?greater_or_equal_to ?less_or_equal_to t t'
  ;;

  let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t

  module Named = struct
    include Tree0.Named

    let is_subset ~comparator t1 ~of_:t2 =
      Tree0.Named.is_subset
        t1
        ~of_:t2
        ~compare_elt:(ce comparator)
        ~sexp_of_elt:comparator.Comparator.sexp_of_t
    ;;

    let equal ~comparator t1 t2 =
      Tree0.Named.equal
        t1
        t2
        ~compare_elt:(ce comparator)
        ~sexp_of_elt:comparator.Comparator.sexp_of_t
    ;;
  end
end

module Using_comparator = struct
  type nonrec ('elt, 'cmp) t = ('elt, 'cmp) t

  include Accessors

  let to_tree t = t.tree
  let of_tree ~comparator tree = { comparator; tree }

  let t_of_sexp_direct ~comparator a_of_sexp sexp =
    of_tree
      ~comparator
      (Tree0.t_of_sexp_direct ~compare_elt:comparator.compare a_of_sexp sexp)
  ;;

  let empty ~comparator = { comparator; tree = Tree0.empty }

  module Empty_without_value_restriction (Elt : Comparator.S1) = struct
    let empty = { comparator = Elt.comparator; tree = Tree0.empty }
  end

  let singleton ~comparator e = { comparator; tree = Tree0.singleton e }

  let union_list ~comparator l =
    of_tree ~comparator (Tree0.union_list ~comparator ~to_tree l)
  ;;

  let of_sorted_array_unchecked ~comparator array =
    let tree =
      Tree0.of_sorted_array_unchecked array ~compare_elt:comparator.Comparator.compare
    in
    { comparator; tree }
  ;;

  let of_increasing_iterator_unchecked ~comparator ~len ~f =
    of_tree ~comparator (Tree0.of_increasing_iterator_unchecked ~len ~f)
  ;;

  let of_sorted_array ~comparator array =
    Or_error.Monad_infix.(
      Tree0.of_sorted_array array ~compare_elt:comparator.Comparator.compare
      >>| fun tree -> { comparator; tree })
  ;;

  let of_list ~comparator l =
    { comparator; tree = Tree0.of_list l ~compare_elt:comparator.Comparator.compare }
  ;;

  let of_array ~comparator a =
    { comparator; tree = Tree0.of_array a ~compare_elt:comparator.Comparator.compare }
  ;;

  let stable_dedup_list ~comparator xs =
    Tree0.stable_dedup_list xs ~compare_elt:comparator.Comparator.compare
  ;;

  let map ~comparator t ~f =
    { comparator; tree = Tree0.map t.tree ~f ~compare_elt:comparator.Comparator.compare }
  ;;

  let filter_map ~comparator t ~f =
    { comparator
    ; tree = Tree0.filter_map t.tree ~f ~compare_elt:comparator.Comparator.compare
    }
  ;;

  module Tree = Tree
end

type ('elt, 'cmp) comparator =
  (module Comparator.S with type t = 'elt and type comparator_witness = 'cmp)

let comparator_s (type k cmp) t : (k, cmp) comparator =
  (module struct
    type t = k
    type comparator_witness = cmp

    let comparator = t.comparator
  end)
;;

let to_comparator (type elt cmp) ((module M) : (elt, cmp) comparator) = M.comparator
let empty m = Using_comparator.empty ~comparator:(to_comparator m)
let singleton m a = Using_comparator.singleton ~comparator:(to_comparator m) a
let union_list m a = Using_comparator.union_list ~comparator:(to_comparator m) a

let of_sorted_array_unchecked m a =
  Using_comparator.of_sorted_array_unchecked ~comparator:(to_comparator m) a
;;

let of_increasing_iterator_unchecked m ~len ~f =
  Using_comparator.of_increasing_iterator_unchecked ~comparator:(to_comparator m) ~len ~f
;;

let of_sorted_array m a =
  Using_comparator.of_sorted_array ~comparator:(to_comparator m) a
;;

let of_list m a = Using_comparator.of_list ~comparator:(to_comparator m) a
let of_array m a = Using_comparator.of_array ~comparator:(to_comparator m) a

let stable_dedup_list m a =
  Using_comparator.stable_dedup_list ~comparator:(to_comparator m) a
;;

let map m a ~f = Using_comparator.map ~comparator:(to_comparator m) a ~f
let filter_map m a ~f = Using_comparator.filter_map ~comparator:(to_comparator m) a ~f

module M (Elt : sig
    type t
    type comparator_witness
  end) =
struct
  type nonrec t = (Elt.t, Elt.comparator_witness) t
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

let sexp_of_m__t (type elt) (module Elt : Sexp_of_m with type t = elt) t =
  sexp_of_t Elt.sexp_of_t (fun _ -> Sexp.Atom "_") t
;;

let m__t_of_sexp
      (type elt cmp)
      (module Elt : M_of_sexp with type t = elt and type comparator_witness = cmp)
      sexp
  =
  Using_comparator.t_of_sexp_direct ~comparator:Elt.comparator Elt.t_of_sexp sexp
;;

let compare_m__t (module Elt : Compare_m) t1 t2 = compare_direct t1 t2
let equal_m__t (module Elt : Equal_m) t1 t2 = equal t1 t2

let hash_fold_m__t (type elt) (module Elt : Hash_fold_m with type t = elt) state =
  hash_fold_direct Elt.hash_fold_t state
;;

let hash_m__t folder t =
  let state = hash_fold_m__t folder (Hash.create ()) t in
  Hash.get_hash_value state
;;

module Poly = struct
  type comparator_witness = Comparator.Poly.comparator_witness
  type nonrec ('elt, 'cmp) set = ('elt, comparator_witness) t
  type nonrec 'elt t = ('elt, comparator_witness) t
  type nonrec 'elt tree = ('elt, comparator_witness) tree
  type nonrec 'elt named = ('elt, comparator_witness) Named.t

  include Accessors

  let comparator = Comparator.Poly.comparator

  include Using_comparator.Empty_without_value_restriction (Comparator.Poly)

  let singleton a = Using_comparator.singleton ~comparator a
  let union_list a = Using_comparator.union_list ~comparator a

  let of_sorted_array_unchecked a =
    Using_comparator.of_sorted_array_unchecked ~comparator a
  ;;

  let of_increasing_iterator_unchecked ~len ~f =
    Using_comparator.of_increasing_iterator_unchecked ~comparator ~len ~f
  ;;

  let of_sorted_array a = Using_comparator.of_sorted_array ~comparator a
  let of_list a = Using_comparator.of_list ~comparator a
  let of_array a = Using_comparator.of_array ~comparator a
  let stable_dedup_list a = Using_comparator.stable_dedup_list ~comparator a
  let map a ~f = Using_comparator.map ~comparator a ~f
  let filter_map a ~f = Using_comparator.filter_map ~comparator a ~f
  let of_tree tree = { comparator; tree }
  let to_tree t = t.tree
end
