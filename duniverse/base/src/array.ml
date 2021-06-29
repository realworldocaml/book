open! Import
include Array0
module Int = Int0

type 'a t = 'a array [@@deriving_inline compare, sexp, sexp_grammar]

let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int = compare_array

let t_of_sexp : 'a. (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> Ppx_sexp_conv_lib.Sexp.t -> 'a t =
  array_of_sexp
;;

let sexp_of_t : 'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t =
  sexp_of_array
;;

let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
  let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
    { implicit_vars = [ "array" ]
    ; ggid = "j\132);\135qH\158\135\222H\001\007\004\158\218"
    ; types = [ "t", Explicit_bind ([ "a" ], Apply (Implicit_var 0, [ Explicit_var 0 ])) ]
    }
  in
  let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
    { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
    ; apply_implicit = [ array_sexp_grammar ]
    ; generic_group = _the_generic_group
    ; origin = "array.ml"
    }
  in
  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    Ref ("t", _the_group)
  in
  t_sexp_grammar
;;

[@@@end]

(* This module implements a new in-place, constant heap sorting algorithm to replace the
   one used by the standard libraries.  Its only purpose is to be faster (hopefully
   strictly faster) than the base sort and stable_sort.

   At a high level the algorithm is:
   - pick two pivot points by:
   - pick 5 arbitrary elements from the array
   - sort them within the array
   - take the elements on either side of the middle element of the sort as the pivots
   - sort the array with:
   - all elements less than pivot1 to the left (range 1)
   - all elements >= pivot1 and <= pivot2 in the middle (range 2)
   - all elements > pivot2 to the right (range 3)
   - if pivot1 and pivot2 are equal, then the middle range is sorted, so ignore it
   - recurse into range 1, 2 (if pivot1 and pivot2 are unequal), and 3
   - during recursion there are two inflection points:
   - if the size of the current range is small, use insertion sort to sort it
   - if the stack depth is large, sort the range with heap-sort to avoid n^2 worst-case
     behavior

   See the following for more information:
   - "Dual-Pivot Quicksort" by Vladimir Yaroslavskiy.
     Available at
     http://www.kriche.com.ar/root/programming/spaceTimeComplexity/DualPivotQuicksort.pdf
   - "Quicksort is Optimal" by Sedgewick and Bentley.
     Slides at http://www.cs.princeton.edu/~rs/talks/QuicksortIsOptimal.pdf
   - http://www.sorting-algorithms.com/quick-sort-3-way *)

module Sort = struct
  (* For the sake of speed we could use unsafe get/set throughout, but speed tests don't
     show a significant improvement. *)
  let get = get
  let set = set

  let swap arr i j =
    let tmp = get arr i in
    set arr i (get arr j);
    set arr j tmp
  ;;

  module type Sort = sig
    val sort
      :  'a t
      -> compare:('a -> 'a -> int)
      -> left:int (* leftmost index of sub-array to sort *)
      -> right:int (* rightmost index of sub-array to sort *)
      -> unit
  end

  (* http://en.wikipedia.org/wiki/Insertion_sort *)
  module Insertion_sort : Sort = struct
    let sort arr ~compare ~left ~right =
      (* loop invariant:
         [arr] is sorted from [left] to [pos - 1], inclusive *)
      for pos = left + 1 to right do
        (* loop invariants:
           1.  the subarray arr[left .. i-1] is sorted
           2.  the subarray arr[i+1 .. pos] is sorted and contains only elements > v
           3.  arr[i] may be thought of as containing v

           Note that this does not allocate a closure, but is left in the for
           loop for the readability of the documentation. *)
        let rec loop arr ~left ~compare i v =
          let i_next = i - 1 in
          if i_next >= left && compare (get arr i_next) v > 0
          then (
            set arr i (get arr i_next);
            loop arr ~left ~compare i_next v)
          else i
        in
        let v = get arr pos in
        let final_pos = loop arr ~left ~compare pos v in
        set arr final_pos v
      done
    ;;
  end

  (* http://en.wikipedia.org/wiki/Heapsort *)
  module Heap_sort : Sort = struct
    (* loop invariant:
       root's children are both either roots of max-heaps or > right *)
    let rec heapify arr ~compare root ~left ~right =
      let relative_root = root - left in
      let left_child = (2 * relative_root) + left + 1 in
      let right_child = (2 * relative_root) + left + 2 in
      let largest =
        if left_child <= right && compare (get arr left_child) (get arr root) > 0
        then left_child
        else root
      in
      let largest =
        if right_child <= right && compare (get arr right_child) (get arr largest) > 0
        then right_child
        else largest
      in
      if largest <> root
      then (
        swap arr root largest;
        heapify arr ~compare largest ~left ~right)
    ;;

    let build_heap arr ~compare ~left ~right =
      (* Elements in the second half of the array are already heaps of size 1.  We move
         through the first half of the array from back to front examining the element at
         hand, and the left and right children, fixing the heap property as we go. *)
      for i = (left + right) / 2 downto left do
        heapify arr ~compare i ~left ~right
      done
    ;;

    let sort arr ~compare ~left ~right =
      build_heap arr ~compare ~left ~right;
      (* loop invariants:
         1.  the subarray arr[left ... i] is a max-heap H
         2.  the subarray arr[i+1 ... right] is sorted (call it S)
         3.  every element of H is less than every element of S *)
      for i = right downto left + 1 do
        swap arr left i;
        heapify arr ~compare left ~left ~right:(i - 1)
      done
    ;;
  end

  (* http://en.wikipedia.org/wiki/Introsort *)
  module Intro_sort : sig
    include Sort

    val five_element_sort
      :  'a t
      -> compare:('a -> 'a -> int)
      -> int
      -> int
      -> int
      -> int
      -> int
      -> unit
  end = struct
    let five_element_sort arr ~compare m1 m2 m3 m4 m5 =
      let compare_and_swap i j =
        if compare (get arr i) (get arr j) > 0 then swap arr i j
      in
      (* Optimal 5-element sorting network:

         {v
            1--o-----o-----o--------------1
               |     |     |
            2--o-----|--o--|-----o--o-----2
                     |  |  |     |  |
            3--------o--o--|--o--|--o-----3
                           |  |  |
            4-----o--------o--o--|-----o--4
                  |              |     |
            5-----o--------------o-----o--5
          v} *)
      compare_and_swap m1 m2;
      compare_and_swap m4 m5;
      compare_and_swap m1 m3;
      compare_and_swap m2 m3;
      compare_and_swap m1 m4;
      compare_and_swap m3 m4;
      compare_and_swap m2 m5;
      compare_and_swap m2 m3;
      compare_and_swap m4 m5
    ;;

    (* choose pivots for the array by sorting 5 elements and examining the center three
       elements.  The goal is to choose two pivots that will either:
       - break the range up into 3 even partitions
         or
       - eliminate a commonly appearing element by sorting it into the center partition
         by itself
         To this end we look at the center 3 elements of the 5 and return pairs of equal
         elements or the widest range *)
    let choose_pivots arr ~compare ~left ~right =
      let sixth = (right - left) / 6 in
      let m1 = left + sixth in
      let m2 = m1 + sixth in
      let m3 = m2 + sixth in
      let m4 = m3 + sixth in
      let m5 = m4 + sixth in
      five_element_sort arr ~compare m1 m2 m3 m4 m5;
      let m2_val = get arr m2 in
      let m3_val = get arr m3 in
      let m4_val = get arr m4 in
      if compare m2_val m3_val = 0
      then m2_val, m3_val, true
      else if compare m3_val m4_val = 0
      then m3_val, m4_val, true
      else m2_val, m4_val, false
    ;;

    let dual_pivot_partition arr ~compare ~left ~right =
      let pivot1, pivot2, pivots_equal = choose_pivots arr ~compare ~left ~right in
      (* loop invariants:
         1.  left <= l < r <= right
         2.  l <= p <= r
         3.  l <= x < p     implies arr[x] >= pivot1
         and arr[x] <= pivot2
         4.  left <= x < l  implies arr[x] < pivot1
         5.  r < x <= right implies arr[x] > pivot2 *)
      let rec loop l p r =
        let pv = get arr p in
        if compare pv pivot1 < 0
        then (
          swap arr p l;
          cont (l + 1) (p + 1) r)
        else if compare pv pivot2 > 0
        then (
          (* loop invariants:  same as those of the outer loop *)
          let rec scan_backwards r =
            if r > p && compare (get arr r) pivot2 > 0 then scan_backwards (r - 1) else r
          in
          let r = scan_backwards r in
          swap arr r p;
          cont l p (r - 1))
        else cont l (p + 1) r
      and cont l p r = if p > r then l, r else loop l p r in
      let l, r = cont left left right in
      l, r, pivots_equal
    ;;

    let rec intro_sort arr ~max_depth ~compare ~left ~right =
      let len = right - left + 1 in
      (* This takes care of some edge cases, such as left > right or very short arrays,
         since Insertion_sort.sort handles these cases properly.  Thus we don't need to
         make sure that left and right are valid in recursive calls. *)
      if len <= 32
      then Insertion_sort.sort arr ~compare ~left ~right
      else if max_depth < 0
      then Heap_sort.sort arr ~compare ~left ~right
      else (
        let max_depth = max_depth - 1 in
        let l, r, middle_sorted = dual_pivot_partition arr ~compare ~left ~right in
        intro_sort arr ~max_depth ~compare ~left ~right:(l - 1);
        if not middle_sorted then intro_sort arr ~max_depth ~compare ~left:l ~right:r;
        intro_sort arr ~max_depth ~compare ~left:(r + 1) ~right)
    ;;

    let log10_of_3 = Caml.log10 3.
    let log3 x = Caml.log10 x /. log10_of_3

    let sort arr ~compare ~left ~right =
      let len = right - left + 1 in
      let heap_sort_switch_depth =
        (* with perfect 3-way partitioning, this is the recursion depth *)
        Int.of_float (log3 (Int.to_float len))
      in
      intro_sort arr ~max_depth:heap_sort_switch_depth ~compare ~left ~right
    ;;
  end
end

let sort ?pos ?len arr ~compare =
  let pos, len =
    Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length arr)
  in
  Sort.Intro_sort.sort arr ~compare ~left:pos ~right:(pos + len - 1)
;;

let to_array t = t
let is_empty t = length t = 0

let is_sorted t ~compare =
  let rec is_sorted_loop t ~compare i =
    if i < 1
    then true
    else compare t.(i - 1) t.(i) <= 0 && is_sorted_loop t ~compare (i - 1)
  in
  is_sorted_loop t ~compare (length t - 1)
;;

let is_sorted_strictly t ~compare =
  let rec is_sorted_strictly_loop t ~compare i =
    if i < 1
    then true
    else compare t.(i - 1) t.(i) < 0 && is_sorted_strictly_loop t ~compare (i - 1)
  in
  is_sorted_strictly_loop t ~compare (length t - 1)
;;

let folding_map t ~init ~f =
  let acc = ref init in
  map t ~f:(fun x ->
    let new_acc, y = f !acc x in
    acc := new_acc;
    y)
;;

let fold_map t ~init ~f =
  let acc = ref init in
  let result =
    map t ~f:(fun x ->
      let new_acc, y = f !acc x in
      acc := new_acc;
      y)
  in
  !acc, result
;;

let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
let fold_until t ~init ~f = Container.fold_until ~fold ~init ~f t
let count t ~f = Container.count ~fold t ~f
let sum m t ~f = Container.sum ~fold m t ~f
let min_elt t ~compare = Container.min_elt ~fold t ~compare
let max_elt t ~compare = Container.max_elt ~fold t ~compare

let foldi t ~init ~f =
  let rec foldi_loop t i ac ~f =
    if i = length t then ac else foldi_loop t (i + 1) (f i ac t.(i)) ~f
  in
  foldi_loop t 0 init ~f
;;

let folding_mapi t ~init ~f =
  let acc = ref init in
  mapi t ~f:(fun i x ->
    let new_acc, y = f i !acc x in
    acc := new_acc;
    y)
;;

let fold_mapi t ~init ~f =
  let acc = ref init in
  let result =
    mapi t ~f:(fun i x ->
      let new_acc, y = f i !acc x in
      acc := new_acc;
      y)
  in
  !acc, result
;;

let counti t ~f =
  foldi t ~init:0 ~f:(fun idx count a -> if f idx a then count + 1 else count)
;;

let concat_map t ~f = concat (to_list (map ~f t))
let concat_mapi t ~f = concat (to_list (mapi ~f t))

let rev_inplace t =
  let i = ref 0 in
  let j = ref (length t - 1) in
  while !i < !j do
    swap t !i !j;
    incr i;
    decr j
  done
;;

let of_list_rev l =
  match l with
  | [] -> [||]
  | a :: l ->
    let len = 1 + List.length l in
    let t = create ~len a in
    let r = ref l in
    (* We start at [len - 2] because we already put [a] at [t.(len - 1)]. *)
    for i = len - 2 downto 0 do
      match !r with
      | [] -> assert false
      | a :: l ->
        t.(i) <- a;
        r := l
    done;
    t
;;

(* [of_list_map] and [of_list_rev_map] are based on functions from the OCaml
   distribution. *)

let of_list_map xs ~f =
  match xs with
  | [] -> [||]
  | hd :: tl ->
    let a = create ~len:(1 + List.length tl) (f hd) in
    let rec fill i = function
      | [] -> a
      | hd :: tl ->
        unsafe_set a i (f hd);
        fill (i + 1) tl
    in
    fill 1 tl
;;

let of_list_mapi xs ~f =
  match xs with
  | [] -> [||]
  | hd :: tl ->
    let a = create ~len:(1 + List.length tl) (f 0 hd) in
    let rec fill a i = function
      | [] -> a
      | hd :: tl ->
        unsafe_set a i (f i hd);
        fill a (i + 1) tl
    in
    fill a 1 tl
;;

let of_list_rev_map xs ~f =
  let t = of_list_map xs ~f in
  rev_inplace t;
  t
;;

let of_list_rev_mapi xs ~f =
  let t = of_list_mapi xs ~f in
  rev_inplace t;
  t
;;

let filter_mapi t ~f =
  let r = ref [||] in
  let k = ref 0 in
  for i = 0 to length t - 1 do
    match f i (unsafe_get t i) with
    | None -> ()
    | Some a ->
      if !k = 0 then r := create ~len:(length t) a;
      unsafe_set !r !k a;
      incr k
  done;
  if !k = length t then !r else if !k > 0 then sub ~pos:0 ~len:!k !r else [||]
;;

let filter_map t ~f = filter_mapi t ~f:(fun _i a -> f a)
let filter_opt t = filter_map t ~f:Fn.id

let raise_length_mismatch name n1 n2 =
  invalid_argf "length mismatch in %s: %d <> %d" name n1 n2 ()
[@@cold] [@@inline never] [@@local never] [@@specialise never]
;;

let check_length2_exn name t1 t2 =
  let n1 = length t1 in
  let n2 = length t2 in
  if n1 <> n2 then raise_length_mismatch name n1 n2
;;

let iter2_exn t1 t2 ~f =
  check_length2_exn "Array.iter2_exn" t1 t2;
  iteri t1 ~f:(fun i x1 -> f x1 t2.(i))
;;

let map2_exn t1 t2 ~f =
  check_length2_exn "Array.map2_exn" t1 t2;
  init (length t1) ~f:(fun i -> f t1.(i) t2.(i))
;;

let fold2_exn t1 t2 ~init ~f =
  check_length2_exn "Array.fold2_exn" t1 t2;
  foldi t1 ~init ~f:(fun i ac x -> f ac x t2.(i))
;;

let filter t ~f = filter_map t ~f:(fun x -> if f x then Some x else None)
let filteri t ~f = filter_mapi t ~f:(fun i x -> if f i x then Some x else None)

let exists t ~f =
  let rec exists_loop t ~f i =
    if i < 0 then false else f t.(i) || exists_loop t ~f (i - 1)
  in
  exists_loop t ~f (length t - 1)
;;

let existsi t ~f =
  let rec existsi_loop t ~f i =
    if i < 0 then false else f i t.(i) || existsi_loop t ~f (i - 1)
  in
  existsi_loop t ~f (length t - 1)
;;

let mem t a ~equal = exists t ~f:(equal a)

let for_all t ~f =
  let rec for_all_loop t ~f i =
    if i < 0 then true else f t.(i) && for_all_loop t ~f (i - 1)
  in
  for_all_loop t ~f (length t - 1)
;;

let for_alli t ~f =
  let rec for_alli_loop t ~f i =
    if i < 0 then true else f i t.(i) && for_alli_loop t ~f (i - 1)
  in
  for_alli_loop t ~f (length t - 1)
;;

let exists2_exn t1 t2 ~f =
  let rec exists2_exn_loop t1 t2 ~f i =
    if i < 0 then false else f t1.(i) t2.(i) || exists2_exn_loop t1 t2 ~f (i - 1)
  in
  check_length2_exn "Array.exists2_exn" t1 t2;
  exists2_exn_loop t1 t2 ~f (length t1 - 1)
;;

let for_all2_exn t1 t2 ~f =
  let rec for_all2_loop t1 t2 ~f i =
    if i < 0 then true else f t1.(i) t2.(i) && for_all2_loop t1 t2 ~f (i - 1)
  in
  check_length2_exn "Array.for_all2_exn" t1 t2;
  for_all2_loop t1 t2 ~f (length t1 - 1)
;;

let equal equal t1 t2 = length t1 = length t2 && for_all2_exn t1 t2 ~f:equal


let map_inplace t ~f =
  for i = 0 to length t - 1 do
    t.(i) <- f t.(i)
  done
;;

let findi t ~f =
  let rec findi_loop t ~f ~length i =
    if i >= length
    then None
    else if f i t.(i)
    then Some (i, t.(i))
    else findi_loop t ~f ~length (i + 1)
  in
  let length = length t in
  findi_loop t ~f ~length 0
;;

let findi_exn =
  let not_found = Not_found_s (Atom "Array.findi_exn: not found") in
  let findi_exn t ~f =
    match findi t ~f with
    | None -> raise not_found
    | Some x -> x
  in
  (* named to preserve symbol in compiled binary *)
  findi_exn
;;

let find_exn =
  let not_found = Not_found_s (Atom "Array.find_exn: not found") in
  let find_exn t ~f =
    match findi t ~f:(fun _i x -> f x) with
    | None -> raise not_found
    | Some (_i, x) -> x
  in
  (* named to preserve symbol in compiled binary *)
  find_exn
;;

let find t ~f = Option.map (findi t ~f:(fun _i x -> f x)) ~f:(fun (_i, x) -> x)

let find_map t ~f =
  let rec find_map_loop t ~f ~length i =
    if i >= length
    then None
    else (
      match f t.(i) with
      | None -> find_map_loop t ~f ~length (i + 1)
      | Some _ as res -> res)
  in
  let length = length t in
  find_map_loop t ~f ~length 0
;;

let find_map_exn =
  let not_found = Not_found_s (Atom "Array.find_map_exn: not found") in
  let find_map_exn t ~f =
    match find_map t ~f with
    | None -> raise not_found
    | Some x -> x
  in
  (* named to preserve symbol in compiled binary *)
  find_map_exn
;;

let find_mapi t ~f =
  let rec find_mapi_loop t ~f ~length i =
    if i >= length
    then None
    else (
      match f i t.(i) with
      | None -> find_mapi_loop t ~f ~length (i + 1)
      | Some _ as res -> res)
  in
  let length = length t in
  find_mapi_loop t ~f ~length 0
;;

let find_mapi_exn =
  let not_found = Not_found_s (Atom "Array.find_mapi_exn: not found") in
  let find_mapi_exn t ~f =
    match find_mapi t ~f with
    | None -> raise not_found
    | Some x -> x
  in
  (* named to preserve symbol in compiled binary *)
  find_mapi_exn
;;

let find_consecutive_duplicate t ~equal =
  let n = length t in
  if n <= 1
  then None
  else (
    let result = ref None in
    let i = ref 1 in
    let prev = ref t.(0) in
    while !i < n do
      let cur = t.(!i) in
      if equal cur !prev
      then (
        result := Some (!prev, cur);
        i := n)
      else (
        prev := cur;
        incr i)
    done;
    !result)
;;

let reduce t ~f =
  if length t = 0
  then None
  else (
    let r = ref t.(0) in
    for i = 1 to length t - 1 do
      r := f !r t.(i)
    done;
    Some !r)
;;

let reduce_exn t ~f =
  match reduce t ~f with
  | None -> invalid_arg "Array.reduce_exn"
  | Some v -> v
;;

let permute = Array_permute.permute

let random_element_exn ?(random_state = Random.State.default) t =
  if is_empty t
  then failwith "Array.random_element_exn: empty array"
  else t.(Random.State.int random_state (length t))
;;

let random_element ?(random_state = Random.State.default) t =
  try Some (random_element_exn ~random_state t) with
  | _ -> None
;;

let zip t1 t2 =
  if length t1 <> length t2 then None else Some (map2_exn t1 t2 ~f:(fun x1 x2 -> x1, x2))
;;

let zip_exn t1 t2 =
  if length t1 <> length t2
  then failwith "Array.zip_exn"
  else map2_exn t1 t2 ~f:(fun x1 x2 -> x1, x2)
;;

let unzip t =
  let n = length t in
  if n = 0
  then [||], [||]
  else (
    let x, y = t.(0) in
    let res1 = create ~len:n x in
    let res2 = create ~len:n y in
    for i = 1 to n - 1 do
      let x, y = t.(i) in
      res1.(i) <- x;
      res2.(i) <- y
    done;
    res1, res2)
;;

let sorted_copy t ~compare =
  let t1 = copy t in
  sort t1 ~compare;
  t1
;;

let partitioni_tf t ~f =
  let both = mapi t ~f:(fun i x -> if f i x then Either.First x else Either.Second x) in
  let trues =
    filter_map both ~f:(function
      | First x -> Some x
      | Second _ -> None)
  in
  let falses =
    filter_map both ~f:(function
      | First _ -> None
      | Second x -> Some x)
  in
  trues, falses
;;

let partition_tf t ~f = partitioni_tf t ~f:(fun _i x -> f x)
let last t = t.(length t - 1)

(* Convert to a sequence but does not attempt to protect against modification
   in the array. *)
let to_sequence_mutable t =
  Sequence.unfold_step ~init:0 ~f:(fun i ->
    if i >= length t then Sequence.Step.Done else Sequence.Step.Yield (t.(i), i + 1))
;;

let to_sequence t = to_sequence_mutable (copy t)

let cartesian_product t1 t2 =
  if is_empty t1 || is_empty t2
  then [||]
  else (
    let n1 = length t1 in
    let n2 = length t2 in
    let t = create ~len:(n1 * n2) (t1.(0), t2.(0)) in
    let r = ref 0 in
    for i1 = 0 to n1 - 1 do
      for i2 = 0 to n2 - 1 do
        t.(!r) <- (t1.(i1), t2.(i2));
        incr r
      done
    done;
    t)
;;

let transpose tt =
  if length tt = 0
  then Some [||]
  else (
    let width = length tt in
    let depth = length tt.(0) in
    if exists tt ~f:(fun t -> length t <> depth)
    then None
    else Some (init depth ~f:(fun d -> init width ~f:(fun w -> tt.(w).(d)))))
;;

let transpose_exn tt =
  match transpose tt with
  | None -> invalid_arg "Array.transpose_exn"
  | Some tt' -> tt'
;;

include Binary_searchable.Make1 (struct
    type nonrec 'a t = 'a t

    let get = get
    let length = length
  end)

include Blit.Make1 (struct
    type nonrec 'a t = 'a t

    let length = length

    let create_like ~len t =
      if len = 0
      then [||]
      else (
        assert (length t > 0);
        create ~len t.(0))
    ;;

    let unsafe_blit = blit
  end)

let invariant invariant_a t = iter t ~f:invariant_a

module Private = struct
  module Sort = Sort
end
