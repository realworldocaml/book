open! Import

(* These functions implement a search for the first (resp. last) element
   satisfying a predicate, assuming that the predicate is increasing on
   the container, meaning that, if the container is [u1...un], there exists a
   k such that p(u1)=....=p(uk) = false and p(uk+1)=....=p(un)= true.
   If this k = 1 (resp n), find_last_not_satisfying (resp find_first_satisfying)
   will return None. *)

let rec linear_search_first_satisfying t ~get ~lo ~hi ~pred =
  if lo > hi
  then None
  else if pred (get t lo)
  then Some lo
  else linear_search_first_satisfying t ~get ~lo:(lo + 1) ~hi ~pred
;;

(* Takes a container [t], a predicate [pred] and two indices [lo < hi], such that
   [pred] is increasing on [t] between [lo] and [hi].

   return a range (lo, hi) where:
   - lo and hi are close enough together for a linear search
   - If [pred] is not constantly [false] on [t] between [lo] and [hi], the first element
     on which [pred] is [true] is between [lo] and [hi]. *)
(* Invariant: the first element satisfying [pred], if it exists is between [lo] and [hi] *)
let rec find_range_near_first_satisfying t ~get ~lo ~hi ~pred =
  (* Warning: this function will not terminate if the constant (currently 8) is
     set <= 1 *)
  if hi - lo <= 8
  then lo, hi
  else (
    let mid = lo + ((hi - lo) / 2) in
    if pred (get t mid)
    (* INVARIANT check: it means the first satisfying element is between [lo] and [mid] *)
    then
      find_range_near_first_satisfying t ~get ~lo ~hi:mid ~pred
      (* INVARIANT check: it means the first satisfying element, if it exists,
         is between [mid+1] and [hi] *)
    else find_range_near_first_satisfying t ~get ~lo:(mid + 1) ~hi ~pred)
;;

let find_first_satisfying ?pos ?len t ~get ~length ~pred =
  let pos, len =
    Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length t)
  in
  let lo = pos in
  let hi = pos + len - 1 in
  let lo, hi = find_range_near_first_satisfying t ~get ~lo ~hi ~pred in
  linear_search_first_satisfying t ~get ~lo ~hi ~pred
;;

(* Takes an array with shape [true,...true,false,...false] (i.e., the _reverse_ of what
   is described above) and returns the index of the last true or None if there are no
   true*)
let find_last_satisfying ?pos ?len t ~pred ~get ~length =
  let pos, len =
    Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length t)
  in
  if len = 0
  then None
  else (
    (* The last satisfying is the one just before the first not satisfying *)
    match find_first_satisfying ~pos ~len t ~get ~length ~pred:(Fn.non pred) with
    | None -> Some (pos + len - 1)
    (* This means that all elements satisfy pred.
       There is at least an element as (len > 0) *)
    | Some i when i = pos -> None (* no element satisfies pred *)
    | Some i -> Some (i - 1))
;;

let binary_search ?pos ?len t ~length ~get ~compare how v =
  match how with
  | `Last_strictly_less_than ->
    find_last_satisfying ?pos ?len t ~get ~length ~pred:(fun x -> compare x v < 0)
  | `Last_less_than_or_equal_to ->
    find_last_satisfying ?pos ?len t ~get ~length ~pred:(fun x -> compare x v <= 0)
  | `First_equal_to ->
    (match
       find_first_satisfying ?pos ?len t ~get ~length ~pred:(fun x -> compare x v >= 0)
     with
     | Some x when compare (get t x) v = 0 -> Some x
     | None | Some _ -> None)
  | `Last_equal_to ->
    (match
       find_last_satisfying ?pos ?len t ~get ~length ~pred:(fun x -> compare x v <= 0)
     with
     | Some x when compare (get t x) v = 0 -> Some x
     | None | Some _ -> None)
  | `First_greater_than_or_equal_to ->
    find_first_satisfying ?pos ?len t ~get ~length ~pred:(fun x -> compare x v >= 0)
  | `First_strictly_greater_than ->
    find_first_satisfying ?pos ?len t ~get ~length ~pred:(fun x -> compare x v > 0)
;;

let binary_search_segmented ?pos ?len t ~length ~get ~segment_of how =
  let is_left x =
    match segment_of x with
    | `Left -> true
    | `Right -> false
  in
  let is_right x = not (is_left x) in
  match how with
  | `Last_on_left -> find_last_satisfying ?pos ?len t ~length ~get ~pred:is_left
  | `First_on_right -> find_first_satisfying ?pos ?len t ~length ~get ~pred:is_right
;;
