open Import0

let phys_equal = phys_equal

external polymorphic_compare : 'a -> 'a -> int = "%compare"
external polymorphic_equal : 'a -> 'a -> bool = "%equal"
external ( && ) : bool -> bool -> bool = "%sequand"

let compare_abstract ~type_name _ _ =
  Printf.ksprintf
    failwith
    "Compare called on the type %s, which is abstract in an implementation."
    type_name
;;

let equal_abstract ~type_name _ _ =
  Printf.ksprintf
    failwith
    "Equal called on the type %s, which is abstract in an implementation."
    type_name
;;

type 'a compare = 'a -> 'a -> int
type 'a equal = 'a -> 'a -> bool

module Builtin = struct
  let compare_bool : bool compare = Poly.compare
  let compare_char : char compare = Poly.compare
  let compare_float : float compare = Poly.compare
  let compare_int : int compare = Poly.compare
  let compare_int32 : int32 compare = Poly.compare
  let compare_int64 : int64 compare = Poly.compare
  let compare_nativeint : nativeint compare = Poly.compare
  let compare_string : string compare = Poly.compare
  let compare_unit : unit compare = Poly.compare

  let compare_array compare_elt a b =
    if phys_equal a b
    then 0
    else (
      let len_a = Array0.length a in
      let len_b = Array0.length b in
      let ret = compare len_a len_b in
      if ret <> 0
      then ret
      else (
        let rec loop i =
          if i = len_a
          then 0
          else (
            let l = Array0.unsafe_get a i
            and r = Array0.unsafe_get b i in
            let res = compare_elt l r in
            if res <> 0 then res else loop (i + 1))
        in
        loop 0))
  ;;

  let rec compare_list compare_elt a b =
    match a, b with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
      let res = compare_elt x y in
      if res <> 0 then res else compare_list compare_elt xs ys
  ;;

  let compare_option compare_elt a b =
    match a, b with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some a, Some b -> compare_elt a b
  ;;

  let compare_ref compare_elt a b = compare_elt !a !b
  let equal_bool : bool equal = Poly.equal
  let equal_char : char equal = Poly.equal
  let equal_int : int equal = Poly.equal
  let equal_int32 : int32 equal = Poly.equal
  let equal_int64 : int64 equal = Poly.equal
  let equal_nativeint : nativeint equal = Poly.equal
  let equal_string : string equal = Poly.equal
  let equal_unit : unit equal = Poly.equal

  (* [Poly.equal] is IEEE compliant, which is not what we want here. *)
  let equal_float x y = equal_int (compare_float x y) 0

  let equal_array equal_elt a b =
    phys_equal a b
    ||
    let len_a = Array0.length a in
    let len_b = Array0.length b in
    equal len_a len_b
    &&
    let rec loop i =
      i = len_a
      ||
      let l = Array0.unsafe_get a i
      and r = Array0.unsafe_get b i in
      equal_elt l r && loop (i + 1)
    in
    loop 0
  ;;

  let rec equal_list equal_elt a b =
    match a, b with
    | [], [] -> true
    | [], _ | _, [] -> false
    | x :: xs, y :: ys -> equal_elt x y && equal_list equal_elt xs ys
  ;;

  let equal_option equal_elt a b =
    match a, b with
    | None, None -> true
    | None, Some _ | Some _, None -> false
    | Some a, Some b -> equal_elt a b
  ;;

  let equal_ref equal_elt a b = equal_elt !a !b
end
