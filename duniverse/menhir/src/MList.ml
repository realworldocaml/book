(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include List

let rec init_aux i n f =
  if n <= i then []
  else
    let r = f i in
    r :: init_aux (i+1) n f

let init n f =
  assert (0 <= n);
  init_aux 0 n f

(** A list subject to a condition. (Be careful, though: the list is
   of course constructed even if the condition is false.) *)
let ifn condition xs =
  if condition then
    xs
  else
    []

(** A list subject to a condition. (Be careful, though: the list is
         of course constructed even if the condition is false.) *)
let if1 condition x =
  if condition then
    [ x ]
  else
    []

(** A cons subject to a condition. *)
let cons_if condition x xs =
  if condition then
    x :: xs
  else
    xs

(** A lazy version of [ifn], where the list is constructed only
         if the condition is true. *)
let ifnlazy condition xs =
  if condition then
    xs()
  else
    []

let sum li =
  fold_left (+) 0 li

let rec at_most k xs =
  match xs with
  | [] ->
      true
  | _ :: xs ->
      k > 0 && at_most (k - 1) xs

let rec drop k xs =
  match k, xs with
  | 0, _
  | _, [] ->
      xs
  | _, _ :: xs ->
      drop (k - 1) xs

let rec take k xs =
  match k, xs with
  | 0, _
  | _, [] ->
      []
  | _, x :: xs ->
      x :: take (k - 1) xs

let leq_join leq_join xs1 xs2 =
  try
    let xs = List.map2 leq_join xs1 xs2 in
    if for_all2 (==) xs2 xs then xs2 else xs
  with Invalid_argument _ ->
    (* The lists have different lengths. *)
    assert false

(** Group equivalent elements of a list. *)
let group_by
    ~(compare:'a -> 'a -> int)
    ~(group:'a -> 'a list -> 'b)
    (list : 'a list) : 'b list
  =
  match List.sort compare list with
  | [] -> []
  | key :: rest ->
    let rec loop acc ks key = function
      | [] -> group key ks :: acc
      | key' :: rest ->
        if compare key key' = 0
        then loop acc (key' :: ks) key rest
        else loop (group key ks :: acc) [] key' rest
    in
    loop [] [] key rest

(** [find_map f xs] applies [f] to elements of [xs] in order and the returns
    the first result of the form [Some y], or [None]. *)
let rec find_map f = function
  | [] -> None
  | x :: xs ->
    match f x with
    | Some _ as result -> result
    | None -> find_map f xs

(** Classify elements of a list in a left and a right list *)
let rec partition_map f = function
  | [] -> ([], [])
  | x :: xs ->
    let y = f x in
    let (ls, rs) = partition_map f xs in
    match y with
    | `L l -> (l :: ls, rs)
    | `R r -> (ls, r :: rs)

(** Compare two lists *)
let rec compare cmp l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | (_ :: _), [] -> 1
  | [], (_ :: _) -> -1
  | (x1 :: xs1), (x2 :: xs2) ->
    match cmp x1 x2 with
    | 0 -> compare cmp xs1 xs2
    | n -> n
