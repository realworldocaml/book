(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include Array

let empty =
  [||]

let last a =
  let n = length a in
  assert (n > 0);
  unsafe_get a (n - 1)

let pop a =
  let n = length a in
  assert (n > 0);
  sub a 0 (n - 1)

let push a x =
  let n = length a in
  init (n + 1) (fun i -> if i = n then x else a.(i))

let truncate k a =
  let n = length a in
  if n <= k then
    a
  else
    sub a (n-k) k

let suffix a k =
  truncate k a
    (* TODO keep only one of [truncate] and [suffix] *)

let rec equal_segments equal a1 i1 a2 i2 n =
  n = 0 ||
  equal a1.(i1) a2.(i2) && equal_segments equal a1 (i1 + 1) a2 (i2 + 1) (n - 1)

let is_suffix equal a1 a2 =
  let n1 = length a1
  and n2 = length a2 in
  n1 <= n2 && equal_segments equal a1 0 a2 (n2 - n1) n1

let rec greatest_suffix_forall p a n k =
  if k = n || not (p a.(n - 1 - k)) then
    k
  else
    greatest_suffix_forall p a n (k + 1)

let greatest_suffix_forall p a =
  let k = greatest_suffix_forall p a (length a) 0 in
  truncate k a

let rev a =
  let n = length a in
  if n = 0 then
    a
  else
    let r = make n a.(0) in
    for i = 0 to n - 2 do
      r.(i) <- a.(n - i - 1)
    done;
    r

let rev_of_list xs =
  match xs with
  | [] ->
      [||]
  | x :: xs ->
      let n = 1 + List.length xs in
      let r = make n x in
      List.iteri (fun i x -> r.(n - i - 2) <- x) xs ;
      r

let rev_to_list a =
  fold_left (fun xs x -> x :: xs) [] a

let iter_rev f a =
  for i = length a - 1 downto 0 do
    f a.(i)
  done

let filter p a =
  a |> to_list |> List.filter p |> of_list

let existsi p a =
  let n = length a in
  let rec loop i =
    if i = n then false
    else if p i (unsafe_get a i) then true
    else loop (succ i) in
  loop 0

let count p a =
  let n = length a in
  let c = ref 0 in
  for i = 0 to n-1 do
    if p (unsafe_get a i) then c := !c + 1
  done;
  !c

(* To keep compatibility with OCaml 4.02, we copy [Array.for_all],
   which appeared in 4.03. *)

let for_all p a =
  let n = length a in
  let rec loop i =
    if i = n then true
    else if p (unsafe_get a i) then loop (succ i)
    else false in
  loop 0

(* Similarly, we copy [Array.for_all2], which appeared in 4.11. *)

let for_all2 p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Array.for_all2"
  else let rec loop i =
    if i = n1 then true
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then loop (succ i)
    else false in
  loop 0

let fold_left2 f accu a1 a2 =
  let n1 = length a1
  and n2 = length a2 in
  if n1 <> n2 then invalid_arg "Array.fold_left2";
  let accu = ref accu in
  for i = 0 to n1 - 1 do
    accu := f !accu (unsafe_get a1 i) (unsafe_get a2 i)
  done;
  !accu

let leq_join leq_join a1 a2 =
  let n = length a1 in
  assert (n = length a2);
  let a = init n (fun i -> leq_join (unsafe_get a1 i) (unsafe_get a2 i)) in
  if for_all2 (==) a2 a then a2 else a

let rec findi f i arr =
  if i >= Array.length arr then
    raise Not_found
  else if f i arr.(i) then
    i
  else
    findi f (i + 1) arr

let test () =
  assert (pop [|1; 2; 3; 4|] = [|1; 2; 3|]) ;
  assert (push [|1; 2; 3|] 4 = [|1; 2; 3; 4|]) ;
  assert (truncate 2 [|1; 2; 3; 4|] = [|3; 4|]) ;
  assert (truncate 4 [|1; 2|] = [|1; 2|]) ;
  assert (is_suffix (=) [||] [||]) ;
  assert (is_suffix (=) [||] [|0;3;4|]) ;
  assert (is_suffix (=) [|2|] [|0;2|]) ;
  assert (is_suffix (=) [|3; 4|] [|0;3;4|]) ;
  assert (greatest_suffix_forall ((<) 4) [|1; 2; 3; 4|] = [||]) ;
  assert (greatest_suffix_forall ((<) 2) [|1; 2; 3; 4|] = [|3; 4|]) ;
  assert (greatest_suffix_forall ((<) 0) [|1; 2; 3; 4|] = [|1; 2; 3; 4|]) ;
  assert (greatest_suffix_forall ((<) 0) [|1; 2; 0; 4|] = [|4|]) ;
  assert (rev [|1; 2; 3; 4|] = [|4; 3; 2; 1|]) ;
  assert (rev_of_list [1; 2; 3; 4; 5] = [|5; 4; 3; 2; 1|]) ;
  assert (rev_to_list [|1; 2; 3; 4; 5|] = [5; 4; 3; 2; 1]) ;
  assert (count (fun x -> x mod 2 = 0) [| 1;2;3 |] = 1) ;
  assert (count (fun x -> x mod 2 = 0) [||] = 0) ;
  assert (suffix [|1; 2; 3; 4; 5|] 0 = [||]) ;
  assert (suffix [|1; 2; 3; 4; 5|] 3 = [|3; 4; 5|]) ;
  assert (suffix [|1; 2; 3; 4; 5|] 5 = [|1; 2; 3; 4; 5|]) ;
  ()
