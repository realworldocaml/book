(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

let cs_lex_compare cs1 cs2 =
  let (s1, s2) = Cstruct.(len cs1, len cs2) in
  let rec go i lim =
    if i = lim then
      compare s1 s2
    else
      match compare (Cstruct.get_uint8 cs1 i) (Cstruct.get_uint8 cs2 i) with
      | 0 -> go (succ i) lim
      | n -> n in
  go 0 (min s1 s2)

type t = int * (int -> Cstruct.t -> unit)

let immediate n f = (n, f)

let len (n, _) = n

let empty = (0, (fun _ _ -> ()))

let (<+>) (l1, w1) (l2, w2) =
  let w off buf =
    ( w1 off buf ; w2 (off + l1) buf ) in
  (l1 + l2, w)

let append = (<+>)

let rec concat = function
  | []    -> empty
  | w::ws -> w <+> concat ws

let of_list lst =
  let open List in
  let w off cs =
    iteri (fun i -> Cstruct.set_uint8 cs (off + i)) lst in
  (length lst, w)

let of_string str =
  let n = String.length str in
  (n, fun off cs -> Cstruct.blit_from_string str 0 cs off n)

let of_cstruct cs' =
  let n = Cstruct.len cs' in
  (n, fun off cs -> Cstruct.blit cs' 0 cs off n)

let of_byte b = (1, fun off cs -> Cstruct.set_uint8 cs off b)

let to_cstruct (n, w) =
  let cs = Cstruct.create n in ( w 0 cs ; cs )

let to_writer (n, w) = (n, fun cs -> w 0 cs)

