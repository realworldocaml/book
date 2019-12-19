(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Asn_core
module Prim = Asn_prim

let arr_fold_right_i ~f z arr =
  let rec loop r = function
    | -1 -> r
    |  i -> loop (f i arr.(i) r) (pred i) in
  loop z Array.(length arr - 1)

type cls = [ `Universal | `Application | `Private ]

let fix f = Fix (f, Asn_cache.variant ())

let map ?random f g asn = Iso (f, g, random, asn)

let to_tag id = function
  | Some `Application -> Tag.Application id
  | Some `Private     -> Tag.Private id
  | Some `Universal   -> Tag.Universal id
  | None              -> Tag.Context_specific id

let explicit ?cls id asn = Explicit (to_tag id cls, asn)
let implicit : type a. ?cls:cls -> int -> a asn -> a asn =
  fun ?cls id -> function
  | (Choice (_, _)) as asn -> explicit ?cls id asn
  | asn             -> Implicit (to_tag id cls, asn)

let bool                = Prim Bool
and integer             = Prim Int
and octet_string        = Prim Octets
and null                = Prim Null
and oid                 = Prim OID
and character_string    = Prim CharString

let string tag = implicit ~cls:`Universal tag character_string

let utf8_string      = string 0x0c
let numeric_string   = string 0x12
and printable_string = string 0x13
and teletex_string   = string 0x14
and videotex_string  = string 0x15
and ia5_string       = string 0x16
and graphic_string   = string 0x19
and visible_string   = string 0x1a
and general_string   = string 0x1b
and universal_string = string 0x1c
and bmp_string       = string 0x1e

let (utc_time, generalized_time) =
  let open Asn_prim.Time in
  let time ~frac tag (f, g) =
    map ~random:(random ~frac) f g @@
      implicit ~cls:`Universal tag character_string in
  time ~frac:false 0x17 (utc_time_of_string, of_utc_time),
  time ~frac:true  0x18 (gen_time_of_string, of_gen_time)

let int =
  let f n = try Z.to_int n with Z.Overflow ->
    parse_error "INTEGER: int overflow: %a" Z.pp_print n in
  map f Z.of_int integer

let enumerated f g = map f g @@ implicit ~cls:`Universal 0x0a int

let bit_string    = Prim.Bits.(map to_array of_array (Prim Bits))
and bit_string_cs = map snd (fun cs -> (0, cs)) (Prim Bits)

let bit_string_flags (type a) (xs : (int * a) list) =
  let module M = Map.Make (struct type t = a let compare = compare end) in
  let ixs = List.fold_right (fun (i, x) -> M.add x i) xs M.empty in
  let n   = List.fold_right (fun (i, _) -> max (i + 1)) xs 0 in
  let f = match xs with
    | []        -> fun _ -> []
    | (_, x)::_ ->
        let items = Array.make n x in
        xs |> List.iter (fun (i, x) -> items.(i) <- x) ;
        arr_fold_right_i [] ~f:(fun i b rs ->
          if b && i < n then items.(i) :: rs else rs)
  and g es =
    let arr = Array.make n false in
    let register e = try arr.(M.find e ixs) <- true with Not_found -> () in
    List.iter register es;
    arr
  in
  map f g bit_string


let single a   = Last a
and ( @) a b   = Pair (a, b)
and (-@) a b   = Pair (a, Last b)
and optional ?label a = Optional (label, a)
and required ?label a = Required (label, a)

let product2 fn a b = fn @@ a @ single b

let product3 fn a b c =
  map (fun (a, (b, c)) -> (a, b, c))
      (fun (a, b, c) -> (a, (b, c)))
      (fn @@ a @ b @ single c)

let product4 fn a b c d =
  map (fun (a, (b, (c, d))) -> (a, b, c, d))
      (fun (a, b, c, d) -> (a, (b, (c, d))))
      (fn @@ a @ b @ c @ single d)

let product5 fn a b c d e =
  map (fun (a, (b, (c, (d, e)))) -> (a, b, c, d, e))
      (fun (a, b, c, d, e) -> (a, (b, (c, (d, e)))))
      (fn @@ a @ b @ c @ d @ single e)

let product6 fn a b c d e f =
  map (fun (a, (b, (c, (d, (e, f))))) -> (a, b, c, d, e, f))
      (fun (a, b, c, d, e, f) -> (a, (b, (c, (d, (e, f))))))
      (fn @@ a @ b @ c @ d @ e @ single f)


let sequence seq = Sequence seq

let sequence2 a b         = product2 sequence a b
and sequence3 a b c       = product3 sequence a b c
and sequence4 a b c d     = product4 sequence a b c d
and sequence5 a b c d e   = product5 sequence a b c d e
and sequence6 a b c d e f = product6 sequence a b c d e f

let sequence_of asn = Sequence_of asn

let set seq = Set seq

let set2 a b         = product2 set a b
and set3 a b c       = product3 set a b c
and set4 a b c d     = product4 set a b c d
and set5 a b c d e   = product5 set a b c d e
and set6 a b c d e f = product6 set a b c d e f

let set_of asn = Set_of asn

let choice a b = Choice (a, b)

let choice2 a b =
  map (function L a -> `C1 a | R b -> `C2 b)
      (function `C1 a -> L a | `C2 b -> R b)
      (choice a b)

let choice3 a b c =
  map (function L (L a) -> `C1 a | L (R b) -> `C2 b | R c -> `C3 c)
      (function `C1 a -> L (L a) | `C2 b -> L (R b) | `C3 c -> R c)
      (choice (choice a b) c)

let choice4 a b c d =
  map (function | L (L a) -> `C1 a | L (R b) -> `C2 b
                | R (L c) -> `C3 c | R (R d) -> `C4 d)
      (function | `C1 a -> L (L a) | `C2 b -> L (R b)
                | `C3 c -> R (L c) | `C4 d -> R (R d))
      (choice (choice a b) (choice c d))

let choice5 a b c d e =
  map (function | L (L (L a)) -> `C1 a | L (L (R b)) -> `C2 b
                | L (R c) -> `C3 c
                | R (L d) -> `C4 d | R (R e) -> `C5 e)
      (function | `C1 a -> L (L (L a)) | `C2 b -> L (L (R b))
                | `C3 c -> L (R c)
                | `C4 d -> R (L d) | `C5 e -> R (R e))
      (choice (choice (choice a b) c) (choice d e))

let choice6 a b c d e f =
  map (function | L (L (L a)) -> `C1 a | L (L (R b)) -> `C2 b
                | L (R c) -> `C3 c
                | R (L (L d)) -> `C4 d | R (L (R e)) -> `C5 e
                | R (R f) -> `C6 f)
      (function | `C1 a -> L (L (L a)) | `C2 b -> L (L (R b))
                | `C3 c -> L (R c)
                | `C4 d -> R (L (L d)) | `C5 e -> R (L (R e))
                | `C6 f -> R (R f))
      (choice (choice (choice a b) c) (choice (choice d e) f))
