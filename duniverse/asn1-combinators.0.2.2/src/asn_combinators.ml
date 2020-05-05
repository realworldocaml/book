(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Asn_core
module Prim = Asn_prim

module Int = struct
  type t = int
  let compare (a: t) b = compare a b
  let equal (a: t) b = a = b
end

let clone_cs cs =
  let open Cstruct in
  let ds = create_unsafe (len cs) in
  blit cs 0 ds 0 (len cs); cs

type cls = [ `Universal | `Application | `Private ]

let fix f = Fix (f, Asn_cache.variant ())

let map ?random f g asn = Iso (f, g, random, asn)

let to_tag id = function
  | Some `Application -> Tag.Application id
  | Some `Private     -> Tag.Private id
  | Some `Universal   -> Tag.Universal id
  | None              -> Tag.Context_specific id

let explicit ?cls id asn = Explicit (to_tag id cls, asn)
let rec implicit : type a. ?cls:cls -> int -> a asn -> a asn =
  fun ?cls id -> function
    Fix (f, _) as asn -> implicit ?cls id (f asn)
  | Iso (f, g, r, asn) -> Iso (f, g, r, implicit ?cls id asn)
  | Choice (_, _) as asn -> explicit ?cls id asn
  | asn -> Implicit (to_tag id cls, asn)

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

let bit_string = Prim.Bits.(map to_array of_array (Prim Bits))
and bit_string_cs =
  let f = function
  | 0, cs -> cs
  | clip, cs ->
      let n = Cstruct.len cs in
      let last = Cstruct.get_uint8 cs (n - 1) in
      let cs = clone_cs cs
      and last = last land (lnot (1 lsl clip - 1)) in
      Cstruct.set_uint8 cs (n - 1) last; cs
  in
  map f (fun cs -> (0, cs)) (Prim Bits)

let bit_string_flags (type a) (xs : (int * a) list) =
  let cmp = compare in (* XXX yes... *)
  let module M1 = Map.Make (struct type t = a let compare = cmp end) in
  let module M2 = Map.Make (Int) in
  let aix, ixa =
    List.fold_left (fun (m1, m2) (i, x) -> M1.add x i m1, M2.add i x m2)
    (M1.empty, M2.empty) xs in
  let n = match M2.max_binding_opt ixa with Some (x, _) -> x + 1 | _ -> 0 in
  let f bits =
    let r = ref [] in
    bits |> Array.iteri (fun i -> function
    | false -> ()
    | true -> try r := M2.find i ixa :: !r with Not_found -> ());
    List.sort cmp !r
  and g es =
    let arr = Array.make n false in
    let register e = try arr.(M1.find e aix) <- true with Not_found -> () in
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
