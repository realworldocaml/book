(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

module OID = Asn_oid

let id x      = x
let const x _ = x
let (&.) f g x = f (g x)

let opt def = function Some x -> x | _ -> def

type 'a endo = 'a -> 'a

type ('a, 'b) sum = L of 'a | R of 'b

let (strf, pf) = Format.(asprintf, fprintf)
let kstrf k fmt =
  Format.(kfprintf (fun _ -> flush_str_formatter () |> k) str_formatter fmt)

let invalid_arg fmt = Format.ksprintf invalid_arg fmt

let pp_list ~sep pp ppf xs =
  let rec go ppf = function
    | []    -> ()
    | [x]   -> pp ppf x
    | x::xs -> pf ppf "%a%a@ " pp x sep (); go ppf xs in
  pf ppf "@[%a@]" go xs

let pp_dump_list pp ppf xs =
  let sep ppf () = Format.pp_print_string ppf "," in
  pf ppf "[@[%a@]]" (pp_list ~sep pp) xs

let pp_cs ppf cs =
  let f ppf cs =
    for i = 0 to cs.Cstruct.len - 1 do
      if i mod 8 = 0 && i > 0 then pf ppf "@ ";
      pf ppf "%02x" (Cstruct.get_uint8 cs i)
    done in
  pf ppf "@[%a@]" f cs

module Tag = struct

  type t =
    | Universal        of int
    | Application      of int
    | Context_specific of int
    | Private          of int

  let compare t1 t2 = match (t1, t2) with
    | (Universal        a, Universal        b)
    | (Application      a, Application      b)
    | (Context_specific a, Context_specific b)
    | (Private          a, Private          b) -> compare a b
    | (Universal        _, _)
    | (Application      _, (Context_specific _ | Private _))
    | (Context_specific _, Private _) -> -1
    | _ -> 1

  let equal t1 t2 = match (t1, t2) with
    | (Universal        a, Universal        b)
    | (Application      a, Application      b)
    | (Context_specific a, Context_specific b)
    | (Private          a, Private          b) -> a = b
    | _ -> false

  let pp ppf tag =
    let (name, n) = match tag with
      | Universal n        -> ("UNIVERSAL", n)
      | Application n      -> ("APPLICATION", n)
      | Context_specific n -> ("CONTEXT", n)
      | Private n          -> ("PRIVATE", n) in
    pf ppf "%s %d" name n

end

type tag  = Tag.t
type tags = Tag.t list

module Generic = struct

  type t =
    | Cons of tag * t list
    | Prim of tag * Cstruct.t

  let tag = function Cons (t, _) -> t | Prim (t, _) -> t

  let pp_form_name ppf fsym =
    Format.pp_print_string ppf @@ match fsym with
      `Cons -> "Constructed" | `Prim -> "Primitive" | `Both -> "ANY"

  let pp_tag ppf g =
    let form = match g with Cons _ -> `Cons | Prim _ -> `Prim in
    pf ppf "(%a %a)" pp_form_name form Tag.pp (tag g)
end


type bits = int * Cstruct.t

type 'a rand = unit -> 'a

type _ asn =

  | Iso : ('a -> 'b) * ('b -> 'a) * 'b rand option * 'a asn -> 'b asn
  | Fix : ('a asn -> 'a asn) * 'a Asn_cache.var -> 'a asn

  | Sequence    : 'a sequence -> 'a asn
  | Sequence_of : 'a asn -> 'a list asn
  | Set         : 'a sequence -> 'a asn
  | Set_of      : 'a asn -> 'a list asn
  | Choice      : 'a asn * 'b asn -> ('a, 'b) sum asn

  | Implicit : tag * 'a asn -> 'a asn
  | Explicit : tag * 'a asn -> 'a asn

  | Prim : 'a prim -> 'a asn

and _ element =

  | Required : string option * 'a asn -> 'a element
  | Optional : string option * 'a asn -> 'a option element

and _ sequence =

  | Last : 'a element -> 'a sequence
  | Pair : 'a element * 'b sequence -> ('a * 'b) sequence

and _ prim =

  | Bool       : bool      prim
  | Int        : Z.t       prim
  | Bits       : bits      prim
  | Octets     : Cstruct.t prim
  | Null       : unit      prim
  | OID        : OID.t     prim
  | CharString : string    prim


let label = opt ""

let seq_tag = Tag.Universal 0x10
and set_tag = Tag.Universal 0x11

let tag_of_p : type a. a prim -> tag =
  let open Tag in function
  | Bool       -> Universal 0x01
  | Int        -> Universal 0x02
  | Bits       -> Universal 0x03
  | Octets     -> Universal 0x04
  | Null       -> Universal 0x05
  | OID        -> Universal 0x06
  | CharString -> Universal 0x1d


let rec tag_set : type a. a asn -> tags = function

  | Iso (_, _, _, asn) -> tag_set asn
  | Fix (f, _) as fix  -> tag_set (f fix)

  | Sequence    _ -> [ seq_tag ]
  | Sequence_of _ -> [ seq_tag ]
  | Set _         -> [ set_tag ]
  | Set_of _      -> [ set_tag ]
  | Choice (asn1, asn2) -> tag_set asn1 @ tag_set asn2

  | Implicit (t, _) -> [ t ]
  | Explicit (t, _) -> [ t ]

  | Prim p -> [ tag_of_p p ]

let rec tag : type a. a -> a asn -> tag = fun a -> function

  | Iso (_, g, _, asn) -> tag (g a) asn
  | Fix _ as fix       -> tag a fix
  | Sequence _         -> seq_tag
  | Sequence_of _      -> seq_tag
  | Set _              -> set_tag
  | Set_of _           -> set_tag
  | Choice (a1, a2)    -> (match a with L a' -> tag a' a1 | R b' -> tag b' a2)
  | Implicit (t, _)    -> t
  | Explicit (t, _)    -> t
  | Prim p             -> tag_of_p p


type error = [ `Parse of string ] (* XXX finer-grained *)

let pp_error ppf (`Parse err) = pf ppf "Parse error: %s" err

exception Ambiguous_syntax
exception Parse_error of error

let error err = raise (Parse_error err)
let parse_error fmt = kstrf (fun s -> error (`Parse s)) fmt

(* Check tag ambiguity.
 * XXX: Would be _epic_ to move this to the type-checker.
 *)

module FSet = struct
  type f = Fn : ('a -> 'b) -> f
  include Set.Make ( struct
    type t = f
    (* XXX collisions *)
    let compare (Fn f1) (Fn f2) = Hashtbl.(compare (hash f1) (hash f2))
  end )
  let mem f s = mem (Fn f) s
  and add f s = add (Fn f) s
end

let validate asn =

  let rec check : type a. ?tag:tag -> FSet.t -> a asn -> unit =
    fun ?tag fs -> function
    | Iso (_, _, _, a)  -> check ?tag fs a
    | Fix (f, _) as fix ->
        if not (FSet.mem f fs) then check ?tag FSet.(add f fs) (f fix)

    | Sequence s    -> disjoint_seq s ; check_s fs s
    | Set s         -> disjoint (seq_tags s) ; check_s fs s
    | Sequence_of a -> check fs a
    | Set_of      a -> check fs a

    | Choice (a1, a2) ->
        disjoint [tag_set a1; tag_set a2] ; check fs a1 ; check fs a2

    | Implicit (t, a) -> check ~tag:t fs a
    | Explicit (_, a) -> check fs a
    | Prim _          -> ()

  and check_s : type a. FSet.t -> a sequence -> unit = fun fs -> function
    | Last (Required (_, a))    -> check fs a
    | Last (Optional (_, a))    -> check fs a
    | Pair (Required (_, a), s) -> check fs a ; check_s fs s
    | Pair (Optional (_, a), s) -> check fs a ; check_s fs s

  and seq_tags : type a. a sequence -> tags list = function
    | Last (Required (_, a))    -> [tag_set a]
    | Last (Optional (_, a))    -> [tag_set a]
    | Pair (Required (_, a), s) -> tag_set a :: seq_tags s
    | Pair (Optional (_, a), s) -> tag_set a :: seq_tags s

  and disjoint_seq : type a. a sequence -> unit = fun s ->
    let f1 : type a. tags list -> a element -> tags list = fun tss -> function
      | Required (_, a) -> disjoint (tag_set a :: tss) ; []
      | Optional (_, a) -> disjoint (tag_set a :: tss) ; tag_set a :: tss in
    let rec f2 : type a. tags list -> a sequence -> unit = fun tss -> function
      | Last e      -> ignore (f1 tss e)
      | Pair (e, s) -> f2 (f1 tss e) s in
    f2 [] s

  and disjoint tss =
    let rec go = function
      | t::u::_ when Tag.equal t u -> raise Ambiguous_syntax
      | _::ts -> go ts
      | _     -> () in
    go List.(sort Tag.compare @@ concat tss) in

  check FSet.empty asn
