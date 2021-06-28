(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Asn_core

module Prim   = Asn_prim
module Writer = Asn_writer
module Int64  = Prim.Int64


let (@?) oa a = match oa with Some x -> x | None -> a

module Seq = struct

  type 'r f = { f : 'a. 'a -> 'a asn -> 'r -> 'r }

  let rec fold_with_value : type a. 'r f -> 'r -> a -> a sequence -> 'r
  = fun f r a -> function
    | Last (Required (_, asn)) -> f.f a asn r
    | Last (Optional (_, asn)) ->
      ( match a with None -> r | Some a' -> f.f a' asn r )
    | Pair (Required (_, asn), asns) ->
        let (a1, a2) = a in f.f a1 asn (fold_with_value f r a2 asns)
    | Pair (Optional (_, asn), asns) ->
        match a with
        | (None   , a2) -> fold_with_value f r a2 asns
        | (Some a1, a2) -> f.f a1 asn (fold_with_value f r a2 asns)
end

module R = struct

  module G = Generic

  type config = { strict : bool }

  type coding =
    | Primitive   of int
    | Constructed of int
    | Constructed_indefinite

  module Header = struct

    open Cstruct

    let error cs fmt =
      parse_error ("Header: at %a: " ^^ fmt) pp_cs cs

    let ck_redundant cs cfg (n : int) limit =
      if cfg.strict && n < limit then error cs "redundant form"

    let big_tag cs =
      let rec go acc = function
        | 8 -> error cs "big tag: too long"
        | i ->
            let b = get_uint8 cs i in
            let x = Int64.of_int (b land 0x7f) in
            match (Int64.(acc lsl 7 + x), b land 0x80) with
            | (0L,  _) -> error cs "big tag: leading 0"
            | (acc, 0) ->
              ( match Int64.to_nat_checked acc with
                | Some x -> (x, succ i)
                | None   -> error cs "big tag: overflow: %Li" acc)
            | (acc, _) -> go acc (succ i) in
      go 0L 0

    let big_len cfg cs = function
      0 -> error cs "empty length"
    | n ->
        let rec f cs i = function
          0 -> 0L
        | n -> match get_uint8 cs i with
            0 when cfg.strict -> error cs "redundant length"
          | 0 -> f cs (i + 1) (n - 1)
          | _ when n > 8 -> error cs "length overflow"
          | x -> g (Int64.of_int x) cs (i + 1) (n - 1)
        and g acc cs i = function
          0 -> acc
        | n -> let acc = Int64.(acc lsl 8 + of_int (get_uint8 cs i)) in
               g acc cs (i + 1) (n - 1) in
        match f cs 0 n |> Int64.to_nat_checked with
          Some x -> x | _ -> error cs "length overflow"

    let parse cfg cs =

      let t0 = get_uint8 cs 0 in
      let (tag_v, off_len) =
        match t0 land 0x1f with
        | 0x1f ->
            let (n, i) = big_tag (shift cs 1) in
            ck_redundant cs cfg n 0x1f;
            (n, i + 1)
        | x -> (x, 1) in
      let l0    = get_uint8 cs off_len in
      let lbody = l0 land 0x7f in
      let (len, off_end) =
        if l0 <= 0x80 then (lbody, off_len + 1) else
          let n = big_len cfg (shift cs (off_len + 1)) lbody in
          ck_redundant cs cfg n 0x7f;
          (n, off_len + 1 + lbody) in
      let tag = match t0 land 0xc0 with
        | 0x00 -> Tag.Universal        tag_v
        | 0x40 -> Tag.Application      tag_v
        | 0x80 -> Tag.Context_specific tag_v
        | _    -> Tag.Private          tag_v
      and coding =
        (* according to layman's guide to a subset of ASN.1, BER, and DER, there
           are three possibilities in BER (DER restricts this further):
           - (a) primitive + definitive length
           - (b) constructed + definitive length
           - (c) constructed + indefinite length *)
        match (t0 land 0x20, l0) with
        | (0, 0x80) -> error cs "primitive and indefinite length"
        | (0, _   ) -> Primitive len
        | (_, 0x80) -> Constructed_indefinite
        | _         -> Constructed len in
      (tag, off_end, coding)
  end

  module Gen = struct

    let eof1 cs = cs.Cstruct.len = 0
    and eof2 cs = Cstruct.LE.get_uint16 cs 0 = 0

    let split_off cs off n =
      let k = off + n in
      Cstruct.(sub cs off n, sub cs k (len cs - k))

    let rec children cfg eof acc cs =
      if eof cs then (List.rev acc, cs) else
        let (g, cs) = node cfg cs in
        children cfg eof (g::acc) cs

    and node cfg cs =
      let (tag, off, coding) = Header.parse cfg cs in
      match coding with
      | Primitive n ->
          let (hd, tl) = split_off cs off n in
          (G.Prim (tag, hd), tl)
      | Constructed n ->
          let (hd, tl) = split_off cs off n in
          let (gs, _ ) = children cfg eof1 [] hd in
          (G.Cons (tag, gs), tl)
      | Constructed_indefinite when cfg.strict ->
          parse_error "Constructed indefinite form"
      | Constructed_indefinite ->
          let (gs, tl) = children cfg eof2 [] (Cstruct.shift cs off) in
          (G.Cons (tag, gs), Cstruct.shift tl 2)

    let parse cfg cs =
      try node cfg cs with Invalid_argument _ ->
        parse_error "Unexpected EOF: %a" pp_cs cs
  end


  module TM = Map.Make (Tag)

  module Cache = Asn_cache.Make ( struct
    type 'a k = 'a asn endo
    type 'a v = G.t -> 'a
    let mapv = (&.)
  end )

  let err_type ?(form=`Both) t g =
    parse_error "Type mismatch: expected: (%a %a) got: %a"
      G.pp_form_name form Tag.pp t G.pp_tag g

  let primitive t f = function
    | G.Prim (t1, bs) when Tag.equal t t1 -> f bs
    | g -> err_type ~form:`Prim t g

  let constructed t f = function
    | G.Cons (t1, gs) when Tag.equal t t1 -> f gs
    | g -> err_type ~form:`Cons t g

  let string_like (type a) c t (module P : Prim.Prim_s with type t = a) =
    let rec p = function
      | G.Prim (t1, bs) when Tag.equal t t1 -> P.of_cstruct bs
      | G.Cons (t1, gs) when Tag.equal t t1 && not c.strict ->
          P.concat (List.map p gs)
      | g -> err_type t g in
    p

  let c_prim : type a. config -> tag -> a prim -> G.t -> a = fun c tag -> function
    | Bool       -> primitive tag Prim.Boolean.of_cstruct
    | Int        -> primitive tag Prim.Integer.of_cstruct
    | Bits       -> string_like c tag (module Prim.Bits)
    | Octets     -> string_like c tag (module Prim.Octets)
    | Null       -> primitive tag Prim.Null.of_cstruct
    | OID        -> primitive tag Prim.OID.of_cstruct
    | CharString -> string_like c tag (module Prim.Gen_string)

  let peek asn =
    match tag_set asn with
    | [tag] -> fun g -> Tag.equal (G.tag g) tag
    | tags  -> fun g ->
        let tag = G.tag g in List.exists (fun t -> Tag.equal t tag) tags

  type opt = Cache.t * config

  let rec c_asn : type a. a asn -> opt:opt -> G.t -> a = fun asn ~opt ->

    let rec go : type a. ?t:tag -> a asn -> G.t -> a = fun ?t -> function
      | Iso (f, _, _, a) -> f &. go ?t a
      | Fix (fa, var) as fix ->
          let p = lazy (go ?t (fa fix)) in
          Cache.intern (fst opt) var fa @@ fun g -> Lazy.force p g
      | Sequence s       -> constructed (t @? seq_tag) (c_seq s ~opt)
      | Sequence_of a    -> constructed (t @? seq_tag) (List.map (c_asn a ~opt))
      | Set s            -> constructed (t @? set_tag) (c_set s ~opt)
      | Set_of a         -> constructed (t @? set_tag) (List.map (c_asn a ~opt))
      | Implicit (t0, a) -> go ~t:(t @? t0) a
      | Explicit (t0, a) -> constructed (t @? t0) (c_explicit a ~opt)
      | Choice (a1, a2)  ->
          let (p1, p2) = (c_asn a1 ~opt, c_asn a2 ~opt)
          and accepts1 = peek a1 in
          fun g -> if accepts1 g then L (p1 g) else R (p2 g)
      | Prim p -> c_prim (snd opt) (t @? tag_of_p p) p in

    go asn

  and c_explicit : type a. a asn -> opt:opt -> G.t list -> a = fun a ~opt ->

    let p = c_asn a ~opt in function
      | [g] -> p g
      | gs  -> parse_error "EXPLICIT: sequence: %a" (pp_dump_list G.pp_tag) gs

  and c_seq : type a. a sequence -> opt:opt -> G.t list -> a = fun s ~opt ->

    let rec seq : type a. a sequence -> G.t list -> a = function
      | Pair (e, s) ->
          let (p1, p2) = (element e, c_seq s ~opt) in
          fun gs -> let (r, gs') = p1 gs in (r, p2 gs')
      | Last e ->
          let p = element e in fun gs ->
            match p gs with (a, []) -> a | (_, gs) ->
              parse_error "SEQUENCE: trailing: %a" (pp_dump_list G.pp_tag) gs

    and element : type a. a element -> G.t list -> a * G.t list = function
      | Required (lbl, a) ->
          let p = c_asn a ~opt in (function
            | g::gs -> (p g, gs)
            | []    -> parse_error "SEQUENCE: missing required: %s" (label lbl))
      | Optional (_, a) ->
          let (p, accepts) = (c_asn a ~opt, peek a) in
          function | g::gs when accepts g -> (Some (p g), gs)
                   | gs                   -> (None, gs)
    in seq s

  and c_set : type a. a sequence -> opt:opt -> G.t list -> a = fun s ~opt ->

    let module P = struct

      module C = Asn_core

      type 'a or_missing = Found of 'a | Miss of string option

      type _ element =
        | Required : 'a or_missing -> 'a element
        | Optional : 'a or_missing -> 'a option element

      type _ sequence =
        | Last : 'a element -> 'a sequence
        | Pair : 'a element * 'b sequence -> ('a * 'b) sequence

      let rec of_sequence : type a. a C.sequence -> a sequence = function
        | C.Last (C.Required (lbl, _))    -> Last (Required (Miss lbl))
        | C.Last (C.Optional (lbl, _))    -> Last (Optional (Miss lbl))
        | C.Pair (C.Required (lbl, _), t) -> Pair (Required (Miss lbl), of_sequence t)
        | C.Pair (C.Optional (lbl, _), t) -> Pair (Optional (Miss lbl), of_sequence t)

      let to_tuple =
        let rec element : type a. a element -> a = function
          | Required (Miss  lbl) -> parse_error "SET: missing required: %s" (label lbl)
          | Required (Found a  ) -> a
          | Optional (Miss  _  ) -> None
          | Optional (Found a  ) -> Some a
        and seq : type a. a sequence -> a = function
          | Last e       -> element e
          | Pair (e, tl) -> (element e, seq tl) in
        seq

      let found_r a = Required (Found a)
      and found_o a = Optional (Found a)
    end in

    let put  r = function P.Pair (_, tl) -> P.Pair (r,   tl) | _ -> assert false
    and wrap f = function P.Pair (e, tl) -> P.Pair (e, f tl) | _ -> assert false in

    let rec element : type a. a element -> tags * (G.t -> a P.element) = function
      | Required (_, a) -> (tag_set a, P.found_r &. c_asn a ~opt)
      | Optional (_, a) -> (tag_set a, P.found_o &. c_asn a ~opt)

    and seq :
      type a b. (a P.sequence endo -> b P.sequence endo)
             -> a sequence -> (tags * (G.t -> b P.sequence endo)) list =
      fun k -> function
      | Last e ->
          let (tags, p) = element e in
          [(tags, (fun e' -> k (fun _ -> P.Last e')) &. p)]
      | Pair (e, tl) ->
          let (tags, p) = element e in
          (tags, k &. put &. p) :: seq (k &. wrap) tl in

    let parsers =
      List.fold_right (fun (tags, p) ->
          List.fold_right (fun tag -> TM.add tag p) tags)
        (seq id s) TM.empty in

    let rec step acc ps = function
      | []    -> P.to_tuple acc
      | g::gs ->
          let p =
            try TM.find (G.tag g) ps
            with Not_found -> parse_error "SET: unexpected: %a" G.pp_tag g in
          step (p g acc) (TM.remove (G.tag g) ps) gs in

    step (P.of_sequence s) parsers

  let (compile_ber, compile_der) =
    let compile cfg asn =
      let p = c_asn asn ~opt:(Cache.create (), cfg) in
      fun cs -> let (g, cs') = Gen.parse cfg cs in (p g, cs') in
    (fun asn -> compile { strict = false } asn),
    (fun asn -> compile { strict = true  } asn)

end

module W = struct

  let (<+>) = Writer.(<+>)

  let e_big_tag tag =
    let cons x = function [] -> [x] | xs -> (x lor 0x80)::xs in
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (cons (n land 0x7f) acc) (n lsr 7) in
    loop [] tag

  let e_big_length length =
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (n land 0xff :: acc) (n lsr 8) in
    loop [] length

  let e_header tag mode len =

    let (klass, tagn) =
      let open Tag in
      match tag with
      | Universal n        -> (0x00, n)
      | Application n      -> (0x40, n)
      | Context_specific n -> (0x80, n)
      | Private n          -> (0xc0, n) in

    let constructed = match mode with
      | `Primitive   -> 0x00
      | `Constructed -> 0x20 in

    ( if tagn < 0x1f then
        Writer.of_byte (klass lor constructed lor tagn)
      else
        Writer.of_byte (klass lor constructed lor 0x1f) <+>
        Writer.of_list (e_big_tag tagn) )
    <+>
    ( if len <= 0x7f then
        Writer.of_byte len
      else
        let body = Writer.of_list (e_big_length len) in
        Writer.of_byte (0x80 lor Writer.len body) <+> body )


  type conf = { der : bool }

  let e_constructed tag body =
    e_header tag `Constructed (Writer.len body) <+> body

  let e_primitive tag body =
    e_header tag `Primitive (Writer.len body) <+> body

  let assert_length ?constr f a = match constr with
    | None   -> ()
    | Some n ->
        let n' = f a in
        if n <> n' then invalid_arg "Encode: length: expected %d, got %d" n n'

  let rec encode : type a. conf -> tag option -> a -> a asn -> Writer.t
  = fun conf tag a -> function

    | Iso (_, g, _, asn) -> encode conf tag (g a) asn

    | Fix (fa, _) as fix -> encode conf tag a (fa fix)

    | Sequence asns ->
        e_constructed (tag @? seq_tag) (e_seq conf a asns)

    | Sequence_of asn -> (* size/stack? *)
        e_constructed (tag @? seq_tag) @@
          Writer.concat (List.map (fun e -> encode conf None e asn) a)

    | Set asns ->
        let h_sorted conf a asns =
          let fn = { Seq.f = fun a asn xs ->
            ( Asn_core.tag a asn, encode conf None a asn ) :: xs } in
          Writer.concat @@
            List.( map snd @@
              sort (fun (t1, _) (t2, _) -> compare t1 t2) @@
                Seq.fold_with_value fn [] a asns )
        in
        e_constructed (tag @? set_tag) @@
          if conf.der then h_sorted conf a asns else e_seq conf a asns

    | Set_of asn ->
        let ws = List.map (fun e -> encode conf None e asn) a in
        let body =
          Writer.concat @@
            if conf.der then
              List.( ws |> map  Writer.to_cstruct
                        |> sort Writer.cs_lex_compare
                        |> map  Writer.of_cstruct )
            else ws
        in
        e_constructed (tag @? set_tag) body

    | Choice (asn1, asn2) ->
      ( match a with
        | L a' -> encode conf tag a' asn1
        | R b' -> encode conf tag b' asn2 )

    | Implicit (t, asn) ->
        encode conf (Some (tag @? t)) a asn

    | Explicit (t, asn) ->
        e_constructed (tag @? t) (encode conf None a asn)

    | Prim p -> e_prim tag a p

  and e_seq : type a. conf -> a -> a sequence -> Writer.t = fun conf ->
    let f = { Seq.f = fun e asn w -> encode conf None e asn <+> w } in
    Seq.fold_with_value f Writer.empty

  and e_prim : type a. tag option -> a -> a prim -> Writer.t = fun tag a prim ->
    let encode = e_primitive
      (match tag with Some x -> x | None -> tag_of_p prim) in
    let encode_s (type a) ?length a (module P : Prim.Prim_s with type t = a) =
      assert_length ?constr:length P.length a;
      encode (P.to_writer a) in
    match prim with
    | Bool       -> encode @@ Prim.Boolean.to_writer a
    | Int        -> encode @@ Prim.Integer.to_writer a
    | Bits       -> encode @@ Prim.Bits.to_writer a
    | Octets     -> encode_s a (module Prim.Octets)
    | Null       -> encode @@ Prim.Null.to_writer a
    | OID        -> encode @@ Prim.OID.to_writer a
    | CharString -> encode @@ Prim.Gen_string.to_writer a


  let ber_to_writer asn a = encode { der = false } None a asn

  let der_to_writer asn a = encode { der = true } None a asn

end
