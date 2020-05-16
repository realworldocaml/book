(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Asn_core

module Writer = Asn_writer
module Bigarray = Bigarray_compat

module type Prim = sig
  type t
  val of_cstruct : Cstruct.t -> t
  val to_writer  : t -> Writer.t
  val random     : unit -> t
end

module type Prim_s = sig
  include Prim
  val random : ?size:int -> unit -> t
  val concat : t list -> t
  val length : t -> int
end

let rec replicate_l n f =
  if n < 1 then [] else f () :: replicate_l (pred n) f

let max_r_int = (1 lsl 30) - 1

let random_int () = Random.int max_r_int

let random_int_r a b = a + Random.int (b - a)

let random_size = function
  | Some size -> size
  | None      -> Random.int 20

let random_string ?size ~chars:(lo, hi) =
  String.init (random_size size)
    (fun _ -> Char.chr (random_int_r lo hi))

module Int64 = struct

  include Int64

  let ( + )  = add
  and ( - )  = sub
  and ( * )  = mul
  and ( / )  = div
  and (lsl)  = shift_left
  and (lsr)  = shift_right_logical
  and (asr)  = shift_right
  and (lor)  = logor
  and (land) = logand

  let max_p_int = Int64.of_int Stdlib.max_int

  let to_nat_checked i64 =
    if i64 < 0L || i64 > max_int then None else Some (to_int i64)

end

module Boolean : Prim with type t = bool = struct

  type t = bool

  let of_cstruct cs =
    if cs.Cstruct.len = 1 then
      (* XXX DER check *)
      Cstruct.get_uint8 cs 0 <> 0x00
    else parse_error "BOOLEAN: %a" pp_cs cs

  let to_writer b = Writer.of_byte (if b then 0xff else 0x00)

  let random = Random.bool
end

module Null : Prim with type t = unit = struct

  type t = unit

  let of_cstruct cs = if cs.Cstruct.len <> 0 then
    parse_error "NULL: %a" pp_cs cs

  let to_writer () = Writer.empty

  let random () = ()
end

module Integer : Prim with type t = Z.t = struct

  type t = Z.t

  let of_int8 x = Z.of_int (if x >= 0x80 then x - 0x100 else x) [@@inline]
  let of_int16 x = Z.of_int (if x >= 0x8000 then x - 0x10000 else x) [@@inline]

  let of_cstruct cs =
    let open Cstruct in

    let rec go acc i = function
      n when n >= 8 ->
        let w = Z.of_int64 (BE.get_uint64 cs i) in
        go Z.((acc lsl 64) lor (extract w 0 64)) (i + 8) (n - 8)
    | n when n >= 4 ->
        let w = Z.of_int32 (BE.get_uint32 cs i) in
        go Z.((acc lsl 32) lor (extract w 0 32)) (i + 4) (n - 4)
    | n when n >= 2 ->
        go Z.((acc lsl 16) lor ~$(BE.get_uint16 cs i)) (i + 2) (n - 2)
    | 1 -> Z.((acc lsl 8) lor ~$(get_uint8 cs i))
    | _ -> acc in
    match cs.Cstruct.len with
      0 -> parse_error "INTEGER: length 0"
    | 1 -> of_int8 (get_uint8 cs 0)
    | n ->
        let w0 = BE.get_uint16 cs 0 in
        match w0 land 0xff80 with
          0x0000 | 0xff80 -> parse_error "INTEGER: redundant form"
        | _ -> go (of_int16 w0) 2 (n - 2)

  let last8 z = Z.(extract z 0 8 |> to_int)

  let to_writer n =
    let sz  = Z.size n * 8 + 1 in
    let sz1 = sz - 1 in
    let cs  = Cstruct.create sz in

    let rec write i n =
      if n = Z.(~$(-1)) || n = Z.zero then i else
        ( Cstruct.set_uint8 cs i (last8 n) ;
          write (pred i) Z.(n asr 8) ) in

    let (bad_b0, padding) =
      if n >= Z.zero then ((<=) 0x80, 0x00)
      else ((>) 0x80, 0xff) in
    let off =
      let i = write sz1 n in
      if i = sz1 || bad_b0 (Cstruct.get_uint8 cs (succ i)) then
        ( Cstruct.set_uint8 cs i padding ; i )
      else succ i in
    Writer.of_cstruct Cstruct.(sub cs off (sz - off))


  let random () = Z.of_int (Random.int max_r_int - max_r_int / 2)

end

module Gen_string : Prim_s with type t = string = struct

  type t = string

  let of_cstruct = Cstruct.to_string

  let to_writer = Writer.of_string

  let random ?size () =
    random_string ?size ~chars:(32, 127)

  let (concat, length) = String.(concat "", length)
end

module Octets : Prim_s with type t = Cstruct.t = struct

  type t = Cstruct.t

  let of_cstruct { Cstruct.buffer; off; len } =
    (* XXX Mumbo jumbo to retain cs equality. *)
    Cstruct.of_bigarray @@ Bigarray.Array1.sub buffer off len

  let to_writer = Writer.of_cstruct

  let random ?size () =
    random_string ?size ~chars:(0, 256) |> Cstruct.of_string

  let concat = Cstruct.concat

  let length = Cstruct.len

end

module Bits : sig

  include Prim_s with type t = bits

  val to_array : t -> bool array
  val of_array : bool array -> t

end =
struct

  type t = int * Cstruct.t

  let of_cstruct cs =
    let n = Cstruct.len cs in
    if n = 0 then parse_error "BITS" else
    let unused = Cstruct.get_uint8 cs 0 in
    if n = 1 && unused > 0 || unused > 7 then parse_error "BITS" else
    unused, Octets.of_cstruct (Cstruct.shift cs 1)

  let to_writer (unused, cs) =
    let size = Cstruct.len cs in
    let write off cs' =
      Cstruct.set_uint8 cs' off unused;
      Cstruct.blit cs 0 cs' (off + 1) size in
    Writer.immediate (size + 1) write

  let to_array (unused, cs) =
    Array.init (Cstruct.len cs * 8 - unused) @@ fun i ->
      let byte = (Cstruct.get_uint8 cs (i / 8)) lsl (i mod 8) in
      byte land 0x80 = 0x80

  let (|<) n = function
    | true  -> (n lsl 1) lor 1
    | false -> (n lsl 1)

  let of_array arr =
    let cs = Cstruct.create ((Array.length arr + 7) / 8) in
    match
      Array.fold_left
        (fun (n, acc, i) bit ->
          if n = 8 then
            ( Cstruct.set_uint8 cs i acc ; (1, 0 |< bit, i + 1) )
          else (n + 1, acc |< bit, i))
        (0, 0, 0)
        arr
    with
    | (0, _acc, _) -> (0, cs)
    | (n, acc, i) ->
        Cstruct.set_uint8 cs i (acc lsl (8 - n));
        (8 - n, cs)

  let random ?size () = (0, Octets.random ?size ())

  let concat css =
    let (unused, css') =
      let rec go = function
        | []           -> (0, [])
        | [(u, cs)]    -> (u, [cs])
        | (_, cs)::ucs -> let (u, css') = go ucs in (u, cs::css') in
      go css in
    (unused, Cstruct.concat css')

  and length (unused, cs) = Cstruct.len cs - unused

end

module OID = struct

  open Asn_oid

  let uint64_chain cs i n =
    let rec go acc cs i = function
      0 -> parse_error "OID: unterminated component"
    | n ->
        match Cstruct.get_uint8 cs i with
          0x80 when acc = 0L -> parse_error "OID: redundant form"
        | b ->
            let lo  = b land 0x7f in
            let acc = Int64.(logor (shift_left acc 7) (of_int lo)) in
            if b < 0x80 then (acc, i + 1) else go acc cs (i + 1) (n - 1) in
    if n < 1 then parse_error "OID: 0 length component" else
      go 0L cs i (min n 8)

  let int_chain cs i n =
    let (n, i) = uint64_chain cs i n in
    match Int64.to_nat_checked n with
      Some n -> (n, i) | _ -> parse_error "OID: component out of range"

  let of_cstruct cs =
    let rec components cs i = function
      0 -> []
    | n -> let (c, i') = int_chain cs i n in
           c :: components cs i' (n + i - i') in
    match Cstruct.len cs with
      0 -> parse_error "OID: 0 length"
    | n ->
        let (b1, i) = int_chain cs 0 n in
        let v1 = b1 / 40 and v2 = b1 mod 40 in
        match base_opt v1 v2 with
          Some b -> b <|| components cs i (n - i)
        | None -> parse_error "OID: invalid base"

  let to_writer = fun (Oid (v1, v2, vs)) ->
    let cons x = function [] -> [x] | xs -> x lor 0x80 :: xs in
    let rec component xs x =
      if x < 0x80 then cons x xs
      else component (cons (x land 0x7f) xs) (x lsr 7)
    and values = function
      | []    -> Writer.empty
      | v::vs -> Writer.(of_list (component [] v) <+> values vs) in
    Writer.(of_byte (v1 * 40 + v2) <+> values vs)

  let random () =
    Random.( base (int 3) (int 40) <|| replicate_l (int 10) random_int )
end

module Time = struct

  let ps_per_ms = 1_000_000_000L

  let pp_tz ppf = function
    | 0  -> pf ppf "Z"
    | tz -> pf ppf "%c%02d%02d"
      (if tz < 0 then '+' else '-')
      (abs tz / 3600) ((abs tz mod 3600) / 60)

  (* DER-times must be UTC-normalised. If TZ comes this way, a DER flag must too. *)

  let pp_utc_time ppf t =
    let ((y, m, d), ((hh, mm, ss), tz)) = Ptime.to_date_time ~tz_offset_s:0 t in
    pf ppf "%02d%02d%02d%02d%02d%02d%a" (y mod 100) m d hh mm ss pp_tz tz

  let pp_gen_time ppf t =
    let ((y, m, d), ((hh, mm, ss), tz)) =
      Ptime.to_date_time ~tz_offset_s:0 t in
    let pp_frac ppf t = match Ptime.(frac_s t |> Span.to_d_ps) with
      | (_, 0L) -> ()
      | (_, f)  -> pf ppf ".%03Ld" Int64.(f / ps_per_ms) in
    pf ppf "%04d%02d%02d%02d%02d%02d%a%a" y m d hh mm ss pp_frac t pp_tz tz

  let of_utc_time = Format.asprintf "%a" pp_utc_time
  and of_gen_time = Format.asprintf "%a" pp_gen_time

  let catch pname f s = try f s with
  | End_of_file          -> parse_error "%s: unexpected end: %s" pname s
  | Scanf.Scan_failure _ -> parse_error "%s: invalid format: %s" pname s

  (* XXX get rid of Scanf.
   * - width specifiers are max-width only
   * - %u is too lexically relaxed *)

  let tz ic =
    try Scanf.bscanf ic "%1[+-]%2u%2u%!" @@ fun sgn h m ->
      (match sgn with "-" -> -1 | _ -> 1) * (3600 * h + 60 * m)
    with _ -> Scanf.bscanf ic "Z" 0

  let utc_time_of_string = catch "UTCTime" @@ fun s ->
    Scanf.sscanf s "%2u%2u%2u%2u%2u%r%r%!"
      (fun ic -> try Scanf.bscanf ic "%2u" id with _ -> 0) tz @@
    fun y m d hh mm ss tz ->
      let y  = (if y > 50 then 1900 else 2000) + y in
      let dt = ((y, m, d), ((hh, mm, ss), tz)) in
      match Ptime.of_date_time dt with
        Some t -> t | _ -> parse_error "UTCTime: out of range: %s" s

  let gen_time_of_string = catch "GeneralizedTime" @@ fun s ->
    let m_s_f ic =
      try Scanf.bscanf ic "%2u%r" (fun ic ->
        try Scanf.bscanf ic "%2u%r" (fun ic ->
          try Scanf.bscanf ic ".%3u" @@ fun ms -> Int64.(of_int ms * ps_per_ms)
          with _ -> 0L) @@ fun ss ms -> ss, ms
        with _ -> 0, 0L) @@ fun mm ssms -> mm, ssms
      with _ -> 0, (0, 0L) in
    Scanf.sscanf s "%4u%2u%2u%2u%r%r%!" m_s_f (fun ic -> try tz ic with _ -> 0) @@
    fun y m d hh (mm, (ss, ps)) tz ->
      let dt = ((y, m, d), ((hh, mm, ss), tz)) in match
        match Ptime.of_date_time dt with
          Some t -> Ptime.(Span.v (0, ps) |> add_span t) | _ -> None
      with Some t -> t | _ -> parse_error "GeneralizedTime: out of range: %s" s

  let get = function Some x -> x | _ -> assert false

  let date y m d = Ptime.of_date (y, m, d) |> get

  let r_date ~start ~fin =
    let dd, dps = match Ptime.(diff fin start |> Span.to_d_ps) with
      | (dd, 0L)  -> Random.(int dd, int64 86_400_000_000_000_000L)
      | (dd, dps) -> Random.(int (dd + 1), int64 dps) in
    Ptime.(Span.(v Random.(int (dd + 1), int64 dps)) |> add_span start) |> get

  let random ?(frac=false) () =
    Ptime.truncate ~frac_s:(if frac then 3 else 0) @@
      r_date ~start:(date 1970 1 1) ~fin:(date 2050 12 31)
end
