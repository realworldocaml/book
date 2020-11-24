(** [Uncommon] is a [Common], now with less name clashes. *)

let kasprintf k fmt =
  Format.(kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter fmt)

let invalid_arg fmt = kasprintf invalid_arg ("Mirage_crypto: " ^^ fmt)
let failwith fmt = kasprintf failwith ("Mirage_crypto: " ^^ fmt)

let (//) x y =
  if y < 1 then raise Division_by_zero else
    if x > 0 then 1 + ((x - 1) / y) else 0 [@@inline]

let imin (a : int) b = if a < b then a else b
let imax (a : int) b = if a < b then b else a

type 'a iter = ('a -> unit) -> unit

let iter2 a b   f = f a; f b
let iter3 a b c f = f a; f b; f c

module Cs = struct

  open Cstruct

  let (<+>) = append

  let clone ?len cs =
    let len = match len with None -> cs.len | Some x -> x in
    let cs' = create_unsafe len in
    ( blit cs 0 cs' 0 len ; cs' )

  let xor_into src dst n =
    if n > imin (len src) (len dst) then
      invalid_arg "Uncommon.Cs.xor_into: buffers to small (need %d)" n
    else Native.xor_into src.buffer src.off dst.buffer dst.off n

  let xor cs1 cs2 =
    let len = imin (len cs1) (len cs2) in
    let cs  = clone ~len cs2 in
    ( xor_into cs1 cs len ; cs )

  let is_prefix cs0 cs = cs0.len <= cs.len && equal cs0 (sub cs 0 cs0.len)

  let set_msb bits cs =
    if bits > 0 then
      let n = len cs in
      let rec go width = function
        | i when i = n     -> ()
        | i when width < 8 ->
            set_uint8 cs i (get_uint8 cs i lor (0xff lsl (8 - width)))
        | i ->
            set_uint8 cs i 0xff ; go (width - 8) (succ i) in
      go bits 0

  let split3 cs l1 l2 =
    let l12 = l1 + l2 in
    (sub cs 0 l1, sub cs l1 l2, sub cs l12 (len cs - l12))

  let rpad cs size x =
    let l = len cs and cs' = Cstruct.create_unsafe size in
    if size < l then invalid_arg "Uncommon.Cs.rpad: size < len";
    blit cs 0 cs' 0 l ;
    memset (sub cs' l (size - l)) x ;
    cs'

  let lpad cs size x =
    let l = len cs and cs' = Cstruct.create_unsafe size in
    if size < l then invalid_arg "Uncommon.Cs.lpad: size < len";
    blit cs 0 cs' (size - l) l ;
    memset (sub cs' 0 (size - l)) x ;
    cs'

  let of_bytes xs =
    let cs = Cstruct.create_unsafe @@ List.length xs in
    List.iteri (fun i x -> set_uint8 cs i x) xs;
    cs

  let b x =
    let cs = Cstruct.create_unsafe 1 in ( set_uint8 cs 0 x ; cs )

  let rec shift_left_inplace cs = function
    | 0 -> ()
    | bits when bits mod 8 = 0 ->
        let off = bits / 8 in
        blit cs off cs 0 (cs.len - off) ;
        memset (shift cs (cs.len - off)) 0x00
    | bits when bits < 8 ->
        let foo = 8 - bits in
        for i = 0 to cs.len - 2 do
          let b1 = get_uint8 cs i
          and b2 = get_uint8 cs (i + 1) in
          set_uint8 cs i ((b1 lsl bits) lor (b2 lsr foo))
        done ;
        set_uint8 cs (cs.len - 1) @@ get_uint8 cs (cs.len - 1) lsl bits
    | bits ->
        shift_left_inplace cs (8 * (bits / 8)) ;
        shift_left_inplace cs (bits mod 8)

  let (lsl) cs bits =
    let cs' = clone cs in
    shift_left_inplace cs' bits ; cs'
end
