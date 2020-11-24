(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

exception Parse_error of string * string

type scope =
| Point
| Interface
| Link
| Admin
| Site
| Organization
| Global

let try_with_result fn a =
  try Ok (fn a)
  with Parse_error (msg, _) -> Error (`Msg ("Ipaddr: " ^ msg))

let failwith_msg = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

let map_result v f = match v with Ok v -> Ok (f v) | Error _ as e -> e

let string_of_scope = function
| Point -> "point"
| Interface -> "interface"
| Link -> "link"
| Admin -> "admin"
| Site -> "site"
| Organization -> "organization"
| Global -> "global"

let scope_of_string = function
| "point" -> Ok Point
| "interface" -> Ok Interface
| "link" -> Ok Link
| "admin" -> Ok Admin
| "site" -> Ok Site
| "organization" -> Ok Organization
| "global" -> Ok Global
| s -> Error (`Msg ("unknown scope: " ^ s))

let pp_scope fmt s =
  Format.pp_print_string fmt (string_of_scope s)

let (~|) = Int32.of_int
let (|~) = Int32.to_int
let (&&&) x y = Int32.logand x y
let (|||) x y = Int32.logor x y
let (<|<) x y = Int32.shift_left x y
let (>|>) x y = Int32.shift_right_logical x y
let (>!)  x y = (x >|> y) &&& 0xFF_l
let (<!)  x y = (x &&& 0xFF_l) <|< y

let need_more x = Parse_error ("not enough data", x)

let char_0 = int_of_char '0'
let char_a = int_of_char 'a'
let char_A = int_of_char 'A'

let int_of_char c = match c with
  | '0'..'9' -> Stdlib.int_of_char c - char_0
  | 'a'..'f' -> 10 + Stdlib.int_of_char c - char_a
  | 'A'..'F' -> 10 + Stdlib.int_of_char c - char_A
  | _ -> -1

let bad_char i s =
  let msg = Printf.sprintf "invalid character '%c' at %d" s.[i] i
  in Parse_error (msg, s)

let is_number base n = n >=0 && n < base

let parse_int base s i =
  let len = String.length s in
  let rec next prev =
    let j = !i in
    if j >= len then prev
    else let c = s.[j] in
         let k = int_of_char c in
         if is_number base k
         then (incr i; next (prev*base + k))
         else prev
  in
  let i = !i in
  if i < len
  then if is_number base (int_of_char s.[i])
    then next 0
    else raise (bad_char i s)
  else raise (need_more s)

let parse_dec_int s i = parse_int 10 s i
let parse_hex_int s i = parse_int 16 s i
let expect_char s i c =
  if !i < String.length s
  then if s.[!i] <> c then raise (bad_char !i s) else incr i
  else raise (need_more s)
let expect_end s i =
  if String.length s <= !i
  then ()
  else raise (bad_char !i s)

let hex_char_of_int = function
  |  0 -> '0'
  |  1 -> '1'
  |  2 -> '2'
  |  3 -> '3'
  |  4 -> '4'
  |  5 -> '5'
  |  6 -> '6'
  |  7 -> '7'
  |  8 -> '8'
  |  9 -> '9'
  | 10 -> 'a'
  | 11 -> 'b'
  | 12 -> 'c'
  | 13 -> 'd'
  | 14 -> 'e'
  | 15 -> 'f'
  |  _ -> raise (Invalid_argument "not a hex int")

let hex_string_of_int32 i = String.make 1 (hex_char_of_int (Int32.to_int i))

module V4 = struct
  type t = int32

  let compare a b = (* ignore the sign *)
    let c = Int32.compare (a >|> 1) (b >|> 1) in
    if c = 0 then Int32.compare (a &&& 1l) (b &&& 1l) else c

  let make a b c d =
    ((~| a <! 24) ||| (~| b <! 16)) ||| ((~| c <! 8) ||| (~| d <! 0))

  (* parsing *)

  let parse_dotted_quad s i =
    let a = parse_dec_int s i in
    expect_char s i '.';
    let b = parse_dec_int s i in
    expect_char s i '.';
    let c = parse_dec_int s i in
    expect_char s i '.';
    let d = parse_dec_int s i in
    let valid a = a land 0xff <> a in
    if valid a
    then raise (Parse_error ("first octet out of bounds", s))
    else if valid b
    then raise (Parse_error ("second octet out of bounds", s))
    else if valid c
    then raise (Parse_error ("third octet out of bounds", s))
    else if valid d
    then raise (Parse_error ("fourth octet out of bounds", s))
    else make a b c d

  (* string conversion *)

  let of_string_raw = parse_dotted_quad

  let of_string_exn s =
    let o = ref 0 in
    let x = of_string_raw s o in
    expect_end s o;
    x

  let of_string s = try_with_result of_string_exn s

  let to_buffer b i =
    Printf.bprintf b "%ld.%ld.%ld.%ld" (i >! 24) (i >! 16) (i >! 8) (i >! 0)

  let to_string i =
    let b = Buffer.create 15 in
    to_buffer b i;
    Buffer.contents b

  let pp ppf i =
    Format.fprintf ppf "%s" (to_string i)

  (* Octets conversion *)

  let of_octets_exn ?(off=0) bs =
    try
      make
      (Char.code bs.[0 + off])
      (Char.code bs.[1 + off])
      (Char.code bs.[2 + off])
      (Char.code bs.[3 + off])
    with _ -> raise (need_more bs)

  let of_octets ?off bs = try_with_result (of_octets_exn ?off) bs

  let write_octets_exn ?(off=0) i b =
    try
      Bytes.set b (0 + off) (Char.chr ((|~) (i >! 24)));
      Bytes.set b (1 + off) (Char.chr ((|~) (i >! 16)));
      Bytes.set b (2 + off) (Char.chr ((|~) (i >!  8)));
      Bytes.set b (3 + off) (Char.chr ((|~) (i >!  0)))
    with _ -> raise (need_more (Bytes.to_string b))

  let write_octets ?off i bs = try_with_result (write_octets_exn ?off i) bs

  let to_octets i =
    String.init 4 (function
      | 0 -> Char.chr ((|~) (i >! 24))
      | 1 -> Char.chr ((|~) (i >! 16))
      | 2 -> Char.chr ((|~) (i >! 8))
      | 3 -> Char.chr ((|~) (i >! 0))
      | _ -> assert false)

  (* Int32 *)
  let of_int32 i = i
  let to_int32 i = i

  (* Int16 *)
  let of_int16 (a,b) = (~| a <|< 16) ||| (~| b)
  let to_int16 a = ((|~) (a >|> 16), (|~) (a &&& 0xFF_FF_l))

  (* MAC *)
  (* {{:http://tools.ietf.org/html/rfc1112#section-6.2}RFC 1112}. *)
  let multicast_to_mac i =
    let macb = Bytes.create 6 in
    Bytes.set macb 0 (Char.chr 0x01);
    Bytes.set macb 1 (Char.chr 0x00);
    Bytes.set macb 2 (Char.chr 0x5E);
    Bytes.set macb 3 (Char.chr ((|~) (i >|> 16 &&& 0x7F_l)));
    Bytes.set macb 4 (Char.chr ((|~) (i >! 8)));
    Bytes.set macb 5 (Char.chr ((|~) (i >! 0)));
    Macaddr.of_octets_exn (Bytes.to_string macb)

  (* Host *)
  let to_domain_name i =
    let name = [
      Int32.to_string (i >!  0);
      Int32.to_string (i >!  8);
      Int32.to_string (i >! 16);
      Int32.to_string (i >! 24);
      "in-addr";
      "arpa" ]
    in
    Domain_name.(host_exn (of_strings_exn name))

  let of_domain_name n =
    match Domain_name.to_strings n with
    | [ a ; b ; c ; d ; in_addr ; arpa ] when
        Domain_name.(equal_label arpa "arpa" && equal_label in_addr "in-addr") ->
      begin
        let conv bits data =
          let i = Int32.of_int (parse_dec_int data (ref 0)) in
          if i > 0xFFl then
            raise (Parse_error ("label with a too big number", data))
          else
            i <! bits
        in
        try
          let ( + ) = Int32.add in
          Some ((conv 0 a) + (conv 8 b) + (conv 16 c) + (conv 24 d))
        with
        | Parse_error _ -> None
      end
    | _ -> None

  let succ t =
    if Int32.equal t 0xFF_FF_FF_FFl then
      Error (`Msg "Ipaddr: highest address has been reached")
    else
      Ok (Int32.succ t)

  let pred t =
    if Int32.equal t 0x00_00_00_00l then
      Error (`Msg "Ipaddr: lowest address has been reached")
    else
      Ok (Int32.pred t)

  (* constant *)

  let any         = make   0   0   0   0
  let unspecified = make   0   0   0   0
  let broadcast   = make 255 255 255 255
  let localhost   = make 127   0   0   1
  let nodes       = make 224   0   0   1
  let routers     = make 224   0   0   2

  module Prefix = struct
    type addr = t
    type t = addr * int

    let compare (pre,sz) (pre',sz') =
      let c = compare pre pre' in
      if c = 0 then Stdlib.compare sz sz' else c

    let ip = make

    let mask sz =
      if sz <= 0 then 0_l
      else if sz >= 32 then 0x0_FF_FF_FF_FF_l
      else 0x0_FF_FF_FF_FF_l <|< (32 - sz)

    let prefix (pre,sz) = (pre &&& (mask sz), sz)

    let make sz pre = (pre,sz)

    let network_address (pre,sz) addr =
      (pre &&& (mask sz)) ||| (addr &&& Int32.lognot (mask sz))

    (* string conversion *)

    let _of_string_raw s i =
      let quad = of_string_raw s i in
      expect_char s i '/';
      let p = parse_dec_int s i in
      if p > 32 || p < 0
      then raise (Parse_error ("invalid prefix size", s));
      (p,quad)

    let of_string_raw s i =
      let (p,quad) = _of_string_raw s i in
      make p quad

    let _of_string_exn s =
      let i = ref 0 in
      let res = _of_string_raw s i in
      expect_end s i;
      res

    let of_string_exn s = let (p,quad) = _of_string_exn s in make p quad

    let of_string s = try_with_result of_string_exn s

    let _of_netmask_exn ~netmask address =
      let rec find_greatest_one bits i =
        if bits = 0_l then i-1 else find_greatest_one (bits >|> 1) (i+1)
      in
      let one = netmask &&& (Int32.neg netmask) in
      let sz = 32 - (find_greatest_one one (if one = 0_l then 33 else 0)) in
      if netmask <> (mask sz)
      then raise (Parse_error ("invalid netmask",to_string netmask))
      else make sz address

    let of_netmask_exn ~netmask ~address = _of_netmask_exn ~netmask address

    let of_netmask ~netmask ~address =
      try_with_result (_of_netmask_exn ~netmask) address

    let to_buffer buf (pre,sz) = Printf.bprintf buf "%a/%d" to_buffer pre sz

    let to_string subnet =
      let b = Buffer.create 18 in
      to_buffer b subnet;
      Buffer.contents b

    let pp ppf i =
      Format.fprintf ppf "%s" (to_string i)

    let mem ip (pre,sz) =
      let m = mask sz in
      (ip &&& m) = (pre &&& m)

    let subset ~subnet:(pre1,sz1) ~network:(pre2,sz2) =
      sz1 >= sz2 && mem pre1 (pre2,sz2)

    let of_addr ip = make 32 ip

    let global          = make  0 (ip   0   0 0 0)
    let relative        = make  8 (ip   0   0 0 0)
    let loopback        = make  8 (ip 127   0 0 0)
    let link            = make 16 (ip 169 254 0 0)
    let multicast       = make  4 (ip 224   0 0 0)
    let multicast_org   = make 14 (ip 239 192 0 0)
    let multicast_admin = make 16 (ip 239 255 0 0)
    let multicast_link  = make 24 (ip 224   0 0 0)
    (* http://tools.ietf.org/html/rfc2365 *)

    let private_10  = make 8  (ip 10    0 0 0)
    let private_172 = make 12 (ip 172  16 0 0)
    let private_192 = make 16 (ip 192 168 0 0)

    let private_blocks = [
      loopback ; link ; private_10 ; private_172 ; private_192
    ]

    let broadcast (pre,sz) = pre ||| (0x0_FF_FF_FF_FF_l >|> sz)
    let network (pre,sz) = pre &&& mask sz
    let address (addr,_) = addr
    let bits (_,sz) = sz
    let netmask subnet = mask (bits subnet)

    let first (_,sz as cidr) =
      if sz > 30 then
        network cidr
      else
        network cidr |> succ |> failwith_msg

    let last (_,sz as cidr) =
      if sz > 30 then
        broadcast cidr
      else
        broadcast cidr |> pred |> failwith_msg
  end

  (* TODO: this could be optimized with something trie-like *)
  let scope i =
    let mem = Prefix.mem i in
    if mem Prefix.loopback then Interface
    else if mem Prefix.link then Link
    else if List.exists mem Prefix.private_blocks then Organization
    else if i = unspecified then Point
    else if i = broadcast then Admin
    else if mem Prefix.relative then Admin
    else if mem Prefix.multicast
    then (if mem Prefix.multicast_org then Organization
      else if mem Prefix.multicast_admin then Admin
      else if mem Prefix.multicast_link then Link
      else Global)
    else Global

  let is_global i = (scope i) = Global
  let is_multicast i = Prefix.(mem i multicast)
  let is_private i = (scope i) <> Global
end

module B128 = struct
  type t = int32 * int32 * int32 * int32

  let of_int64 (a, b) =
    Int64.(
      to_int32 (shift_right_logical a 32),
      to_int32 a,
      to_int32 (shift_right_logical b 32),
      to_int32 b)
  let to_int64 (a,b,c,d) =
    Int64.(
      logor (shift_left (of_int32 a) 32) (of_int32 b),
      logor (shift_left (of_int32 c) 32) (of_int32 d))

  let of_int32 x = x
  let to_int32 x = x

  let of_int16 (a, b, c, d, e, f, g, h) =
    V4.of_int16 (a,b),
    V4.of_int16 (c,d),
    V4.of_int16 (e,f),
    V4.of_int16 (g,h)

  let to_int16 (x,y,z,t) =
    let a,b = V4.to_int16 x
    and c,d = V4.to_int16 y
    and e,f = V4.to_int16 z
    and g,h = V4.to_int16 t
    in
    (a,b,c,d,e,f,g,h)

  let write_octets_exn ?(off=0) (a,b,c,d) byte =
    V4.write_octets_exn ~off a byte;
    V4.write_octets_exn ~off:(off+4) b byte;
    V4.write_octets_exn ~off:(off+8) c byte;
    V4.write_octets_exn ~off:(off+12) d byte

  let compare (a1,b1,c1,d1) (a2,b2,c2,d2) =
    match V4.compare a1 a2 with
      | 0 -> begin
        match V4.compare b1 b2 with
          | 0 -> begin
            match V4.compare c1 c2 with
              | 0 -> V4.compare d1 d2
              | n -> n end
          | n -> n end
      | n -> n

  let logand (a1,b1,c1,d1) (a2,b2,c2,d2) =
    (a1 &&& a2, b1 &&& b2, c1 &&& c2, d1 &&& d2)

  let logor (a1,b1,c1,d1) (a2,b2,c2,d2) =
    (a1 ||| a2, b1 ||| b2, c1 ||| c2, d1 ||| d2)

  let lognot (a,b,c,d) = Int32.(lognot a, lognot b, lognot c, lognot d)

  let succ (a,b,c,d) =
    let cb (n,tl) v =
      match n with
      | 0l -> (0l,v::tl)
      | n ->
         let n =
           if Int32.equal v 0xFF_FF_FF_FFl then
             n
           else
             0l
         in
         (n,Int32.succ v::tl)
    in
    match List.fold_left cb (1l,[]) [d;c;b;a] with
    | 0l, [a;b;c;d] -> Ok (of_int32 (a,b,c,d))
    | n, [_;_;_;_] when n > 0l ->
      Error (`Msg "Ipaddr: highest address has been reached")
    | _ -> Error (`Msg "Ipaddr: unexpected error with B128")

  let pred (a,b,c,d) =
    let cb (n,tl) v =
      match n with
      | 0l -> (0l,v::tl)
      | n ->
         let n =
           if v = 0x00_00_00_00l then
             n
           else
             0l
         in
         (n,Int32.pred v::tl)
    in
    match List.fold_left cb (-1l,[]) [d;c;b;a] with
    | 0l, [a;b;c;d] -> Ok (of_int32 (a,b,c,d))
    | n, [_;_;_;_] when n < 0l ->
      Error (`Msg "Ipaddr: lowest address has been reached")
    | _ -> Error (`Msg "Ipaddr: unexpected error with B128")

  let shift_right (a,b,c,d) sz =
    let rec loop (a,b,c,d) sz =
      if sz < 32 then (sz, (a,b,c,d))
      else loop (0l,a,b,c) (sz - 32)
    in
    let (sz, (a,b,c,d)) = loop (a,b,c,d) sz in
    let fn (saved,tl) part =
      let new_saved = Int32.logand part (0xFF_FF_FF_FFl >|> sz) in
      let new_part = (part >|> sz) ||| (saved <|< 32 - sz) in
      (new_saved, new_part::tl)
    in
    match List.fold_left fn (0l,[]) [a;b;c;d] with
    | _, [d;c;b;a] -> Ok (of_int32 (a, b, c, d))
    | _ -> Error (`Msg "Ipaddr: unexpected error with B128.shift_right")
end

module V6 = struct
  include B128

  (* TODO: Perhaps represent with bytestring? *)
  let make a b c d e f g h = of_int16 (a,b,c,d,e,f,g,h)

  (* parsing *)
  let parse_ipv6 s i =
    let compressed = ref false in (* :: *)
    let len = String.length s in
    if len < !i + 1 then (raise (need_more s));
    let use_bracket = s.[!i] = '[';  in
    if use_bracket then incr i;
    if len < !i + 2 then (raise (need_more s));
    (* check if it starts with :: *)
    let l =
      if s.[!i] = ':' then begin
        incr i;
        if s.[!i] = ':' then begin
          compressed := true;
          incr i;
          [-1]
        end
        else
          raise (bad_char !i s);
      end
      else []
    in

    let rec loop nb acc =
      if nb >= 8 then acc
      else if !i >= len
      then acc
      else
        let pos = !i in
        let x = try parse_hex_int s i with _ -> -1 in
        if x < 0 then acc
        else if nb = 7
        then x::acc
        else if !i < len && s.[!i] = ':'
        then begin
          incr i;
          if !i < len
          then if s.[!i] = ':'
            then
              if !compressed then (decr i; x::acc) (* trailing :: *)
              else begin
                compressed:=true;
                incr i;
                loop (nb + 2) (-1::x::acc)
              end
            else begin
              if is_number 16 (int_of_char s.[!i])
              then loop (nb+1) (x::acc)
              else raise (bad_char !i s)
            end
          else raise (need_more s)
        end
        else if !i < len && s.[!i] = '.'
        then begin
          i:= pos;
          let v4 = V4.of_string_raw s i in
          let (hi,lo) = V4.to_int16 v4 in
          lo :: hi :: acc
        end
        else x::acc
    in

    let res = loop (List.length l) l in
    let res_len = List.length res in
    if res_len > 8
    then raise (Parse_error ("too many components",s))
    else if res_len = 0
    then raise (need_more s)
    else
      let a = Array.make 8 0 in
      let missing =
        if !compressed
        then 8 - (res_len - 1)
        else if res_len <> 8
        then
          if !i < len
          then raise (bad_char !i s)
          else raise (need_more s)
        else 0
      in
      let _ = List.fold_left (fun i x ->
        if x = -1
        then i - missing
        else begin
          if x land 0xffff <> x
          then raise (Parse_error
                        (Printf.sprintf "component %d out of bounds" i, s));
          a.(i) <- x;
          i - 1
        end
      ) 7 res in
      (if use_bracket then expect_char s i ']');
      a

  (* string conversion *)

  let of_string_raw s offset =
    let a = parse_ipv6 s offset in
    make a.(0) a.(1) a.(2) a.(3) a.(4) a.(5) a.(6) a.(7)

  let of_string_exn s =
    let o = ref 0 in
    let x = of_string_raw s o in
    expect_end s o;
    x

  let of_string s = try_with_result of_string_exn s

  (* http://tools.ietf.org/html/rfc5952 *)
  let to_buffer buf addr =

    let (a,b,c,d,e,f,g,h) as comp = to_int16 addr in

    let v4 = match comp with
      | (0,0,0,0,0,0xffff,_,_) -> true
      | _ -> false
    in

    let rec loop elide zeros acc = function
      | 0 :: xs -> loop elide (zeros - 1) acc xs
      | n :: xs when zeros = 0 -> loop elide 0 (n::acc) xs
      | n :: xs -> loop (min elide zeros) 0 (n::zeros::acc) xs
      | [] ->
        let elide = min elide zeros in
        (if elide < -1 then Some elide else None),
        (if zeros = 0 then acc else zeros::acc)
    in

    let elide,l = loop 0 0 [] [h;g;f;e;d;c;b;a] in
    assert(match elide with Some x when x < -8 -> false | _ -> true);

    let rec cons_zeros l x =
      if x >= 0 then l else cons_zeros (Some 0::l) (x+1)
    in

    let _,lrev = List.fold_left (fun (patt, l) x ->
      if Some x = patt
      then (None, (None::l))
      else if x < 0
      then (patt, (cons_zeros l x))
      else (patt, ((Some x)::l))
    ) (elide, []) l in

    let rec fill = function
      | [Some hi;Some lo] when v4 ->
        let addr = V4.of_int16 (hi, lo) in
        V4.to_buffer buf addr
      | None::xs -> Buffer.add_string buf "::"; fill xs
      | [Some n] -> Printf.bprintf buf "%x" n
      | (Some n)::None::xs -> Printf.bprintf buf "%x::" n; fill xs
      | (Some n)::xs -> Printf.bprintf buf "%x:" n; fill xs
      | [] -> ()
    in fill (List.rev lrev)

  let to_string l =
    let buf = Buffer.create 39 in
    to_buffer buf l;
    Buffer.contents buf

  let pp ppf i =
    Format.fprintf ppf "%s" (to_string i)

  (* byte conversion *)

  let of_octets_exn ?(off=0) bs = (* TODO : from cstruct *)
    let hihi = V4.of_octets_exn ~off bs in
    let hilo = V4.of_octets_exn ~off:(off+4) bs in
    let lohi = V4.of_octets_exn ~off:(off+8) bs in
    let lolo = V4.of_octets_exn ~off:(off+12) bs in
    of_int32 (hihi, hilo, lohi, lolo)

  let of_octets ?off bs = try_with_result (of_octets_exn ?off) bs

  let write_octets ?off i bs = try_with_result (write_octets_exn ?off i) bs

  let to_octets i =
    let b = Bytes.create 16 in
    write_octets_exn i b;
    Bytes.to_string b

  (* MAC *)
  (* {{:https://tools.ietf.org/html/rfc2464#section-7}RFC 2464}. *)
  let multicast_to_mac i =
    let (_,_,_,i) = to_int32 i in
    let macb = Bytes.create 6 in
    Bytes.set macb 0 (Char.chr 0x33);
    Bytes.set macb 1 (Char.chr 0x33);
    Bytes.set macb 2 (Char.chr ((|~) (i >! 24)));
    Bytes.set macb 3 (Char.chr ((|~) (i >! 16)));
    Bytes.set macb 4 (Char.chr ((|~) (i >! 8)));
    Bytes.set macb 5 (Char.chr ((|~) (i >! 0)));
    Macaddr.of_octets_exn (Bytes.to_string macb)

  (* Host *)
  let to_domain_name (a,b,c,d) =
    let name = [
      hex_string_of_int32 ((d >|>  0) &&& 0xF_l);
      hex_string_of_int32 ((d >|>  4) &&& 0xF_l);
      hex_string_of_int32 ((d >|>  8) &&& 0xF_l);
      hex_string_of_int32 ((d >|> 12) &&& 0xF_l);
      hex_string_of_int32 ((d >|> 16) &&& 0xF_l);
      hex_string_of_int32 ((d >|> 20) &&& 0xF_l);
      hex_string_of_int32 ((d >|> 24) &&& 0xF_l);
      hex_string_of_int32 ((d >|> 28) &&& 0xF_l);
      hex_string_of_int32 ((c >|>  0) &&& 0xF_l);
      hex_string_of_int32 ((c >|>  4) &&& 0xF_l);
      hex_string_of_int32 ((c >|>  8) &&& 0xF_l);
      hex_string_of_int32 ((c >|> 12) &&& 0xF_l);
      hex_string_of_int32 ((c >|> 16) &&& 0xF_l);
      hex_string_of_int32 ((c >|> 20) &&& 0xF_l);
      hex_string_of_int32 ((c >|> 24) &&& 0xF_l);
      hex_string_of_int32 ((c >|> 28) &&& 0xF_l);
      hex_string_of_int32 ((b >|>  0) &&& 0xF_l);
      hex_string_of_int32 ((b >|>  4) &&& 0xF_l);
      hex_string_of_int32 ((b >|>  8) &&& 0xF_l);
      hex_string_of_int32 ((b >|> 12) &&& 0xF_l);
      hex_string_of_int32 ((b >|> 16) &&& 0xF_l);
      hex_string_of_int32 ((b >|> 20) &&& 0xF_l);
      hex_string_of_int32 ((b >|> 24) &&& 0xF_l);
      hex_string_of_int32 ((b >|> 28) &&& 0xF_l);
      hex_string_of_int32 ((a >|>  0) &&& 0xF_l);
      hex_string_of_int32 ((a >|>  4) &&& 0xF_l);
      hex_string_of_int32 ((a >|>  8) &&& 0xF_l);
      hex_string_of_int32 ((a >|> 12) &&& 0xF_l);
      hex_string_of_int32 ((a >|> 16) &&& 0xF_l);
      hex_string_of_int32 ((a >|> 20) &&& 0xF_l);
      hex_string_of_int32 ((a >|> 24) &&& 0xF_l);
      hex_string_of_int32 ((a >|> 28) &&& 0xF_l);
      "ip6";
      "arpa"
    ]
    in
    Domain_name.(host_exn (of_strings_exn name))

  let of_domain_name n =
    let open Domain_name in
    if count_labels n = 34 then
      let ip6 = get_label_exn n 32 and arpa = get_label_exn n 33 in
      if equal_label ip6 "ip6" && equal_label arpa "arpa" then
        let rev = true in
        let n' = drop_label_exn ~rev ~amount:2 n in
        let d = drop_label_exn ~rev ~amount:24 n'
        and c = drop_label_exn ~amount:8 (drop_label_exn ~rev ~amount:16 n')
        and b = drop_label_exn ~amount:16 (drop_label_exn ~rev ~amount:8 n')
        and a = drop_label_exn ~amount:24 n'
        in
        let t b d =
          let v = Int32.of_int (parse_hex_int d (ref 0)) in
          if v > 0xFl then
            raise (Parse_error ("number in label too big", d))
          else
            v <|< b
        in
        let f d =
          List.fold_left (fun (acc, b) d -> Int32.add acc (t b d), b + 4)
            (0l, 0) (to_strings d)
        in
        try
          let a', _ = f a and b', _ = f b and c', _ = f c and d', _ = f d in
          Some (a', b', c', d')
        with
        | Parse_error _ -> None
      else
        None
    else
      None

  (* constant *)

  let unspecified       = make      0 0 0 0 0 0 0 0
  let localhost         = make      0 0 0 0 0 0 0 1
  let interface_nodes   = make 0xff01 0 0 0 0 0 0 1
  let link_nodes        = make 0xff02 0 0 0 0 0 0 1
  let interface_routers = make 0xff01 0 0 0 0 0 0 2
  let link_routers      = make 0xff02 0 0 0 0 0 0 2
  let site_routers      = make 0xff05 0 0 0 0 0 0 2

  module Prefix = struct
    type addr = t
    type t = addr * int

    let compare (pre,sz) (pre',sz') =
      let c = compare pre pre' in
      if c = 0 then Stdlib.compare sz sz' else c

    let ip = make

    let _full =
      let f = 0x0_FFFF_FFFF_l in
      f,f,f,f

    let mask sz = V4.Prefix.(
      mask (sz -  0),
      mask (sz - 32),
      mask (sz - 64),
      mask (sz - 96))

    let prefix (pre,sz) = (logand pre (mask sz),sz)

    let make sz pre = (pre,sz)

    let network_address (pre,sz) addr =
      logor (logand pre (mask sz)) (logand addr (lognot (mask sz)))

    let _of_string_raw s i =
      let v6 = of_string_raw s i in
      expect_char s i '/';
      let p = parse_dec_int s i in
      if p > 128 || p < 0
      then raise (Parse_error ("invalid prefix size", s));
      (p, v6)

    let of_string_raw s i =
      let (p,v6) = _of_string_raw s i in
      make p v6

    let _of_string_exn s =
      let i = ref 0 in
      let res = _of_string_raw s i in
      expect_end s i;
      res

    let of_string_exn s = let (p,v6) = _of_string_exn s in make p v6

    let of_string s = try_with_result of_string_exn s

    let _of_netmask_exn ~netmask address =
      let nm =
        let bits netmask =
          V4.Prefix.bits (V4.Prefix.of_netmask_exn ~netmask ~address:V4.any)
        in
        match netmask with
        | (0_l,0_l,0_l,0_l) -> 0
        | (lsw ,0_l ,0_l ,0_l) -> bits lsw
        | (-1_l,lsw ,0_l ,0_l) -> bits lsw + 32
        | (-1_l,-1_l,lsw ,0_l) -> bits lsw + 64
        | (-1_l,-1_l,-1_l,lsw) -> bits lsw + 96
        | _ -> raise (Parse_error ("invalid netmask", to_string netmask))
      in
      make nm address

    let of_netmask_exn ~netmask ~address = _of_netmask_exn ~netmask address

    let of_netmask ~netmask ~address =
      try_with_result (_of_netmask_exn ~netmask) address

    let to_buffer buf (pre,sz) =
      Printf.bprintf buf "%a/%d" to_buffer pre sz

    let to_string subnet =
      let buf = Buffer.create 43 in
      to_buffer buf subnet;
      Buffer.contents buf

    let pp ppf i =
      Format.fprintf ppf "%s" (to_string i)

    let mem ip (pre,sz) =
      let m = mask sz in
      logand ip m = logand pre m

    let subset ~subnet:(pre1,sz1) ~network:(pre2,sz2) =
      sz1 >= sz2 && mem pre1 (pre2,sz2)

    let of_addr ip = make 128 ip

    let global_unicast_001  = make   3 (ip 0x2000 0 0 0 0 0 0 0)
    let link                = make  64 (ip 0xfe80 0 0 0 0 0 0 0)
    let unique_local        = make   7 (ip 0xfc00 0 0 0 0 0 0 0)
    let multicast           = make   8 (ip 0xff00 0 0 0 0 0 0 0)
    let ipv4_mapped         = make  96 (ip 0 0 0 0 0 0xffff 0 0)
    let noneui64_interface  = make   3 (ip 0x0000 0 0 0 0 0 0 0)
    let solicited_node      = make 104 (ip 0xff02 0 0 0 0 1 0xff00 0)

    let network (pre,sz) = logand pre (mask sz)
    let address (addr,_) = addr
    let bits (_,sz) = sz
    let netmask subnet = mask (bits subnet)

    let first (_,sz as cidr) =
      if sz > 126 then
        network cidr
      else
        network cidr |> succ |> failwith_msg

    let last (_,sz as cidr) =
      let ffff = ip 0xffff 0xffff 0xffff 0xffff
          0xffff 0xffff 0xffff 0xffff in
      logor (network cidr) (shift_right ffff sz |> failwith_msg)
  end

  (* TODO: This could be optimized with something trie-like *)
  let scope i =
    let mem = Prefix.mem i in
    if mem Prefix.global_unicast_001 then Global
    else if mem Prefix.ipv4_mapped
    (* rfc says they are technically global but... *)
    then V4.scope (let (_,_,_,v4) = to_int32 i in V4.of_int32 v4)
    else if mem Prefix.multicast then
      let (x,_,_,_,_,_,_,_) = to_int16 i in
      match x land 0xf with
      | 0 -> Point
      | 1 -> Interface
      | 2 | 3 -> Link
      | 4 -> Admin
      | 5 | 6 | 7 -> Site
      | 8 | 9 | 10 | 11 | 12 | 13 -> Organization
      | 14 | 15 -> Global
      | _ -> assert false
    else if mem Prefix.link then Link
    else if mem Prefix.unique_local then Global
    else if i = localhost then Interface
    else if i = unspecified then Point
    else Global

  let link_address_of_mac =
    let c b i = Char.code (String.get b i) in
    fun mac ->
      let bmac = Macaddr.to_octets mac in
      let c_0 = c bmac 0 lxor 2 in
      let addr = make 0 0 0 0
        (c_0      lsl 8 + c bmac 1)
        (c bmac 2 lsl 8 + 0xff    )
        (0xfe00         + c bmac 3)
        (c bmac 4 lsl 8 + c bmac 5)
      in
      Prefix.(network_address link addr)

  let is_global i = (scope i) = Global
  let is_multicast i = Prefix.(mem i multicast)
  let is_private i = (scope i) <> Global
end

type ('v4,'v6) v4v6 = V4 of 'v4 | V6 of 'v6
type t = (V4.t,V6.t) v4v6

let compare a b = match a,b with
  | V4 a, V4 b -> V4.compare a b
  | V6 a, V6 b -> V6.compare a b
  | V4 _, V6 _ -> -1
  | V6 _, V4 _ -> 1

let to_string = function
  | V4 x -> V4.to_string x
  | V6 x -> V6.to_string x

let to_buffer buf = function
  | V4 x -> V4.to_buffer buf x
  | V6 x -> V6.to_buffer buf x

let pp ppf i =
      Format.fprintf ppf "%s" (to_string i)

let of_string_raw s offset =
  let len = String.length s in
  if len < !offset + 1 then raise (need_more s);
  match s.[0] with
    | '[' -> V6 (V6.of_string_raw s offset)
    | _ ->
      let pos = !offset in
      try V4 (V4.of_string_raw s offset)
      with Parse_error (v4_msg,_) ->
        offset := pos;
        try V6 (V6.of_string_raw s offset)
        with Parse_error(v6_msg,s) ->
          let msg = Printf.sprintf
            "not an IPv4 address: %s\nnot an IPv6 address: %s"
            v4_msg v6_msg
          in raise (Parse_error (msg,s))

let of_string_exn s = of_string_raw s (ref 0)

let of_string s = try_with_result of_string_exn s

let v6_of_v4 v4 =
  V6.(Prefix.(network_address ipv4_mapped (of_int32 (0l,0l,0l,v4))))

let v4_of_v6 v6 =
  if V6.Prefix.(mem v6 ipv4_mapped)
  then let (_,_,_,v4) = V6.to_int32 v6 in Some V4.(of_int32 v4)
  else None

let to_v4 = function V4 v4 -> Some v4 | V6 v6 -> v4_of_v6 v6

let to_v6 = function V4 v4 -> v6_of_v4 v4 | V6 v6 -> v6

let scope = function V4 v4 -> V4.scope v4 | V6 v6 -> V6.scope v6

let is_global = function
  | V4 v4 -> V4.is_global v4
  | V6 v6 -> V6.is_global v6

let is_multicast = function
  | V4 v4 -> V4.is_multicast v4
  | V6 v6 -> V6.is_multicast v6

let is_private = function
  | V4 v4 -> V4.is_private v4
  | V6 v6 -> V6.is_private v6

let multicast_to_mac = function
  | V4 v4 -> V4.multicast_to_mac v4
  | V6 v6 -> V6.multicast_to_mac v6

let to_domain_name = function
  | V4 v4 -> V4.to_domain_name v4
  | V6 v6 -> V6.to_domain_name v6

let of_domain_name n =
  match Domain_name.count_labels n with
  | 6 ->
    begin match V4.of_domain_name n with
      | None -> None
      | Some x -> Some (V4 x)
    end
  | 34 ->
    begin match V6.of_domain_name n with
      | None -> None
      | Some x -> Some (V6 x)
    end
  | _ -> None

let succ = function
  | V4 addr -> map_result (V4.succ addr) (fun v -> V4 v)
  | V6 addr -> map_result (V6.succ addr) (fun v -> V6 v)

let pred = function
  | V4 addr -> map_result (V4.pred addr) (fun v -> V4 v)
  | V6 addr -> map_result (V6.pred addr) (fun v -> V6 v)

module Prefix = struct
  module Addr = struct
    let to_v6 = to_v6
  end

  type addr = t
  type t = (V4.Prefix.t,V6.Prefix.t) v4v6

  let compare a b = match a,b with
    | V4 a , V4 b -> V4.Prefix.compare a b
    | V6 a , V6 b -> V6.Prefix.compare a b
    | V4 _ , V6 _ -> -1
    | V6 _ , V4 _ -> 1

  let of_string_raw s offset =
    let len = String.length s in
    if len < !offset + 1 then raise (need_more s);
    match s.[0] with
      | '[' -> V6 (V6.Prefix.of_string_raw s offset)
      | _ ->
        let pos = !offset in
        try V4 (V4.Prefix.of_string_raw s offset)
        with Parse_error (v4_msg,_) ->
          offset := pos;
          try V6 (V6.Prefix.of_string_raw s offset)
          with Parse_error(v6_msg,s) ->
            let msg = Printf.sprintf
                "not an IPv4 prefix: %s\nnot an IPv6 prefix: %s"
                v4_msg v6_msg
            in raise (Parse_error (msg,s))

  let of_string_exn s = of_string_raw s (ref 0)

  let of_string s = try_with_result of_string_exn s

  let v6_of_v4 v4 = V6.Prefix.make
    (96 + V4.Prefix.bits v4)
    (v6_of_v4 (V4.Prefix.network v4))

  let v4_of_v6 v6 = match v4_of_v6 (V6.Prefix.network v6) with
    | Some v4 -> Some (V4.Prefix.make (V6.Prefix.bits v6 - 96) v4)
    | None -> None

  let to_v4 = function V4 v4 -> Some v4 | V6 v6 -> v4_of_v6 v6

  let to_v6 = function V4 v4 -> v6_of_v4 v4 | V6 v6 -> v6

  let mem ip prefix = V6.Prefix.mem (Addr.to_v6 ip) (to_v6 prefix)

  let subset ~subnet ~network =
    V6.Prefix.subset ~subnet:(to_v6 subnet) ~network:(to_v6 network)

  let of_addr = function
    | V4 p -> V4 (V4.Prefix.of_addr p)
    | V6 p -> V6 (V6.Prefix.of_addr p)

  let to_string = function
    | V4 p -> V4.Prefix.to_string p
    | V6 p -> V6.Prefix.to_string p

  let to_buffer buf = function
    | V4 p -> V4.Prefix.to_buffer buf p
    | V6 p -> V6.Prefix.to_buffer buf p

  let network = function
    | V4 p -> V4 (V4.Prefix.network p)
    | V6 p -> V6 (V6.Prefix.network p)

  let netmask = function
    | V4 p -> V4 (V4.Prefix.netmask p)
    | V6 p -> V6 (V6.Prefix.netmask p)

  let pp ppf i =
    Format.fprintf ppf "%s" (to_string i)

  let first = function
    | V4 p -> V4 (V4.Prefix.first p)
    | V6 p -> V6 (V6.Prefix.first p)

  let last = function
    | V4 p -> V4 (V4.Prefix.last p)
    | V6 p -> V6 (V6.Prefix.last p)

end
