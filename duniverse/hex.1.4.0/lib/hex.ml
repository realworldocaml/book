(*
 * Copyright (c) 2015 Trevor Summers Smith <trevorsummerssmith@gmail.com>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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
 *)

module Bigarray = Bigarray_compat

type t = [`Hex of string]

let invalid_arg fmt =
  Printf.ksprintf (fun str -> raise (Invalid_argument str)) fmt

let hexa = "0123456789abcdef"
and hexa1 =
  "0000000000000000111111111111111122222222222222223333333333333333\
   4444444444444444555555555555555566666666666666667777777777777777\
   88888888888888889999999999999999aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbb\
   ccccccccccccccccddddddddddddddddeeeeeeeeeeeeeeeeffffffffffffffff"
and hexa2 =
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\
   0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\
   0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\
   0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

let of_char c =
  let x = Char.code c in
  hexa.[x lsr 4], hexa.[x land 0xf]

let to_char x y =
  let code c = match c with
    | '0'..'9' -> Char.code c - 48 (* Char.code '0' *)
    | 'A'..'F' -> Char.code c - 55 (* Char.code 'A' + 10 *)
    | 'a'..'f' -> Char.code c - 87 (* Char.code 'a' + 10 *)
    | _ -> invalid_arg "Hex.to_char: %d is an invalid char" (Char.code c)
  in
  Char.chr (code x lsl 4 + code y)

let of_string_fast s =
  let len = String.length s in
  let buf = Bytes.create (len * 2) in
  for i = 0 to len - 1 do
    Bytes.unsafe_set buf (i * 2)
      (String.unsafe_get hexa1 (Char.code (String.unsafe_get s i)));
    Bytes.unsafe_set buf (succ (i * 2))
      (String.unsafe_get hexa2 (Char.code (String.unsafe_get s i)));
  done;
  `Hex (Bytes.to_string buf)

let of_helper ~ignore (next : int -> char) len =
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    let c = next i in
    if List.mem c ignore then ()
    else
      let x,y = of_char c in
      Buffer.add_char buf x;
      Buffer.add_char buf y;
  done;
  `Hex (Buffer.contents buf)

let of_string ?ignore s =
  match ignore with
    None -> of_string_fast s
  | Some ignore -> of_helper ~ignore (fun i -> s.[i]) (String.length s)

let of_bytes ?ignore b =
  of_string ?ignore (Bytes.to_string b)

let to_helper ~empty_return ~create ~set (`Hex s) =
  if s = "" then empty_return
  else
    let n = String.length s in
    let buf = create (n/2) in
    let rec aux i j =
      if i >= n then ()
      else if j >= n then invalid_arg "hex conversion: invalid hex string"
      else (
        set buf (i/2) (to_char s.[i] s.[j]);
        aux (j+1) (j+2)
      )
    in
    aux 0 1;
    buf

let to_bytes hex =
  to_helper ~empty_return:Bytes.empty ~create:Bytes.create ~set:Bytes.set hex

let to_string hex = Bytes.to_string @@ to_bytes hex

let of_cstruct ?(ignore=[]) cs =
  let open Cstruct in
  of_helper
    ~ignore
    (fun i -> Bigarray.Array1.get cs.buffer (cs.off+i))
    cs.len

(* Allocate just once for to_cstruct *)
let empty_cstruct = Cstruct.of_string ""

let to_cstruct hex =
  to_helper
    ~empty_return:empty_cstruct ~create:Cstruct.create ~set:Cstruct.set_char hex

let of_bigstring ?(ignore=[]) buf =
  of_helper
    ~ignore
    (Bigarray.Array1.get buf)
    (Bigarray.Array1.dim buf)

let to_bigstring hex =
  to_helper
    ~empty_return:empty_cstruct.buffer
    ~create:Bigarray.(Array1.create char c_layout)
    ~set:Bigarray.Array1.set hex

let hexdump_s ?(print_row_numbers=true) ?(print_chars=true) (`Hex s) =
  let char_len = 16 in (* row width in # chars *)
  let hex_len = char_len * 2 in (* row width in # hex chars *)
  (* Buf length is roughly 4... could put this in exactly but very brittle *)
  let buf = Buffer.create ((String.length s) * 4) in
  let ( <= ) buf s = Buffer.add_string buf s in
  (* Create three columns -- row #, hex and ascii chars*)
  let n = String.length s in
  let rows = (n / hex_len) + (if n mod hex_len = 0 then 0 else 1) in
  for row = 0 to rows-1 do
    let last_row = row = rows-1 in
    (* First column is row number *)
    if print_row_numbers then
      buf <= Printf.sprintf "%.8d: " row;
    (* Row length is hex_length, unless we are on the last row and we
       have less than hex_length left *)
    let row_len = if last_row then
        (let rem = n mod hex_len in
         if rem = 0 then hex_len else rem)
      else hex_len in
    for i = 0 to row_len-1 do
      (* Second column is the hex *)
      if i mod 4 = 0 && i <> 0 then buf <= Printf.sprintf " ";
      let i = i + (row * hex_len) in
      buf <= Printf.sprintf "%c" (String.get s i)
    done;
    (* This is only needed for the last row -- pad if less than len *)
    if last_row then
      let missed_chars = hex_len - row_len in
      let pad = missed_chars in
      (* Every four chars add spacing *)
      let pad = pad + (missed_chars / 4) in
      buf <= Printf.sprintf "%s" (String.make pad ' ')
    else ();
    (* Third column is ascii *)
    if print_chars then begin
      buf <= "  ";
      let rec aux i j =
        if i > row_len - 2 then ()
        else begin
          let pos = i + (row * hex_len) in
          let pos' = pos + 1 in
          let c = to_char (String.get s pos) (String.get s pos') in
          let () = match c with
            | '\t' | '\n' -> buf <= "."
            | _ -> buf <= Printf.sprintf "%c" c
          in ();
          aux (j+1) (j+2)
        end
      in
      aux 0 1;
    end;
    buf <= "\n";
  done;
  Buffer.contents buf

let hexdump ?print_row_numbers ?print_chars hex =
  Printf.printf "%s" (hexdump_s ?print_row_numbers ?print_chars hex)

let pp ppf (`Hex hex) =
  Format.pp_print_string ppf hex

let show (`Hex hex) = hex
