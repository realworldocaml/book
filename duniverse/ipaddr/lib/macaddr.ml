(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
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

exception Parse_error of string * string

let need_more x = Parse_error ("not enough data", x)

let try_with_result fn a =
  try Ok (fn a)
  with Parse_error (msg, _) -> Error (`Msg ("Macaddr: " ^ msg))

type t = Bytes.t (* length 6 only *)

let compare = Bytes.compare

(* Raw MAC address off the wire (network endian) *)
let of_octets_exn x =
  if String.length x <> 6
  then raise (Parse_error ("MAC is exactly 6 bytes", x))
  else Bytes.of_string x

let of_octets x = try_with_result of_octets_exn x

let int_of_hex_char c =
  let c = int_of_char (Char.uppercase_ascii c) - 48 in
  if c > 9
  then if c > 16
    then c - 7 (* upper hex offset *)
    else -1 (* :;<=>?@ *)
  else c

let is_hex i = i >=0 && i < 16

let bad_char i s =
  let msg = Printf.sprintf "invalid character '%c' at %d" s.[i] i
  in Parse_error (msg, s)

let parse_hex_int term s i =
  let len = String.length s in
  let rec hex prev =
    let j = !i in
    if j >= len then prev
    else let c = s.[j] in
         let k = int_of_hex_char c in
         if is_hex k
         then (incr i; hex ((prev lsl 4) + k))
         else if List.mem c term
         then prev
         else raise (bad_char j s)
  in
  let i = !i in
  if i < len
  then if is_hex (int_of_hex_char s.[i])
    then hex 0
    else raise (bad_char i s)
  else raise (need_more s)

let parse_sextuple s i =
  let m = Bytes.create 6 in
  try
    let p = !i in
    Bytes.set m 0 (Char.chr (parse_hex_int [':';'-'] s i));
    if !i >= String.length s
    then raise (need_more s)
    else
      let sep = [s.[!i]] in
      (if !i - p <> 2 then raise (Parse_error ("hex pairs required",s)));
      incr i;
      for k=1 to 4 do
        let p = !i in
        Bytes.set m k (Char.chr (parse_hex_int sep s i));
        (if !i - p <> 2 then raise (Parse_error ("hex pairs required",s)));
        incr i;
      done;
      let p = !i in
      Bytes.set m 5 (Char.chr (parse_hex_int [] s i));
      (if !i - p <> 2 then raise (Parse_error ("hex pairs required",s)));
      m
  with Invalid_argument _ ->
    raise (Parse_error ("address segment too large",s))

(* Read a MAC address colon-separated string *)
let of_string_exn x = parse_sextuple x (ref 0)

let of_string x = try_with_result of_string_exn x

let chri x i = Char.code (Bytes.get x i)

let to_string ?(sep=':') x =
  Printf.sprintf "%02x%c%02x%c%02x%c%02x%c%02x%c%02x"
    (chri x 0) sep
    (chri x 1) sep
    (chri x 2) sep
    (chri x 3) sep
    (chri x 4) sep
    (chri x 5)

let to_octets x = Bytes.to_string x

let pp ppf i =
  Format.fprintf ppf "%s" (to_string i)

let broadcast = Bytes.make 6 '\255'

let make_local bytegenf =
  let x = Bytes.create 6 in
  (* set locally administered and unicast bits *)
  Bytes.set x 0 (Char.chr ((((bytegenf 0) lor 2) lsr 1) lsl 1));
  for i = 1 to 5 do Bytes.set x i (Char.chr (bytegenf i)) done;
  x

let get_oui x =
  ((chri x 0) lsl 16) lor ((chri x 1) lsl 8) lor (chri x 2)

let is_local x = (((chri x 0) lsr 1) land 1) = 1

let is_unicast x = ((chri x 0) land 1) = 0
