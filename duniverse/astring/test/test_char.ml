(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Astring

let eq = eq ~pp:Char.dump
let eq_opt = eq_option ~pp:Char.dump ~eq:Char.equal
let invalid = app_invalid ~pp:Char.dump

let misc = test "Char.{of_byte,of_int,to_int}" @@ fun () ->
  invalid Char.of_byte (-1);
  invalid Char.of_byte (256);
  eq_opt (Char.of_int (-1)) None;
  eq_opt (Char.of_int 256) None;
  for i = 0 to 0xFF do
    let of_int = Char.of_int $ pp_int @-> ret_get_option Char.dump in
    eq_int (Char.to_int (of_int i)) i
  done;
  ()

let predicates = test "Char.{equal,compare}" @@ fun () ->
  eq_bool (Char.equal ' ' ' ') true;
  eq_bool (Char.equal ' ' 'a') false;
  eq_int (Char.compare ' ' 'a') (-1);
  eq_int (Char.compare ' ' ' ') (0);
  eq_int (Char.compare 'a' ' ') (1);
  eq_int (Char.compare '\x00' ' ') (-1);
  ()

let ascii_predicates = test "Char.Ascii.is_*" @@ fun () ->
  let pp_int ppf i = Format.fprintf ppf "%X" i in
  let test_pred p pi i =
    let pred p i = p (Char.of_byte i) in
    (pred p $ pp_int @-> ret_eq ~eq:(=) pp_bool (pi i)) i
  in
  let test p pi = for i = 0 to 255 do ignore (test_pred p pi i) done in
  test Char.Ascii.is_valid (fun i -> i <= 0x7F);
  test Char.Ascii.is_digit (fun i -> 0x30 <= i && i <= 0x39);
  test Char.Ascii.is_hex_digit (fun i -> (0x30 <= i && i <= 0x39) ||
                                         (0x41 <= i && i <= 0x46) ||
                                         (0x61 <= i && i <= 0x66));
  test Char.Ascii.is_upper (fun i -> 0x41 <= i && i <= 0x5A);
  test Char.Ascii.is_lower (fun i -> 0x61 <= i && i <= 0x7A);
  test Char.Ascii.is_letter (fun i -> (0x41 <= i && i <= 0x5A) ||
                                      (0x61 <= i && i <= 0x7A));
  test Char.Ascii.is_alphanum (fun i -> (0x30 <= i && i <= 0x39) ||
                                        (0x41 <= i && i <= 0x5A) ||
                                        (0x61 <= i && i <= 0x7A));
  test Char.Ascii.is_white (fun i -> (0x09 <= i && i <= 0x0D) || i = 0x20);
  test Char.Ascii.is_blank (fun i -> (i = 0x20 || i = 0x09));
  test Char.Ascii.is_graphic (fun i -> (0x21 <= i && i <= 0x7E));
  test Char.Ascii.is_print (fun i -> (0x21 <= i && i <= 0x7E) || i = 0x20);
  test Char.Ascii.is_control (fun i -> (0x00 <= i && i <= 0x1F) || i = 0x7F);
  ()

let ascii_transforms = test "Char.Ascii.{uppercase,lowercase}" @@ fun () ->
  for i = 0 to 255 do
    if (0x61 <= i && i <= 0x7A)
    then eq_char Char.(Ascii.uppercase @@ of_byte i) (Char.of_byte (i - 32))
    else eq_char Char.(Ascii.uppercase @@ of_byte i) (Char.of_byte i)
  done;
  for i = 0 to 255 do
    if (0x41 <= i && i <= 0x5A)
    then eq_char Char.(Ascii.lowercase @@ of_byte i) (Char.of_byte (i + 32))
    else eq_char Char.(Ascii.lowercase @@ of_byte i) (Char.of_byte i)
  done;
  ()

let ascii_escape = test "Char.Ascii.{escape,escape_char}" @@ fun () ->
  for i = 0 to 255 do
    let c = Char.of_byte i in
    let esc = Char.Ascii.escape c in
    begin match String.Ascii.unescape esc with
    | None -> fail "could not unescape";
    | Some unesc ->
        eq_int (String.length unesc) 1;
        eq_char unesc.[0] (Char.of_byte i);
    end;
    if (0x00 <= i && i <= 0x1F) || (0x7F <= i && i <= 0xFF)
    then eq_str esc (Printf.sprintf "\\x%02X" i)
    else if (i = 0x5C)
    then eq_str esc "\\\\"
    else eq_str esc (Printf.sprintf "%c" c)
  done;
  for i = 0 to 255 do
    let c = Char.of_byte i in
    let esc = Char.Ascii.escape_char c in
    begin match String.Ascii.unescape_string esc with
    | None -> fail "could not unescape";
    | Some unesc ->
        eq_int (String.length unesc) 1;
        eq_char unesc.[0] (Char.of_byte i);
    end;
    if (i = 0x08) then eq_str esc "\\b" else
    if (i = 0x09) then eq_str esc "\\t" else
    if (i = 0x0A) then eq_str esc "\\n" else
    if (i = 0x0D) then eq_str esc "\\r" else
    if (i = 0x27) then eq_str esc "\\'" else
    if (i = 0x5C) then eq_str esc "\\\\" else
    if (0x00 <= i && i <= 0x1F) || (0x7F <= i && i <= 0xFF)
    then eq_str esc (Printf.sprintf "\\x%02X" i)
    else eq_str esc (Printf.sprintf "%c" c)
  done;
  ()

let suite = suite "Char functions"
    [ misc;
      predicates;
      ascii_predicates;
      ascii_transforms;
      ascii_escape; ]

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
