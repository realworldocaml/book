(*
 * Copyright (c) 2016 Anil Madhavapeddy <anil@recoil.org>
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

open Printf
open Rresult

(* Test vectors from RFC4648
   BASE64("") = ""
   BASE64("f") = "Zg=="
   BASE64("fo") = "Zm8="
   BASE64("foo") = "Zm9v"
   BASE64("foob") = "Zm9vYg=="
   BASE64("fooba") = "Zm9vYmE="
   BASE64("foobar") = "Zm9vYmFy"
*)

let rfc4648_tests = [
  "", "";
  "f", "Zg==";
  "fo", "Zm8=";
  "foo", "Zm9v";
  "foob", "Zm9vYg==";
  "fooba", "Zm9vYmE=";
  "foobar", "Zm9vYmFy";
]

let hannes_tests = [
  "dummy", "ZHVtbXk=";
  "dummy", "ZHVtbXk";
  "dummy", "ZHVtbXk==";
  "dummy", "ZHVtbXk===";
  "dummy", "ZHVtbXk====";
  "dummy", "ZHVtbXk=====";
  "dummy", "ZHVtbXk======";
]

let php_tests = [
  "πάντα χωρεῖ καὶ οὐδὲν μένει …", "z4DOrM69z4TOsSDPh8-Jz4HOteG_liDOus6x4b22IM6_4b2QzrThvbLOvSDOvM6tzr3Otc65IOKApg"
]

let rfc3548_tests = [
  "\x14\xfb\x9c\x03\xd9\x7e", "FPucA9l+";
  "\x14\xfb\x9c\x03\xd9", "FPucA9k=";
  "\x14\xfb\x9c\x03", "FPucAw==";
]

let cfcs_tests = [
  0, 2, "\004", "BB";
  1, 2, "\004", "ABB";
  1, 2, "\004", "ABBA";
  2, 2, "\004", "AABBA";
  2, 2, "\004", "AABBAA";
  0, 0, "", "BB";
  1, 0, "", "BB";
  2, 0, "", "BB";
]

let nocrypto_tests =
  [ "\x00\x5a\x6d\x39\x76", None
  ; "\x5a\x6d\x39\x76", Some "\x66\x6f\x6f"
  ; "\x5a\x6d\x39\x76\x76", None
  ; "\x5a\x6d\x39\x76\x76\x76", None
  ; "\x5a\x6d\x39\x76\x76\x76\x76", None
  ; "\x5a\x6d\x39\x76\x00", None
  ; "\x5a\x6d\x39\x76\x62\x77\x3d\x3d", Some "\x66\x6f\x6f\x6f"
  ; "\x5a\x6d\x39\x76\x62\x77\x3d\x3d\x00", None
  ; "\x5a\x6d\x39\x76\x62\x77\x3d\x3d\x00\x01", None
  ; "\x5a\x6d\x39\x76\x62\x77\x3d\x3d\x00\x01\x02", None
  ; "\x5a\x6d\x39\x76\x62\x77\x3d\x3d\x00\x01\x02\x03", None
  ; "\x5a\x6d\x39\x76\x62\x32\x38\x3d", Some "\x66\x6f\x6f\x6f\x6f"
  ; "\x5a\x6d\x39\x76\x62\x32\x39\x76", Some "\x66\x6f\x6f\x6f\x6f\x6f"
  ; "YWE=", Some "aa"
  ; "YWE==", None
  ; "YWE===", None
  ; "YWE=====", None
  ; "YWE======", None ]

let alphabet_size () =
  List.iter (fun (name,alphabet) ->
    Alcotest.(check int) (sprintf "Alphabet size %s = 64" name)
     64 (Base64.length_alphabet alphabet))
     ["default",Base64.default_alphabet; "uri_safe",Base64.uri_safe_alphabet]

(* Encode using OpenSSL `base64` utility *)
let openssl_encode buf =
  Bos.(OS.Cmd.in_string buf |> OS.Cmd.run_io (Cmd.v "base64") |> OS.Cmd.to_string ~trim:true) |>
  function | Ok r -> prerr_endline r; r | Error (`Msg e) -> raise (Failure (sprintf "OpenSSL decode: %s" e))

(* Encode using this library *)
let lib_encode buf =
  Base64.encode_exn ~pad:true buf

let test_rfc4648 () =
  List.iter (fun (c,r) ->
    (* Base64 vs openssl *)
    Alcotest.(check string) (sprintf "encode %s" c) (openssl_encode c) (lib_encode c);
    (* Base64 vs test cases above *)
    Alcotest.(check string) (sprintf "encode rfc4648 %s" c) r (lib_encode c);
    (* Base64 decode vs library *)
    Alcotest.(check string) (sprintf "decode %s" r) c (Base64.decode_exn r);
  ) rfc4648_tests

let test_rfc3548 () =
  List.iter (fun (c,r) ->
    (* Base64 vs openssl *)
    Alcotest.(check string) (sprintf "encode %s" c) (openssl_encode c) (lib_encode c);
    (* Base64 vs test cases above *)
    Alcotest.(check string) (sprintf "encode rfc3548 %s" c) r (lib_encode c);
    (* Base64 decode vs library *)
    Alcotest.(check string) (sprintf "decode %s" r) c (Base64.decode_exn r);
  ) rfc3548_tests

let test_hannes () =
  List.iter (fun (c,r) ->
    (* Base64 vs test cases above *)
    Alcotest.(check string) (sprintf "decode %s" r) c (Base64.decode_exn ~pad:false r);
  ) hannes_tests

let test_php () =
  List.iter (fun (c,r) ->
    Alcotest.(check string) (sprintf "decode %s" r) c (Base64.decode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet r);
  ) php_tests

let test_cfcs () =
  List.iter (fun (off, len, c,r) ->
    Alcotest.(check string) (sprintf "decode %s" r) c (Base64.decode_exn ~pad:false ~off ~len r);
  ) cfcs_tests 

let test_nocrypto () =
  List.iter (fun (input, res) ->
    let res' = match Base64.decode ~pad:true input with
      | Ok v -> Some v
      | Error _ -> None in
    Alcotest.(check (option string)) (sprintf "decode %S" input) res' res ;
  ) nocrypto_tests

exception Malformed
exception Wrong_padding

let strict_base64_rfc2045_of_string x =
  let decoder = Base64_rfc2045.decoder (`String x) in
  let res = Buffer.create 16 in

  let rec go () = match Base64_rfc2045.decode decoder with
  | `End -> ()
  | `Wrong_padding -> raise Wrong_padding
  | `Malformed _ -> raise Malformed
  | `Flush x -> Buffer.add_string res x ; go ()
  | `Await -> Alcotest.failf "Retrieve impossible case: `Await" in

  Base64_rfc2045.src decoder (Bytes.unsafe_of_string x) 0 (String.length x) ;
  go () ; Buffer.contents res

let relaxed_base64_rfc2045_of_string x =
  let decoder = Base64_rfc2045.decoder (`String x) in
  let res = Buffer.create 16 in

  let rec go () = match Base64_rfc2045.decode decoder with
  | `End -> ()
  | `Wrong_padding -> go ()
  | `Malformed _ -> go ()
  | `Flush x -> Buffer.add_string res x ; go ()
  | `Await -> Alcotest.failf "Retrieve impossible case: `Await" in

  Base64_rfc2045.src decoder (Bytes.unsafe_of_string x) 0 (String.length x) ;
  go () ; Buffer.contents res

let test_strict_rfc2045 =
  [ "c2FsdXQgbGVzIGNvcGFpbnMgZmF1dCBhYnNvbHVtZW50IHF1ZSBqZSBkw6lwYXNzZSBsZXMgODAg\r\n\
     Y2hhcmFjdGVycyBwb3VyIHZvaXIgc2kgbW9uIGVuY29kZXIgZml0cyBiaWVuIGRhbnMgbGVzIGxp\r\n\
     bWl0ZXMgZGUgbGEgUkZDIDIwNDUgLi4u",
    "salut les copains faut absolument que je dépasse les 80 characters pour voir si \
     mon encoder fits bien dans les limites de la RFC 2045 ..."
  ; "", ""
  ; "Zg==", "f"
  ; "Zm8=", "fo"
  ; "Zm9v", "foo"
  ; "Zm9vYg==", "foob"
  ; "Zm9vYmE=", "fooba"
  ; "Zm9vYmFy", "foobar" ]

let test_relaxed_rfc2045 =
  [ "Zg", "f"
  ; "Zm\n8", "fo"
  ; "Zm\r9v", "foo"
  ; "Zm9 vYg", "foob"
  ; "Zm9\r\n vYmE", "fooba"
  ; "Zm9évYmFy", "foobar" ]

let strict_base64_rfc2045_to_string x =
  let res = Buffer.create 16 in
  let encoder = Base64_rfc2045.encoder (`Buffer res) in
  String.iter
    (fun chr -> match Base64_rfc2045.encode encoder (`Char chr) with
      | `Ok -> ()
      | `Partial -> Alcotest.failf "Retrieve impossible case for (`Char %02x): `Partial" (Char.code chr))
    x ;
  match Base64_rfc2045.encode encoder `End with
  | `Ok -> Buffer.contents res
  | `Partial -> Alcotest.fail "Retrieve impossible case for `End: `Partial"

let test_strict_with_malformed_input_rfc2045 =
  List.mapi (fun i (has, _) ->
      Alcotest.test_case (Fmt.strf "strict rfc2045 - %02d" i) `Quick @@ fun () ->
      try
        let _ = strict_base64_rfc2045_of_string has in
        Alcotest.failf "Strict parser valids malformed input: %S" has
      with Malformed | Wrong_padding -> () )
    test_relaxed_rfc2045

let test_strict_rfc2045 =
  List.mapi (fun i (has, expect) ->
      Alcotest.test_case (Fmt.strf "strict rfc2045 - %02d" i) `Quick @@ fun () ->
      try
        let res0 = strict_base64_rfc2045_of_string has in
        let res1 = strict_base64_rfc2045_to_string res0 in
        Alcotest.(check string) "encode(decode(x)) = x" res1 has ;
        Alcotest.(check string) "decode(x)" res0 expect
      with Malformed | Wrong_padding -> Alcotest.failf "Invalid input %S" has)
    test_strict_rfc2045

let test_relaxed_rfc2045 =
  List.mapi (fun i (has, expect) ->
      Alcotest.test_case (Fmt.strf "relaxed rfc2045 - %02d" i) `Quick @@ fun () ->
      let res0 = relaxed_base64_rfc2045_of_string has in
      Alcotest.(check string) "decode(x)" res0 expect)
    test_relaxed_rfc2045

let test_invariants = [ "Alphabet size", `Quick, alphabet_size ]
let test_codec = [ "RFC4648 test vectors", `Quick, test_rfc4648
                 ; "RFC3548 test vectors", `Quick, test_rfc3548
                 ; "Hannes test vectors", `Quick, test_hannes
                 ; "Cfcs test vectors", `Quick, test_cfcs
                 ; "PHP test vectors", `Quick, test_php
                 ; "Nocrypto test vectors", `Quick, test_nocrypto ]

let () =
  Alcotest.run "Base64" [
    "invariants", test_invariants;
    "codec", test_codec;
    "rfc2045 (0)", test_strict_rfc2045;
    "rfc2045 (1)", test_strict_with_malformed_input_rfc2045;
    "rfc2045 (2)", test_relaxed_rfc2045;
  ]

