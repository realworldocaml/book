(*
 * Copyright (c) 2013-2014 David Sheets <sheets@alum.mit.edu>
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

open OUnit
open Macaddr

let test_string_rt () =
  let addrs = [
    "ca:fe:ba:be:ee:ee", ':';
    "ca-fe-ba-be-ee-ee", '-';
  ] in
  List.iter (fun (addr,sep) ->
    let os = of_string_exn addr in
    let ts = to_string ~sep os in
    assert_equal ~msg:(addr ^ " <> " ^ ts) ts addr;
    let os = Macaddr_sexp.(t_of_sexp (sexp_of_t os)) in
    let ts = to_string ~sep os in
    assert_equal ~msg:(addr ^ " <> " ^ ts) ts addr;
  ) addrs

let assert_result_failure ~msg a =
  match a with
  | Ok _ -> assert_failure msg
  | Error (`Msg _) -> ()

let test_string_rt_bad () =
  let addrs = [
    "ca:fe:ba:be:ee:e";
    "ca:fe:ba:be:ee:eee";
    "ca:fe:ba:be:eeee";
    "ca:fe:ba:be:ee::ee";
    "ca:fe:ba:be:e:eee";
    "ca:fe:ba:be:ee-ee";
  ] in
  List.iter (fun addr -> assert_result_failure ~msg:addr (of_string addr)) addrs

let test_bytes_rt () =
  let addr = "\254\099\003\128\000\000" in
  assert_equal ~msg:(String.escaped addr) (to_octets (of_octets_exn addr)) addr

let test_bytes_rt_bad () =
  let addrs = [
    "\254\099\003\128\000";
    "\254\099\003\128\000\000\233";
  ] in
  List.iter (fun addr ->
    assert_result_failure ~msg:(String.escaped addr) (of_octets addr)) addrs

let test_cstruct_rt () =
  let open Macaddr_cstruct in
  let addr = "\254\099\003\128\000\000" in
  assert_equal ~msg:(String.escaped addr)
    (Cstruct.to_string (to_cstruct (of_cstruct_exn (Cstruct.of_string addr)))) addr

let error s = s, Parse_error ("MAC is exactly 6 bytes",s)

let test_cstruct_rt_bad () =
  let open Macaddr_cstruct in
  let addrs = [
    error "\254\099\003\128\000";
    error "\254\099\003\128\000\000\233";
  ] in
  List.iter (fun (addr,exn) ->
    assert_raises ~msg:(String.escaped addr) exn (fun () -> of_cstruct_exn (Cstruct.of_string addr))) addrs

let test_make_local () =
  let () = Random.self_init () in
  let bytegen i = if i = 0 then 253 else 255 - i in
  let local_addr = make_local bytegen in
  assert_equal ~msg:"is_local" (is_local local_addr) true;
  assert_equal ~msg:"is_unicast" (is_unicast local_addr) true;
  assert_equal ~msg:"localize" (to_octets local_addr).[0] (Char.chr 254);
  for i=1 to 5 do
    assert_equal ~msg:("addr.["^(string_of_int i)^"]")
      (to_octets local_addr).[i] (Char.chr (bytegen i))
  done;
  assert_equal ~msg:"get_oui" (get_oui local_addr)
    ((254 lsl 16) + (254 lsl 8) + 253)

let suite = "Test" >::: [
  "string_rt"            >:: test_string_rt;
  "string_rt_bad"        >:: test_string_rt_bad;
  "bytes_rt"             >:: test_bytes_rt;
  "bytes_rt_bad"         >:: test_bytes_rt_bad;
  "cstruct_rt"           >:: test_cstruct_rt;
  "cstruct_rt_bad"       >:: test_cstruct_rt_bad;
  "make_local"           >:: test_make_local;
]
;;
run_test_tt_main suite
