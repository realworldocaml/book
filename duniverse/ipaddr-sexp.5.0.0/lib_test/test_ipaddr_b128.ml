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

let test_shift_right () =
  let open Ipaddr_internal in
  let open V6 in
  let printer = function
    | Ok v -> Printf.sprintf "Ok %s" (to_string v)
    | Error (`Msg e) -> Printf.sprintf "Error `Msg \"%s\"" e
  in
  let assert_equal = assert_equal ~printer in
  assert_equal ~msg:":: >> 32"
    (of_string "::")
    (B128.shift_right (of_string_exn "::ffff:ffff") 32);
  assert_equal ~msg:"::aaaa:bbbb:ffff:ffff >> 32"
    (of_string "::aaaa:bbbb")
    (B128.shift_right (of_string_exn "::aaaa:bbbb:ffff:ffff") 32);
  assert_equal ~msg:"::aaaa:bbbb:ffff:ffff >> 40"
    (of_string "::aa:aabb")
    (B128.shift_right (of_string_exn "::aaaa:bbbb:ffff:ffff")  40);
  assert_equal ~msg:"::ffff >> 2"
    (of_string "::3fff")
    (B128.shift_right (of_string_exn "::ffff")  2);
  assert_equal ~msg:"ffff:: >> 128"
    (of_string "::")
    (B128.shift_right (of_string_exn "ffff::")  128);
  assert_equal ~msg:"aaaa:bbbb:cccc:dddd:: >> 120"
    (of_string "::aa")
    (B128.shift_right (of_string_exn "aaaa:bbbb:cccc:dddd::")  120);
  assert_equal ~msg:"ffff:: >> 140"
    (of_string "::")
    (B128.shift_right (of_string_exn "ffff::")  140);
  assert_equal ~msg:"::ffff:ffff >> -8"
    (of_string "::")
    (B128.shift_right (of_string_exn "::ffff:ffff") (-8))

let suite = "Test B128 module" >::: [
  "shift_right"       >:: test_shift_right;
]

;;
let _results = run_test_tt_main suite in
()
