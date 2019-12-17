(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix
open Mirage_flow_combinators

let pp_buf ppf buf = Fmt.string ppf (Cstruct.to_string buf)
let eq_buf b1 b2 = Cstruct.to_string b1 = Cstruct.to_string b2

let cstruct = Alcotest.testable pp_buf eq_buf
let fail fmt = Fmt.kstrf Alcotest.fail fmt

let check_buffer = Alcotest.(check cstruct)
let check_buffers = Alcotest.(check @@ list cstruct)

let check_ok_buffer msg buf = function
  | Ok (`Data b) -> check_buffer msg buf b
  | Ok `Eof      -> fail "%s: eof" msg
  | Error e      -> fail "%s: error=%a" msg F.pp_error e

let check_ok_unit msg = function
  | Ok ()   -> ()
  | Error e -> fail "%s: error=%a" msg F.pp_error e

let check_ok_write msg = function
  | Ok ()   -> ()
  | Error e -> fail "%s: error=%a" msg F.pp_write_error e

let check_closed msg = function
  | Ok ()         -> fail "%s: not closed" msg
  | Error `Closed -> ()
  | Error e       -> fail "%s: error=%a" msg F.pp_write_error e

let check_eof msg = function
  | Ok `Eof -> ()
  | Ok _    -> fail "%s: ok" msg
  | Error e -> fail "%s: error=%a" msg F.pp_error e

let cs str = Cstruct.of_string str
let cb str = Cstruct.of_bytes str

let css = List.map cs
let cbs = List.map cb

let filter x =
  let zero = Cstruct.of_string "" in
  List.filter ((<>) zero) x

let input_string () =
  let input = "xxxxxxxxxx" in
  let ic = F.string ~input () in
  F.read ic >>= fun x1 ->
  F.read ic >>= fun x2 ->
  F.write ic (cs "hihi") >>= fun r ->
  check_ok_buffer "read 1" (cs input) x1;
  check_eof "read 2" x2;
  check_closed "write"  r;
  Lwt.return_unit

let output_string () =
  let output = Bytes.of_string "xxxxxxxxxx" in
  let oc = F.string ~output () in
  F.write oc (cs  "hell") >>= fun x1 ->
  F.write oc (cs   "o! ") >>= fun x2 ->
  F.write oc (cs "world") >>= fun x3 ->
  F.read oc >>= fun r ->
  check_buffer "result" (cb output) (cs "hello! wor");
  check_ok_write "write 1" x1;
  check_ok_write "write 2" x2;
  check_closed   "write 3" x3;
  check_eof      "read"    r;
  Lwt.return_unit

let input_strings () =
  let input = [ ""; "123"; "45"; "6789"; "0" ] in
  let ic = F.strings ~input () in
  F.read ic >>= fun x1 ->
  F.read ic >>= fun x2 ->
  F.read ic >>= fun x3 ->
  F.read ic >>= fun x4 ->
  F.read ic >>= fun y ->
  F.read ic >>= fun z ->
  F.write ic (cs "hihi") >>= fun w ->
  check_ok_buffer "read 1" (cs  "123") x1;
  check_ok_buffer "read 2" (cs   "45") x2;
  check_ok_buffer "read 3" (cs "6789") x3;
  check_ok_buffer "read 4" (cs    "0") x4;
  check_eof       "read 5" y;
  check_eof       "read 6" z;
  check_closed    "write"  w;
  Lwt.return_unit

let output_strings () =
  let output = List.map Bytes.of_string ["xxx"; ""; "xx"; "xxx"; ] in
  let oc = F.strings ~output () in
  F.write oc (cs  "hell") >>= fun x1 ->
  F.write oc (cs   "o! ") >>= fun x2 ->
  F.write oc (cs "world") >>= fun x3 ->
  F.read oc >>= fun r ->
  check_buffers "result" (filter (cbs output)) (css ["hel"; "lo"; "! w"]);
  check_ok_write "write 1" x1;
  check_ok_write "write 2" x2;
  check_closed   "write 3" x3;
  check_eof     "read"    r;
  Lwt.return_unit

let input_cstruct () =
  let input = Cstruct.of_string "xxxxxxxxxx" in
  let ic = F.cstruct ~input () in
  F.read ic >>= fun x1 ->
  F.read ic >>= fun x2 ->
  F.write ic (cs "hihi") >>= fun r ->
  check_ok_buffer "read 1" input x1;
  check_eof       "read 2" x2;
  check_closed    "write"  r;
  Lwt.return_unit

let output_cstruct () =
  let output = Cstruct.of_string "xxxxxxxxxx" in
  let oc = F.cstruct ~output () in
  F.write oc (cs  "hell") >>= fun x1 ->
  F.write oc (cs   "o! ") >>= fun x2 ->
  F.write oc (cs "world") >>= fun x3 ->
  F.read oc >>= fun r ->
  check_buffer  "result" output (cs "hello! wor");
  check_ok_write "write 1" x1;
  check_ok_write "write 2" x2;
  check_closed   "write 3" x3;
  check_eof     "read"    r;
  Lwt.return_unit

let input_cstructs () =
  let inputs = List.map cs [ "123"; "45"; ""; "6789"; "0" ] in
  let ic = F.cstructs ~input:inputs () in
  F.read ic >>= fun x1 ->
  F.read ic >>= fun x2 ->
  F.read ic >>= fun x3 ->
  F.read ic >>= fun x4 ->
  F.read ic >>= fun y ->
  F.read ic >>= fun z ->
  F.write ic (cs "hihi") >>= fun w ->
  check_ok_buffer "read 1" (cs  "123") x1;
  check_ok_buffer "read 2" (cs   "45") x2;
  check_ok_buffer "read 3" (cs "6789") x3;
  check_ok_buffer "read 4" (cs    "0") x4;
  check_eof       "read 5 "y;
  check_eof       "read 6" z;
  check_closed    "read 7" w;
  Lwt.return_unit

let output_cstructs () =
  let output = List.map cs [ ""; "xxx"; "xx"; "xxx" ] in
  let oc = F.cstructs ~output () in
  F.write oc (cs  "hell") >>= fun x1 ->
  F.write oc (cs   "o! ") >>= fun x2 ->
  F.write oc (cs "world") >>= fun x3 ->
  F.read oc >>= fun r ->
  check_buffers "result" (filter output) (css ["hel"; "lo"; "! w"]);
  check_ok_write "write 1" x1;
  check_ok_write "write 2" x2;
  check_closed   "write 3" x3;
  check_eof     "read"    r;
  Lwt.return_unit

module Lwt_io_flow = Mirage_flow_unix.Make(F)

let input_lwt_io () =
  let ic = F.strings ~input:["1"; "234"; "56"; "78\n90"] () in
  let lic = Lwt_io_flow.ic ic in
  Lwt_io.read_line lic >>= fun l ->
  check_buffer "result" (cs "12345678") (cs l);
  Lwt.return_unit

let output_lwt_io () =
  let output = css ["xxxx";"xxxx"; "xxxxxx"] in
  let oc = F.cstructs ~output () in
  let loc = Lwt_io_flow.oc oc in
  Lwt_io.write_line loc "Hello world!" >>= fun () ->
  Lwt_io.flush loc >>= fun () ->
  check_buffers "result" (css ["Hell"; "o wo"; "rld!\nx"]) output;
  Lwt.return_unit

let run f () = Lwt_main.run (f ())

let string = [
  "input" , `Quick, run input_string;
  "output", `Quick, run output_string;
]

let strings = [
  "input" , `Quick, run input_strings;
  "output", `Quick, run output_strings;
]

let cstruct = [
  "input" , `Quick, run input_cstruct;
  "output", `Quick, run output_cstruct;
]

let cstructs = [
  "input" , `Quick, run input_cstructs;
  "output", `Quick, run output_cstructs;
]

let lwt_io = [
  "input" , `Quick, run input_lwt_io;
  "output", `Quick, run output_lwt_io;
]
let () =
  Alcotest.run "mirage-flow" [
    "string"  , string;
    "strings" , strings;
    "cstruct" , cstruct;
    "cstructs", cstructs;
    "lwt-io"  , lwt_io;
  ]
