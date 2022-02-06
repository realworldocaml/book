(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fmt programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(*
let test_exn_backtrace () = (* Don't move this test in the file. *)
  try failwith "Test" with
  | ex ->
      let bt = Printexc.get_raw_backtrace () in
      let fmt = Fmt.strf "%a" Fmt.exn_backtrace (ex,bt) in
      assert begin match Printexc.backtrace_status () with
      | false -> fmt = "Exception: Failure(\"Test\")\nNo backtrace available."
      | true ->
          fmt = "Exception: Failure(\"Test\")\n\
                 Raised at file \"pervasives.ml\", line 32, characters 22-33\n\
                 Called from file \"test/test.ml\", line 8, characters 6-21"
      end
*)

let test_dump_uchar () =
 let str u = Format.asprintf "%a" Fmt.Dump.uchar u in
 assert (str Uchar.min = "U+0000");
 assert (str Uchar.(succ min) = "U+0001");
 assert (str Uchar.(of_int 0xFFFF) = "U+FFFF");
 assert (str Uchar.(succ (of_int 0xFFFF)) = "U+10000");
 assert (str Uchar.(pred max) = "U+10FFFE");
 assert (str Uchar.max = "U+10FFFF");
 ()

let test_utf_8 () =
  let ppf = Format.formatter_of_buffer (Buffer.create 23) in
  assert (Fmt.utf_8 ppf = true);
  Fmt.set_utf_8 ppf false;
  assert (Fmt.utf_8 ppf = false);
  Fmt.set_utf_8 ppf true;
  assert (Fmt.utf_8 ppf = true);
  ()

let test_style_renderer () =
  let ppf = Format.formatter_of_buffer (Buffer.create 23) in
  assert (Fmt.style_renderer ppf = `None);
  Fmt.set_style_renderer ppf `Ansi_tty;
  assert (Fmt.style_renderer ppf = `Ansi_tty);
  Fmt.set_style_renderer ppf `None;
  assert (Fmt.style_renderer ppf = `None);
  ()

let test_exn_typechecks () =
  let (_ : bool) = true || Fmt.failwith "%s" "" in
  let (_ : bool) = true || Fmt.invalid_arg "%s" "" in
  ()

let test_kstr_str_like_partial_app () =
  let assertf f = assert (f "X" = f "X") in
  let test_kstrf fmt = Fmt.kstr (fun x -> x) fmt in
  let test_strf_like fmt = Fmt.str_like Fmt.stderr fmt in
  assertf (test_strf_like "%s");
  assertf (test_kstrf "%s");
  ()


let test_byte_size () =
  let size s = Fmt.str "%a" Fmt.byte_size s in
  assert (size 0 = "0B");
  assert (size 999 = "999B");
  assert (size 1000 = "1kB");
  assert (size 1001 = "1.01kB");
  assert (size 1010 = "1.01kB");
  assert (size 1011 = "1.02kB");
  assert (size 1020 = "1.02kB");
  assert (size 1100 = "1.1kB");
  assert (size 1101 = "1.11kB");
  assert (size 1109 = "1.11kB");
  assert (size 1111 = "1.12kB");
  assert (size 1119 = "1.12kB");
  assert (size 1120 = "1.12kB");
  assert (size 1121 = "1.13kB");
  assert (size 9990 = "9.99kB");
  assert (size 9991 = "10kB");
  assert (size 9999 = "10kB");
  assert (size 10_000 = "10kB");
  assert (size 10_001 = "10.1kB");
  assert (size 10_002 = "10.1kB");
  assert (size 10_099 = "10.1kB");
  assert (size 10_100 = "10.1kB");
  assert (size 10_100 = "10.1kB");
  assert (size 10_101 = "10.2kB");
  assert (size 10_199 = "10.2kB");
  assert (size 10_199 = "10.2kB");
  assert (size 10_200 = "10.2kB");
  assert (size 10_201 = "10.3kB");
  assert (size 99_901 = "100kB");
  assert (size 99_999 = "100kB");
  assert (size 100_000 = "100kB");
  assert (size 100_001 = "101kB");
  assert (size 100_999 = "101kB");
  assert (size 101_000 = "101kB");
  assert (size 101_001 = "102kB");
  assert (size 999_000 = "999kB");
  assert (size 999_001 = "1MB");
  assert (size 999_999 = "1MB");
  assert (size 1_000_000 = "1MB");
  assert (size 1_000_001 = "1.01MB");
  assert (size 1_009_999 = "1.01MB");
  assert (size 1_010_000 = "1.01MB");
  assert (size 1_010_001 = "1.02MB");
  assert (size 1_019_999 = "1.02MB");
  assert (size 1_020_000 = "1.02MB");
  assert (size 1_020_001 = "1.03MB");
  assert (size 1_990_000 = "1.99MB");
  assert (size 1_990_001 = "2MB");
  assert (size 1_999_999 = "2MB");
  assert (size 2_000_000 = "2MB");
  assert (size 9_990_000 = "9.99MB");
  assert (size 9_990_001 = "10MB");
  assert (size 9_990_999 = "10MB");
  assert (size 10_000_000 = "10MB");
  assert (size 10_000_001 = "10.1MB");
  assert (size 10_099_999 = "10.1MB");
  assert (size 10_100_000 = "10.1MB");
  assert (size 10_900_001 = "11MB");
  assert (size 10_999_999 = "11MB");
  assert (size 11_000_000 = "11MB");
  assert (size 11_000_001 = "11.1MB");
  assert (size 99_900_000 = "99.9MB");
  assert (size 99_900_001 = "100MB");
  assert (size 99_999_999 = "100MB");
  assert (size 100_000_000 = "100MB");
  assert (size 100_000_001 = "101MB");
  assert (size 100_999_999 = "101MB");
  assert (size 101_000_000 = "101MB");
  assert (size 101_000_000 = "101MB");
  assert (size 999_000_000 = "999MB");
  assert (size 999_000_001 = "1GB");
  assert (size 999_999_999 = "1GB");
  assert (size 1_000_000_000 = "1GB");
  assert (size 1_000_000_001 = "1.01GB");
  assert (size 1_000_000_001 = "1.01GB");
  ()

let test_uint64_ns_span () =
  let span s = Fmt.str "%a" Fmt.uint64_ns_span (Int64.of_string s) in
  assert (span "0u0" = "0ns");
  assert (span "0u999" = "999ns");
  assert (span "0u1_000" = "1us");
  assert (span "0u1_001" = "1.01us");
  assert (span "0u1_009" = "1.01us");
  assert (span "0u1_010" = "1.01us");
  assert (span "0u1_011" = "1.02us");
  assert (span "0u1_090" = "1.09us");
  assert (span "0u1_091" = "1.1us");
  assert (span "0u1_100" = "1.1us");
  assert (span "0u1_101" = "1.11us");
  assert (span "0u1_109" = "1.11us");
  assert (span "0u1_110" = "1.11us");
  assert (span "0u1_111" = "1.12us");
  assert (span "0u1_990" = "1.99us");
  assert (span "0u1_991" = "2us");
  assert (span "0u1_999" = "2us");
  assert (span "0u2_000" = "2us");
  assert (span "0u2_001" = "2.01us");
  assert (span "0u9_990" = "9.99us");
  assert (span "0u9_991" = "10us");
  assert (span "0u9_999" = "10us");
  assert (span "0u10_000" = "10us");
  assert (span "0u10_001" = "10.1us");
  assert (span "0u10_099" = "10.1us");
  assert (span "0u10_100" = "10.1us");
  assert (span "0u10_101" = "10.2us");
  assert (span "0u10_900" = "10.9us");
  assert (span "0u10_901" = "11us");
  assert (span "0u10_999" = "11us");
  assert (span "0u11_000" = "11us");
  assert (span "0u11_001" = "11.1us");
  assert (span "0u11_099" = "11.1us");
  assert (span "0u11_100" = "11.1us");
  assert (span "0u11_101" = "11.2us");
  assert (span "0u99_900" = "99.9us");
  assert (span "0u99_901" = "100us");
  assert (span "0u99_999" = "100us");
  assert (span "0u100_000" = "100us");
  assert (span "0u100_001" = "101us");
  assert (span "0u100_999" = "101us");
  assert (span "0u101_000" = "101us");
  assert (span "0u101_001" = "102us");
  assert (span "0u101_999" = "102us");
  assert (span "0u102_000" = "102us");
  assert (span "0u999_000" = "999us");
  assert (span "0u999_001" = "1ms");
  assert (span "0u999_001" = "1ms");
  assert (span "0u999_999" = "1ms");
  assert (span "0u1_000_000" = "1ms");
  assert (span "0u1_000_001" = "1.01ms");
  assert (span "0u1_009_999" = "1.01ms");
  assert (span "0u1_010_000" = "1.01ms");
  assert (span "0u1_010_001" = "1.02ms");
  assert (span "0u9_990_000" = "9.99ms");
  assert (span "0u9_990_001" = "10ms");
  assert (span "0u9_999_999" = "10ms");
  assert (span "0u10_000_000" = "10ms");
  assert (span "0u10_000_001" = "10.1ms");
  assert (span "0u10_000_001" = "10.1ms");
  assert (span "0u10_099_999" = "10.1ms");
  assert (span "0u10_100_000" = "10.1ms");
  assert (span "0u10_100_001" = "10.2ms");
  assert (span "0u99_900_000" = "99.9ms");
  assert (span "0u99_900_001" = "100ms");
  assert (span "0u99_999_999" = "100ms");
  assert (span "0u100_000_000" = "100ms");
  assert (span "0u100_000_001" = "101ms");
  assert (span "0u100_999_999" = "101ms");
  assert (span "0u101_000_000" = "101ms");
  assert (span "0u101_000_001" = "102ms");
  assert (span "0u999_000_000" = "999ms");
  assert (span "0u999_000_001" = "1s");
  assert (span "0u999_999_999" = "1s");
  assert (span "0u1_000_000_000" = "1s");
  assert (span "0u1_000_000_001" = "1.01s");
  assert (span "0u1_009_999_999" = "1.01s");
  assert (span "0u1_010_000_000" = "1.01s");
  assert (span "0u1_010_000_001" = "1.02s");
  assert (span "0u1_990_000_000" = "1.99s");
  assert (span "0u1_990_000_001" = "2s");
  assert (span "0u1_999_999_999" = "2s");
  assert (span "0u2_000_000_000" = "2s");
  assert (span "0u2_000_000_001" = "2.01s");
  assert (span "0u9_990_000_000" = "9.99s");
  assert (span "0u9_999_999_999" = "10s");
  assert (span "0u10_000_000_000" = "10s");
  assert (span "0u10_000_000_001" = "10.1s");
  assert (span "0u10_099_999_999" = "10.1s");
  assert (span "0u10_100_000_000" = "10.1s");
  assert (span "0u10_100_000_001" = "10.2s");
  assert (span "0u59_900_000_000" = "59.9s");
  assert (span "0u59_900_000_001" = "1min");
  assert (span "0u59_999_999_999" = "1min");
  assert (span "0u60_000_000_000" = "1min");
  assert (span "0u60_000_000_001" = "1min1s");
  assert (span "0u60_999_999_999" = "1min1s");
  assert (span "0u61_000_000_000" = "1min1s");
  assert (span "0u61_000_000_001" = "1min2s");
  assert (span "0u119_000_000_000" = "1min59s");
  assert (span "0u119_000_000_001" = "2min");
  assert (span "0u119_999_999_999" = "2min");
  assert (span "0u120_000_000_000" = "2min");
  assert (span "0u120_000_000_001" = "2min1s");
  assert (span "0u3599_000_000_000" = "59min59s");
  assert (span "0u3599_000_000_001" = "1h");
  assert (span "0u3599_999_999_999" = "1h");
  assert (span "0u3600_000_000_000" = "1h");
  assert (span "0u3600_000_000_001" = "1h1min");
  assert (span "0u3659_000_000_000" = "1h1min");
  assert (span "0u3659_000_000_001" = "1h1min");
  assert (span "0u3659_999_999_999" = "1h1min");
  assert (span "0u3660_000_000_000" = "1h1min");
  assert (span "0u3660_000_000_001" = "1h2min");
  assert (span "0u3660_000_000_001" = "1h2min");
  assert (span "0u3660_000_000_001" = "1h2min");
  assert (span "0u3720_000_000_000" = "1h2min");
  assert (span "0u3720_000_000_001" = "1h3min");
  assert (span "0u7140_000_000_000" = "1h59min");
  assert (span "0u7140_000_000_001" = "2h");
  assert (span "0u7199_999_999_999" = "2h");
  assert (span "0u7200_000_000_000" = "2h");
  assert (span "0u7200_000_000_001" = "2h1min");
  assert (span "0u86340_000_000_000" = "23h59min");
  assert (span "0u86340_000_000_001" = "1d");
  assert (span "0u86400_000_000_000" = "1d");
  assert (span "0u86400_000_000_001" = "1d1h");
  assert (span "0u89999_999_999_999" = "1d1h");
  assert (span "0u90000_000_000_000" = "1d1h");
  assert (span "0u90000_000_000_001" = "1d2h");
  assert (span "0u169200_000_000_000" = "1d23h");
  assert (span "0u169200_000_000_001" = "2d");
  assert (span "0u169200_000_000_001" = "2d");
  assert (span "0u172799_999_999_999" = "2d");
  assert (span "0u172800_000_000_000" = "2d");
  assert (span "0u172800_000_000_001" = "2d1h");
  assert (span "0u31536000_000_000_000" = "365d");
  assert (span "0u31554000_000_000_000" = "365d5h");
  assert (
    (* Technically this should round to a year but it does get rendered.
       I don't think it matters, it's not inacurate per se. *)
    span "0u31554000_000_000_001" = "365d6h");
  assert (span "0u31557600_000_000_000" = "1a");
  assert (span "0u31557600_000_000_001" = "1a1d");
  assert (span "0u63028800_000_000_000" = "1a365d");
  assert (span "0u63093600_000_000_000" = "1a365d");
  assert (span "0u63093600_000_000_001" = "2a");
  assert (span "0u63115200_000_000_000" = "2a");
  assert (span "0u63115200_000_000_001" = "2a1d");
  ()

let tests () =
  test_dump_uchar ();
  test_utf_8 ();
  test_style_renderer ();
  test_kstr_str_like_partial_app ();
  test_byte_size ();
  test_uint64_ns_span ();
  Printf.printf "Done.\n";
  ()

let () = tests ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fmt programmers

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
