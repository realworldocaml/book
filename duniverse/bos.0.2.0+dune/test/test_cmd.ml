(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Rresult
open Astring
open Bos

let of_string = test "Cmd.of_string" @@ fun () ->
  let eq cmd l = match Cmd.of_string cmd with
  | Error (`Msg msg) -> fail "%s" msg
  | Ok l' -> eq_list ~eq:(=) ~pp:pp_str (Cmd.to_list l') l
  in
  eq "" [];
  eq "bla" ["bla"];
  eq " bla bli" ["bla"; "bli"];
  eq " bla bli  " ["bla"; "bli"];
  eq " bla b\\li  " ["bla"; "b\\li"];
  eq " b'haha'la bli  " ["bhahala"; "bli"];
  eq " b\"haha\"la bli  " ["bhahala"; "bli"];
  eq " b\"'\"la bli  " ["b'la"; "bli"];
  eq " b''''la bli  " ["bla"; "bli"];
  eq " b'u'\"'\"'i'la bli  " ["bu'ila"; "bli"];
  eq " b\"\\\"\"ila bli  " ["b\"ila"; "bli"];
  eq " b\"\\\n\"ila bli  " ["bila"; "bli"];
  ()

let suite = suite "Cmd module"
    [ of_string; ]

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
