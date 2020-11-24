(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Astring
open Rresult
open Bos

let eqb = eq_result_msg ~eq_ok:(=) ~pp_ok:pp_bool
let eqs = eq_result_msg ~eq_ok:(=) ~pp_ok:pp_str
let equ = eq_result_msg ~eq_ok:(=) ~pp_ok:pp_unit
let eql = eq_result_msg ~eq_ok:(=) ~pp_ok:(pp_list pp_str)

let cat = Cmd.(v "cat")
let cat_stdin = Cmd.(cat % "-")
let unlikely = Cmd.v "6AC0E501-4E30-4CBC-AD03-F880F885BC18"

let exists = test "OS.Cmd.exists" @@ fun () ->
  eqb (OS.Cmd.exists cat) (Ok true);
  eqb (OS.Cmd.exists unlikely) (Ok false);
  ()

let must_exist = test "OS.Cmd.must_exist" @@ fun () ->
  begin match (OS.Cmd.must_exist cat) with
  | Error (`Msg err) -> fail "%s" err
  | Ok _ -> ()
  end;
  begin match (OS.Cmd.must_exist unlikely) with
  | Ok _ -> fail "%a exists" Cmd.dump unlikely
  | Error _ -> ()
  end;
  ()

let run_io = test "OS.Cmd.run_io" @@ fun () ->
  let in_hey = OS.Cmd.in_string "hey" in
  let tmp () = OS.File.tmp "bos_test_%s" in
  eqs OS.Cmd.(in_hey |> run_io cat_stdin |> to_string) (Ok "hey");
  eql OS.Cmd.(in_string "hey\nho\n" |> run_io cat_stdin |> to_lines)
    (Ok ["hey";"ho"]);
  equ OS.Cmd.(in_hey |> run_io cat_stdin |> to_null) (Ok ());
  eqs (tmp ()
       >>= fun tmp -> OS.Cmd.(in_hey |> run_io cat_stdin |> to_file tmp)
       >>= fun () -> OS.Cmd.(in_hey |> run_io cat |> to_file tmp ~append:true)
       >>= fun () -> OS.Cmd.(in_file tmp |> run_io Cmd.(cat_stdin % p tmp) |>
                             to_string))
    (Ok "heyheyheyhey");
  eqs (tmp ()
       >>= fun tmp1 -> tmp()
       >>= fun tmp2 -> OS.Cmd.(in_hey |> run_io cat_stdin |> to_file tmp1)
       >>= fun () -> OS.Cmd.(in_file tmp1 |> run_io cat_stdin |> out_run_in)
       >>= fun pipe ->  OS.Cmd.(pipe |> run_io cat_stdin |> to_file tmp2)
       >>= fun () -> OS.Cmd.(in_file tmp2 |> run_io cat_stdin |> to_string))
  (Ok "hey");
  ()

let suite = suite "OS command run functions"
    [ exists;
      run_io; ]

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
