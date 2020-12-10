(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult

let log f = Format.printf (f ^^ "@.")

let raises_invalid_arg f x =
  try f x; raise Exit with
  | Invalid_argument _ -> ()
  | e -> assert false

let test_constructors () =
  log "Test R.{ok,error}";
  assert (R.ok 3 = Ok 3);
  assert (R.error `An_error = Error `An_error);
  ()

let test_reword_error () =
  log "Test R.reword_error";
  let reword `An_error = `Another_one in
  assert (R.reword_error reword (Ok 3) = Ok 3);
  assert (R.reword_error reword (Error `An_error) = (Error `Another_one));
  ()

let test_gets () =
  log "Test R.get_{ok,error}";
  assert (R.get_ok (Ok 2) = 2);
  assert (R.get_error (Error 3) = 3);
  raises_invalid_arg R.get_ok (Error 3);
  raises_invalid_arg R.get_error (Ok 2);
  ()

let test_bind () =
  log "Test R.bind";
  assert (R.bind (Ok 3) (fun v -> Error (string_of_int v)) = Error "3");
  assert (R.bind (Ok 3) (fun v -> Ok (string_of_int v)) = Ok "3");
  assert (R.bind (Error 1) (fun v -> Ok (string_of_int v)) = Error 1);
  ()

let test_map () =
  log "Test R.map";
  assert (R.map (fun v -> string_of_int v) (Ok 2) = Ok "2");
  assert (R.map (fun v -> string_of_int v) (Error 2) = Error 2);
  ()

let test_join () =
  log "Test R.join";
  assert (R.join (Ok (Ok 3)) = Ok 3);
  assert (R.join (Ok (Error 2)) = Error 2);
  assert (R.join (Error 3) = Error 3);
  assert (R.join (Error 4) = Error 4);
  ()

let test_msgs () =
  log "Test error messages.";
  assert (R.msg "bla" = `Msg "bla");
  assert (R.msgf "bla%d" 3 = `Msg "bla3");
  assert (R.error_msg "bla" = Error (`Msg "bla"));
  assert (R.error_msgf "bla%d" 3 = Error (`Msg "bla3"));
  let reword s = `Msg (s ^ "++")  in
  assert (R.reword_error_msg ~replace:true reword (Ok 2) = Ok 2);
  assert (R.reword_error_msg ~replace:false reword (Ok 2) = Ok 2);
  assert (R.reword_error_msg ~replace:true reword
            (Error (`Msg "ha")) = (Error (`Msg "ha++")));
  assert (R.reword_error_msg ~replace:false reword
            (Error (`Msg "ha")) = (Error (`Msg "ha\nha++")));
  let pp_error ppf = function `E -> Format.fprintf ppf "E" in
  assert (R.error_to_msg ~pp_error (Ok 2) = (Ok 2));
  assert (R.error_to_msg ~pp_error (Error `E) = (Error (`Msg "E")));
  assert (R.error_msg_to_invalid_arg (Ok 2) = 2);
  raises_invalid_arg R.error_msg_to_invalid_arg (Error (`Msg "E"));
  ()

let test_exn_trap () =
  log "Test trapping unexpected exceptions.";
  let no_raise x = string_of_int x in
  let do_raise x = raise Exit in
  assert (R.trap_exn no_raise 3 = Ok "3");
  begin match R.trap_exn do_raise 3 with
  | Ok _ -> assert false
  | Error (`Exn_trap (Exit, _)) -> ()
  | Error _ -> assert false
  end;
  ()

let test_is () =
  log "Test R.is_{ok,error}";
  assert (R.is_ok (Ok 2));
  assert (not @@ R.is_ok (Error 2));
  assert (R.is_error (Error 2));
  assert (not @@ R.is_error (Ok 2));
  ()

let test_converting () =
  log "Test R.{to,of}_{option,presult}";
  assert (R.to_option (Ok 3) = Some 3);
  assert (R.to_option (Error 3) = None);
  assert (R.of_option ~none:(fun () -> Error "none") (Some 3) = Ok 3);
  assert (R.of_option ~none:(fun () -> Error "none") (None) = Error "none");
  assert (R.to_presult (Ok 3) = (`Ok 3));
  assert (R.to_presult (Error 3) = (`Error 3));
  assert (R.of_presult (`Ok 3) = (Ok 3));
  assert (R.of_presult (`Error 3) = (Error 3));
  ()

let test_ignoring () =
  log "Test.[k]ignore_error";
  assert (R.ignore_error ~use:(fun _ -> 3) (Ok 4) = 4);
  assert (R.ignore_error ~use:(fun _ -> 3) (Error 4) = 3);
  assert (R.kignore_error ~use:(fun _ -> Ok 3) (Ok 4) = (Ok 4));
  assert (R.kignore_error ~use:(fun _ -> Ok 3) (Error 4) = (Ok 3));
  assert (R.kignore_error ~use:(fun _ -> Error 3) (Ok 4) = (Ok 4));
  assert (R.kignore_error ~use:(fun _ -> Error 3) (Error 4) = (Error 3));
  ()

let tests () =
  test_constructors ();
  test_reword_error ();
  test_gets ();
  test_bind ();
  test_map ();
  test_join ();
  test_msgs ();
  test_exn_trap ();
  test_is ();
  test_converting ();
  test_ignoring ();
  ()

let () =
  tests ();
  log "All tests succeeded."

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
