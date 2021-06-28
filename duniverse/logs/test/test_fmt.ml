(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let pp_key = Format.pp_print_string
let pp_val = Format.pp_print_string


let err_invalid_kv args =
  Logs.err @@ fun m ->
  args (fun k v -> m "invalid kv (%a,%a)" pp_key k pp_val v)

let err_no_carrier args =
  Logs.err @@ fun m -> args (m "NO CARRIER")

let main () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level @@ Some Logs.Debug;
  Logs.set_reporter @@ Logs_fmt.reporter ();
  Logs.info (fun m -> m ~header:"START" ?tags:None "Starting main");
  Logs.warn (fun m -> m "Hey be warned by %d." 7);
  Logs.err (fun m -> m "Hey be errored.");
  Logs.debug (fun m -> m "Would you mind to be debugged a bit ?");
  Logs.app (fun m -> m "This is for the application console or stdout.");
  Logs.app (fun m -> m ~header:"HEAD" "Idem but with a header");
  let k = "key" in
  let v = "value" in
  Logs.err (fun m -> m "invalid kv (%a,%a)" pp_key k pp_val v);
  Logs.err (fun m -> m "NO CARRIER");
  err_invalid_kv (fun args -> args k v);
  err_no_carrier (fun () -> ());
  Logs.info (fun m -> m "Ending main");
  exit (if (Logs.err_count () > 0) then 1 else 0)

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers

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
