(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos

let debug = OS.Arg.(flag ["g"; "debug"] ~env:"DEBUG" ~doc:"Debug mode.")

let () = Fmt.(set_style_renderer stdout `Ansi_tty)

let print_parse depth ints =
  Logs.app (fun m -> m "debug: %b" debug);
  Logs.app (fun m -> m "depth: %d" depth);
  Logs.app (fun m -> m "pos: @[%a@]" Fmt.(list ~sep:sp int) ints);
  ()

let main () =
  Logs.set_reporter (Logs_fmt.reporter ());
  let depth =
    OS.Arg.(opt ["d"; "depth"] int ~absent:2
              ~doc:"Specifies depth of $(docv) iterations.")
  in
  let doc = "Testing the OS.Arg module." in
  print_parse depth (OS.Arg.(parse ~doc ~pos:int ()))

let () = main ()


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
