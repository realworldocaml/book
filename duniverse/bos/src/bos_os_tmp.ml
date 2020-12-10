(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring

(* Base functions for handling temporary file and directories. *)

let default_dir_init =
  let from_env var ~absent =
    match try Some (Sys.getenv var) with Not_found -> None with
    | None -> absent
    | Some v ->
        match Fpath.of_string v with
        | Result.Error _ -> absent (* FIXME log something ? *)
        | Result.Ok v -> v
  in
  if Sys.os_type = "Win32" then from_env "TEMP" ~absent:Fpath.(v "./") else
  from_env "TMPDIR" ~absent:(Fpath.v "/tmp")

let default_dir = ref default_dir_init
let set_default_dir p = default_dir := p
let default_dir () = !default_dir

let rand_gen = lazy (Random.State.make_self_init ())

let rand_path dir pat =
  let rand = Random.State.bits (Lazy.force rand_gen) land 0xFFFFFF in
  Fpath.(dir / strf pat (strf "%06x" rand))

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
