(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult

type 'a result = ('a, [`Unix of Unix.error]) Rresult.result
let pp_error ppf (`Unix e ) = Fmt.string ppf (Unix.error_message e)
let open_error = function Ok _ as r -> r | Error (`Unix _) as r -> r
let error_to_msg r = R.error_to_msg ~pp_error r

let rec call f v = try Ok (f v) with
| Unix.Unix_error (Unix.EINTR, _, _) -> call f v
| Unix.Unix_error (e, _, _) -> Error (`Unix e)

let mkdir p m = try Ok (Unix.mkdir (Fpath.to_string p) m) with
| Unix.Unix_error (e, _, _) -> Error (`Unix e)

let link p p' =
  try Ok (Unix.link (Fpath.to_string p) (Fpath.to_string p')) with
  | Unix.Unix_error (e, _, _) -> Error (`Unix e)

let unlink p = try Ok (Unix.unlink (Fpath.to_string p)) with
| Unix.Unix_error (e, _, _) -> Error (`Unix e)

let rename p p' =
  try Ok (Unix.rename (Fpath.to_string p) (Fpath.to_string p')) with
  | Unix.Unix_error (e, _, _) -> Error (`Unix e)

let stat p = try Ok (Unix.stat (Fpath.to_string p)) with
| Unix.Unix_error (e, _, _) -> Error (`Unix e)

let lstat p = try Ok (Unix.lstat (Fpath.to_string p)) with
| Unix.Unix_error (e, _, _) -> Error (`Unix e)

let rec truncate p size = try Ok (Unix.truncate (Fpath.to_string p) size) with
| Unix.Unix_error (Unix.EINTR, _, _) -> truncate p size
| Unix.Unix_error (e, _, _) -> Error (`Unix e)

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
