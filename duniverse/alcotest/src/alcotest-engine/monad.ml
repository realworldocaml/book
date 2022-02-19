(*
 * Copyright (c) 2019 Craig Ferguson <craig@tarides.com>
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

include Monad_intf

module Identity = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let catch f on_error = match f () with x -> x | exception ex -> on_error ex
end

module Extend (M : S) = struct
  include M

  module Syntax = struct
    let ( >>= ) = M.bind
    let ( >|= ) x f = x >>= fun y -> M.return (f y)
    let ( let* ) = ( >>= )
    let ( let+ ) = ( >|= )
  end

  open Syntax

  module List = struct
    let fold_map_s f init l =
      let rec inner acc results = function
        | [] -> M.return (acc, List.rev results)
        | hd :: tl ->
            let* acc, r = f acc hd in
            (inner [@ocaml.tailcall]) acc (r :: results) tl
      in
      inner init [] l
  end
end
