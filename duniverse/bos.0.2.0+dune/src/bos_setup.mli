(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Quick setup for simple programs.

    Linking against this module setups {!Logs} and issuing:
{[
open Bos_setup
]}
    in a module is sufficient to bring {!Rresult}, {!Astring} and
    {!Bos} in scope. See also how to use this for
    {{!interpreted}interpreted programs}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:interpreted Interpreted programs}

To use {!Bos} and this setup in an interpreted program, start the
file with:
{[
#!/usr/bin/env ocaml
#use "topfind"
#require "bos.setup"
open Bos_setup
]}
To allow {{:https://github.com/the-lambda-church/merlin}merlin} to function
correctly issue [M-x merlin-use bos.setup] in [emacs] or
[:MerlinUse bos.setup] in [vim]. *)

(** {1 Results} *)

(** The type for results. *)
type ('a, 'b) result = ('a, 'b) Rresult.result = Ok of 'a | Error of 'b

open Result

val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
(** [(>>=)] is {!R.( >>= )}. *)

val ( >>| ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
(** [(>>|)] is {!R.( >>| )}. *)

module R : sig
  include module type of struct include Rresult.R end
end

(** {1 Astring} *)

val strf : ('a, Format.formatter, unit, string) Pervasives.format4 -> 'a
(** [strf] is {!Astring.strf}. *)

val (^) : string -> string -> string
(** [^] is {!Astring.(^)}. *)

module Char : sig
  include module type of struct include Astring.Char end
end

module String : sig
  include module type of struct include Astring.String end
end

(** {1 Bos} *)

module Pat : sig
  include module type of struct include Bos.Pat end
end

module Cmd : sig
  include module type of struct include Bos.Cmd end
end

module OS : sig
  include module type of struct include Bos.OS end
end

(** {1 Fmt & Logs}

    {b Note.} The following aliases are strictly speaking not needed but they
    allow to end-users to use them by expressing a single dependency towards
    [bos.setup]. *)

module Fmt : sig
  include module type of struct include Fmt end
end

module Logs : sig
  include module type of struct include Logs end
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

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
