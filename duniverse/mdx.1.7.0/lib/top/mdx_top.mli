(*
 * Copyright (c) 2017 Frédéric Bour
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Result

(** Toplevel logic for [mdx]. *)

type t
(** The type for configuration values. *)

val init : verbose:bool -> silent:bool -> verbose_findlib:bool -> unit -> t
(** [init ()] is a new configuration value. *)

val eval : t -> string list -> (string list, string list) result
(** [eval t p] evaluates the toplevel phrase [p] (possibly spawning on
    mulitple lines) with the configuration value [t]. *)

val in_env : Mdx.Ocaml_env.t -> (unit -> 'a) -> 'a
