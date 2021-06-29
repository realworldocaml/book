(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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
 *
 *)

(** Resolve URIs to endpoints using Unix system calls *)

(** {2 Prebuilt resolvers} *)

val system : Resolver_lwt.t
(** Use the Unix system name resolver via [getaddrinfo] and [getservbyname] *)

val static : (string, Conduit.endp) Hashtbl.t -> Resolver_lwt.t
(** [static hosts] constructs a resolver that looks up any resolution requests
    from the static [hosts] hashtable instead of using the system resolver. *)

(** {2 Rewrite and service functions}

    These can be used to assemble your own resolvers if the prebuilt ones are
    not quite what you need. *)

val system_service : string -> Resolver_lwt.svc option Lwt.t
(** Perform service lookup using [getservbyname] *)

val static_service : string -> Resolver_lwt.svc option Lwt.t
(** Perform service lookup using the builtin {!Uri_services} module *)

val system_resolver : Resolver_lwt.rewrite_fn
(** Rewrite function that uses the {!system_service} and {!static_service} to
    resolve hosts *)

(** {2 Debugging Hooks} *)

val debug : bool ref
(** If [debug] is true, the builtin resolvers will output their resolution
    responses via the {!debug_print} function. The default value of [debug] is
    true if the [CONDUIT_DEBUG] environment variable is set, and false
    otherwise. *)

val debug_print :
  ((string -> string -> string -> string -> unit, out_channel, unit) format ->
  string ->
  string ->
  string ->
  string ->
  unit)
  ref
(** [debug_print] is called by the {!debug} functions to output the results of
    resolution. Defaults to {!Printf.eprintf} to go to the standard error. *)
