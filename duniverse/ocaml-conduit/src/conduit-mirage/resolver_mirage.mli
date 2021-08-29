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

(** Functorial interface for resolving URIs to endpoints. *)

module type S = sig
  include Resolver_lwt.S

  val static : (string, port:int -> Conduit.endp) Hashtbl.t -> t
  (** [static hosts] constructs a resolver that looks up any resolution requests
      from the static [hosts] hashtable instead of using the system resolver. *)

  val localhost : t
  (** [localhost] is a static resolver that has a single entry that maps
      [localhost] to [127.0.0.1], and fails on all other hostnames. *)
end

(** Provides a DNS-enabled {!Resolver_lwt} given a network stack. *)
module Make
    (R : Mirage_random.S)
    (T : Mirage_time.S)
    (C : Mirage_clock.MCLOCK)
    (S : Mirage_stack.V4V6) : sig
  include S

  val v : ?ns:Ipaddr.t -> ?ns_port:int -> S.t -> t
  (** [v ?ns ?ns_port ?stack ()] TODO *)
end
