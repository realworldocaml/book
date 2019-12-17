(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
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

(** {2 Abstract devices}

    This module define the basic functions that a MirageOS device
    should implement.

    {e Release %%VERSION%% } *)

(** The type for device errors. *)
type error = [
  | `Unimplemented     (** operation not yet implemented in the code *)
  | `Disconnected      (** the device has been previously disconnected *)
]

val pp_error: error Fmt.t
(** [pp_error] is the pretty-printer for errors. *)

(** Defines the functions to define what is a device state and how to
    disconnect such a device. *)
module type S = sig

  type t
  (** The type representing the internal state of the device *)

  val disconnect: t -> unit Lwt.t
  (** Disconnect from the device. While this might take some time to
      complete, it can never result in an error. *)

end
