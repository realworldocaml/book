(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012-2014 Citrix Systems Inc
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

(** Blocking Lwt functions to read and write from Cstruct buffers. *)

val read: Lwt_unix.file_descr -> Cstruct.t -> int Lwt.t
(** [read fd t] reads data from the file descriptor [fd] into the
    [t] cstruct.
    @return the numbers of bytes actually read. *)

val write: Lwt_unix.file_descr -> Cstruct.t -> int Lwt.t
(** [write fd t] writes data from the [t] cstruct to the file
    descriptor [fd].
    @return the numbers of bytes actually written. *)

val complete: (Cstruct.t -> int Lwt.t) -> Cstruct.t -> unit Lwt.t
(** [complete (read fd) t] fills [t] with data from [fd].

    [complete (write fd) t] fully-writes [t] to [fd].

    @raise End_of_file if the file descriptor operation fails.
  *)

val sendto: Lwt_unix.file_descr -> Cstruct.t -> Unix.msg_flag list -> Unix.sockaddr -> int Lwt.t
(** [sendto fd t flags sa] invokes {!Lwt_unix.sendto} on the [t] cstruct.
    @return the number of bytes sent. *)

val recvfrom: Lwt_unix.file_descr -> Cstruct.t -> Unix.msg_flag list -> (int * Unix.sockaddr) Lwt.t
(** [recvfrom fd t flags sa] invokes {!Lwt_unix.recvfrom} on the [t] cstruct.
    @return the number of bytes read and the socket address of the remote side. *)
