(*{{{ Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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
  }}}*)

(** Map of HTTP header key and value(s) associated with them.  Since HTTP
    headers can contain duplicate keys, this structure can return a list
    of values associated with a single key. *)
type t [@@deriving sexp]

(** Construct a fresh, empty map of HTTP headers *)
val init : unit -> t

(** Test whether a HTTP headers are empty or not. *)
val is_empty : t -> bool

(** Construct a fresh map of HTTP headers with a single key and value entry *)
val init_with  : string -> string -> t

(** Add a key and value to an existing header map *)
val add : t -> string -> string -> t

(** Add multiple key and value pairs to an existing header map *)
val add_list : t -> (string * string) list -> t

(** Add multiple values to a key in an existing header map *)
val add_multi : t -> string -> string list -> t

(** Given an optional header, either update the existing one with
    a key and value, or construct a fresh header with those values if
    the header is [None] *)
val add_opt : t option -> string -> string -> t

(** Given a header, update it with the key and value unless the key is
    already present in the header *)
val add_unless_exists : t -> string -> string -> t

(** [add_unless_exists h k v] updates [h] with the key [k] and value [v]
    unless the key is already present in the header.  If [h] is [None]
    then a fresh header is allocated containing the key [k] and the
    value [v]. *)
val add_opt_unless_exists : t option -> string -> string -> t

(** Remove a key from the header map and return a fresh header set.  The
    original header parameter is not modified. *)
val remove : t -> string -> t

(** Replace a key from the header map if it exists.  The original
    header parameter is not modified. *)
val replace : t -> string -> string -> t

(** Check if a key exists in the header. *)
val mem : t -> string -> bool

(** Structural comparison of two [Header] values. *)
val compare : t -> t -> int

(** Retrieve a key from a header.  If the header is one of the set of
    headers defined to have list values, then all of the values are
    concatenated into a single string separated by commas and returned.
    If it is a singleton header, then the first value is selected and
    no concatenation is performed. *)
val get : t -> string -> string option

(** Retrieve all of the values associated with a key *)
val get_multi : t -> string -> string list

val iter : (string -> string list -> unit) -> t -> unit
val map : (string -> string list -> string list) -> t -> t
val fold : (string -> string -> 'a -> 'a) -> t -> 'a -> 'a
val of_list : (string * string) list -> t
val to_list : t -> (string * string) list

(** Return header fieds as a list of lines. Beware that each line
  ends with "\r\n" characters. *)
val to_lines : t -> string list

(** Same as {!to_lines} but lines do not end with "\r\n" characters. *)
val to_frames : t -> string list

val to_string : t -> string

val get_content_range : t -> Int64.t option
val get_media_type : t -> string option
val get_connection_close : t -> bool
val get_acceptable_media_ranges : t -> (Accept.media_range * Accept.p list) Accept.qlist
val get_acceptable_charsets : t -> Accept.charset Accept.qlist
val get_acceptable_encodings : t -> Accept.encoding Accept.qlist
val get_acceptable_languages : t -> Accept.language Accept.qlist
val get_transfer_encoding : t -> Transfer.encoding
val add_transfer_encoding : t -> Transfer.encoding -> t
val add_authorization : t -> Auth.credential -> t
val get_authorization : t -> Auth.credential option
val add_authorization_req : t -> Auth.challenge -> t
val is_form : t -> bool
val get_location : t -> Uri.t option

val add_links : t -> Link.t list -> t
val get_links : t -> Link.t list

val user_agent : string
(** The User-Agent header used by this library, including the version
    of cohttp. *)
val prepend_user_agent : t -> string -> t
(** Prepend [user_agent] to the product token already declared in the
    "User-Agent" field (if any). *)

val connection : t -> [`Keep_alive | `Close | `Unknown of string] option

(** Human-readable output, used by the toplevel printer *)
val pp_hum : Format.formatter -> t -> unit
