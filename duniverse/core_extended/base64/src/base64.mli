(** Simple library for encoding and decoding base64 strings *)
open! Core_kernel

module type T = sig
  val encode : string -> string

  (** The decoded string along with any unconsumed data *)
  val decode : string -> string * [`Unconsumed_data of string] option

  (** Raises if [decode] has any unconsumed data *)
  val decode_exn : string -> string
end

include T

(** Web/file safe encoding, replacing '+' and '/' with '-' and '_' respectively. There is
    no padding when encoding.
    See Table 2 in RFC4648 (http://www.ietf.org/rfc/rfc4648.txt)
    for the full character set *)
module Websafe : T

(** Create other Base64 derivatives *)
module Make (D : sig
    (** Usually [ '+' ] *)
    val char62 : char

    (** Usually [ '/' ] *)
    val char63 : char

    (** Usually [ '=' ]. *)
    val pad_char : char

    (** Usually [ true ] *)
    val pad_when_encoding : bool

    (** Usually [ Char.is_whitespace ] *)
    val ignore_char : char -> bool
  end) : T
