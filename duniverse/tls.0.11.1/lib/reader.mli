
type error =
  | TrailingBytes  of string
  | WrongLength    of string
  | Unknown        of string
  | Underflow
  | Overflow       of int
  | UnknownVersion of (int * int)
  | UnknownContent of int

val error_of_sexp : Sexplib.Sexp.t -> error
val sexp_of_error : error -> Sexplib.Sexp.t

type nonrec 'a result = ('a, error) result

val parse_version     : Cstruct.t -> Core.tls_version result
val parse_any_version : Cstruct.t -> Core.tls_any_version result
val parse_record      : Cstruct.t ->
  [ `Record of (Core.tls_hdr * Cstruct.t) * Cstruct.t
  | `Fragment of Cstruct.t
  ] result

val parse_handshake_frame : Cstruct.t -> (Cstruct.t option * Cstruct.t)
val parse_handshake : Cstruct.t -> Core.tls_handshake result

val parse_alert     : Cstruct.t -> Core.tls_alert result

val parse_change_cipher_spec   : Cstruct.t -> unit result

val parse_certificate_request     : Cstruct.t -> (Packet.client_certificate_type list * Cstruct.t list) result
val parse_certificate_request_1_2 : Cstruct.t -> (Packet.client_certificate_type list * (Mirage_crypto.Hash.hash * Packet.signature_algorithm_type) list * Cstruct.t list) result

val parse_dh_parameters        : Cstruct.t -> (Core.dh_parameters * Cstruct.t * Cstruct.t) result
val parse_digitally_signed     : Cstruct.t -> Cstruct.t result
val parse_digitally_signed_1_2 : Cstruct.t -> (Mirage_crypto.Hash.hash * Packet.signature_algorithm_type * Cstruct.t) result
