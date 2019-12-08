open! Core_kernel


(** This library offers a lightweight way for applications protocols to version
    themselves. The more protocols that add themselves to [Known_protocol], the nicer
    error messages we will get when connecting to a service while using the wrong
    protocol. *)

type t [@@deriving bin_io, sexp]

val create : protocol:Known_protocol.t -> supported_versions:int list -> t

(** [negotiate ~allow_legacy_peer ~us ~peer] inspects the magic numbers of [us] and
    [peer]. If the magic numbers match, the highest shared version number is returned.

    If [allow_legacy_peer] then the magic number of [peer] is assumed to be [us] if no
    magic number exists. *)
val negotiate : allow_legacy_peer:bool -> us:t -> peer:t -> int Or_error.t

(** [contains_magic_prefix] reads a bin_protted value of type [t] and returns a boolean
    saying whether this magic number was observed. *)
val contains_magic_prefix : protocol:Known_protocol.t -> bool Bin_prot.Type_class.reader

(** [any_magic_prefix] reads the magic number for one of the known protocols. *)
val any_magic_prefix : Known_protocol.t option Bin_prot.Type_class.reader
