
val assemble_protocol_version : Core.tls_version -> Cstruct.t

val assemble_handshake : Core.tls_handshake -> Cstruct.t

val assemble_hdr : Core.tls_version -> (Packet.content_type * Cstruct.t) -> Cstruct.t

val assemble_alert : ?level:Packet.alert_level -> Packet.alert_type -> Cstruct.t

val assemble_change_cipher_spec : Cstruct.t

val assemble_dh_parameters : Core.dh_parameters -> Cstruct.t

val assemble_digitally_signed : Cstruct.t -> Cstruct.t

val assemble_digitally_signed_1_2 : Nocrypto.Hash.hash -> Packet.signature_algorithm_type -> Cstruct.t -> Cstruct.t

val assemble_certificate_request : Packet.client_certificate_type list -> Cstruct.t list -> Cstruct.t

val assemble_certificate_request_1_2 : Packet.client_certificate_type list -> (Nocrypto.Hash.hash * Packet.signature_algorithm_type) list -> Cstruct.t list -> Cstruct.t
