
val assemble_protocol_version : Core.tls_version -> Cstruct.t

val assemble_handshake : Core.tls_handshake -> Cstruct.t

val assemble_message_hash : int -> Cstruct.t

val assemble_hdr : Core.tls_version -> (Packet.content_type * Cstruct.t) -> Cstruct.t

val assemble_alert : ?level:Packet.alert_level -> Packet.alert_type -> Cstruct.t

val assemble_change_cipher_spec : Cstruct.t

val assemble_dh_parameters : Core.dh_parameters -> Cstruct.t

val assemble_ec_parameters : Core.group -> Cstruct.t -> Cstruct.t

val assemble_client_dh_key_exchange : Cstruct.t -> Cstruct.t

val assemble_client_ec_key_exchange : Cstruct.t -> Cstruct.t

val assemble_digitally_signed : Cstruct.t -> Cstruct.t

val assemble_digitally_signed_1_2 : Core.signature_algorithm -> Cstruct.t -> Cstruct.t

val assemble_certificate_request : Packet.client_certificate_type list -> Cstruct.t list -> Cstruct.t

val assemble_certificate_request_1_2 : Packet.client_certificate_type list -> Core.signature_algorithm list -> Cstruct.t list -> Cstruct.t

val assemble_certificate_request_1_3 : ?context:Cstruct.t -> Core.certificate_request_extension list -> Cstruct.t

val assemble_certificates : Cstruct.t list -> Cstruct.t

val assemble_certificates_1_3 : Cstruct.t -> Cstruct.t list -> Cstruct.t
