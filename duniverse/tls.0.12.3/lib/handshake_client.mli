open Core
open State

val default_client_hello : Config.config -> (client_hello * tls_version * (group * dh_secret) list)
val handle_change_cipher_spec : client_handshake_state -> handshake_state -> Cstruct.t -> handshake_return eff
val handle_handshake : client_handshake_state -> handshake_state -> Cstruct.t -> handshake_return eff
val answer_hello_request : handshake_state -> handshake_return eff
