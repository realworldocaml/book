open State

val hello_request : handshake_state -> handshake_return eff

val handle_change_cipher_spec : server_handshake_state -> handshake_state -> Cstruct.t -> handshake_return eff
val handle_handshake : server_handshake_state -> handshake_state -> Cstruct.t -> handshake_return eff
