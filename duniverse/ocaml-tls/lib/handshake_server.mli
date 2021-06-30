open State

val hello_request : handshake_state -> (handshake_return, failure) result

val handle_change_cipher_spec : server_handshake_state -> handshake_state -> Cstruct.t -> (handshake_return, failure) result
val handle_handshake : server_handshake_state -> handshake_state -> Cstruct.t -> (handshake_return, failure) result
