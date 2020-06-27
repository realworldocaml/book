open State

val derive_master_secret : Core.tls_before_13 -> session_data -> Cstruct.t -> Cstruct.t list -> Core.master_secret
val initialise_crypto_ctx : Core.tls_before_13 -> session_data -> (crypto_context * crypto_context)
val finished : Core.tls_before_13 -> Ciphersuite.ciphersuite -> Cstruct.t -> string -> Cstruct.t list -> Cstruct.t
