module type AEAD = sig
  type key
  val authenticate_encrypt : key:key -> nonce:Cstruct.t -> ?adata:Cstruct.t ->
    Cstruct.t -> Cstruct.t
  val authenticate_decrypt : key:key -> nonce:Cstruct.t -> ?adata:Cstruct.t ->
    Cstruct.t -> Cstruct.t option
end
