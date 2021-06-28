(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Result

module Core = Asn_core
module OID  = Asn_oid

exception Ambiguous_syntax = Core.Ambiguous_syntax

type error = Core.error
let pp_error = Core.pp_error

module S = struct
  type 'a t        = 'a Core.asn
  type 'a element  = 'a Core.element
  type 'a sequence = 'a Core.sequence
  include Asn_combinators
  let (error, parse_error) = Core.(error, parse_error)
end

type 'a t = 'a S.t
type oid = OID.t

type encoding = {
  mk_decoder : 'a. 'a t -> Cstruct.t -> 'a * Cstruct.t;
  mk_encoder : 'a. 'a t -> 'a -> Asn_writer.t
}

let ber = {
  mk_decoder = Asn_ber_der.R.compile_ber ;
  mk_encoder = Asn_ber_der.W.ber_to_writer ;
}

let der = {
  mk_decoder = Asn_ber_der.R.compile_der ;
  mk_encoder = Asn_ber_der.W.der_to_writer ;
}

type 'a codec =
  Codec of (Cstruct.t -> ('a * Cstruct.t)) * ('a -> Asn_writer.t)

let codec { mk_encoder ; mk_decoder } asn =
  let () = Core.validate asn in
  Codec (mk_decoder asn, mk_encoder asn)

let encode (Codec (_, enc)) a =
  Asn_writer.to_cstruct (enc a)

let encode_into (Codec (_, enc)) a =
  Asn_writer.to_writer (enc a)

let decode (Codec (dec, _)) b =
  try Ok (dec b) with Core.Parse_error err -> Error err

let random = Asn_random.r_asn
