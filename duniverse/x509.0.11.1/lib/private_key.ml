
type t = [ `RSA of Mirage_crypto_pk.Rsa.priv ]

let keytype = function
  | `RSA _ -> `RSA

module Asn = struct
  open Asn_grammars
  open Asn.S
  open Mirage_crypto_pk

  (* RSA *)
  let other_prime_infos =
    sequence_of @@
      (sequence3
        (required ~label:"prime"       integer)
        (required ~label:"exponent"    integer)
        (required ~label:"coefficient" integer))

  let rsa_private_key =
    let f (v, (n, (e, (d, (p, (q, (dp, (dq, (q', other))))))))) =
      match (v, other) with
      | (0, None) ->
        begin match Rsa.priv ~e ~d ~n ~p ~q ~dp ~dq ~q' with
          | Ok p -> p
          | Error (`Msg m) -> parse_error "bad RSA private key %s" m
        end
      | _         -> parse_error "multi-prime RSA keys not supported"
    and g { Rsa.e; d; n; p; q; dp; dq; q' } =
      (0, (n, (e, (d, (p, (q, (dp, (dq, (q', None))))))))) in
    map f g @@
    sequence @@
        (required ~label:"version"         int)
      @ (required ~label:"modulus"         integer)  (* n    *)
      @ (required ~label:"publicExponent"  integer)  (* e    *)
      @ (required ~label:"privateExponent" integer)  (* d    *)
      @ (required ~label:"prime1"          integer)  (* p    *)
      @ (required ~label:"prime2"          integer)  (* q    *)
      @ (required ~label:"exponent1"       integer)  (* dp   *)
      @ (required ~label:"exponent2"       integer)  (* dq   *)
      @ (required ~label:"coefficient"     integer)  (* qinv *)
     -@ (optional ~label:"otherPrimeInfos" other_prime_infos)

  (* For outside uses. *)
  let (rsa_private_of_cstruct, rsa_private_to_cstruct) =
    projections_of Asn.der rsa_private_key

  (* PKCS8 *)
  let rsa_priv_of_cs, rsa_priv_to_cs = project_exn rsa_private_key

  let reparse_private = function
    | (0, Algorithm.RSA, cs) -> rsa_priv_of_cs cs
    | _ -> parse_error "unknown private key info"

  let unparse_private pk =
    (0, Algorithm.RSA, rsa_priv_to_cs pk)

  let private_key_info =
    map reparse_private unparse_private @@
    sequence3
      (required ~label:"version"             int)
      (required ~label:"privateKeyAlgorithm" Algorithm.identifier)
      (required ~label:"privateKey"          octet_string)
      (* TODO: there's an
         (optional ~label:"attributes" @@ implicit 0 (SET of Attributes)
         which are defined in X.501; but nobody seems to use them anyways *)

  let (private_of_cstruct, private_to_cstruct) =
    projections_of Asn.der private_key_info
end

(* TODO what about RSA PRIVATE vs PRIVATE?
   - atm decode handles both, encode uses PRIVATE *)

let decode_der cs =
  let open Rresult.R.Infix in
  Asn_grammars.err_to_msg (Asn.private_of_cstruct cs) >>| fun key ->
  `RSA key

let encode_der = function
  | `RSA k -> Asn.private_to_cstruct k

let decode_pem cs =
  let open Rresult.R.Infix in
  Pem.parse cs >>= fun data ->
  let rsa_p (t, _) = String.equal "RSA PRIVATE KEY" t
  and pk_p (t, _) = String.equal "PRIVATE KEY" t
  in
  let r, _ = List.partition rsa_p data
  and p, _ = List.partition pk_p data
  in
  Pem.foldM (fun (_, k) ->
      Asn_grammars.err_to_msg (Asn.rsa_private_of_cstruct k)) r >>= fun k ->
  Pem.foldM (fun (_, k) ->
      Asn_grammars.err_to_msg (Asn.private_of_cstruct k)) p >>= fun k' ->
  Pem.exactly_one ~what:"private key" (k @ k') >>| fun k ->
  `RSA k

let encode_pem = function
  | `RSA v ->
    Pem.unparse ~tag:"PRIVATE KEY" (Asn.private_to_cstruct v)
