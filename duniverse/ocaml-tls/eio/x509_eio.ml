open Eio.Std

module Path = Eio.Path

let (</>) = Path.( / )

let read_file path =
  Path.load path |> Cstruct.of_string

let extension str =
  let n = String.length str in
  let rec scan = function
    | i when i = 0 -> None
    | i when str.[i - 1] = '.' ->
        Some (String.sub str i (n - i))
    | i -> scan (pred i) in
  scan n


let private_of_pems ~cert ~priv_key =
  let certs =
    try
      let pem = read_file cert in
      match X509.Certificate.decode_pem_multiple pem with
      | Ok cs -> cs
      | Error (`Msg m) -> invalid_arg ("failed to parse certificates " ^ m)
    with Invalid_argument m ->
      Fmt.failwith "Private certificates %a: %s" Path.pp cert m
  in
  let pk =
    try
      let pem = read_file priv_key in
      match X509.Private_key.decode_pem pem with
      | Ok key -> key
      | Error (`Msg m) -> invalid_arg ("failed to parse private key " ^ m)
    with Invalid_argument m ->
      Fmt.failwith "Private key (%a): %s" Path.pp priv_key m
  in
  (certs, pk)

let certs_of_pem path =
  try
    let pem = read_file path in
    match X509.Certificate.decode_pem_multiple pem with
    | Ok cs -> cs
    | Error (`Msg m) -> invalid_arg ("failed to parse certificates " ^ m)
  with Invalid_argument m ->
    Fmt.failwith "Certificates in %a: %s" Path.pp path m

let certs_of_pem_dir path =
  Path.read_dir path
  |> List.filter (fun file -> extension file = Some "crt")
  |> Eio.Fiber.map (fun file -> certs_of_pem (path </> file))
  |> List.concat

let crl_of_pem path =
  try
    let data = read_file path in
    match X509.CRL.decode_der data with
    | Ok cs -> cs
    | Error (`Msg m) -> invalid_arg ("failed to parse CRL " ^ m)
  with Invalid_argument m ->
    Fmt.failwith "CRL in %a: %s" Path.pp path m

let crls_of_pem_dir path =
  Path.read_dir path
  |> Fiber.map (fun file -> crl_of_pem (path </> file))

(* Would be better to take an Eio.Time.clock here, but that API is likely to change soon. *)
let authenticator ?allowed_hashes ?crls param =
  let time () = Some (Ptime_clock.now ()) in
  let of_cas cas =
    let crls = Option.map crls_of_pem_dir crls in
    X509.Authenticator.chain_of_trust ?allowed_hashes ?crls ~time cas
  and dotted_hex_to_cs hex =
    Cstruct.of_hex (String.map (function ':' -> ' ' | x -> x) hex)
  and fingerp hash fingerprint =
    X509.Authenticator.server_key_fingerprint ~time ~hash ~fingerprint
  and cert_fingerp hash fingerprint =
    X509.Authenticator.server_cert_fingerprint ~time ~hash ~fingerprint
  in
  match param with
  | `Ca_file path -> certs_of_pem path |> of_cas
  | `Ca_dir path  -> certs_of_pem_dir path |> of_cas
  | `Key_fingerprint (hash, fp) -> fingerp hash fp
  | `Hex_key_fingerprint (hash, fp) ->
    let fp = dotted_hex_to_cs fp in
    fingerp hash fp
  | `Cert_fingerprint (hash, fp) -> cert_fingerp hash fp
  | `Hex_cert_fingerprint (hash, fp) ->
    let fp = dotted_hex_to_cs fp in
    cert_fingerp hash fp
