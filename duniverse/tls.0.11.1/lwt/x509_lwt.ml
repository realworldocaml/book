open Lwt

type priv = X509.Certificate.t list * Mirage_crypto_pk.Rsa.priv

type authenticator = X509.Authenticator.t

let failure msg = fail @@ Failure msg

let catch_invalid_arg th h =
  Lwt.catch (fun () -> th)
    (function
      | Invalid_argument msg -> h msg
      | exn                  -> fail exn)

let (</>) a b = a ^ "/" ^ b

let o f g x = f (g x)

let read_file path =
  let open Lwt_io in
  open_file ~mode:Input path >>= fun file ->
  read file >|= Cstruct.of_string >>= fun cs ->
  close file >|= fun () ->
  cs

let read_dir path =
  let open Lwt_unix in
  let rec collect acc d =
    readdir_n d 10 >>= function
      | [||] -> return acc
      | xs   -> collect (Array.to_list xs @ acc) d in
  opendir path >>= fun dir ->
  collect [] dir >>= fun entries ->
  closedir dir >|= fun () ->
  entries

let extension str =
  let n = String.length str in
  let rec scan = function
    | i when i = 0 -> None
    | i when str.[i - 1] = '.' ->
        Some (String.sub str i (n - i))
    | i -> scan (pred i) in
  scan n


let private_of_pems ~cert ~priv_key =
  catch_invalid_arg
    (read_file cert >|= fun pem ->
     match X509.Certificate.decode_pem_multiple pem with
     | Ok cs -> cs
     | Error (`Msg m) -> invalid_arg ("failed to parse certificates " ^ m))
    (o failure @@ Printf.sprintf "Private certificates (%s): %s" cert) >>= fun certs ->
  catch_invalid_arg
    (read_file priv_key >|= fun pem ->
     match X509.Private_key.decode_pem pem with
     | Ok (`RSA key) -> key
     | Error (`Msg m) -> invalid_arg ("failed to parse private key " ^ m))
    (o failure @@ Printf.sprintf "Private key (%s): %s" priv_key) >>= fun pk ->
  return (certs, pk)

let certs_of_pem path =
  catch_invalid_arg
    (read_file path >|= fun pem ->
     match X509.Certificate.decode_pem_multiple pem with
     | Ok cs -> cs
     | Error (`Msg m) -> invalid_arg ("failed to parse certificates " ^ m))
    (o failure @@ Printf.sprintf "Certificates in %s: %s" path)

let certs_of_pem_dir path =
  read_dir path
  >|= List.filter (fun file -> extension file = Some "crt")
  >>= Lwt_list.map_p (fun file -> certs_of_pem (path </> file))
  >|= List.concat

let crl_of_pem path =
  catch_invalid_arg
    (read_file path >|= fun data ->
     match X509.CRL.decode_der data with
     | Ok cs -> cs
     | Error (`Msg m) -> invalid_arg ("failed to parse CRL " ^ m))
    (o failure @@ Printf.sprintf "CRL in %s: %s" path)

let crls_of_pem_dir = function
  | None -> Lwt.return None
  | Some path ->
    read_dir path >>= fun files ->
    Lwt_list.map_p (fun file -> crl_of_pem (path </> file)) files >|= fun crls ->
    Some crls

let authenticator ?hash_whitelist ?crls param =
  let time () = Some (Ptime_clock.now ()) in
  let of_cas cas =
    crls_of_pem_dir crls >|= fun crls ->
    X509.Authenticator.chain_of_trust ?hash_whitelist ?crls ~time cas
  and dotted_hex_to_cs hex =
    Cstruct.of_hex (String.map (function ':' -> ' ' | x -> x) hex)
  and fingerp hash fingerprints =
    X509.Authenticator.server_key_fingerprint ~time ~hash ~fingerprints
  and cert_fingerp hash fingerprints =
    X509.Authenticator.server_cert_fingerprint ~time ~hash ~fingerprints
  in
  match param with
  | `Ca_file path -> certs_of_pem path >>= of_cas
  | `Ca_dir path  -> certs_of_pem_dir path >>= of_cas
  | `Key_fingerprints (hash, fps) -> return (fingerp hash fps)
  | `Hex_key_fingerprints (hash, fps) ->
    let fps = List.map (fun (n, v) -> (n, dotted_hex_to_cs v)) fps in
    return (fingerp hash fps)
  | `Cert_fingerprints (hash, fps) -> return (cert_fingerp hash fps)
  | `Hex_cert_fingerprints (hash, fps) ->
    let fps = List.map (fun (n, v) -> (n, dotted_hex_to_cs v)) fps in
    return (cert_fingerp hash fps)
