type t = [ `RSA | `ED25519 | `P224 | `P256 | `P384 | `P521  ]

let strings =
  [ ("rsa", `RSA) ; ("ed25519", `ED25519) ; ("p224", `P224) ;
    ("p256", `P256) ; ("p384", `P384) ; ("p521", `P521) ]

let to_string kt = fst (List.find (fun (_, k) -> kt = k) strings)

let of_string s =
  match List.assoc_opt (String.lowercase_ascii s) strings with
  | Some kt -> Ok kt
  | None ->
    Error (`Msg (Fmt.str "unkown key type %s, supported are %a"
                   s Fmt.(list ~sep:(any ", ") string) (List.map fst strings)))

let pp ppf t = Fmt.string ppf (to_string t)

type signature_scheme = [ `RSA_PSS | `RSA_PKCS1 | `ECDSA | `ED25519 ]

let signature_scheme_to_string = function
  | `RSA_PSS -> "RSA-PSS"
  | `RSA_PKCS1 -> "RSA-PKCS1"
  | `ECDSA -> "ECDSA"
  | `ED25519 -> "ED25519"

let pp_signature_scheme ppf s = Fmt.string ppf (signature_scheme_to_string s)

let supports_signature_scheme key_typ scheme =
  match key_typ, scheme with
  | `RSA, (`RSA_PSS | `RSA_PKCS1) -> true
  | `ED25519, `ED25519 -> true
  | (`P224 | `P256 | `P384 | `P521), `ECDSA -> true
  | _ -> false

let opt_signature_scheme ?scheme kt =
  match scheme with
  | Some x -> x
  | None -> match kt with
    | `RSA -> `RSA_PSS
    | `ED25519 -> `ED25519
    | `P224 | `P256 | `P384 | `P521 -> `ECDSA

(* the default of RSA keys should be PSS, but most deployed certificates still
   use PKCS1 (and this library uses pkcs1 by default as well) *)
let x509_default_scheme = function
  | `RSA -> `RSA_PKCS1
  | x -> opt_signature_scheme x
