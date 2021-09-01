open X509

(*
How files for test1 were generated:
key.pem:
openssl genpkey -algorithm RSA -out key.pem -outform PEM

certificate.pem:
openssl req -new -x509 -days 3650 -key key.pem -out certificate.pem -subj '/CN=example.com/' -sha256

test1.pem:
openssl req -new -key key.pem -nodes -out test1.csr \
    -subj '/CN=test1.example.com/'
openssl x509 -req -days 3650 -in test1.csr -CA certificate.pem \
    -CAkey key.pem -set_serial 10000 -out test1.pem
openssl x509 -in test1.pem -noout -text

request.der:
openssl ocsp -issuer certificate.pem \
   -cert test1.pem \
   -no_nonce -reqout request.der
openssl ocsp -reqin request.der -text

response.der:
openssl ocsp -index index.txt -rsigner certificate.pem \
   -rkey key.pem -CA certificate.pem \
   -reqin request.der -respout response.der
openssl ocsp -respin response.der -CAfile certificate.pem -text

*)

let cs_mmap file =
  Unix_cstruct.of_fd Unix.(openfile file [O_RDONLY] 0)

let data file = cs_mmap ("./ocsp/" ^ file)

let responder_cert = match Certificate.decode_pem (data "certificate.pem") with
  | Ok c -> c
  | Error _ -> assert false
let responder_dn = Certificate.subject responder_cert
let test1_serial = Z.of_int 0x2710

let responder_key = match Private_key.decode_pem (data "key.pem") with
  | Ok k -> k
  | Error _ -> assert false

let z_testable = Alcotest.testable Z.pp_print Z.equal
let cert_dn_testable = Alcotest.testable Distinguished_name.pp Distinguished_name.equal


let test_request () =
  let open OCSP.Request in
  match decode_der (data "request.der") with
  | Error _ -> Alcotest.fail "could not decode OCSP request"
  | Ok request ->
    (* Fmt.pr "request=%a" pp request; *)
    (* TODO: verify *)
    match cert_ids request with
    | [certid] ->
      let serialNumber = OCSP.cert_id_serial certid in
      Alcotest.(check z_testable __LOC__ test1_serial serialNumber)
    | _ -> Alcotest.fail "something wrong with OCSP request"

let test_response () =
  let open OCSP.Response in
  match decode_der (data "response.der") with
  | Error e ->
    Alcotest.failf "could not decode OCSP response: %a" Asn.pp_error e
  | Ok response ->
    (* Fmt.pr "response=%a" pp response; *)
    (match validate response (Private_key.public responder_key) with
     | Ok () -> ()
     | Error _ -> Alcotest.fail "cannot verify the signature of OCSP response");
    let responder = match responder_id response with
      | Ok (`ByName r) -> r
      | Ok _ -> Alcotest.fail "expected responder identifyed by name"
      | Error (`Msg e) -> Alcotest.fail e
    in
    let response = match responses response with
      | Ok [r] -> r
      | Ok _ -> Alcotest.fail "must be exactly one response"
      | Error (`Msg e) -> Alcotest.fail e
    in
    let certid = single_response_cert_id response in
    let serialNumber = OCSP.cert_id_serial certid in
    Alcotest.(check z_testable __LOC__ test1_serial serialNumber);
    Alcotest.(check cert_dn_testable __LOC__ responder responder_dn)

let test_simple_responder () =
  match OCSP.Request.decode_der (data "request.der") with
  | Error _ -> Alcotest.fail "could not decode OCSP request"
  | Ok request ->
    let certids = OCSP.Request.cert_ids request in
    let now = Ptime_clock.now () in
    let response_logic cert_id =
      let serial = OCSP.cert_id_serial cert_id in
      let cert_status =
        if Z.equal test1_serial serial then
          `Revoked (now, None)
        else
          `Good
      in
      OCSP.Response.create_single_response cert_id cert_status now
    in
    let responses = List.map response_logic certids in
    let responder_id = `ByName responder_dn in
    (* Fmt.pr "keytype = %a" Key_type.pp (Private_key.key_type responder_key); *)
    match OCSP.Response.create_success ~certs:[responder_cert] responder_key responder_id now responses with
    | Error (`Msg e) -> Alcotest.fail e
    | Ok resp ->
      match OCSP.Response.validate resp (Private_key.public responder_key) with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "cannot verify the signature of OCSP response"

let tests = [
  "OpenSSL request", `Quick, test_request ;
  "OpenSSL response", `Quick, test_response ;
  "Simple OCSP responder", `Quick, test_simple_responder ;
]
