open X509

let time () = None

(* some revocation scenarios to convince myself *)
let ca_exts ?pathlen () =
  let ku =
    [ `Key_cert_sign ; `CRL_sign ; `Digital_signature ; `Content_commitment ]
  in
  Extension.(add Basic_constraints (true, (true, pathlen))
               (singleton Key_usage (true, ku)))

let key_ids exts subject_pubkey issuer_pubkey =
  let subject_key_id =
    false, Public_key.id subject_pubkey
  and authority_key_id =
    let cs = Public_key.id issuer_pubkey in
    false, (Some cs, General_name.empty, None)
  in
  Extension.(add Subject_key_id subject_key_id
               (add Authority_key_id authority_key_id exts))

let leaf_exts =
  Extension.(add Key_usage (true, [ `Digital_signature ; `Key_encipherment ])
               (add Ext_key_usage (true, [ `Server_auth ])
                  (singleton Basic_constraints (true, (false, None)))))

let validity now =
  match Ptime.add_span now (Ptime.Span.of_int_s 3600) with
  | Some fut -> (now, fut)
  | None -> invalid_arg "couldn't add 3600 seconds to now"

let key () =
  let key = Mirage_crypto_pk.Rsa.generate ~bits:1024 () in
  (`RSA (Mirage_crypto_pk.Rsa.pub_of_priv key), `RSA key)

let selfsigned ?(name = "test") now =
  let pub, priv = key () in
  let name = [ Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ] in
  let req = Signing_request.create name priv in
  let valid_from, valid_until = validity now in
  match X509.Signing_request.sign req ~valid_from ~valid_until ~extensions:(ca_exts ()) priv name with
  | Ok cacert -> (cacert, pub, priv)
  | Error _ -> assert false

let cert ?serial ?(name = "sub") now ca pubca privca issuer =
  let pub, priv = key () in
  let name = [ Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ] in
  let req = Signing_request.create name priv in
  let valid_from, valid_until = validity now in
  let extensions = key_ids (if ca then ca_exts () else leaf_exts)  pub pubca in
  match X509.Signing_request.sign req ~valid_from ~valid_until ?serial ~extensions privca issuer with
  | Ok cert -> (cert, pub, priv)
  | Error _ -> assert false

let verify () =
  let now = Ptime_clock.now () in
  let ca, capub, capriv = selfsigned now in
  let cert, _, _ = cert now false capub capriv (Certificate.subject ca) in
  match Validation.verify_chain ~host:None ~time ~anchors:[ca] [cert] with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "expected verification to succeed"

let crl () =
  let now = Ptime_clock.now () in
  let ca, capub, capriv = selfsigned now in
  let serial = Z.of_int 42 in
  let issuer = Certificate.subject ca in
  let cert, _, _ = cert ~serial now false capub capriv issuer in
  let revoked = { CRL.serial ; date = now ; extensions = Extension.empty } in
  let extensions = Extension.(singleton CRL_number (false, 1)) in
  let crl = CRL.revoke ~issuer ~this_update:now ~extensions [revoked] capriv in
  let revoked = CRL.is_revoked [crl] ?hash_whitelist:None in
  match Validation.verify_chain ~host:None ~time ~revoked ~anchors:[ca] [cert] with
  | Ok _ -> Alcotest.fail "expected revocation"
  | Error (`Revoked _) -> ()
  | Error _ -> Alcotest.fail "expected revoked failure!"

let verify' () =
  let now = Ptime_clock.now () in
  let ca, capub, capriv = selfsigned now in
  let serial = Z.of_int 42 in
  let issuer = Certificate.subject ca in
  let ica, ipub, ipriv = cert ~name:"subCA" ~serial now true capub capriv issuer in
  let cert, _pub, _priv = cert now false ipub ipriv (Certificate.subject ica) in
  match Validation.verify_chain ~host:None ~time ~anchors:[ca] [cert ; ica] with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "expected verification!"

let crl' () =
  let now = Ptime_clock.now () in
  let ca, capub, capriv = selfsigned now in
  let serial = Z.of_int 42 in
  let issuer = Certificate.subject ca in
  let ica, ipub, ipriv = cert ~name:"subCA" ~serial now true capub capriv issuer in
  let cert, _pub, _priv = cert now false ipub ipriv (Certificate.subject ica) in
  let revoked = { CRL.serial ; date = now ; extensions = Extension.empty } in
  let extensions = Extension.(singleton CRL_number (false, 1)) in
  let crl = CRL.revoke ~issuer ~this_update:now ~extensions [revoked] capriv in
  let revoked = CRL.is_revoked [crl] ?hash_whitelist:None in
  match Validation.verify_chain ~host:None ~time ~revoked ~anchors:[ca] [cert ; ica] with
  | Ok _ -> Alcotest.fail "expected revocation"
  | Error (`Revoked _) -> ()
  | Error _ -> Alcotest.fail "expected revoked failure!"

let crl'leaf () =
  let now = Ptime_clock.now () in
  let ca, capub, capriv = selfsigned now in
  let serial = Z.of_int 42 in
  let ica, ipub, ipriv = cert ~name:"subCA" now true capub capriv (Certificate.subject ca) in
  let issuer = Certificate.subject ica in
  let cert, _pub, _priv = cert ~serial now false ipub ipriv issuer in
  let revoked = { CRL.serial ; date = now ; extensions = Extension.empty } in
  let extensions = Extension.(singleton CRL_number (false, 1)) in
  let crl = CRL.revoke ~issuer ~this_update:now ~extensions [revoked] ipriv in
  let revoked = CRL.is_revoked [crl] ?hash_whitelist:None in
  match Validation.verify_chain ~host:None ~time ~revoked ~anchors:[ca] [cert ; ica] with
  | Ok _ -> Alcotest.fail "expected revocation"
  | Error (`Revoked _) -> ()
  | Error _ -> Alcotest.fail "expected revoked failure!"

let crl'leaf'wrong () =
  let now = Ptime_clock.now () in
  let ca, capub, capriv = selfsigned now in
  let serial = Z.of_int 42 in
  let issuer = Certificate.subject ca in
  let ica, ipub, ipriv = cert ~name:"subCA" now true capub capriv issuer in
  let cert, _pub, _priv = cert ~serial now false ipub ipriv (Certificate.subject ica) in
  let revoked = { CRL.serial ; date = now ; extensions = Extension.empty } in
  let extensions = Extension.(singleton CRL_number (false, 1)) in
  let crl = CRL.revoke ~issuer ~this_update:now ~extensions [revoked] ipriv in
  let revoked = CRL.is_revoked [crl] ?hash_whitelist:None in
  match Validation.verify_chain ~host:None ~time ~revoked ~anchors:[ca] [cert ; ica] with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "expected success!"

let verify'' () =
  let now = Ptime_clock.now () in
  let ca, capub, capriv = selfsigned now in
  let serial = Z.of_int 42 in
  let issuer = Certificate.subject ca in
  let ica, ipub, ipriv = cert ~name:"subCA" now true capub capriv issuer in
  let cert, _pub, _priv = cert now false ipub ipriv (Certificate.subject ica) in
  let revoked = { CRL.serial ; date = now ; extensions = Extension.empty } in
  let extensions = Extension.(singleton CRL_number (false, 1)) in
  let crl = CRL.revoke ~issuer ~this_update:now ~extensions [revoked] capriv in
  let revoked = CRL.is_revoked [crl] ?hash_whitelist:None in
  match Validation.verify_chain ~host:None ~time ~revoked ~anchors:[ca] [cert ; ica] with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "expected verify to succeed!"

let crl'' () =
  let now = Ptime_clock.now () in
  let ca, capub, capriv = selfsigned now in
  let serial = Z.of_int 42 in
  let issuer = Certificate.subject ca in
  let ica, ipub, ipriv = cert ~name:"subCA" ~serial now true capub capriv issuer in
  let cert, _pub, _priv = cert now false ipub ipriv (Certificate.subject ica) in
  let extensions = Extension.(singleton Reason (false, `Remove_from_CRL)) in
  let revoked = { CRL.serial ; date = now ; extensions } in
  let extensions = Extension.(singleton CRL_number (false, 1)) in
  let crl = CRL.revoke ~issuer ~this_update:now ~extensions [revoked] capriv in
  let revoked = CRL.is_revoked [crl] ?hash_whitelist:None in
  match Validation.verify_chain ~host:None ~time ~revoked ~anchors:[ca] [cert ; ica] with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "expected proper verification!"

let revoke_tests = [
  "Verify with a chain works", `Quick, verify ;
  "Verify with a revoked leaf fails", `Quick, crl ;
  "Verify with a longer chain works", `Quick, verify' ;
  "Verify with a revoked intermediate fails", `Quick, crl' ;
  "Verify with a longer chain works, even if some random serial is revoked", `Quick, verify'' ;
  "Verify with a revoked `Remove_from_CRL works", `Quick, crl'' ;
  "Verify with revoked leaf fails", `Quick, crl'leaf ;
  "Verify with wrongly revoked leaf works", `Quick, crl'leaf'wrong ;
]
