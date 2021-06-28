open OUnit2

let () = Mirage_crypto_rng_unix.initialize ()

let time f =
  let t1 = Sys.time () in
  let r  = f () in
  let t2 = Sys.time () in
  ( Printf.eprintf "[time] %f.04 s\n%!" (t2 -.  t1) ; r )

let (<+>) = Tls.Utils.Cs.(<+>)

let list_to_cstruct xs =
  let open Cstruct in
  let buf = create (List.length xs) in
  List.iteri (set_uint8 buf) xs ;
  buf

let uint16_to_cstruct i =
  let open Cstruct in
  let buf = create 2 in
  BE.set_uint16 buf 0 i;
  buf

let hexdump_to_str cs =
  let b = Buffer.create 16 in
  Cstruct.hexdump_to_buffer b cs ;
  Buffer.contents b

let assert_cs_eq ?msg cs1 cs2 =
  assert_equal
    ~cmp:Tls.Utils.Cs.equal
    ~printer:hexdump_to_str
    ?msg
    cs1 cs2

let rec assert_lists_eq comparison a b =
  match a, b with
  | [], [] -> ()
  | a::r1, b::r2 -> comparison a b ; assert_lists_eq comparison r1 r2
  | _ -> assert_failure "lists not equal"


let assert_sessionid_equal a b =
  match a, b with
  | None, None -> ()
  | Some x, Some y -> assert_cs_eq x y
  | _ -> assert_failure "session id not equal"

let assert_client_extension_equal a b =
  match a, b with
  | `Hostname a, `Hostname b -> assert_equal a b
  | `MaxFragmentLength a, `MaxFragmentLength b -> assert_equal a b
  | `SupportedGroups a, `SupportedGroups b -> assert_lists_eq assert_equal a b
  | `SecureRenegotiation a, `SecureRenegotiation b -> assert_cs_eq a b
  | `Padding a, `Padding b -> assert_equal a b
  | `SignatureAlgorithms a, `SignatureAlgorithms b ->
    assert_lists_eq (fun sa sa' -> assert_equal sa sa') a b
  | `ALPN a, `ALPN b -> assert_lists_eq assert_equal a b
  | _ -> assert_failure "extensions did not match"

let assert_server_extension_equal a b =
  match a, b with
  | `Hostname, `Hostname -> ()
  | `MaxFragmentLength a, `MaxFragmentLength b -> assert_equal a b
  | `SecureRenegotiation a, `SecureRenegotiation b -> assert_cs_eq a b
  | `ALPN a, `ALPN b -> assert_equal a b
  | _ -> assert_failure "extensions did not match"

let cs_mmap file =
  Unix_cstruct.of_fd Unix.(openfile file [O_RDONLY] 0)
