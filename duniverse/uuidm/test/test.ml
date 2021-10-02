(* This code is in the public domain *)

let str = Printf.sprintf
let exec = Filename.basename Sys.executable_name

let str_eq u s = (Uuidm.to_string u) = s
let id_eq u s = match Uuidm.of_string s with
| None -> false
| Some u' -> Uuidm.equal u u'

let main () =
  let usage =
    str "Usage: %s [OPTION]...\n\
         \ UUID test suite\n\
         Options:" exec
  in
  Arg.parse (Arg.align []) (fun _ -> ()) usage;
  assert (str_eq Uuidm.ns_dns "6ba7b810-9dad-11d1-80b4-00c04fd430c8");
  assert (str_eq Uuidm.ns_url "6ba7b811-9dad-11d1-80b4-00c04fd430c8");
  assert (str_eq Uuidm.ns_oid "6ba7b812-9dad-11d1-80b4-00c04fd430c8");
  assert (str_eq Uuidm.ns_X500 "6ba7b814-9dad-11d1-80b4-00c04fd430c8");
  assert (id_eq Uuidm.ns_dns "6ba7b810-9dad-11d1-80b4-00c04fd430c8");
  assert (id_eq Uuidm.ns_url "6bA7b811-9daD-11d1-80b4-00c04fd430c8");
  assert (id_eq Uuidm.ns_oid "6ba7b812-9dad-11d1-80b4-00c04Fd430C8");
  assert (id_eq Uuidm.ns_X500 "6ba7B814-9dad-11d1-80b4-00c04fd430c8");
  assert (id_eq Uuidm.(unsafe_of_bytes @@ to_mixed_endian_bytes ns_X500)
            "14B8a76b-ad9d-d111-80b4-00c04fd430c8");
  assert (match Uuidm.(of_mixed_endian_bytes (to_bytes ns_X500)) with
    | None -> assert false
    | Some id -> id_eq id "14B8a76b-ad9d-d111-80b4-00c04fd430c8");
  assert (id_eq (Uuidm.v (`V3 (Uuidm.ns_dns, "www.widgets.com")))
	    "3D813CBB-47FB-32BA-91DF-831E1593AC29");
  assert (id_eq (Uuidm.v (`V5 (Uuidm.ns_dns, "www.widgets.com")))
	    "21F7F8DE-8051-5B89-8680-0195EF798B6A");
  assert (id_eq (Uuidm.v (`V3 (Uuidm.ns_dns, "www.example.org")))
	    "0012416f-9eec-3ed4-a8b0-3bceecde1cd9");
  assert (id_eq (Uuidm.v (`V5 (Uuidm.ns_dns, "www.example.org")))
	    "74738ff5-5367-5958-9aee-98fffdcd1876");
  print_endline "Tests succeeded."

let () = main ()
