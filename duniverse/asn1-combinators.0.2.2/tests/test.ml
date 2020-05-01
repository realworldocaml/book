(* Copyright (c) 2014-2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

let pp_hex_cs ppf =
  let pp ppf cs =
    for i = 0 to Cstruct.len cs - 1 do
      Format.fprintf ppf "%02x@ " (Cstruct.get_uint8 cs i)
    done in
  Format.fprintf ppf "@[%a@]" pp

let pp_hex_s ppf =
  let pp ppf = String.iter @@ fun c ->
    Format.fprintf ppf "%02x@ " (Char.code c) in
  Format.fprintf ppf "@[%a@]" pp

let x s =
  let rec go buf sb =
    match Scanf.bscanf sb " %02x" Char.chr with
      b -> Buffer.add_char buf b; go buf sb
    | exception End_of_file -> Buffer.contents buf in
  go (Buffer.create 17) (Scanf.Scanning.from_string s)

let x_cs s = Cstruct.of_string (x s)

let cstruct = Alcotest.testable pp_hex_cs Cstruct.equal
let err = Alcotest.testable Asn.pp_error (fun (`Parse _) (`Parse _) -> true)
let dec t = Alcotest.(result (pair t cstruct) err)
let testable ?(pp = fun ppf _ -> Fmt.pf ppf "*shrug*") ?(cmp = (=)) () =
  Alcotest.testable pp cmp

let pp_e ppf = function
  | #Asn.error as e -> Asn.pp_error ppf e
  | `Leftover b     -> Format.fprintf ppf "Leftover: %a" pp_hex_cs b

let get = function Some x -> x | _ -> assert false

type 'a cmp = 'a -> 'a -> bool

type case_eq =
  CEQ : string * 'a Alcotest.testable * 'a Asn.t * ('a * string) list -> case_eq

let case_eq name ?pp ?cmp asn examples =
  CEQ (name, testable ?pp ?cmp (), asn, examples)

type case = C : string * 'a Asn.t * string list -> case

let case name asn examples = C (name, asn, examples)

let accepts_eq name enc cases =
  let tests = cases |> List.map @@ fun (CEQ (name, alc, asn, xs)) ->
    let codec = Asn.codec enc asn
    and t = dec alc in
    let f () = xs |> List.iter @@ fun (x, s) ->
      Alcotest.check t name (Ok (x, Cstruct.empty)) (Asn.decode codec (x_cs s)) in
    (name, `Quick, f) in
  (name, tests)

let rejects name enc cases =
  let tests = cases |> List.map @@ fun (C (name, asn, ss)) ->
    let codec = Asn.codec enc asn
    and t = dec (testable ()) in
    let f () = ss |> List.iter @@ fun s ->
      Alcotest.check t name (Error (`Parse "...")) (Asn.decode codec (x_cs s)) in
    (name, `Quick, f) in
  (name, tests)

let accepts name enc cases =
  let tests = cases |> List.map @@ fun (C (name, asn, ss)) ->
    let f () = ss |> List.iter @@ fun s ->
      match Asn.(decode (codec enc asn)) (x_cs s) with
        Ok (_, t) ->
          Alcotest.check cstruct "no remainder" Cstruct.empty t
      | Error e ->
          Alcotest.failf "decode failed with: %a" pp_e e in
    (name, `Quick, f) in
  (name, tests)

let inverts1 ?(iters = 1000) name enc cases =
  let tests = cases |> List.map @@ fun (CEQ (name, alc, asn, _)) ->
    let codec = Asn.codec enc asn and t = dec alc in
    let f () =
      for _ = 1 to iters do
        let x = Asn.random asn in
        Alcotest.check t "invert" (Ok (x, Cstruct.empty))
          (Asn.decode codec (Asn.encode codec x))
      done in
    (name, `Quick, f) in
  (name, tests)

let time ?(frac=0) dtz =
  Ptime.(add_span (of_date_time dtz |> get)
    (Span.v (0, Int64.(mul (of_int frac) 1_000_000_000L))) |> get)

let v32 = Z.of_int32
let (<+) z1 i64 = Z.((z1 lsl 64) lor (extract (of_int64 i64) 0 64))

let cases = [

  case_eq "bool" Asn.S.bool [
    false, "010100" ;
    true , "0101ff"
  ];

  case_eq "integer" Asn.S.integer [

    v32 0x000000_00l, "0201 00";
    v32 0x000000_7fl, "0201 7f";
    v32 0xffffff_80l, "0201 80";
    v32 0xffffff_ffl, "0201 ff";

    v32 0x0000_0080l, "0202 0080";
    v32 0x0000_7fffl, "0202 7fff";
    v32 0xffff_8000l, "0202 8000";
    v32 0xffff_ff7fl, "0202 ff7f";

    v32 0x00_008000l, "0203 008000";
    v32 0x00_00ffffl, "0203 00ffff";
    v32 0xff_800000l, "0203 800000";
    v32 0xff_ff7fffl, "0203 ff7fff";

    v32 0x00800000l, "0204 00800000";
    v32 0x7fffffffl, "0204 7fffffff";
    v32 0x80000000l, "0204 80000000";
    v32 0xff7fffffl, "0204 ff7fffff";

    v32 0x00800000l <+ 0x00000000_00000000L, "020c 00800000 00000000 00000000";
    v32 0x00ffffffl <+ 0xffffffff_ffffffffL, "020c 00ffffff ffffffff ffffffff";
    v32 0x00ffffffl <+ 0x7fffffff_ffffffffL, "020c 00ffffff 7fffffff ffffffff";
    v32 0x00ffffffl <+ 0xffffffff_7fffffffL, "020c 00ffffff ffffffff 7fffffff";
    v32 0x80ffffffl <+ 0xffffffff_ffffffffL, "020c 80ffffff ffffffff ffffffff";
    v32 0xff7fffffl <+ 0xffffffff_ffffffffL, "020c ff7fffff ffffffff ffffffff";
  ];

  case_eq "null" Asn.S.null [
    (), "0500";
    (), "058100"
  ];

  case_eq "singleton seq"
    Asn.S.(sequence (single @@ required bool))
    [ true, "30030101ff";
      true, "30800101ff0000" ; ];

  case_eq "rename stack"
    Asn.S.(implicit 1 @@ implicit 2 @@ explicit 3 @@ implicit 4 @@ int)
    [ 42, "a18084012a0000";
      42, "a10384012a" ];

  case_eq "sequence with implicits"
    Asn.S.(sequence3
            (required int)
            (required @@ implicit 1 bool)
            (required bool))

    [ (42, false, true), "3009 02012a 810100 0101ff";
      (42, false, true), "3080 02012a 810100 0101ff 0000" ];

  case_eq "sequence with optional and explicit fields"
    Asn.S.(sequence3
            (required @@ implicit 1 int)
            (optional @@ explicit 2 bool)
            (optional @@ implicit 3 bool))

    [ (255, Some true, Some false),
      "300c 810200ff a203 0101f0 830100" ;
      (255, Some true, Some false),
      "3080 810200ff a203 0101f0 830100 0000";
      (255, Some true, Some false),
      "3080 810200ff a280 0101f0 0000 830100 0000" ];

  case_eq "sequence with missing optional and choice fields"
    Asn.S.(sequence3 (required @@ choice2 bool int)
                     (optional @@ choice2 bool int)
                     (optional @@ explicit 0
                               @@ choice2 int (implicit 1 int)))

    [ (`C1 true, None, None), "30030101ff" ;
      (`C1 false, Some (`C2 42), None), "3006 010100 02012a";
      (`C1 true, None, Some (`C1 42)), "3008 0101ff a003 02012a" ;
      (`C2 (-2), Some (`C2 42), Some (`C2 42)), "300b 0201fe 02012a a003 81012a";
      (`C2 (-3), None, Some (`C2 42)), "300a 0201fd a080 81012a0000";
      (`C2 (-4), None, Some (`C1 42)), "3080 0201fc a080 02012a0000 0000" ];

  case_eq "sequence with sequence"
    Asn.S.(sequence2
            (required @@
              sequence2
                (optional @@ implicit 1 bool)
                (optional bool))
            (required bool))

    [ ((Some true, Some false), true), "300b 3006 8101ff 010100 0101ff";
      ((None, Some false), true), "3008 3003 010100 0101ff";
      ((Some true, None), true), "3008 3003 8101ff 0101ff" ;
      ((Some true, None), true), "3080 3080 8101ff 0000 0101ff 0000";
      ((None, None), true), "3007308000000101ff";
      ((None, None), true), "300530000101ff" ];

  case_eq "sequence_of choice"
    Asn.S.(sequence2
            (required @@
              sequence_of
                (choice2 bool (implicit 0 bool)))
            (required @@ bool))

    [ ([`C2 true; `C2 false; `C1 true], true),
      "300e 3009 8001ff 800100 0101ff 0101ff";
      ([`C2 true; `C2 false; `C1 true], true),
      "3080 3080 8001ff 800100 0101ff 0000 0101ff 0000" ];

  case_eq "sets"
    Asn.S.(set4 (required @@ implicit 1 bool)
                (required @@ implicit 2 bool)
                (required @@ implicit 3 int )
                (optional @@ implicit 4 int ))

    [ (true, false, 42, None), "3109 8101ff 820100 83012a";
      (true, false, 42, Some (-1)), "310c 820100 8401ff 8101ff 83012a";
      (true, false, 42, None), "3109 820100 83012a 8101ff";
      (true, false, 42, Some 15), "310c 83012a 820100 8101ff 84010f";
      (true, false, 42, None), "3180 820100 83012a 8101ff 0000";
      (true, false, 42, Some 15), "3180 83012a 820100 8101ff 84010f 0000" ];

  case_eq "set or seq"
    Asn.S.(choice2 (set2 (optional int ) (optional bool))
                   (sequence2 (optional int ) (optional bool)))

    [ (`C1 (None, Some true)), "3103 0101ff";
      (`C1 (Some 42, None)), "3103 02012a";
      (`C1 (Some 42, Some true)), "3106 0101ff 02012a";
      (`C2 (None, Some true)), "3003 0101ff";
      (`C2 (Some 42, None)), "3003 02012a";
      (`C2 (Some 42, Some true)), "3006 02012a 0101ff" ];

  case_eq
    "large tag"
    Asn.S.(implicit 6666666 bool)
    [ true , "9f8396f32a01ff";
      false, "9f8396f32a0100";
    ];


  case_eq
    "recursive encoding"
    Asn.S.(
      fix @@ fun list ->
        map (function `C1 () -> [] | `C2 (x, xs) -> x::xs)
            (function [] -> `C1 () | x::xs -> `C2 (x, xs)) @@
        choice2 null (sequence2 (required bool) (required list)))

    [ [], "0500" ;
      [true], "3005 0101ff 0500" ;
      [true; false; true],
      "300f 0101ff 300a 010100 3005 0101ff 0500";
      [false; true; false],
      "3080 010100 3080 0101ff 3080 010100 0500 0000 0000 0000";
      [false; true; false],
      "3080 010100 3080 0101ff 3080 010100 0500 0000 0000 0000" ];

  case_eq "ia5 string" Asn.S.ia5_string

    [ "abc", "1603616263";
      "abcd", "360a 160161 160162 16026364";
      "abcd", "3680 160161 160162 16026364 0000";
      "abcd", "3680 3606 160161 160162 16026364 0000";
      "test1@rsa.com", "160d7465737431407273 612e636f6d";
      "test1@rsa.com", "16810d 7465737431407273612e636f6d" ;
      "test1@rsa.com", "3613 16057465737431 160140 16077273612e636f6d" ];

  case_eq "bit string" Asn.S.bit_string

    ( let example =
        [| false; true; true; false; true; true; true; false; false;
          true; false; true; true; true; false; true; true; true |] in

      [ example, "0304066e5dc0";
        example, "0304066e5de0";
        example, "038104066e5dc0";
        example, "2309 0303006e5d 030206c0" ] );

  case_eq "bit flags"
    (Asn.S.bit_string_flags [(2, `A); (4, `C); (8, `B); (10, `E); (12, `D)])

    [ [`A; `B; `C], "030304ffdf";
      [`A; `B; `C; `D], "030303ffdf"; ];

  ( let open Asn.OID in

    let rsa = base 1 2 <| 840 <| 113549 in

    case_eq "oid" Asn.S.oid [

      ( rsa ), "06062a864886f70d";
      ( rsa <| 1 <| 7 <| 2 ), "06092a864886f70d010702";
      ( rsa <| 1 <| 7 <| 1 ), "06092a864886f70d010701";
      ( base 1 3 <| 14 <| 3 <| 2 <| 26 ), "06052b0e03021a";
      ( base 2 5 <| 4 <| 3 ), "0603550403";
      ( base 2 5 <| 29 <| 15 ), "0603551d0f";
      ( base 1 2 <| 99999 ), "06042a868d1f";
    ] );

  case_eq "octets" Asn.S.octet_string [
    x_cs "0123456789abcdef", "0408 0123456789abcdef" ;
    x_cs "0123456789abcdef", "048108 0123456789abcdef";
    x_cs "0123456789abcdef", "240c 040401234567 040489abcdef" ];

  case_eq "utc time" ~cmp:Ptime.equal Asn.S.utc_time [

    ( time ((1991, 5, 6), ((23, 45, 40), 0)),
      "170d3931303530363233343534305a " ) ;
    ( time ((1991, 5, 6), ((16, 45, 40), -7 * 3600)),
      "17113931303530363136343534302D30373030 " );
    ( time ((1991, 5, 6), ((16, 45, 0), 9000)),
      "170f393130353036313634352b30323330");

  ] ;

  case_eq "generalized time" ~cmp:Ptime.equal Asn.S.generalized_time [

    ( time ((1991, 5, 6), ((16, 0, 0), 0)),
      "180a313939313035303631 36");
    ( time ((1991, 5, 6), ((16, 0, 0), 0)),
      "180b313939313035303631 365a ");
    ( time ((1991, 5, 6), ((16, 0, 0), 15 * 60)),
      "180f313939313035303631 362b30303135");
    ( time ((1991, 5, 6), ((16, 45, 0), 15 * 60)),
      "1811313939313035303631 3634352b30303135");
    ( time ((1991, 5, 6), ((16, 45, 40), -15 * 60)),
      "1813313939313035303631 36343534302d30303135");
    ( time ~frac:001 ((1991, 5, 6), ((16, 45, 40), -(10 * 3600 + 10 * 60))),
      "1817313939313035303631 36343534302e3030312d31 303130");
    ( Ptime.min,
      "1817303030303031303130 30303030302e3030302b30 303030");
    ( Ptime.(truncate ~frac_s:3 max),
      "1817393939393132333132 33353935392e3939392b30 303030")
  ] ;

]

let anticases = [

  (* thx @alpha-60 *)
  case "tag overflow" Asn.S.bool
  [ "1f a080 8080 8080 8080 8001 01ff" ];

  case "leading zero" Asn.S.(implicit 127 bool)
  [ "9f807f01ff" ];

  case "length overflow" Asn.S.bool
  [ "01 88 8000000000000001 ff" ] ;

  case "oid overflow" Asn.S.oid
  [ "06 0b 2a bfffffffffffffffff7f" ] ;

  case "empty integer" Asn.S.integer [ "0200" ];

  case "redundant int form" Asn.S.integer [
    "02020000"; "0202007f"; "0202ff80"; "0202ffff";
    "0203000000"; "0203007fff"; "0203ff8000"; "0203ffffff";
  ];

  case "redundant oid form" Asn.S.oid
  [ "06028001"; "06032a8001" ];

  case "length overflow" Asn.S.integer
  [ "02890100000000000000012a" ];

  case "silly bit strings" Asn.S.bit_string
  [ "0300"; "030101"; "030208ff" ];

  case "null with indefinite length" Asn.S.null
  [ "0580"; "058000"; "05800000" ]
]

let der_anticases = [
  case "constructed string 1" Asn.S.octet_string
  [ "2400"; "24 06 04 04 46 55 43 4b" ];

  case "constructed string 2" Asn.S.utf8_string
  [ "2c00"; "2c060c044655434b" ];

  case "expanded length" Asn.S.integer
  [ "0281012a" ];

  case "redundant length" Asn.S.octet_string
  [ "048200ff" ^
    Fmt.strf "%a" pp_hex_s (String.init 0xff (fun _ -> '\xaa')) ];
]

let certs = List.map (fun s -> case "cert" X509.certificate [s]) X509.examples

let () = Alcotest.run ~and_exit:false "BER" [
  accepts_eq "value samples" Asn.ber cases;
  rejects "- BER antisamples" Asn.ber anticases;
  accepts "+ DER antisamples" Asn.ber der_anticases;
  accepts "certs" Asn.ber certs;
  inverts1 "inv" Asn.ber cases;
  (* invert certs *)
]

let () = Alcotest.run "DER" [
  (* accepts_eq "value samples" Asn.der cases; *)
  rejects "- BER antisamples" Asn.der anticases;
  rejects "- DER antisamples" Asn.der der_anticases;
  accepts "certs" Asn.der certs;
  inverts1 "inv" Asn.der cases;
  (* invert certs *)
  (* injectivity *)
]
