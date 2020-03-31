/*
 * Copyright (c) 2005-2006 Tim Deegan <tjd@phlegethon.org>
 * Copyright (c) 2017, 2018 Hannes Mehnert <hannes@mehnert.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  dnsparser.mly -- ocamlyacc parser for DNS "Master zone file" format
 */

%{

open Dns_zone_state
open Dns

let parse_error s = raise (Zone_parse_problem s)

(* Parsers for numbers *)
let parse_uint8 s =
  try let d = int_of_string s in
  if d < 0 || d > 255 then raise Parsing.Parse_error;
  d
  with Failure _ -> raise Parsing.Parse_error

let parse_uint16 s =
  try
    let n = int_of_string s in
    if n > 65535 then raise Parsing.Parse_error;
    n
  with Failure _ -> raise Parsing.Parse_error

let parse_uint32 s =
  try
    let n = Int64.of_string s in
    if n >= 4294967296L then raise Parsing.Parse_error;
    Int64.to_int32 n
  with Failure _ -> raise Parsing.Parse_error

(* Parse an IPv6 address.  (RFC 3513 section 2.2) *)
let parse_ipv6 s =
  Ipaddr.V6.of_string_exn s

let add_to_map name (Rr_map.B (k, v)) = Name_rr_map.add name k v
%}

%token EOF
%token EOL
%token SORIGIN
%token STTL
%token AT
%token DOT
%token SPACE
%token GENERIC
%token <string> NUMBER
%token <string> CHARSTRING

%token <string> TYPE_A
%token <string> TYPE_NS
%token <string> TYPE_CNAME
%token <string> TYPE_SOA
%token <string> TYPE_PTR
%token <string> TYPE_MX
%token <string> TYPE_TXT
%token <string> TYPE_AAAA
%token <string> TYPE_SRV
%token <string> TYPE_CAA
%token <string> TYPE_DNSKEY
%token <string> TYPE_TLSA
%token <string> TYPE_SSHFP
%token <string> TYPE_GENERIC

%token <string> CLASS_IN
%token <string> CLASS_CS
%token <string> CLASS_CH
%token <string> CLASS_HS

%start zfile
%type <Dns.Name_rr_map.t> zfile

%%

zfile: lines EOF { state.zone }

lines:
   /* nothing */ { }
 | lines EOL { }
 | lines origin EOL { }
 | lines ttl EOL { }
 | lines rrline EOL { }

s: SPACE {} | s SPACE {}

origin: SORIGIN s domain { state.origin <- $3 }

ttl: STTL s int32 { state.ttl <- $3 }

rrline:
   owner s int32 s rrclass s rr { state.zone <- add_to_map $1 (Rr_map.with_ttl $7 $3) state.zone }
 | owner s rrclass s int32 s rr { state.zone <- add_to_map $1 (Rr_map.with_ttl $7 $5) state.zone  }
 | owner s rrclass s rr { state.zone <- add_to_map $1 (Rr_map.with_ttl $5 state.ttl) state.zone }
 | owner s int32 s rr { state.zone <- add_to_map $1 (Rr_map.with_ttl $5 $3) state.zone }
 | owner s rr { state.zone <- add_to_map $1 (Rr_map.with_ttl $3 state.ttl) state.zone }

rrclass:
   CLASS_IN {}
 | CLASS_CS { parse_error "class must be \"IN\"" }
 | CLASS_CH { parse_error "class must be \"IN\"" }
 | CLASS_HS { parse_error "class must be \"IN\"" }

rr:
generic_type s generic_rdata {
  match Rr_map.I.of_int $1 with
  | Ok i -> B (Unknown i, (0l, Rr_map.Txt_set.singleton $3))
  | Error _ -> parse_error "type code reserved, not generic"
}
     /* RFC 1035 */
 | TYPE_A s ipv4 { B (A, (0l, Rr_map.Ipv4_set.singleton $3)) }
 | TYPE_NS s hostname { B (Ns, (0l, Domain_name.Host_set.singleton $3)) }
 | TYPE_CNAME s domain { B (Cname, (0l, $3)) }
 | TYPE_SOA s domain s domain s int32 s int32 s int32 s int32 s int32
     { B (Soa, { Soa.nameserver = $3 ; hostmaster = $5 ; serial = $7 ;
                 refresh = $9 ; retry = $11 ; expiry = $13 ; minimum = $15 }) }
 | TYPE_PTR s hostname { B (Ptr, (0l, $3)) }
 | TYPE_MX s int16 s hostname { B (Mx, (0l, Rr_map.Mx_set.singleton { Mx.preference = $3 ; mail_exchange = $5 })) }
 | TYPE_TXT s charstrings { B (Txt, (0l, Rr_map.Txt_set.of_list $3)) }
     /* RFC 2782 */
 | TYPE_SRV s int16 s int16 s int16 s hostname
     { B (Srv, (0l, Rr_map.Srv_set.singleton { Srv.priority = $3 ; weight = $5 ; port = $7 ; target = $9 })) }
     /* RFC 3596 */
 | TYPE_TLSA s int8 s int8 s int8 s hex
     { match
         Tlsa.int_to_cert_usage $3,
         Tlsa.int_to_selector $5,
         Tlsa.int_to_matching_type $7
       with
       | Ok cert_usage, Ok selector, Ok matching_type ->
          let tlsa = { Tlsa.cert_usage ; selector ; matching_type ; data = $9 } in
          B (Tlsa, (0l, Rr_map.Tlsa_set.singleton tlsa ))
       | _ -> raise Parsing.Parse_error
     }
 | TYPE_SSHFP s int8 s int8 s hex
     { match
         Sshfp.int_to_algorithm $3,
         Sshfp.int_to_typ $5
       with
       | Ok algorithm, Ok typ ->
          let sshfp = { Sshfp.algorithm ; typ ; fingerprint = $7 } in
          B (Sshfp, (0l, Rr_map.Sshfp_set.singleton sshfp))
       | _ -> raise Parsing.Parse_error
     }
 | TYPE_AAAA s ipv6 { B (Aaaa, (0l, Rr_map.Ipv6_set.singleton $3)) }
 | TYPE_DNSKEY s int16 s int16 s int16 s charstring
     { if not ($5 = 3) then
           parse_error ("DNSKEY protocol is not 3, but " ^ string_of_int $5) ;
       match Dnskey.int_to_algorithm $7 with
       | Error _ -> parse_error ("DNSKEY algorithm not supported " ^ string_of_int $7)
       | Ok x ->
          let dnskey = { Dnskey.flags = $3 ; algorithm = x ; key = Cstruct.of_string $9 } in
          B (Dnskey, (0l, Rr_map.Dnskey_set.singleton dnskey))
     }
 | TYPE_CAA s int16 s charstring s charstrings
     { let critical = if $3 = 0x80 then true else false in
       let caa = { Caa.critical ; tag = $5 ; value = $7 } in
       B (Caa, (0l, Rr_map.Caa_set.singleton caa)) }
 | CHARSTRING s { parse_error ("TYPE " ^ $1 ^ " not supported") }

single_hex: charstring
  { Cstruct.of_hex $1 }

hex:
   single_hex { $1 }
 | hex s single_hex { Cstruct.append $1 $3 }

generic_type: TYPE_GENERIC
     { try parse_uint16 (String.sub $1 4 (String.length $1 - 4))
       with Parsing.Parse_error -> parse_error ($1 ^ " is not a 16-bit number")
     }

generic_rdata: GENERIC s NUMBER s hex
     { try
         let len = int_of_string $3
         and data = Cstruct.to_string $5
         in
         if not (String.length data = len) then
           parse_error ("generic data length field is "
			   ^ $3 ^ " but actual length is "
			      ^ string_of_int (String.length data));
	 data
       with Failure _ ->
	 parse_error ("\\# should be followed by a number")
     }

ipv4: NUMBER DOT NUMBER DOT NUMBER DOT NUMBER
     { try
	 let a = parse_uint8 $1 in
	 let b = parse_uint8 $3 in
	 let c = parse_uint8 $5 in
	 let d = parse_uint8 $7 in
         Ipaddr.V4.make a b c d
       with Failure _ | Parsing.Parse_error ->
	 parse_error ("invalid IPv4 address " ^ $1 ^ "." ^ $3 ^ "." ^ $5 ^ "." ^ $7)
     }

ipv6: charstring
     { try parse_ipv6 $1 with
       | Failure _ | Parsing.Parse_error ->
	  parse_error ("invalid IPv6 address " ^ $1)
     }

int8: NUMBER
     { try parse_uint8 $1
       with Parsing.Parse_error ->
	 parse_error ($1 ^ " is not a 8-bit number") }

int16: NUMBER
     { try parse_uint16 $1
       with Parsing.Parse_error ->
	 parse_error ($1 ^ " is not a 16-bit number") }

int32: NUMBER
     { try parse_uint32 $1
       with Failure _ ->
	 parse_error ($1 ^ " is not a 32-bit number") }

/* The owner of an RR is more restricted than a general domain name: it
   can't be a pure number or a type or class.  If we see one of those we
   assume the owner field was omitted */
owner:
   /* nothing */ { state.owner }
 | domain { state.owner <- $1 ; state.owner }

domain:
   DOT { Domain_name.root }
 | AT { state.origin }
 | label_except_at { Domain_name.prepend_label_exn state.origin $1 }
 | label DOT { Domain_name.of_strings_exn [$1] }
 | label DOT domain_labels { Domain_name.of_strings_exn ($1 :: $3 @ (Domain_name.to_strings state.origin)) }
 | label DOT domain_labels DOT { Domain_name.of_strings_exn ($1 :: $3) }

domain_labels:
   label { [$1] }
 | domain_labels DOT label { $1 @ [$3] }

hostname: domain { Domain_name.host_exn $1 }

/* It's acceptable to re-use numbers and keywords as character-strings.
   This is pretty ugly: we need special cases to distinguish a domain
   that's made up of just an '@'. */

charstrings: charstring { [$1] } | charstrings s charstring { $1 @ [$3] }

charstring: CHARSTRING { $1 } | keyword_or_number { $1 } | AT { "@" }

label_except_specials: CHARSTRING
    { if String.length $1 > 63 then
        parse_error "label is longer than 63 bytes";
      $1 }

label_except_at: label_except_specials { $1 } | keyword_or_number { $1 }

label: label_except_at { $1 } | AT { "@" }

keyword_or_number:
   NUMBER { $1 }
 | TYPE_A { $1 }
 | TYPE_NS { $1 }
 | TYPE_CNAME { $1 }
 | TYPE_SOA { $1 }
 | TYPE_PTR { $1 }
 | TYPE_MX { $1 }
 | TYPE_TXT { $1 }
 | TYPE_AAAA { $1 }
 | TYPE_SRV { $1 }
 | TYPE_DNSKEY { $1 }
 | TYPE_TLSA { $1 }
 | TYPE_SSHFP { $1 }
 | CLASS_IN { $1 }
 | CLASS_CS { $1 }
 | CLASS_CH { $1 }
 | CLASS_HS { $1 }

%%
