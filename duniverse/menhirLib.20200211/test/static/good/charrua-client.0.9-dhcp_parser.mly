(* Original file: charrua-client.0.9/charrua-core-0.9/lib/dhcp_parser.mly *)
(*
 * Copyright (c) 2015 Christiano F. Haesbaert <haesbaert@haesbaert.org>
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
 *)

%{
  type statement =
    | Range of Ipaddr.V4.t * Ipaddr.V4.t
    | Dhcp_option of Dhcp_wire.dhcp_option
    | Hw_eth of Macaddr.t
    | Fixed_addr of Ipaddr.V4.t
    | Default_lease_time of int32
    | Max_lease_time of int32

  let choke s = invalid_arg s

%}

%token <Ipaddr.V4.t> IP
%token <Macaddr.t> MACADDR
%token <string> STRING
%token COMMA
%token DEFAULTLEASETIME
%token DOMAINNAME
%token DOMAINNAMESERVERS
%token EOF
%token ETHERNET
%token FIXEDADDRESS
%token HARDWARE
%token HOST
%token <int> INTEGER
%token LBRACKET
%token MAXLEASETIME
%token NETMASK
%token OPTION
%token RANGE
%token RBRACKET
%token ROUTERS
%token SCOLON
%token SUBNET
%token <string> WORD

%start <Ast.t> main
%%

main:
  | s = statement; ss = statements; sub = subnet; subs = subnets; EOF {
  let statements = s :: ss in
  let subnets = sub :: subs in
  (* Now extract the options from the statements *)
  let () = List.iter (function
      | Dhcp_option o -> ()
      | Default_lease_time t -> ()
      | Max_lease_time t -> ()
      | _ -> choke "Only dhcp options and default|max-lease-time \
                    are allowed in the global section")
      statements
  in
  let options = Util.filter_map (function
      | Dhcp_option o -> Some o
      | _ -> None)
      statements
  in
  let default_lease_time =
    match (Util.find_map (function
        | Default_lease_time t -> Some t
        | _ -> None)
        statements)
    with | Some time -> time
         | None -> Int32.of_int (60 * 60 * 60) (* 1h *)
  in
  let max_lease_time =
    match (Util.find_map (function
        | Max_lease_time t -> Some t
        | _ -> None)
        statements)
    with | Some time -> time
         | None -> Int32.of_int (60 * 60 * 60 * 24) (* 24h *)
  in
  { Ast.subnets; options; default_lease_time; max_lease_time }
}

ips:
  | ip = IP { [ip] }
  | ip = IP; COMMA; ips = ips { ip :: ips }

statements:
  | (* empty *) { [] }
  | s = statement; ss = statements { s :: (List.rev ss) }

statement:
  | OPTION; DOMAINNAME; v = STRING; SCOLON { Dhcp_option (Dhcp_wire.Domain_name v)}
  | OPTION; DOMAINNAMESERVERS; ips = ips; SCOLON { Dhcp_option (Dhcp_wire.Dns_servers ips) }
  | OPTION; ROUTERS; ips = ips; SCOLON { Dhcp_option (Dhcp_wire.Routers ips) }
  | RANGE; v1 = IP; v2 = IP; SCOLON {
  if Int32.compare (Ipaddr.V4.to_int32 v1) (Ipaddr.V4.to_int32 v2) >= 0 then
    choke "Invalid `range` statement, must be `low high`";
  Range (v1, v2)
  }
  | HARDWARE; ETHERNET; mac = MACADDR; SCOLON { Hw_eth mac }
  | FIXEDADDRESS; v = IP; SCOLON { Fixed_addr v }
  | DEFAULTLEASETIME; v = INTEGER; SCOLON { Default_lease_time (Int32.of_int v) }
  | MAXLEASETIME; v = INTEGER; SCOLON { Max_lease_time (Int32.of_int v) }

subnets:
  | (* empty *) { [] }
  | sub = subnet; subs = subnets { sub :: (List.rev subs) }

subnet:
  | SUBNET; ip = IP; NETMASK; mask = IP; LBRACKET;
  statements = statements; hosts = hosts; RBRACKET {
  let network = Ipaddr.V4.Prefix.of_netmask mask ip in
  (* Catch statements that don't make sense in a subnet *)
  let () = List.iter (function
      | Hw_eth _ | Fixed_addr _ ->
        choke "`hardware` and `fixed-address` belong to `host` context, not subnet"
      | _ -> ())
      statements
  in
  (* First find the range statement, XXX ignoring if multiple *)
  let range = Util.find_map (function
      | Range (v1, v2) -> Some (v1, v2)
      | _ -> None)
      statements |> (function
      | Some (v1, v2) -> (v1, v2)
      | None -> choke ("Missing `range` statement for subnet " ^
                       (Ipaddr.V4.to_string ip)))
  in
  let options = Util.filter_map (function
      | Dhcp_option o -> Some o
      | _ -> None)
      statements
  in
  let default_lease_time =
    (Util.find_map (function Default_lease_time t -> Some t | _ -> None)
        statements)
  in
  let max_lease_time =
    (Util.find_map (function Max_lease_time t -> Some t | _ -> None)
        statements)
  in
  { Ast.network; range = Some range; options; hosts; default_lease_time; max_lease_time }
}

hosts:
  | (* empty *) { [] }
  | host = host; hosts = hosts { host :: hosts }

host:
  | HOST; hostname = WORD; LBRACKET; statements = statements; RBRACKET {
  let () = List.iter (function
      | Range _ -> choke "Range is invalid in host context"
      | _ -> ())
      statements
  in
  let options = Util.filter_map (function
      | Dhcp_option o -> Some o
      | _ -> None)
      statements
  in
  let fixed_addr = Util.find_map (function
      | Fixed_addr fa -> Some fa
      | _ -> None)
      statements
  in
  let hw_addr = Util.find_map (function
      | Hw_eth he -> Some he
      | _ -> None)
      statements
  in
  let hw_addr =
    Util.some_or_f hw_addr
                   (fun () -> choke "Missing hardware ethernet statement." )
  in
  { Ast.hostname; options; fixed_addr; hw_addr }
}
