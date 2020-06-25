(*
 * Copyright (c) 2013-2014 David Sheets <sheets@alum.mit.edu>
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
 *)

open OUnit
open Ipaddr

let error s msg = s, Parse_error (msg,s)
let need_more s = error s "not enough data"
let bad_char i s =
  error s (Printf.sprintf "invalid character '%c' at %d" s.[i] i)

let (>>=) v f = match v with Ok v -> f v | Error _ as e -> e

let assert_raises ~msg exn test_fn =
  assert_raises ~msg exn (fun () ->
    try test_fn ()
    with rtexn -> begin
      (if exn <> rtexn then (
        Printf.eprintf "Stacktrace for '%s':\n%!" msg;
        Printexc.print_backtrace stderr;
       ));
      raise rtexn
    end)

module Test_v4 = struct
  let test_string_rt () =
    let addrs = [
      "192.168.0.1", "192.168.0.1";
    ] in
    List.iter (fun (addr,rt) ->
      let os = V4.of_string_exn addr in
      let ts = V4.to_string os in
      assert_equal ~msg:addr ts rt;
      let os = Ipaddr_sexp.(V4.t_of_sexp (V4.sexp_of_t os)) in
      let ts = V4.to_string os in
      assert_equal ~msg:addr ts rt;
    ) addrs

  let test_string_rt_bad () =
    let addrs = [
      need_more "192.168.0";
      bad_char 11 "192.168.0.1.1";
      error "192.268.2.1" "second octet out of bounds";
      bad_char 4 "192. 168.1.1";
      bad_char 4 "192..0.1";
      bad_char 3 "192,168.0.1";
    ] in
    List.iter (fun (addr,exn) ->
      assert_raises ~msg:addr exn (fun () -> V4.of_string_exn addr)
    ) addrs

  let test_string_raw_rt () =
    let addrs = [
      ("IP: 192.168.0.1!!!",4),   ("192.168.0.1",15);
      ("IP: 192.168.0.1.1!!!",4), ("192.168.0.1",15);
    ] in
    List.iter (fun ((addr,off),result) ->
      let c = ref off in
      let os = V4.of_string_raw addr c in
      let ts = V4.to_string os in
      assert_equal ~msg:addr (ts,!c) result
    ) addrs

  let test_string_raw_rt_bad () =
    let addrs = [
      (let s = "IP: 192.168.0!!!" in
       (s,4), (Parse_error ("invalid character '!' at 13",s), 13));
    ] in
    List.iter (fun ((addr,off),(exn,cursor)) ->
      let c = ref off in
      assert_raises ~msg:addr exn (fun () -> V4.of_string_raw addr c);
      assert_equal ~msg:(Printf.sprintf "%s cursor <> %d (%d)" addr cursor !c)
        !c cursor
    ) addrs

  let test_bytes_rt () =
    let addr = "\254\099\003\128" in
    assert_equal ~msg:(String.escaped addr)
      V4.(to_octets (of_octets_exn addr)) addr

  let test_bytes_rt_bad () =
    let addrs = [
      need_more "\254\099\003";
    ] in
    List.iter (fun (addr,exn) ->
      assert_raises ~msg:(String.escaped addr) exn
        (fun () -> V4.of_octets_exn addr)
    ) addrs

  let test_int32_rt () =
    let addr = 0x0_F0_AB_00_01_l in
    assert_equal ~msg:(Printf.sprintf "%08lX" addr)
      V4.(to_int32 (of_int32 addr)) addr

  let test_prefix_string_rt () =
    let subnets = [
      "192.168.0.0/24", "192.168.0.0/24";
      "0.0.0.0/0",      "0.0.0.0/0";
      "192.168.0.1/24", "192.168.0.0/24";
      "192.168.0.0/0",  "0.0.0.0/0";
    ] in
    List.iter (fun (subnet,rt) ->
      let os = V4.Prefix.of_string_exn subnet |> V4.Prefix.prefix in
      let ts = V4.Prefix.to_string os in
      assert_equal ~msg:subnet ts rt;
      let os = Ipaddr_sexp.(V4.Prefix.(t_of_sexp (sexp_of_t os))) in
      let ts = V4.Prefix.to_string os in
      assert_equal ~msg:subnet ts rt;
    ) subnets

  let test_prefix_string_rt_bad () =
    let subnets = [
      bad_char 9 "192.168.0/24";
      bad_char 10 "192.168.0./24";
      error "192.168.0.0/33" "invalid prefix size";
      bad_char 14 "192.168.0.0/30/1";
      bad_char 12 "192.168.0.0/-1";
    ] in
    List.iter (fun (subnet,exn) ->
      assert_raises ~msg:subnet exn (fun () -> V4.Prefix.of_string_exn subnet)
    ) subnets

  let test_network_address_rt () =
    let netaddrs = [
      "192.168.0.1/24", "192.168.0.0/24", "192.168.0.1";
    ] in
    List.iter (fun (netaddr,net,addr) ->
      let netv4 = V4.Prefix.of_string_exn net in
      let addrv4 = V4.of_string_exn addr in
      let cidr = V4.Prefix.of_string_exn netaddr in
      let prefix = V4.Prefix.prefix cidr
      and v4 = V4.Prefix.address cidr
      in
      assert_equal ~msg:(net^" <> "^(V4.Prefix.to_string prefix)) netv4 prefix;
      assert_equal ~msg:(addr^" <> "^(V4.to_string v4)) addrv4 v4;
      let addrstr = V4.Prefix.to_string cidr in
      assert_equal ~msg:(netaddr^" <> "^addrstr) netaddr addrstr;
    ) netaddrs

  let test_prefix_broadcast () =
    let pairs = [
      "192.168.0.0/16",   "192.168.255.255";
      "192.168.0.0/24",   "192.168.0.255";
      "192.168.1.1/24",   "192.168.1.255";
      "192.168.0.128/29", "192.168.0.135";
      "0.0.0.0/0",        "255.255.255.255";
    ] in
    List.iter (fun (subnet,bcast) ->
      let r = V4.(to_string (Prefix.(broadcast (of_string_exn subnet)))) in
      assert_equal ~msg:(subnet ^ " <> " ^ r) r bcast
    ) pairs

  let test_prefix_bits () =
    let pairs = V4.Prefix.([
      global, 0;
      loopback, 8;
      link, 16;
      relative, 8;
      multicast, 4;
      private_10, 8;
      private_172, 12;
      private_192, 16;
    ]) in
    List.iter (fun (subnet,bits) ->
      let msg = (V4.Prefix.to_string subnet) ^ " <> " ^ (string_of_int bits) in
      assert_equal ~msg (V4.Prefix.bits subnet) bits
    ) pairs

  let test_prefix_netmask () =
    let nets = [
      "192.168.0.1/32","255.255.255.255";
      "192.168.0.1/31","255.255.255.254";
      "192.168.0.1/1", "128.0.0.0";
      "192.168.0.1/0", "0.0.0.0";
    ] in
    List.iter (fun (net_str,nm_str) ->
      let cidr = V4.Prefix.of_string_exn net_str in
      let prefix = V4.Prefix.prefix cidr
      and address = V4.Prefix.address cidr
      in
      let netmask = V4.Prefix.netmask prefix in
      let nnm_str = V4.to_string netmask in
      let msg = Printf.sprintf "netmask %s <> %s" nnm_str nm_str in
      assert_equal ~msg nnm_str nm_str;
      let prefix = V4.Prefix.of_netmask_exn ~netmask ~address in
      let nns = V4.Prefix.to_string prefix in
      let msg = Printf.sprintf "%s is %s under netmask iso" net_str nns in
      assert_equal ~msg net_str nns
    ) nets

  let test_prefix_netmask_bad () =
    let bad_masks = [
      error "127.255.255.255" "invalid netmask";
      error "255.255.254.128" "invalid netmask";
    ] in
    List.iter (fun (nm_str,exn) ->
      let netmask = V4.of_string_exn nm_str in
      let address = V4.of_string_exn "192.168.0.1" in
      assert_raises ~msg:nm_str exn
        (fun () -> V4.Prefix.of_netmask_exn ~netmask ~address)
    ) bad_masks

  let test_scope () =
    let ip = V4.of_string_exn in
    (*let is subnet addr = V4.Prefix.(mem addr subnet) in*)
    let is_scope scop addr = scop = V4.scope addr in
    let ships = V4.([
      unspecified,         "global",    is_global,          false;
      unspecified,         "multicast", is_multicast,       false;
      unspecified,         "point",     is_scope Point,     true;
      localhost,           "global",    is_global,          false;
      localhost,           "multicast", is_multicast,       false;
      localhost,           "interface", is_scope Interface, true;
      broadcast,           "global",    is_global,          false;
      broadcast,           "multicast", is_multicast,       false;
      broadcast,           "admin",     is_scope Admin,     true;
      nodes,               "global",    is_global,          false;
      nodes,               "multicast", is_multicast,       true;
      nodes,               "interface", is_scope Link,      true;
      routers,             "global",    is_global,          false;
      routers,             "multicast", is_multicast,       true;
      routers,             "link",      is_scope Link,      true;
      ip "192.168.0.1",    "private",   is_private,         true;
      ip "10.3.21.155",    "private",   is_private,         true;
      ip "172.16.0.0",     "private",   is_private,         true;
      ip "172.31.255.255", "private",   is_private,         true;
      ip "172.15.255.255", "private",   is_private,         false;
      ip "172.32.0.0",     "private",   is_private,         false;
    ]) in
    List.iter (fun (addr,lbl,pred,is_mem) ->
      let mems = if is_mem then "" else " not" in
      let msg = (V4.to_string addr)^" is"^mems^" in "^lbl in
      assert_equal ~msg (pred addr) is_mem
    ) ships

  let test_map () =
    let module M = Map.Make(V4) in
    let m = M.add (V4.of_string_exn "1.0.0.1") "min" M.empty in
    let m = M.add (V4.of_string_exn "254.254.254.254") "the greatest host" m in
    let m = M.add (V4.of_string_exn "1.0.0.1") "the least host" m in
    assert_equal ~msg:"size" (M.cardinal m) 2;
    let (min_key, min_val) = M.min_binding m in
    assert_equal ~msg:("min is '" ^ min_val ^"'") (min_key, min_val)
      (V4.of_string_exn "1.0.0.1", "the least host");
    assert_equal ~msg:"max" (M.max_binding m)
      (V4.of_string_exn "254.254.254.254", "the greatest host")

  let test_prefix_map () =
    let module M = Map.Make(V4.Prefix) in
    let of_string s = s |> V4.Prefix.of_string_exn |> V4.Prefix.prefix in
    let m = M.add (of_string "0.0.0.0/0") "everyone" M.empty in
    let m = M.add (of_string "192.0.0.0/1") "weirdos" m in
    let m = M.add (of_string "128.0.0.0/1") "high-bitters" m in
    let m = M.add (of_string "254.0.0.0/8") "top-end" m in
    let m = M.add (of_string "0.0.0.0/0") "iana" m in
    assert_equal ~msg:"size" (M.cardinal m) 3;
    assert_equal ~msg:"min" (M.min_binding m)
      (V4.Prefix.of_string_exn "0.0.0.0/0", "iana");
    assert_equal ~msg:"max" (M.max_binding m)
      (V4.Prefix.of_string_exn "254.0.0.0/8", "top-end");
    assert_equal ~msg:"third"
      (M.find (V4.Prefix.of_string_exn "128.0.0.0/1") m) "high-bitters"

  let test_special_addr () =
    assert_equal ~msg:"broadcast" V4.broadcast V4.Prefix.(broadcast global);
    assert_equal ~msg:"any"       V4.any       V4.Prefix.(network global);
    assert_equal ~msg:"localhost" true V4.(Prefix.(mem localhost loopback))

  let test_multicast_mac () =
    let ip = V4.of_octets_exn "\xff\xbf\x9f\x8f" in
    let multicast = V4.Prefix.(network_address multicast ip) in
    let unicast_mac_str   = Macaddr.to_string (V4.multicast_to_mac ip) in
    let multicast_mac_str = Macaddr.to_string (V4.multicast_to_mac multicast) in
    let mac_str = "01:00:5e:3f:9f:8f" in
    assert_equal ~msg:("unicast_mac "^unicast_mac_str^" <> "^mac_str)
      unicast_mac_str   mac_str;
    assert_equal ~msg:("multicast_mac "^multicast_mac_str^" <> "^mac_str)
      multicast_mac_str mac_str

  let test_domain_name () =
    let ip = V4.of_string_exn "128.64.32.16" in
    let name =
      Domain_name.(host_exn (of_string_exn "16.32.64.128.in-addr.arpa"))
    in
    assert_equal ~cmp:Domain_name.equal ~msg:"to_domain_name"
      (V4.to_domain_name ip) name ;
    assert_equal ~msg:"of_domain_name" (V4.of_domain_name name) (Some ip)

  let test_cstruct_rt () =
    let addr = "\254\099\003\128" in
    assert_equal ~msg:(String.escaped addr)
      (Cstruct.to_string Ipaddr_cstruct.V4.(to_cstruct (of_cstruct_exn (Cstruct.of_string addr)))) addr

  let test_cstruct_rt_bad () =
    let addrs = [
      need_more "\254\099\003";
    ] in
    List.iter (fun (addr,exn) ->
      assert_raises ~msg:(String.escaped addr) exn
        (fun () -> Ipaddr_cstruct.V4.of_cstruct_exn (Cstruct.of_string addr))
    ) addrs

  let test_prefix_mem () =
    let ip = V4.of_string_exn in
    let prefix = V4.Prefix.of_string_exn in
    let ships = [
      ip "10.0.0.7",            prefix "10.0.0.0/29",            true;
      ip "172.16.255.254",      prefix "172.16.255.254/31",      true;
      ip "192.168.0.1",         prefix "0.0.0.0/0",              true;
      ip "192.168.0.1",         V4.Prefix.private_192,           true;
      ip "255.255.255.255",     prefix "255.255.255.255/32",     true;
      ip "192.0.2.1",           prefix "192.0.2.0/32",           false;
      ip "192.0.2.1",           prefix "192.0.0.0/23",           false;
      ip "255.255.255.255",     prefix "0.0.0.0/1",              false;
    ] in
    List.iter (fun (addr,subnet,is_mem) ->
      let msg = Printf.sprintf "%s is%s in %s"
        (V4.to_string addr) (if is_mem then "" else " not") (V4.Prefix.to_string subnet)
      in
      assert_equal ~msg (V4.Prefix.mem addr subnet) is_mem
    ) ships

  let test_succ_pred () =
    let open V4 in
    let printer = function
      | Ok v -> Printf.sprintf "Ok %s" (to_string v)
      | Error (`Msg e) -> Printf.sprintf "Error `Msg \"%s\"" e
    in
    let assert_equal = assert_equal ~printer in
    let ip1 = of_string_exn "0.0.0.0" in
    let ip2 = of_string_exn "255.255.255.255" in
    assert_equal ~msg:"succ 0.0.0.0"
      (of_string "0.0.0.1") (succ ip1);
    assert_equal ~msg:"succ 255.255.255.255"
      (Error (`Msg "Ipaddr: highest address has been reached")) (succ ip2);
    assert_equal ~msg:"succ (succ 255.255.255.255)"
      (Error (`Msg "Ipaddr: highest address has been reached"))
      (succ ip2 >>= succ);
    assert_equal ~msg:"pred 0.0.0.0"
      (Error (`Msg "Ipaddr: lowest address has been reached")) (pred ip1);
    ()

  let test_prefix_first_last () =
    let open V4.Prefix in
    let assert_equal = assert_equal ~printer:V4.to_string in
    assert_equal ~msg:"first 192.168.1.0/24"
      (V4.of_string_exn "192.168.1.1")
      (first (of_string_exn "192.168.1.0/24"));
    assert_equal ~msg:"first 169.254.169.254/31"
      (Ipaddr.V4.of_string_exn "169.254.169.254")
      (first (of_string_exn "169.254.169.254/31"));
    assert_equal ~msg:"first 169.254.169.254/32"
      (Ipaddr.V4.of_string_exn "169.254.169.254")
      (first (of_string_exn "169.254.169.254/32"));
    assert_equal ~msg:"last 192.168.1.0/24"
      (Ipaddr.V4.of_string_exn "192.168.1.254")
      (last (of_string_exn "192.168.1.0/24"));
    assert_equal ~msg:"last 169.254.169.254/31"
      (Ipaddr.V4.of_string_exn "169.254.169.255")
      (last (of_string_exn "169.254.169.254/31"));
    assert_equal ~msg:"last 169.254.169.254/32"
      (Ipaddr.V4.of_string_exn "169.254.169.254")
      (last (of_string_exn "169.254.169.254/32"))

  let suite = "Test V4" >::: [
    "string_rt"            >:: test_string_rt;
    "string_rt_bad"        >:: test_string_rt_bad;
    "string_raw_rt"        >:: test_string_raw_rt;
    "string_raw_rt_bad"    >:: test_string_raw_rt_bad;
    "bytes_rt"             >:: test_bytes_rt;
    "bytes_rt_bad"         >:: test_bytes_rt_bad;
    "cstruct_rt"           >:: test_cstruct_rt;
    "cstruct_rt_bad"       >:: test_cstruct_rt_bad;
    "int32_rt"             >:: test_int32_rt;
    "prefix_string_rt"     >:: test_prefix_string_rt;
    "prefix_string_rt_bad" >:: test_prefix_string_rt_bad;
    "network_address_rt"   >:: test_network_address_rt;
    "prefix_broadcast"     >:: test_prefix_broadcast;
    "prefix_bits"          >:: test_prefix_bits;
    "prefix_netmask"       >:: test_prefix_netmask;
    "prefix_netmask_bad"   >:: test_prefix_netmask_bad;
    "scope"                >:: test_scope;
    "map"                  >:: test_map;
    "prefix_map"           >:: test_prefix_map;
    "special_addr"         >:: test_special_addr;
    "multicast_mac"        >:: test_multicast_mac;
    "domain_name"          >:: test_domain_name;
    "prefix_mem"           >:: test_prefix_mem;
    "succ_pred"            >:: test_succ_pred;
    "prefix_first_last"    >:: test_prefix_first_last;
  ]
end


module Test_v6 = struct
  let test_string_rt () =
    let addrs = [
      "2001:db8::ff00:42:8329","2001:db8::ff00:42:8329";
      "::ffff:192.168.1.1",    "::ffff:192.168.1.1";
      "::",                    "::";
      "[::]",                  "::";
      "1:1:1:1::1:1:1",        "1:1:1:1:0:1:1:1";
      "0:0:0:1:1:0:0:0",       "::1:1:0:0:0";
      "0:0:0:1:1::",           "::1:1:0:0:0";
      "::1:0:0:0:0",           "0:0:0:1::";
      "FE80::",                "fe80::";
      "::192.168.0.1",         "::c0a8:1";
    ] in
    List.iter (fun (addr,rt) ->
      let os = V6.of_string_exn addr in
      let ts = V6.to_string os in
      assert_equal ~msg:(addr^" <> "^rt^" ("^ts^")") ts rt;
      let os = Ipaddr_sexp.(V6.t_of_sexp (V6.sexp_of_t os)) in
      let ts = V6.to_string os in
      assert_equal ~msg:(addr^" <> "^rt^" ("^ts^")") ts rt;
    ) addrs

  let test_string_rt_bad () =
    let addrs = [
      need_more "[";
      need_more "[:";
      need_more "[]"; (* ? *)
      need_more ":";
      need_more "[::";
      bad_char 4 "::1:g:f";
      bad_char 3 "::1::";
      bad_char 4 "1::2::3";
      need_more "1:2:3:4:5:6:7";
      bad_char 15 "1:2:3:4:5:6:7:8:9";
      bad_char 15 "1:2:3:4:5:6:7:8::";
      error "12345::12:2" "component 0 out of bounds";
      bad_char 1 ":1";
    ] in
    List.iter (fun (addr,exn) ->
      assert_raises ~msg:addr exn (fun () -> V6.of_string_exn addr)
    ) addrs

  let test_string_raw_rt () =
    let addrs = [
      ("IP: 2001:db8::ff00:42:8329!",4), ("2001:db8::ff00:42:8329",26);
      ("IP: ::ffff:192.168.1.1 ",4),     ("::ffff:192.168.1.1",22);
      ("IP: :::",4),                     ("::",6);
      ("IP: [::]:",4),                   ("::",8);
      ("IP: 1:1:1:1::1:1:1:1",4),        ("1:1:1:1:0:1:1:1",18);
      ("IP: ::1:1:0:0:0::g",4),          ("::1:1:0:0:0",15);
    ] in
    List.iter (fun ((addr,off),(result,cursor)) ->
      let c = ref off in
      let os = V6.of_string_raw addr c in
      let ts = V6.to_string os in
      let msg = Printf.sprintf "%s at %d: %s at %d <> %s at %d"
        addr off result cursor ts !c
      in assert_equal ~msg (ts,!c) (result,cursor)
    ) addrs

  let test_string_raw_rt_bad () =
    let error (s,c) msg c' = (s,c), (Parse_error (msg,s),c') in
    let need_more loc = error loc "not enough data" in
    let bad_char i (s,c) =
      error (s,c) (Printf.sprintf "invalid character '%c' at %d" s.[i] i) i
    in
    let addrs = [
      need_more   ("IP: [] ",4) 5;
      bad_char 5  ("IP: : ",4);
      bad_char 7  ("IP: [:: ",4);
      bad_char 17 ("IP: 1:2:3:4:5:6:7 ",4);
      error       ("IP: 12345::12:2 ",4) "component 0 out of bounds" 15;
      bad_char 5  ("IP: :1 ",4);
      need_more   ("IP: ::1:1:0:0:0:",4) 16;
      bad_char 8  ("IP: ::1:g:f ",4);
    ] in
    List.iter (fun ((addr,off),(exn,cursor)) ->
      let c = ref off in
      assert_raises ~msg:addr exn (fun () -> V6.of_string_raw addr c);
      assert_equal ~msg:(Printf.sprintf "%s cursor <> %d (%d)" addr cursor !c)
        !c cursor
    ) addrs

  let test_bytes_rt () =
    let addr =
      "\000\000\000\000\000\000\000\000\000\000\255\255\192\168\000\001"
    in
    let v6 = V6.of_octets_exn addr in
    assert_equal ~msg:(String.escaped addr) V6.(to_octets v6) addr

  let test_bytes_rt_bad () =
    let addrs = [
      need_more "\000\000\000\000\000\000\000\000\000\000\255\255\192\168\001";
    ] in
    List.iter (fun (addr,exn) ->
      assert_raises ~msg:(String.escaped addr) exn
        (fun () -> V6.of_octets_exn addr)
    ) addrs

  let test_cstruct_rt () =
    let addr =
      "\000\000\000\000\000\000\000\000\000\000\255\255\192\168\000\001"
    in
    let v6 = Ipaddr_cstruct.V6.of_cstruct_exn (Cstruct.of_string addr) in
    assert_equal ~msg:(String.escaped addr) (Cstruct.to_string Ipaddr_cstruct.V6.(to_cstruct v6)) addr

  let test_cstruct_rt_bad () =
    let addrs = [
      need_more "\000\000\000\000\000\000\000\000\000\000\255\255\192\168\001";
    ] in
    List.iter (fun (addr,exn) ->
      assert_raises ~msg:(String.escaped addr) exn
        (fun () -> Ipaddr_cstruct.V6.of_cstruct_exn (Cstruct.of_string addr))
    ) addrs

  let test_int32_rt () =
    let (a,b,c,d) as addr =
      0x2001_0665_l, 0x0000_0000_l, 0xff00_00ff_l, 0xfe00_0001_l
    in
    assert_equal ~msg:(Printf.sprintf "%08lx %08lx %08lx %08lx" a b c d)
      V6.(to_int32 (of_int32 addr)) addr

  let test_prefix_string_rt () =
    let subnets = [
      "2000::/3",              "2000::/3";
      "c012::/2",              "c000::/2";
      "ffff:ffff:ffff::ffff/0",    "::/0";
      "::/0",                      "::/0";
      "::/128",                  "::/128";
      "::1/128",                "::1/128";
      "::/64",                    "::/64";
      "[::]/64",                  "::/64";
    ] in
    List.iter (fun (subnet,rt) ->
      let os = V6.Prefix.of_string_exn subnet |> V6.Prefix.prefix in
      let ts = V6.Prefix.to_string os in
      assert_equal ~msg:subnet ts rt;
      let os = Ipaddr_sexp.(V6.Prefix.(t_of_sexp (sexp_of_t os))) in
      let ts = V6.Prefix.to_string os in
      assert_equal ~msg:subnet ts rt;
    ) subnets

  let test_prefix_string_rt_bad () =
    let subnets = [
      need_more "/24";
      need_more "::";
      error "::/130" "invalid prefix size";
      bad_char 5 "::/30/1";
      bad_char 7 "2000::/-1";
      bad_char 5 "1::3:/4";
    ] in
    List.iter (fun (subnet,exn) ->
      assert_raises ~msg:subnet exn (fun () -> V6.Prefix.of_string_exn subnet)
    ) subnets

  let test_network_address_rt () =
    let netaddrs = [
      "::1/24", "::/24", "::1";
    ] in
    List.iter (fun (netaddr,net,addr) ->
      let netv4 = V6.Prefix.of_string_exn net in
      let addrv4 = V6.of_string_exn addr in
      let cidr = V6.Prefix.of_string_exn netaddr in
      let prefix = V6.Prefix.prefix cidr
      and v4 = V6.Prefix.address cidr
      in
      let prefix = V6.Prefix.prefix prefix in
      assert_equal ~msg:(net^" <> "^(V6.Prefix.to_string prefix)) netv4 prefix;
      assert_equal ~msg:(addr^" <> "^(V6.to_string v4)) addrv4 v4;
      let addrstr = V6.Prefix.to_string cidr in
      assert_equal ~msg:(netaddr^" <> "^addrstr) netaddr addrstr;
    ) netaddrs

  let test_prefix_bits () =
    let pairs = V6.Prefix.([
      global_unicast_001, 3;
      link,              64;
      unique_local,       7;
      multicast,          8;
      ipv4_mapped,       96;
      noneui64_interface, 3;
    ]) in
    List.iter (fun (subnet,bits) ->
      let msg = (V6.Prefix.to_string subnet) ^ " <> bits "
        ^ (string_of_int bits) in
      assert_equal ~msg (V6.Prefix.bits subnet) bits
    ) pairs

  let test_prefix_netmask () =
    let nets = [
      "8::1/128","ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff";
      "8::1/127","ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe";
      "8::1/96", "ffff:ffff:ffff:ffff:ffff:ffff::";
      "8::1/64", "ffff:ffff:ffff:ffff::";
      "8::1/32", "ffff:ffff::";
      "8::1/1",  "8000::";
      "8::1/0",  "::";
    ] in
    List.iter (fun (net_str,nm_str) ->
      let cidr = V6.Prefix.of_string_exn net_str in
      let prefix = V6.Prefix.prefix cidr
      and address = V6.Prefix.address cidr
      in
      let netmask = V6.Prefix.netmask prefix in
      let nnm_str = V6.to_string netmask in
      let msg = Printf.sprintf "netmask %s <> %s" nnm_str nm_str in
      assert_equal ~msg nnm_str nm_str;
      let prefix = V6.Prefix.of_netmask_exn ~netmask ~address in
      let nns = V6.Prefix.to_string prefix in
      let msg = Printf.sprintf "%s is %s under netmask iso" net_str nns in
      assert_equal ~msg net_str nns
    ) nets

  let test_prefix_netmask_bad () =
    let bad_masks = [
      error "7fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff" "invalid netmask";
      error "ffff:ffff:ffff:ffff:ffff:fffe:8000:0" "invalid netmask";
      error "ffff:ffff:ffff:fffe:8000::" "invalid netmask";
      error "ffff:fffe:8000::" "invalid netmask";
    ] in
    List.iter (fun (nm_str,exn) ->
      let netmask = V6.of_string_exn nm_str in
      let address = V6.of_string_exn "::" in
      assert_raises ~msg:nm_str exn
        (fun () -> V6.Prefix.of_netmask_exn ~netmask ~address)
    ) bad_masks

  let test_scope () =
    let localhost_v4 = V6.of_string_exn "::ffff:127.0.0.1" in
    let is subnet addr = V6.Prefix.(mem addr subnet) in
    let is_scope scop addr = scop = V6.scope addr in
    let ships = V6.([
      unspecified,     "global",    is_global,                    false;
      unspecified,     "multicast", is_multicast,                 false;
      unspecified,     "point",     is_scope Point,                true;
      localhost,       "global",    is_global,                    false;
      localhost,       "multicast", is_multicast,                 false;
      localhost,       "interface", is_scope Interface,            true;
      interface_nodes, "global",    is_global,                    false;
      interface_nodes, "multicast", is_multicast,                  true;
      interface_nodes, "interface", is_scope Interface,            true;
      link_nodes,      "global",    is_global,                    false;
      link_nodes,      "multicast", is_multicast,                  true;
      link_nodes,      "link",      is_scope Link,                 true;
      link_routers,    "global",    is_global,                    false;
      link_routers,    "multicast", is_multicast,                  true;
      link_routers,    "link",      is_scope Link,                 true;
      localhost_v4,    "global",    is_global,                    false;
      localhost_v4,    "multicast", is_multicast,                 false;
      localhost_v4,    "ipv4",      is Prefix.ipv4_mapped,         true;
      localhost_v4,    "noneui64",  is Prefix.noneui64_interface,  true;
      localhost_v4,    "global_001",is Prefix.global_unicast_001, false;
      localhost_v4,    "interface", is_scope Interface,            true;
    ]) in
    List.iter (fun (addr,lbl,pred,is_mem) ->
      let mems = if is_mem then "" else " not" in
      let msg = (V6.to_string addr)^" is"^mems^" in "^lbl in
      assert_equal ~msg (pred addr) is_mem
    ) ships

  let test_map () =
    let module M = Map.Make(V6) in
    let maxs = "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff" in
    let m = M.add (V6.of_string_exn "::0:0") "min" M.empty in
    let m = M.add (V6.of_string_exn maxs) "the greatest host" m in
    let m = M.add (V6.of_string_exn "::") "the least host" m in
    assert_equal ~msg:"size" (M.cardinal m) 2;
    let (min_key, min_val) = M.min_binding m in
    assert_equal ~msg:("min is '" ^ min_val ^"'") (min_key, min_val)
      (V6.of_string_exn "::0:0:0", "the least host");
    assert_equal ~msg:"max" (M.max_binding m)
      (V6.of_string_exn maxs, "the greatest host")

  let test_prefix_map () =
    let module M = Map.Make(V6.Prefix) in
    let of_string s = s |> V6.Prefix.of_string_exn |> V6.Prefix.prefix in
    let m = M.add (of_string "::ffff:0.0.0.0/0") "everyone" M.empty in
    let m = M.add (of_string "::ffff:192.0.0.0/1") "weirdos" m in
    let m = M.add (of_string "::ffff:128.0.0.0/1") "high-bitters" m in
    let m = M.add (of_string "::ffff:254.0.0.0/8") "top-end" m in
    let m = M.add (of_string "::ffff:0.0.0.0/0") "iana" m in
    assert_equal ~msg:"size" (M.cardinal m) 3;
    assert_equal ~msg:"min" (M.min_binding m)
      (of_string "::ffff:0.0.0.0/0", "iana");
    assert_equal ~msg:"max" (M.max_binding m)
      (of_string "::ffff:254.0.0.0/8", "top-end");
    assert_equal ~msg:"third"
      (M.find (of_string "::ffff:128.0.0.0/1") m) "high-bitters"

  let test_multicast_mac () =
    let on = 0xFFFF in
    let ip = V6.make on on on on on 0xFFFF 0xFEFE 0xFDFD in
    let unicast   = V6.Prefix.(network_address global_unicast_001 ip) in
    let multicast = V6.Prefix.(network_address multicast ip) in
    let unicast_mac_str   = Macaddr.to_string (V6.multicast_to_mac unicast) in
    let multicast_mac_str = Macaddr.to_string (V6.multicast_to_mac multicast) in
    let mac_str = "33:33:fe:fe:fd:fd" in
    assert_equal ~msg:("unicast_mac "^unicast_mac_str^" <> "^mac_str)
      unicast_mac_str   mac_str;
    assert_equal ~msg:("multicast_mac "^multicast_mac_str^" <> "^mac_str)
      multicast_mac_str mac_str

  let test_domain_name () =
    let ip = V6.of_string_exn "2a00:1450:4009:800::200e" in
    let name =
      "e.0.0.2.0.0.0.0.0.0.0.0.0.0.0.0.0.0.8.0.9.0.0.4.0.5.4.1.0.0.a.2.ip6.arpa"
    in
    let name = Domain_name.(host_exn (of_string_exn name)) in
    assert_equal ~cmp:Domain_name.equal ~msg:"to_domain_name"
      (V6.to_domain_name ip) name ;
    assert_equal ~msg:"of_domain_name" (V6.of_domain_name name) (Some ip)

  let test_link_address_of_mac () =
    let mac = Macaddr.of_string_exn "34-56-78-9A-BC-DE" in
    let ip_str = V6.(to_string (link_address_of_mac mac)) in
    let expected = "fe80::3656:78ff:fe9a:bcde" in
    assert_equal ~msg:("link_address_of_mac "^ip_str^" <> "^expected)
      ip_str expected

  let test_succ_pred () =
    let open V6 in
    let printer = function
      | Ok v -> Printf.sprintf "Ok %s" (V6.to_string v)
      | Error (`Msg e) -> Printf.sprintf "Error `Msg \"%s\"" e
    in
    let assert_equal = assert_equal ~printer in
    let ip1 = of_string_exn "::" in
    let ip2 = of_string_exn "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff" in
    let ip3 = of_string_exn "::2" in
    assert_equal ~msg:"succ ::" (of_string "::1") (succ ip1);
    assert_equal ~msg:"succ (succ ::)"
      (of_string "::2") (succ ip1 >>= succ);
    assert_equal ~msg:"succ ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"
      (Error (`Msg "Ipaddr: highest address has been reached")) (succ ip2);
    assert_equal ~msg:"pred ::2" (of_string "::1") (pred ip3) ;
    assert_equal ~msg:"pred ::ffff:ffff"
      (of_string "::ffff:fffd")
      (of_string "::ffff:ffff" >>= pred >>= pred);
    assert_equal ~msg:"pred ::"
      (Error (`Msg "Ipaddr: lowest address has been reached")) (pred ip1);
    assert_equal ~msg:"pred (succ ::2)" (Ok ip3) (succ ip3 >>= pred)

  let test_first_last () =
    let open V6 in
    let open Prefix in
    let ip_of_string = V6.of_string_exn in
    let assert_equal = assert_equal ~printer:V6.to_string in
    assert_equal ~msg:"first ::/64"
      (ip_of_string "::1") (first @@ of_string_exn "::/64");
    assert_equal ~msg:"first ::ff00/120"
      (ip_of_string "::ff01") (first @@ of_string_exn "::ff00/120");
    assert_equal ~msg:"first ::aaa0/127"
      (ip_of_string "::aaa0") (first @@ of_string_exn "::aaa0/127");
    assert_equal ~msg:"first ::aaa0/128" (ip_of_string "::aaa0")
      (first @@ of_string_exn "::aaa0/128");
    assert_equal ~msg:"last ::/64" (ip_of_string "::ffff:ffff:ffff:ffff")
      (last @@ of_string_exn "::/64");
    assert_equal ~msg:"last ::/120" (ip_of_string "::ff")
      (last @@ of_string_exn "::/120");
    assert_equal ~msg:"last ::/112" (ip_of_string "::ffff")
      (last @@ of_string_exn "::/112");
    assert_equal ~msg:"last ::bbbb:eeee:0000:0000/64" (ip_of_string "::ffff:ffff:ffff:ffff")
      (last @@ of_string_exn "::bbbb:eeee:0000:0000/64");
    assert_equal ~msg:"last ::aaa0/127" (ip_of_string "::aaa1")
      (last @@ of_string_exn "::aaa0/127");
    assert_equal ~msg:"last ::aaa0/128" (ip_of_string "::aaa0")
      (last @@ of_string_exn "::aaa0/128")

  let suite = "Test V6" >::: [
    "string_rt"            >:: test_string_rt;
    "string_rt_bad"        >:: test_string_rt_bad;
    "string_raw_rt"        >:: test_string_raw_rt;
    "string_raw_rt_bad"    >:: test_string_raw_rt_bad;
    "bytes_rt"             >:: test_bytes_rt;
    "bytes_rt_bad"         >:: test_bytes_rt_bad;
    "cstruct_rt"           >:: test_cstruct_rt;
    "cstruct_rt_bad"       >:: test_cstruct_rt_bad;
    "int32_rt"             >:: test_int32_rt;
    "prefix_string_rt"     >:: test_prefix_string_rt;
    "prefix_string_rt_bad" >:: test_prefix_string_rt_bad;
    "network_address_rt"   >:: test_network_address_rt;
    "prefix_bits"          >:: test_prefix_bits;
    "prefix_netmask"       >:: test_prefix_netmask;
    "prefix_netmask_bad"   >:: test_prefix_netmask_bad;
    "scope"                >:: test_scope;
    "map"                  >:: test_map;
    "prefix_map"           >:: test_prefix_map;
    "multicast_mac"        >:: test_multicast_mac;
    "domain_name"          >:: test_domain_name;
    "link_address_of_mac"  >:: test_link_address_of_mac;
    "succ_pred"            >:: test_succ_pred;
    "first_last"           >:: test_first_last;
  ]
end

let test_string_raw_rt () =
  let addrs = [
    ("IP: 192.168.0.0!!",4), ("192.168.0.0",15);
    ("IP: 192:168:0::!!",4), ("192:168::",15);
    ("IP: [192:168::]!!",4), ("192:168::",15);
  ] in
  List.iter (fun ((addr,off),(result,cursor)) ->
    let c = ref off in
    let os = of_string_raw addr c in
    let ts = to_string os in
    let msg = Printf.sprintf "%s at %d: %s at %d <> %s at %d"
      addr off result cursor ts !c
    in assert_equal ~msg (ts,!c) (result,cursor)
  ) addrs

let test_string_raw_rt_bad () =
  let error (s,c) msg c' = (s,c), (Parse_error (msg,s),c') in
  let addrs = [
    error ("IP: ::192.168 ",4) "not an IPv4 address: invalid character ':' at 4\nnot an IPv6 address: invalid character ' ' at 13" 13;
    error ("IP: [::192.168] ",4) "not an IPv4 address: invalid character '[' at 4\nnot an IPv6 address: invalid character ']' at 14" 14; (* ? *)
    error ("IP: 192:168::3.5 ",4) "not an IPv4 address: invalid character ':' at 7\nnot an IPv6 address: invalid character ' ' at 16" 16;
  ] in
  List.iter (fun ((addr,off),(exn,cursor)) ->
    let c = ref off in
    assert_raises ~msg:addr exn (fun () -> of_string_raw addr c);
    assert_equal ~msg:(Printf.sprintf "%s cursor <> %d (%d)" addr cursor !c)
      !c cursor
  ) addrs

let test_map () =
  let module M = Map.Make(Ipaddr) in
  let maxv6 = "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff" in
  let maxv4 = "254.254.254.254" in
  let m = M.add (of_string_exn maxv4) "the greatest host v4" M.empty in
  let m = M.add (of_string_exn "::0:0") "minv6" m in
  let m = M.add (of_string_exn maxv6) "the greatest host v6" m in
  let m = M.add (of_string_exn "::") "the least host v6" m in
  let m = M.add (of_string_exn "1.0.0.1") "minv4" m in
  let m = M.add (of_string_exn "1.0.0.1") "the least host v4" m in
  assert_equal ~msg:"size" (M.cardinal m) 4;
  let (min_key, min_val) = M.min_binding m in
  assert_equal ~msg:("min is '" ^ min_val ^"'") (min_key, min_val)
    (of_string_exn "1.0.0.1", "the least host v4");
  assert_equal ~msg:"max" (M.max_binding m)
    (of_string_exn maxv6, "the greatest host v6")

let test_prefix_mem () =
  let ip = of_string_exn in
  let ships = [
    ip "192.168.0.1",     V4 V4.Prefix.private_192,                   true;
    ip "192.168.0.1",     Prefix.of_string_exn "::ffff:0:0/96",       true;
    ip "192.168.0.1",     Prefix.of_string_exn "::ffff:0:0/95",       true;
    ip "192.168.0.1",     Prefix.of_string_exn "::ffff:0:0/97",       false;
    ip "192.168.0.1",     Prefix.of_string_exn "::ffff:128.0.0.0/97", true;
    ip "::ffff:10.0.0.1", V4 V4.Prefix.private_10,                    true;
    ip "::fffe:10.0.0.1", V4 V4.Prefix.private_10,                    false;
  ] in
  List.iter (fun (addr,subnet,is_mem) ->
    let msg = Printf.sprintf "%s is%s in %s"
      (to_string addr) (if is_mem then "" else " not") (Prefix.to_string subnet)
    in
    assert_equal ~msg (Prefix.mem addr subnet) is_mem
  ) ships

let test_prefix_subset () =
  let pre = Prefix.of_string_exn in
  let ships = [
    pre "10.0.0.1/32",       pre "10.0.0.1/32",       true;
    pre "10.0.0.1/32",       pre "10.0.0.2/32",       false;
    pre "10.0.0.3/32",       pre "10.0.0.2/31",       true;
    pre "10.0.0.2/31",       pre "10.0.0.3/32",       false;
    pre "10.0.10.0/24",      V4 V4.Prefix.private_10, true;
    V4 V4.Prefix.private_10, pre "10.0.10.0/24",      false;
  ] in
  List.iter (fun (subnet1,subnet2,is_subset) ->
    let msg = Printf.sprintf "%s is%s subset of %s"
        (Prefix.to_string subnet1)
        (if is_subset then "" else " not")
        (Prefix.to_string subnet2)
    in
    assert_equal ~msg
      (Prefix.subset ~subnet:subnet1 ~network:subnet2) is_subset
  ) ships

let suite = "Test Generic Addresses" >::: [
  "string_raw_rt"     >:: test_string_raw_rt;
  "string_raw_rt_bad" >:: test_string_raw_rt_bad;
  "map"               >:: test_map;
  "prefix_mem"        >:: test_prefix_mem;
  "prefix_subset"     >:: test_prefix_subset;
]

;;
let _results = run_test_tt_main Test_v4.suite in
let _results = run_test_tt_main Test_v6.suite in
let _results = run_test_tt_main suite in
()
