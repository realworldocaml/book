open Lwt.Infix

let client : Conduit_mirage.client =
  `TCP (Ipaddr.of_string_exn "127.0.0.1", 12345)

let server : Conduit_mirage.server = `TCP 12345

module TCP = Conduit_mirage.TCP (Tcpip_stack_socket.V4V6)

let tcp () =
  let ipv4_only = false and ipv6_only = false in
  Udpv4v6_socket.connect ~ipv4_only ~ipv6_only Ipaddr.V4.Prefix.global None
  >>= fun udp ->
  Tcpv4v6_socket.connect ~ipv4_only ~ipv6_only Ipaddr.V4.Prefix.global None
  >>= fun tcp -> Tcpip_stack_socket.V4V6.connect udp tcp

let _client () = tcp () >>= fun t -> TCP.connect t client

let _server () =
  tcp () >>= fun t -> TCP.listen t server (fun _flow -> Lwt.return ())
