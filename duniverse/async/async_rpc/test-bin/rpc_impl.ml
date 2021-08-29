open Core
open Async
open Rpc

(* Generic wrapper over both the Async.Tcp and Netkit backed RPC implementations *)

module Server = struct
  type t =
    | Tcp of (Socket.Address.Inet.t, int) Tcp.Server.t
    | Netkit of Netkit.Network.Tcp_endpoint.Server.t

  let bound_on = function
    | Tcp s -> Tcp.Server.listening_on s
    | Netkit s -> Netkit.Network.Tcp_endpoint.Server.bound s |> Sockaddr.port
  ;;

  let close = function
    | Tcp s -> Tcp.Server.close s
    | Netkit s ->
      Netkit.Network.Tcp_endpoint.Server.close s;
      return ()
  ;;
end

type make_transport = Unix.Fd.t -> max_message_size:int -> Async_rpc.Rpc.Transport.t

type t =
  | Async of make_transport
  | Netkit of Netkit.Network.t

let make_client ?heartbeat_config t host port =
  match t with
  | Async make_transport ->
    Connection.client
      ?heartbeat_config
      ~make_transport
      (Tcp.Where_to_connect.of_host_and_port { host; port })
  | Netkit network ->
    Netkit_rpc.Rpc.Connection.client
      ?heartbeat_config
      network
      (Sockaddr.create ~addr:(Sockaddr.Addr.of_string host) ~port)
    >>| Result.map_error ~f:Error.to_exn
;;

let with_client ?heartbeat_config t host port f =
  match%bind make_client ?heartbeat_config t host port with
  | Ok conn ->
    let%bind result = f conn in
    let%bind () = Connection.close conn in
    return (Ok result)
  | Error _ as err -> return err
;;

let make_server ?heartbeat_config ?port ~implementations ~initial_connection_state t =
  match t with
  | Async make_transport ->
    let where_to_listen =
      match port with
      | None -> Tcp.Where_to_listen.of_port_chosen_by_os
      | Some port -> Tcp.Where_to_listen.of_port port
    in
    Connection.serve
      ?heartbeat_config
      ~implementations
      ~initial_connection_state:(fun _ x -> initial_connection_state x)
      ~make_transport
      ~where_to_listen
      ()
    >>| fun s -> Server.Tcp s
  | Netkit network ->
    let sockaddr =
      Sockaddr.create ~addr:Sockaddr.Addr.any ~port:(Option.value ~default:0 port)
    in
    Netkit_rpc.Rpc.Connection.serve
      ?heartbeat_config
      ~implementations
      ~initial_connection_state:(fun _ x -> initial_connection_state x)
      ~on_handler_error:`Ignore
      network
      sockaddr
    |> Server.Netkit
    |> return
;;

let spec =
  let open Command.Spec in
  let standard_mt fd ~max_message_size = Transport.of_fd fd ~max_message_size in
  let low_latency_mt fd ~max_message_size =
    Low_latency_transport.create fd ~max_message_size
  in
  let standard _netkit_ifname = Async standard_mt in
  let low_latency _netkit_ifname = Async low_latency_mt in
  let network ifname =
    match Netkit.Network.find ~name:ifname with
    | Some network -> network
    | None ->
      let network = Netkit_sockets.create ~ifname () in
      Netkit.Network.add_exn
        Netkit_sockets.kind
        ~name:ifname
        (module Netkit_sockets)
        network;
      Netkit.Network.find_exn ~name:ifname
  in
  let netkit netkit_ifname = Netkit (network netkit_ifname) in
  let typ =
    Arg_type.of_alist_exn
      [ "standard", standard; "low-latency", low_latency; "netkit", netkit ]
  in
  let transport_flag = optional_with_default standard typ in
  let netkit_ifname_flag = optional_with_default "eth0" string in
  fun () ->
    step (fun main rpc_impl netkit_ifname -> main ~rpc_impl:(rpc_impl netkit_ifname))
    +> flag "-transport" transport_flag ~doc:" RPC transport backend"
    +> flag
         "-netkit-ifname"
         netkit_ifname_flag
         ~doc:" Interface to create a Netkit network on"
;;
