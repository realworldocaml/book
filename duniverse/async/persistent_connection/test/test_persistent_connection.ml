open Core
open Async

module Hello = struct
  module Model = struct
    let name = "hello"

    type query = unit
    type response = unit
  end

  include Model
  include Versioned_rpc.Caller_converts.Rpc.Make (Model)

  module V1 = Register (struct
      let version = 1

      type query = unit [@@deriving bin_io]
      type response = unit [@@deriving bin_io]

      let query_of_model = Fn.id
      let model_of_response = Fn.id
    end)
end

let%expect_test _ =
  let implementations =
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Close_connection
      ~implementations:
        (Versioned_rpc.Menu.add
           [ Rpc.Rpc.implement Hello.V1.rpc (fun () () ->
               printf "server says hi\n";
               return ())
           ])
  in
  let%bind server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _addr _conn -> ())
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ()
  in
  let host_and_port =
    Host_and_port.create ~host:"localhost" ~port:(Tcp.Server.listening_on server)
  in
  (* test Persistent_connection.Rpc *)
  let on_unversioned_event : Persistent_connection.Rpc.Event.t -> unit Deferred.t
    = function
      | Obtained_address _ ->
        printf "(Obtained_address <elided>)\n";
        return ()
      | event ->
        print_s [%sexp (event : Persistent_connection.Rpc.Event.t)];
        return ()
  in
  let unversioned_conn =
    Persistent_connection.Rpc.create'
      ~on_event:on_unversioned_event
      ~server_name:"unversioned rpc"
      (fun () -> return (Ok host_and_port))
  in
  let%bind () = [%expect {| Attempting_to_connect |}] in
  let%bind this_conn = Persistent_connection.Rpc.connected unversioned_conn in
  let%bind () =
    [%expect
      {|
        (Obtained_address <elided>)
        (Connected <opaque>)
      |}]
  in
  let%bind () = Rpc.Rpc.dispatch_exn Hello.V1.rpc this_conn () in
  let%bind () = [%expect {| server says hi |}] in
  let%bind () = Persistent_connection.Rpc.close unversioned_conn in
  let%bind () = [%expect {| Disconnected |}] in
  (* test Persistent_connection.Versioned_rpc *)
  let on_versioned_event : Persistent_connection.Versioned_rpc.Event.t -> unit Deferred.t
    = function
      | Obtained_address _ ->
        printf "(Obtained_address <elided>)\n";
        return ()
      | event ->
        print_s [%sexp (event : Persistent_connection.Versioned_rpc.Event.t)];
        return ()
  in
  let versioned_conn =
    Persistent_connection.Versioned_rpc.create'
      ~on_event:on_versioned_event
      ~server_name:"versioned rpc"
      (fun () -> return (Ok host_and_port))
  in
  let%bind () = [%expect {| Attempting_to_connect |}] in
  let%bind this_conn = Persistent_connection.Versioned_rpc.connected versioned_conn in
  let%bind () =
    [%expect
      {|
        (Obtained_address <elided>)
        (Connected <opaque>)
      |}]
  in
  let%bind () = Hello.dispatch_multi this_conn () |> Deferred.Or_error.ok_exn in
  let%bind () = [%expect {| server says hi |}] in
  let%bind () = Persistent_connection.Versioned_rpc.close versioned_conn in
  let%bind () = [%expect {|
    Disconnected |}] in
  return ()
;;
