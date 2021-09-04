open! Core
open! Async
include Io_intf

module Tls_error = struct
  type t =
    | Tls_alert of Tls.Packet.alert_type
    (** [Tls_alert] exception received from the other endpoint *)
    | Tls_failure of Tls.Engine.failure
    (** [Tls_failure] exception while processing incoming data *)
    | Connection_closed
    | Connection_not_ready
    | Unexpected_eof
    | Unable_to_renegotiate
    | Unable_to_update_key
  [@@deriving sexp_of]
end

module Make (Fd : Fd) : S with module Fd := Fd = struct
  open Deferred.Or_error.Let_syntax

  module State = struct
    type t =
      | Active of Tls.Engine.state
      | Eof
      | Error of Tls_error.t
  end

  type t =
    { fd : Fd.t
    ; mutable state : State.t
    ; mutable linger : Cstruct.t option
    ; recv_buf : Cstruct.t
    }

  let tls_error = Fn.compose Deferred.Or_error.error_s Tls_error.sexp_of_t

  let rec read_react t =
    let handle tls buf =
      match Tls.Engine.handle_tls tls buf with
      | Ok (state, `Response resp, `Data data) ->
        t.state
        <- (match state with
          | `Ok tls -> Active tls
          | `Eof -> Eof
          | `Alert a -> Error (Tls_alert a));
        let%map () =
          match resp with
          | None -> return ()
          | Some resp -> Fd.write_full t.fd resp
        in
        `Ok data
      | Error (alert, `Response resp) ->
        t.state <- Error (Tls_failure alert);
        let%bind () = Fd.write_full t.fd resp in
        read_react t
    in
    match t.state with
    | Error e -> tls_error e
    | Eof -> return `Eof
    | Active _ ->
      let%bind n = Fd.read t.fd t.recv_buf in
      (match t.state, n with
       | Active _, `Eof ->
         t.state <- Eof;
         return `Eof
       | Active tls, `Ok n -> handle tls (Cstruct.sub t.recv_buf 0 n)
       | Error e, _ -> tls_error e
       | Eof, _ -> return `Eof)
  ;;

  let rec read t buf =
    let writeout res =
      let open Cstruct in
      let rlen = length res in
      let n = min (length buf) rlen in
      blit res 0 buf 0 n;
      t.linger <- (if n < rlen then Some (sub res n (rlen - n)) else None);
      return n
    in
    match t.linger with
    | Some res -> writeout res
    | None ->
      (match%bind read_react t with
       | `Eof -> return 0
       | `Ok None -> read t buf
       | `Ok (Some res) -> writeout res)
  ;;

  let writev t css =
    match t.state with
    | Error err -> tls_error err
    | Eof -> tls_error Connection_closed
    | Active tls ->
      (match Tls.Engine.send_application_data tls css with
       | Some (tls, tlsdata) ->
         t.state <- Active tls;
         Fd.write_full t.fd tlsdata
       | None -> tls_error Connection_not_ready)
  ;;

  (*
   * XXX bad XXX
   * This is a point that should particularly be protected from concurrent r/w.
   * Doing this before a `t` is returned is safe; redoing it during rekeying is
   * not, as the API client already sees the `t` and can mistakenly interleave
   * writes while this is in progress.
   * *)
  let rec drain_handshake t =
    let push_linger t mcs =
      match mcs, t.linger with
      | None, _ -> ()
      | scs, None -> t.linger <- scs
      | Some cs, Some l -> t.linger <- Some (Cstruct.append l cs)
    in
    match t.state with
    | Active tls when not (Tls.Engine.handshake_in_progress tls) -> return t
    | _ ->
      (match%bind read_react t with
       | `Eof -> tls_error Unexpected_eof
       | `Ok cs ->
         push_linger t cs;
         drain_handshake t)
  ;;

  let reneg ?authenticator ?acceptable_cas ?cert ?(drop = true) t =
    match t.state with
    | Error err -> tls_error err
    | Eof -> tls_error Connection_closed
    | Active tls ->
      (match Tls.Engine.reneg ?authenticator ?acceptable_cas ?cert tls with
       | None -> tls_error Unable_to_renegotiate
       | Some (tls', buf) ->
         if drop then t.linger <- None;
         t.state <- Active tls';
         let%bind () = Fd.write_full t.fd buf in
         let%bind _ = drain_handshake t in
         return ())
  ;;

  let key_update ?request t =
    match t.state with
    | Error err -> tls_error err
    | Eof -> tls_error Connection_closed
    | Active tls ->
      (match Tls.Engine.key_update ?request tls with
       | Error _ -> tls_error Unable_to_update_key
       | Ok (tls', buf) ->
         t.state <- Active tls';
         Fd.write_full t.fd buf)
  ;;

  let close_tls t =
    match t.state with
    | Active tls ->
      let _, buf = Tls.Engine.send_close_notify tls in
      t.state <- Eof;
      Fd.write_full t.fd buf
    | _ -> return ()
  ;;

  let server_of_fd config fd =
    drain_handshake
      { state = Active (Tls.Engine.server config)
      ; fd
      ; linger = None
      ; recv_buf = Cstruct.create 4096
      }
  ;;

  let client_of_fd config ?host fd =
    let config' =
      match host with
      | None -> config
      | Some host -> Tls.Config.peer config host
    in
    let t = { state = Eof; fd; linger = None; recv_buf = Cstruct.create 4096 } in
    let tls, init = Tls.Engine.client config' in
    let t = { t with state = Active tls } in
    let%bind () = Fd.write_full t.fd init in
    drain_handshake t
  ;;

  let epoch t =
    match t.state with
    | Active tls ->
      (match Tls.Engine.epoch tls with
       | `InitialEpoch -> assert false (* can never occur! *)
       | `Epoch data -> Ok data)
    | Eof -> Or_error.error_string "TLS state is end of file"
    | Error _ -> Or_error.error_string "TLS state is error"
  ;;
end
