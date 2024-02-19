module Flow = Eio.Flow

exception Tls_alert   of Tls.Packet.alert_type
exception Tls_failure of Tls.Engine.failure

module Raw = struct

  (* We could replace [`Eof] with [`Error End_of_file] and then use
     a regular [result] type here. *)
  type t = {
    flow           : Flow.two_way ;
    mutable state  : [ `Active of Tls.Engine.state
                     | `Eof
                     | `Error of exn ] ;
    mutable linger : Cstruct.t option ;
    recv_buf       : Cstruct.t ;
  }

  let read_t t cs =
    try Flow.read t.flow cs
    with
    | End_of_file as ex ->
      t.state <- `Eof;
      raise ex
    | exn ->
      (match t.state with
       | `Error _ | `Eof -> ()
       | `Active _ -> t.state <- `Error exn) ;
      raise exn

  let write_t t cs =
    try Flow.copy (Flow.cstruct_source [cs]) t.flow
    with exn ->
      (match t.state with
       | `Error _ | `Eof -> ()
       | `Active _ -> t.state <- `Error exn) ;
      raise exn

  let try_write_t t cs =
    try write_t t cs
    with _ -> Eio.Fiber.check ()      (* Error is in [t.state] *)

  let rec read_react t =

    let handle tls buf =
      match Tls.Engine.handle_tls tls buf with
      | Ok (state', `Response resp, `Data data) ->
          let state' = match state' with
            | `Ok tls  -> `Active tls
            | `Eof     -> raise End_of_file
            | `Alert a -> `Error (Tls_alert a)
          in
          t.state <- state' ;
          Option.iter (try_write_t t) resp;
          data

      | Error (alert, `Response resp) ->
          t.state <- `Error (Tls_failure alert) ;
          write_t t resp; read_react t
    in

    match t.state with
    | `Error e  -> raise e
    | `Eof      -> raise End_of_file
    | `Active _ ->
        let n = read_t t t.recv_buf in
        match (t.state, n) with
        | (`Active tls, n) -> handle tls (Cstruct.sub t.recv_buf 0 n)
        | (`Error e, _)    -> raise e
        | (`Eof, _)        -> raise End_of_file

  let rec read t buf =

    let writeout res =
      let open Cstruct in
      let rlen = length res in
      let n    = min (length buf) rlen in
      blit res 0 buf 0 n ;
      t.linger <-
        (if n < rlen then Some (sub res n (rlen - n)) else None) ;
      n in

    match t.linger with
    | Some res -> writeout res
    | None     ->
        match read_react t with
          | None     -> read t buf
          | Some res -> writeout res

  let writev t css =
    match t.state with
    | `Error err  -> raise err
    | `Eof        -> raise End_of_file
    | `Active tls ->
        match Tls.Engine.send_application_data tls css with
        | Some (tls, tlsdata) ->
            ( t.state <- `Active tls ; write_t t tlsdata )
        | None -> invalid_arg "tls: write: socket not ready"

  let write t cs = writev t [cs]

  (*
   * XXX bad XXX
   * This is a point that should particularly be protected from concurrent r/w.
   * Doing this before a `t` is returned is safe; redoing it during rekeying is
   * not, as the API client already sees the `t` and can mistakenly interleave
   * writes while this is in progress.
   * *)
  let rec drain_handshake t =
    let push_linger t mcs =
      match (mcs, t.linger) with
      | (None, _)         -> ()
      | (scs, None)       -> t.linger <- scs
      | (Some cs, Some l) -> t.linger <- Some (Cstruct.append l cs)
    in
    match t.state with
    | `Active tls when not (Tls.Engine.handshake_in_progress tls) ->
        t
    | _ ->
        let cs = read_react t in
        push_linger t cs; drain_handshake t

  let reneg ?authenticator ?acceptable_cas ?cert ?(drop = true) t =
    match t.state with
    | `Error err  -> raise err
    | `Eof        -> raise End_of_file
    | `Active tls ->
        match Tls.Engine.reneg ?authenticator ?acceptable_cas ?cert tls with
        | None -> invalid_arg "tls: can't renegotiate"
        | Some (tls', buf) ->
           if drop then t.linger <- None ;
           t.state <- `Active tls' ;
           write_t t buf;
           ignore (drain_handshake t : t)

  let key_update ?request t =
    match t.state with
    | `Error err  -> raise err
    | `Eof        -> raise End_of_file
    | `Active tls ->
      match Tls.Engine.key_update ?request tls with
      | Error _ -> invalid_arg "tls: can't update key"
      | Ok (tls', buf) ->
        t.state <- `Active tls' ;
        write_t t buf

  let close_tls t =
    match t.state with
    | `Active tls ->
        let (_, buf) = Tls.Engine.send_close_notify tls in
        t.state <- `Eof ;       (* XXX: this looks wrong - we're only trying to close the sending side *)
        write_t t buf
    | _ -> ()

  (* Not sure if we need to keep both directions open on the underlying flow when closing
     one direction at the TLS level. *)
  let shutdown t = function
    | `Send -> close_tls t
    | `All -> close_tls t; Flow.shutdown t.flow `All
    | `Receive -> ()  (* Not obvious how to do this with TLS, so ignore for now. *)

  let server_of_flow config flow =
    drain_handshake {
      state    = `Active (Tls.Engine.server config) ;
      flow     = (flow :> Flow.two_way) ;
      linger   = None ;
      recv_buf = Cstruct.create 4096
    }

  let client_of_flow config ?host flow =
    let config' = match host with
      | None -> config
      | Some host -> Tls.Config.peer config host
    in
    let t = {
      state    = `Eof ;
      flow     = (flow :> Flow.two_way);
      linger   = None ;
      recv_buf = Cstruct.create 4096
    } in
    let (tls, init) = Tls.Engine.client config' in
    let t = { t with state  = `Active tls } in
    write_t t init;
    drain_handshake t


  let epoch t =
    match t.state with
    | `Active tls -> ( match Tls.Engine.epoch tls with
        | `InitialEpoch -> assert false (* can never occur! *)
        | `Epoch data   -> Ok data )
    | `Eof      -> Error ()
    | `Error _  -> Error ()

  let copy_from t src =
    try
      while true do
        let buf = Cstruct.create 4096 in
        let got = Flow.read src buf in
        write t (Cstruct.sub buf 0 got)
      done
    with End_of_file -> ()
end

type t = <
  Eio.Flow.two_way;
  t : Raw.t;
>

let of_t t =
  object
    inherit Eio.Flow.two_way
    method read_into = Raw.read t
    method copy = Raw.copy_from t
    method shutdown = Raw.shutdown t
    method t = t
  end

let server_of_flow config       flow = Raw.server_of_flow config       flow |> of_t
let client_of_flow config ?host flow = Raw.client_of_flow config ?host flow |> of_t

let reneg ?authenticator ?acceptable_cas ?cert ?drop (t:t) = Raw.reneg ?authenticator ?acceptable_cas ?cert ?drop t#t
let key_update ?request (t:t) = Raw.key_update ?request t#t
let epoch (t:t) = Raw.epoch t#t

let () =
  Printexc.register_printer (function
      | Tls_alert typ ->
        Some ("TLS alert from peer: " ^ Tls.Packet.alert_type_to_string typ)
      | Tls_failure f ->
        Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
      | _ -> None)
