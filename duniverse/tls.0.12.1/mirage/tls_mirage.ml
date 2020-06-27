open Lwt

module Make (F : Mirage_flow.S) = struct

  module FLOW = F

  type error  = [ `Tls_alert   of Tls.Packet.alert_type
                | `Tls_failure of Tls.Engine.failure
                | `Read of F.error
                | `Write of F.write_error ]

  type write_error = [ Mirage_flow.write_error | error ]

  type buffer = Cstruct.t
  type +'a io = 'a Lwt.t

  let pp_error ppf = function
    | `Tls_failure f -> Fmt.string ppf @@ Tls.Engine.string_of_failure f
    | `Tls_alert a   -> Fmt.string ppf @@ Tls.Packet.alert_type_to_string a
    | `Read  e       -> F.pp_error ppf e
    | `Write e       -> F.pp_write_error ppf e

  let pp_write_error ppf = function
    | #Mirage_flow.write_error as e -> Mirage_flow.pp_write_error ppf e
    | #error as e                   -> pp_error ppf e

  type flow = {
    role           : [ `Server | `Client ] ;
    flow           : FLOW.flow ;
    mutable state  : [ `Active of Tls.Engine.state
                     | `Eof
                     | `Error of error ] ;
    mutable linger : Cstruct.t list ;
  }

  let tls_alert a = `Error (`Tls_alert a)
  let tls_fail f  = `Error (`Tls_failure f)

  let list_of_option = function None -> [] | Some x -> [x]

  let lift_read_result = function
    | Ok (`Data _ | `Eof as x) -> x
    | Error e                  -> `Error (`Read e)

  let lift_write_result = function
    | Ok ()   -> `Ok ()
    | Error e -> `Error (`Write e)

  let check_write flow f_res =
    let res = lift_write_result f_res in
    ( match flow.state, res with
      | `Active _, (`Eof | `Error _ as e) ->
          flow.state <- e ; FLOW.close flow.flow
      | _ -> return_unit ) >|= fun () ->
    match f_res with
    | Ok ()   -> Ok ()
    | Error e -> Error (`Write e :> write_error)

  let read_react flow =

    let handle tls buf =
      match Tls.Engine.handle_tls tls buf with
      | `Ok (res, `Response resp, `Data data) ->
          flow.state <- ( match res with
            | `Ok tls      -> `Active tls
            | `Eof         -> `Eof
            | `Alert alert -> tls_alert alert );
          ( match resp with
            | None     -> return @@ Ok ()
            | Some buf -> FLOW.write flow.flow buf >>= check_write flow ) >>= fun _ ->
          ( match res with
            | `Ok _ -> return_unit
            | _     -> FLOW.close flow.flow ) >>= fun () ->
          return @@ `Ok data
      | `Fail (fail, `Response resp) ->
          let reason = tls_fail fail in
          flow.state <- reason ;
          FLOW.(write flow.flow resp >>= fun _ -> close flow.flow) >>= fun () -> return reason
    in
    match flow.state with
    | `Eof | `Error _ as e -> return e
    | `Active _            ->
      FLOW.read flow.flow >|= lift_read_result >>=
      function
      | `Eof | `Error _ as e -> flow.state <- e ; return e
      | `Data buf            -> match flow.state with
        | `Active tls          -> handle tls buf
        | `Eof | `Error _ as e -> return e

  let rec read flow =
    match flow.linger with
    | [] ->
      ( read_react flow >>= function
          | `Ok None       -> read flow
          | `Ok (Some buf) -> return @@ Ok (`Data buf)
          | `Eof           -> return @@ Ok `Eof
          | `Error e       -> return @@ Error e )
    | bufs ->
      flow.linger <- [] ;
      return @@ Ok (`Data (Tls.Utils.Cs.appends @@ List.rev bufs))

  let writev flow bufs =
    match flow.state with
    | `Eof     -> return @@ Error `Closed
    | `Error e -> return @@ Error (e :> write_error)
    | `Active tls ->
        match Tls.Engine.send_application_data tls bufs with
        | Some (tls, answer) ->
            flow.state <- `Active tls ;
            FLOW.write flow.flow answer >>= check_write flow
        | None ->
            (* "Impossible" due to handshake draining. *)
            assert false

  let write flow buf = writev flow [buf]

  (*
   * XXX bad XXX
   * This is a point that should particularly be protected from concurrent r/w.
   * Doing this before a `t` is returned is safe; redoing it during rekeying is
   * not, as the API client already sees the `t` and can mistakenly interleave
   * writes while this is in progress.
   * *)
  let rec drain_handshake flow =
    match flow.state with
    | `Active tls when not (Tls.Engine.handshake_in_progress tls) ->
        return @@ Ok flow
    | _ ->
      (* read_react re-throws *)
        read_react flow >>= function
        | `Ok mbuf ->
            flow.linger <- list_of_option mbuf @ flow.linger ;
            drain_handshake flow
        | `Error e -> return @@ Error (e :> write_error)
        | `Eof     -> return @@ Error `Closed

  let reneg ?authenticator ?acceptable_cas ?cert ?(drop = true) flow =
    match flow.state with
    | `Eof        -> return @@ Error `Closed
    | `Error e    -> return @@ Error (e :> write_error)
    | `Active tls ->
        match Tls.Engine.reneg ?authenticator ?acceptable_cas ?cert tls with
        | None             ->
            (* XXX make this impossible to reach *)
            invalid_arg "Renegotiation already in progress"
        | Some (tls', buf) ->
            if drop then flow.linger <- [] ;
            flow.state <- `Active tls' ;
            FLOW.write flow.flow buf >>= fun _ ->
            drain_handshake flow >|= function
            | Ok _         -> Ok ()
            | Error _ as e -> e

  let key_update ?request flow =
    match flow.state with
    | `Eof        -> return @@ Error `Closed
    | `Error e    -> return @@ Error (e :> write_error)
    | `Active tls ->
      match Tls.Engine.key_update ?request tls with
      | Error _ -> invalid_arg "Key update failed"
      | Ok (tls', buf) ->
        flow.state <- `Active tls' ;
        FLOW.write flow.flow buf >>= check_write flow

  let close flow =
    match flow.state with
    | `Active tls ->
      flow.state <- `Eof ;
      let (_, buf) = Tls.Engine.send_close_notify tls in
      FLOW.(write flow.flow buf >>= fun _ -> close flow.flow)
    | _           -> return_unit

  let client_of_flow conf ?host flow =
    let conf' = match host with
      | None      -> conf
      | Some host -> Tls.Config.peer conf host
    in
    let (tls, init) = Tls.Engine.client conf' in
    let tls_flow = {
      role   = `Client ;
      flow   = flow ;
      state  = `Active tls ;
      linger = [] ;
    } in
    FLOW.write flow init >>= fun _ -> drain_handshake tls_flow

  let server_of_flow conf flow =
    let tls_flow = {
      role   = `Server ;
      flow   = flow ;
      state  = `Active (Tls.Engine.server conf) ;
      linger = [] ;
    } in
    drain_handshake tls_flow

  let epoch flow =
    match flow.state with
    | `Eof | `Error _ -> Error ()
    | `Active tls     ->
        match Tls.Engine.epoch tls with
        | `InitialEpoch -> assert false (* `drain_handshake` invariant. *)
        | `Epoch e      -> Ok e

(*   let create_connection t tls_params host (addr, port) =
    |+ XXX addr -> (host : string) +|
    TCP.create_connection t (addr, port) >>= function
      | `Error _ as e -> return e
      | `Ok flow      -> client_of_tcp_flow tls_params host flow *)

(*   let listen_ssl t cert ~port callback =
    let cb flow =
      server_of_tcp_flow cert flow >>= callback in
    TCP.input t ~listeners:(fun p -> if p = port then Some cb else None) *)

end

module X509 (KV : Mirage_kv.RO) (C: Mirage_clock.PCLOCK) = struct

  let ca_roots_file = Mirage_kv.Key.v "ca-roots.crt"
  let default_cert  = "server"

  let err_fail pp = function
    | Ok x -> return x
    | Error e -> Fmt.kstrf fail_with "%a" pp e

  let pp_msg ppf = function `Msg m -> Fmt.string ppf m

  let decode_or_fail f cs = err_fail pp_msg (f cs)

  let read kv name =
    KV.get kv name >>= err_fail KV.pp_error >|= Cstruct.of_string

  let read_crl kv = function
    | None -> Lwt.return None
    | Some filename ->
      read kv (Mirage_kv.Key.v filename) >>= fun data ->
      err_fail pp_msg (X509.CRL.decode_der data) >|= fun crl ->
      Some [ crl ]

  let authenticator ?hash_whitelist ?crl kv =
    let time () = Some (Ptime.v (C.now_d_ps ())) in
    let now = Ptime.v (C.now_d_ps ()) in
    read kv ca_roots_file >>=
    decode_or_fail X509.Certificate.decode_pem_multiple >>= fun cas ->
    let ta = X509.Validation.valid_cas ~time:now cas in
    read_crl kv crl >|= fun crls ->
    X509.Authenticator.chain_of_trust ?crls ?hash_whitelist ~time ta

  let certificate kv =
    let read name =
      read kv (Mirage_kv.Key.v (name ^ ".pem")) >>=
      decode_or_fail X509.Certificate.decode_pem_multiple >>= fun certs ->
      read kv (Mirage_kv.Key.v (name ^ ".key")) >>=
      decode_or_fail X509.Private_key.decode_pem >|= fun (`RSA pk) ->
      (certs, pk)
    in function | `Default   -> read default_cert
                | `Name name -> read name
end
