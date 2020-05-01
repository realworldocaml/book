open Core
open Async
open Async_ssl

let verify_certificate connection =
  match Ssl.Connection.peer_certificate connection with
  | None -> return false
  | Some (Error _) -> return false
  | Some (Ok _) -> return true

let teardown_connection r w =
  Writer.close ~force_close:(Clock.after (sec 30.)) w >>= fun () ->
  Reader.close r

(* One needs to be careful around Async Readers and Writers that share the same underyling
   file descriptor, which is something that happens when they're used for sockets.

   Closing the Reader before the Writer will cause the Writer to throw and complain about
   its underlying file descriptor being closed. This is why instead of using Reader.pipe
   directly below, we write out an equivalent version which will first close the Writer
   before closing the Reader once the input pipe is fully consumed.

   Additionally, [Writer.pipe] will not close the writer if the pipe is closed, so in
   order to avoid leaking file descriptors, we allow the pipe 30 seconds to flush before
   closing the writer. *)
let reader_writer_pipes r w =
  let reader_pipe_r, reader_pipe_w = Pipe.create () in
  let writer_pipe = Writer.pipe w in
  upon (Reader.transfer r reader_pipe_w) (fun () ->
    teardown_connection r w >>> fun () ->
    Pipe.close reader_pipe_w);
  upon (Pipe.closed writer_pipe) (fun () ->
    Deferred.choose
      [ Deferred.choice (Clock.after (sec 30.))
          (fun () -> ())
      ; Deferred.choice (Pipe.downstream_flushed writer_pipe)
          (fun (_ : Pipe.Flushed_result.t) -> ()) ] >>> fun () ->
    don't_wait_for (teardown_connection r w));
  reader_pipe_r, writer_pipe

(* [Reader.of_pipe] will not close the pipe when the returned [Reader] is closed, so we
   manually do that ourselves.

   [Writer.of_pipe] will create a writer that will raise once the pipe is closed, so we
   set [raise_when_consumer_leaves] to false. *)
let reader_writer_of_pipes app_rd app_wr =
  Reader.of_pipe (Info.of_string "async_conduit_ssl_reader") app_rd >>= fun app_reader ->
  upon (Reader.close_finished app_reader) (fun () -> Pipe.close_read app_rd);
  Writer.of_pipe (Info.of_string "async_conduit_ssl_writer") app_wr >>| fun (app_writer,_) ->
  Writer.set_raise_when_consumer_leaves app_writer false;
  app_reader, app_writer

module V1 = struct
  module Ssl = struct
    module Config = struct
      type t = {
        version : Ssl.Version.t option;
        name : string option;
        ca_file : string option;
        ca_path : string option;
        session : Ssl.Session.t option sexp_opaque;
        verify : (Ssl.Connection.t -> bool Deferred.t) option;
      } [@@deriving sexp]

      let verify_certificate = verify_certificate

      let create ?version ?name ?ca_file ?ca_path ?session ?verify () =
        { version; name; ca_file; ca_path; session; verify}
    end

    let connect cfg r w =
      let {Config.version; name; ca_file; ca_path; session; verify} = cfg in
      let net_to_ssl, ssl_to_net = reader_writer_pipes r w in
      let app_to_ssl, app_wr = Pipe.create () in
      let app_rd, ssl_to_app = Pipe.create () in
      let verify_connection = match verify with
        | None -> Fn.const (return true)
        | Some f -> f
      in
      Ssl.client
        ?version
        ?name
        ?ca_file
        ?ca_path
        ?session
        ~app_to_ssl
        ~ssl_to_app
        ~net_to_ssl
        ~ssl_to_net
        ()
      >>= function
      | Error error ->
        teardown_connection r w >>= fun () ->
        Error.raise error
      | Ok conn ->
        verify_connection conn >>= function
        | false ->
          teardown_connection r w >>= fun () ->
          failwith "Connection verification failed."
        | true ->
          reader_writer_of_pipes app_rd app_wr >>| fun (app_reader, app_writer) ->
          (app_reader, app_writer)

    let listen ?(version=Ssl.Version.Tlsv1_2) ?ca_file ?ca_path ~crt_file ~key_file r w =
      let net_to_ssl, ssl_to_net = reader_writer_pipes r w in
      let app_to_ssl, app_wr = Pipe.create () in
      let app_rd, ssl_to_app = Pipe.create () in
      Ssl.server
        ?ca_file
        ?ca_path
        ~version
        ~crt_file
        ~key_file
        ~app_to_ssl
        ~ssl_to_app
        ~net_to_ssl
        ~ssl_to_net
        ()
      >>= function
      | Error error ->
        teardown_connection r w >>= fun () ->
        Error.raise error
      | Ok _ ->
        reader_writer_of_pipes app_rd app_wr >>| fun (app_reader, app_writer) ->
        (app_reader, app_writer)

    type session = Ssl.Session.t sexp_opaque [@@deriving sexp]
    type version = Ssl.Version.t  [@@deriving sexp]
    type connection = Ssl.Connection.t sexp_opaque [@@deriving sexp]
  end
end

module V2 = struct
  module Ssl = struct
    type allowed_ciphers =
      [ `Only of string list | `Openssl_default | `Secure ]
      [@@deriving sexp]

    module Config = struct
      type t = {
        version : Ssl.Version.t option;
        options: Ssl.Opt.t list option;
        name : string option;
        hostname : string option;
        allowed_ciphers: allowed_ciphers option;
        ca_file : string option;
        ca_path : string option;
        crt_file : string option;
        key_file : string option;
        session : Ssl.Session.t option sexp_opaque;
        verify_modes:Verify_mode.t sexp_opaque list option;
        verify : (Ssl.Connection.t -> bool Deferred.t) option;
      } [@@deriving sexp_of]

      let verify_certificate = verify_certificate

      let create
          ?version ?options ?name ?hostname ?allowed_ciphers
          ?ca_file ?ca_path ?crt_file ?key_file
          ?session ?verify_modes ?verify () =
        { version; options; name; hostname; allowed_ciphers;
          ca_file; ca_path; crt_file; key_file; session; verify_modes;
          verify}
    end

    let connect ?(cfg=Config.create ()) r w =
      let { Config.version; options; name; hostname;
            allowed_ciphers; ca_file; ca_path;
            crt_file; key_file; session; verify_modes; verify } = cfg in
      let net_to_ssl, ssl_to_net = reader_writer_pipes r w in
      let app_to_ssl, app_wr = Pipe.create () in
      let app_rd, ssl_to_app = Pipe.create () in
      let verify_connection = match verify with
        | None -> Fn.const (return true)
        | Some f -> f
      in
      Ssl.client
        ?version
        ?options
        ?name
        ?hostname
        ?allowed_ciphers
        ?ca_file
        ?ca_path
        ?crt_file
        ?key_file
        ?session
        ?verify_modes
        ~app_to_ssl
        ~ssl_to_app
        ~net_to_ssl
        ~ssl_to_net
        ()
      >>= function
      | Error error ->
        teardown_connection r w >>= fun () ->
        Error.raise error
      | Ok conn ->
        verify_connection conn >>= function
        | false ->
          teardown_connection r w >>= fun () ->
          failwith "Connection verification failed."
        | true ->
          reader_writer_of_pipes app_rd app_wr >>| fun (app_reader, app_writer) ->
          (app_reader, app_writer)

    let listen
        { Config.version; options; name; allowed_ciphers; ca_file; ca_path;
          crt_file; key_file; verify_modes ; _ } r w =
      let crt_file, key_file =
        match crt_file, key_file with
        | Some crt_file, Some key_file -> crt_file, key_file
        | _ -> invalid_arg "Conduit_async_ssl.ssl_listen: crt_file and \
                            key_file must be specified in cfg." in
      let net_to_ssl, ssl_to_net = reader_writer_pipes r w in
      let app_to_ssl, app_wr = Pipe.create () in
      let app_rd, ssl_to_app = Pipe.create () in
      Ssl.server
        ?version
        ?options
        ?name
        ?allowed_ciphers
        ?ca_file
        ?ca_path
        ~crt_file
        ~key_file
        ?verify_modes
        ~app_to_ssl
        ~ssl_to_app
        ~net_to_ssl
        ~ssl_to_net
        ()
      >>= function
      | Error error ->
        teardown_connection r w >>= fun () ->
        Error.raise error
      | Ok _ ->
        reader_writer_of_pipes app_rd app_wr >>| fun (app_reader, app_writer) ->
        (app_reader, app_writer)

    type verify_mode = Ssl.Verify_mode.t [@@deriving sexp_of]
    type session = Ssl.Session.t sexp_opaque [@@deriving sexp_of]
    type version = Ssl.Version.t [@@deriving sexp]
    type connection = Ssl.Connection.t [@@deriving sexp_of]
    type opt = Ssl.Opt.t [@@deriving sexp]
  end
end
