open Core

module V1 = struct
  module Ssl = struct
    module Config = struct
      type t = [`Ssl_not_compiled_in] [@@deriving sexp]

      let verify_certificate _ =
        failwith "Ssl not available, recompile with Async_ssl"

      let create ?version:_ ?name:_ ?ca_file:_ ?ca_path:_ ?session:_ ?verify:_
          () = failwith "Ssl not available, recompile with Async_ssl"
    end

    let connect _cfg _r _w =
      failwith "Ssl not available, recompile with Async_ssl"

    let listen ?version:_ ?ca_file:_ ?ca_path:_ ~crt_file:_ ~key_file:_ _ _ =
      failwith "Ssl not available, recompile with Async_ssl"

    type session = [`Ssl_not_compiled_in] [@@deriving sexp]
    type version = [`Ssl_not_compiled_in] [@@deriving sexp]
    type connection = [`Ssl_not_compiled_in] [@@deriving sexp]
  end
end

module V2 = struct
  module Ssl = struct
    module Config = struct
      type t = [`Ssl_not_compiled_in] [@@deriving sexp]

      let verify_certificate _ =
        failwith "Ssl not available, recompile with Async_ssl"

      let create
          ?version:_
          ?options:_
          ?name:_
          ?hostname:_
          ?allowed_ciphers:_
          ?ca_file:_
          ?ca_path:_
          ?crt_file:_
          ?key_file:_
          ?session:_
          ?verify_modes:_
          ?verify:_
          () =
        failwith "Ssl not available, recompile with Async_ssl"
    end

    let connect ?cfg:_ _r _w =
      failwith "Ssl not available, recompile with Async_ssl"

    let listen _ _r _w =
      failwith "Ssl not available, recompile with Async_ssl"

    type version = [`Ssl_not_compiled_in] [@@deriving sexp]
    type session = [`Ssl_not_compiled_in] [@@deriving sexp]
    type verify = [`Ssl_not_compiled_in] [@@deriving sexp]
    type connection = [`Ssl_not_compiled_in] [@@deriving sexp]
    type verify_mode = [`Ssl_not_compiled_in] [@@deriving sexp]
    type opt = [`Ssl_not_compiled_in] [@@deriving sexp]
  end
end
