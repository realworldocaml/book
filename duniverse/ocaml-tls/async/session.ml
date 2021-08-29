open! Core
open! Async

module Fd = struct
  type t = Reader.t * Writer.t

  let read (reader, (_ : Writer.t)) buf =
    Deferred.Or_error.try_with (fun () -> Async_cstruct.read reader buf)
  ;;

  let write ((_ : Reader.t), writer) buf =
    Deferred.Or_error.try_with (fun () ->
      Async_cstruct.schedule_write writer buf;
      Writer.flushed writer)
  ;;

  let rec write_full fd buf =
    let open Deferred.Or_error.Let_syntax in
    match Cstruct.length buf with
    | 0 -> return ()
    | len ->
      let%bind () = write fd buf in
      write_full fd (Cstruct.shift buf len)
  ;;
end

include Io.Make (Fd)
