open Core
open Import
include Writer0

let of_pipe ?time_source info pipe_w =
  let%map `Reader reader_fd, `Writer writer_fd = Unix.pipe info in
  let reader = Reader.create reader_fd in
  let writer = create ?time_source writer_fd in
  if Debug.writer
  then
    Debug.log
      "Writer.of_pipe"
      (pipe_w, reader, writer)
      [%sexp_of: string Pipe.Writer.t * Reader.t * t];
  (* Shuttle bytes from [reader] to [pipe_w].  If the user calls [close writer],
     then [reader] will see EOF, which will cause [transfer] to complete.  If [pipe_w]
     is closed, then [transfer] will complete. *)
  let closed_and_flushed_downstream =
    let%bind () = Reader.transfer reader pipe_w in
    if raise_when_consumer_leaves writer && not (is_closed writer)
    then
      Monitor.send_exn
        (monitor writer)
        (Unix.Unix_error (EPIPE, "Writer.of_pipe", Sexp.to_string (Info.sexp_of_t info)));
    let%map (), () = Deferred.both (Reader.close reader) (close writer) in
    if not (Pipe.is_closed pipe_w) then Pipe.close pipe_w
  in
  writer, `Closed_and_flushed_downstream closed_and_flushed_downstream
;;

module Private = struct
  let set_bytes_received t i = t.bytes_received <- i
  let set_bytes_written t i = t.bytes_written <- i

  module Check_buffer_age = Check_buffer_age
end
