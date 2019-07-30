open Core
open Import
include Reader0
module Writer = Writer0

let of_pipe info pipe_r =
  let%map `Reader reader_fd, `Writer writer_fd = Unix.pipe info in
  let reader = create reader_fd in
  let writer =
    Writer.create
      ~buffer_age_limit:`Unlimited
      ~raise_when_consumer_leaves:false
      writer_fd
  in
  if false
  then
    Debug.log
      "Reader.of_pipe"
      (pipe_r, reader, writer)
      [%sexp_of: string Pipe.Reader.t * t * Writer.t];
  don't_wait_for
    (let%bind () =
       Writer.transfer writer pipe_r ~stop:(close_finished reader) (fun s ->
         Writer.write writer s)
     in
     Writer.close writer);
  reader
;;
