open Core
open Poly
open Async

let sexps =
  let a x = Sexp.Atom x in
  let l x = Sexp.List x in
  [ a ""; a "hello"; l []; l [ a "" ]; l [ a ""; a "hello" ] ]
;;

let test () =
  Unix.pipe (Info.of_string "unpack_sequence_test")
  >>= fun (`Reader reader_fd, `Writer writer_fd) ->
  let string_writer = Writer.create writer_fd in
  let sexp_reader, unpack_result =
    Unpack_sequence.unpack_into_pipe
      ~from:(Reader (Reader.create reader_fd))
      ~using:(Unpack_buffer.create Unpack_buffer.Unpack_one.sexp)
  in
  (* write all the sexps at a single go. *)
  List.iter sexps ~f:(fun sexp -> Writer.write string_writer (Sexp.to_string sexp));
  (* write all the sexps, one ... character ... at ... a ... time *)
  let rec loop_sexps sexps =
    match sexps with
    | [] -> don't_wait_for (Writer.close string_writer)
    | sexp :: sexps ->
      let packed = Sexp.to_string sexp in
      let rec loop_bytes i =
        if i = String.length packed
        then loop_sexps sexps
        else (
          Writer.write string_writer (String.sub packed ~pos:i ~len:1);
          after (sec 0.001) >>> fun () -> loop_bytes (i + 1))
      in
      loop_bytes 0
  in
  loop_sexps sexps;
  let all = Pipe.read_all sexp_reader in
  unpack_result
  >>= fun result ->
  (match result with
   | Input_closed -> ()
   | r -> Error.raise (Unpack_sequence.Unpack_result.to_error r));
  all
  >>= fun all ->
  assert (sexps @ sexps = Queue.to_list all);
  Fd.close reader_fd
;;

let tests = [ "Unpack_Sequence_test", test ]
