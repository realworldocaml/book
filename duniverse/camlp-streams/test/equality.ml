(* Test type equality between a library using camlp-streams and one using
   Stdlib.Stream *)

let () =
  Stream.empty Stream_stdlib.stream;
  Stream.empty Stream_camlp_streams.stream
