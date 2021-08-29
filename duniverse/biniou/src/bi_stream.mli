(** Streaming utilities (experimental) *)

(**
   This module offers a streaming interface for representing long lists
   of elements that cannot fit in memory.
   Stream items are serialized as chunks of configurable length.

   Stream format (independent from the biniou serialization format):

{v
  ( ONE INT64 BYTE* )* ZERO
v}

  where [INT64] is the length of a chunk (unsigned big-endian 64-bit int),
  i.e. the number of following [BYTE]s.
  [ONE] and [ZERO] are the single-byte representations of 1 and 0 and are used
  to indicate whether the end of the stream is reached.
*)

val read_stream : (string -> 'a array) -> in_channel -> 'a Stream.t
  (** [read_stream of_string ic] creates an OCaml stream from
      an input channel [ic]. The data come in chunks and each chunk
      is converted from a string to an array by calling [of_string]. *)

val write_stream :
  ?chunk_len:int ->
  ('a array -> string) -> out_channel -> 'a Stream.t -> unit
  (** [write_stream to_string oc st] writes an OCaml stream to the
      output channel [oc]. It creates chunks of [chunk_len],
      except for the last chunk which is usually smaller.
      @param chunk_len  has a default value of 1024. The limit
                        supported by this OCaml implementation on 32-bit
                        platforms is 16777215.
  *)

(**/**)

val test : int list -> bool
