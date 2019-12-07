(** Input buffer *)

type t = {
  mutable i_s : bytes;
    (** This is the buffer string.
       It can be accessed for reading but should normally only
       be written to or replaced only by the [i_refill] function.
    *)

  mutable i_pos : int;
    (** This is the current position in the input buffer.
       All data before that may be erased at anytime. *)

  mutable i_len : int;
    (**
      This is the position of the first byte of invalid input data.
      Data starting at [i_pos] and ending at [i_len-1] is considered
      valid input data that is available to the user.
      Beware that calls to [try_preread], [read] and other read functions
      may move data around and therefore modify the values
      of [i_pos] and [i_len] in order to keep pointing to the
      correct data segment.
    *)

  mutable i_offs : int;
    (** Length of data read and discarded from the buffer.
        This indicates the position in the input stream of
        the first byte of the buffer.
        The current position in the input stream is [i_offs + i_pos].
        The total length of input stream data put into the buffer is
        [i_offs + i_len].
    *)

  mutable i_max_len : int;
    (** This is the length of [i_s]. *)

  i_refill : t -> int -> unit;
    (**
      Function called when not enough data is available in the buffer.
      The int argument is the total number of bytes that must be
      available starting at position [i_pos] when the function returns.
      This function typically does nothing if all input data already has been
      placed into the buffer.
      The [i_pos] and [i_len] fields can be modified the [i_refill] function,
      as long as the available data that was starting from [i_pos]
      still starts from the new value of [i_pos].
      All the other fields can be modified as well.
    *)

  i_shared : Bi_share.Rd.tbl;
    (**
       Hash table used to map positions in the input stream to
       shared objects (if any).
    *)
}

exception End_of_input
  (**
     Exception raised by all the functions of this module
     when it is not possible to return a valid result
     because there is not enough data to read from the buffer.
  *)

val try_preread : t -> int -> int
  (**
     [try_preread ib n] make at least [n] bytes available for reading
     in [ib.i_s], unless the end of the input is reached.
     The result indicates how many bytes were made available. If smaller than
     [n], the result indicates that the end of the input was reached.
     [ib.i_pos] is set to point to the first available byte.
  *)

val read : t -> int -> int
  (**
     [read ib n] makes at least [n] bytes available for reading or raises
     the [End_of_input] exception.
     The result is the position of the first available byte.
     [ib.i_pos] is moved to point to the next position after the [n] bytes.
     @raise End_of_input if there is less than [n] bytes
     before the end of input.
  *)

val read_char : t -> char
  (**
    Read just one byte.
    @raise End_of_input if the end of input has already been reached.
  *)

val peek : t -> char
  (**
    Return the next byte without moving forward.
    @raise End_of_input if the end of input has already been reached.
  *)

val from_string : ?pos:int -> ?shrlen:int -> string -> t
  (**
     Create an input buffer from a string.
     @param pos     position to start from. Default: 0.
     @param shrlen  initial length of the table used to store shared values.
  *)

val from_bytes : ?pos:int -> ?shrlen:int -> bytes -> t
  (**
     Create an input buffer from bytes.
     @param pos     position to start from. Default: 0.
     @param shrlen  initial length of the table used to store shared values.
     @since 1.2.0
  *)

val from_channel : ?len:int -> ?shrlen:int -> in_channel -> t
  (**
     Create an input buffer from an in_channel.
     Such a buffer is not extensible and [read] requests may not exceed [len].
     @param len     buffer length.
     @param shrlen  initial length of the table used to store shared values.
  *)
