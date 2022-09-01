(** {2 JSON writers} *)

val to_string :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  t -> string
  (** Write a compact JSON value to a string.
      @param buf allows to reuse an existing buffer created with
      [Buffer.create]. The buffer is cleared of all contents
      before starting and right before returning.
      @param len initial length of the output buffer.
      @param suf appended to the output as a suffix,
      defaults to empty string.
      @param std use only standard JSON syntax,
      i.e. convert tuples and variants into standard JSON (if applicable),
      refuse to print NaN and infinities,
      require the root node to be either an object or an array.
      Default is [false].
      @raise Json_error if [float] value is not allowed in standard JSON.
  *)

val to_channel :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  out_channel -> t -> unit
  (** Write a compact JSON value to a channel.
      Note: the [out_channel] is not flushed by this function.

      See [to_string] for the role of the optional arguments and raised exceptions. *)

val to_output :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  < output : string -> int -> int -> int; .. > -> t -> unit
  (** Write a compact JSON value to an OO channel.

      See [to_string] for the role of the optional arguments and raised exceptions. *)

val to_file :
  ?len:int ->
  ?std:bool ->
  ?suf:string ->
  string -> t -> unit
  (** Write a compact JSON value to a file.
      See [to_string] for the role of the optional arguments and raised exceptions.
      @param suf is a suffix appended to the output Newline by default
      for POSIX compliance. *)

val to_buffer :
  ?suf:string ->
  ?std:bool ->
  Buffer.t -> t -> unit
  (** Write a compact JSON value to an existing buffer.
      See [to_string] for the role of the optional argument and raised exceptions. *)

val seq_to_string :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  t Seq.t -> string
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a string.
      @param suf is the suffix ouf each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val seq_to_channel :
  ?buf:Buffer.t ->
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  out_channel -> t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a channel.
      @param suf is the suffix of each value written. Newline by default.
      See [to_channel] for the role of the optional arguments and raised exceptions. *)

val seq_to_file :
  ?len:int ->
  ?suf:string ->
  ?std:bool ->
  string -> t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      a file.
      @param suf is the suffix of each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val seq_to_buffer :
  ?suf:string ->
  ?std:bool ->
  Buffer.t ->
  t Seq.t -> unit
  (** Write a sequence of [suf]-suffixed compact one-line JSON values to
      an existing buffer.
      @param suf is the suffix of each value written. Newline by default.
      See [to_string] for the role of the optional arguments and raised exceptions. *)

val write_t : Buffer.t -> t -> unit
(** Write the given JSON value to the given buffer.
    Provided as a writer function for atdgen.
*)

(** {2 Miscellaneous} *)

val sort : t -> t
  (** Sort object fields (stable sort, comparing field names
      and treating them as byte sequences) *)


(**/**)
(* begin undocumented section *)

val write_null : Buffer.t -> unit -> unit
val write_bool : Buffer.t -> bool -> unit
#ifdef INT
val write_int : Buffer.t -> int -> unit
#endif
#ifdef FLOAT
val write_float : Buffer.t -> float -> unit
val write_std_float : Buffer.t -> float -> unit
val write_float_prec : int -> Buffer.t -> float -> unit
val write_std_float_prec : int -> Buffer.t -> float -> unit
#endif
#ifdef STRING
val write_string : Buffer.t -> string -> unit
#endif

#ifdef INTLIT
val write_intlit : Buffer.t -> string -> unit
#endif
#ifdef FLOATLIT
val write_floatlit : Buffer.t -> string -> unit
#endif
#ifdef STRINGLIT
val write_stringlit : Buffer.t -> string -> unit
#endif

val write_assoc : Buffer.t -> (string * t) list -> unit
val write_list : Buffer.t -> t list -> unit
#ifdef TUPLE
val write_tuple : Buffer.t -> t list -> unit
val write_std_tuple : Buffer.t -> t list -> unit
#endif
#ifdef VARIANT
val write_variant : Buffer.t -> string -> t option -> unit
val write_std_variant : Buffer.t -> string -> t option -> unit
#endif

val write_json : Buffer.t -> t -> unit
val write_std_json : Buffer.t -> t -> unit

(* end undocumented section *)
(**/**)
