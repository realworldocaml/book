(** Input and output functions for the Biniou serialization format *)

(** {1 Node tags} *)

type node_tag = int

val bool_tag : node_tag (** Tag indicating a bool node. *)
val int8_tag : node_tag (** Tag indicating an int8 node. *)
val int16_tag : node_tag (** Tag indicating an int16 node. *)
val int32_tag : node_tag (** Tag indicating an int32 node. *)
val int64_tag : node_tag (** Tag indicating an int64 node. *)
val float32_tag : node_tag (** Tag indicating a float32 node. *)
val float64_tag : node_tag (** Tag indicating a float64 node. *)
val uvint_tag : node_tag (** Tag indicating a uvint node. *)
val svint_tag : node_tag (** Tag indicating an svint node. *)
val string_tag : node_tag (** Tag indicating a string node. *)
val array_tag : node_tag (** Tag indicating an array node. *)
val tuple_tag : node_tag (** Tag indicating a tuple node. *)
val record_tag : node_tag (** Tag indicating a record node. *)
val num_variant_tag : node_tag (** Tag indicating a num_variant node. *)
val variant_tag : node_tag (** Tag indicating a variant node. *)
val unit_tag : node_tag (** Tag indicating a unit node. *)
val table_tag : node_tag (** Tag indicating a table node. *)
val shared_tag : node_tag (** Tag indicating a shared node. *)

val write_tag : Bi_outbuf.t -> node_tag -> unit
  (** Write one-byte tag to a buffer. *)

val read_tag : Bi_inbuf.t -> node_tag
  (** Read one-byte tag from a buffer. *)


(** {1 Tags of variants and record fields} *)

type hash = int (** 31-bit hash *)

val hash_name : string -> hash
  (** Hash function used to compute field name tags and variant tags from
      their full name. *)

val write_hashtag : Bi_outbuf.t -> hash -> bool -> unit
  (** [write_hashtag ob h has_arg] writes variant tag [h] to buffer [ob].
      [has_arg] indicates whether the variant has an argument.
      This function can be used for record field names as well,
      in which case [has_arg] may only be [true]. *)

val string_of_hashtag : hash -> bool -> string
  (** Same as [write_hashtag] but writes to a string. *)

val read_hashtag :
  Bi_inbuf.t ->
  (Bi_inbuf.t -> hash -> bool -> 'a) -> 'a
  (** [read_hashtag ib f] reads a variant tag as hash [h] and flag [has_arg]
      and returns [f h has_arg]. *)

val read_field_hashtag : Bi_inbuf.t -> hash
  (** [read_field_hashtag ib] reads a field tag and returns the 31-bit hash. *)

val make_unhash : string list -> (hash -> string option)
  (** Compute the hash of each string of the input list
      and return a function that converts a hash back
      to the original string. Lookups do not allocate memory blocks.
      @raise Failure if the input list contains two different strings
      with the same hash.
  *)

type int7 = int
    (** 7-bit int used to represent a num_variant tag. *)

val write_numtag : Bi_outbuf.t -> int7 -> bool -> unit
  (** [write_numtag ob i has_arg] writes the tag of a num_variant.
      The tag name is represented by [i] which must be within \[0, 127\]
      and the flag [has_arg] which indicates the presence of an argument. *)

val read_numtag :
  Bi_inbuf.t ->
  (Bi_inbuf.t -> int7 -> bool -> 'a) -> 'a
  (** [read_numtag ib f] reads a num_variant tag
      and processes the tag name [i] and flag [has_arg]
      using [f]. *)


(** {1 Atom writers} *)

(** The [write_untagged_] functions write an untagged value (VAL)
    to an output buffer
    while the other [write_] functions write a tagged value (TAGVAL). *)

val write_untagged_unit : Bi_outbuf.t -> unit -> unit
val write_untagged_bool : Bi_outbuf.t -> bool -> unit
val write_untagged_char : Bi_outbuf.t -> char -> unit
val write_untagged_int8 : Bi_outbuf.t -> int -> unit
val write_untagged_int16 : Bi_outbuf.t -> int -> unit
val write_untagged_int32 : Bi_outbuf.t -> int32 -> unit
val write_untagged_int64 : Bi_outbuf.t -> int64 -> unit
val write_untagged_float32 : Bi_outbuf.t -> float -> unit
val write_untagged_float64 : Bi_outbuf.t -> float -> unit
val write_untagged_string : Bi_outbuf.t -> string -> unit
val write_untagged_uvint : Bi_outbuf.t -> int -> unit
val write_untagged_svint : Bi_outbuf.t -> int -> unit

val write_unit : Bi_outbuf.t -> unit -> unit
val write_bool : Bi_outbuf.t -> bool -> unit
val write_char : Bi_outbuf.t -> char -> unit
val write_int8 : Bi_outbuf.t -> int -> unit
val write_int16 : Bi_outbuf.t -> int -> unit
val write_int32 : Bi_outbuf.t -> int32 -> unit
val write_int64 : Bi_outbuf.t -> int64 -> unit
val write_float32 : Bi_outbuf.t -> float -> unit
val write_float64 : Bi_outbuf.t -> float -> unit
val write_string : Bi_outbuf.t -> string -> unit
val write_uvint : Bi_outbuf.t -> int -> unit
val write_svint : Bi_outbuf.t -> int -> unit

(** {1 Atom readers} *)

(** The [read_untagged_] functions read an untagged value (VAL)
    from an input buffer. *)

val read_untagged_unit : Bi_inbuf.t -> unit
val read_untagged_bool : Bi_inbuf.t -> bool
val read_untagged_char : Bi_inbuf.t -> char
val read_untagged_int8 : Bi_inbuf.t -> int
val read_untagged_int16 : Bi_inbuf.t -> int
val read_untagged_int32 : Bi_inbuf.t -> int32
val read_untagged_int64 : Bi_inbuf.t -> int64
val read_untagged_float32 : Bi_inbuf.t -> float
val read_untagged_float64 : Bi_inbuf.t -> float
val read_untagged_string : Bi_inbuf.t -> string
val read_untagged_uvint : Bi_inbuf.t -> int
val read_untagged_svint : Bi_inbuf.t -> int

val skip : Bi_inbuf.t -> unit
  (** Read and discard a value. Useful for skipping unknown record fields. *)


(** {1 Generic tree} *)

type tree =
    [
    | `Unit
    | `Bool of bool
    | `Int8 of char
    | `Int16 of int
    | `Int32 of Int32.t
    | `Int64 of Int64.t
    | `Float32 of float
    | `Float64 of float
    | `Uvint of int
    | `Svint of int
    | `String of string
    | `Array of (node_tag * tree array) option
    | `Tuple of tree array
    | `Record of (string option * hash * tree) array
    | `Num_variant of (int * tree option)
    | `Variant of (string option * hash * tree option)
    | `Table of
	((string option * hash * node_tag) array * tree array array) option
    | `Shared of tree ]
  (** Tree representing serialized data, useful for testing
      and for untyped transformations. *)

val write_tree : Bi_outbuf.t -> tree -> unit
  (** Serialization of a tree to a buffer. *)

val string_of_tree : tree -> string
  (** Serialization of a tree into a string. *)

val read_tree : ?unhash:(hash -> string option) -> Bi_inbuf.t -> tree
  (** Deserialization of a tree from a buffer. *)

val tree_of_string : ?unhash:(hash -> string option) -> string -> tree
  (** Deserialization of a tree from a string. *)

val tag_of_tree : tree -> node_tag
  (** Returns the node tag of the given tree. *)


val view_of_tree : tree -> string
val view :
  ?unhash:(hash -> string option) -> string -> string
  (** Prints a human-readable representation of the data into a string. *)

val print_view_of_tree : tree -> unit
val print_view :
  ?unhash:(hash -> string option) -> string -> unit
  (** Prints a human-readable representation of the data to stdout. *)

val output_view_of_tree : out_channel -> tree -> unit
val output_view :
  ?unhash:(hash -> string option) -> out_channel -> string -> unit
  (** Prints a human-readable representation of the data to an out_channel. *)

val safety_test : unit -> unit
  (** Check that certain low-level hacks work as expected *)
