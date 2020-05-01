(** A rope is a standard data structure that represents a single string as a tree of
    strings, allowing concatenation to do no work up front. See the README.md file for
    details and motivating examples. *)

open! Core_kernel

type t [@@deriving equal, quickcheck, sexp]

(** Takes O(1) time. The string isn't copied, so don't mutate it. *)
val of_string : string -> t

val empty : t
val is_empty : t -> bool
val length : t -> int

(** Allocates a fresh string, so takes time proportional to the total
    size of the result. *)
val to_string : t -> string


(** [to_char_sequence] can often produce characters incrementally, but in the worst case
    it takes time and memory proportional to the total length of the string to produce
    even a single character. (In such cases, it should still only take O(length) time to
    produce the rest of the string.) *)
val to_char_sequence : t -> char Sequence.t

(** These take time proportional to the number of [t]'s passed. *)
val ( ^ ) : t -> t -> t

val concat : ?sep:t -> t list -> t
val concat_array : ?sep:t -> t array -> t

(** Appends the contents of the Rope at the end of a destination buffer. *)
val add_to_buffer : t -> Buffer.t -> unit

(** [is_prefix a ~prefix:b] is a more efficient version of
    [String.is_prefix (Rope.to_string a) ~prefix:(Rope.to_string b)].
    However, the worst-case time complexity is still [O(length t)]
    instead of [O(min(length t, length prefix))] as one could expect. *)
val is_prefix : t -> prefix:t -> bool

module For_testing : sig
  val sexp_of_t : t -> Sexp.t
  val num_bases : t -> int
  val to_string_tailcall : t -> string
end
