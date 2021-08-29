(** Common definitions used by binary protocol converters *)

open Bigarray

(** {2 Buffers} *)

(** Position within buffers *)
type pos = int

(** Reference to a position within buffers *)
type pos_ref = pos ref

(** Buffers *)
type buf = (char, int8_unsigned_elt, c_layout) Array1.t

(** [create_buf n] creates a buffer of size [n]. *)
val create_buf : int -> buf

(** [buf_len buf] returns the length of [buf]. *)
val buf_len : buf -> int

(** [assert_pos pos] @raise Invalid_argument if position [pos] is negative. *)
val assert_pos : pos -> unit

(** [check_pos buf pos] @raise Buffer_short if position [pos] exceeds
    the length of buffer [buf]. *)
val check_pos : buf -> pos -> unit

(** [check_next buf pos] @raise Buffer_short if the next position after
    [pos] exceeds the length of buffer [buf]. *)
val check_next : buf -> pos -> unit

(** [safe_get_pos buf pos_ref] @return the position referenced by
    [pos_ref] within buffer [buf].  @raise Buffer_short if the position
    exceeds the length of the buffer. *)
val safe_get_pos : buf -> pos_ref -> pos

(** [blit_string_buf ?src_pos src ?dst_pos dst ~len] blits [len]
    bytes of the source string [src] starting at position [src_pos]
    to buffer [dst] starting at position [dst_pos].

    @raise Invalid_argument if the designated ranges are invalid.
*)
val blit_string_buf : ?src_pos:int -> string -> ?dst_pos:int -> buf -> len:int -> unit

(** [blit_bytes_buf ?src_pos src ?dst_pos dst ~len] blits [len]
    bytes of the source byte sequence [src] starting at position [src_pos]
    to buffer [dst] starting at position [dst_pos].

    @raise Invalid_argument if the designated ranges are invalid.
*)
val blit_bytes_buf : ?src_pos:int -> bytes -> ?dst_pos:int -> buf -> len:int -> unit


(** [blit_buf_string ?src_pos src ?dst_pos dst ~len] blits [len]
    bytes of the source buffer [src] starting at position [src_pos]
    to string [dst] starting at position [dst_pos].

    @raise Invalid_argument if the designated ranges are invalid.
*)
val blit_buf_string : ?src_pos:int -> buf -> ?dst_pos:int -> bytes -> len:int -> unit

(** [blit_buf_bytes ?src_pos src ?dst_pos dst ~len] blits [len]
    bytes of the source buffer [src] starting at position [src_pos]
    to byte sequence [dst] starting at position [dst_pos].

    @raise Invalid_argument if the designated ranges are invalid.
*)
val blit_buf_bytes : ?src_pos:int -> buf -> ?dst_pos:int -> bytes -> len:int -> unit

(** [blit_buf ?src_pos ~src ?dst_pos ~dst len] blits [len] bytes of the
    source buffer [src] starting at position [src_pos] to destination
    buffer [dst] starting at position [dst_pos].

    @raise Invalid_argument if the designated ranges are invalid.
*)
val blit_buf : ?src_pos:int -> src:buf -> ?dst_pos:int -> dst:buf -> int -> unit

(** {2 Errors and exceptions} *)

(** Buffer too short for read/write operation *)
exception Buffer_short

(** Used internally for backtracking *)
exception No_variant_match

module ReadError : sig
  type t =
    | Neg_int8 (** Negative integer was positive or zero *)
    | Int_code (** Unknown integer code while reading integer *)
    | Int_overflow (** Overflow reading integer *)
    | Nat0_code (** Unknown integer code while reading natural number *)
    | Nat0_overflow (** Overflow reading natural number *)
    | Int32_code (** Unknown integer code while reading 32bit integer *)
    | Int64_code (** Unknown integer code while reading 64bit integer *)
    | Nativeint_code (** Unknown integer code while reading native integer *)
    | Unit_code (** Illegal unit value *)
    | Bool_code (** Illegal boolean value *)
    | Option_code (** Illegal option code *)
    | String_too_long (** String too long *)
    | Variant_tag (** Untagged integer encoding for variant tag *)
    | Array_too_long (** Array too long *)
    | List_too_long of
        { len : int
        ; max_len : int
        } (** List too long *)
    | Hashtbl_too_long (** Hashtable too long *)
    | Sum_tag of string (** Illegal sum tag for given type *)
    | Variant of string (** Illegal variant for given type *)
    | Poly_rec_bound of string
    (** Attempt to read data bound through polymorphic record fields *)
    | Variant_wrong_type of string
    (** Unexpected attempt to read variant with given non-variant type *)
    | Silly_type of string
    (** [Silly_type type_name] indicates unhandled but silly case
        where a type of the sort [type 'a type_name = 'a] is used
        with a polymorphic variant as type parameter and included
        in another polymorphic variant type. *)
    | Empty_type of string (** Attempt to read data that corresponds to an empty type. *)

  (** [to_string err] @return string representation of read error [err]. *)
  val to_string : t -> string
end

(** [ReadError (err, err_pos)] *)
exception Read_error of ReadError.t * pos

(** [PolyRecWrite type] gets raised when the user attempts to write or
    estimate the size of a value of a type that is bound through a
    polymorphic record field in type definition [type]. *)
exception Poly_rec_write of string

(** [EmptyType] gets raised when the user attempts to write or estimate
    the size of a value of an empty type, which would not make sense. *)
exception Empty_type of string

(** [raise_read_error err pos] *)
val raise_read_error : ReadError.t -> pos -> 'a

(** [raise_variant_wrong_type name pos] *)
val raise_variant_wrong_type : string -> pos -> 'a

(** [raise_concurrent_modification loc] @raise Failure if a binary writer
    detects a concurrent change to the underlying data structure. *)
val raise_concurrent_modification : string -> 'a

(** [array_bound_error ()] *)
val array_bound_error : unit -> 'a

(** {2 Bigarrays} *)

type vec32 = (float, float32_elt, fortran_layout) Array1.t
type vec64 = (float, float64_elt, fortran_layout) Array1.t
type vec = vec64
type mat32 = (float, float32_elt, fortran_layout) Array2.t
type mat64 = (float, float64_elt, fortran_layout) Array2.t
type mat = mat64

(** {2 Miscellaneous} *)

(** [copy_htbl_list htbl lst] adds all [(key, value)] pairs in [lst]
    to hash table [htbl]. *)
val copy_htbl_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list -> ('a, 'b) Hashtbl.t

(** {2 NOTE: unsafe functions!!!} *)

external unsafe_blit_buf
  :  src_pos:int
  -> src:buf
  -> dst_pos:int
  -> dst:buf
  -> len:int
  -> unit
  = "bin_prot_blit_buf_stub"

external unsafe_blit_string_buf
  :  src_pos:int
  -> string
  -> dst_pos:int
  -> buf
  -> len:int
  -> unit
  = "bin_prot_blit_string_buf_stub"
[@@noalloc]

external unsafe_blit_bytes_buf
  :  src_pos:int
  -> bytes
  -> dst_pos:int
  -> buf
  -> len:int
  -> unit
  = "bin_prot_blit_bytes_buf_stub"
[@@noalloc]

external unsafe_blit_buf_string
  :  src_pos:int
  -> buf
  -> dst_pos:int
  -> bytes
  -> len:int
  -> unit
  = "bin_prot_blit_buf_bytes_stub"
[@@noalloc]

external unsafe_blit_buf_bytes
  :  src_pos:int
  -> buf
  -> dst_pos:int
  -> bytes
  -> len:int
  -> unit
  = "bin_prot_blit_buf_bytes_stub"
[@@noalloc]

external unsafe_blit_float_array_buf
  :  src_pos:int
  -> float array
  -> dst_pos:int
  -> buf
  -> len:int
  -> unit
  = "bin_prot_blit_float_array_buf_stub"
[@@noalloc]

external unsafe_blit_buf_float_array
  :  src_pos:int
  -> buf
  -> dst_pos:int
  -> float array
  -> len:int
  -> unit
  = "bin_prot_blit_buf_float_array_stub"
[@@noalloc]

val ( + ) : int -> int -> int
