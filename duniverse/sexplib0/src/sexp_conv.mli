(** Utility Module for S-expression Conversions *)

(** {6 Conversion of OCaml-values to S-expressions} *)

(** [default_string_of_float] reference to the default function used
    to convert floats to strings.

    Initially set to [fun n -> sprintf "%.20G" n]. *)
val default_string_of_float : (float -> string) ref

(** [write_old_option_format] reference for the default option format
    used to write option values.  If set to [true], the old-style option
    format will be used, the new-style one otherwise.

    Initially set to [true]. *)
val write_old_option_format : bool ref

(** [read_old_option_format] reference for the default option format
    used to read option values.  [Of_sexp_error] will be raised
    with old-style option values if this reference is set to [false].
    Reading new-style option values is always supported.  Using a global
    reference instead of changing the converter calling conventions is
    the only way to avoid breaking old code with the standard macros.

    Initially set to [true]. *)
val read_old_option_format : bool ref

(** We re-export a tail recursive map function, because some modules
    override the standard library functions (e.g. [StdLabels]) which
    wrecks havoc with the camlp4 extension. *)
val list_map : ('a -> 'b) -> 'a list -> 'b list

(** [sexp_of_unit ()] converts a value of type [unit] to an S-expression. *)
val sexp_of_unit : unit -> Sexp.t

(** [sexp_of_bool b] converts the value [x] of type [bool] to an
    S-expression. *)
val sexp_of_bool : bool -> Sexp.t

(** [sexp_of_bool str] converts the value [str] of type [string] to an
    S-expression. *)
val sexp_of_string : string -> Sexp.t

(** [sexp_of_bool str] converts the value [str] of type [bytes] to an
    S-expression. *)
val sexp_of_bytes : bytes -> Sexp.t

(** [sexp_of_char c] converts the value [c] of type [char] to an
    S-expression. *)
val sexp_of_char : char -> Sexp.t

(** [sexp_of_int n] converts the value [n] of type [int] to an
    S-expression. *)
val sexp_of_int : int -> Sexp.t

(** [sexp_of_float n] converts the value [n] of type [float] to an
    S-expression. *)
val sexp_of_float : float -> Sexp.t

(** [sexp_of_int32 n] converts the value [n] of type [int32] to an
    S-expression. *)
val sexp_of_int32 : int32 -> Sexp.t

(** [sexp_of_int64 n] converts the value [n] of type [int64] to an
    S-expression. *)
val sexp_of_int64 : int64 -> Sexp.t

(** [sexp_of_nativeint n] converts the value [n] of type [nativeint] to an
    S-expression. *)
val sexp_of_nativeint : nativeint -> Sexp.t

(** [sexp_of_ref conv r] converts the value [r] of type ['a ref] to
    an S-expression.  Uses [conv] to convert values of type ['a] to an
    S-expression. *)
val sexp_of_ref : ('a -> Sexp.t) -> 'a ref -> Sexp.t

(** [sexp_of_lazy_t conv l] converts the value [l] of type ['a lazy_t] to
    an S-expression.  Uses [conv] to convert values of type ['a] to an
    S-expression. *)
val sexp_of_lazy_t : ('a -> Sexp.t) -> 'a lazy_t -> Sexp.t

(** [sexp_of_option conv opt] converts the value [opt] of type ['a
    option] to an S-expression.  Uses [conv] to convert values of type
    ['a] to an S-expression. *)
val sexp_of_option : ('a -> Sexp.t) -> 'a option -> Sexp.t

(** [sexp_of_pair conv1 conv2 pair] converts a pair to an S-expression.
    It uses its first argument to convert the first element of the pair,
    and its second argument to convert the second element of the pair. *)
val sexp_of_pair : ('a -> Sexp.t) -> ('b -> Sexp.t) -> 'a * 'b -> Sexp.t

(** [sexp_of_triple conv1 conv2 conv3 triple] converts a triple to
    an S-expression using [conv1], [conv2], and [conv3] to convert its
    elements. *)
val sexp_of_triple
  :  ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> 'a * 'b * 'c
  -> Sexp.t

(** [sexp_of_list conv lst] converts the value [lst] of type ['a
    list] to an S-expression.  Uses [conv] to convert values of type
    ['a] to an S-expression. *)
val sexp_of_list : ('a -> Sexp.t) -> 'a list -> Sexp.t

(** [sexp_of_array conv ar] converts the value [ar] of type ['a
    array] to an S-expression.  Uses [conv] to convert values of type
    ['a] to an S-expression. *)
val sexp_of_array : ('a -> Sexp.t) -> 'a array -> Sexp.t

(** [sexp_of_hashtbl conv_key conv_value htbl] converts the value [htbl]
    of type [('a, 'b) Hashtbl.t] to an S-expression.  Uses [conv_key]
    to convert the hashtable keys of type ['a], and [conv_value] to
    convert hashtable values of type ['b] to S-expressions. *)
val sexp_of_hashtbl : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) Hashtbl.t -> Sexp.t

(** [sexp_of_opaque x] converts the value [x] of opaque type to an
    S-expression.  This means the user need not provide converters,
    but the result cannot be interpreted. *)
val sexp_of_opaque : 'a -> Sexp.t

(** [sexp_of_fun f] converts the value [f] of function type to a
    dummy S-expression.  Functions cannot be serialized as S-expressions,
    but at least a placeholder can be generated for pretty-printing. *)
val sexp_of_fun : ('a -> 'b) -> Sexp.t

(** {6 Conversion of S-expressions to OCaml-values} *)

(** [Of_sexp_error (exn, sexp)] the exception raised when an S-expression
    could not be successfully converted to an OCaml-value. *)
exception Of_sexp_error of exn * Sexp.t

(** [record_check_extra_fields] checks for extra (= unknown) fields
    in record S-expressions. *)
val record_check_extra_fields : bool ref

(** [of_sexp_error reason sexp] @raise Of_sexp_error (Failure reason, sexp). *)
val of_sexp_error : string -> Sexp.t -> 'a

(** [of_sexp_error exc sexp] @raise Of_sexp_error (exc, sexp). *)
val of_sexp_error_exn : exn -> Sexp.t -> 'a

(** [unit_of_sexp sexp] converts S-expression [sexp] to a value of type
    [unit]. *)
val unit_of_sexp : Sexp.t -> unit

(** [bool_of_sexp sexp] converts S-expression [sexp] to a value of type
    [bool]. *)
val bool_of_sexp : Sexp.t -> bool

(** [string_of_sexp sexp] converts S-expression [sexp] to a value of type
    [string]. *)
val string_of_sexp : Sexp.t -> string

(** [bytes_of_sexp sexp] converts S-expression [sexp] to a value of type
    [bytes]. *)
val bytes_of_sexp : Sexp.t -> bytes

(** [char_of_sexp sexp] converts S-expression [sexp] to a value of type
    [char]. *)
val char_of_sexp : Sexp.t -> char

(** [int_of_sexp sexp] converts S-expression [sexp] to a value of type
    [int]. *)
val int_of_sexp : Sexp.t -> int

(** [float_of_sexp sexp] converts S-expression [sexp] to a value of type
    [float]. *)
val float_of_sexp : Sexp.t -> float

(** [int32_of_sexp sexp] converts S-expression [sexp] to a value of type
    [int32]. *)
val int32_of_sexp : Sexp.t -> int32

(** [int64_of_sexp sexp] converts S-expression [sexp] to a value of type
    [int64]. *)
val int64_of_sexp : Sexp.t -> int64

(** [nativeint_of_sexp sexp] converts S-expression [sexp] to a value
    of type [nativeint]. *)
val nativeint_of_sexp : Sexp.t -> nativeint

(** [ref_of_sexp conv sexp] converts S-expression [sexp] to a value
    of type ['a ref] using conversion function [conv], which converts
    an S-expression to a value of type ['a]. *)
val ref_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a ref

(** [lazy_t_of_sexp conv sexp] converts S-expression [sexp] to a value
    of type ['a lazy_t] using conversion function [conv], which converts
    an S-expression to a value of type ['a]. *)
val lazy_t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a lazy_t

(** [option_of_sexp conv sexp] converts S-expression [sexp] to a value
    of type ['a option] using conversion function [conv], which converts
    an S-expression to a value of type ['a]. *)
val option_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a option

(** [pair_of_sexp conv1 conv2 sexp] converts S-expression [sexp] to a pair
    of type ['a * 'b] using conversion functions [conv1] and [conv2],
    which convert S-expressions to values of type ['a] and ['b]
    respectively. *)
val pair_of_sexp : (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> 'a * 'b

(** [triple_of_sexp conv1 conv2 conv3 sexp] converts S-expression [sexp]
    to a triple of type ['a * 'b * 'c] using conversion functions [conv1],
    [conv2], and [conv3], which convert S-expressions to values of type
    ['a], ['b], and ['c] respectively. *)
val triple_of_sexp
  :  (Sexp.t -> 'a)
  -> (Sexp.t -> 'b)
  -> (Sexp.t -> 'c)
  -> Sexp.t
  -> 'a * 'b * 'c

(** [list_of_sexp conv sexp] converts S-expression [sexp] to a value
    of type ['a list] using conversion function [conv], which converts
    an S-expression to a value of type ['a]. *)
val list_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a list

(** [array_of_sexp conv sexp] converts S-expression [sexp] to a value
    of type ['a array] using conversion function [conv], which converts
    an S-expression to a value of type ['a]. *)
val array_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a array

(** [hashtbl_of_sexp conv_key conv_value sexp] converts S-expression
    [sexp] to a value of type [('a, 'b) Hashtbl.t] using conversion
    function [conv_key], which converts an S-expression to hashtable
    key of type ['a], and function [conv_value], which converts an
    S-expression to hashtable value of type ['b]. *)
val hashtbl_of_sexp : (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) Hashtbl.t

(** [opaque_of_sexp sexp] @raise Of_sexp_error when attempting to
    convert an S-expression to an opaque value. *)
val opaque_of_sexp : Sexp.t -> 'a

(** [fun_of_sexp sexp] @raise Of_sexp_error when attempting to
    convert an S-expression to a function. *)
val fun_of_sexp : Sexp.t -> 'a

(** Sexp Grammars *)

include module type of struct
  include Sexp_conv_grammar
end

(** Exception converters *)

(** [sexp_of_exn exc] converts exception [exc] to an S-expression.
    If no suitable converter is found, the standard converter in
    [Printexc] will be used to generate an atomic S-expression. *)
val sexp_of_exn : exn -> Sexp.t

(** Converts an exception to a string via sexp, falling back to [Printexc.to_string] if no
    sexp conversion is registered for this exception.

    This is different from [Printexc.to_string] in that it additionally uses the sexp
    converters registered with [~printexc:false]. Another difference is that the behavior
    of [Printexc] can be overridden with [Printexc.register], but here we always try sexp
    conversion first.
*)
val printexc_prefer_sexp : exn -> string

(** [sexp_of_exn_opt exc] converts exception [exc] to [Some sexp].
    If no suitable converter is found, [None] is returned instead. *)
val sexp_of_exn_opt : exn -> Sexp.t option

module Exn_converter : sig
  val add_auto : ?finalise:bool -> exn -> (exn -> Sexp.t) -> unit
  [@@deprecated "[since 2016-07] use Conv.Exn_converter.add"]

  (** [add ?finalise constructor sexp_of_exn] registers exception S-expression
      converter [sexp_of_exn] for exceptions with the given [constructor].

      NOTE: If [finalise] is [true], then the exception will be automatically
      registered for removal with the GC (default).  Finalisation will not work
      with exceptions that have been allocated outside the heap, which is the
      case for some standard ones e.g. [Sys_error].

      @param finalise default = [true] *)
  val add
    :  ?printexc:bool
    -> ?finalise:bool
    -> extension_constructor
    -> (exn -> Sexp.t)
    -> unit

  module For_unit_tests_only : sig
    val size : unit -> int
  end
end

(**/**)

(*_ For the syntax extension *)
external ignore : _ -> unit = "%ignore"
external ( = ) : 'a -> 'a -> bool = "%equal"
