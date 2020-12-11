open! Core_kernel
open! Import

module type S = sig
  type t [@@deriving sexp_of]

  val all : t list
end

type 'a t = (module S with type t = 'a)

val enum : 'a t -> (string * 'a) list

(** Map a constructor name to a command-line string: downcase the name and convert [_] to
    [-]. *)
val to_string_hum : 'a t -> 'a -> string

val check_field_name : 'a t -> 'a -> (_, _, _) Field.t_with_perm -> unit
val assert_alphabetic_order_exn : Source_code_position.t -> 'a t -> unit

type ('a, 'b) make_param =
  ?represent_choice_with:string
  -> ?aliases:string list
  -> string
  -> doc:string
  -> 'a t
  -> 'b Command.Param.t

val make_param : f:('a Command.Arg_type.t -> 'b Command.Flag.t) -> ('a, 'b) make_param
val make_param_optional_with_default_doc : default:'a -> ('a, 'a) make_param
val arg_type : 'a t -> 'a Command.Arg_type.t
val doc : ?represent_choice_with:string -> 'a t -> doc:string -> string

module Make_stringable (M : S) : Stringable with type t := M.t
