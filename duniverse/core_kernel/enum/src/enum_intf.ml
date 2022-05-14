open! Core
open! Import

module type Sexp_of = sig
  type t [@@deriving sexp_of]
end

module type Single = sig
  (** These functions take single values of ['a] instead of enumerating all of them. *)

  type 'a t

  (** Map a constructor name to a command-line string: downcase the name and convert [_] to
      [-]. *)
  val to_string_hum : 'a t -> 'a -> string

  val check_field_name : 'a t -> 'a -> (_, _, _) Field.t_with_perm -> unit
end

module type S = Command.Enumerable_sexpable

module type Enum = sig
  module type S = S

  type 'a t = (module S with type t = 'a)

  include Single with type 'a t := 'a t

  val enum : 'a t -> (string * 'a) list
  val assert_alphabetic_order_exn : Source_code_position.t -> 'a t -> unit

  type ('a, 'b) make_param =
    ?represent_choice_with:string
    -> ?list_values_in_help:bool
    -> ?aliases:string list
    -> ?key:'a Univ_map.Multi.Key.t
    -> string
    -> doc:string
    -> 'a t
    -> 'b Command.Param.t

  val make_param : f:('a Command.Arg_type.t -> 'b Command.Flag.t) -> ('a, 'b) make_param

  val make_param_one_of_flags
    :  ?if_nothing_chosen:('a, 'a) Command.Param.If_nothing_chosen.t (** Default: Raise *)
    -> ?aliases:('a -> string list)
    -> doc:('a -> string)
    -> 'a t
    -> 'a Command.Param.t

  val make_param_optional_with_default_doc : default:'a -> ('a, 'a) make_param

  val arg_type
    :  ?case_sensitive:bool
    -> ?key:'a Univ_map.Multi.Key.t
    -> ?list_values_in_help:bool
    -> 'a t
    -> 'a Command.Arg_type.t

  (** the sexp representation of M.t must be an sexp atom *)
  module Make_stringable (M : S) : Stringable with type t := M.t

  module Single : sig
    module type S = Sexp_of

    type 'a t = (module S with type t = 'a)

    include Single with type 'a t := 'a t
  end
end
