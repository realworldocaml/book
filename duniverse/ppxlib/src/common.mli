open! Import

val lident : string -> Longident.t
val core_type_of_type_declaration : type_declaration -> core_type

val name_type_params_in_td_res :
  type_declaration -> (type_declaration, Location.Error.t NonEmptyList.t) result

val name_type_params_in_td : type_declaration -> type_declaration

val combinator_type_of_type_declaration :
  type_declaration -> f:(loc:Location.t -> core_type -> core_type) -> core_type

val gen_symbol : ?prefix:string -> unit -> string
(** [gen_symbol ?prefix ()] generates a fresh variable name with [prefix].

    @param prefix default = "_x" *)

val string_of_core_type : core_type -> string
val assert_no_attributes : attributes -> unit
val assert_no_attributes_in : Ast_traverse0.iter
val attributes_errors : attributes -> Location.Error.t list
val collect_attributes_errors : Location.Error.t list Ast_traverse0.fold

val get_type_param_name_res :
  core_type * (variance * injectivity) ->
  (string Loc.t, Location.Error.t NonEmptyList.t) result
(** [get_type_param_name_res tp] returns the string identifier associated with
    [tp] if it is a type parameter, as a result. *)

val get_type_param_name : core_type * (variance * injectivity) -> string Loc.t
(** See {!get_type_param_name_res}. Raises a located error in case of failure. *)

(** [(new type_is_recursive rec_flag tds)#go ()] returns whether [rec_flag, tds]
    is really a recursive type. We disregard recursive occurrences appearing in
    arrow types. You can override the search for certain type expressions by
    inheriting from this class. *)
class type_is_recursive :
  rec_flag
  -> type_declaration list
  -> object
       inherit Ast_traverse0.iter
       val type_names : string list
       method return_true : unit -> unit
       method go : unit -> rec_flag
     end

val really_recursive : rec_flag -> type_declaration list -> rec_flag
(** [really_recursive rec_flag tds = (new type_is_recursive rec_flag tds)#go ()] *)

val loc_of_payload : attribute -> Location.t
val loc_of_attribute : attribute -> Location.t
val loc_of_extension : extension -> Location.t

val curry_applications : expression -> expression
(** convert multi-arg function applications into a cascade of 1-arg applications *)

val attribute_of_warning : Location.t -> string -> attribute
(** Encode a warning message into an 'ocaml.ppwarning' attribute which can be
    inserted in a generated Parsetree. The compiler will be responsible for
    reporting the warning. *)

val is_polymorphic_variant :
  type_declaration -> sig_:bool -> [> `Definitely | `Maybe | `Surely_not ]

val mk_named_sig :
  loc:Location.t ->
  sg_name:string ->
  handle_polymorphic_variant:bool ->
  type_declaration list ->
  include_description option
(** [mk_named_sig ~loc ~sg_name:"Foo" ~handle_polymorphic_variant tds] will
    generate

    {[
      include Foo (* or Foo1, Foo2, Foo3 *)
        with type (* ('a, 'b, 'c) *) t := (* ('a, 'b, 'c) *) t
    ]}

    when:

    - there is only one type declaration
    - the type is named t
    - there are less than 4 type parameters
    - there are no constraints on the type parameters

    It will take care of giving fresh names to unnamed type parameters. *)

module With_errors : sig
  type 'a t = 'a * Location.Error.t list

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val of_result :
    ('a, Location.Error.t NonEmptyList.t) result -> default:'a -> 'a t

  val combine_errors : 'a t list -> 'a list t
end
