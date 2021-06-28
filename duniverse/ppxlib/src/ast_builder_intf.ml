open! Import

module type Loc = sig
  val loc : Location.t
end

module type Additional_helpers = sig
  type 'a with_loc

  val eint       : (int       -> expression) with_loc
  val echar      : (char      -> expression) with_loc
  val estring    : (string    -> expression) with_loc
  val efloat     : (string    -> expression) with_loc
  val eint32     : (int32     -> expression) with_loc
  val eint64     : (int64     -> expression) with_loc
  val enativeint : (nativeint -> expression) with_loc
  val ebool      : (bool      -> expression) with_loc

  val pint       : (int       -> pattern) with_loc
  val pchar      : (char      -> pattern) with_loc
  val pstring    : (string    -> pattern) with_loc
  val pfloat     : (string    -> pattern) with_loc
  val pint32     : (int32     -> pattern) with_loc
  val pint64     : (int64     -> pattern) with_loc
  val pnativeint : (nativeint -> pattern) with_loc
  val pbool      : (bool      -> pattern) with_loc

  val eunit : expression with_loc
  val punit : pattern    with_loc

  (** [evar id] produces a [Pexp_ident _] expression, it parses its input so you can pass
      any dot-separated identifier, for instance: [evar ~loc "Foo.bar"]. *)
  val evar : (string -> expression) with_loc
  val pvar : (string -> pattern   ) with_loc

  (** Same as pexp_apply but without labels *)
  val eapply : (expression -> expression list -> expression) with_loc

  val eabstract : (pattern list -> expression -> expression) with_loc

  val esequence : (expression list -> expression) with_loc

  val ppat_tuple_opt : (pattern list -> pattern option) with_loc
  val pexp_tuple_opt : (expression list -> expression option) with_loc

  val pconstruct : constructor_declaration -> pattern    option -> pattern
  val econstruct : constructor_declaration -> expression option -> expression

  val elist : (expression list -> expression) with_loc
  val plist : (pattern    list -> pattern   ) with_loc

  val pstr_value_list :
    loc:Location.t -> Asttypes.rec_flag -> value_binding list -> structure_item list
  (** [pstr_value_list ~loc rf vbs] = [pstr_value ~loc rf vbs] if [vbs <> []], [[]]
      otherwise. *)

  val nonrec_type_declaration :
    (name:string Loc.t
     -> params:(core_type * Asttypes.variance) list
     -> cstrs:(core_type * core_type * Location.t) list
     -> kind:type_kind
     -> private_:Asttypes.private_flag
     -> manifest:core_type option
     -> type_declaration
    ) with_loc
  [@@deprecated
    "[since 2016-10] use Nonrecursive on the P(str|sig)_type instead"]

  (** [unapplied_type_constr_conv] is the standard way to map identifiers to conversion
      fonctions, for preprocessor that creates values that follow the structure of types.
      More precisely, [path_conv path (sprintf "sexp_of_%s")] is:
      - sexp_of_t if path is "t"
      - A.B.sexp_of_foo if path is "A.B.foo"
      - A.B.sexp_of_f__foo (module A1) (module A2) if path is "A.B.F(A1)(A2).foo"
      [type_constr_conv] also applies it to a list of expression, which both prevents
      the compiler from allocating useless closures, and almost always what is needed,
      since type constructors are always applied. *)
  val unapplied_type_constr_conv :
    (Longident.t Loc.t -> f:(string -> string) -> expression) with_loc
  val type_constr_conv :
    (Longident.t Loc.t -> f:(string -> string) -> expression list -> expression) with_loc

  (** Tries to simplify [fun v1 v2 .. -> f v1 v2 ..] into [f]. Only works when [f] is a
      path, not an arbitrary expression as that would change the meaning of
      the code.
      This can be used either for cleaning up the generated code, or to reduce allocation
      if [f] is a local variable (the compiler won't optimize the allocation of the
      closure).

      Eta-reduction can change the types/behavior in some corner cases that are unlikely
      to show up in generated code:
      - if [f] has optional arguments, eta-expanding [f] can drop them
      - because labels commute, it can change the type of an expression:
        $ let f ~x y = x + y
          let f2 = fun x -> add x;;
        val f  : x:int -> int -> int = <fun>
        val f2 : int -> x:int -> int = <fun>
        In fact, if [f] does side effects before receiving all its arguments, and if
        the eta-expansion is partially applied, eta-reducing could change behavior.

      [eta_reduce_if_possible_and_nonrec] is meant for the case where the resulting
      expression is going to be bound in a potentially recursive let-binding, where
      we have to keep the eta-expansion when [rec_flag] is [Recursive] to avoid
      a compile error. *)
  val eta_reduce : expression -> expression option
  val eta_reduce_if_possible : expression -> expression
  val eta_reduce_if_possible_and_nonrec : expression -> rec_flag:rec_flag -> expression
end

module type Located = sig
  type 'a with_loc

  type 'a t = 'a Loc.t

  val loc : _ t -> Location.t

  val mk : ('a -> 'a t) with_loc

  val map        : ('a -> 'b) -> 'a t -> 'b t
  val map_lident : string t -> Longident.t t

  val lident : (string -> Longident.t t) with_loc
end

type 'a without_location = 'a
type 'a with_location    = loc:Location.t -> 'a

module type S = sig
  module Located : Located
    with type 'a with_loc := 'a without_location

  include module type of Ast_builder_generated.Make(struct let loc = Location.none end)

  include Additional_helpers
    with type 'a with_loc := 'a without_location
end
