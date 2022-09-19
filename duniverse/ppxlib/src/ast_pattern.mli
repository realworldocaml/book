(** First class AST patterns *)

open! Import

(** PPX rewriters often need to recognize fragments the OCaml AST, for instance
    to parse the payload of an attribute/expression. You can do that with a
    pattern matching and manual error reporting when the input is not what you
    expect but this has proven to quickly become extremely verbose and
    unreadable.

    This module aims to help with that by providing first class AST patterns.

    To understand how to use it, let's consider the example of ppx_inline_test.
    We want to recognize patterns of the form:

    {[ let%test "name" = expr ]}

    Which is a syntactic sugar for:

    {[ [%%test let "name" = expr] ]}

    If we wanted to write a function that recognizes the payload of [%%test]
    using normal pattern matching we would write:

    {[
      let match_payload = function
        | Pstr [ { pstr_desc = Pstr_value (Nonrecursive,
                                           [ { pvb_pat = Ppat_constant (Constant_string
                                                                          (name, None))
                                             ; pvb_expr = e
                                             ; _ } ])
                 ; _ } ] ->
          (name, e)
        | _ -> Location.raisef ...
    ]}

    This is quite cumbersome, and this is still not right: this function drops
    all attributes without notice.

    Now let's imagine we wanted to construct the payload instead, using
    [Ast_builder] one would write:

    {[
      let build_payload ~loc name expr =
        let (module B) = Ast_builder.with_loc loc in
        let open B in
        pstr
          [ pstr_value Nonrecursive (value_binding ~pat:(pstring name) ~expr) ]
    ]}

    Constructing a first class pattern is almost as simple as replacing
    [Ast_builder] by [Ast_pattern]:

    {[
      let payload_pattern name expr =
        let open Ast_pattern in
        pstr
          (pstr_value nonrecursive (value_binding ~pat:(pstring __) ~expr:__)
          ^:: nil)
    ]}

    Notice that the place-holders for [name] and [expr] have been replaced by
    [__]. The following pattern with have type:

    {[ (payload, string -> expression -> 'a, 'a) Ast_pattern.t ]}

    which means that it matches values of type [payload] and captures a string
    and expression from it. The two captured elements comes from the use of
    [__].

    An empty payload (e.g. for an attribute that has no payload) is matched by
    [Ast_pattern.(pstr nil)]. A payload with exactly one expression (e.g. to
    specify a custom function in a deriver) is matched by
    [Ast_pattern.(single_expr_payload __)]. *)

type ('a, 'b, 'c) t = ('a, 'b, 'c) Ast_pattern0.t
(** Type of a pattern:

    - ['a] is the type of value matched by the pattern
    - ['b] is the continuation, for instance for a pattern that captures an
      [int] and a [string], ['b] will be [int -> string -> _]
    - ['c] is the result of the continuation. *)

val parse :
  ('a, 'b, 'c) t -> Location.t -> ?on_error:(unit -> 'c) -> 'a -> 'b -> 'c
(** Matches a value against a pattern. Raise a located error in case of failure. *)

val parse_res :
  ('a, 'b, 'c) t ->
  Location.t ->
  ?on_error:(unit -> 'c) ->
  'a ->
  'b ->
  ('c, Location.Error.t NonEmptyList.t) result
(** Matches a value against a pattern and return a result. *)

module Packed : sig
  type ('a, 'b, 'c) pattern = ('a, 'b, 'c) t
  type ('a, 'b) t

  val create : ('a, 'b, 'c) pattern -> 'b -> ('a, 'c) t
  val parse : ('a, 'b) t -> Location.t -> 'a -> 'b

  val parse_res :
    ('a, 'b) t ->
    Location.t ->
    'a ->
    ('b, Location.Error.t NonEmptyList.t) result
end
with type ('a, 'b, 'c) pattern := ('a, 'b, 'c) t

val __ : ('a, 'a -> 'b, 'b) t
(** Pattern that captures its input. *)

val __' : ('a, 'a Loc.t -> 'b, 'b) t
(** Same as [__] but also captures the location.

    Note: this should only be used for types that do not embed a location. For
    instance you can use it to capture a string constant:

    {[ estring __' ]}

    but using it to capture an expression would not yield the expected result:

    {[ pair (eint (int 42)) __' ]}

    In the latter case you should use the [pexp_loc] field of the captured
    expression instead. *)

val drop : ('a, 'b, 'b) t
(** Useful when some part of the AST is irrelevant. With [__], the captured
    value is passed to the continuation, with [drop] it is ignored. In
    higher-level pattern matching, it is called wildcard pattern. *)

val as__ : ('a, 'b, 'c) t -> ('a, 'a -> 'b, 'c) t
(** As-pattern. Passes the current node to the continuation.

    Pitfall. In general, the continuation is called step by step by being
    applied partially to every next captured node in the pattern. That means
    that the node captured by [as__] is passed to the continuation before
    checking if the pattern is matched. *)

val alt : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
(** [alt] stands for `alternatives'. It matches either the first pattern or the
    second one. *)

val alt_option :
  ('a, 'v -> 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'v option -> 'b, 'c) t
(** Same as [alt], for the common case where the left-hand-side captures a value
    but not the right-hand-side. *)

val ( ||| ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
(** Same as [alt] *)

val map : ('a, 'b, 'c) t -> f:('d -> 'b) -> ('a, 'd, 'c) t
val map' : ('a, 'b, 'c) t -> f:(Location.t -> 'd -> 'b) -> ('a, 'd, 'c) t
val map_result : ('a, 'b, 'c) t -> f:('c -> 'd) -> ('a, 'b, 'd) t

val ( >>| ) : ('a, 'b, 'c) t -> ('d -> 'b) -> ('a, 'd, 'c) t
(** Same as [map] *)

val map0 : ('a, 'b, 'c) t -> f:'v -> ('a, 'v -> 'b, 'c) t
val map1 : ('a, 'v1 -> 'b, 'c) t -> f:('v1 -> 'v) -> ('a, 'v -> 'b, 'c) t

val map2 :
  ('a, 'v1 -> 'v2 -> 'b, 'c) t -> f:('v1 -> 'v2 -> 'v) -> ('a, 'v -> 'b, 'c) t

val map0' : ('a, 'b, 'c) t -> f:(Location.t -> 'v) -> ('a, 'v -> 'b, 'c) t

val map1' :
  ('a, 'v1 -> 'b, 'c) t -> f:(Location.t -> 'v1 -> 'v) -> ('a, 'v -> 'b, 'c) t

val map2' :
  ('a, 'v1 -> 'v2 -> 'b, 'c) t ->
  f:(Location.t -> 'v1 -> 'v2 -> 'v) ->
  ('a, 'v -> 'b, 'c) t

val nil : (_ list, 'a, 'a) t
val ( ^:: ) : ('a, 'b, 'c) t -> ('a list, 'c, 'd) t -> ('a list, 'b, 'd) t
val many : ('a, 'b -> 'c, 'c) t -> ('a list, 'b list -> 'c, 'c) t
val int : int -> (int, 'a, 'a) t
val char : char -> (char, 'a, 'a) t
val string : string -> (string, 'a, 'a) t
val float : float -> (float, 'a, 'a) t
val int32 : int32 -> (int32, 'a, 'a) t
val int64 : int64 -> (int64, 'a, 'a) t
val nativeint : nativeint -> (nativeint, 'a, 'a) t
val bool : bool -> (bool, 'a, 'a) t

val cst :
  to_string:('a -> string) -> ?equal:('a -> 'a -> bool) -> 'a -> ('a, 'b, 'b) t

val none : (_ option, 'a, 'a) t
val some : ('a, 'b, 'c) t -> ('a option, 'b, 'c) t
val pair : ('a1, 'b, 'c) t -> ('a2, 'c, 'd) t -> ('a1 * 'a2, 'b, 'd) t
val ( ** ) : ('a1, 'b, 'c) t -> ('a2, 'c, 'd) t -> ('a1 * 'a2, 'b, 'd) t

val triple :
  ('a1, 'b, 'c) t ->
  ('a2, 'c, 'd) t ->
  ('a3, 'd, 'e) t ->
  ('a1 * 'a2 * 'a3, 'b, 'e) t

val loc : ('a, 'b, 'c) t -> ('a Loc.t, 'b, 'c) t
val pack0 : ('a, 'b, 'c) t -> ('a, unit -> 'b, 'c) t
val pack2 : ('a, 'b -> 'c -> 'd, 'e) t -> ('a, 'b * 'c -> 'd, 'e) t
val pack3 : ('a, 'b -> 'c -> 'd -> 'e, 'f) t -> ('a, 'b * 'c * 'd -> 'e, 'f) t

include module type of Ast_pattern_generated
(** AST patterns for each constructor/record of the parsetree are generated in
    the same way AST builders are generated. In addition, for every {i wrapper}
    we generate a pattern to match the [loc] and [attributes] fields. For
    instance for the [expression] type:

    {[
      val pexp_loc :
        (Location.t, 'a, 'b) t ->
        (expression, 'b, 'c) t ->
        (expression, 'a, 'c) t

      val pexp_attributes :
        (attributes, 'a, 'b) t ->
        (expression, 'b, 'c) t ->
        (expression, 'a, 'c) t
    ]} *)

val true_ : (bool, 'a, 'a) t
val false_ : (bool, 'a, 'a) t
val eint : (int, 'a, 'b) t -> (expression, 'a, 'b) t
val echar : (char, 'a, 'b) t -> (expression, 'a, 'b) t
val estring : (string, 'a, 'b) t -> (expression, 'a, 'b) t
val efloat : (string, 'a, 'b) t -> (expression, 'a, 'b) t
val eint32 : (int32, 'a, 'b) t -> (expression, 'a, 'b) t
val eint64 : (int64, 'a, 'b) t -> (expression, 'a, 'b) t
val enativeint : (nativeint, 'a, 'b) t -> (expression, 'a, 'b) t
val pint : (int, 'a, 'b) t -> (pattern, 'a, 'b) t
val pchar : (char, 'a, 'b) t -> (pattern, 'a, 'b) t
val pstring : (string, 'a, 'b) t -> (pattern, 'a, 'b) t
val pfloat : (string, 'a, 'b) t -> (pattern, 'a, 'b) t
val pint32 : (int32, 'a, 'b) t -> (pattern, 'a, 'b) t
val pint64 : (int64, 'a, 'b) t -> (pattern, 'a, 'b) t
val pnativeint : (nativeint, 'a, 'b) t -> (pattern, 'a, 'b) t
val single_expr_payload : (expression, 'a, 'b) t -> (payload, 'a, 'b) t

val no_label :
  (expression, 'a, 'b) t -> (Asttypes.arg_label * expression, 'a, 'b) t

val attribute :
  name:(string, 'a, 'b) t ->
  payload:(payload, 'b, 'c) t ->
  (attribute, 'a, 'c) t

val extension :
  (string, 'a, 'b) t -> (payload, 'b, 'c) t -> (extension, 'a, 'c) t

val elist : (expression, 'a -> 'a, 'b) t -> (expression, 'b list -> 'c, 'c) t

val esequence :
  (expression, 'a -> 'a, 'b) t -> (expression, 'b list -> 'c, 'c) t

type context

val of_func : (context -> Location.t -> 'a -> 'b -> 'c) -> ('a, 'b, 'c) t
val to_func : ('a, 'b, 'c) t -> context -> Location.t -> 'a -> 'b -> 'c
