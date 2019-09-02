(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
#ifdef INT
    | `Int of int
#endif
#ifdef INTLIT
    | `Intlit of string
#endif
#ifdef FLOAT
    | `Float of float
#endif
#ifdef FLOATLIT
    | `Floatlit of string
#endif
#ifdef STRING
    | `String of string
#endif
#ifdef STRINGLIT
    | `Stringlit of string
#endif
    | `Assoc of (string * t) list
    | `List of t list
#ifdef TUPLE
    | `Tuple of t list
#endif
#ifdef VARIANT
    | `Variant of (string * t option)
#endif
    ]
(**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

type json = t [@@deprecated "json types are being renamed and will be removed in the next Yojson major version. Use type t instead"]
(**
 * Compatibility type alias for type `t`
 *)

(*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)
