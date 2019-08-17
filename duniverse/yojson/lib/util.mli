(**
   This module provides combinators for extracting fields from JSON
   values. This approach is recommended for reading a few fields
   from data returned by public APIs. However for more complex applications
   we recommend {{:https://github.com/MyLifeLabs/atdgen}Atdgen}.

   Here is some sample JSON data:
{v
\{
  "id": "398eb027",
  "name": "John Doe",
  "pages": [
    \{
      "id": 1,
      "title": "The Art of Flipping Coins",
      "url": "http://example.com/398eb027/1"
    },
    \{
      "id": 2,
      "deleted": true
    },
    \{
      "id": 3,
      "title": "Artichoke Salad",
      "url": "http://example.com/398eb027/3"
    },
    \{
      "id": 4,
      "title": "Flying Bananas",
      "url": "http://example.com/398eb027/4"
    }
  ]
}
v}

   In order to extract the "id" field, assuming it is mandatory,
   we would use the following OCaml code that operates on single JSON
   nodes:
{v
open Yojson.Basic.Util
...
  let id = json |> member "id" |> to_string in
  ...
v}

   In order to extract all the "title" fields, we would write the following
   OCaml code that operates on lists of JSON nodes, skipping
   undefined nodes and nodes of unexpected type:
{v
open Yojson.Basic.Util

let extract_titles (json : Yojson.Basic.t) : string list =
  [json]
    |> filter_member "pages"
    |> flatten
    |> filter_member "title"
    |> filter_string
v}
*)

exception Type_error of string * t
  (** Raised when the JSON value is not of the correct type to support an
      operation, e.g. [member] on an [`Int]. The string message explains the
      mismatch. *)

exception Undefined of string * t
  (** Raised when the equivalent JavaScript operation on the JSON value would
      return undefined. Currently this only happens when an array index is out
      of bounds. *)

val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** @deprecated Forward pipe operator; useful for composing JSON
    access functions without too many parentheses *)

val keys : t -> string list
  (** Returns all the key names in the given JSON object *)

val values : t -> t list
  (** Return all the value in the given JSON object *)

val combine : t -> t -> t
  (** Combine two JSON Objects together *)

val member : string -> t -> t
  (** [member k obj] returns the value associated with the key [k] in the JSON
      object [obj], or [`Null] if [k] is not present in [obj]. *)

val index : int -> t -> t
  (** [index i arr] returns the value at index [i] in the JSON array [arr].
      Negative indices count from the end of the list (so -1 is the last
      element). *)

val map : (t -> t) -> t -> t
  (** [map f arr] calls the function [f] on each element of the JSON array
      [arr], and returns a JSON array containing the results. *)

val to_assoc : t -> (string * t) list
  (** Extract the items of a JSON object or raise [Type_error]. *)

val to_option : (t -> 'a) -> t -> 'a option
  (** Return [None] if the JSON value is null or map the JSON value
      to [Some] value using the provided function. *)

val to_bool : t -> bool
  (** Extract a boolean value or raise [Type_error]. *)

val to_bool_option : t -> bool option
  (** Extract [Some] boolean value,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

val to_number : t -> float
  (** Extract a number or raise [Type_error]. *)

val to_number_option : t -> float option
  (** Extract [Some] number,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

val to_float : t -> float
  (** Extract a float value or raise [Type_error].
      [to_number] is generally preferred as it also works with int literals. *)

val to_float_option : t -> float option
  (** Extract [Some] float value,
      return [None] if the value is null,
      or raise [Type_error] otherwise.
      [to_number_option] is generally preferred as it also works
      with int literals. *)

val to_int : t -> int
  (** Extract an int from a JSON int or raise [Type_error]. *)

val to_int_option : t -> int option
  (** Extract [Some] int from a JSON int,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

val to_list : t -> t list
  (** Extract a list from JSON array or raise [Type_error]. *)

val to_string : t -> string
  (** Extract a string from a JSON string or raise [Type_error]. *)

val to_string_option : t -> string option
  (** Extract [Some] string from a JSON string,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

val convert_each : (t -> 'a) -> t -> 'a list
  (** The conversion functions above cannot be used with [map], because they do
      not return JSON values. This convenience function [convert_each to_f arr]
      is equivalent to [List.map to_f (to_list arr)]. *)


(** {3 Exception-free filters} *)

(**
   The following functions operate on lists of JSON nodes.
   None of them raises an exception when a certain kind of node is expected
   but no node or the wrong kind of node is found.
   Instead of raising an exception, nodes that are not as expected
   are simply ignored.
*)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f l] maps each element of the list [l] to an optional value
      using function [f] and unwraps the resulting values. *)

val flatten : t list -> t list
  (** Expects JSON arrays and returns all their elements as a single
      list. [flatten l] is equivalent to [List.flatten (filter_list l)]. *)

val filter_index : int -> t list -> t list
  (** Expects JSON arrays and returns all their elements existing at the given
      position. *)

val filter_list : t list -> t list list
  (** Expects JSON arrays and unwraps them. *)

val filter_member : string -> t list -> t list
  (** Expects JSON objects and returns all the fields of the given name
      (at most one field per object). *)

val filter_assoc : t list -> (string * t) list list
  (** Expects JSON objects and unwraps them. *)

val filter_bool : t list -> bool list
  (** Expects JSON booleans and unwraps them. *)

val filter_int : t list -> int list
  (** Expects JSON integers ([`Int] nodes) and unwraps them. *)

val filter_float : t list -> float list
  (** Expects JSON floats ([`Float] nodes) and unwraps them. *)

val filter_number : t list -> float list
  (** Expects JSON numbers ([`Int] or [`Float]) and unwraps them.
      Ints are converted to floats. *)

val filter_string : t list -> string list
  (** Expects JSON strings and unwraps them. *)
