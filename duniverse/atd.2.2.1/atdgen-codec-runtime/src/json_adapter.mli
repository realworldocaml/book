(** Json adapters and tools for the user to make their own json adapters
    for common situations.

    Json adapters are used to rewrite json node into a form compatible
    with atdgen's conventions.
*)

(** Module signature required of any json adapter.
    For example, an ATD annotation
    [<json
       adapter.ocaml="Atdgen_codec_runtime.Json_adapter.Type_field"]
    refers to the OCaml module
    [Atdgen_codec_runtime.Json_adapter.Type_field].
*)

module type S = sig
  (** Convert a real json tree into an atd-compliant form. *)
  val normalize : Json.t -> Json.t

  (** Convert an atd-compliant json tree into a real json tree. *)
  val restore : Json.t -> Json.t
end

(** Support for json objects that contain a field that indicates
    the type of that object.
    The following
{[
{
  "type": "User",
  "id": "abc123",
  "age": 52
}
]}
        gets converted into a pair
{[
[
  "User",
  {
    "type": "User",
    "id": "abc123",
    "age": 52
  }
]
]}
        A corresponding ATD type definition is
{[
type obj = [
  | User of user
  | ...
] <json adapter.ocaml="Atdgen_codec_runtime.Json_adapter.Type_field">

type user = {
  id: string;
  age: int;

  (* The following field definition is supported, albeit useless. *)
  type_ <json name="type">: string;
}
]}
*)
module Type_field : sig
  module type Param = sig
    val type_field_name : string
  end

  (** Default parameters, using [type_field_name = "type"]. *)
  module Default_param : Param

  (** Default adapter assuming a ["type"] field. *)
  include S

  (** Functor, allowing the use of a custom parameter:
{[
module Kind = Type_field.Make (struct type_field_name = "kind" end)
]}
  *)
  module Make (Param : Param) : S
end
