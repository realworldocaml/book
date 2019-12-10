type t = Foo of int

let fail _ = failwith "not implemented"

module Biniou =
struct
  type def = t
  let def_tag = 0
  let write_untagged_def _ : 'a -> unit = fail
  let write_def _ : 'a -> unit = fail
  let string_of_def = fail
  let get_def_reader _ = fail
  let read_def = fail
  let def_of_string = fail
end

module Json =
struct
  type def = t
  let write_def _ : 'a -> unit = fail
  let string_of_def = fail
  let read_def _ = fail
  let def_of_string = fail
end

module Natural :
sig
  type t = private int
  val wrap : int -> t
  val unwrap : t -> int
end =
struct
  type t = int
  let wrap x =
    if x < 0 then
      failwith ("Out of bounds number " ^ string_of_int x)
    else
      x
  let unwrap x = x
end

module Even_natural :
sig
  type t = private Natural.t
  val wrap : Natural.t -> t
  val unwrap : t -> Natural.t
end =
struct
  type t = Natural.t
  let wrap (x : Natural.t) =
    if (x :> int) mod 2 <> 0 then
      failwith ("Odd number " ^ string_of_int (x :> int))
    else
      x
  let unwrap x = x
end

module Tag_field_example =
  Atdgen_runtime.Json_adapter.Type_and_value_fields.Make (struct
    let type_field_name = "the_type"
    let value_field_name = "the_value"
    let known_tags = None
  end)

module Tag_field_with_catchall =
  Atdgen_runtime.Json_adapter.Type_and_value_fields.Make (struct
    let type_field_name = "the_type"
    let value_field_name = "the_value2"
    let known_tags = Some (["a";"b"], "Unknown")
  end)
