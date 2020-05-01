(* Auto-generated from "test3j.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

type rec_type = { more: rec_type list }

type unixtime_list = float list

type json = Yojson.Safe.t

type tf_variant2 = [
    `A of int
  | `B of int
  | `Unknown of (string * json option)
]

type tf_variant = [ `A of int | `B of int ]

type tf_record2 = { the_value2: tf_variant2; etc2: string }

type tf_record = { the_value: tf_variant; etc: string }

type dyn = Yojson.Safe.t

type t = { foo: int; bar: json; baz: dyn }

type sf_adapted = [ `A of bool | `B of int ]

type sample_open_enum = [ `Alpha | `Beta | `Other of string ]

type sample_open_enums = sample_open_enum list

type patch = {
  patch1: int option option;
  patch2: int option option;
  patch3: int option option
}

type b = { thing: int }

type a = { thing: string; other_thing: bool }

type adapted = [ `A of a | `B of b ]
