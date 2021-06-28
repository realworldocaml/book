(* Auto-generated from "test.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

(** This is just a test. *)

type test_variant = Test.test_variant

type ('x, 'y) poly = ('x, 'y) Test.poly = {
  fst: 'x list;
  snd: ('x, 'y) poly option
}

type 'a p' = 'a Test.p' =  A | Bb of 'a p' | Ccccc of 'a 

type p = Test.p

and r = Test.r = { a: int; mutable b: bool; c: p }

type validated_string_check = Test.validated_string_check

type validate_me = Test.validate_me

type val1 = Test.val1 = { val1_x: int }

type val2 = Test.val2 = { val2_x: val1; val2_y: val1 option }

type unixtime_list = Test.unixtime_list

type date = Test.date

type mixed_record = Test.mixed_record = {
  field0: int option;
  field1: float option;
  field2: string option;
  field3: Int64.t;
  field4: float Atdgen_runtime.Util.ocaml_array;
  field5: bool option;
  field6: string option;
  field7: test_variant;
  field8: string Atdgen_runtime.Util.ocaml_array;
  field9: (int * int * Char.t * int * Int32.t * Int64.t);
  field10: bool;
  field11: bool;
  field12: unit list;
  field13: string option list;
  field14: date
}

type mixed = Test.mixed

type test = Test.test = {
  x0: int option;
  x1: float option;
  x2: mixed;
  x3: mixed_record list;
  x4: Int64.t
}

type tup = Test.tup

type star_rating = Test.star_rating

type 'a generic = 'a Test.generic = { x294623: int }

type specialized = Test.specialized

type some_record = Test.some_record = { some_field: int }

type precision = Test.precision = {
  sqrt2_5: float;
  small_2: float;
  large_2: float
}

type p'' = Test.p''

type option_validation = Test.option_validation

type no_real_wrap = Test.no_real_wrap

type natural = Test.natural

type id = Test.id

type json_map = Test.json_map

type intopt = Test.intopt

type int_assoc_list = Test.int_assoc_list

type int_assoc_array = Test.int_assoc_array

type int8 = Test.int8

type int64 = Test.int64

type int32 = Test.int32

type hello = Test.hello

type floats = Test.floats = { f32: float; f64: float }

type extended_tuple = Test.extended_tuple

type extended = Test.extended = {
  b0x (*atd b0 *): int;
  b1x (*atd b1 *): bool;
  b2x (*atd b2 *): string;
  b3x (*atd b3 *): string option;
  b4x (*atd b4 *): string option;
  b5x (*atd b5 *): float
}

type even_natural = Test.even_natural

(**
  \}\}\}abc[def]ghi
  
{v
j  *  j
 k * k
  l*l
v}
  
{v
mno
v}
  
  [pqr]\{stu\}vwx
  
  yz
  
  [\} \[ \] \{v]
  
{v
\} [x] v\} \{v [ @ 
v}
*)
type def = Test.def

type char = Test.char

type base_tuple = Test.base_tuple

type base = Test.base = { b0: int; b1: bool }

type 'a array = 'a Test.array

type 'a abs3 = 'a Test.abs3

type 'a abs2 = 'a Test.abs2

type 'a abs1 = 'a Test.abs1

val validate_test_variant :
  Atdgen_runtime.Util.Validation.path -> test_variant -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!test_variant}. *)

val create_poly :
  fst: 'x list ->
  snd: ('x, 'y) poly option ->
  unit -> ('x, 'y) poly
  (** Create a record of type {!poly}. *)

val validate_poly :
  (Atdgen_runtime.Util.Validation.path -> 'x -> Atdgen_runtime.Util.Validation.error option) ->
  (Atdgen_runtime.Util.Validation.path -> 'y -> Atdgen_runtime.Util.Validation.error option) ->
  Atdgen_runtime.Util.Validation.path -> ('x, 'y) poly -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!poly}. *)

val validate_p' :
  (Atdgen_runtime.Util.Validation.path -> 'a -> Atdgen_runtime.Util.Validation.error option) ->
  Atdgen_runtime.Util.Validation.path -> 'a p' -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!p'}. *)

val validate_p :
  Atdgen_runtime.Util.Validation.path -> p -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!p}. *)

val create_r :
  a: int ->
  b: bool ->
  c: p ->
  unit -> r
  (** Create a record of type {!r}. *)

val validate_r :
  Atdgen_runtime.Util.Validation.path -> r -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!r}. *)

val validate_validated_string_check :
  Atdgen_runtime.Util.Validation.path -> validated_string_check -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!validated_string_check}. *)

val validate_validate_me :
  Atdgen_runtime.Util.Validation.path -> validate_me -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!validate_me}. *)

val create_val1 :
  val1_x: int ->
  unit -> val1
  (** Create a record of type {!val1}. *)

val validate_val1 :
  Atdgen_runtime.Util.Validation.path -> val1 -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!val1}. *)

val create_val2 :
  val2_x: val1 ->
  ?val2_y: val1 ->
  unit -> val2
  (** Create a record of type {!val2}. *)

val validate_val2 :
  Atdgen_runtime.Util.Validation.path -> val2 -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!val2}. *)

val validate_unixtime_list :
  Atdgen_runtime.Util.Validation.path -> unixtime_list -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!unixtime_list}. *)

val validate_date :
  Atdgen_runtime.Util.Validation.path -> date -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!date}. *)

val create_mixed_record :
  ?field0: int ->
  ?field1: float ->
  field2: string option ->
  field3: Int64.t ->
  field4: float Atdgen_runtime.Util.ocaml_array ->
  ?field5: bool ->
  ?field6: string ->
  field7: test_variant ->
  field8: string Atdgen_runtime.Util.ocaml_array ->
  field9: (int * int * Char.t * int * Int32.t * Int64.t) ->
  field10: bool ->
  ?field11: bool ->
  field12: unit list ->
  field13: string option list ->
  field14: date ->
  unit -> mixed_record
  (** Create a record of type {!mixed_record}. *)

val validate_mixed_record :
  Atdgen_runtime.Util.Validation.path -> mixed_record -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!mixed_record}. *)

val validate_mixed :
  Atdgen_runtime.Util.Validation.path -> mixed -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!mixed}. *)

val create_test :
  ?x0: int ->
  ?x1: float ->
  x2: mixed ->
  x3: mixed_record list ->
  x4: Int64.t ->
  unit -> test
  (** Create a record of type {!test}. *)

val validate_test :
  Atdgen_runtime.Util.Validation.path -> test -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!test}. *)

val validate_tup :
  Atdgen_runtime.Util.Validation.path -> tup -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!tup}. *)

val validate_star_rating :
  Atdgen_runtime.Util.Validation.path -> star_rating -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!star_rating}. *)

val create_generic :
  x294623: int ->
  unit -> 'a generic
  (** Create a record of type {!generic}. *)

val validate_generic :
  (Atdgen_runtime.Util.Validation.path -> 'a -> Atdgen_runtime.Util.Validation.error option) ->
  Atdgen_runtime.Util.Validation.path -> 'a generic -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!generic}. *)

val validate_specialized :
  Atdgen_runtime.Util.Validation.path -> specialized -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!specialized}. *)

val create_some_record :
  some_field: int ->
  unit -> some_record
  (** Create a record of type {!some_record}. *)

val validate_some_record :
  Atdgen_runtime.Util.Validation.path -> some_record -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!some_record}. *)

val create_precision :
  sqrt2_5: float ->
  small_2: float ->
  large_2: float ->
  unit -> precision
  (** Create a record of type {!precision}. *)

val validate_precision :
  Atdgen_runtime.Util.Validation.path -> precision -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!precision}. *)

val validate_p'' :
  Atdgen_runtime.Util.Validation.path -> p'' -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!p''}. *)

val validate_option_validation :
  Atdgen_runtime.Util.Validation.path -> option_validation -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!option_validation}. *)

val validate_no_real_wrap :
  Atdgen_runtime.Util.Validation.path -> no_real_wrap -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!no_real_wrap}. *)

val validate_natural :
  Atdgen_runtime.Util.Validation.path -> natural -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!natural}. *)

val validate_id :
  Atdgen_runtime.Util.Validation.path -> id -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!id}. *)

val validate_json_map :
  Atdgen_runtime.Util.Validation.path -> json_map -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!json_map}. *)

val validate_intopt :
  Atdgen_runtime.Util.Validation.path -> intopt -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!intopt}. *)

val validate_int_assoc_list :
  Atdgen_runtime.Util.Validation.path -> int_assoc_list -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!int_assoc_list}. *)

val validate_int_assoc_array :
  Atdgen_runtime.Util.Validation.path -> int_assoc_array -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!int_assoc_array}. *)

val validate_int8 :
  Atdgen_runtime.Util.Validation.path -> int8 -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!int8}. *)

val validate_int64 :
  Atdgen_runtime.Util.Validation.path -> int64 -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!int64}. *)

val validate_int32 :
  Atdgen_runtime.Util.Validation.path -> int32 -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!int32}. *)

val validate_hello :
  Atdgen_runtime.Util.Validation.path -> hello -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!hello}. *)

val create_floats :
  f32: float ->
  f64: float ->
  unit -> floats
  (** Create a record of type {!floats}. *)

val validate_floats :
  Atdgen_runtime.Util.Validation.path -> floats -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!floats}. *)

val validate_extended_tuple :
  Atdgen_runtime.Util.Validation.path -> extended_tuple -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!extended_tuple}. *)

val create_extended :
  b0x: int ->
  b1x: bool ->
  b2x: string ->
  ?b3x: string ->
  b4x: string option ->
  ?b5x: float ->
  unit -> extended
  (** Create a record of type {!extended}. *)

val validate_extended :
  Atdgen_runtime.Util.Validation.path -> extended -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!extended}. *)

val validate_even_natural :
  Atdgen_runtime.Util.Validation.path -> even_natural -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!even_natural}. *)

val validate_char :
  Atdgen_runtime.Util.Validation.path -> char -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!char}. *)

val validate_base_tuple :
  Atdgen_runtime.Util.Validation.path -> base_tuple -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!base_tuple}. *)

val create_base :
  b0: int ->
  b1: bool ->
  unit -> base
  (** Create a record of type {!base}. *)

val validate_base :
  Atdgen_runtime.Util.Validation.path -> base -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!base}. *)

val validate_array :
  (Atdgen_runtime.Util.Validation.path -> 'a -> Atdgen_runtime.Util.Validation.error option) ->
  Atdgen_runtime.Util.Validation.path -> 'a array -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!array}. *)

val validate_abs3 :
  (Atdgen_runtime.Util.Validation.path -> 'a -> Atdgen_runtime.Util.Validation.error option) ->
  Atdgen_runtime.Util.Validation.path -> 'a abs3 -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!abs3}. *)

val validate_abs2 :
  (Atdgen_runtime.Util.Validation.path -> 'a -> Atdgen_runtime.Util.Validation.error option) ->
  Atdgen_runtime.Util.Validation.path -> 'a abs2 -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!abs2}. *)

val validate_abs1 :
  (Atdgen_runtime.Util.Validation.path -> 'a -> Atdgen_runtime.Util.Validation.error option) ->
  Atdgen_runtime.Util.Validation.path -> 'a abs1 -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!abs1}. *)

