(* Auto-generated from "test.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

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

type test_field_prefix = Test.test_field_prefix = {
  theprefix_hello (*atd hello *): bool;
  theprefix_world (*atd world *): int
}

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
type def = Test_lib.Json.def

type char = Test.char

type base_tuple = Test.base_tuple

type base = Test.base = { b0: int; b1: bool }

type 'a array = 'a Test.array

type 'a abs3 = 'a Test.abs3

type 'a abs2 = 'a Test.abs2

type 'a abs1 = 'a Test.abs1

val write_test_variant :
  Buffer.t -> test_variant -> unit
  (** Output a JSON value of type {!type:test_variant}. *)

val string_of_test_variant :
  ?len:int -> test_variant -> string
  (** Serialize a value of type {!type:test_variant}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_test_variant :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> test_variant
  (** Input JSON data of type {!type:test_variant}. *)

val test_variant_of_string :
  string -> test_variant
  (** Deserialize JSON data of type {!type:test_variant}. *)


val write_poly :
  (Buffer.t -> 'x -> unit) ->
  (Buffer.t -> 'y -> unit) ->
  Buffer.t -> ('x, 'y) poly -> unit
  (** Output a JSON value of type {!type:poly}. *)

val string_of_poly :
  (Buffer.t -> 'x -> unit) ->
  (Buffer.t -> 'y -> unit) ->
  ?len:int -> ('x, 'y) poly -> string
  (** Serialize a value of type {!type:poly}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_poly :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'x) ->
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'y) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ('x, 'y) poly
  (** Input JSON data of type {!type:poly}. *)

val poly_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'x) ->
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'y) ->
  string -> ('x, 'y) poly
  (** Deserialize JSON data of type {!type:poly}. *)

val create_poly :
  fst: 'x list ->
  snd: ('x, 'y) poly option ->
  unit -> ('x, 'y) poly
  (** Create a record of type {!type:poly}. *)


val write_p' :
  (Buffer.t -> 'a -> unit) ->
  Buffer.t -> 'a p' -> unit
  (** Output a JSON value of type {!type:p'}. *)

val string_of_p' :
  (Buffer.t -> 'a -> unit) ->
  ?len:int -> 'a p' -> string
  (** Serialize a value of type {!type:p'}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_p' :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a p'
  (** Input JSON data of type {!type:p'}. *)

val p'_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a p'
  (** Deserialize JSON data of type {!type:p'}. *)


val write_p :
  Buffer.t -> p -> unit
  (** Output a JSON value of type {!type:p}. *)

val string_of_p :
  ?len:int -> p -> string
  (** Serialize a value of type {!type:p}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_p :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> p
  (** Input JSON data of type {!type:p}. *)

val p_of_string :
  string -> p
  (** Deserialize JSON data of type {!type:p}. *)


val write_r :
  Buffer.t -> r -> unit
  (** Output a JSON value of type {!type:r}. *)

val string_of_r :
  ?len:int -> r -> string
  (** Serialize a value of type {!type:r}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_r :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> r
  (** Input JSON data of type {!type:r}. *)

val r_of_string :
  string -> r
  (** Deserialize JSON data of type {!type:r}. *)

val create_r :
  a: int ->
  b: bool ->
  c: p ->
  unit -> r
  (** Create a record of type {!type:r}. *)


val write_validated_string_check :
  Buffer.t -> validated_string_check -> unit
  (** Output a JSON value of type {!type:validated_string_check}. *)

val string_of_validated_string_check :
  ?len:int -> validated_string_check -> string
  (** Serialize a value of type {!type:validated_string_check}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_validated_string_check :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> validated_string_check
  (** Input JSON data of type {!type:validated_string_check}. *)

val validated_string_check_of_string :
  string -> validated_string_check
  (** Deserialize JSON data of type {!type:validated_string_check}. *)


val write_validate_me :
  Buffer.t -> validate_me -> unit
  (** Output a JSON value of type {!type:validate_me}. *)

val string_of_validate_me :
  ?len:int -> validate_me -> string
  (** Serialize a value of type {!type:validate_me}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_validate_me :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> validate_me
  (** Input JSON data of type {!type:validate_me}. *)

val validate_me_of_string :
  string -> validate_me
  (** Deserialize JSON data of type {!type:validate_me}. *)


val write_val1 :
  Buffer.t -> val1 -> unit
  (** Output a JSON value of type {!type:val1}. *)

val string_of_val1 :
  ?len:int -> val1 -> string
  (** Serialize a value of type {!type:val1}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_val1 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> val1
  (** Input JSON data of type {!type:val1}. *)

val val1_of_string :
  string -> val1
  (** Deserialize JSON data of type {!type:val1}. *)

val create_val1 :
  val1_x: int ->
  unit -> val1
  (** Create a record of type {!type:val1}. *)


val write_val2 :
  Buffer.t -> val2 -> unit
  (** Output a JSON value of type {!type:val2}. *)

val string_of_val2 :
  ?len:int -> val2 -> string
  (** Serialize a value of type {!type:val2}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_val2 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> val2
  (** Input JSON data of type {!type:val2}. *)

val val2_of_string :
  string -> val2
  (** Deserialize JSON data of type {!type:val2}. *)

val create_val2 :
  val2_x: val1 ->
  ?val2_y: val1 ->
  unit -> val2
  (** Create a record of type {!type:val2}. *)


val write_unixtime_list :
  Buffer.t -> unixtime_list -> unit
  (** Output a JSON value of type {!type:unixtime_list}. *)

val string_of_unixtime_list :
  ?len:int -> unixtime_list -> string
  (** Serialize a value of type {!type:unixtime_list}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_unixtime_list :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> unixtime_list
  (** Input JSON data of type {!type:unixtime_list}. *)

val unixtime_list_of_string :
  string -> unixtime_list
  (** Deserialize JSON data of type {!type:unixtime_list}. *)


val write_date :
  Buffer.t -> date -> unit
  (** Output a JSON value of type {!type:date}. *)

val string_of_date :
  ?len:int -> date -> string
  (** Serialize a value of type {!type:date}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_date :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> date
  (** Input JSON data of type {!type:date}. *)

val date_of_string :
  string -> date
  (** Deserialize JSON data of type {!type:date}. *)


val write_mixed_record :
  Buffer.t -> mixed_record -> unit
  (** Output a JSON value of type {!type:mixed_record}. *)

val string_of_mixed_record :
  ?len:int -> mixed_record -> string
  (** Serialize a value of type {!type:mixed_record}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_mixed_record :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> mixed_record
  (** Input JSON data of type {!type:mixed_record}. *)

val mixed_record_of_string :
  string -> mixed_record
  (** Deserialize JSON data of type {!type:mixed_record}. *)

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
  (** Create a record of type {!type:mixed_record}. *)


val write_mixed :
  Buffer.t -> mixed -> unit
  (** Output a JSON value of type {!type:mixed}. *)

val string_of_mixed :
  ?len:int -> mixed -> string
  (** Serialize a value of type {!type:mixed}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_mixed :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> mixed
  (** Input JSON data of type {!type:mixed}. *)

val mixed_of_string :
  string -> mixed
  (** Deserialize JSON data of type {!type:mixed}. *)


val write_test :
  Buffer.t -> test -> unit
  (** Output a JSON value of type {!type:test}. *)

val string_of_test :
  ?len:int -> test -> string
  (** Serialize a value of type {!type:test}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_test :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> test
  (** Input JSON data of type {!type:test}. *)

val test_of_string :
  string -> test
  (** Deserialize JSON data of type {!type:test}. *)

val create_test :
  ?x0: int ->
  ?x1: float ->
  x2: mixed ->
  x3: mixed_record list ->
  x4: Int64.t ->
  unit -> test
  (** Create a record of type {!type:test}. *)


val write_tup :
  Buffer.t -> tup -> unit
  (** Output a JSON value of type {!type:tup}. *)

val string_of_tup :
  ?len:int -> tup -> string
  (** Serialize a value of type {!type:tup}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tup :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tup
  (** Input JSON data of type {!type:tup}. *)

val tup_of_string :
  string -> tup
  (** Deserialize JSON data of type {!type:tup}. *)


val write_test_field_prefix :
  Buffer.t -> test_field_prefix -> unit
  (** Output a JSON value of type {!type:test_field_prefix}. *)

val string_of_test_field_prefix :
  ?len:int -> test_field_prefix -> string
  (** Serialize a value of type {!type:test_field_prefix}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_test_field_prefix :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> test_field_prefix
  (** Input JSON data of type {!type:test_field_prefix}. *)

val test_field_prefix_of_string :
  string -> test_field_prefix
  (** Deserialize JSON data of type {!type:test_field_prefix}. *)

val create_test_field_prefix :
  theprefix_hello: bool ->
  theprefix_world: int ->
  unit -> test_field_prefix
  (** Create a record of type {!type:test_field_prefix}. *)


val write_star_rating :
  Buffer.t -> star_rating -> unit
  (** Output a JSON value of type {!type:star_rating}. *)

val string_of_star_rating :
  ?len:int -> star_rating -> string
  (** Serialize a value of type {!type:star_rating}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_star_rating :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> star_rating
  (** Input JSON data of type {!type:star_rating}. *)

val star_rating_of_string :
  string -> star_rating
  (** Deserialize JSON data of type {!type:star_rating}. *)


val write_generic :
  (Buffer.t -> 'a -> unit) ->
  Buffer.t -> 'a generic -> unit
  (** Output a JSON value of type {!type:generic}. *)

val string_of_generic :
  (Buffer.t -> 'a -> unit) ->
  ?len:int -> 'a generic -> string
  (** Serialize a value of type {!type:generic}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_generic :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a generic
  (** Input JSON data of type {!type:generic}. *)

val generic_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a generic
  (** Deserialize JSON data of type {!type:generic}. *)

val create_generic :
  x294623: int ->
  unit -> 'a generic
  (** Create a record of type {!type:generic}. *)


val write_specialized :
  Buffer.t -> specialized -> unit
  (** Output a JSON value of type {!type:specialized}. *)

val string_of_specialized :
  ?len:int -> specialized -> string
  (** Serialize a value of type {!type:specialized}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_specialized :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> specialized
  (** Input JSON data of type {!type:specialized}. *)

val specialized_of_string :
  string -> specialized
  (** Deserialize JSON data of type {!type:specialized}. *)


val write_some_record :
  Buffer.t -> some_record -> unit
  (** Output a JSON value of type {!type:some_record}. *)

val string_of_some_record :
  ?len:int -> some_record -> string
  (** Serialize a value of type {!type:some_record}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_some_record :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> some_record
  (** Input JSON data of type {!type:some_record}. *)

val some_record_of_string :
  string -> some_record
  (** Deserialize JSON data of type {!type:some_record}. *)

val create_some_record :
  some_field: int ->
  unit -> some_record
  (** Create a record of type {!type:some_record}. *)


val write_precision :
  Buffer.t -> precision -> unit
  (** Output a JSON value of type {!type:precision}. *)

val string_of_precision :
  ?len:int -> precision -> string
  (** Serialize a value of type {!type:precision}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_precision :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> precision
  (** Input JSON data of type {!type:precision}. *)

val precision_of_string :
  string -> precision
  (** Deserialize JSON data of type {!type:precision}. *)

val create_precision :
  sqrt2_5: float ->
  small_2: float ->
  large_2: float ->
  unit -> precision
  (** Create a record of type {!type:precision}. *)


val write_p'' :
  Buffer.t -> p'' -> unit
  (** Output a JSON value of type {!type:p''}. *)

val string_of_p'' :
  ?len:int -> p'' -> string
  (** Serialize a value of type {!type:p''}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_p'' :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> p''
  (** Input JSON data of type {!type:p''}. *)

val p''_of_string :
  string -> p''
  (** Deserialize JSON data of type {!type:p''}. *)


val write_option_validation :
  Buffer.t -> option_validation -> unit
  (** Output a JSON value of type {!type:option_validation}. *)

val string_of_option_validation :
  ?len:int -> option_validation -> string
  (** Serialize a value of type {!type:option_validation}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_option_validation :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> option_validation
  (** Input JSON data of type {!type:option_validation}. *)

val option_validation_of_string :
  string -> option_validation
  (** Deserialize JSON data of type {!type:option_validation}. *)


val write_no_real_wrap :
  Buffer.t -> no_real_wrap -> unit
  (** Output a JSON value of type {!type:no_real_wrap}. *)

val string_of_no_real_wrap :
  ?len:int -> no_real_wrap -> string
  (** Serialize a value of type {!type:no_real_wrap}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_no_real_wrap :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> no_real_wrap
  (** Input JSON data of type {!type:no_real_wrap}. *)

val no_real_wrap_of_string :
  string -> no_real_wrap
  (** Deserialize JSON data of type {!type:no_real_wrap}. *)


val write_natural :
  Buffer.t -> natural -> unit
  (** Output a JSON value of type {!type:natural}. *)

val string_of_natural :
  ?len:int -> natural -> string
  (** Serialize a value of type {!type:natural}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_natural :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> natural
  (** Input JSON data of type {!type:natural}. *)

val natural_of_string :
  string -> natural
  (** Deserialize JSON data of type {!type:natural}. *)


val write_id :
  Buffer.t -> id -> unit
  (** Output a JSON value of type {!type:id}. *)

val string_of_id :
  ?len:int -> id -> string
  (** Serialize a value of type {!type:id}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_id :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> id
  (** Input JSON data of type {!type:id}. *)

val id_of_string :
  string -> id
  (** Deserialize JSON data of type {!type:id}. *)


val write_json_map :
  Buffer.t -> json_map -> unit
  (** Output a JSON value of type {!type:json_map}. *)

val string_of_json_map :
  ?len:int -> json_map -> string
  (** Serialize a value of type {!type:json_map}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_json_map :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> json_map
  (** Input JSON data of type {!type:json_map}. *)

val json_map_of_string :
  string -> json_map
  (** Deserialize JSON data of type {!type:json_map}. *)


val write_intopt :
  Buffer.t -> intopt -> unit
  (** Output a JSON value of type {!type:intopt}. *)

val string_of_intopt :
  ?len:int -> intopt -> string
  (** Serialize a value of type {!type:intopt}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_intopt :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> intopt
  (** Input JSON data of type {!type:intopt}. *)

val intopt_of_string :
  string -> intopt
  (** Deserialize JSON data of type {!type:intopt}. *)


val write_int_assoc_list :
  Buffer.t -> int_assoc_list -> unit
  (** Output a JSON value of type {!type:int_assoc_list}. *)

val string_of_int_assoc_list :
  ?len:int -> int_assoc_list -> string
  (** Serialize a value of type {!type:int_assoc_list}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_int_assoc_list :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> int_assoc_list
  (** Input JSON data of type {!type:int_assoc_list}. *)

val int_assoc_list_of_string :
  string -> int_assoc_list
  (** Deserialize JSON data of type {!type:int_assoc_list}. *)


val write_int_assoc_array :
  Buffer.t -> int_assoc_array -> unit
  (** Output a JSON value of type {!type:int_assoc_array}. *)

val string_of_int_assoc_array :
  ?len:int -> int_assoc_array -> string
  (** Serialize a value of type {!type:int_assoc_array}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_int_assoc_array :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> int_assoc_array
  (** Input JSON data of type {!type:int_assoc_array}. *)

val int_assoc_array_of_string :
  string -> int_assoc_array
  (** Deserialize JSON data of type {!type:int_assoc_array}. *)


val write_int8 :
  Buffer.t -> int8 -> unit
  (** Output a JSON value of type {!type:int8}. *)

val string_of_int8 :
  ?len:int -> int8 -> string
  (** Serialize a value of type {!type:int8}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_int8 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> int8
  (** Input JSON data of type {!type:int8}. *)

val int8_of_string :
  string -> int8
  (** Deserialize JSON data of type {!type:int8}. *)


val write_int64 :
  Buffer.t -> int64 -> unit
  (** Output a JSON value of type {!type:int64}. *)

val string_of_int64 :
  ?len:int -> int64 -> string
  (** Serialize a value of type {!type:int64}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_int64 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> int64
  (** Input JSON data of type {!type:int64}. *)

val int64_of_string :
  string -> int64
  (** Deserialize JSON data of type {!type:int64}. *)


val write_int32 :
  Buffer.t -> int32 -> unit
  (** Output a JSON value of type {!type:int32}. *)

val string_of_int32 :
  ?len:int -> int32 -> string
  (** Serialize a value of type {!type:int32}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_int32 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> int32
  (** Input JSON data of type {!type:int32}. *)

val int32_of_string :
  string -> int32
  (** Deserialize JSON data of type {!type:int32}. *)


val write_hello :
  Buffer.t -> hello -> unit
  (** Output a JSON value of type {!type:hello}. *)

val string_of_hello :
  ?len:int -> hello -> string
  (** Serialize a value of type {!type:hello}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_hello :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> hello
  (** Input JSON data of type {!type:hello}. *)

val hello_of_string :
  string -> hello
  (** Deserialize JSON data of type {!type:hello}. *)


val write_floats :
  Buffer.t -> floats -> unit
  (** Output a JSON value of type {!type:floats}. *)

val string_of_floats :
  ?len:int -> floats -> string
  (** Serialize a value of type {!type:floats}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_floats :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> floats
  (** Input JSON data of type {!type:floats}. *)

val floats_of_string :
  string -> floats
  (** Deserialize JSON data of type {!type:floats}. *)

val create_floats :
  f32: float ->
  f64: float ->
  unit -> floats
  (** Create a record of type {!type:floats}. *)


val write_extended_tuple :
  Buffer.t -> extended_tuple -> unit
  (** Output a JSON value of type {!type:extended_tuple}. *)

val string_of_extended_tuple :
  ?len:int -> extended_tuple -> string
  (** Serialize a value of type {!type:extended_tuple}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_extended_tuple :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> extended_tuple
  (** Input JSON data of type {!type:extended_tuple}. *)

val extended_tuple_of_string :
  string -> extended_tuple
  (** Deserialize JSON data of type {!type:extended_tuple}. *)


val write_extended :
  Buffer.t -> extended -> unit
  (** Output a JSON value of type {!type:extended}. *)

val string_of_extended :
  ?len:int -> extended -> string
  (** Serialize a value of type {!type:extended}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_extended :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> extended
  (** Input JSON data of type {!type:extended}. *)

val extended_of_string :
  string -> extended
  (** Deserialize JSON data of type {!type:extended}. *)

val create_extended :
  b0x: int ->
  b1x: bool ->
  b2x: string ->
  ?b3x: string ->
  b4x: string option ->
  ?b5x: float ->
  unit -> extended
  (** Create a record of type {!type:extended}. *)


val write_even_natural :
  Buffer.t -> even_natural -> unit
  (** Output a JSON value of type {!type:even_natural}. *)

val string_of_even_natural :
  ?len:int -> even_natural -> string
  (** Serialize a value of type {!type:even_natural}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_even_natural :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> even_natural
  (** Input JSON data of type {!type:even_natural}. *)

val even_natural_of_string :
  string -> even_natural
  (** Deserialize JSON data of type {!type:even_natural}. *)


val write_def :
  Buffer.t -> def -> unit
  (** Output a JSON value of type {!type:def}. *)

val string_of_def :
  ?len:int -> def -> string
  (** Serialize a value of type {!type:def}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_def :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> def
  (** Input JSON data of type {!type:def}. *)

val def_of_string :
  string -> def
  (** Deserialize JSON data of type {!type:def}. *)


val write_char :
  Buffer.t -> char -> unit
  (** Output a JSON value of type {!type:char}. *)

val string_of_char :
  ?len:int -> char -> string
  (** Serialize a value of type {!type:char}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_char :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> char
  (** Input JSON data of type {!type:char}. *)

val char_of_string :
  string -> char
  (** Deserialize JSON data of type {!type:char}. *)


val write_base_tuple :
  Buffer.t -> base_tuple -> unit
  (** Output a JSON value of type {!type:base_tuple}. *)

val string_of_base_tuple :
  ?len:int -> base_tuple -> string
  (** Serialize a value of type {!type:base_tuple}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_base_tuple :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> base_tuple
  (** Input JSON data of type {!type:base_tuple}. *)

val base_tuple_of_string :
  string -> base_tuple
  (** Deserialize JSON data of type {!type:base_tuple}. *)


val write_base :
  Buffer.t -> base -> unit
  (** Output a JSON value of type {!type:base}. *)

val string_of_base :
  ?len:int -> base -> string
  (** Serialize a value of type {!type:base}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_base :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> base
  (** Input JSON data of type {!type:base}. *)

val base_of_string :
  string -> base
  (** Deserialize JSON data of type {!type:base}. *)

val create_base :
  b0: int ->
  b1: bool ->
  unit -> base
  (** Create a record of type {!type:base}. *)


val write_array :
  (Buffer.t -> 'a -> unit) ->
  Buffer.t -> 'a array -> unit
  (** Output a JSON value of type {!type:array}. *)

val string_of_array :
  (Buffer.t -> 'a -> unit) ->
  ?len:int -> 'a array -> string
  (** Serialize a value of type {!type:array}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_array :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a array
  (** Input JSON data of type {!type:array}. *)

val array_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a array
  (** Deserialize JSON data of type {!type:array}. *)


val write_abs3 :
  (Buffer.t -> 'a -> unit) ->
  Buffer.t -> 'a abs3 -> unit
  (** Output a JSON value of type {!type:abs3}. *)

val string_of_abs3 :
  (Buffer.t -> 'a -> unit) ->
  ?len:int -> 'a abs3 -> string
  (** Serialize a value of type {!type:abs3}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_abs3 :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a abs3
  (** Input JSON data of type {!type:abs3}. *)

val abs3_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a abs3
  (** Deserialize JSON data of type {!type:abs3}. *)


val write_abs2 :
  (Buffer.t -> 'a -> unit) ->
  Buffer.t -> 'a abs2 -> unit
  (** Output a JSON value of type {!type:abs2}. *)

val string_of_abs2 :
  (Buffer.t -> 'a -> unit) ->
  ?len:int -> 'a abs2 -> string
  (** Serialize a value of type {!type:abs2}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_abs2 :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a abs2
  (** Input JSON data of type {!type:abs2}. *)

val abs2_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a abs2
  (** Deserialize JSON data of type {!type:abs2}. *)


val write_abs1 :
  (Buffer.t -> 'a -> unit) ->
  Buffer.t -> 'a abs1 -> unit
  (** Output a JSON value of type {!type:abs1}. *)

val string_of_abs1 :
  (Buffer.t -> 'a -> unit) ->
  ?len:int -> 'a abs1 -> string
  (** Serialize a value of type {!type:abs1}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_abs1 :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a abs1
  (** Input JSON data of type {!type:abs1}. *)

val abs1_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a abs1
  (** Deserialize JSON data of type {!type:abs1}. *)


