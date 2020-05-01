(* Auto-generated from "test.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

(** This is just a test. *)

type test_variant = [
    `Case1
  | `Case2 of int
  | `Case3 of string
  | `Case4 of test_variant list
]

type ('x, 'y) poly = { fst: 'x list; snd: ('x, 'y) poly option }

type 'a p' =  A | Bb of 'a p' | Ccccc of 'a 

type p = [ `A | `B of r | `C ]

and r = { a: int; mutable b: bool; c: p }

type validated_string_check = string

type validate_me = string list

type val1 = { val1_x: int }

type val2 = { val2_x: val1; val2_y: val1 option }

type unixtime_list = float list

type date = (int * int option * int option)

type mixed_record = {
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

type mixed =
  (
      mixed_record Atdgen_runtime.Util.ocaml_array
    * mixed_record Atdgen_runtime.Util.ocaml_array
  ) list

type test = {
  x0: int option;
  x1: float option;
  x2: mixed;
  x3: mixed_record list;
  x4: Int64.t
}

type tup = (int * test)

type star_rating = int

type 'a generic = { x294623: int }

type specialized = string generic

type some_record = { some_field: int }

type precision = { sqrt2_5: float; small_2: float; large_2: float }

type p'' = int p'

type option_validation = int option

type no_real_wrap = some_record

type natural = Test_lib.Natural.t

type id = [ `Id of string ]

type json_map = (id * int) list

type intopt = int option

type int_assoc_list = (string * int) list

type int_assoc_array = (string * int) Atdgen_runtime.Util.ocaml_array

type int8 = int

type int64 = Int64.t

type int32 = Int32.t

type hello = [ `Hello of string | `World ]

type floats = { f32: float; f64: float }

type extended_tuple = (
    int
  * float
  * bool
  * int option
  * string
  * string list
)

type extended = {
  b0x (*atd b0 *): int;
  b1x (*atd b1 *): bool;
  b2x (*atd b2 *): string;
  b3x (*atd b3 *): string option;
  b4x (*atd b4 *): string option;
  b5x (*atd b5 *): float
}

type even_natural = Test_lib.Even_natural.t

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
type def = Test_lib.Biniou.def

type char = Char.t

type base_tuple = (int * float)

type base = { b0: int; b1: bool }

type 'a array = 'a Atdgen_runtime.Util.ocaml_array

type 'a abs3 = 'a list

type 'a abs2 = 'a list

type 'a abs1 = 'a list

(* Writers for type test_variant *)

val test_variant_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!test_variant}.
      Readers may support more than just this tag. *)

val write_untagged_test_variant :
  Bi_outbuf.t -> test_variant -> unit
  (** Output an untagged biniou value of type {!test_variant}. *)

val write_test_variant :
  Bi_outbuf.t -> test_variant -> unit
  (** Output a biniou value of type {!test_variant}. *)

val string_of_test_variant :
  ?len:int -> test_variant -> string
  (** Serialize a value of type {!test_variant} into
      a biniou string. *)

(* Readers for type test_variant *)

val get_test_variant_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> test_variant)
  (** Return a function that reads an untagged
      biniou value of type {!test_variant}. *)

val read_test_variant :
  Bi_inbuf.t -> test_variant
  (** Input a tagged biniou value of type {!test_variant}. *)

val test_variant_of_string :
  ?pos:int -> string -> test_variant
  (** Deserialize a biniou value of type {!test_variant}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type poly *)

val poly_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!poly}.
      Readers may support more than just this tag. *)

val write_untagged_poly :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'x -> unit) ->
  (Bi_outbuf.t -> 'x -> unit) ->
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'y -> unit) ->
  (Bi_outbuf.t -> 'y -> unit) ->
  Bi_outbuf.t -> ('x, 'y) poly -> unit
  (** Output an untagged biniou value of type {!poly}. *)

val write_poly :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'x -> unit) ->
  (Bi_outbuf.t -> 'x -> unit) ->
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'y -> unit) ->
  (Bi_outbuf.t -> 'y -> unit) ->
  Bi_outbuf.t -> ('x, 'y) poly -> unit
  (** Output a biniou value of type {!poly}. *)

val string_of_poly :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'x -> unit) ->
  (Bi_outbuf.t -> 'x -> unit) ->
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'y -> unit) ->
  (Bi_outbuf.t -> 'y -> unit) ->
  ?len:int -> ('x, 'y) poly -> string
  (** Serialize a value of type {!poly} into
      a biniou string. *)

(* Readers for type poly *)

val get_poly_reader :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'x)) ->
  (Bi_inbuf.t -> 'x) ->
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'y)) ->
  (Bi_inbuf.t -> 'y) ->
  Bi_io.node_tag -> (Bi_inbuf.t -> ('x, 'y) poly)
  (** Return a function that reads an untagged
      biniou value of type {!poly}. *)

val read_poly :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'x)) ->
  (Bi_inbuf.t -> 'x) ->
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'y)) ->
  (Bi_inbuf.t -> 'y) ->
  Bi_inbuf.t -> ('x, 'y) poly
  (** Input a tagged biniou value of type {!poly}. *)

val poly_of_string :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'x)) ->
  (Bi_inbuf.t -> 'x) ->
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'y)) ->
  (Bi_inbuf.t -> 'y) ->
  ?pos:int -> string -> ('x, 'y) poly
  (** Deserialize a biniou value of type {!poly}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_poly :
  fst: 'x list ->
  snd: ('x, 'y) poly option ->
  unit -> ('x, 'y) poly
  (** Create a record of type {!poly}. *)


(* Writers for type p' *)

val p'_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!p'}.
      Readers may support more than just this tag. *)

val write_untagged_p' :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a p' -> unit
  (** Output an untagged biniou value of type {!p'}. *)

val write_p' :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a p' -> unit
  (** Output a biniou value of type {!p'}. *)

val string_of_p' :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  ?len:int -> 'a p' -> string
  (** Serialize a value of type {!p'} into
      a biniou string. *)

(* Readers for type p' *)

val get_p'_reader :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  Bi_io.node_tag -> (Bi_inbuf.t -> 'a p')
  (** Return a function that reads an untagged
      biniou value of type {!p'}. *)

val read_p' :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  Bi_inbuf.t -> 'a p'
  (** Input a tagged biniou value of type {!p'}. *)

val p'_of_string :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  ?pos:int -> string -> 'a p'
  (** Deserialize a biniou value of type {!p'}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type p *)

val p_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!p}.
      Readers may support more than just this tag. *)

val write_untagged_p :
  Bi_outbuf.t -> p -> unit
  (** Output an untagged biniou value of type {!p}. *)

val write_p :
  Bi_outbuf.t -> p -> unit
  (** Output a biniou value of type {!p}. *)

val string_of_p :
  ?len:int -> p -> string
  (** Serialize a value of type {!p} into
      a biniou string. *)

(* Readers for type p *)

val get_p_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> p)
  (** Return a function that reads an untagged
      biniou value of type {!p}. *)

val read_p :
  Bi_inbuf.t -> p
  (** Input a tagged biniou value of type {!p}. *)

val p_of_string :
  ?pos:int -> string -> p
  (** Deserialize a biniou value of type {!p}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type r *)

val r_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!r}.
      Readers may support more than just this tag. *)

val write_untagged_r :
  Bi_outbuf.t -> r -> unit
  (** Output an untagged biniou value of type {!r}. *)

val write_r :
  Bi_outbuf.t -> r -> unit
  (** Output a biniou value of type {!r}. *)

val string_of_r :
  ?len:int -> r -> string
  (** Serialize a value of type {!r} into
      a biniou string. *)

(* Readers for type r *)

val get_r_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> r)
  (** Return a function that reads an untagged
      biniou value of type {!r}. *)

val read_r :
  Bi_inbuf.t -> r
  (** Input a tagged biniou value of type {!r}. *)

val r_of_string :
  ?pos:int -> string -> r
  (** Deserialize a biniou value of type {!r}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_r :
  a: int ->
  b: bool ->
  c: p ->
  unit -> r
  (** Create a record of type {!r}. *)


(* Writers for type validated_string_check *)

val validated_string_check_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!validated_string_check}.
      Readers may support more than just this tag. *)

val write_untagged_validated_string_check :
  Bi_outbuf.t -> validated_string_check -> unit
  (** Output an untagged biniou value of type {!validated_string_check}. *)

val write_validated_string_check :
  Bi_outbuf.t -> validated_string_check -> unit
  (** Output a biniou value of type {!validated_string_check}. *)

val string_of_validated_string_check :
  ?len:int -> validated_string_check -> string
  (** Serialize a value of type {!validated_string_check} into
      a biniou string. *)

(* Readers for type validated_string_check *)

val get_validated_string_check_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> validated_string_check)
  (** Return a function that reads an untagged
      biniou value of type {!validated_string_check}. *)

val read_validated_string_check :
  Bi_inbuf.t -> validated_string_check
  (** Input a tagged biniou value of type {!validated_string_check}. *)

val validated_string_check_of_string :
  ?pos:int -> string -> validated_string_check
  (** Deserialize a biniou value of type {!validated_string_check}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type validate_me *)

val validate_me_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!validate_me}.
      Readers may support more than just this tag. *)

val write_untagged_validate_me :
  Bi_outbuf.t -> validate_me -> unit
  (** Output an untagged biniou value of type {!validate_me}. *)

val write_validate_me :
  Bi_outbuf.t -> validate_me -> unit
  (** Output a biniou value of type {!validate_me}. *)

val string_of_validate_me :
  ?len:int -> validate_me -> string
  (** Serialize a value of type {!validate_me} into
      a biniou string. *)

(* Readers for type validate_me *)

val get_validate_me_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> validate_me)
  (** Return a function that reads an untagged
      biniou value of type {!validate_me}. *)

val read_validate_me :
  Bi_inbuf.t -> validate_me
  (** Input a tagged biniou value of type {!validate_me}. *)

val validate_me_of_string :
  ?pos:int -> string -> validate_me
  (** Deserialize a biniou value of type {!validate_me}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type val1 *)

val val1_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!val1}.
      Readers may support more than just this tag. *)

val write_untagged_val1 :
  Bi_outbuf.t -> val1 -> unit
  (** Output an untagged biniou value of type {!val1}. *)

val write_val1 :
  Bi_outbuf.t -> val1 -> unit
  (** Output a biniou value of type {!val1}. *)

val string_of_val1 :
  ?len:int -> val1 -> string
  (** Serialize a value of type {!val1} into
      a biniou string. *)

(* Readers for type val1 *)

val get_val1_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> val1)
  (** Return a function that reads an untagged
      biniou value of type {!val1}. *)

val read_val1 :
  Bi_inbuf.t -> val1
  (** Input a tagged biniou value of type {!val1}. *)

val val1_of_string :
  ?pos:int -> string -> val1
  (** Deserialize a biniou value of type {!val1}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_val1 :
  val1_x: int ->
  unit -> val1
  (** Create a record of type {!val1}. *)


(* Writers for type val2 *)

val val2_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!val2}.
      Readers may support more than just this tag. *)

val write_untagged_val2 :
  Bi_outbuf.t -> val2 -> unit
  (** Output an untagged biniou value of type {!val2}. *)

val write_val2 :
  Bi_outbuf.t -> val2 -> unit
  (** Output a biniou value of type {!val2}. *)

val string_of_val2 :
  ?len:int -> val2 -> string
  (** Serialize a value of type {!val2} into
      a biniou string. *)

(* Readers for type val2 *)

val get_val2_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> val2)
  (** Return a function that reads an untagged
      biniou value of type {!val2}. *)

val read_val2 :
  Bi_inbuf.t -> val2
  (** Input a tagged biniou value of type {!val2}. *)

val val2_of_string :
  ?pos:int -> string -> val2
  (** Deserialize a biniou value of type {!val2}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_val2 :
  val2_x: val1 ->
  ?val2_y: val1 ->
  unit -> val2
  (** Create a record of type {!val2}. *)


(* Writers for type unixtime_list *)

val unixtime_list_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!unixtime_list}.
      Readers may support more than just this tag. *)

val write_untagged_unixtime_list :
  Bi_outbuf.t -> unixtime_list -> unit
  (** Output an untagged biniou value of type {!unixtime_list}. *)

val write_unixtime_list :
  Bi_outbuf.t -> unixtime_list -> unit
  (** Output a biniou value of type {!unixtime_list}. *)

val string_of_unixtime_list :
  ?len:int -> unixtime_list -> string
  (** Serialize a value of type {!unixtime_list} into
      a biniou string. *)

(* Readers for type unixtime_list *)

val get_unixtime_list_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> unixtime_list)
  (** Return a function that reads an untagged
      biniou value of type {!unixtime_list}. *)

val read_unixtime_list :
  Bi_inbuf.t -> unixtime_list
  (** Input a tagged biniou value of type {!unixtime_list}. *)

val unixtime_list_of_string :
  ?pos:int -> string -> unixtime_list
  (** Deserialize a biniou value of type {!unixtime_list}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type date *)

val date_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!date}.
      Readers may support more than just this tag. *)

val write_untagged_date :
  Bi_outbuf.t -> date -> unit
  (** Output an untagged biniou value of type {!date}. *)

val write_date :
  Bi_outbuf.t -> date -> unit
  (** Output a biniou value of type {!date}. *)

val string_of_date :
  ?len:int -> date -> string
  (** Serialize a value of type {!date} into
      a biniou string. *)

(* Readers for type date *)

val get_date_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> date)
  (** Return a function that reads an untagged
      biniou value of type {!date}. *)

val read_date :
  Bi_inbuf.t -> date
  (** Input a tagged biniou value of type {!date}. *)

val date_of_string :
  ?pos:int -> string -> date
  (** Deserialize a biniou value of type {!date}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type mixed_record *)

val mixed_record_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!mixed_record}.
      Readers may support more than just this tag. *)

val write_untagged_mixed_record :
  Bi_outbuf.t -> mixed_record -> unit
  (** Output an untagged biniou value of type {!mixed_record}. *)

val write_mixed_record :
  Bi_outbuf.t -> mixed_record -> unit
  (** Output a biniou value of type {!mixed_record}. *)

val string_of_mixed_record :
  ?len:int -> mixed_record -> string
  (** Serialize a value of type {!mixed_record} into
      a biniou string. *)

(* Readers for type mixed_record *)

val get_mixed_record_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> mixed_record)
  (** Return a function that reads an untagged
      biniou value of type {!mixed_record}. *)

val read_mixed_record :
  Bi_inbuf.t -> mixed_record
  (** Input a tagged biniou value of type {!mixed_record}. *)

val mixed_record_of_string :
  ?pos:int -> string -> mixed_record
  (** Deserialize a biniou value of type {!mixed_record}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

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


(* Writers for type mixed *)

val mixed_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!mixed}.
      Readers may support more than just this tag. *)

val write_untagged_mixed :
  Bi_outbuf.t -> mixed -> unit
  (** Output an untagged biniou value of type {!mixed}. *)

val write_mixed :
  Bi_outbuf.t -> mixed -> unit
  (** Output a biniou value of type {!mixed}. *)

val string_of_mixed :
  ?len:int -> mixed -> string
  (** Serialize a value of type {!mixed} into
      a biniou string. *)

(* Readers for type mixed *)

val get_mixed_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> mixed)
  (** Return a function that reads an untagged
      biniou value of type {!mixed}. *)

val read_mixed :
  Bi_inbuf.t -> mixed
  (** Input a tagged biniou value of type {!mixed}. *)

val mixed_of_string :
  ?pos:int -> string -> mixed
  (** Deserialize a biniou value of type {!mixed}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type test *)

val test_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!test}.
      Readers may support more than just this tag. *)

val write_untagged_test :
  Bi_outbuf.t -> test -> unit
  (** Output an untagged biniou value of type {!test}. *)

val write_test :
  Bi_outbuf.t -> test -> unit
  (** Output a biniou value of type {!test}. *)

val string_of_test :
  ?len:int -> test -> string
  (** Serialize a value of type {!test} into
      a biniou string. *)

(* Readers for type test *)

val get_test_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> test)
  (** Return a function that reads an untagged
      biniou value of type {!test}. *)

val read_test :
  Bi_inbuf.t -> test
  (** Input a tagged biniou value of type {!test}. *)

val test_of_string :
  ?pos:int -> string -> test
  (** Deserialize a biniou value of type {!test}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_test :
  ?x0: int ->
  ?x1: float ->
  x2: mixed ->
  x3: mixed_record list ->
  x4: Int64.t ->
  unit -> test
  (** Create a record of type {!test}. *)


(* Writers for type tup *)

val tup_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!tup}.
      Readers may support more than just this tag. *)

val write_untagged_tup :
  Bi_outbuf.t -> tup -> unit
  (** Output an untagged biniou value of type {!tup}. *)

val write_tup :
  Bi_outbuf.t -> tup -> unit
  (** Output a biniou value of type {!tup}. *)

val string_of_tup :
  ?len:int -> tup -> string
  (** Serialize a value of type {!tup} into
      a biniou string. *)

(* Readers for type tup *)

val get_tup_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> tup)
  (** Return a function that reads an untagged
      biniou value of type {!tup}. *)

val read_tup :
  Bi_inbuf.t -> tup
  (** Input a tagged biniou value of type {!tup}. *)

val tup_of_string :
  ?pos:int -> string -> tup
  (** Deserialize a biniou value of type {!tup}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type star_rating *)

val star_rating_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!star_rating}.
      Readers may support more than just this tag. *)

val write_untagged_star_rating :
  Bi_outbuf.t -> star_rating -> unit
  (** Output an untagged biniou value of type {!star_rating}. *)

val write_star_rating :
  Bi_outbuf.t -> star_rating -> unit
  (** Output a biniou value of type {!star_rating}. *)

val string_of_star_rating :
  ?len:int -> star_rating -> string
  (** Serialize a value of type {!star_rating} into
      a biniou string. *)

(* Readers for type star_rating *)

val get_star_rating_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> star_rating)
  (** Return a function that reads an untagged
      biniou value of type {!star_rating}. *)

val read_star_rating :
  Bi_inbuf.t -> star_rating
  (** Input a tagged biniou value of type {!star_rating}. *)

val star_rating_of_string :
  ?pos:int -> string -> star_rating
  (** Deserialize a biniou value of type {!star_rating}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type generic *)

val generic_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!generic}.
      Readers may support more than just this tag. *)

val write_untagged_generic :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a generic -> unit
  (** Output an untagged biniou value of type {!generic}. *)

val write_generic :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a generic -> unit
  (** Output a biniou value of type {!generic}. *)

val string_of_generic :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  ?len:int -> 'a generic -> string
  (** Serialize a value of type {!generic} into
      a biniou string. *)

(* Readers for type generic *)

val get_generic_reader :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  Bi_io.node_tag -> (Bi_inbuf.t -> 'a generic)
  (** Return a function that reads an untagged
      biniou value of type {!generic}. *)

val read_generic :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  Bi_inbuf.t -> 'a generic
  (** Input a tagged biniou value of type {!generic}. *)

val generic_of_string :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  ?pos:int -> string -> 'a generic
  (** Deserialize a biniou value of type {!generic}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_generic :
  x294623: int ->
  unit -> 'a generic
  (** Create a record of type {!generic}. *)


(* Writers for type specialized *)

val specialized_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!specialized}.
      Readers may support more than just this tag. *)

val write_untagged_specialized :
  Bi_outbuf.t -> specialized -> unit
  (** Output an untagged biniou value of type {!specialized}. *)

val write_specialized :
  Bi_outbuf.t -> specialized -> unit
  (** Output a biniou value of type {!specialized}. *)

val string_of_specialized :
  ?len:int -> specialized -> string
  (** Serialize a value of type {!specialized} into
      a biniou string. *)

(* Readers for type specialized *)

val get_specialized_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> specialized)
  (** Return a function that reads an untagged
      biniou value of type {!specialized}. *)

val read_specialized :
  Bi_inbuf.t -> specialized
  (** Input a tagged biniou value of type {!specialized}. *)

val specialized_of_string :
  ?pos:int -> string -> specialized
  (** Deserialize a biniou value of type {!specialized}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type some_record *)

val some_record_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!some_record}.
      Readers may support more than just this tag. *)

val write_untagged_some_record :
  Bi_outbuf.t -> some_record -> unit
  (** Output an untagged biniou value of type {!some_record}. *)

val write_some_record :
  Bi_outbuf.t -> some_record -> unit
  (** Output a biniou value of type {!some_record}. *)

val string_of_some_record :
  ?len:int -> some_record -> string
  (** Serialize a value of type {!some_record} into
      a biniou string. *)

(* Readers for type some_record *)

val get_some_record_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> some_record)
  (** Return a function that reads an untagged
      biniou value of type {!some_record}. *)

val read_some_record :
  Bi_inbuf.t -> some_record
  (** Input a tagged biniou value of type {!some_record}. *)

val some_record_of_string :
  ?pos:int -> string -> some_record
  (** Deserialize a biniou value of type {!some_record}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_some_record :
  some_field: int ->
  unit -> some_record
  (** Create a record of type {!some_record}. *)


(* Writers for type precision *)

val precision_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!precision}.
      Readers may support more than just this tag. *)

val write_untagged_precision :
  Bi_outbuf.t -> precision -> unit
  (** Output an untagged biniou value of type {!precision}. *)

val write_precision :
  Bi_outbuf.t -> precision -> unit
  (** Output a biniou value of type {!precision}. *)

val string_of_precision :
  ?len:int -> precision -> string
  (** Serialize a value of type {!precision} into
      a biniou string. *)

(* Readers for type precision *)

val get_precision_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> precision)
  (** Return a function that reads an untagged
      biniou value of type {!precision}. *)

val read_precision :
  Bi_inbuf.t -> precision
  (** Input a tagged biniou value of type {!precision}. *)

val precision_of_string :
  ?pos:int -> string -> precision
  (** Deserialize a biniou value of type {!precision}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_precision :
  sqrt2_5: float ->
  small_2: float ->
  large_2: float ->
  unit -> precision
  (** Create a record of type {!precision}. *)


(* Writers for type p'' *)

val p''_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!p''}.
      Readers may support more than just this tag. *)

val write_untagged_p'' :
  Bi_outbuf.t -> p'' -> unit
  (** Output an untagged biniou value of type {!p''}. *)

val write_p'' :
  Bi_outbuf.t -> p'' -> unit
  (** Output a biniou value of type {!p''}. *)

val string_of_p'' :
  ?len:int -> p'' -> string
  (** Serialize a value of type {!p''} into
      a biniou string. *)

(* Readers for type p'' *)

val get_p''_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> p'')
  (** Return a function that reads an untagged
      biniou value of type {!p''}. *)

val read_p'' :
  Bi_inbuf.t -> p''
  (** Input a tagged biniou value of type {!p''}. *)

val p''_of_string :
  ?pos:int -> string -> p''
  (** Deserialize a biniou value of type {!p''}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type option_validation *)

val option_validation_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!option_validation}.
      Readers may support more than just this tag. *)

val write_untagged_option_validation :
  Bi_outbuf.t -> option_validation -> unit
  (** Output an untagged biniou value of type {!option_validation}. *)

val write_option_validation :
  Bi_outbuf.t -> option_validation -> unit
  (** Output a biniou value of type {!option_validation}. *)

val string_of_option_validation :
  ?len:int -> option_validation -> string
  (** Serialize a value of type {!option_validation} into
      a biniou string. *)

(* Readers for type option_validation *)

val get_option_validation_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> option_validation)
  (** Return a function that reads an untagged
      biniou value of type {!option_validation}. *)

val read_option_validation :
  Bi_inbuf.t -> option_validation
  (** Input a tagged biniou value of type {!option_validation}. *)

val option_validation_of_string :
  ?pos:int -> string -> option_validation
  (** Deserialize a biniou value of type {!option_validation}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type no_real_wrap *)

val no_real_wrap_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!no_real_wrap}.
      Readers may support more than just this tag. *)

val write_untagged_no_real_wrap :
  Bi_outbuf.t -> no_real_wrap -> unit
  (** Output an untagged biniou value of type {!no_real_wrap}. *)

val write_no_real_wrap :
  Bi_outbuf.t -> no_real_wrap -> unit
  (** Output a biniou value of type {!no_real_wrap}. *)

val string_of_no_real_wrap :
  ?len:int -> no_real_wrap -> string
  (** Serialize a value of type {!no_real_wrap} into
      a biniou string. *)

(* Readers for type no_real_wrap *)

val get_no_real_wrap_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> no_real_wrap)
  (** Return a function that reads an untagged
      biniou value of type {!no_real_wrap}. *)

val read_no_real_wrap :
  Bi_inbuf.t -> no_real_wrap
  (** Input a tagged biniou value of type {!no_real_wrap}. *)

val no_real_wrap_of_string :
  ?pos:int -> string -> no_real_wrap
  (** Deserialize a biniou value of type {!no_real_wrap}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type natural *)

val natural_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!natural}.
      Readers may support more than just this tag. *)

val write_untagged_natural :
  Bi_outbuf.t -> natural -> unit
  (** Output an untagged biniou value of type {!natural}. *)

val write_natural :
  Bi_outbuf.t -> natural -> unit
  (** Output a biniou value of type {!natural}. *)

val string_of_natural :
  ?len:int -> natural -> string
  (** Serialize a value of type {!natural} into
      a biniou string. *)

(* Readers for type natural *)

val get_natural_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> natural)
  (** Return a function that reads an untagged
      biniou value of type {!natural}. *)

val read_natural :
  Bi_inbuf.t -> natural
  (** Input a tagged biniou value of type {!natural}. *)

val natural_of_string :
  ?pos:int -> string -> natural
  (** Deserialize a biniou value of type {!natural}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type id *)

val id_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!id}.
      Readers may support more than just this tag. *)

val write_untagged_id :
  Bi_outbuf.t -> id -> unit
  (** Output an untagged biniou value of type {!id}. *)

val write_id :
  Bi_outbuf.t -> id -> unit
  (** Output a biniou value of type {!id}. *)

val string_of_id :
  ?len:int -> id -> string
  (** Serialize a value of type {!id} into
      a biniou string. *)

(* Readers for type id *)

val get_id_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> id)
  (** Return a function that reads an untagged
      biniou value of type {!id}. *)

val read_id :
  Bi_inbuf.t -> id
  (** Input a tagged biniou value of type {!id}. *)

val id_of_string :
  ?pos:int -> string -> id
  (** Deserialize a biniou value of type {!id}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type json_map *)

val json_map_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!json_map}.
      Readers may support more than just this tag. *)

val write_untagged_json_map :
  Bi_outbuf.t -> json_map -> unit
  (** Output an untagged biniou value of type {!json_map}. *)

val write_json_map :
  Bi_outbuf.t -> json_map -> unit
  (** Output a biniou value of type {!json_map}. *)

val string_of_json_map :
  ?len:int -> json_map -> string
  (** Serialize a value of type {!json_map} into
      a biniou string. *)

(* Readers for type json_map *)

val get_json_map_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> json_map)
  (** Return a function that reads an untagged
      biniou value of type {!json_map}. *)

val read_json_map :
  Bi_inbuf.t -> json_map
  (** Input a tagged biniou value of type {!json_map}. *)

val json_map_of_string :
  ?pos:int -> string -> json_map
  (** Deserialize a biniou value of type {!json_map}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type intopt *)

val intopt_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!intopt}.
      Readers may support more than just this tag. *)

val write_untagged_intopt :
  Bi_outbuf.t -> intopt -> unit
  (** Output an untagged biniou value of type {!intopt}. *)

val write_intopt :
  Bi_outbuf.t -> intopt -> unit
  (** Output a biniou value of type {!intopt}. *)

val string_of_intopt :
  ?len:int -> intopt -> string
  (** Serialize a value of type {!intopt} into
      a biniou string. *)

(* Readers for type intopt *)

val get_intopt_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> intopt)
  (** Return a function that reads an untagged
      biniou value of type {!intopt}. *)

val read_intopt :
  Bi_inbuf.t -> intopt
  (** Input a tagged biniou value of type {!intopt}. *)

val intopt_of_string :
  ?pos:int -> string -> intopt
  (** Deserialize a biniou value of type {!intopt}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type int_assoc_list *)

val int_assoc_list_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!int_assoc_list}.
      Readers may support more than just this tag. *)

val write_untagged_int_assoc_list :
  Bi_outbuf.t -> int_assoc_list -> unit
  (** Output an untagged biniou value of type {!int_assoc_list}. *)

val write_int_assoc_list :
  Bi_outbuf.t -> int_assoc_list -> unit
  (** Output a biniou value of type {!int_assoc_list}. *)

val string_of_int_assoc_list :
  ?len:int -> int_assoc_list -> string
  (** Serialize a value of type {!int_assoc_list} into
      a biniou string. *)

(* Readers for type int_assoc_list *)

val get_int_assoc_list_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> int_assoc_list)
  (** Return a function that reads an untagged
      biniou value of type {!int_assoc_list}. *)

val read_int_assoc_list :
  Bi_inbuf.t -> int_assoc_list
  (** Input a tagged biniou value of type {!int_assoc_list}. *)

val int_assoc_list_of_string :
  ?pos:int -> string -> int_assoc_list
  (** Deserialize a biniou value of type {!int_assoc_list}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type int_assoc_array *)

val int_assoc_array_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!int_assoc_array}.
      Readers may support more than just this tag. *)

val write_untagged_int_assoc_array :
  Bi_outbuf.t -> int_assoc_array -> unit
  (** Output an untagged biniou value of type {!int_assoc_array}. *)

val write_int_assoc_array :
  Bi_outbuf.t -> int_assoc_array -> unit
  (** Output a biniou value of type {!int_assoc_array}. *)

val string_of_int_assoc_array :
  ?len:int -> int_assoc_array -> string
  (** Serialize a value of type {!int_assoc_array} into
      a biniou string. *)

(* Readers for type int_assoc_array *)

val get_int_assoc_array_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> int_assoc_array)
  (** Return a function that reads an untagged
      biniou value of type {!int_assoc_array}. *)

val read_int_assoc_array :
  Bi_inbuf.t -> int_assoc_array
  (** Input a tagged biniou value of type {!int_assoc_array}. *)

val int_assoc_array_of_string :
  ?pos:int -> string -> int_assoc_array
  (** Deserialize a biniou value of type {!int_assoc_array}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type int8 *)

val int8_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!int8}.
      Readers may support more than just this tag. *)

val write_untagged_int8 :
  Bi_outbuf.t -> int8 -> unit
  (** Output an untagged biniou value of type {!int8}. *)

val write_int8 :
  Bi_outbuf.t -> int8 -> unit
  (** Output a biniou value of type {!int8}. *)

val string_of_int8 :
  ?len:int -> int8 -> string
  (** Serialize a value of type {!int8} into
      a biniou string. *)

(* Readers for type int8 *)

val get_int8_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> int8)
  (** Return a function that reads an untagged
      biniou value of type {!int8}. *)

val read_int8 :
  Bi_inbuf.t -> int8
  (** Input a tagged biniou value of type {!int8}. *)

val int8_of_string :
  ?pos:int -> string -> int8
  (** Deserialize a biniou value of type {!int8}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type int64 *)

val int64_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!int64}.
      Readers may support more than just this tag. *)

val write_untagged_int64 :
  Bi_outbuf.t -> int64 -> unit
  (** Output an untagged biniou value of type {!int64}. *)

val write_int64 :
  Bi_outbuf.t -> int64 -> unit
  (** Output a biniou value of type {!int64}. *)

val string_of_int64 :
  ?len:int -> int64 -> string
  (** Serialize a value of type {!int64} into
      a biniou string. *)

(* Readers for type int64 *)

val get_int64_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> int64)
  (** Return a function that reads an untagged
      biniou value of type {!int64}. *)

val read_int64 :
  Bi_inbuf.t -> int64
  (** Input a tagged biniou value of type {!int64}. *)

val int64_of_string :
  ?pos:int -> string -> int64
  (** Deserialize a biniou value of type {!int64}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type int32 *)

val int32_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!int32}.
      Readers may support more than just this tag. *)

val write_untagged_int32 :
  Bi_outbuf.t -> int32 -> unit
  (** Output an untagged biniou value of type {!int32}. *)

val write_int32 :
  Bi_outbuf.t -> int32 -> unit
  (** Output a biniou value of type {!int32}. *)

val string_of_int32 :
  ?len:int -> int32 -> string
  (** Serialize a value of type {!int32} into
      a biniou string. *)

(* Readers for type int32 *)

val get_int32_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> int32)
  (** Return a function that reads an untagged
      biniou value of type {!int32}. *)

val read_int32 :
  Bi_inbuf.t -> int32
  (** Input a tagged biniou value of type {!int32}. *)

val int32_of_string :
  ?pos:int -> string -> int32
  (** Deserialize a biniou value of type {!int32}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type hello *)

val hello_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!hello}.
      Readers may support more than just this tag. *)

val write_untagged_hello :
  Bi_outbuf.t -> hello -> unit
  (** Output an untagged biniou value of type {!hello}. *)

val write_hello :
  Bi_outbuf.t -> hello -> unit
  (** Output a biniou value of type {!hello}. *)

val string_of_hello :
  ?len:int -> hello -> string
  (** Serialize a value of type {!hello} into
      a biniou string. *)

(* Readers for type hello *)

val get_hello_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> hello)
  (** Return a function that reads an untagged
      biniou value of type {!hello}. *)

val read_hello :
  Bi_inbuf.t -> hello
  (** Input a tagged biniou value of type {!hello}. *)

val hello_of_string :
  ?pos:int -> string -> hello
  (** Deserialize a biniou value of type {!hello}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type floats *)

val floats_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!floats}.
      Readers may support more than just this tag. *)

val write_untagged_floats :
  Bi_outbuf.t -> floats -> unit
  (** Output an untagged biniou value of type {!floats}. *)

val write_floats :
  Bi_outbuf.t -> floats -> unit
  (** Output a biniou value of type {!floats}. *)

val string_of_floats :
  ?len:int -> floats -> string
  (** Serialize a value of type {!floats} into
      a biniou string. *)

(* Readers for type floats *)

val get_floats_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> floats)
  (** Return a function that reads an untagged
      biniou value of type {!floats}. *)

val read_floats :
  Bi_inbuf.t -> floats
  (** Input a tagged biniou value of type {!floats}. *)

val floats_of_string :
  ?pos:int -> string -> floats
  (** Deserialize a biniou value of type {!floats}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_floats :
  f32: float ->
  f64: float ->
  unit -> floats
  (** Create a record of type {!floats}. *)


(* Writers for type extended_tuple *)

val extended_tuple_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!extended_tuple}.
      Readers may support more than just this tag. *)

val write_untagged_extended_tuple :
  Bi_outbuf.t -> extended_tuple -> unit
  (** Output an untagged biniou value of type {!extended_tuple}. *)

val write_extended_tuple :
  Bi_outbuf.t -> extended_tuple -> unit
  (** Output a biniou value of type {!extended_tuple}. *)

val string_of_extended_tuple :
  ?len:int -> extended_tuple -> string
  (** Serialize a value of type {!extended_tuple} into
      a biniou string. *)

(* Readers for type extended_tuple *)

val get_extended_tuple_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> extended_tuple)
  (** Return a function that reads an untagged
      biniou value of type {!extended_tuple}. *)

val read_extended_tuple :
  Bi_inbuf.t -> extended_tuple
  (** Input a tagged biniou value of type {!extended_tuple}. *)

val extended_tuple_of_string :
  ?pos:int -> string -> extended_tuple
  (** Deserialize a biniou value of type {!extended_tuple}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type extended *)

val extended_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!extended}.
      Readers may support more than just this tag. *)

val write_untagged_extended :
  Bi_outbuf.t -> extended -> unit
  (** Output an untagged biniou value of type {!extended}. *)

val write_extended :
  Bi_outbuf.t -> extended -> unit
  (** Output a biniou value of type {!extended}. *)

val string_of_extended :
  ?len:int -> extended -> string
  (** Serialize a value of type {!extended} into
      a biniou string. *)

(* Readers for type extended *)

val get_extended_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> extended)
  (** Return a function that reads an untagged
      biniou value of type {!extended}. *)

val read_extended :
  Bi_inbuf.t -> extended
  (** Input a tagged biniou value of type {!extended}. *)

val extended_of_string :
  ?pos:int -> string -> extended
  (** Deserialize a biniou value of type {!extended}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_extended :
  b0x: int ->
  b1x: bool ->
  b2x: string ->
  ?b3x: string ->
  b4x: string option ->
  ?b5x: float ->
  unit -> extended
  (** Create a record of type {!extended}. *)


(* Writers for type even_natural *)

val even_natural_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!even_natural}.
      Readers may support more than just this tag. *)

val write_untagged_even_natural :
  Bi_outbuf.t -> even_natural -> unit
  (** Output an untagged biniou value of type {!even_natural}. *)

val write_even_natural :
  Bi_outbuf.t -> even_natural -> unit
  (** Output a biniou value of type {!even_natural}. *)

val string_of_even_natural :
  ?len:int -> even_natural -> string
  (** Serialize a value of type {!even_natural} into
      a biniou string. *)

(* Readers for type even_natural *)

val get_even_natural_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> even_natural)
  (** Return a function that reads an untagged
      biniou value of type {!even_natural}. *)

val read_even_natural :
  Bi_inbuf.t -> even_natural
  (** Input a tagged biniou value of type {!even_natural}. *)

val even_natural_of_string :
  ?pos:int -> string -> even_natural
  (** Deserialize a biniou value of type {!even_natural}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type def *)

val def_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!def}.
      Readers may support more than just this tag. *)

val write_untagged_def :
  Bi_outbuf.t -> def -> unit
  (** Output an untagged biniou value of type {!def}. *)

val write_def :
  Bi_outbuf.t -> def -> unit
  (** Output a biniou value of type {!def}. *)

val string_of_def :
  ?len:int -> def -> string
  (** Serialize a value of type {!def} into
      a biniou string. *)

(* Readers for type def *)

val get_def_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> def)
  (** Return a function that reads an untagged
      biniou value of type {!def}. *)

val read_def :
  Bi_inbuf.t -> def
  (** Input a tagged biniou value of type {!def}. *)

val def_of_string :
  ?pos:int -> string -> def
  (** Deserialize a biniou value of type {!def}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type char *)

val char_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!char}.
      Readers may support more than just this tag. *)

val write_untagged_char :
  Bi_outbuf.t -> char -> unit
  (** Output an untagged biniou value of type {!char}. *)

val write_char :
  Bi_outbuf.t -> char -> unit
  (** Output a biniou value of type {!char}. *)

val string_of_char :
  ?len:int -> char -> string
  (** Serialize a value of type {!char} into
      a biniou string. *)

(* Readers for type char *)

val get_char_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> char)
  (** Return a function that reads an untagged
      biniou value of type {!char}. *)

val read_char :
  Bi_inbuf.t -> char
  (** Input a tagged biniou value of type {!char}. *)

val char_of_string :
  ?pos:int -> string -> char
  (** Deserialize a biniou value of type {!char}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type base_tuple *)

val base_tuple_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!base_tuple}.
      Readers may support more than just this tag. *)

val write_untagged_base_tuple :
  Bi_outbuf.t -> base_tuple -> unit
  (** Output an untagged biniou value of type {!base_tuple}. *)

val write_base_tuple :
  Bi_outbuf.t -> base_tuple -> unit
  (** Output a biniou value of type {!base_tuple}. *)

val string_of_base_tuple :
  ?len:int -> base_tuple -> string
  (** Serialize a value of type {!base_tuple} into
      a biniou string. *)

(* Readers for type base_tuple *)

val get_base_tuple_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> base_tuple)
  (** Return a function that reads an untagged
      biniou value of type {!base_tuple}. *)

val read_base_tuple :
  Bi_inbuf.t -> base_tuple
  (** Input a tagged biniou value of type {!base_tuple}. *)

val base_tuple_of_string :
  ?pos:int -> string -> base_tuple
  (** Deserialize a biniou value of type {!base_tuple}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type base *)

val base_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!base}.
      Readers may support more than just this tag. *)

val write_untagged_base :
  Bi_outbuf.t -> base -> unit
  (** Output an untagged biniou value of type {!base}. *)

val write_base :
  Bi_outbuf.t -> base -> unit
  (** Output a biniou value of type {!base}. *)

val string_of_base :
  ?len:int -> base -> string
  (** Serialize a value of type {!base} into
      a biniou string. *)

(* Readers for type base *)

val get_base_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> base)
  (** Return a function that reads an untagged
      biniou value of type {!base}. *)

val read_base :
  Bi_inbuf.t -> base
  (** Input a tagged biniou value of type {!base}. *)

val base_of_string :
  ?pos:int -> string -> base
  (** Deserialize a biniou value of type {!base}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_base :
  b0: int ->
  b1: bool ->
  unit -> base
  (** Create a record of type {!base}. *)


(* Writers for type array *)

val array_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!array}.
      Readers may support more than just this tag. *)

val write_untagged_array :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a array -> unit
  (** Output an untagged biniou value of type {!array}. *)

val write_array :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a array -> unit
  (** Output a biniou value of type {!array}. *)

val string_of_array :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  ?len:int -> 'a array -> string
  (** Serialize a value of type {!array} into
      a biniou string. *)

(* Readers for type array *)

val get_array_reader :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  Bi_io.node_tag -> (Bi_inbuf.t -> 'a array)
  (** Return a function that reads an untagged
      biniou value of type {!array}. *)

val read_array :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  Bi_inbuf.t -> 'a array
  (** Input a tagged biniou value of type {!array}. *)

val array_of_string :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  ?pos:int -> string -> 'a array
  (** Deserialize a biniou value of type {!array}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type abs3 *)

val abs3_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!abs3}.
      Readers may support more than just this tag. *)

val write_untagged_abs3 :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a abs3 -> unit
  (** Output an untagged biniou value of type {!abs3}. *)

val write_abs3 :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a abs3 -> unit
  (** Output a biniou value of type {!abs3}. *)

val string_of_abs3 :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  ?len:int -> 'a abs3 -> string
  (** Serialize a value of type {!abs3} into
      a biniou string. *)

(* Readers for type abs3 *)

val get_abs3_reader :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  Bi_io.node_tag -> (Bi_inbuf.t -> 'a abs3)
  (** Return a function that reads an untagged
      biniou value of type {!abs3}. *)

val read_abs3 :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  Bi_inbuf.t -> 'a abs3
  (** Input a tagged biniou value of type {!abs3}. *)

val abs3_of_string :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  ?pos:int -> string -> 'a abs3
  (** Deserialize a biniou value of type {!abs3}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type abs2 *)

val abs2_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!abs2}.
      Readers may support more than just this tag. *)

val write_untagged_abs2 :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a abs2 -> unit
  (** Output an untagged biniou value of type {!abs2}. *)

val write_abs2 :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a abs2 -> unit
  (** Output a biniou value of type {!abs2}. *)

val string_of_abs2 :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  ?len:int -> 'a abs2 -> string
  (** Serialize a value of type {!abs2} into
      a biniou string. *)

(* Readers for type abs2 *)

val get_abs2_reader :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  Bi_io.node_tag -> (Bi_inbuf.t -> 'a abs2)
  (** Return a function that reads an untagged
      biniou value of type {!abs2}. *)

val read_abs2 :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  Bi_inbuf.t -> 'a abs2
  (** Input a tagged biniou value of type {!abs2}. *)

val abs2_of_string :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  ?pos:int -> string -> 'a abs2
  (** Deserialize a biniou value of type {!abs2}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type abs1 *)

val abs1_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!abs1}.
      Readers may support more than just this tag. *)

val write_untagged_abs1 :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a abs1 -> unit
  (** Output an untagged biniou value of type {!abs1}. *)

val write_abs1 :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a abs1 -> unit
  (** Output a biniou value of type {!abs1}. *)

val string_of_abs1 :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'a -> unit) ->
  (Bi_outbuf.t -> 'a -> unit) ->
  ?len:int -> 'a abs1 -> string
  (** Serialize a value of type {!abs1} into
      a biniou string. *)

(* Readers for type abs1 *)

val get_abs1_reader :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  Bi_io.node_tag -> (Bi_inbuf.t -> 'a abs1)
  (** Return a function that reads an untagged
      biniou value of type {!abs1}. *)

val read_abs1 :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  Bi_inbuf.t -> 'a abs1
  (** Input a tagged biniou value of type {!abs1}. *)

val abs1_of_string :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'a)) ->
  (Bi_inbuf.t -> 'a) ->
  ?pos:int -> string -> 'a abs1
  (** Deserialize a biniou value of type {!abs1}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


