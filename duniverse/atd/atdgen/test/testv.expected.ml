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

let validate__19 validate__a = (
  Atdgen_runtime.Ov_run.validate_list (
    validate__a
  )
)
let rec validate_p' validate__a : _ -> 'a p' -> _ = (
  fun path x ->
    match x with
      | A -> None
      | Bb x ->
        (
          validate_p' validate__a
        ) path x
      | Ccccc x ->
        (
          validate__a
        ) path x
)
let rec validate_p = (
  fun path x ->
    match ( fun path x ->
         match x with
           `A -> Some (Atdgen_runtime.Util.Validation.error path)
         | _ -> None ) path x with
      | Some _ as err -> err
      | None ->
        match x with
          | `A -> None
          | `B x ->
            (
              validate_r
            ) path x
          | `C -> None
)
and validate_r : _ -> r -> _ = (
  fun path x ->
    match
      (
        fun _ _ -> None
      ) (`Field "a" :: path) x.a
    with
      | Some _ as err -> err
      | None ->
        (
          validate_p
        ) (`Field "c" :: path) x.c
)
let rec validate__20 validate__a validate__b path x = (
  Atdgen_runtime.Ov_run.validate_option (
    validate_poly validate__a validate__b
  )
) path x
and validate_poly validate__x validate__y : _ -> ('x, 'y) poly -> _ = (
  fun path x ->
    match
      (
        validate__19 validate__x
      ) (`Field "fst" :: path) x.fst
    with
      | Some _ as err -> err
      | None ->
        (
          validate__20 validate__x validate__y
        ) (`Field "snd" :: path) x.snd
)
let rec validate__2 path x = (
  fun _ _ -> None
) path x
and validate_test_variant path x = (
  fun _ _ -> None
) path x
let rec validate__1 path (x : _ p') = (
  fun _ _ -> None
) path x
let validate_validated_string_check = (
  fun path x ->
    let msg = "Failed check by fun s -> s = \"abc\"" in
    if (fun s -> s = "abc") x then
      None
    else
      Some (Atdgen_runtime.Util.Validation.error ~msg path)
)
let validate__31 = (
  (fun path x ->
    (match ( fun path x ->
    let msg = "Failed check by fun l -> true" in
    if (fun l -> true) x then
      None
    else
      Some (Atdgen_runtime.Util.Validation.error ~msg path) ) path x with
      | Some _ as err -> err
      | None -> (
          Atdgen_runtime.Ov_run.validate_list (
            fun path x ->
    let msg = "Failed check by fun s -> true" in
    if (fun s -> true) x then
      None
    else
      Some (Atdgen_runtime.Util.Validation.error ~msg path)
          )
        ) path x
    )
  )
)
let validate_validate_me = (
  validate__31
)
let validate_val1 : _ -> val1 -> _ = (
  fun path x ->
    (
      fun path _ -> Some (Atdgen_runtime.Util.Validation.error path)
    ) (`Field "val1_x" :: path) x.val1_x
)
let validate__16 = (
  Atdgen_runtime.Ov_run.validate_option (
    validate_val1
  )
)
let validate_val2 : _ -> val2 -> _ = (
  fun path x ->
    match
      (
        validate_val1
      ) (`Field "val2_x" :: path) x.val2_x
    with
      | Some _ as err -> err
      | None ->
        (
          validate__16
        ) (`Field "val2_y" :: path) x.val2_y
)
let validate__29 = (
  fun _ _ -> None
)
let validate_unixtime_list = (
  validate__29
)
let validate__3 = (
  fun _ _ -> None
)
let validate_date = (
  fun _ _ -> None
)
let validate__9 = (
  fun _ _ -> None
)
let validate__8 = (
  fun _ _ -> None
)
let validate__7 = (
  fun _ _ -> None
)
let validate__6 = (
  fun _ _ -> None
)
let validate__5 = (
  fun _ _ -> None
)
let validate__4 = (
  fun _ _ -> None
)
let validate__11 = (
  fun _ _ -> None
)
let validate__10 = (
  fun _ _ -> None
)
let validate_mixed_record : _ -> mixed_record -> _ = (
  fun _ _ -> None
)
let validate__13 = (
  fun _ _ -> None
)
let validate__12 = (
  fun _ _ -> None
)
let validate__14 = (
  fun _ _ -> None
)
let validate_mixed = (
  validate__14
)
let validate__15 = (
  fun _ _ -> None
)
let validate_test : _ -> test -> _ = (
  fun _ _ -> None
)
let validate_tup = (
  fun _ _ -> None
)
let validate_star_rating = (
  fun path x ->
    let msg = "Failed check by fun x -> x >= 1 && x <= 5" in
    if (fun x -> x >= 1 && x <= 5) x then
      None
    else
      Some (Atdgen_runtime.Util.Validation.error ~msg path)
)
let validate__30 : _ -> _ generic -> _ = (
  fun _ _ -> None
)
let validate_specialized = (
  validate__30
)
let validate_some_record : _ -> some_record -> _ = (
  fun path x ->
    (
      fun path x -> failwith "passed"
    ) (`Field "some_field" :: path) x.some_field
)
let validate_precision : _ -> precision -> _ = (
  fun _ _ -> None
)
let validate_p'' = (
  validate__1
)
let validate__18 = (
  Atdgen_runtime.Ov_run.validate_option (
    fun path _ -> Some (Atdgen_runtime.Util.Validation.error path)
  )
)
let validate_option_validation = (
  validate__18
)
let validate__28 = (
  validate_some_record
)
let validate_no_real_wrap = (
  validate__28
)
let validate__26 = (
  fun _ _ -> None
)
let validate_natural = (
  validate__26
)
let validate__24 = (
  fun path x ->
                       match x with
                         `Id "" -> failwith "empty"
                       | _ -> None
)
let validate_id = (
  validate__24
)
let validate__25 = (
  Atdgen_runtime.Ov_run.validate_list (
    fun path x ->
      (let x, _ = x in
      (
        validate_id
      ) (`Index 0 :: path) x
      )
  )
)
let validate_json_map = (
  validate__25
)
let validate_intopt = (
  validate__4
)
let validate__21 = (
  fun _ _ -> None
)
let validate_int_assoc_list = (
  validate__21
)
let validate__22 = (
  fun _ _ -> None
)
let validate_int_assoc_array = (
  validate__22
)
let validate_int8 = (
  (fun _ _ -> None)
)
let validate_int64 = (
  (fun _ _ -> None)
)
let validate_int32 = (
  (fun _ _ -> None)
)
let validate_hello = (
  fun _ _ -> None
)
let validate_generic validate__a : _ -> 'a generic -> _ = (
  fun _ _ -> None
)
let validate_floats : _ -> floats -> _ = (
  fun _ _ -> None
)
let validate__17 = (
  fun _ _ -> None
)
let validate_extended_tuple = (
  fun _ _ -> None
)
let validate_extended : _ -> extended -> _ = (
  fun path x ->
    match ( fun path x ->
      if x.b0x >= 0 then None
      else Some (Atdgen_runtime.Util.Validation.error path) ) path x with
      | Some _ as err -> err
      | None ->
        (
          fun path x ->
        if x = false then None else Some (Atdgen_runtime.Util.Validation.error path)
        ) (`Field "b1x" :: path) x.b1x
)
let validate__27 = (
  fun _ _ -> None
)
let validate_even_natural = (
  validate__27
)
let validate_char = (
  (fun _ _ -> None)
)
let validate_base_tuple = (
  fun path (i, f) -> if float i < f then None else Some (Atdgen_runtime.Util.Validation.error ~msg:"i < f" path)
)
let validate_base : _ -> base -> _ = (
  fun _ _ -> None
)
let validate__23 validate__a = (
  Atdgen_runtime.Ov_run.validate_array (
    validate__a
  )
)
let validate_array validate__a = (
  validate__23 validate__a
)
let validate_abs3 validate__a = (
  validate__19 validate__a
)
let validate_abs2 validate__a = (
  validate__19 validate__a
)
let validate_abs1 validate__a = (
  validate__19 validate__a
)
let create_r 
  ~a
  ~b
  ~c
  () : r =
  {
    a = a;
    b = b;
    c = c;
  }
let create_poly 
  ~fst
  ~snd
  () : ('x, 'y) poly =
  {
    fst = fst;
    snd = snd;
  }
let create_val1 
  ~val1_x
  () : val1 =
  {
    val1_x = val1_x;
  }
let create_val2 
  ~val2_x
  ?val2_y
  () : val2 =
  {
    val2_x = val2_x;
    val2_y = val2_y;
  }
let create_mixed_record 
  ?field0
  ?field1
  ~field2
  ~field3
  ~field4
  ?field5
  ?field6
  ~field7
  ~field8
  ~field9
  ~field10
  ?(field11 = false)
  ~field12
  ~field13
  ~field14
  () : mixed_record =
  {
    field0 = field0;
    field1 = field1;
    field2 = field2;
    field3 = field3;
    field4 = field4;
    field5 = field5;
    field6 = field6;
    field7 = field7;
    field8 = field8;
    field9 = field9;
    field10 = field10;
    field11 = field11;
    field12 = field12;
    field13 = field13;
    field14 = field14;
  }
let create_test 
  ?x0
  ?x1
  ~x2
  ~x3
  ~x4
  () : test =
  {
    x0 = x0;
    x1 = x1;
    x2 = x2;
    x3 = x3;
    x4 = x4;
  }
let create_some_record 
  ~some_field
  () : some_record =
  {
    some_field = some_field;
  }
let create_precision 
  ~sqrt2_5
  ~small_2
  ~large_2
  () : precision =
  {
    sqrt2_5 = sqrt2_5;
    small_2 = small_2;
    large_2 = large_2;
  }
let create_generic 
  ~x294623
  () : 'a generic =
  {
    x294623 = x294623;
  }
let create_floats 
  ~f32
  ~f64
  () : floats =
  {
    f32 = f32;
    f64 = f64;
  }
let create_extended 
  ~b0x
  ~b1x
  ~b2x
  ?b3x
  ~b4x
  ?(b5x = 0.5)
  () : extended =
  {
    b0x = b0x;
    b1x = b1x;
    b2x = b2x;
    b3x = b3x;
    b4x = b4x;
    b5x = b5x;
  }
let create_base 
  ~b0
  ~b1
  () : base =
  {
    b0 = b0;
    b1 = b1;
  }
