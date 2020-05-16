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

let _19_tag = Bi_io.array_tag
let write_untagged__19 _a_tag write_untagged__a write__a = (
  Atdgen_runtime.Ob_run.write_untagged_list
    _a_tag
    (
      write_untagged__a
    )
)
let write__19 _a_tag write_untagged__a write__a ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__19 _a_tag write_untagged__a write__a ob x
let string_of__19 _a_tag write_untagged__a write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__19 _a_tag write_untagged__a write__a ob x;
  Bi_outbuf.contents ob
let get__19_reader get__a_reader read__a = (
  Atdgen_runtime.Ob_run.get_list_reader (
    get__a_reader
  )
)
let read__19 get__a_reader read__a = (
  Atdgen_runtime.Ob_run.read_list (
    get__a_reader
  )
)
let _19_of_string get__a_reader read__a ?pos s =
  read__19 get__a_reader read__a (Bi_inbuf.from_string ?pos s)
let rec p'_tag = Bi_io.variant_tag
and write_untagged_p' _a_tag write_untagged__a write__a : Bi_outbuf.t -> 'a p' -> unit = (
  fun ob x ->
    match x with
      | A -> Bi_outbuf.add_char4 ob '\000' '\000' '\000' 'A'
      | Bb x ->
        Bi_outbuf.add_char4 ob '\128' '\000' '9' '\224';
        (
          write_p' _a_tag write_untagged__a write__a
        ) ob x
      | Ccccc x ->
        Bi_outbuf.add_char4 ob '\213' '\148' 's' '\003';
        (
          write__a
        ) ob x
)
and write_p' _a_tag write_untagged__a write__a ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_p' _a_tag write_untagged__a write__a ob x
and string_of_p' _a_tag write_untagged__a write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_p' _a_tag write_untagged__a write__a ob x;
  Bi_outbuf.contents ob
let rec get_p'_reader get__a_reader read__a = (
  fun tag ->
    if tag <> 23 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | 65, false -> (A : 'a p')
            | 14816, true -> (Bb (
                (
                  read_p' get__a_reader read__a
                ) ib
              ) : 'a p')
            | -711691517, true -> (Ccccc (
                (
                  read__a
                ) ib
              ) : 'a p')
            | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
        )
)
and read_p' get__a_reader read__a = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Atdgen_runtime.Ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | 65, false -> (A : 'a p')
        | 14816, true -> (Bb (
            (
              read_p' get__a_reader read__a
            ) ib
          ) : 'a p')
        | -711691517, true -> (Ccccc (
            (
              read__a
            ) ib
          ) : 'a p')
        | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
    )
)
and p'_of_string get__a_reader read__a ?pos s =
  read_p' get__a_reader read__a (Bi_inbuf.from_string ?pos s)
let rec p_tag = Bi_io.variant_tag
and write_untagged_p = (
  fun ob x ->
    match x with
      | `A -> Bi_outbuf.add_char4 ob '\000' '\000' '\000' 'A'
      | `B x ->
        Bi_outbuf.add_char4 ob '\128' '\000' '\000' 'B';
        (
          write_r
        ) ob x
      | `C -> Bi_outbuf.add_char4 ob '\000' '\000' '\000' 'C'
)
and write_p ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_p ob x
and string_of_p ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_p ob x;
  Bi_outbuf.contents ob
and r_tag = Bi_io.record_tag
and write_untagged_r : Bi_outbuf.t -> r -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 3;
    Bi_outbuf.add_char4 ob '\128' '\000' '\000' 'a';
    (
      Bi_io.write_svint
    ) ob x.a;
    Bi_outbuf.add_char4 ob '\128' '\000' '\000' 'b';
    (
      Bi_io.write_bool
    ) ob x.b;
    Bi_outbuf.add_char4 ob '\128' '\000' '\000' 'c';
    (
      write_p
    ) ob x.c;
)
and write_r ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_r ob x
and string_of_r ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_r ob x;
  Bi_outbuf.contents ob
let rec get_p_reader = (
  fun tag ->
    if tag <> 23 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | 65, false -> `A
            | 66, true -> (`B (
                (
                  read_r
                ) ib
              ))
            | 67, false -> `C
            | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
        )
)
and read_p = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Atdgen_runtime.Ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | 65, false -> `A
        | 66, true -> (`B (
            (
              read_r
            ) ib
          ))
        | 67, false -> `C
        | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
    )
)
and p_of_string ?pos s =
  read_p (Bi_inbuf.from_string ?pos s)
and get_r_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_a = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_b = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_c = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 97 ->
              field_a := (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 98 ->
              field_b := (
                (
                  Atdgen_runtime.Ob_run.read_bool
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | 99 ->
              field_c := (
                (
                  read_p
                ) ib
              );
              bits0 := !bits0 lor 0x4;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x7 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "a"; "b"; "c" |];
        (
          {
            a = !field_a;
            b = !field_b;
            c = !field_c;
          }
         : r)
)
and read_r = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_a = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_b = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_c = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 97 ->
          field_a := (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 98 ->
          field_b := (
            (
              Atdgen_runtime.Ob_run.read_bool
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | 99 ->
          field_c := (
            (
              read_p
            ) ib
          );
          bits0 := !bits0 lor 0x4;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x7 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "a"; "b"; "c" |];
    (
      {
        a = !field_a;
        b = !field_b;
        c = !field_c;
      }
     : r)
)
and r_of_string ?pos s =
  read_r (Bi_inbuf.from_string ?pos s)
let rec _20_tag = Bi_io.num_variant_tag
and write_untagged__20 _a_tag write_untagged__a write__a _b_tag write_untagged__b write__b ob x = (
  Atdgen_runtime.Ob_run.write_untagged_option (
    write_poly _a_tag write_untagged__a write__a _b_tag write_untagged__b write__b
  )
) ob x
and write__20 _a_tag write_untagged__a write__a _b_tag write_untagged__b write__b ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged__20 _a_tag write_untagged__a write__a _b_tag write_untagged__b write__b ob x
and string_of__20 _a_tag write_untagged__a write__a _b_tag write_untagged__b write__b ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__20 _a_tag write_untagged__a write__a _b_tag write_untagged__b write__b ob x;
  Bi_outbuf.contents ob
and poly_tag = Bi_io.record_tag
and write_untagged_poly _x_tag write_untagged__x write__x _y_tag write_untagged__y write__y : Bi_outbuf.t -> ('x, 'y) poly -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\128' 'M' '\202' '\135';
    (
      write__19 _x_tag write_untagged__x write__x
    ) ob x.fst;
    Bi_outbuf.add_char4 ob '\128' 'W' '\163' 'i';
    (
      write__20 _x_tag write_untagged__x write__x _y_tag write_untagged__y write__y
    ) ob x.snd;
)
and write_poly _x_tag write_untagged__x write__x _y_tag write_untagged__y write__y ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_poly _x_tag write_untagged__x write__x _y_tag write_untagged__y write__y ob x
and string_of_poly _x_tag write_untagged__x write__x _y_tag write_untagged__y write__y ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_poly _x_tag write_untagged__x write__x _y_tag write_untagged__y write__y ob x;
  Bi_outbuf.contents ob
let rec get__20_reader get__a_reader read__a get__b_reader read__b = (
  fun tag ->
    if tag <> 22 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        match Char.code (Bi_inbuf.read_char ib) with
          | 0 -> None
          | 0x80 ->
            Some (
              (
                read_poly get__a_reader read__a get__b_reader read__b
              )
                ib
            )
          | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
and read__20 get__a_reader read__a get__b_reader read__b = (
  fun ib ->
    if Bi_io.read_tag ib <> 22 then Atdgen_runtime.Ob_run.read_error_at ib;
    match Char.code (Bi_inbuf.read_char ib) with
      | 0 -> None
      | 0x80 ->
        Some (
          (
            read_poly get__a_reader read__a get__b_reader read__b
          )
            ib
        )
      | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
and _20_of_string get__a_reader read__a get__b_reader read__b ?pos s =
  read__20 get__a_reader read__a get__b_reader read__b (Bi_inbuf.from_string ?pos s)
and get_poly_reader get__x_reader read__x get__y_reader read__y = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_fst = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_snd = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 5098119 ->
              field_fst := (
                (
                  read__19 get__x_reader read__x
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 5743465 ->
              field_snd := (
                (
                  read__20 get__x_reader read__x get__y_reader read__y
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "fst"; "snd" |];
        (
          {
            fst = !field_fst;
            snd = !field_snd;
          }
         : ('x, 'y) poly)
)
and read_poly get__x_reader read__x get__y_reader read__y = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_fst = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_snd = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 5098119 ->
          field_fst := (
            (
              read__19 get__x_reader read__x
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 5743465 ->
          field_snd := (
            (
              read__20 get__x_reader read__x get__y_reader read__y
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "fst"; "snd" |];
    (
      {
        fst = !field_fst;
        snd = !field_snd;
      }
     : ('x, 'y) poly)
)
and poly_of_string get__x_reader read__x get__y_reader read__y ?pos s =
  read_poly get__x_reader read__x get__y_reader read__y (Bi_inbuf.from_string ?pos s)
let rec _2_tag = Bi_io.array_tag
and write_untagged__2 ob x = (
  Atdgen_runtime.Ob_run.write_untagged_list
    test_variant_tag
    (
      write_untagged_test_variant
    )
) ob x
and write__2 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__2 ob x
and string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
and test_variant_tag = Bi_io.variant_tag
and write_untagged_test_variant = (
  fun ob x ->
    match x with
      | `Case1 -> Bi_outbuf.add_char4 ob 'T' 'N' '+' 'a'
      | `Case2 x ->
        Bi_outbuf.add_char4 ob '\212' 'N' '+' 'b';
        (
          Bi_io.write_svint
        ) ob x
      | `Case3 x ->
        Bi_outbuf.add_char4 ob '\212' 'N' '+' 'c';
        (
          Bi_io.write_string
        ) ob x
      | `Case4 x ->
        Bi_outbuf.add_char4 ob '\212' 'N' '+' 'd';
        (
          write__2
        ) ob x
)
and write_test_variant ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_test_variant ob x
and string_of_test_variant ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_test_variant ob x;
  Bi_outbuf.contents ob
let rec get__2_reader tag = (
  Atdgen_runtime.Ob_run.get_list_reader (
    get_test_variant_reader
  )
) tag
and read__2 ib = (
  Atdgen_runtime.Ob_run.read_list (
    get_test_variant_reader
  )
) ib
and _2_of_string ?pos s =
  read__2 (Bi_inbuf.from_string ?pos s)
and get_test_variant_reader = (
  fun tag ->
    if tag <> 23 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | -733074591, false -> `Case1
            | -733074590, true -> (`Case2 (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              ))
            | -733074589, true -> (`Case3 (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              ))
            | -733074588, true -> (`Case4 (
                (
                  read__2
                ) ib
              ))
            | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
        )
)
and read_test_variant = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Atdgen_runtime.Ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | -733074591, false -> `Case1
        | -733074590, true -> (`Case2 (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          ))
        | -733074589, true -> (`Case3 (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          ))
        | -733074588, true -> (`Case4 (
            (
              read__2
            ) ib
          ))
        | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
    )
)
and test_variant_of_string ?pos s =
  read_test_variant (Bi_inbuf.from_string ?pos s)
let rec _1_tag = Bi_io.variant_tag
and write_untagged__1 : Bi_outbuf.t -> _ p' -> unit = (
  fun ob x ->
    match x with
      | A -> Bi_outbuf.add_char4 ob '\000' '\000' '\000' 'A'
      | Bb x ->
        Bi_outbuf.add_char4 ob '\128' '\000' '9' '\224';
        (
          write__1
        ) ob x
      | Ccccc x ->
        Bi_outbuf.add_char4 ob '\213' '\148' 's' '\003';
        (
          Bi_io.write_svint
        ) ob x
)
and write__1 ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged__1 ob x
and string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let rec get__1_reader = (
  fun tag ->
    if tag <> 23 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | 65, false -> (A : _ p')
            | 14816, true -> (Bb (
                (
                  read__1
                ) ib
              ) : _ p')
            | -711691517, true -> (Ccccc (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              ) : _ p')
            | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
        )
)
and read__1 = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Atdgen_runtime.Ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | 65, false -> (A : _ p')
        | 14816, true -> (Bb (
            (
              read__1
            ) ib
          ) : _ p')
        | -711691517, true -> (Ccccc (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          ) : _ p')
        | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
    )
)
and _1_of_string ?pos s =
  read__1 (Bi_inbuf.from_string ?pos s)
let validated_string_check_tag = Bi_io.string_tag
let write_untagged_validated_string_check = (
  Bi_io.write_untagged_string
)
let write_validated_string_check ob x =
  Bi_io.write_tag ob Bi_io.string_tag;
  write_untagged_validated_string_check ob x
let string_of_validated_string_check ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_validated_string_check ob x;
  Bi_outbuf.contents ob
let get_validated_string_check_reader = (
  Atdgen_runtime.Ob_run.get_string_reader
)
let read_validated_string_check = (
  Atdgen_runtime.Ob_run.read_string
)
let validated_string_check_of_string ?pos s =
  read_validated_string_check (Bi_inbuf.from_string ?pos s)
let _31_tag = Bi_io.array_tag
let write_untagged__31 = (
  Atdgen_runtime.Ob_run.write_untagged_list
    Bi_io.string_tag
    (
      Bi_io.write_untagged_string
    )
)
let write__31 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__31 ob x
let string_of__31 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__31 ob x;
  Bi_outbuf.contents ob
let get__31_reader = (
  Atdgen_runtime.Ob_run.get_list_reader (
    Atdgen_runtime.Ob_run.get_string_reader
  )
)
let read__31 = (
  Atdgen_runtime.Ob_run.read_list (
    Atdgen_runtime.Ob_run.get_string_reader
  )
)
let _31_of_string ?pos s =
  read__31 (Bi_inbuf.from_string ?pos s)
let validate_me_tag = Bi_io.array_tag
let write_untagged_validate_me = (
  write_untagged__31
)
let write_validate_me ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged_validate_me ob x
let string_of_validate_me ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_validate_me ob x;
  Bi_outbuf.contents ob
let get_validate_me_reader = (
  get__31_reader
)
let read_validate_me = (
  read__31
)
let validate_me_of_string ?pos s =
  read_validate_me (Bi_inbuf.from_string ?pos s)
let val1_tag = Bi_io.record_tag
let write_untagged_val1 : Bi_outbuf.t -> val1 -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 1;
    Bi_outbuf.add_char4 ob '\207' '\131' 'e' 'i';
    (
      Bi_io.write_svint
    ) ob x.val1_x;
)
let write_val1 ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_val1 ob x
let string_of_val1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_val1 ob x;
  Bi_outbuf.contents ob
let get_val1_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_val1_x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | -813472407 ->
              field_val1_x := (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x1 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "val1_x" |];
        (
          {
            val1_x = !field_val1_x;
          }
         : val1)
)
let read_val1 = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_val1_x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | -813472407 ->
          field_val1_x := (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x1 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "val1_x" |];
    (
      {
        val1_x = !field_val1_x;
      }
     : val1)
)
let val1_of_string ?pos s =
  read_val1 (Bi_inbuf.from_string ?pos s)
let _16_tag = Bi_io.num_variant_tag
let write_untagged__16 = (
  Atdgen_runtime.Ob_run.write_untagged_option (
    write_val1
  )
)
let write__16 ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged__16 ob x
let string_of__16 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__16 ob x;
  Bi_outbuf.contents ob
let get__16_reader = (
  fun tag ->
    if tag <> 22 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        match Char.code (Bi_inbuf.read_char ib) with
          | 0 -> None
          | 0x80 ->
            Some (
              (
                read_val1
              )
                ib
            )
          | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let read__16 = (
  fun ib ->
    if Bi_io.read_tag ib <> 22 then Atdgen_runtime.Ob_run.read_error_at ib;
    match Char.code (Bi_inbuf.read_char ib) with
      | 0 -> None
      | 0x80 ->
        Some (
          (
            read_val1
          )
            ib
        )
      | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let _16_of_string ?pos s =
  read__16 (Bi_inbuf.from_string ?pos s)
let val2_tag = Bi_io.record_tag
let write_untagged_val2 : Bi_outbuf.t -> val2 -> unit = (
  fun ob x ->
    let len = ref 1 in
    let x_val2_y = x.val2_y in
    if x_val2_y != None then incr len;
    Bi_vint.write_uvint ob !len;
    Bi_outbuf.add_char4 ob '\207' '\132' '\'' '\170';
    (
      write_val1
    ) ob x.val2_x;
    (match x_val2_y with None -> () | Some x ->
      Bi_outbuf.add_char4 ob '\207' '\132' '\'' '\171';
      (
        write_val1
      ) ob x;
    );
)
let write_val2 ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_val2 ob x
let string_of_val2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_val2 ob x;
  Bi_outbuf.contents ob
let get_val2_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_val2_x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_val2_y = ref (None) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | -813422678 ->
              field_val2_x := (
                (
                  read_val1
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | -813422677 ->
              field_val2_y := (
                Some (
                  (
                    read_val1
                  ) ib
                )
              );
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x1 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "val2_x" |];
        (
          {
            val2_x = !field_val2_x;
            val2_y = !field_val2_y;
          }
         : val2)
)
let read_val2 = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_val2_x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_val2_y = ref (None) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | -813422678 ->
          field_val2_x := (
            (
              read_val1
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | -813422677 ->
          field_val2_y := (
            Some (
              (
                read_val1
              ) ib
            )
          );
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x1 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "val2_x" |];
    (
      {
        val2_x = !field_val2_x;
        val2_y = !field_val2_y;
      }
     : val2)
)
let val2_of_string ?pos s =
  read_val2 (Bi_inbuf.from_string ?pos s)
let _29_tag = Bi_io.array_tag
let write_untagged__29 = (
  Atdgen_runtime.Ob_run.write_untagged_list
    Bi_io.float64_tag
    (
      Bi_io.write_untagged_float64
    )
)
let write__29 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__29 ob x
let string_of__29 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__29 ob x;
  Bi_outbuf.contents ob
let get__29_reader = (
  Atdgen_runtime.Ob_run.get_list_reader (
    Atdgen_runtime.Ob_run.get_float64_reader
  )
)
let read__29 = (
  Atdgen_runtime.Ob_run.read_list (
    Atdgen_runtime.Ob_run.get_float64_reader
  )
)
let _29_of_string ?pos s =
  read__29 (Bi_inbuf.from_string ?pos s)
let unixtime_list_tag = Bi_io.array_tag
let write_untagged_unixtime_list = (
  write_untagged__29
)
let write_unixtime_list ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged_unixtime_list ob x
let string_of_unixtime_list ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_unixtime_list ob x;
  Bi_outbuf.contents ob
let get_unixtime_list_reader = (
  get__29_reader
)
let read_unixtime_list = (
  read__29
)
let unixtime_list_of_string ?pos s =
  read_unixtime_list (Bi_inbuf.from_string ?pos s)
let _3_tag = Bi_io.num_variant_tag
let write_untagged__3 = (
  Atdgen_runtime.Ob_run.write_untagged_option (
    Bi_io.write_svint
  )
)
let write__3 ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged__3 ob x
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let get__3_reader = (
  fun tag ->
    if tag <> 22 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        match Char.code (Bi_inbuf.read_char ib) with
          | 0 -> None
          | 0x80 ->
            Some (
              (
                Atdgen_runtime.Ob_run.read_int
              )
                ib
            )
          | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let read__3 = (
  fun ib ->
    if Bi_io.read_tag ib <> 22 then Atdgen_runtime.Ob_run.read_error_at ib;
    match Char.code (Bi_inbuf.read_char ib) with
      | 0 -> None
      | 0x80 ->
        Some (
          (
            Atdgen_runtime.Ob_run.read_int
          )
            ib
        )
      | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let _3_of_string ?pos s =
  read__3 (Bi_inbuf.from_string ?pos s)
let date_tag = Bi_io.tuple_tag
let write_untagged_date = (
  fun ob x ->
    Bi_vint.write_uvint ob 3;
    (
      let x, _, _ = x in (
        Bi_io.write_svint
      ) ob x
    );
    (
      let _, x, _ = x in (
        write__3
      ) ob x
    );
    (
      let _, _, x = x in (
        write__3
      ) ob x
    );
)
let write_date ob x =
  Bi_io.write_tag ob Bi_io.tuple_tag;
  write_untagged_date ob x
let string_of_date ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_date ob x;
  Bi_outbuf.contents ob
let get_date_reader = (
  fun tag ->
    if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let len = Bi_vint.read_uvint ib in
        if len < 3 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1; 2 ];
        let x0 =
          (
            Atdgen_runtime.Ob_run.read_int
          ) ib
        in
        let x1 =
          (
            read__3
          ) ib
        in
        let x2 =
          (
            read__3
          ) ib
        in
        for i = 3 to len - 1 do Bi_io.skip ib done;
        (x0, x1, x2)
)
let read_date = (
  fun ib ->
    if Bi_io.read_tag ib <> 20 then Atdgen_runtime.Ob_run.read_error_at ib;
    let len = Bi_vint.read_uvint ib in
    if len < 3 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1; 2 ];
    let x0 =
      (
        Atdgen_runtime.Ob_run.read_int
      ) ib
    in
    let x1 =
      (
        read__3
      ) ib
    in
    let x2 =
      (
        read__3
      ) ib
    in
    for i = 3 to len - 1 do Bi_io.skip ib done;
    (x0, x1, x2)
)
let date_of_string ?pos s =
  read_date (Bi_inbuf.from_string ?pos s)
let _9_tag = Bi_io.array_tag
let write_untagged__9 = (
  Atdgen_runtime.Ob_run.write_untagged_array
    Bi_io.string_tag
    (
      Bi_io.write_untagged_string
    )
)
let write__9 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__9 ob x
let string_of__9 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__9 ob x;
  Bi_outbuf.contents ob
let get__9_reader = (
  Atdgen_runtime.Ob_run.get_array_reader (
    Atdgen_runtime.Ob_run.get_string_reader
  )
)
let read__9 = (
  Atdgen_runtime.Ob_run.read_array (
    Atdgen_runtime.Ob_run.get_string_reader
  )
)
let _9_of_string ?pos s =
  read__9 (Bi_inbuf.from_string ?pos s)
let _8_tag = Bi_io.num_variant_tag
let write_untagged__8 = (
  Atdgen_runtime.Ob_run.write_untagged_option (
    Bi_io.write_bool
  )
)
let write__8 ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged__8 ob x
let string_of__8 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__8 ob x;
  Bi_outbuf.contents ob
let get__8_reader = (
  fun tag ->
    if tag <> 22 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        match Char.code (Bi_inbuf.read_char ib) with
          | 0 -> None
          | 0x80 ->
            Some (
              (
                Atdgen_runtime.Ob_run.read_bool
              )
                ib
            )
          | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let read__8 = (
  fun ib ->
    if Bi_io.read_tag ib <> 22 then Atdgen_runtime.Ob_run.read_error_at ib;
    match Char.code (Bi_inbuf.read_char ib) with
      | 0 -> None
      | 0x80 ->
        Some (
          (
            Atdgen_runtime.Ob_run.read_bool
          )
            ib
        )
      | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let _8_of_string ?pos s =
  read__8 (Bi_inbuf.from_string ?pos s)
let _7_tag = Bi_io.array_tag
let write_untagged__7 = (
  Atdgen_runtime.Ob_run.write_untagged_array
    Bi_io.float64_tag
    (
      Bi_io.write_untagged_float64
    )
)
let write__7 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__7 ob x
let string_of__7 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__7 ob x;
  Bi_outbuf.contents ob
let get__7_reader = (
  Atdgen_runtime.Ob_run.get_array_reader (
    Atdgen_runtime.Ob_run.get_float64_reader
  )
)
let read__7 = (
  Atdgen_runtime.Ob_run.read_array (
    Atdgen_runtime.Ob_run.get_float64_reader
  )
)
let _7_of_string ?pos s =
  read__7 (Bi_inbuf.from_string ?pos s)
let _6_tag = Bi_io.num_variant_tag
let write_untagged__6 = (
  Atdgen_runtime.Ob_run.write_untagged_option (
    Bi_io.write_string
  )
)
let write__6 ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged__6 ob x
let string_of__6 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__6 ob x;
  Bi_outbuf.contents ob
let get__6_reader = (
  fun tag ->
    if tag <> 22 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        match Char.code (Bi_inbuf.read_char ib) with
          | 0 -> None
          | 0x80 ->
            Some (
              (
                Atdgen_runtime.Ob_run.read_string
              )
                ib
            )
          | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let read__6 = (
  fun ib ->
    if Bi_io.read_tag ib <> 22 then Atdgen_runtime.Ob_run.read_error_at ib;
    match Char.code (Bi_inbuf.read_char ib) with
      | 0 -> None
      | 0x80 ->
        Some (
          (
            Atdgen_runtime.Ob_run.read_string
          )
            ib
        )
      | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let _6_of_string ?pos s =
  read__6 (Bi_inbuf.from_string ?pos s)
let _5_tag = Bi_io.num_variant_tag
let write_untagged__5 = (
  Atdgen_runtime.Ob_run.write_untagged_option (
    Bi_io.write_float64
  )
)
let write__5 ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged__5 ob x
let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob
let get__5_reader = (
  fun tag ->
    if tag <> 22 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        match Char.code (Bi_inbuf.read_char ib) with
          | 0 -> None
          | 0x80 ->
            Some (
              (
                Atdgen_runtime.Ob_run.read_float64
              )
                ib
            )
          | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let read__5 = (
  fun ib ->
    if Bi_io.read_tag ib <> 22 then Atdgen_runtime.Ob_run.read_error_at ib;
    match Char.code (Bi_inbuf.read_char ib) with
      | 0 -> None
      | 0x80 ->
        Some (
          (
            Atdgen_runtime.Ob_run.read_float64
          )
            ib
        )
      | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let _5_of_string ?pos s =
  read__5 (Bi_inbuf.from_string ?pos s)
let _4_tag = Bi_io.num_variant_tag
let write_untagged__4 = (
  Atdgen_runtime.Ob_run.write_untagged_option (
    Bi_io.write_svint
  )
)
let write__4 ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged__4 ob x
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let get__4_reader = (
  fun tag ->
    if tag <> 22 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        match Char.code (Bi_inbuf.read_char ib) with
          | 0 -> None
          | 0x80 ->
            Some (
              (
                Atdgen_runtime.Ob_run.read_int
              )
                ib
            )
          | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let read__4 = (
  fun ib ->
    if Bi_io.read_tag ib <> 22 then Atdgen_runtime.Ob_run.read_error_at ib;
    match Char.code (Bi_inbuf.read_char ib) with
      | 0 -> None
      | 0x80 ->
        Some (
          (
            Atdgen_runtime.Ob_run.read_int
          )
            ib
        )
      | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let _4_of_string ?pos s =
  read__4 (Bi_inbuf.from_string ?pos s)
let _11_tag = Bi_io.array_tag
let write_untagged__11 = (
  Atdgen_runtime.Ob_run.write_untagged_list
    _6_tag
    (
      write_untagged__6
    )
)
let write__11 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__11 ob x
let string_of__11 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__11 ob x;
  Bi_outbuf.contents ob
let get__11_reader = (
  Atdgen_runtime.Ob_run.get_list_reader (
    get__6_reader
  )
)
let read__11 = (
  Atdgen_runtime.Ob_run.read_list (
    get__6_reader
  )
)
let _11_of_string ?pos s =
  read__11 (Bi_inbuf.from_string ?pos s)
let _10_tag = Bi_io.array_tag
let write_untagged__10 = (
  Atdgen_runtime.Ob_run.write_untagged_list
    Bi_io.unit_tag
    (
      Bi_io.write_untagged_unit
    )
)
let write__10 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__10 ob x
let string_of__10 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__10 ob x;
  Bi_outbuf.contents ob
let get__10_reader = (
  Atdgen_runtime.Ob_run.get_list_reader (
    Atdgen_runtime.Ob_run.get_unit_reader
  )
)
let read__10 = (
  Atdgen_runtime.Ob_run.read_list (
    Atdgen_runtime.Ob_run.get_unit_reader
  )
)
let _10_of_string ?pos s =
  read__10 (Bi_inbuf.from_string ?pos s)
let mixed_record_tag = Bi_io.record_tag
let write_untagged_mixed_record : Bi_outbuf.t -> mixed_record -> unit = (
  fun ob x ->
    let len = ref 10 in
    let x_field0 = x.field0 in
    if x_field0 != None then incr len;
    let x_field1 = x.field1 in
    if x_field1 != None then incr len;
    let x_field5 = x.field5 in
    if x_field5 != None then incr len;
    let x_field6 = x.field6 in
    if x_field6 != None then incr len;
    let x_field11 = x.field11 in
    if x_field11 != false then incr len;
    Bi_vint.write_uvint ob !len;
    (match x_field0 with None -> () | Some x ->
      Bi_outbuf.add_char4 ob '\128' '\142' '\142' '6';
      (
        Bi_io.write_svint
      ) ob x;
    );
    (match x_field1 with None -> () | Some x ->
      Bi_outbuf.add_char4 ob '\128' '\142' '\142' '7';
      (
        Bi_io.write_float64
      ) ob x;
    );
    Bi_outbuf.add_char4 ob '\128' '\142' '\142' '8';
    (
      write__6
    ) ob x.field2;
    Bi_outbuf.add_char4 ob '\128' '\142' '\142' '9';
    (
      Bi_io.write_int64
    ) ob x.field3;
    Bi_outbuf.add_char4 ob '\128' '\142' '\142' ':';
    (
      write__7
    ) ob x.field4;
    (match x_field5 with None -> () | Some x ->
      Bi_outbuf.add_char4 ob '\128' '\142' '\142' ';';
      (
        Bi_io.write_bool
      ) ob x;
    );
    (match x_field6 with None -> () | Some x ->
      Bi_outbuf.add_char4 ob '\128' '\142' '\142' '<';
      (
        Bi_io.write_string
      ) ob x;
    );
    Bi_outbuf.add_char4 ob '\128' '\142' '\142' '=';
    (
      write_test_variant
    ) ob x.field7;
    Bi_outbuf.add_char4 ob '\128' '\142' '\142' '>';
    (
      write__9
    ) ob x.field8;
    Bi_outbuf.add_char4 ob '\128' '\142' '\142' '?';
    (
      fun ob x ->
        Bi_io.write_tag ob Bi_io.tuple_tag;
        Bi_vint.write_uvint ob 6;
        (
          let x, _, _, _, _, _ = x in (
            Bi_io.write_uvint
          ) ob x
        );
        (
          let _, x, _, _, _, _ = x in (
            Bi_io.write_int8
          ) ob x
        );
        (
          let _, _, x, _, _, _ = x in (
            Bi_io.write_char
          ) ob x
        );
        (
          let _, _, _, x, _, _ = x in (
            Bi_io.write_int16
          ) ob x
        );
        (
          let _, _, _, _, x, _ = x in (
            Bi_io.write_int32
          ) ob x
        );
        (
          let _, _, _, _, _, x = x in (
            Bi_io.write_int64
          ) ob x
        );
    ) ob x.field9;
    Bi_outbuf.add_char4 ob '\252' '-' '\226' '\025';
    (
      Bi_io.write_bool
    ) ob x.field10;
    if x_field11 != false then (
      Bi_outbuf.add_char4 ob '\252' '-' '\226' '\026';
      (
        Bi_io.write_bool
      ) ob x_field11;
    );
    Bi_outbuf.add_char4 ob '\252' '-' '\226' '\027';
    (
      write__10
    ) ob x.field12;
    Bi_outbuf.add_char4 ob '\252' '-' '\226' '\028';
    (
      write__11
    ) ob x.field13;
    Bi_outbuf.add_char4 ob '\252' '-' '\226' '\029';
    (
      write_date
    ) ob x.field14;
)
let write_mixed_record ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_mixed_record ob x
let string_of_mixed_record ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_mixed_record ob x;
  Bi_outbuf.contents ob
let get_mixed_record_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_field0 = ref (None) in
        let field_field1 = ref (None) in
        let field_field2 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_field3 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_field4 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_field5 = ref (None) in
        let field_field6 = ref (None) in
        let field_field7 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_field8 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_field9 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_field10 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_field11 = ref (false) in
        let field_field12 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_field13 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_field14 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 9342518 ->
              field_field0 := (
                Some (
                  (
                    Atdgen_runtime.Ob_run.read_int
                  ) ib
                )
              );
            | 9342519 ->
              field_field1 := (
                Some (
                  (
                    Atdgen_runtime.Ob_run.read_float64
                  ) ib
                )
              );
            | 9342520 ->
              field_field2 := (
                (
                  read__6
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 9342521 ->
              field_field3 := (
                (
                  Atdgen_runtime.Ob_run.read_int64
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | 9342522 ->
              field_field4 := (
                (
                  read__7
                ) ib
              );
              bits0 := !bits0 lor 0x4;
            | 9342523 ->
              field_field5 := (
                Some (
                  (
                    Atdgen_runtime.Ob_run.read_bool
                  ) ib
                )
              );
            | 9342524 ->
              field_field6 := (
                Some (
                  (
                    Atdgen_runtime.Ob_run.read_string
                  ) ib
                )
              );
            | 9342525 ->
              field_field7 := (
                (
                  read_test_variant
                ) ib
              );
              bits0 := !bits0 lor 0x8;
            | 9342526 ->
              field_field8 := (
                (
                  read__9
                ) ib
              );
              bits0 := !bits0 lor 0x10;
            | 9342527 ->
              field_field9 := (
                (
                  fun ib ->
                    if Bi_io.read_tag ib <> 20 then Atdgen_runtime.Ob_run.read_error_at ib;
                    let len = Bi_vint.read_uvint ib in
                    if len < 6 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1; 2; 3; 4; 5 ];
                    let x0 =
                      (
                        Atdgen_runtime.Ob_run.read_int
                      ) ib
                    in
                    let x1 =
                      (
                        Atdgen_runtime.Ob_run.read_int
                      ) ib
                    in
                    let x2 =
                      (
                        Atdgen_runtime.Ob_run.read_char
                      ) ib
                    in
                    let x3 =
                      (
                        Atdgen_runtime.Ob_run.read_int
                      ) ib
                    in
                    let x4 =
                      (
                        Atdgen_runtime.Ob_run.read_int32
                      ) ib
                    in
                    let x5 =
                      (
                        Atdgen_runtime.Ob_run.read_int64
                      ) ib
                    in
                    for i = 6 to len - 1 do Bi_io.skip ib done;
                    (x0, x1, x2, x3, x4, x5)
                ) ib
              );
              bits0 := !bits0 lor 0x20;
            | -64101863 ->
              field_field10 := (
                (
                  Atdgen_runtime.Ob_run.read_bool
                ) ib
              );
              bits0 := !bits0 lor 0x40;
            | -64101862 ->
              field_field11 := (
                (
                  Atdgen_runtime.Ob_run.read_bool
                ) ib
              );
            | -64101861 ->
              field_field12 := (
                (
                  read__10
                ) ib
              );
              bits0 := !bits0 lor 0x80;
            | -64101860 ->
              field_field13 := (
                (
                  read__11
                ) ib
              );
              bits0 := !bits0 lor 0x100;
            | -64101859 ->
              field_field14 := (
                (
                  read_date
                ) ib
              );
              bits0 := !bits0 lor 0x200;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3ff then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "field2"; "field3"; "field4"; "field7"; "field8"; "field9"; "field10"; "field12"; "field13"; "field14" |];
        (
          {
            field0 = !field_field0;
            field1 = !field_field1;
            field2 = !field_field2;
            field3 = !field_field3;
            field4 = !field_field4;
            field5 = !field_field5;
            field6 = !field_field6;
            field7 = !field_field7;
            field8 = !field_field8;
            field9 = !field_field9;
            field10 = !field_field10;
            field11 = !field_field11;
            field12 = !field_field12;
            field13 = !field_field13;
            field14 = !field_field14;
          }
         : mixed_record)
)
let read_mixed_record = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_field0 = ref (None) in
    let field_field1 = ref (None) in
    let field_field2 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_field3 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_field4 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_field5 = ref (None) in
    let field_field6 = ref (None) in
    let field_field7 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_field8 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_field9 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_field10 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_field11 = ref (false) in
    let field_field12 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_field13 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_field14 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 9342518 ->
          field_field0 := (
            Some (
              (
                Atdgen_runtime.Ob_run.read_int
              ) ib
            )
          );
        | 9342519 ->
          field_field1 := (
            Some (
              (
                Atdgen_runtime.Ob_run.read_float64
              ) ib
            )
          );
        | 9342520 ->
          field_field2 := (
            (
              read__6
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 9342521 ->
          field_field3 := (
            (
              Atdgen_runtime.Ob_run.read_int64
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | 9342522 ->
          field_field4 := (
            (
              read__7
            ) ib
          );
          bits0 := !bits0 lor 0x4;
        | 9342523 ->
          field_field5 := (
            Some (
              (
                Atdgen_runtime.Ob_run.read_bool
              ) ib
            )
          );
        | 9342524 ->
          field_field6 := (
            Some (
              (
                Atdgen_runtime.Ob_run.read_string
              ) ib
            )
          );
        | 9342525 ->
          field_field7 := (
            (
              read_test_variant
            ) ib
          );
          bits0 := !bits0 lor 0x8;
        | 9342526 ->
          field_field8 := (
            (
              read__9
            ) ib
          );
          bits0 := !bits0 lor 0x10;
        | 9342527 ->
          field_field9 := (
            (
              fun ib ->
                if Bi_io.read_tag ib <> 20 then Atdgen_runtime.Ob_run.read_error_at ib;
                let len = Bi_vint.read_uvint ib in
                if len < 6 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1; 2; 3; 4; 5 ];
                let x0 =
                  (
                    Atdgen_runtime.Ob_run.read_int
                  ) ib
                in
                let x1 =
                  (
                    Atdgen_runtime.Ob_run.read_int
                  ) ib
                in
                let x2 =
                  (
                    Atdgen_runtime.Ob_run.read_char
                  ) ib
                in
                let x3 =
                  (
                    Atdgen_runtime.Ob_run.read_int
                  ) ib
                in
                let x4 =
                  (
                    Atdgen_runtime.Ob_run.read_int32
                  ) ib
                in
                let x5 =
                  (
                    Atdgen_runtime.Ob_run.read_int64
                  ) ib
                in
                for i = 6 to len - 1 do Bi_io.skip ib done;
                (x0, x1, x2, x3, x4, x5)
            ) ib
          );
          bits0 := !bits0 lor 0x20;
        | -64101863 ->
          field_field10 := (
            (
              Atdgen_runtime.Ob_run.read_bool
            ) ib
          );
          bits0 := !bits0 lor 0x40;
        | -64101862 ->
          field_field11 := (
            (
              Atdgen_runtime.Ob_run.read_bool
            ) ib
          );
        | -64101861 ->
          field_field12 := (
            (
              read__10
            ) ib
          );
          bits0 := !bits0 lor 0x80;
        | -64101860 ->
          field_field13 := (
            (
              read__11
            ) ib
          );
          bits0 := !bits0 lor 0x100;
        | -64101859 ->
          field_field14 := (
            (
              read_date
            ) ib
          );
          bits0 := !bits0 lor 0x200;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3ff then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "field2"; "field3"; "field4"; "field7"; "field8"; "field9"; "field10"; "field12"; "field13"; "field14" |];
    (
      {
        field0 = !field_field0;
        field1 = !field_field1;
        field2 = !field_field2;
        field3 = !field_field3;
        field4 = !field_field4;
        field5 = !field_field5;
        field6 = !field_field6;
        field7 = !field_field7;
        field8 = !field_field8;
        field9 = !field_field9;
        field10 = !field_field10;
        field11 = !field_field11;
        field12 = !field_field12;
        field13 = !field_field13;
        field14 = !field_field14;
      }
     : mixed_record)
)
let mixed_record_of_string ?pos s =
  read_mixed_record (Bi_inbuf.from_string ?pos s)
let _13_tag = Bi_io.array_tag
let write_untagged__13 = (
  Atdgen_runtime.Ob_run.write_untagged_array
    mixed_record_tag
    (
      write_untagged_mixed_record
    )
)
let write__13 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__13 ob x
let string_of__13 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__13 ob x;
  Bi_outbuf.contents ob
let get__13_reader = (
  Atdgen_runtime.Ob_run.get_array_reader (
    get_mixed_record_reader
  )
)
let read__13 = (
  Atdgen_runtime.Ob_run.read_array (
    get_mixed_record_reader
  )
)
let _13_of_string ?pos s =
  read__13 (Bi_inbuf.from_string ?pos s)
let _12_tag = Bi_io.table_tag
let write_untagged__12 = (
  fun ob x ->
    let len = Array.length x in
    Bi_vint.write_uvint ob len;
    if len > 0 then (
      Bi_vint.write_uvint ob 15;
      Bi_io.write_hashtag ob (9342518) true;
      Bi_io.write_tag ob _4_tag;
      Bi_io.write_hashtag ob (9342519) true;
      Bi_io.write_tag ob _5_tag;
      Bi_io.write_hashtag ob (9342520) true;
      Bi_io.write_tag ob _6_tag;
      Bi_io.write_hashtag ob (9342521) true;
      Bi_io.write_tag ob Bi_io.int64_tag;
      Bi_io.write_hashtag ob (9342522) true;
      Bi_io.write_tag ob _7_tag;
      Bi_io.write_hashtag ob (9342523) true;
      Bi_io.write_tag ob _8_tag;
      Bi_io.write_hashtag ob (9342524) true;
      Bi_io.write_tag ob _6_tag;
      Bi_io.write_hashtag ob (9342525) true;
      Bi_io.write_tag ob test_variant_tag;
      Bi_io.write_hashtag ob (9342526) true;
      Bi_io.write_tag ob _9_tag;
      Bi_io.write_hashtag ob (9342527) true;
      Bi_io.write_tag ob Bi_io.tuple_tag;
      Bi_io.write_hashtag ob (-64101863) true;
      Bi_io.write_tag ob Bi_io.bool_tag;
      Bi_io.write_hashtag ob (-64101862) true;
      Bi_io.write_tag ob Bi_io.bool_tag;
      Bi_io.write_hashtag ob (-64101861) true;
      Bi_io.write_tag ob _10_tag;
      Bi_io.write_hashtag ob (-64101860) true;
      Bi_io.write_tag ob _11_tag;
      Bi_io.write_hashtag ob (-64101859) true;
      Bi_io.write_tag ob date_tag;
      Atdgen_runtime.Ob_run.array_iter2 (fun ob x ->
        (
          write_untagged__4
        )
          ob x.field0;
        (
          write_untagged__5
        )
          ob x.field1;
        (
          write_untagged__6
        )
          ob x.field2;
        (
          Bi_io.write_untagged_int64
        )
          ob x.field3;
        (
          write_untagged__7
        )
          ob x.field4;
        (
          write_untagged__8
        )
          ob x.field5;
        (
          write_untagged__6
        )
          ob x.field6;
        (
          write_untagged_test_variant
        )
          ob x.field7;
        (
          write_untagged__9
        )
          ob x.field8;
        (
          fun ob x ->
            Bi_vint.write_uvint ob 6;
            (
              let x, _, _, _, _, _ = x in (
                Bi_io.write_uvint
              ) ob x
            );
            (
              let _, x, _, _, _, _ = x in (
                Bi_io.write_int8
              ) ob x
            );
            (
              let _, _, x, _, _, _ = x in (
                Bi_io.write_char
              ) ob x
            );
            (
              let _, _, _, x, _, _ = x in (
                Bi_io.write_int16
              ) ob x
            );
            (
              let _, _, _, _, x, _ = x in (
                Bi_io.write_int32
              ) ob x
            );
            (
              let _, _, _, _, _, x = x in (
                Bi_io.write_int64
              ) ob x
            );
        )
          ob x.field9;
        (
          Bi_io.write_untagged_bool
        )
          ob x.field10;
        (
          Bi_io.write_untagged_bool
        )
          ob x.field11;
        (
          write_untagged__10
        )
          ob x.field12;
        (
          write_untagged__11
        )
          ob x.field13;
        (
          write_untagged_date
        )
          ob x.field14;
      ) ob x;
    );
)
let write__12 ob x =
  Bi_io.write_tag ob Bi_io.table_tag;
  write_untagged__12 ob x
let string_of__12 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__12 ob x;
  Bi_outbuf.contents ob
let get__12_reader = (
  function
    | 25 -> 
      (fun ib ->
        let row_num = Bi_vint.read_uvint ib in
        if row_num = 0 then [| |]
        else
          let col_num = Bi_vint.read_uvint ib in
          let field_field0 = ref (None) in
          let field_field1 = ref (None) in
          let field_field2 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field3 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field4 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field5 = ref (None) in
          let field_field6 = ref (None) in
          let field_field7 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field8 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field9 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field10 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field11 = ref (false) in
          let field_field12 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field13 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field14 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let bits0 = ref 0 in
          let readers =
            Atdgen_runtime.Ob_run.array_init2 col_num ib (
              fun col ib ->
                let h = Bi_io.read_field_hashtag ib in
                let tag = Bi_io.read_tag ib in
                match h with
                  | 9342518 ->
                    let read =
                      (
                        get__4_reader
                      )
                        tag
                    in
                    (fun ib -> field_field0 := read ib)
                  | 9342519 ->
                    let read =
                      (
                        get__5_reader
                      )
                        tag
                    in
                    (fun ib -> field_field1 := read ib)
                  | 9342520 ->
                    bits0 := !bits0 lor 0x1;
                    let read =
                      (
                        get__6_reader
                      )
                        tag
                    in
                    (fun ib -> field_field2 := read ib)
                  | 9342521 ->
                    bits0 := !bits0 lor 0x2;
                    let read =
                      (
                        Atdgen_runtime.Ob_run.get_int64_reader
                      )
                        tag
                    in
                    (fun ib -> field_field3 := read ib)
                  | 9342522 ->
                    bits0 := !bits0 lor 0x4;
                    let read =
                      (
                        get__7_reader
                      )
                        tag
                    in
                    (fun ib -> field_field4 := read ib)
                  | 9342523 ->
                    let read =
                      (
                        get__8_reader
                      )
                        tag
                    in
                    (fun ib -> field_field5 := read ib)
                  | 9342524 ->
                    let read =
                      (
                        get__6_reader
                      )
                        tag
                    in
                    (fun ib -> field_field6 := read ib)
                  | 9342525 ->
                    bits0 := !bits0 lor 0x8;
                    let read =
                      (
                        get_test_variant_reader
                      )
                        tag
                    in
                    (fun ib -> field_field7 := read ib)
                  | 9342526 ->
                    bits0 := !bits0 lor 0x10;
                    let read =
                      (
                        get__9_reader
                      )
                        tag
                    in
                    (fun ib -> field_field8 := read ib)
                  | 9342527 ->
                    bits0 := !bits0 lor 0x20;
                    let read =
                      (
                        fun tag ->
                          if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
                            fun ib ->
                              let len = Bi_vint.read_uvint ib in
                              if len < 6 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1; 2; 3; 4; 5 ];
                              let x0 =
                                (
                                  Atdgen_runtime.Ob_run.read_int
                                ) ib
                              in
                              let x1 =
                                (
                                  Atdgen_runtime.Ob_run.read_int
                                ) ib
                              in
                              let x2 =
                                (
                                  Atdgen_runtime.Ob_run.read_char
                                ) ib
                              in
                              let x3 =
                                (
                                  Atdgen_runtime.Ob_run.read_int
                                ) ib
                              in
                              let x4 =
                                (
                                  Atdgen_runtime.Ob_run.read_int32
                                ) ib
                              in
                              let x5 =
                                (
                                  Atdgen_runtime.Ob_run.read_int64
                                ) ib
                              in
                              for i = 6 to len - 1 do Bi_io.skip ib done;
                              (x0, x1, x2, x3, x4, x5)
                      )
                        tag
                    in
                    (fun ib -> field_field9 := read ib)
                  | -64101863 ->
                    bits0 := !bits0 lor 0x40;
                    let read =
                      (
                        Atdgen_runtime.Ob_run.get_bool_reader
                      )
                        tag
                    in
                    (fun ib -> field_field10 := read ib)
                  | -64101862 ->
                    let read =
                      (
                        Atdgen_runtime.Ob_run.get_bool_reader
                      )
                        tag
                    in
                    (fun ib -> field_field11 := read ib)
                  | -64101861 ->
                    bits0 := !bits0 lor 0x80;
                    let read =
                      (
                        get__10_reader
                      )
                        tag
                    in
                    (fun ib -> field_field12 := read ib)
                  | -64101860 ->
                    bits0 := !bits0 lor 0x100;
                    let read =
                      (
                        get__11_reader
                      )
                        tag
                    in
                    (fun ib -> field_field13 := read ib)
                  | -64101859 ->
                    bits0 := !bits0 lor 0x200;
                    let read =
                      (
                        get_date_reader
                      )
                        tag
                    in
                    (fun ib -> field_field14 := read ib)
                  | _ -> (fun ib -> Bi_io.skip ib)
            )
          in
          if !bits0 <> 0x3ff then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "field2"; "field3"; "field4"; "field7"; "field8"; "field9"; "field10"; "field12"; "field13"; "field14" |];
          let a = Array.make row_num (Obj.magic 0) in
          for row = 0 to row_num - 1 do
            for i = 0 to Array.length readers - 1 do
              readers.(i) ib
            done;
            a.(row) <-
              {
                field0 = !field_field0;
                field1 = !field_field1;
                field2 = !field_field2;
                field3 = !field_field3;
                field4 = !field_field4;
                field5 = !field_field5;
                field6 = !field_field6;
                field7 = !field_field7;
                field8 = !field_field8;
                field9 = !field_field9;
                field10 = !field_field10;
                field11 = !field_field11;
                field12 = !field_field12;
                field13 = !field_field13;
                field14 = !field_field14;
              }
          done;
          a
      )
    | 19 -> 
      (fun ib ->
        Atdgen_runtime.Ob_run.read_array_value (
          get_mixed_record_reader
        ) ib
      )
    | _ -> Atdgen_runtime.Ob_run.read_error ()
)
let read__12 = (
  fun ib ->
    match Bi_io.read_tag ib with
      | 25 -> 
        let row_num = Bi_vint.read_uvint ib in
        if row_num = 0 then [| |]
        else
          let col_num = Bi_vint.read_uvint ib in
          let field_field0 = ref (None) in
          let field_field1 = ref (None) in
          let field_field2 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field3 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field4 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field5 = ref (None) in
          let field_field6 = ref (None) in
          let field_field7 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field8 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field9 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field10 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field11 = ref (false) in
          let field_field12 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field13 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let field_field14 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
          let bits0 = ref 0 in
          let readers =
            Atdgen_runtime.Ob_run.array_init2 col_num ib (
              fun col ib ->
                let h = Bi_io.read_field_hashtag ib in
                let tag = Bi_io.read_tag ib in
                match h with
                  | 9342518 ->
                    let read =
                      (
                        get__4_reader
                      )
                        tag
                    in
                    (fun ib -> field_field0 := read ib)
                  | 9342519 ->
                    let read =
                      (
                        get__5_reader
                      )
                        tag
                    in
                    (fun ib -> field_field1 := read ib)
                  | 9342520 ->
                    bits0 := !bits0 lor 0x1;
                    let read =
                      (
                        get__6_reader
                      )
                        tag
                    in
                    (fun ib -> field_field2 := read ib)
                  | 9342521 ->
                    bits0 := !bits0 lor 0x2;
                    let read =
                      (
                        Atdgen_runtime.Ob_run.get_int64_reader
                      )
                        tag
                    in
                    (fun ib -> field_field3 := read ib)
                  | 9342522 ->
                    bits0 := !bits0 lor 0x4;
                    let read =
                      (
                        get__7_reader
                      )
                        tag
                    in
                    (fun ib -> field_field4 := read ib)
                  | 9342523 ->
                    let read =
                      (
                        get__8_reader
                      )
                        tag
                    in
                    (fun ib -> field_field5 := read ib)
                  | 9342524 ->
                    let read =
                      (
                        get__6_reader
                      )
                        tag
                    in
                    (fun ib -> field_field6 := read ib)
                  | 9342525 ->
                    bits0 := !bits0 lor 0x8;
                    let read =
                      (
                        get_test_variant_reader
                      )
                        tag
                    in
                    (fun ib -> field_field7 := read ib)
                  | 9342526 ->
                    bits0 := !bits0 lor 0x10;
                    let read =
                      (
                        get__9_reader
                      )
                        tag
                    in
                    (fun ib -> field_field8 := read ib)
                  | 9342527 ->
                    bits0 := !bits0 lor 0x20;
                    let read =
                      (
                        fun tag ->
                          if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
                            fun ib ->
                              let len = Bi_vint.read_uvint ib in
                              if len < 6 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1; 2; 3; 4; 5 ];
                              let x0 =
                                (
                                  Atdgen_runtime.Ob_run.read_int
                                ) ib
                              in
                              let x1 =
                                (
                                  Atdgen_runtime.Ob_run.read_int
                                ) ib
                              in
                              let x2 =
                                (
                                  Atdgen_runtime.Ob_run.read_char
                                ) ib
                              in
                              let x3 =
                                (
                                  Atdgen_runtime.Ob_run.read_int
                                ) ib
                              in
                              let x4 =
                                (
                                  Atdgen_runtime.Ob_run.read_int32
                                ) ib
                              in
                              let x5 =
                                (
                                  Atdgen_runtime.Ob_run.read_int64
                                ) ib
                              in
                              for i = 6 to len - 1 do Bi_io.skip ib done;
                              (x0, x1, x2, x3, x4, x5)
                      )
                        tag
                    in
                    (fun ib -> field_field9 := read ib)
                  | -64101863 ->
                    bits0 := !bits0 lor 0x40;
                    let read =
                      (
                        Atdgen_runtime.Ob_run.get_bool_reader
                      )
                        tag
                    in
                    (fun ib -> field_field10 := read ib)
                  | -64101862 ->
                    let read =
                      (
                        Atdgen_runtime.Ob_run.get_bool_reader
                      )
                        tag
                    in
                    (fun ib -> field_field11 := read ib)
                  | -64101861 ->
                    bits0 := !bits0 lor 0x80;
                    let read =
                      (
                        get__10_reader
                      )
                        tag
                    in
                    (fun ib -> field_field12 := read ib)
                  | -64101860 ->
                    bits0 := !bits0 lor 0x100;
                    let read =
                      (
                        get__11_reader
                      )
                        tag
                    in
                    (fun ib -> field_field13 := read ib)
                  | -64101859 ->
                    bits0 := !bits0 lor 0x200;
                    let read =
                      (
                        get_date_reader
                      )
                        tag
                    in
                    (fun ib -> field_field14 := read ib)
                  | _ -> (fun ib -> Bi_io.skip ib)
            )
          in
          if !bits0 <> 0x3ff then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "field2"; "field3"; "field4"; "field7"; "field8"; "field9"; "field10"; "field12"; "field13"; "field14" |];
          let a = Array.make row_num (Obj.magic 0) in
          for row = 0 to row_num - 1 do
            for i = 0 to Array.length readers - 1 do
              readers.(i) ib
            done;
            a.(row) <-
              {
                field0 = !field_field0;
                field1 = !field_field1;
                field2 = !field_field2;
                field3 = !field_field3;
                field4 = !field_field4;
                field5 = !field_field5;
                field6 = !field_field6;
                field7 = !field_field7;
                field8 = !field_field8;
                field9 = !field_field9;
                field10 = !field_field10;
                field11 = !field_field11;
                field12 = !field_field12;
                field13 = !field_field13;
                field14 = !field_field14;
              }
          done;
          a
      | 19 -> 
        Atdgen_runtime.Ob_run.read_array_value (
          get_mixed_record_reader
        ) ib
      | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let _12_of_string ?pos s =
  read__12 (Bi_inbuf.from_string ?pos s)
let _14_tag = Bi_io.array_tag
let write_untagged__14 = (
  Atdgen_runtime.Ob_run.write_untagged_list
    Bi_io.tuple_tag
    (
      fun ob x ->
        Bi_vint.write_uvint ob 2;
        (
          let x, _ = x in (
            write__12
          ) ob x
        );
        (
          let _, x = x in (
            write__13
          ) ob x
        );
    )
)
let write__14 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__14 ob x
let string_of__14 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__14 ob x;
  Bi_outbuf.contents ob
let get__14_reader = (
  Atdgen_runtime.Ob_run.get_list_reader (
    fun tag ->
      if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
        fun ib ->
          let len = Bi_vint.read_uvint ib in
          if len < 2 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1 ];
          let x0 =
            (
              read__12
            ) ib
          in
          let x1 =
            (
              read__13
            ) ib
          in
          for i = 2 to len - 1 do Bi_io.skip ib done;
          (x0, x1)
  )
)
let read__14 = (
  Atdgen_runtime.Ob_run.read_list (
    fun tag ->
      if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
        fun ib ->
          let len = Bi_vint.read_uvint ib in
          if len < 2 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1 ];
          let x0 =
            (
              read__12
            ) ib
          in
          let x1 =
            (
              read__13
            ) ib
          in
          for i = 2 to len - 1 do Bi_io.skip ib done;
          (x0, x1)
  )
)
let _14_of_string ?pos s =
  read__14 (Bi_inbuf.from_string ?pos s)
let mixed_tag = Bi_io.array_tag
let write_untagged_mixed = (
  write_untagged__14
)
let write_mixed ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged_mixed ob x
let string_of_mixed ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_mixed ob x;
  Bi_outbuf.contents ob
let get_mixed_reader = (
  get__14_reader
)
let read_mixed = (
  read__14
)
let mixed_of_string ?pos s =
  read_mixed (Bi_inbuf.from_string ?pos s)
let _15_tag = Bi_io.array_tag
let write_untagged__15 = (
  Atdgen_runtime.Ob_run.write_untagged_list
    mixed_record_tag
    (
      write_untagged_mixed_record
    )
)
let write__15 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__15 ob x
let string_of__15 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__15 ob x;
  Bi_outbuf.contents ob
let get__15_reader = (
  Atdgen_runtime.Ob_run.get_list_reader (
    get_mixed_record_reader
  )
)
let read__15 = (
  Atdgen_runtime.Ob_run.read_list (
    get_mixed_record_reader
  )
)
let _15_of_string ?pos s =
  read__15 (Bi_inbuf.from_string ?pos s)
let test_tag = Bi_io.record_tag
let write_untagged_test : Bi_outbuf.t -> test -> unit = (
  fun ob x ->
    let len = ref 3 in
    let x_x0 = x.x0 in
    if x_x0 != None then incr len;
    let x_x1 = x.x1 in
    if x_x1 != None then incr len;
    Bi_vint.write_uvint ob !len;
    (match x_x0 with None -> () | Some x ->
      Bi_outbuf.add_char4 ob '\128' '\000' 'h' '\184';
      (
        Bi_io.write_svint
      ) ob x;
    );
    (match x_x1 with None -> () | Some x ->
      Bi_outbuf.add_char4 ob '\128' '\000' 'h' '\185';
      (
        Bi_io.write_float64
      ) ob x;
    );
    Bi_outbuf.add_char4 ob '\128' '\000' 'h' '\186';
    (
      write_mixed
    ) ob x.x2;
    Bi_outbuf.add_char4 ob '\128' '\000' 'h' '\187';
    (
      write__15
    ) ob x.x3;
    Bi_outbuf.add_char4 ob '\128' '\000' 'h' '\188';
    (
      Bi_io.write_int64
    ) ob x.x4;
)
let write_test ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_test ob x
let string_of_test ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_test ob x;
  Bi_outbuf.contents ob
let get_test_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_x0 = ref (None) in
        let field_x1 = ref (None) in
        let field_x2 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_x3 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_x4 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 26808 ->
              field_x0 := (
                Some (
                  (
                    Atdgen_runtime.Ob_run.read_int
                  ) ib
                )
              );
            | 26809 ->
              field_x1 := (
                Some (
                  (
                    Atdgen_runtime.Ob_run.read_float64
                  ) ib
                )
              );
            | 26810 ->
              field_x2 := (
                (
                  read_mixed
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 26811 ->
              field_x3 := (
                (
                  read__15
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | 26812 ->
              field_x4 := (
                (
                  Atdgen_runtime.Ob_run.read_int64
                ) ib
              );
              bits0 := !bits0 lor 0x4;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x7 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "x2"; "x3"; "x4" |];
        (
          {
            x0 = !field_x0;
            x1 = !field_x1;
            x2 = !field_x2;
            x3 = !field_x3;
            x4 = !field_x4;
          }
         : test)
)
let read_test = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_x0 = ref (None) in
    let field_x1 = ref (None) in
    let field_x2 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_x3 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_x4 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 26808 ->
          field_x0 := (
            Some (
              (
                Atdgen_runtime.Ob_run.read_int
              ) ib
            )
          );
        | 26809 ->
          field_x1 := (
            Some (
              (
                Atdgen_runtime.Ob_run.read_float64
              ) ib
            )
          );
        | 26810 ->
          field_x2 := (
            (
              read_mixed
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 26811 ->
          field_x3 := (
            (
              read__15
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | 26812 ->
          field_x4 := (
            (
              Atdgen_runtime.Ob_run.read_int64
            ) ib
          );
          bits0 := !bits0 lor 0x4;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x7 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "x2"; "x3"; "x4" |];
    (
      {
        x0 = !field_x0;
        x1 = !field_x1;
        x2 = !field_x2;
        x3 = !field_x3;
        x4 = !field_x4;
      }
     : test)
)
let test_of_string ?pos s =
  read_test (Bi_inbuf.from_string ?pos s)
let tup_tag = Bi_io.tuple_tag
let write_untagged_tup = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    (
      let x, _ = x in (
        Bi_io.write_svint
      ) ob x
    );
    (
      let _, x = x in (
        write_test
      ) ob x
    );
)
let write_tup ob x =
  Bi_io.write_tag ob Bi_io.tuple_tag;
  write_untagged_tup ob x
let string_of_tup ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tup ob x;
  Bi_outbuf.contents ob
let get_tup_reader = (
  fun tag ->
    if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let len = Bi_vint.read_uvint ib in
        if len < 2 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1 ];
        let x0 =
          (
            Atdgen_runtime.Ob_run.read_int
          ) ib
        in
        let x1 =
          (
            read_test
          ) ib
        in
        for i = 2 to len - 1 do Bi_io.skip ib done;
        (x0, x1)
)
let read_tup = (
  fun ib ->
    if Bi_io.read_tag ib <> 20 then Atdgen_runtime.Ob_run.read_error_at ib;
    let len = Bi_vint.read_uvint ib in
    if len < 2 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1 ];
    let x0 =
      (
        Atdgen_runtime.Ob_run.read_int
      ) ib
    in
    let x1 =
      (
        read_test
      ) ib
    in
    for i = 2 to len - 1 do Bi_io.skip ib done;
    (x0, x1)
)
let tup_of_string ?pos s =
  read_tup (Bi_inbuf.from_string ?pos s)
let star_rating_tag = Bi_io.svint_tag
let write_untagged_star_rating = (
  Bi_io.write_untagged_svint
)
let write_star_rating ob x =
  Bi_io.write_tag ob Bi_io.svint_tag;
  write_untagged_star_rating ob x
let string_of_star_rating ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_star_rating ob x;
  Bi_outbuf.contents ob
let get_star_rating_reader = (
  Atdgen_runtime.Ob_run.get_int_reader
)
let read_star_rating = (
  Atdgen_runtime.Ob_run.read_int
)
let star_rating_of_string ?pos s =
  read_star_rating (Bi_inbuf.from_string ?pos s)
let _30_tag = Bi_io.record_tag
let write_untagged__30 : Bi_outbuf.t -> _ generic -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 1;
    Bi_outbuf.add_char4 ob '\240' 'G' '\003' '\130';
    (
      Bi_io.write_svint
    ) ob x.x294623;
)
let write__30 ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged__30 ob x
let string_of__30 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__30 ob x;
  Bi_outbuf.contents ob
let get__30_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_x294623 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | -263781502 ->
              field_x294623 := (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x1 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "x294623" |];
        (
          {
            x294623 = !field_x294623;
          }
         : _ generic)
)
let read__30 = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_x294623 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | -263781502 ->
          field_x294623 := (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x1 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "x294623" |];
    (
      {
        x294623 = !field_x294623;
      }
     : _ generic)
)
let _30_of_string ?pos s =
  read__30 (Bi_inbuf.from_string ?pos s)
let specialized_tag = Bi_io.record_tag
let write_untagged_specialized = (
  write_untagged__30
)
let write_specialized ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_specialized ob x
let string_of_specialized ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_specialized ob x;
  Bi_outbuf.contents ob
let get_specialized_reader = (
  get__30_reader
)
let read_specialized = (
  read__30
)
let specialized_of_string ?pos s =
  read_specialized (Bi_inbuf.from_string ?pos s)
let some_record_tag = Bi_io.record_tag
let write_untagged_some_record : Bi_outbuf.t -> some_record -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 1;
    Bi_outbuf.add_char4 ob '\151' '\r' '\173' '\239';
    (
      Bi_io.write_svint
    ) ob x.some_field;
)
let write_some_record ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_some_record ob x
let string_of_some_record ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_some_record ob x;
  Bi_outbuf.contents ob
let get_some_record_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_some_field = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 386772463 ->
              field_some_field := (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x1 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "some_field" |];
        (
          {
            some_field = !field_some_field;
          }
         : some_record)
)
let read_some_record = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_some_field = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 386772463 ->
          field_some_field := (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x1 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "some_field" |];
    (
      {
        some_field = !field_some_field;
      }
     : some_record)
)
let some_record_of_string ?pos s =
  read_some_record (Bi_inbuf.from_string ?pos s)
let precision_tag = Bi_io.record_tag
let write_untagged_precision : Bi_outbuf.t -> precision -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 3;
    Bi_outbuf.add_char4 ob '\204' '\249' 'C' '\200';
    (
      Bi_io.write_float64
    ) ob x.sqrt2_5;
    Bi_outbuf.add_char4 ob '\228' '\158' 'C' 'z';
    (
      Bi_io.write_float64
    ) ob x.small_2;
    Bi_outbuf.add_char4 ob '\187' '\188' '+' 'n';
    (
      Bi_io.write_float64
    ) ob x.large_2;
)
let write_precision ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_precision ob x
let string_of_precision ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_precision ob x;
  Bi_outbuf.contents ob
let get_precision_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_sqrt2_5 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_small_2 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_large_2 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | -856079416 ->
              field_sqrt2_5 := (
                (
                  Atdgen_runtime.Ob_run.read_float64
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | -459390086 ->
              field_small_2 := (
                (
                  Atdgen_runtime.Ob_run.read_float64
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | 1002187630 ->
              field_large_2 := (
                (
                  Atdgen_runtime.Ob_run.read_float64
                ) ib
              );
              bits0 := !bits0 lor 0x4;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x7 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "sqrt2_5"; "small_2"; "large_2" |];
        (
          {
            sqrt2_5 = !field_sqrt2_5;
            small_2 = !field_small_2;
            large_2 = !field_large_2;
          }
         : precision)
)
let read_precision = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_sqrt2_5 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_small_2 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_large_2 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | -856079416 ->
          field_sqrt2_5 := (
            (
              Atdgen_runtime.Ob_run.read_float64
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | -459390086 ->
          field_small_2 := (
            (
              Atdgen_runtime.Ob_run.read_float64
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | 1002187630 ->
          field_large_2 := (
            (
              Atdgen_runtime.Ob_run.read_float64
            ) ib
          );
          bits0 := !bits0 lor 0x4;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x7 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "sqrt2_5"; "small_2"; "large_2" |];
    (
      {
        sqrt2_5 = !field_sqrt2_5;
        small_2 = !field_small_2;
        large_2 = !field_large_2;
      }
     : precision)
)
let precision_of_string ?pos s =
  read_precision (Bi_inbuf.from_string ?pos s)
let p''_tag = Bi_io.variant_tag
let write_untagged_p'' = (
  write_untagged__1
)
let write_p'' ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_p'' ob x
let string_of_p'' ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_p'' ob x;
  Bi_outbuf.contents ob
let get_p''_reader = (
  get__1_reader
)
let read_p'' = (
  read__1
)
let p''_of_string ?pos s =
  read_p'' (Bi_inbuf.from_string ?pos s)
let _18_tag = Bi_io.num_variant_tag
let write_untagged__18 = (
  Atdgen_runtime.Ob_run.write_untagged_option (
    Bi_io.write_svint
  )
)
let write__18 ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged__18 ob x
let string_of__18 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__18 ob x;
  Bi_outbuf.contents ob
let get__18_reader = (
  fun tag ->
    if tag <> 22 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        match Char.code (Bi_inbuf.read_char ib) with
          | 0 -> None
          | 0x80 ->
            Some (
              (
                Atdgen_runtime.Ob_run.read_int
              )
                ib
            )
          | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let read__18 = (
  fun ib ->
    if Bi_io.read_tag ib <> 22 then Atdgen_runtime.Ob_run.read_error_at ib;
    match Char.code (Bi_inbuf.read_char ib) with
      | 0 -> None
      | 0x80 ->
        Some (
          (
            Atdgen_runtime.Ob_run.read_int
          )
            ib
        )
      | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let _18_of_string ?pos s =
  read__18 (Bi_inbuf.from_string ?pos s)
let option_validation_tag = Bi_io.num_variant_tag
let write_untagged_option_validation = (
  write_untagged__18
)
let write_option_validation ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged_option_validation ob x
let string_of_option_validation ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_option_validation ob x;
  Bi_outbuf.contents ob
let get_option_validation_reader = (
  get__18_reader
)
let read_option_validation = (
  read__18
)
let option_validation_of_string ?pos s =
  read_option_validation (Bi_inbuf.from_string ?pos s)
let _28_tag = some_record_tag
let write_untagged__28 = (
  write_untagged_some_record
)
let write__28 ob x =
  Bi_io.write_tag ob some_record_tag;
  write_untagged__28 ob x
let string_of__28 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__28 ob x;
  Bi_outbuf.contents ob
let get__28_reader = (
  get_some_record_reader
)
let read__28 = (
  read_some_record
)
let _28_of_string ?pos s =
  read__28 (Bi_inbuf.from_string ?pos s)
let no_real_wrap_tag = some_record_tag
let write_untagged_no_real_wrap = (
  write_untagged__28
)
let write_no_real_wrap ob x =
  Bi_io.write_tag ob some_record_tag;
  write_untagged_no_real_wrap ob x
let string_of_no_real_wrap ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_no_real_wrap ob x;
  Bi_outbuf.contents ob
let get_no_real_wrap_reader = (
  get__28_reader
)
let read_no_real_wrap = (
  read__28
)
let no_real_wrap_of_string ?pos s =
  read_no_real_wrap (Bi_inbuf.from_string ?pos s)
let _26_tag = Bi_io.svint_tag
let write_untagged__26 = (
  fun ob x -> (
    let x = ( Test_lib.Natural.unwrap ) x in (
      Bi_io.write_untagged_svint
    ) ob x)
)
let write__26 ob x =
  Bi_io.write_tag ob Bi_io.svint_tag;
  write_untagged__26 ob x
let string_of__26 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__26 ob x;
  Bi_outbuf.contents ob
let get__26_reader = (
  fun tag ib ->
    ( Test_lib.Natural.wrap ) ((
      Atdgen_runtime.Ob_run.get_int_reader
    ) tag ib)
)
let read__26 = (
  fun ib ->
    ( Test_lib.Natural.wrap ) ((
      Atdgen_runtime.Ob_run.read_int
    ) ib)
)
let _26_of_string ?pos s =
  read__26 (Bi_inbuf.from_string ?pos s)
let natural_tag = Bi_io.svint_tag
let write_untagged_natural = (
  write_untagged__26
)
let write_natural ob x =
  Bi_io.write_tag ob Bi_io.svint_tag;
  write_untagged_natural ob x
let string_of_natural ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_natural ob x;
  Bi_outbuf.contents ob
let get_natural_reader = (
  get__26_reader
)
let read_natural = (
  read__26
)
let natural_of_string ?pos s =
  read_natural (Bi_inbuf.from_string ?pos s)
let _24_tag = Bi_io.string_tag
let write_untagged__24 = (
  fun ob x -> (
    let x = ( function `Id s -> s ) x in (
      Bi_io.write_untagged_string
    ) ob x)
)
let write__24 ob x =
  Bi_io.write_tag ob Bi_io.string_tag;
  write_untagged__24 ob x
let string_of__24 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__24 ob x;
  Bi_outbuf.contents ob
let get__24_reader = (
  fun tag ib ->
    ( fun s -> `Id s ) ((
      Atdgen_runtime.Ob_run.get_string_reader
    ) tag ib)
)
let read__24 = (
  fun ib ->
    ( fun s -> `Id s ) ((
      Atdgen_runtime.Ob_run.read_string
    ) ib)
)
let _24_of_string ?pos s =
  read__24 (Bi_inbuf.from_string ?pos s)
let id_tag = Bi_io.string_tag
let write_untagged_id = (
  write_untagged__24
)
let write_id ob x =
  Bi_io.write_tag ob Bi_io.string_tag;
  write_untagged_id ob x
let string_of_id ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_id ob x;
  Bi_outbuf.contents ob
let get_id_reader = (
  get__24_reader
)
let read_id = (
  read__24
)
let id_of_string ?pos s =
  read_id (Bi_inbuf.from_string ?pos s)
let _25_tag = Bi_io.array_tag
let write_untagged__25 = (
  Atdgen_runtime.Ob_run.write_untagged_list
    Bi_io.tuple_tag
    (
      fun ob x ->
        Bi_vint.write_uvint ob 2;
        (
          let x, _ = x in (
            write_id
          ) ob x
        );
        (
          let _, x = x in (
            Bi_io.write_svint
          ) ob x
        );
    )
)
let write__25 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__25 ob x
let string_of__25 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__25 ob x;
  Bi_outbuf.contents ob
let get__25_reader = (
  Atdgen_runtime.Ob_run.get_list_reader (
    fun tag ->
      if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
        fun ib ->
          let len = Bi_vint.read_uvint ib in
          if len < 2 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1 ];
          let x0 =
            (
              read_id
            ) ib
          in
          let x1 =
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          in
          for i = 2 to len - 1 do Bi_io.skip ib done;
          (x0, x1)
  )
)
let read__25 = (
  Atdgen_runtime.Ob_run.read_list (
    fun tag ->
      if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
        fun ib ->
          let len = Bi_vint.read_uvint ib in
          if len < 2 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1 ];
          let x0 =
            (
              read_id
            ) ib
          in
          let x1 =
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          in
          for i = 2 to len - 1 do Bi_io.skip ib done;
          (x0, x1)
  )
)
let _25_of_string ?pos s =
  read__25 (Bi_inbuf.from_string ?pos s)
let json_map_tag = Bi_io.array_tag
let write_untagged_json_map = (
  write_untagged__25
)
let write_json_map ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged_json_map ob x
let string_of_json_map ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_json_map ob x;
  Bi_outbuf.contents ob
let get_json_map_reader = (
  get__25_reader
)
let read_json_map = (
  read__25
)
let json_map_of_string ?pos s =
  read_json_map (Bi_inbuf.from_string ?pos s)
let intopt_tag = Bi_io.num_variant_tag
let write_untagged_intopt = (
  write_untagged__4
)
let write_intopt ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged_intopt ob x
let string_of_intopt ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_intopt ob x;
  Bi_outbuf.contents ob
let get_intopt_reader = (
  get__4_reader
)
let read_intopt = (
  read__4
)
let intopt_of_string ?pos s =
  read_intopt (Bi_inbuf.from_string ?pos s)
let _21_tag = Bi_io.array_tag
let write_untagged__21 = (
  Atdgen_runtime.Ob_run.write_untagged_list
    Bi_io.tuple_tag
    (
      fun ob x ->
        Bi_vint.write_uvint ob 2;
        (
          let x, _ = x in (
            Bi_io.write_string
          ) ob x
        );
        (
          let _, x = x in (
            Bi_io.write_svint
          ) ob x
        );
    )
)
let write__21 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__21 ob x
let string_of__21 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__21 ob x;
  Bi_outbuf.contents ob
let get__21_reader = (
  Atdgen_runtime.Ob_run.get_list_reader (
    fun tag ->
      if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
        fun ib ->
          let len = Bi_vint.read_uvint ib in
          if len < 2 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1 ];
          let x0 =
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          in
          let x1 =
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          in
          for i = 2 to len - 1 do Bi_io.skip ib done;
          (x0, x1)
  )
)
let read__21 = (
  Atdgen_runtime.Ob_run.read_list (
    fun tag ->
      if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
        fun ib ->
          let len = Bi_vint.read_uvint ib in
          if len < 2 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1 ];
          let x0 =
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          in
          let x1 =
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          in
          for i = 2 to len - 1 do Bi_io.skip ib done;
          (x0, x1)
  )
)
let _21_of_string ?pos s =
  read__21 (Bi_inbuf.from_string ?pos s)
let int_assoc_list_tag = Bi_io.array_tag
let write_untagged_int_assoc_list = (
  write_untagged__21
)
let write_int_assoc_list ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged_int_assoc_list ob x
let string_of_int_assoc_list ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_int_assoc_list ob x;
  Bi_outbuf.contents ob
let get_int_assoc_list_reader = (
  get__21_reader
)
let read_int_assoc_list = (
  read__21
)
let int_assoc_list_of_string ?pos s =
  read_int_assoc_list (Bi_inbuf.from_string ?pos s)
let _22_tag = Bi_io.array_tag
let write_untagged__22 = (
  Atdgen_runtime.Ob_run.write_untagged_array
    Bi_io.tuple_tag
    (
      fun ob x ->
        Bi_vint.write_uvint ob 2;
        (
          let x, _ = x in (
            Bi_io.write_string
          ) ob x
        );
        (
          let _, x = x in (
            Bi_io.write_svint
          ) ob x
        );
    )
)
let write__22 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__22 ob x
let string_of__22 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__22 ob x;
  Bi_outbuf.contents ob
let get__22_reader = (
  Atdgen_runtime.Ob_run.get_array_reader (
    fun tag ->
      if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
        fun ib ->
          let len = Bi_vint.read_uvint ib in
          if len < 2 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1 ];
          let x0 =
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          in
          let x1 =
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          in
          for i = 2 to len - 1 do Bi_io.skip ib done;
          (x0, x1)
  )
)
let read__22 = (
  Atdgen_runtime.Ob_run.read_array (
    fun tag ->
      if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
        fun ib ->
          let len = Bi_vint.read_uvint ib in
          if len < 2 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1 ];
          let x0 =
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          in
          let x1 =
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          in
          for i = 2 to len - 1 do Bi_io.skip ib done;
          (x0, x1)
  )
)
let _22_of_string ?pos s =
  read__22 (Bi_inbuf.from_string ?pos s)
let int_assoc_array_tag = Bi_io.array_tag
let write_untagged_int_assoc_array = (
  write_untagged__22
)
let write_int_assoc_array ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged_int_assoc_array ob x
let string_of_int_assoc_array ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_int_assoc_array ob x;
  Bi_outbuf.contents ob
let get_int_assoc_array_reader = (
  get__22_reader
)
let read_int_assoc_array = (
  read__22
)
let int_assoc_array_of_string ?pos s =
  read_int_assoc_array (Bi_inbuf.from_string ?pos s)
let int8_tag = Bi_io.int8_tag
let write_untagged_int8 = (
  Bi_io.write_untagged_int8
)
let write_int8 ob x =
  Bi_io.write_tag ob Bi_io.int8_tag;
  write_untagged_int8 ob x
let string_of_int8 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_int8 ob x;
  Bi_outbuf.contents ob
let get_int8_reader = (
  Atdgen_runtime.Ob_run.get_int_reader
)
let read_int8 = (
  Atdgen_runtime.Ob_run.read_int
)
let int8_of_string ?pos s =
  read_int8 (Bi_inbuf.from_string ?pos s)
let int64_tag = Bi_io.int64_tag
let write_untagged_int64 = (
  Bi_io.write_untagged_int64
)
let write_int64 ob x =
  Bi_io.write_tag ob Bi_io.int64_tag;
  write_untagged_int64 ob x
let string_of_int64 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_int64 ob x;
  Bi_outbuf.contents ob
let get_int64_reader = (
  Atdgen_runtime.Ob_run.get_int64_reader
)
let read_int64 = (
  Atdgen_runtime.Ob_run.read_int64
)
let int64_of_string ?pos s =
  read_int64 (Bi_inbuf.from_string ?pos s)
let int32_tag = Bi_io.int32_tag
let write_untagged_int32 = (
  Bi_io.write_untagged_int32
)
let write_int32 ob x =
  Bi_io.write_tag ob Bi_io.int32_tag;
  write_untagged_int32 ob x
let string_of_int32 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_int32 ob x;
  Bi_outbuf.contents ob
let get_int32_reader = (
  Atdgen_runtime.Ob_run.get_int32_reader
)
let read_int32 = (
  Atdgen_runtime.Ob_run.read_int32
)
let int32_of_string ?pos s =
  read_int32 (Bi_inbuf.from_string ?pos s)
let hello_tag = Bi_io.variant_tag
let write_untagged_hello = (
  fun ob x ->
    match x with
      | `Hello x ->
        Bi_outbuf.add_char4 ob '\183' '\238' '\162' '\242';
        (
          Bi_io.write_string
        ) ob x
      | `World -> Bi_outbuf.add_char4 ob 'a' '\146' '\006' '2'
)
let write_hello ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_hello ob x
let string_of_hello ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_hello ob x;
  Bi_outbuf.contents ob
let get_hello_reader = (
  fun tag ->
    if tag <> 23 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | 938386162, true -> (`Hello (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              ))
            | -510523854, false -> `World
            | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
        )
)
let read_hello = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Atdgen_runtime.Ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | 938386162, true -> (`Hello (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          ))
        | -510523854, false -> `World
        | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
    )
)
let hello_of_string ?pos s =
  read_hello (Bi_inbuf.from_string ?pos s)
let generic_tag = Bi_io.record_tag
let write_untagged_generic _a_tag write_untagged__a write__a : Bi_outbuf.t -> 'a generic -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 1;
    Bi_outbuf.add_char4 ob '\240' 'G' '\003' '\130';
    (
      Bi_io.write_svint
    ) ob x.x294623;
)
let write_generic _a_tag write_untagged__a write__a ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_generic _a_tag write_untagged__a write__a ob x
let string_of_generic _a_tag write_untagged__a write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_generic _a_tag write_untagged__a write__a ob x;
  Bi_outbuf.contents ob
let get_generic_reader get__a_reader read__a = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_x294623 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | -263781502 ->
              field_x294623 := (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x1 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "x294623" |];
        (
          {
            x294623 = !field_x294623;
          }
         : 'a generic)
)
let read_generic get__a_reader read__a = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_x294623 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | -263781502 ->
          field_x294623 := (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x1 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "x294623" |];
    (
      {
        x294623 = !field_x294623;
      }
     : 'a generic)
)
let generic_of_string get__a_reader read__a ?pos s =
  read_generic get__a_reader read__a (Bi_inbuf.from_string ?pos s)
let floats_tag = Bi_io.record_tag
let write_untagged_floats : Bi_outbuf.t -> floats -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\128' 'M' '\146' '\133';
    (
      Bi_io.write_float32
    ) ob x.f32;
    Bi_outbuf.add_char4 ob '\128' 'M' '\149' '$';
    (
      Bi_io.write_float64
    ) ob x.f64;
)
let write_floats ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_floats ob x
let string_of_floats ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_floats ob x;
  Bi_outbuf.contents ob
let get_floats_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_f32 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_f64 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 5083781 ->
              field_f32 := (
                (
                  Atdgen_runtime.Ob_run.read_float32
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 5084452 ->
              field_f64 := (
                (
                  Atdgen_runtime.Ob_run.read_float64
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "f32"; "f64" |];
        (
          {
            f32 = !field_f32;
            f64 = !field_f64;
          }
         : floats)
)
let read_floats = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_f32 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_f64 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 5083781 ->
          field_f32 := (
            (
              Atdgen_runtime.Ob_run.read_float32
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 5084452 ->
          field_f64 := (
            (
              Atdgen_runtime.Ob_run.read_float64
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "f32"; "f64" |];
    (
      {
        f32 = !field_f32;
        f64 = !field_f64;
      }
     : floats)
)
let floats_of_string ?pos s =
  read_floats (Bi_inbuf.from_string ?pos s)
let _17_tag = Bi_io.array_tag
let write_untagged__17 = (
  Atdgen_runtime.Ob_run.write_untagged_list
    Bi_io.string_tag
    (
      Bi_io.write_untagged_string
    )
)
let write__17 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__17 ob x
let string_of__17 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__17 ob x;
  Bi_outbuf.contents ob
let get__17_reader = (
  Atdgen_runtime.Ob_run.get_list_reader (
    Atdgen_runtime.Ob_run.get_string_reader
  )
)
let read__17 = (
  Atdgen_runtime.Ob_run.read_list (
    Atdgen_runtime.Ob_run.get_string_reader
  )
)
let _17_of_string ?pos s =
  read__17 (Bi_inbuf.from_string ?pos s)
let extended_tuple_tag = Bi_io.tuple_tag
let write_untagged_extended_tuple = (
  fun ob x ->
    Bi_vint.write_uvint ob 6;
    (
      let x, _, _, _, _, _ = x in (
        Bi_io.write_svint
      ) ob x
    );
    (
      let _, x, _, _, _, _ = x in (
        Bi_io.write_float64
      ) ob x
    );
    (
      let _, _, x, _, _, _ = x in (
        Bi_io.write_bool
      ) ob x
    );
    (
      let _, _, _, x, _, _ = x in (
        write__4
      ) ob x
    );
    (
      let _, _, _, _, x, _ = x in (
        Bi_io.write_string
      ) ob x
    );
    (
      let _, _, _, _, _, x = x in (
        write__17
      ) ob x
    );
)
let write_extended_tuple ob x =
  Bi_io.write_tag ob Bi_io.tuple_tag;
  write_untagged_extended_tuple ob x
let string_of_extended_tuple ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_extended_tuple ob x;
  Bi_outbuf.contents ob
let get_extended_tuple_reader = (
  fun tag ->
    if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let len = Bi_vint.read_uvint ib in
        if len < 5 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1; 4 ];
        let x0 =
          (
            Atdgen_runtime.Ob_run.read_int
          ) ib
        in
        let x1 =
          (
            Atdgen_runtime.Ob_run.read_float64
          ) ib
        in
        let x2 =
          (
            Atdgen_runtime.Ob_run.read_bool
          ) ib
        in
        let x3 =
          (
            read__4
          ) ib
        in
        let x4 =
          (
            Atdgen_runtime.Ob_run.read_string
          ) ib
        in
        let x5 =
          if len >= 6 then (
            read__17
          ) ib
          else
            []
        in
        for i = 6 to len - 1 do Bi_io.skip ib done;
        (x0, x1, x2, x3, x4, x5)
)
let read_extended_tuple = (
  fun ib ->
    if Bi_io.read_tag ib <> 20 then Atdgen_runtime.Ob_run.read_error_at ib;
    let len = Bi_vint.read_uvint ib in
    if len < 5 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1; 4 ];
    let x0 =
      (
        Atdgen_runtime.Ob_run.read_int
      ) ib
    in
    let x1 =
      (
        Atdgen_runtime.Ob_run.read_float64
      ) ib
    in
    let x2 =
      (
        Atdgen_runtime.Ob_run.read_bool
      ) ib
    in
    let x3 =
      (
        read__4
      ) ib
    in
    let x4 =
      (
        Atdgen_runtime.Ob_run.read_string
      ) ib
    in
    let x5 =
      if len >= 6 then (
        read__17
      ) ib
      else
        []
    in
    for i = 6 to len - 1 do Bi_io.skip ib done;
    (x0, x1, x2, x3, x4, x5)
)
let extended_tuple_of_string ?pos s =
  read_extended_tuple (Bi_inbuf.from_string ?pos s)
let extended_tag = Bi_io.record_tag
let write_untagged_extended : Bi_outbuf.t -> extended -> unit = (
  fun ob x ->
    let len = ref 4 in
    let x_b3x = x.b3x in
    if x_b3x != None then incr len;
    let x_b5x = x.b5x in
    if x_b5x != 0.5 then incr len;
    Bi_vint.write_uvint ob !len;
    Bi_outbuf.add_char4 ob '\128' '\000' 'U' '\142';
    (
      Bi_io.write_svint
    ) ob x.b0x;
    Bi_outbuf.add_char4 ob '\128' '\000' 'U' '\143';
    (
      Bi_io.write_bool
    ) ob x.b1x;
    Bi_outbuf.add_char4 ob '\128' '\000' 'U' '\144';
    (
      Bi_io.write_string
    ) ob x.b2x;
    (match x_b3x with None -> () | Some x ->
      Bi_outbuf.add_char4 ob '\128' '\000' 'U' '\145';
      (
        Bi_io.write_string
      ) ob x;
    );
    Bi_outbuf.add_char4 ob '\128' '\000' 'U' '\146';
    (
      write__6
    ) ob x.b4x;
    if x_b5x != 0.5 then (
      Bi_outbuf.add_char4 ob '\128' '\000' 'U' '\147';
      (
        Bi_io.write_float64
      ) ob x_b5x;
    );
)
let write_extended ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_extended ob x
let string_of_extended ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_extended ob x;
  Bi_outbuf.contents ob
let get_extended_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_b0x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_b1x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_b2x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_b3x = ref (None) in
        let field_b4x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_b5x = ref (0.5) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 21902 ->
              field_b0x := (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 21903 ->
              field_b1x := (
                (
                  Atdgen_runtime.Ob_run.read_bool
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | 21904 ->
              field_b2x := (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              );
              bits0 := !bits0 lor 0x4;
            | 21905 ->
              field_b3x := (
                Some (
                  (
                    Atdgen_runtime.Ob_run.read_string
                  ) ib
                )
              );
            | 21906 ->
              field_b4x := (
                (
                  read__6
                ) ib
              );
              bits0 := !bits0 lor 0x8;
            | 21907 ->
              field_b5x := (
                (
                  Atdgen_runtime.Ob_run.read_float64
                ) ib
              );
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0xf then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "b0"; "b1"; "b2"; "b4" |];
        (
          {
            b0x = !field_b0x;
            b1x = !field_b1x;
            b2x = !field_b2x;
            b3x = !field_b3x;
            b4x = !field_b4x;
            b5x = !field_b5x;
          }
         : extended)
)
let read_extended = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_b0x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_b1x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_b2x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_b3x = ref (None) in
    let field_b4x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_b5x = ref (0.5) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 21902 ->
          field_b0x := (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 21903 ->
          field_b1x := (
            (
              Atdgen_runtime.Ob_run.read_bool
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | 21904 ->
          field_b2x := (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          );
          bits0 := !bits0 lor 0x4;
        | 21905 ->
          field_b3x := (
            Some (
              (
                Atdgen_runtime.Ob_run.read_string
              ) ib
            )
          );
        | 21906 ->
          field_b4x := (
            (
              read__6
            ) ib
          );
          bits0 := !bits0 lor 0x8;
        | 21907 ->
          field_b5x := (
            (
              Atdgen_runtime.Ob_run.read_float64
            ) ib
          );
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0xf then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "b0"; "b1"; "b2"; "b4" |];
    (
      {
        b0x = !field_b0x;
        b1x = !field_b1x;
        b2x = !field_b2x;
        b3x = !field_b3x;
        b4x = !field_b4x;
        b5x = !field_b5x;
      }
     : extended)
)
let extended_of_string ?pos s =
  read_extended (Bi_inbuf.from_string ?pos s)
let _27_tag = natural_tag
let write_untagged__27 = (
  fun ob x -> (
    let x = ( Test_lib.Even_natural.unwrap ) x in (
      write_untagged_natural
    ) ob x)
)
let write__27 ob x =
  Bi_io.write_tag ob natural_tag;
  write_untagged__27 ob x
let string_of__27 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__27 ob x;
  Bi_outbuf.contents ob
let get__27_reader = (
  fun tag ib ->
    ( Test_lib.Even_natural.wrap ) ((
      get_natural_reader
    ) tag ib)
)
let read__27 = (
  fun ib ->
    ( Test_lib.Even_natural.wrap ) ((
      read_natural
    ) ib)
)
let _27_of_string ?pos s =
  read__27 (Bi_inbuf.from_string ?pos s)
let even_natural_tag = natural_tag
let write_untagged_even_natural = (
  write_untagged__27
)
let write_even_natural ob x =
  Bi_io.write_tag ob natural_tag;
  write_untagged_even_natural ob x
let string_of_even_natural ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_even_natural ob x;
  Bi_outbuf.contents ob
let get_even_natural_reader = (
  get__27_reader
)
let read_even_natural = (
  read__27
)
let even_natural_of_string ?pos s =
  read_even_natural (Bi_inbuf.from_string ?pos s)
let def_tag = Test_lib.Biniou.def_tag
let write_untagged_def = (
  Test_lib.Biniou.write_untagged_def
)
let write_def ob x =
  Bi_io.write_tag ob Test_lib.Biniou.def_tag;
  write_untagged_def ob x
let string_of_def ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_def ob x;
  Bi_outbuf.contents ob
let get_def_reader = (
  Test_lib.Biniou.get_def_reader
)
let read_def = (
  Test_lib.Biniou.read_def
)
let def_of_string ?pos s =
  read_def (Bi_inbuf.from_string ?pos s)
let char_tag = Bi_io.int8_tag
let write_untagged_char = (
  Bi_io.write_untagged_char
)
let write_char ob x =
  Bi_io.write_tag ob Bi_io.int8_tag;
  write_untagged_char ob x
let string_of_char ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_char ob x;
  Bi_outbuf.contents ob
let get_char_reader = (
  Atdgen_runtime.Ob_run.get_char_reader
)
let read_char = (
  Atdgen_runtime.Ob_run.read_char
)
let char_of_string ?pos s =
  read_char (Bi_inbuf.from_string ?pos s)
let base_tuple_tag = Bi_io.tuple_tag
let write_untagged_base_tuple = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    (
      let x, _ = x in (
        Bi_io.write_svint
      ) ob x
    );
    (
      let _, x = x in (
        Bi_io.write_float64
      ) ob x
    );
)
let write_base_tuple ob x =
  Bi_io.write_tag ob Bi_io.tuple_tag;
  write_untagged_base_tuple ob x
let string_of_base_tuple ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_base_tuple ob x;
  Bi_outbuf.contents ob
let get_base_tuple_reader = (
  fun tag ->
    if tag <> 20 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let len = Bi_vint.read_uvint ib in
        if len < 2 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1 ];
        let x0 =
          (
            Atdgen_runtime.Ob_run.read_int
          ) ib
        in
        let x1 =
          (
            Atdgen_runtime.Ob_run.read_float64
          ) ib
        in
        for i = 2 to len - 1 do Bi_io.skip ib done;
        (x0, x1)
)
let read_base_tuple = (
  fun ib ->
    if Bi_io.read_tag ib <> 20 then Atdgen_runtime.Ob_run.read_error_at ib;
    let len = Bi_vint.read_uvint ib in
    if len < 2 then Atdgen_runtime.Ob_run.missing_tuple_fields len [ 0; 1 ];
    let x0 =
      (
        Atdgen_runtime.Ob_run.read_int
      ) ib
    in
    let x1 =
      (
        Atdgen_runtime.Ob_run.read_float64
      ) ib
    in
    for i = 2 to len - 1 do Bi_io.skip ib done;
    (x0, x1)
)
let base_tuple_of_string ?pos s =
  read_base_tuple (Bi_inbuf.from_string ?pos s)
let base_tag = Bi_io.record_tag
let write_untagged_base : Bi_outbuf.t -> base -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\128' '\000' 'U' '\142';
    (
      Bi_io.write_svint
    ) ob x.b0;
    Bi_outbuf.add_char4 ob '\128' '\000' 'U' '\143';
    (
      Bi_io.write_bool
    ) ob x.b1;
)
let write_base ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_base ob x
let string_of_base ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_base ob x;
  Bi_outbuf.contents ob
let get_base_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_b0 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_b1 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 21902 ->
              field_b0 := (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 21903 ->
              field_b1 := (
                (
                  Atdgen_runtime.Ob_run.read_bool
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "b0"; "b1" |];
        (
          {
            b0 = !field_b0;
            b1 = !field_b1;
          }
         : base)
)
let read_base = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_b0 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_b1 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 21902 ->
          field_b0 := (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 21903 ->
          field_b1 := (
            (
              Atdgen_runtime.Ob_run.read_bool
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "b0"; "b1" |];
    (
      {
        b0 = !field_b0;
        b1 = !field_b1;
      }
     : base)
)
let base_of_string ?pos s =
  read_base (Bi_inbuf.from_string ?pos s)
let _23_tag = Bi_io.array_tag
let write_untagged__23 _a_tag write_untagged__a write__a = (
  Atdgen_runtime.Ob_run.write_untagged_array
    _a_tag
    (
      write_untagged__a
    )
)
let write__23 _a_tag write_untagged__a write__a ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__23 _a_tag write_untagged__a write__a ob x
let string_of__23 _a_tag write_untagged__a write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__23 _a_tag write_untagged__a write__a ob x;
  Bi_outbuf.contents ob
let get__23_reader get__a_reader read__a = (
  Atdgen_runtime.Ob_run.get_array_reader (
    get__a_reader
  )
)
let read__23 get__a_reader read__a = (
  Atdgen_runtime.Ob_run.read_array (
    get__a_reader
  )
)
let _23_of_string get__a_reader read__a ?pos s =
  read__23 get__a_reader read__a (Bi_inbuf.from_string ?pos s)
let array_tag = Bi_io.array_tag
let write_untagged_array _a_tag write_untagged__a write__a = (
  write_untagged__23 _a_tag write_untagged__a write__a
)
let write_array _a_tag write_untagged__a write__a ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged_array _a_tag write_untagged__a write__a ob x
let string_of_array _a_tag write_untagged__a write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_array _a_tag write_untagged__a write__a ob x;
  Bi_outbuf.contents ob
let get_array_reader get__a_reader read__a = (
  get__23_reader get__a_reader read__a
)
let read_array get__a_reader read__a = (
  read__23 get__a_reader read__a
)
let array_of_string get__a_reader read__a ?pos s =
  read_array get__a_reader read__a (Bi_inbuf.from_string ?pos s)
let abs3_tag = Bi_io.array_tag
let write_untagged_abs3 _a_tag write_untagged__a write__a = (
  write_untagged__19 _a_tag write_untagged__a write__a
)
let write_abs3 _a_tag write_untagged__a write__a ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged_abs3 _a_tag write_untagged__a write__a ob x
let string_of_abs3 _a_tag write_untagged__a write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_abs3 _a_tag write_untagged__a write__a ob x;
  Bi_outbuf.contents ob
let get_abs3_reader get__a_reader read__a = (
  get__19_reader get__a_reader read__a
)
let read_abs3 get__a_reader read__a = (
  read__19 get__a_reader read__a
)
let abs3_of_string get__a_reader read__a ?pos s =
  read_abs3 get__a_reader read__a (Bi_inbuf.from_string ?pos s)
let abs2_tag = Bi_io.array_tag
let write_untagged_abs2 _a_tag write_untagged__a write__a = (
  write_untagged__19 _a_tag write_untagged__a write__a
)
let write_abs2 _a_tag write_untagged__a write__a ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged_abs2 _a_tag write_untagged__a write__a ob x
let string_of_abs2 _a_tag write_untagged__a write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_abs2 _a_tag write_untagged__a write__a ob x;
  Bi_outbuf.contents ob
let get_abs2_reader get__a_reader read__a = (
  get__19_reader get__a_reader read__a
)
let read_abs2 get__a_reader read__a = (
  read__19 get__a_reader read__a
)
let abs2_of_string get__a_reader read__a ?pos s =
  read_abs2 get__a_reader read__a (Bi_inbuf.from_string ?pos s)
let abs1_tag = Bi_io.array_tag
let write_untagged_abs1 _a_tag write_untagged__a write__a = (
  write_untagged__19 _a_tag write_untagged__a write__a
)
let write_abs1 _a_tag write_untagged__a write__a ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged_abs1 _a_tag write_untagged__a write__a ob x
let string_of_abs1 _a_tag write_untagged__a write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_abs1 _a_tag write_untagged__a write__a ob x;
  Bi_outbuf.contents ob
let get_abs1_reader get__a_reader read__a = (
  get__19_reader get__a_reader read__a
)
let read_abs1 get__a_reader read__a = (
  read__19 get__a_reader read__a
)
let abs1_of_string get__a_reader read__a ?pos s =
  read_abs1 get__a_reader read__a (Bi_inbuf.from_string ?pos s)
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
