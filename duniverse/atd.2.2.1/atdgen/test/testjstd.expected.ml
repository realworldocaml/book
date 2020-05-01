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
type def = Test_lib.Json.def

type char = Test.char

type base_tuple = Test.base_tuple

type base = Test.base = { b0: int; b1: bool }

type 'a array = 'a Test.array

type 'a abs3 = 'a Test.abs3

type 'a abs2 = 'a Test.abs2

type 'a abs1 = 'a Test.abs1

let write__19 write__a = (
  Atdgen_runtime.Oj_run.write_list (
    write__a
  )
)
let string_of__19 write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__19 write__a ob x;
  Bi_outbuf.contents ob
let read__19 read__a = (
  Atdgen_runtime.Oj_run.read_list (
    read__a
  )
)
let _19_of_string read__a s =
  read__19 read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let rec write_p' write__a : _ -> 'a p' -> _ = (
  fun ob x ->
    match x with
      | A -> Bi_outbuf.add_string ob "\"A\""
      | Bb x ->
        Bi_outbuf.add_string ob "[\"Bb\",";
        (
          write_p' write__a
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | Ccccc x ->
        Bi_outbuf.add_string ob "[\"Ccccc\",";
        (
          write__a
        ) ob x;
        Bi_outbuf.add_char ob ']'
)
and string_of_p' write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_p' write__a ob x;
  Bi_outbuf.contents ob
let rec read_p' read__a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "A" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (A : 'a p')
            | "Bb" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read_p' read__a
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Bb x : 'a p')
            | "Ccccc" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read__a
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Ccccc x : 'a p')
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "A" ->
              (A : 'a p')
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Bb" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_p' read__a
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Bb x : 'a p')
            | "Ccccc" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__a
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Ccccc x : 'a p')
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
and p'_of_string read__a s =
  read_p' read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let rec write_p = (
  fun ob x ->
    match x with
      | `A -> Bi_outbuf.add_string ob "\"A\""
      | `B x ->
        Bi_outbuf.add_string ob "[\"B\",";
        (
          write_r
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | `C -> Bi_outbuf.add_string ob "\"C\""
)
and string_of_p ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_p ob x;
  Bi_outbuf.contents ob
and write_r : _ -> r -> _ = (
  fun ob (x : r) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"a\":";
    (
      Yojson.Safe.write_int
    )
      ob x.a;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"b\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.b;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"c\":";
    (
      write_p
    )
      ob x.c;
    Bi_outbuf.add_char ob '}';
)
and string_of_r ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_r ob x;
  Bi_outbuf.contents ob
let rec read_p = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "A" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `A
            | "B" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read_r
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `B x
            | "C" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `C
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "A" ->
              `A
            | "C" ->
              `C
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "B" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_r
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `B x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
and p_of_string s =
  read_p (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
and read_r = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_a = ref (None) in
    let field_b = ref (None) in
    let field_c = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 1 then (
            match String.unsafe_get s pos with
              | 'a' -> (
                  0
                )
              | 'b' -> (
                  1
                )
              | 'c' -> (
                  2
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_a := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_b := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 2 ->
            field_c := (
              Some (
                (
                  read_p
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 1 then (
              match String.unsafe_get s pos with
                | 'a' -> (
                    0
                  )
                | 'b' -> (
                    1
                  )
                | 'c' -> (
                    2
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_a := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_b := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 2 ->
              field_c := (
                Some (
                  (
                    read_p
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            a = (match !field_a with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "a");
            b = (match !field_b with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b");
            c = (match !field_c with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "c");
          }
         : r)
      )
)
and r_of_string s =
  read_r (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let rec write__20 write__a write__b ob x = (
  Atdgen_runtime.Oj_run.write_std_option (
    write_poly write__a write__b
  )
) ob x
and string_of__20 write__a write__b ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__20 write__a write__b ob x;
  Bi_outbuf.contents ob
and write_poly write__x write__y : _ -> ('x, 'y) poly -> _ = (
  fun ob (x : ('x, 'y) poly) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"fst\":";
    (
      write__19 write__x
    )
      ob x.fst;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"snd\":";
    (
      write__20 write__x write__y
    )
      ob x.snd;
    Bi_outbuf.add_char ob '}';
)
and string_of_poly write__x write__y ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_poly write__x write__y ob x;
  Bi_outbuf.contents ob
let rec read__20 read__a read__b = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read_poly read__a read__b
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_poly read__a read__b
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
and _20_of_string read__a read__b s =
  read__20 read__a read__b (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
and read_poly read__x read__y = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_fst = ref (None) in
    let field_snd = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 3 then (
            match String.unsafe_get s pos with
              | 'f' -> (
                  if String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 's' -> (
                  if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_fst := (
              Some (
                (
                  read__19 read__x
                ) p lb
              )
            );
          | 1 ->
            field_snd := (
              Some (
                (
                  read__20 read__x read__y
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 3 then (
              match String.unsafe_get s pos with
                | 'f' -> (
                    if String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 't' then (
                      0
                    )
                    else (
                      -1
                    )
                  )
                | 's' -> (
                    if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' then (
                      1
                    )
                    else (
                      -1
                    )
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_fst := (
                Some (
                  (
                    read__19 read__x
                  ) p lb
                )
              );
            | 1 ->
              field_snd := (
                Some (
                  (
                    read__20 read__x read__y
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            fst = (match !field_fst with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "fst");
            snd = (match !field_snd with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "snd");
          }
         : ('x, 'y) poly)
      )
)
and poly_of_string read__x read__y s =
  read_poly read__x read__y (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let rec write__2 ob x = (
  Atdgen_runtime.Oj_run.write_list (
    write_test_variant
  )
) ob x
and string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
and write_test_variant = (
  fun ob x ->
    match x with
      | `Case1 -> Bi_outbuf.add_string ob "\"Case1\""
      | `Case2 x ->
        Bi_outbuf.add_string ob "[\"Case2\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | `Case3 x ->
        Bi_outbuf.add_string ob "[\"Case3\",";
        (
          Yojson.Safe.write_string
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | `Case4 x ->
        Bi_outbuf.add_string ob "[\"Case4\",";
        (
          write__2
        ) ob x;
        Bi_outbuf.add_char ob ']'
)
and string_of_test_variant ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_test_variant ob x;
  Bi_outbuf.contents ob
let rec read__2 p lb = (
  Atdgen_runtime.Oj_run.read_list (
    read_test_variant
  )
) p lb
and _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
and read_test_variant = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "Case1" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Case1
            | "Case2" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Case2 x
            | "Case3" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Case3 x
            | "Case4" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read__2
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Case4 x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "Case1" ->
              `Case1
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Case2" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Case2 x
            | "Case3" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Case3 x
            | "Case4" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__2
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Case4 x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
and test_variant_of_string s =
  read_test_variant (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let rec write__1 : _ -> _ p' -> _ = (
  fun ob x ->
    match x with
      | A -> Bi_outbuf.add_string ob "\"A\""
      | Bb x ->
        Bi_outbuf.add_string ob "[\"Bb\",";
        (
          write__1
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | Ccccc x ->
        Bi_outbuf.add_string ob "[\"Ccccc\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Bi_outbuf.add_char ob ']'
)
and string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let rec read__1 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "A" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (A : _ p')
            | "Bb" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read__1
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Bb x : _ p')
            | "Ccccc" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Ccccc x : _ p')
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "A" ->
              (A : _ p')
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Bb" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__1
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Bb x : _ p')
            | "Ccccc" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Ccccc x : _ p')
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
and _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_validated_string_check = (
  Yojson.Safe.write_string
)
let string_of_validated_string_check ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_validated_string_check ob x;
  Bi_outbuf.contents ob
let read_validated_string_check = (
  Atdgen_runtime.Oj_run.read_string
)
let validated_string_check_of_string s =
  read_validated_string_check (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__31 = (
  Atdgen_runtime.Oj_run.write_list (
    Yojson.Safe.write_string
  )
)
let string_of__31 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__31 ob x;
  Bi_outbuf.contents ob
let read__31 = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_string
  )
)
let _31_of_string s =
  read__31 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_validate_me = (
  write__31
)
let string_of_validate_me ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_validate_me ob x;
  Bi_outbuf.contents ob
let read_validate_me = (
  read__31
)
let validate_me_of_string s =
  read_validate_me (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_val1 : _ -> val1 -> _ = (
  fun ob (x : val1) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"val1_x\":";
    (
      Yojson.Safe.write_int
    )
      ob x.val1_x;
    Bi_outbuf.add_char ob '}';
)
let string_of_val1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_val1 ob x;
  Bi_outbuf.contents ob
let read_val1 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_val1_x = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 6 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = '1' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'x' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_val1_x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 6 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = '1' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'x' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_val1_x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            val1_x = (match !field_val1_x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "val1_x");
          }
         : val1)
      )
)
let val1_of_string s =
  read_val1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__16 = (
  Atdgen_runtime.Oj_run.write_std_option (
    write_val1
  )
)
let string_of__16 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__16 ob x;
  Bi_outbuf.contents ob
let read__16 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  read_val1
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_val1
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _16_of_string s =
  read__16 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_val2 : _ -> val2 -> _ = (
  fun ob (x : val2) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"val2_x\":";
    (
      write_val1
    )
      ob x.val2_x;
    (match x.val2_y with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"val2_y\":";
      (
        write_val1
      )
        ob x;
    );
    Bi_outbuf.add_char ob '}';
)
let string_of_val2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_val2 ob x;
  Bi_outbuf.contents ob
let read_val2 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_val2_x = ref (None) in
    let field_val2_y = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 6 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = '2' && String.unsafe_get s (pos+4) = '_' then (
            match String.unsafe_get s (pos+5) with
              | 'x' -> (
                  0
                )
              | 'y' -> (
                  1
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_val2_x := (
              Some (
                (
                  read_val1
                ) p lb
              )
            );
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_val2_y := (
                Some (
                  (
                    read_val1
                  ) p lb
                )
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 6 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = '2' && String.unsafe_get s (pos+4) = '_' then (
              match String.unsafe_get s (pos+5) with
                | 'x' -> (
                    0
                  )
                | 'y' -> (
                    1
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_val2_x := (
                Some (
                  (
                    read_val1
                  ) p lb
                )
              );
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_val2_y := (
                  Some (
                    (
                      read_val1
                    ) p lb
                  )
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            val2_x = (match !field_val2_x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "val2_x");
            val2_y = !field_val2_y;
          }
         : val2)
      )
)
let val2_of_string s =
  read_val2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__29 = (
  Atdgen_runtime.Oj_run.write_list (
    Atdgen_runtime.Oj_run.write_float_as_int
  )
)
let string_of__29 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__29 ob x;
  Bi_outbuf.contents ob
let read__29 = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_number
  )
)
let _29_of_string s =
  read__29 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_unixtime_list = (
  write__29
)
let string_of_unixtime_list ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_unixtime_list ob x;
  Bi_outbuf.contents ob
let read_unixtime_list = (
  read__29
)
let unixtime_list_of_string s =
  read_unixtime_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  Atdgen_runtime.Oj_run.write_nullable (
    Yojson.Safe.write_int
  )
)
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let read__3 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    (if Yojson.Safe.read_null_if_possible p lb then None
    else Some ((
      Atdgen_runtime.Oj_run.read_int
    ) p lb) : _ option)
)
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_date = (
  fun ob x ->
    Bi_outbuf.add_char ob '[';
    (let x, _, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x, _ = x in
    (
      write__3
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, _, x = x in
    (
      write__3
    ) ob x
    );
    Bi_outbuf.add_char ob ']';
)
let string_of_date ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_date ob x;
  Bi_outbuf.contents ob
let read_date = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_int
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            read__3
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x2 =
        let x =
          (
            read__3
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1, x2)
    with Yojson.End_of_tuple ->
      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1; 2 ]);
)
let date_of_string s =
  read_date (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__9 = (
  Atdgen_runtime.Oj_run.write_array (
    Yojson.Safe.write_string
  )
)
let string_of__9 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__9 ob x;
  Bi_outbuf.contents ob
let read__9 = (
  Atdgen_runtime.Oj_run.read_array (
    Atdgen_runtime.Oj_run.read_string
  )
)
let _9_of_string s =
  read__9 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__8 = (
  Atdgen_runtime.Oj_run.write_std_option (
    Yojson.Safe.write_bool
  )
)
let string_of__8 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__8 ob x;
  Bi_outbuf.contents ob
let read__8 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _8_of_string s =
  read__8 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__7 = (
  Atdgen_runtime.Oj_run.write_array (
    Yojson.Safe.write_std_float
  )
)
let string_of__7 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__7 ob x;
  Bi_outbuf.contents ob
let read__7 = (
  Atdgen_runtime.Oj_run.read_array (
    Atdgen_runtime.Oj_run.read_number
  )
)
let _7_of_string s =
  read__7 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__6 = (
  Atdgen_runtime.Oj_run.write_std_option (
    Yojson.Safe.write_string
  )
)
let string_of__6 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__6 ob x;
  Bi_outbuf.contents ob
let read__6 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _6_of_string s =
  read__6 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__5 = (
  Atdgen_runtime.Oj_run.write_std_option (
    Yojson.Safe.write_std_float
  )
)
let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob
let read__5 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _5_of_string s =
  read__5 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__4 = (
  Atdgen_runtime.Oj_run.write_std_option (
    Yojson.Safe.write_int
  )
)
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let read__4 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _4_of_string s =
  read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__11 = (
  Atdgen_runtime.Oj_run.write_list (
    write__6
  )
)
let string_of__11 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__11 ob x;
  Bi_outbuf.contents ob
let read__11 = (
  Atdgen_runtime.Oj_run.read_list (
    read__6
  )
)
let _11_of_string s =
  read__11 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__10 = (
  Atdgen_runtime.Oj_run.write_list (
    Yojson.Safe.write_null
  )
)
let string_of__10 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__10 ob x;
  Bi_outbuf.contents ob
let read__10 = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_null
  )
)
let _10_of_string s =
  read__10 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_mixed_record : _ -> mixed_record -> _ = (
  fun ob (x : mixed_record) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    (match x.field0 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"field0\":";
      (
        Yojson.Safe.write_int
      )
        ob x;
    );
    (match x.field1 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"field1\":";
      (
        Yojson.Safe.write_std_float
      )
        ob x;
    );
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"field2\":";
    (
      write__6
    )
      ob x.field2;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"field3\":";
    (
      Atdgen_runtime.Oj_run.write_int64
    )
      ob x.field3;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"field4\":";
    (
      write__7
    )
      ob x.field4;
    (match x.field5 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"field5\":";
      (
        Yojson.Safe.write_bool
      )
        ob x;
    );
    (match x.field6 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"field6\":";
      (
        Yojson.Safe.write_string
      )
        ob x;
    );
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"field7\":";
    (
      write_test_variant
    )
      ob x.field7;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"field8\":";
    (
      write__9
    )
      ob x.field8;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"field9\":";
    (
      fun ob x ->
        Bi_outbuf.add_char ob '[';
        (let x, _, _, _, _, _ = x in
        (
          Yojson.Safe.write_int
        ) ob x
        );
        Bi_outbuf.add_char ob ',';
        (let _, x, _, _, _, _ = x in
        (
          Yojson.Safe.write_int
        ) ob x
        );
        Bi_outbuf.add_char ob ',';
        (let _, _, x, _, _, _ = x in
        (
          Atdgen_runtime.Oj_run.write_int8
        ) ob x
        );
        Bi_outbuf.add_char ob ',';
        (let _, _, _, x, _, _ = x in
        (
          Yojson.Safe.write_int
        ) ob x
        );
        Bi_outbuf.add_char ob ',';
        (let _, _, _, _, x, _ = x in
        (
          Atdgen_runtime.Oj_run.write_int32
        ) ob x
        );
        Bi_outbuf.add_char ob ',';
        (let _, _, _, _, _, x = x in
        (
          Atdgen_runtime.Oj_run.write_int64
        ) ob x
        );
        Bi_outbuf.add_char ob ']';
    )
      ob x.field9;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"field10\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.field10;
    if x.field11 <> false then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"field11\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.field11;
    );
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"field12\":";
    (
      write__10
    )
      ob x.field12;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"field13\":";
    (
      write__11
    )
      ob x.field13;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"field14\":";
    (
      write_date
    )
      ob x.field14;
    Bi_outbuf.add_char ob '}';
)
let string_of_mixed_record ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_mixed_record ob x;
  Bi_outbuf.contents ob
let read_mixed_record = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_field0 = ref (None) in
    let field_field1 = ref (None) in
    let field_field2 = ref (None) in
    let field_field3 = ref (None) in
    let field_field4 = ref (None) in
    let field_field5 = ref (None) in
    let field_field6 = ref (None) in
    let field_field7 = ref (None) in
    let field_field8 = ref (None) in
    let field_field9 = ref (None) in
    let field_field10 = ref (None) in
    let field_field11 = ref (false) in
    let field_field12 = ref (None) in
    let field_field13 = ref (None) in
    let field_field14 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 6 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' then (
                  match String.unsafe_get s (pos+5) with
                    | '0' -> (
                        0
                      )
                    | '1' -> (
                        1
                      )
                    | '2' -> (
                        2
                      )
                    | '3' -> (
                        3
                      )
                    | '4' -> (
                        4
                      )
                    | '5' -> (
                        5
                      )
                    | '6' -> (
                        6
                      )
                    | '7' -> (
                        7
                      )
                    | '8' -> (
                        8
                      )
                    | '9' -> (
                        9
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = '1' then (
                  match String.unsafe_get s (pos+6) with
                    | '0' -> (
                        10
                      )
                    | '1' -> (
                        11
                      )
                    | '2' -> (
                        12
                      )
                    | '3' -> (
                        13
                      )
                    | '4' -> (
                        14
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_field0 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            )
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_field1 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            )
          | 2 ->
            field_field2 := (
              Some (
                (
                  read__6
                ) p lb
              )
            );
          | 3 ->
            field_field3 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int64
                ) p lb
              )
            );
          | 4 ->
            field_field4 := (
              Some (
                (
                  read__7
                ) p lb
              )
            );
          | 5 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_field5 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            )
          | 6 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_field6 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            )
          | 7 ->
            field_field7 := (
              Some (
                (
                  read_test_variant
                ) p lb
              )
            );
          | 8 ->
            field_field8 := (
              Some (
                (
                  read__9
                ) p lb
              )
            );
          | 9 ->
            field_field9 := (
              Some (
                (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x2 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int8
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x3 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x4 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int32
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x5 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int64
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1, x2, x3, x4, x5)
                    with Yojson.End_of_tuple ->
                      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1; 2; 3; 4; 5 ]);
                ) p lb
              )
            );
          | 10 ->
            field_field10 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 11 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_field11 := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 12 ->
            field_field12 := (
              Some (
                (
                  read__10
                ) p lb
              )
            );
          | 13 ->
            field_field13 := (
              Some (
                (
                  read__11
                ) p lb
              )
            );
          | 14 ->
            field_field14 := (
              Some (
                (
                  read_date
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 6 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' then (
                    match String.unsafe_get s (pos+5) with
                      | '0' -> (
                          0
                        )
                      | '1' -> (
                          1
                        )
                      | '2' -> (
                          2
                        )
                      | '3' -> (
                          3
                        )
                      | '4' -> (
                          4
                        )
                      | '5' -> (
                          5
                        )
                      | '6' -> (
                          6
                        )
                      | '7' -> (
                          7
                        )
                      | '8' -> (
                          8
                        )
                      | '9' -> (
                          9
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = '1' then (
                    match String.unsafe_get s (pos+6) with
                      | '0' -> (
                          10
                        )
                      | '1' -> (
                          11
                        )
                      | '2' -> (
                          12
                        )
                      | '3' -> (
                          13
                        )
                      | '4' -> (
                          14
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_field0 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_int
                    ) p lb
                  )
                );
              )
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_field1 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_number
                    ) p lb
                  )
                );
              )
            | 2 ->
              field_field2 := (
                Some (
                  (
                    read__6
                  ) p lb
                )
              );
            | 3 ->
              field_field3 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int64
                  ) p lb
                )
              );
            | 4 ->
              field_field4 := (
                Some (
                  (
                    read__7
                  ) p lb
                )
              );
            | 5 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_field5 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_bool
                    ) p lb
                  )
                );
              )
            | 6 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_field6 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_string
                    ) p lb
                  )
                );
              )
            | 7 ->
              field_field7 := (
                Some (
                  (
                    read_test_variant
                  ) p lb
                )
              );
            | 8 ->
              field_field8 := (
                Some (
                  (
                    read__9
                  ) p lb
                )
              );
            | 9 ->
              field_field9 := (
                Some (
                  (
                    fun p lb ->
                      Yojson.Safe.read_space p lb;
                      let std_tuple = Yojson.Safe.start_any_tuple p lb in
                      let len = ref 0 in
                      let end_of_tuple = ref false in
                      (try
                        let x0 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x1 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x2 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int8
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x3 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x4 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int32
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x5 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int64
                            ) p lb
                          in
                          incr len;
                          (try
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          with Yojson.End_of_tuple -> end_of_tuple := true);
                          x
                        in
                        if not !end_of_tuple then (
                          try
                            while true do
                              Yojson.Safe.skip_json p lb;
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                            done
                          with Yojson.End_of_tuple -> ()
                        );
                        (x0, x1, x2, x3, x4, x5)
                      with Yojson.End_of_tuple ->
                        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1; 2; 3; 4; 5 ]);
                  ) p lb
                )
              );
            | 10 ->
              field_field10 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 11 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_field11 := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 12 ->
              field_field12 := (
                Some (
                  (
                    read__10
                  ) p lb
                )
              );
            | 13 ->
              field_field13 := (
                Some (
                  (
                    read__11
                  ) p lb
                )
              );
            | 14 ->
              field_field14 := (
                Some (
                  (
                    read_date
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            field0 = !field_field0;
            field1 = !field_field1;
            field2 = (match !field_field2 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field2");
            field3 = (match !field_field3 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field3");
            field4 = (match !field_field4 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field4");
            field5 = !field_field5;
            field6 = !field_field6;
            field7 = (match !field_field7 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field7");
            field8 = (match !field_field8 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field8");
            field9 = (match !field_field9 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field9");
            field10 = (match !field_field10 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field10");
            field11 = !field_field11;
            field12 = (match !field_field12 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field12");
            field13 = (match !field_field13 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field13");
            field14 = (match !field_field14 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "field14");
          }
         : mixed_record)
      )
)
let mixed_record_of_string s =
  read_mixed_record (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__13 = (
  Atdgen_runtime.Oj_run.write_array (
    write_mixed_record
  )
)
let string_of__13 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__13 ob x;
  Bi_outbuf.contents ob
let read__13 = (
  Atdgen_runtime.Oj_run.read_array (
    read_mixed_record
  )
)
let _13_of_string s =
  read__13 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__12 = (
  Atdgen_runtime.Oj_run.write_array (
    write_mixed_record
  )
)
let string_of__12 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__12 ob x;
  Bi_outbuf.contents ob
let read__12 = (
  Atdgen_runtime.Oj_run.read_array (
    read_mixed_record
  )
)
let _12_of_string s =
  read__12 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__14 = (
  Atdgen_runtime.Oj_run.write_list (
    fun ob x ->
      Bi_outbuf.add_char ob '[';
      (let x, _ = x in
      (
        write__12
      ) ob x
      );
      Bi_outbuf.add_char ob ',';
      (let _, x = x in
      (
        write__13
      ) ob x
      );
      Bi_outbuf.add_char ob ']';
  )
)
let string_of__14 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__14 ob x;
  Bi_outbuf.contents ob
let read__14 = (
  Atdgen_runtime.Oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              read__12
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              read__13
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _14_of_string s =
  read__14 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_mixed = (
  write__14
)
let string_of_mixed ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_mixed ob x;
  Bi_outbuf.contents ob
let read_mixed = (
  read__14
)
let mixed_of_string s =
  read_mixed (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__15 = (
  Atdgen_runtime.Oj_run.write_list (
    write_mixed_record
  )
)
let string_of__15 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__15 ob x;
  Bi_outbuf.contents ob
let read__15 = (
  Atdgen_runtime.Oj_run.read_list (
    read_mixed_record
  )
)
let _15_of_string s =
  read__15 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_test : _ -> test -> _ = (
  fun ob (x : test) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    (match x.x0 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"x0\":";
      (
        Yojson.Safe.write_int
      )
        ob x;
    );
    (match x.x1 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"x1\":";
      (
        Yojson.Safe.write_std_float
      )
        ob x;
    );
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"x2\":";
    (
      write_mixed
    )
      ob x.x2;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"x3\":";
    (
      write__15
    )
      ob x.x3;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"x4\":";
    (
      Atdgen_runtime.Oj_run.write_int64
    )
      ob x.x4;
    Bi_outbuf.add_char ob '}';
)
let string_of_test ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_test ob x;
  Bi_outbuf.contents ob
let read_test = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_x0 = ref (None) in
    let field_x1 = ref (None) in
    let field_x2 = ref (None) in
    let field_x3 = ref (None) in
    let field_x4 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 2 && String.unsafe_get s pos = 'x' then (
            match String.unsafe_get s (pos+1) with
              | '0' -> (
                  0
                )
              | '1' -> (
                  1
                )
              | '2' -> (
                  2
                )
              | '3' -> (
                  3
                )
              | '4' -> (
                  4
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_x0 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            )
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_x1 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            )
          | 2 ->
            field_x2 := (
              Some (
                (
                  read_mixed
                ) p lb
              )
            );
          | 3 ->
            field_x3 := (
              Some (
                (
                  read__15
                ) p lb
              )
            );
          | 4 ->
            field_x4 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int64
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 2 && String.unsafe_get s pos = 'x' then (
              match String.unsafe_get s (pos+1) with
                | '0' -> (
                    0
                  )
                | '1' -> (
                    1
                  )
                | '2' -> (
                    2
                  )
                | '3' -> (
                    3
                  )
                | '4' -> (
                    4
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_x0 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_int
                    ) p lb
                  )
                );
              )
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_x1 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_number
                    ) p lb
                  )
                );
              )
            | 2 ->
              field_x2 := (
                Some (
                  (
                    read_mixed
                  ) p lb
                )
              );
            | 3 ->
              field_x3 := (
                Some (
                  (
                    read__15
                  ) p lb
                )
              );
            | 4 ->
              field_x4 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int64
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            x0 = !field_x0;
            x1 = !field_x1;
            x2 = (match !field_x2 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x2");
            x3 = (match !field_x3 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x3");
            x4 = (match !field_x4 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x4");
          }
         : test)
      )
)
let test_of_string s =
  read_test (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tup = (
  fun ob x ->
    Bi_outbuf.add_char ob '[';
    (let x, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x = x in
    (
      write_test
    ) ob x
    );
    Bi_outbuf.add_char ob ']';
)
let string_of_tup ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tup ob x;
  Bi_outbuf.contents ob
let read_tup = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_int
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            read_test
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1)
    with Yojson.End_of_tuple ->
      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
)
let tup_of_string s =
  read_tup (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_star_rating = (
  Yojson.Safe.write_int
)
let string_of_star_rating ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_star_rating ob x;
  Bi_outbuf.contents ob
let read_star_rating = (
  Atdgen_runtime.Oj_run.read_int
)
let star_rating_of_string s =
  read_star_rating (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__30 : _ -> _ generic -> _ = (
  fun ob (x : _ generic) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"x294623\":";
    (
      Yojson.Safe.write_int
    )
      ob x.x294623;
    Bi_outbuf.add_char ob '}';
)
let string_of__30 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__30 ob x;
  Bi_outbuf.contents ob
let read__30 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_x294623 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 7 && String.unsafe_get s pos = 'x' && String.unsafe_get s (pos+1) = '2' && String.unsafe_get s (pos+2) = '9' && String.unsafe_get s (pos+3) = '4' && String.unsafe_get s (pos+4) = '6' && String.unsafe_get s (pos+5) = '2' && String.unsafe_get s (pos+6) = '3' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_x294623 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 7 && String.unsafe_get s pos = 'x' && String.unsafe_get s (pos+1) = '2' && String.unsafe_get s (pos+2) = '9' && String.unsafe_get s (pos+3) = '4' && String.unsafe_get s (pos+4) = '6' && String.unsafe_get s (pos+5) = '2' && String.unsafe_get s (pos+6) = '3' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_x294623 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            x294623 = (match !field_x294623 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x294623");
          }
         : _ generic)
      )
)
let _30_of_string s =
  read__30 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_specialized = (
  write__30
)
let string_of_specialized ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_specialized ob x;
  Bi_outbuf.contents ob
let read_specialized = (
  read__30
)
let specialized_of_string s =
  read_specialized (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_some_record : _ -> some_record -> _ = (
  fun ob (x : some_record) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"some_field\":";
    (
      Yojson.Safe.write_int
    )
      ob x.some_field;
    Bi_outbuf.add_char ob '}';
)
let string_of_some_record ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_some_record ob x;
  Bi_outbuf.contents ob
let read_some_record = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_some_field = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 10 && String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'd' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_some_field := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 10 && String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'd' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_some_field := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            some_field = (match !field_some_field with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "some_field");
          }
         : some_record)
      )
)
let some_record_of_string s =
  read_some_record (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_precision : _ -> precision -> _ = (
  fun ob (x : precision) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"sqrt2_5\":";
    (
      Yojson.Safe.write_std_float_prec 5
    )
      ob x.sqrt2_5;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"small_2\":";
    (
      Yojson.Safe.write_std_float_prec 2
    )
      ob x.small_2;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"large_2\":";
    (
      Yojson.Safe.write_std_float_prec 2
    )
      ob x.large_2;
    Bi_outbuf.add_char ob '}';
)
let string_of_precision ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_precision ob x;
  Bi_outbuf.contents ob
let read_precision = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_sqrt2_5 = ref (None) in
    let field_small_2 = ref (None) in
    let field_large_2 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 7 then (
            match String.unsafe_get s pos with
              | 'l' -> (
                  if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = '2' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 's' -> (
                  match String.unsafe_get s (pos+1) with
                    | 'm' -> (
                        if String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = '2' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 'q' -> (
                        if String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '2' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = '5' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_sqrt2_5 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 1 ->
            field_small_2 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 2 ->
            field_large_2 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 7 then (
              match String.unsafe_get s pos with
                | 'l' -> (
                    if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = '2' then (
                      2
                    )
                    else (
                      -1
                    )
                  )
                | 's' -> (
                    match String.unsafe_get s (pos+1) with
                      | 'm' -> (
                          if String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = '2' then (
                            1
                          )
                          else (
                            -1
                          )
                        )
                      | 'q' -> (
                          if String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '2' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = '5' then (
                            0
                          )
                          else (
                            -1
                          )
                        )
                      | _ -> (
                          -1
                        )
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_sqrt2_5 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 1 ->
              field_small_2 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 2 ->
              field_large_2 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            sqrt2_5 = (match !field_sqrt2_5 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "sqrt2_5");
            small_2 = (match !field_small_2 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "small_2");
            large_2 = (match !field_large_2 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "large_2");
          }
         : precision)
      )
)
let precision_of_string s =
  read_precision (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_p'' = (
  write__1
)
let string_of_p'' ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_p'' ob x;
  Bi_outbuf.contents ob
let read_p'' = (
  read__1
)
let p''_of_string s =
  read_p'' (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__18 = (
  Atdgen_runtime.Oj_run.write_std_option (
    Yojson.Safe.write_int
  )
)
let string_of__18 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__18 ob x;
  Bi_outbuf.contents ob
let read__18 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _18_of_string s =
  read__18 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_option_validation = (
  write__18
)
let string_of_option_validation ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_option_validation ob x;
  Bi_outbuf.contents ob
let read_option_validation = (
  read__18
)
let option_validation_of_string s =
  read_option_validation (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__28 = (
  write_some_record
)
let string_of__28 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__28 ob x;
  Bi_outbuf.contents ob
let read__28 = (
  read_some_record
)
let _28_of_string s =
  read__28 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_no_real_wrap = (
  write__28
)
let string_of_no_real_wrap ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_no_real_wrap ob x;
  Bi_outbuf.contents ob
let read_no_real_wrap = (
  read__28
)
let no_real_wrap_of_string s =
  read_no_real_wrap (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__26 = (
  fun ob x -> (
    let x = ( Test_lib.Natural.unwrap ) x in (
      Yojson.Safe.write_int
    ) ob x)
)
let string_of__26 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__26 ob x;
  Bi_outbuf.contents ob
let read__26 = (
  fun p lb ->
    let x = (
      Atdgen_runtime.Oj_run.read_int
    ) p lb in
    ( Test_lib.Natural.wrap ) x
)
let _26_of_string s =
  read__26 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_natural = (
  write__26
)
let string_of_natural ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_natural ob x;
  Bi_outbuf.contents ob
let read_natural = (
  read__26
)
let natural_of_string s =
  read_natural (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__24 = (
  fun ob x -> (
    let x = ( function `Id s -> s ) x in (
      Yojson.Safe.write_string
    ) ob x)
)
let string_of__24 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__24 ob x;
  Bi_outbuf.contents ob
let read__24 = (
  fun p lb ->
    let x = (
      Atdgen_runtime.Oj_run.read_string
    ) p lb in
    ( fun s -> `Id s ) x
)
let _24_of_string s =
  read__24 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_id = (
  write__24
)
let string_of_id ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_id ob x;
  Bi_outbuf.contents ob
let read_id = (
  read__24
)
let id_of_string s =
  read_id (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__25 = (
  Atdgen_runtime.Oj_run.write_assoc_list (
    write_id
  ) (
    Yojson.Safe.write_int
  )
)
let string_of__25 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__25 ob x;
  Bi_outbuf.contents ob
let read__25 = (
  Atdgen_runtime.Oj_run.read_assoc_list (
    read_id
  ) (
    Atdgen_runtime.Oj_run.read_int
  )
)
let _25_of_string s =
  read__25 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_json_map = (
  write__25
)
let string_of_json_map ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_json_map ob x;
  Bi_outbuf.contents ob
let read_json_map = (
  read__25
)
let json_map_of_string s =
  read_json_map (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_intopt = (
  write__4
)
let string_of_intopt ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_intopt ob x;
  Bi_outbuf.contents ob
let read_intopt = (
  read__4
)
let intopt_of_string s =
  read_intopt (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__21 = (
  Atdgen_runtime.Oj_run.write_assoc_list (
    Yojson.Safe.write_string
  ) (
    Yojson.Safe.write_int
  )
)
let string_of__21 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__21 ob x;
  Bi_outbuf.contents ob
let read__21 = (
  Atdgen_runtime.Oj_run.read_assoc_list (
    Atdgen_runtime.Oj_run.read_string
  ) (
    Atdgen_runtime.Oj_run.read_int
  )
)
let _21_of_string s =
  read__21 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_int_assoc_list = (
  write__21
)
let string_of_int_assoc_list ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_int_assoc_list ob x;
  Bi_outbuf.contents ob
let read_int_assoc_list = (
  read__21
)
let int_assoc_list_of_string s =
  read_int_assoc_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__22 = (
  Atdgen_runtime.Oj_run.write_assoc_array (
    Yojson.Safe.write_string
  ) (
    Yojson.Safe.write_int
  )
)
let string_of__22 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__22 ob x;
  Bi_outbuf.contents ob
let read__22 = (
  Atdgen_runtime.Oj_run.read_assoc_array (
    Atdgen_runtime.Oj_run.read_string
  ) (
    Atdgen_runtime.Oj_run.read_int
  )
)
let _22_of_string s =
  read__22 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_int_assoc_array = (
  write__22
)
let string_of_int_assoc_array ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_int_assoc_array ob x;
  Bi_outbuf.contents ob
let read_int_assoc_array = (
  read__22
)
let int_assoc_array_of_string s =
  read_int_assoc_array (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_int8 = (
  Yojson.Safe.write_int
)
let string_of_int8 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_int8 ob x;
  Bi_outbuf.contents ob
let read_int8 = (
  Atdgen_runtime.Oj_run.read_int
)
let int8_of_string s =
  read_int8 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_int64 = (
  Atdgen_runtime.Oj_run.write_int64
)
let string_of_int64 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_int64 ob x;
  Bi_outbuf.contents ob
let read_int64 = (
  Atdgen_runtime.Oj_run.read_int64
)
let int64_of_string s =
  read_int64 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_int32 = (
  Atdgen_runtime.Oj_run.write_int32
)
let string_of_int32 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_int32 ob x;
  Bi_outbuf.contents ob
let read_int32 = (
  Atdgen_runtime.Oj_run.read_int32
)
let int32_of_string s =
  read_int32 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_hello = (
  fun ob x ->
    match x with
      | `Hello x ->
        Bi_outbuf.add_string ob "[\"Hello\",";
        (
          Yojson.Safe.write_string
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | `World -> Bi_outbuf.add_string ob "\"World\""
)
let string_of_hello ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_hello ob x;
  Bi_outbuf.contents ob
let read_hello = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "Hello" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Hello x
            | "World" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `World
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "World" ->
              `World
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Hello" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Hello x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let hello_of_string s =
  read_hello (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_generic write__a : _ -> 'a generic -> _ = (
  fun ob (x : 'a generic) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"x294623\":";
    (
      Yojson.Safe.write_int
    )
      ob x.x294623;
    Bi_outbuf.add_char ob '}';
)
let string_of_generic write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_generic write__a ob x;
  Bi_outbuf.contents ob
let read_generic read__a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_x294623 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 7 && String.unsafe_get s pos = 'x' && String.unsafe_get s (pos+1) = '2' && String.unsafe_get s (pos+2) = '9' && String.unsafe_get s (pos+3) = '4' && String.unsafe_get s (pos+4) = '6' && String.unsafe_get s (pos+5) = '2' && String.unsafe_get s (pos+6) = '3' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_x294623 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 7 && String.unsafe_get s pos = 'x' && String.unsafe_get s (pos+1) = '2' && String.unsafe_get s (pos+2) = '9' && String.unsafe_get s (pos+3) = '4' && String.unsafe_get s (pos+4) = '6' && String.unsafe_get s (pos+5) = '2' && String.unsafe_get s (pos+6) = '3' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_x294623 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            x294623 = (match !field_x294623 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "x294623");
          }
         : 'a generic)
      )
)
let generic_of_string read__a s =
  read_generic read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_floats : _ -> floats -> _ = (
  fun ob (x : floats) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"f32\":";
    (
      Yojson.Safe.write_std_float
    )
      ob x.f32;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"f64\":";
    (
      Yojson.Safe.write_std_float
    )
      ob x.f64;
    Bi_outbuf.add_char ob '}';
)
let string_of_floats ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_floats ob x;
  Bi_outbuf.contents ob
let read_floats = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_f32 = ref (None) in
    let field_f64 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 3 && String.unsafe_get s pos = 'f' then (
            match String.unsafe_get s (pos+1) with
              | '3' -> (
                  if String.unsafe_get s (pos+2) = '2' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | '6' -> (
                  if String.unsafe_get s (pos+2) = '4' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_f32 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 1 ->
            field_f64 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 3 && String.unsafe_get s pos = 'f' then (
              match String.unsafe_get s (pos+1) with
                | '3' -> (
                    if String.unsafe_get s (pos+2) = '2' then (
                      0
                    )
                    else (
                      -1
                    )
                  )
                | '6' -> (
                    if String.unsafe_get s (pos+2) = '4' then (
                      1
                    )
                    else (
                      -1
                    )
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_f32 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 1 ->
              field_f64 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            f32 = (match !field_f32 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "f32");
            f64 = (match !field_f64 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "f64");
          }
         : floats)
      )
)
let floats_of_string s =
  read_floats (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__17 = (
  Atdgen_runtime.Oj_run.write_list (
    Yojson.Safe.write_string
  )
)
let string_of__17 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__17 ob x;
  Bi_outbuf.contents ob
let read__17 = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_string
  )
)
let _17_of_string s =
  read__17 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_extended_tuple = (
  fun ob x ->
    Bi_outbuf.add_char ob '[';
    (let x, _, _, _, _, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x, _, _, _, _ = x in
    (
      Yojson.Safe.write_std_float
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, _, x, _, _, _ = x in
    (
      Yojson.Safe.write_bool
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, _, _, x, _, _ = x in
    (
      write__4
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, _, _, _, x, _ = x in
    (
      Yojson.Safe.write_string
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, _, _, _, _, x = x in
    (
      write__17
    ) ob x
    );
    Bi_outbuf.add_char ob ']';
)
let string_of_extended_tuple ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_extended_tuple ob x;
  Bi_outbuf.contents ob
let read_extended_tuple = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_int
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_number
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x2 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_bool
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x3 =
        let x =
          (
            read__4
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x4 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_string
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      let x5 =
        if !end_of_tuple then ([])
        else (
          let x = (
            (
              read__17
            ) p lb
          ) in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple ->
            end_of_tuple := true);
          x
        )
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1, x2, x3, x4, x5)
    with Yojson.End_of_tuple ->
      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1; 4 ]);
)
let extended_tuple_of_string s =
  read_extended_tuple (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_extended : _ -> extended -> _ = (
  fun ob (x : extended) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"b0\":";
    (
      Yojson.Safe.write_int
    )
      ob x.b0x;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"b1\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.b1x;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"b2\":";
    (
      Yojson.Safe.write_string
    )
      ob x.b2x;
    (match x.b3x with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"b3\":";
      (
        Yojson.Safe.write_string
      )
        ob x;
    );
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"b4\":";
    (
      write__6
    )
      ob x.b4x;
    if x.b5x <> 0.5 then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"b5\":";
      (
        Yojson.Safe.write_std_float
      )
        ob x.b5x;
    );
    Bi_outbuf.add_char ob '}';
)
let string_of_extended ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_extended ob x;
  Bi_outbuf.contents ob
let read_extended = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_b0x = ref (None) in
    let field_b1x = ref (None) in
    let field_b2x = ref (None) in
    let field_b3x = ref (None) in
    let field_b4x = ref (None) in
    let field_b5x = ref (0.5) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 2 && String.unsafe_get s pos = 'b' then (
            match String.unsafe_get s (pos+1) with
              | '0' -> (
                  0
                )
              | '1' -> (
                  1
                )
              | '2' -> (
                  2
                )
              | '3' -> (
                  3
                )
              | '4' -> (
                  4
                )
              | '5' -> (
                  5
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_b0x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_b1x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 2 ->
            field_b2x := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_b3x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            )
          | 4 ->
            field_b4x := (
              Some (
                (
                  read__6
                ) p lb
              )
            );
          | 5 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_b5x := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 2 && String.unsafe_get s pos = 'b' then (
              match String.unsafe_get s (pos+1) with
                | '0' -> (
                    0
                  )
                | '1' -> (
                    1
                  )
                | '2' -> (
                    2
                  )
                | '3' -> (
                    3
                  )
                | '4' -> (
                    4
                  )
                | '5' -> (
                    5
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_b0x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_b1x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 2 ->
              field_b2x := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_b3x := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_string
                    ) p lb
                  )
                );
              )
            | 4 ->
              field_b4x := (
                Some (
                  (
                    read__6
                  ) p lb
                )
              );
            | 5 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_b5x := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            b0x = (match !field_b0x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b0x");
            b1x = (match !field_b1x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b1x");
            b2x = (match !field_b2x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b2x");
            b3x = !field_b3x;
            b4x = (match !field_b4x with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b4x");
            b5x = !field_b5x;
          }
         : extended)
      )
)
let extended_of_string s =
  read_extended (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__27 = (
  fun ob x -> (
    let x = ( Test_lib.Even_natural.unwrap ) x in (
      write_natural
    ) ob x)
)
let string_of__27 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__27 ob x;
  Bi_outbuf.contents ob
let read__27 = (
  fun p lb ->
    let x = (
      read_natural
    ) p lb in
    ( Test_lib.Even_natural.wrap ) x
)
let _27_of_string s =
  read__27 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_even_natural = (
  write__27
)
let string_of_even_natural ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_even_natural ob x;
  Bi_outbuf.contents ob
let read_even_natural = (
  read__27
)
let even_natural_of_string s =
  read_even_natural (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_def = (
  Test_lib.Json.write_def
)
let string_of_def ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_def ob x;
  Bi_outbuf.contents ob
let read_def = (
  Test_lib.Json.read_def
)
let def_of_string s =
  read_def (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_char = (
  Atdgen_runtime.Oj_run.write_int8
)
let string_of_char ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_char ob x;
  Bi_outbuf.contents ob
let read_char = (
  Atdgen_runtime.Oj_run.read_int8
)
let char_of_string s =
  read_char (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_base_tuple = (
  fun ob x ->
    Bi_outbuf.add_char ob '[';
    (let x, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x = x in
    (
      Yojson.Safe.write_std_float
    ) ob x
    );
    Bi_outbuf.add_char ob ']';
)
let string_of_base_tuple ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_base_tuple ob x;
  Bi_outbuf.contents ob
let read_base_tuple = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_int
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            Atdgen_runtime.Oj_run.read_number
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1)
    with Yojson.End_of_tuple ->
      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
)
let base_tuple_of_string s =
  read_base_tuple (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_base : _ -> base -> _ = (
  fun ob (x : base) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"b0\":";
    (
      Yojson.Safe.write_int
    )
      ob x.b0;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"b1\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.b1;
    Bi_outbuf.add_char ob '}';
)
let string_of_base ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_base ob x;
  Bi_outbuf.contents ob
let read_base = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_b0 = ref (None) in
    let field_b1 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 2 && String.unsafe_get s pos = 'b' then (
            match String.unsafe_get s (pos+1) with
              | '0' -> (
                  0
                )
              | '1' -> (
                  1
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_b0 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_b1 := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 2 && String.unsafe_get s pos = 'b' then (
              match String.unsafe_get s (pos+1) with
                | '0' -> (
                    0
                  )
                | '1' -> (
                    1
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_b0 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_b1 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            b0 = (match !field_b0 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b0");
            b1 = (match !field_b1 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "b1");
          }
         : base)
      )
)
let base_of_string s =
  read_base (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__23 write__a = (
  Atdgen_runtime.Oj_run.write_array (
    write__a
  )
)
let string_of__23 write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__23 write__a ob x;
  Bi_outbuf.contents ob
let read__23 read__a = (
  Atdgen_runtime.Oj_run.read_array (
    read__a
  )
)
let _23_of_string read__a s =
  read__23 read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_array write__a = (
  write__23 write__a
)
let string_of_array write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_array write__a ob x;
  Bi_outbuf.contents ob
let read_array read__a = (
  read__23 read__a
)
let array_of_string read__a s =
  read_array read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_abs3 write__a = (
  write__19 write__a
)
let string_of_abs3 write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_abs3 write__a ob x;
  Bi_outbuf.contents ob
let read_abs3 read__a = (
  read__19 read__a
)
let abs3_of_string read__a s =
  read_abs3 read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_abs2 write__a = (
  write__19 write__a
)
let string_of_abs2 write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_abs2 write__a ob x;
  Bi_outbuf.contents ob
let read_abs2 read__a = (
  read__19 read__a
)
let abs2_of_string read__a s =
  read_abs2 read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_abs1 write__a = (
  write__19 write__a
)
let string_of_abs1 write__a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_abs1 write__a ob x;
  Bi_outbuf.contents ob
let read_abs1 read__a = (
  read__19 read__a
)
let abs1_of_string read__a s =
  read_abs1 read__a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
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
