(* Auto-generated from "test2.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]
open Test

type ('aa, 'bb) poly = ('aa, 'bb) Test.poly

type poly_int2 = (int, int) poly

type test2 = { test0: poly_int2; test1: (int, string option) poly }

type poly_int_string = (int, string) poly

let poly_tag = Test.poly_tag
let write_untagged_poly _aa_tag write_untagged__aa write__aa _bb_tag write_untagged__bb write__bb = (
  Test.write_untagged_poly _aa_tag write_untagged__aa write__aa _bb_tag write_untagged__bb write__bb
)
let write_poly _aa_tag write_untagged__aa write__aa _bb_tag write_untagged__bb write__bb ob x =
  Bi_io.write_tag ob Test.poly_tag;
  write_untagged_poly _aa_tag write_untagged__aa write__aa _bb_tag write_untagged__bb write__bb ob x
let string_of_poly _aa_tag write_untagged__aa write__aa _bb_tag write_untagged__bb write__bb ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_poly _aa_tag write_untagged__aa write__aa _bb_tag write_untagged__bb write__bb ob x;
  Bi_outbuf.contents ob
let get_poly_reader get__aa_reader read__aa get__bb_reader read__bb = (
  Test.get_poly_reader get__aa_reader read__aa get__bb_reader read__bb
)
let read_poly get__aa_reader read__aa get__bb_reader read__bb = (
  Test.read_poly get__aa_reader read__aa get__bb_reader read__bb
)
let poly_of_string get__aa_reader read__aa get__bb_reader read__bb ?pos s =
  read_poly get__aa_reader read__aa get__bb_reader read__bb (Bi_inbuf.from_string ?pos s)
let _1_tag = Test.poly_tag
let write_untagged__1 = (
  write_untagged_poly Bi_io.svint_tag Bi_io.write_untagged_svint Bi_io.write_svint Bi_io.svint_tag Bi_io.write_untagged_svint Bi_io.write_svint
)
let write__1 ob x =
  Bi_io.write_tag ob Test.poly_tag;
  write_untagged__1 ob x
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let get__1_reader = (
  get_poly_reader Atdgen_runtime.Ob_run.get_int_reader Atdgen_runtime.Ob_run.read_int Atdgen_runtime.Ob_run.get_int_reader Atdgen_runtime.Ob_run.read_int
)
let read__1 = (
  read_poly Atdgen_runtime.Ob_run.get_int_reader Atdgen_runtime.Ob_run.read_int Atdgen_runtime.Ob_run.get_int_reader Atdgen_runtime.Ob_run.read_int
)
let _1_of_string ?pos s =
  read__1 (Bi_inbuf.from_string ?pos s)
let poly_int2_tag = Test.poly_tag
let write_untagged_poly_int2 = (
  write_untagged__1
)
let write_poly_int2 ob x =
  Bi_io.write_tag ob Test.poly_tag;
  write_untagged_poly_int2 ob x
let string_of_poly_int2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_poly_int2 ob x;
  Bi_outbuf.contents ob
let get_poly_int2_reader = (
  get__1_reader
)
let read_poly_int2 = (
  read__1
)
let poly_int2_of_string ?pos s =
  read_poly_int2 (Bi_inbuf.from_string ?pos s)
let _3_tag = Bi_io.num_variant_tag
let write_untagged__3 = (
  Atdgen_runtime.Ob_run.write_untagged_option (
    Bi_io.write_string
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
                Atdgen_runtime.Ob_run.read_string
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
            Atdgen_runtime.Ob_run.read_string
          )
            ib
        )
      | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let _3_of_string ?pos s =
  read__3 (Bi_inbuf.from_string ?pos s)
let _4_tag = Test.poly_tag
let write_untagged__4 = (
  write_untagged_poly Bi_io.svint_tag Bi_io.write_untagged_svint Bi_io.write_svint _3_tag write_untagged__3 write__3
)
let write__4 ob x =
  Bi_io.write_tag ob Test.poly_tag;
  write_untagged__4 ob x
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let get__4_reader = (
  get_poly_reader Atdgen_runtime.Ob_run.get_int_reader Atdgen_runtime.Ob_run.read_int get__3_reader read__3
)
let read__4 = (
  read_poly Atdgen_runtime.Ob_run.get_int_reader Atdgen_runtime.Ob_run.read_int get__3_reader read__3
)
let _4_of_string ?pos s =
  read__4 (Bi_inbuf.from_string ?pos s)
let test2_tag = Bi_io.record_tag
let write_untagged_test2 : Bi_outbuf.t -> test2 -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\141' '\149' '\127' '\158';
    (
      write_poly_int2
    ) ob x.test0;
    Bi_outbuf.add_char4 ob '\141' '\149' '\127' '\159';
    (
      write__4
    ) ob x.test1;
)
let write_test2 ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_test2 ob x
let string_of_test2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_test2 ob x;
  Bi_outbuf.contents ob
let get_test2_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_test0 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_test1 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 227901342 ->
              field_test0 := (
                (
                  read_poly_int2
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 227901343 ->
              field_test1 := (
                (
                  read__4
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "test0"; "test1" |];
        (
          {
            test0 = !field_test0;
            test1 = !field_test1;
          }
         : test2)
)
let read_test2 = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_test0 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_test1 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 227901342 ->
          field_test0 := (
            (
              read_poly_int2
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 227901343 ->
          field_test1 := (
            (
              read__4
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "test0"; "test1" |];
    (
      {
        test0 = !field_test0;
        test1 = !field_test1;
      }
     : test2)
)
let test2_of_string ?pos s =
  read_test2 (Bi_inbuf.from_string ?pos s)
let _2_tag = Test.poly_tag
let write_untagged__2 = (
  write_untagged_poly Bi_io.svint_tag Bi_io.write_untagged_svint Bi_io.write_svint Bi_io.string_tag Bi_io.write_untagged_string Bi_io.write_string
)
let write__2 ob x =
  Bi_io.write_tag ob Test.poly_tag;
  write_untagged__2 ob x
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let get__2_reader = (
  get_poly_reader Atdgen_runtime.Ob_run.get_int_reader Atdgen_runtime.Ob_run.read_int Atdgen_runtime.Ob_run.get_string_reader Atdgen_runtime.Ob_run.read_string
)
let read__2 = (
  read_poly Atdgen_runtime.Ob_run.get_int_reader Atdgen_runtime.Ob_run.read_int Atdgen_runtime.Ob_run.get_string_reader Atdgen_runtime.Ob_run.read_string
)
let _2_of_string ?pos s =
  read__2 (Bi_inbuf.from_string ?pos s)
let poly_int_string_tag = Test.poly_tag
let write_untagged_poly_int_string = (
  write_untagged__2
)
let write_poly_int_string ob x =
  Bi_io.write_tag ob Test.poly_tag;
  write_untagged_poly_int_string ob x
let string_of_poly_int_string ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_poly_int_string ob x;
  Bi_outbuf.contents ob
let get_poly_int_string_reader = (
  get__2_reader
)
let read_poly_int_string = (
  read__2
)
let poly_int_string_of_string ?pos s =
  read_poly_int_string (Bi_inbuf.from_string ?pos s)
let create_test2 
  ~test0
  ~test1
  () : test2 =
  {
    test0 = test0;
    test1 = test1;
  }
