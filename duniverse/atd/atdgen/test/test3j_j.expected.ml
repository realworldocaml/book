(* Auto-generated from "test3j.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type rec_type = Test3j_t.rec_type = { more: rec_type list }

type unixtime_list = Test3j_t.unixtime_list

type json = Yojson.Safe.t

type tf_variant2 = Test3j_t.tf_variant2

type tf_variant = Test3j_t.tf_variant

type tf_record2 = Test3j_t.tf_record2 = {
  the_value2: tf_variant2;
  etc2: string
}

type tf_record = Test3j_t.tf_record = { the_value: tf_variant; etc: string }

type dyn = Yojson.Safe.t

type t = Test3j_t.t = { foo: int; bar: json; baz: dyn }

type sf_adapted = Test3j_t.sf_adapted

type sample_open_enum = Test3j_t.sample_open_enum

type sample_open_enums = Test3j_t.sample_open_enums

type patch = Test3j_t.patch = {
  patch1: int option option;
  patch2: int option option;
  patch3: int option option
}

type b = Test3j_t.b = { thing: int }

type a = Test3j_t.a = { thing: string; other_thing: bool }

type adapted = Test3j_t.adapted

let rec write__4 ob x = (
  Atdgen_runtime.Oj_run.write_list (
    write_rec_type
  )
) ob x
and string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
and write_rec_type ob (x : rec_type) = (
  Atdgen_runtime.Oj_run.write_with_adapter Json_adapters.Identity.restore (
    fun ob (x : rec_type) ->
      Bi_outbuf.add_char ob '{';
      let is_first = ref true in
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"more\":";
      (
        write__4
      )
        ob x.more;
      Bi_outbuf.add_char ob '}';
  )
) ob x
and string_of_rec_type ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_rec_type ob x;
  Bi_outbuf.contents ob
let rec read__4 p lb = (
  Atdgen_runtime.Oj_run.read_list (
    read_rec_type
  )
) p lb
and _4_of_string s =
  read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
and read_rec_type p lb = (
  Atdgen_runtime.Oj_run.read_with_adapter Json_adapters.Identity.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_lcurl p lb;
      let field_more = ref (None) in
      try
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_end lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 4 && String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'e' then (
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
              field_more := (
                Some (
                  (
                    read__4
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
              if len = 4 && String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'e' then (
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
                field_more := (
                  Some (
                    (
                      read__4
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
              more = (match !field_more with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "more");
            }
           : rec_type)
        )
  )
) p lb
and rec_type_of_string s =
  read_rec_type (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  Atdgen_runtime.Oj_run.write_list (
    Atdgen_runtime.Oj_run.write_float_as_int
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_number
  )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_unixtime_list = (
  write__1
)
let string_of_unixtime_list ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_unixtime_list ob x;
  Bi_outbuf.contents ob
let read_unixtime_list = (
  read__1
)
let unixtime_list_of_string s =
  read_unixtime_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_json = (
  Yojson.Safe.write_t
)
let string_of_json ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_json ob x;
  Bi_outbuf.contents ob
let read_json = (
  Yojson.Safe.read_t
)
let json_of_string s =
  read_json (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__5 = (
  Atdgen_runtime.Oj_run.write_nullable (
    write_json
  )
)
let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob
let read__5 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    (if Yojson.Safe.read_null_if_possible p lb then None
    else Some ((
      read_json
    ) p lb) : _ option)
)
let _5_of_string s =
  read__5 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tf_variant2 = (
  fun ob x ->
    match x with
      | `A x ->
        Bi_outbuf.add_string ob "[\"a\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | `B x ->
        Bi_outbuf.add_string ob "[\"b\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | `Unknown x ->
        Bi_outbuf.add_string ob "[\"Unknown\",";
        (
          fun ob x ->
            Bi_outbuf.add_char ob '[';
            (let x, _ = x in
            (
              Yojson.Safe.write_string
            ) ob x
            );
            Bi_outbuf.add_char ob ',';
            (let _, x = x in
            (
              write__5
            ) ob x
            );
            Bi_outbuf.add_char ob ']';
        ) ob x;
        Bi_outbuf.add_char ob ']'
)
let string_of_tf_variant2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tf_variant2 ob x;
  Bi_outbuf.contents ob
let read_tf_variant2 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "a" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `A x
            | "b" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `B x
            | "Unknown" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_string
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
                            read__5
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
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Unknown x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "a" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `A x
            | "b" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `B x
            | "Unknown" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_string
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
                            read__5
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
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Unknown x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let tf_variant2_of_string s =
  read_tf_variant2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tf_variant = (
  fun ob x ->
    match x with
      | `A x ->
        Bi_outbuf.add_string ob "[\"a\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Bi_outbuf.add_char ob ']'
      | `B x ->
        Bi_outbuf.add_string ob "[\"b\",";
        (
          Yojson.Safe.write_int
        ) ob x;
        Bi_outbuf.add_char ob ']'
)
let string_of_tf_variant ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tf_variant ob x;
  Bi_outbuf.contents ob
let read_tf_variant = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "a" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `A x
            | "b" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `B x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "a" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `A x
            | "b" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `B x
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let tf_variant_of_string s =
  read_tf_variant (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tf_record2 : _ -> tf_record2 -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter Test_lib.Tag_field_with_catchall.restore (
    fun ob (x : tf_record2) ->
      Bi_outbuf.add_char ob '{';
      let is_first = ref true in
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"the_value2\":";
      (
        write_tf_variant2
      )
        ob x.the_value2;
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"etc2\":";
      (
        Yojson.Safe.write_string
      )
        ob x.etc2;
      Bi_outbuf.add_char ob '}';
  )
)
let string_of_tf_record2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tf_record2 ob x;
  Bi_outbuf.contents ob
let read_tf_record2 = (
  Atdgen_runtime.Oj_run.read_with_adapter Test_lib.Tag_field_with_catchall.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_lcurl p lb;
      let field_the_value2 = ref (None) in
      let field_etc2 = ref (None) in
      try
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_end lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = '2' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = '2' then (
                    0
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
              field_the_value2 := (
                Some (
                  (
                    read_tf_variant2
                  ) p lb
                )
              );
            | 1 ->
              field_etc2 := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
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
                | 4 -> (
                    if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = '2' then (
                      1
                    )
                    else (
                      -1
                    )
                  )
                | 10 -> (
                    if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = '2' then (
                      0
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
                field_the_value2 := (
                  Some (
                    (
                      read_tf_variant2
                    ) p lb
                  )
                );
              | 1 ->
                field_etc2 := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_string
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
              the_value2 = (match !field_the_value2 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "the_value2");
              etc2 = (match !field_etc2 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "etc2");
            }
           : tf_record2)
        )
  )
)
let tf_record2_of_string s =
  read_tf_record2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tf_record : _ -> tf_record -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter Test_lib.Tag_field_example.restore (
    fun ob (x : tf_record) ->
      Bi_outbuf.add_char ob '{';
      let is_first = ref true in
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"the_value\":";
      (
        write_tf_variant
      )
        ob x.the_value;
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"etc\":";
      (
        Yojson.Safe.write_string
      )
        ob x.etc;
      Bi_outbuf.add_char ob '}';
  )
)
let string_of_tf_record ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tf_record ob x;
  Bi_outbuf.contents ob
let read_tf_record = (
  Atdgen_runtime.Oj_run.read_with_adapter Test_lib.Tag_field_example.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_lcurl p lb;
      let field_the_value = ref (None) in
      let field_etc = ref (None) in
      try
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_end lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 3 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'c' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'e' then (
                    0
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
              field_the_value := (
                Some (
                  (
                    read_tf_variant
                  ) p lb
                )
              );
            | 1 ->
              field_etc := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
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
                | 3 -> (
                    if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'c' then (
                      1
                    )
                    else (
                      -1
                    )
                  )
                | 9 -> (
                    if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'e' then (
                      0
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
                field_the_value := (
                  Some (
                    (
                      read_tf_variant
                    ) p lb
                  )
                );
              | 1 ->
                field_etc := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_string
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
              the_value = (match !field_the_value with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "the_value");
              etc = (match !field_etc with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "etc");
            }
           : tf_record)
        )
  )
)
let tf_record_of_string s =
  read_tf_record (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_dyn = (
  Yojson.Safe.write_t
)
let string_of_dyn ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_dyn ob x;
  Bi_outbuf.contents ob
let read_dyn = (
  Yojson.Safe.read_t
)
let dyn_of_string s =
  read_dyn (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_t : _ -> t -> _ = (
  fun ob (x : t) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"foo\":";
    (
      Yojson.Safe.write_int
    )
      ob x.foo;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"bar\":";
    (
      write_json
    )
      ob x.bar;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"baz\":";
    (
      write_dyn
    )
      ob x.baz;
    Bi_outbuf.add_char ob '}';
)
let string_of_t ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_t ob x;
  Bi_outbuf.contents ob
let read_t = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_foo = ref (None) in
    let field_bar = ref (None) in
    let field_baz = ref (None) in
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
              | 'b' -> (
                  if String.unsafe_get s (pos+1) = 'a' then (
                    match String.unsafe_get s (pos+2) with
                      | 'r' -> (
                          1
                        )
                      | 'z' -> (
                          2
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 'f' -> (
                  if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'o' then (
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
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_foo := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_bar := (
              Some (
                (
                  read_json
                ) p lb
              )
            );
          | 2 ->
            field_baz := (
              Some (
                (
                  read_dyn
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
                | 'b' -> (
                    if String.unsafe_get s (pos+1) = 'a' then (
                      match String.unsafe_get s (pos+2) with
                        | 'r' -> (
                            1
                          )
                        | 'z' -> (
                            2
                          )
                        | _ -> (
                            -1
                          )
                    )
                    else (
                      -1
                    )
                  )
                | 'f' -> (
                    if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'o' then (
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
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_foo := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_bar := (
                Some (
                  (
                    read_json
                  ) p lb
                )
              );
            | 2 ->
              field_baz := (
                Some (
                  (
                    read_dyn
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
            foo = (match !field_foo with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "foo");
            bar = (match !field_bar with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "bar");
            baz = (match !field_baz with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "baz");
          }
         : t)
      )
)
let t_of_string s =
  read_t (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_sf_adapted = (
  Atdgen_runtime.Oj_run.write_with_adapter Atdgen_runtime.Json_adapter.One_field.restore (
    fun ob x ->
      match x with
        | `A x ->
          Bi_outbuf.add_string ob "[\"a\",";
          (
            Yojson.Safe.write_bool
          ) ob x;
          Bi_outbuf.add_char ob ']'
        | `B x ->
          Bi_outbuf.add_string ob "[\"b\",";
          (
            Yojson.Safe.write_int
          ) ob x;
          Bi_outbuf.add_char ob ']'
  )
)
let string_of_sf_adapted ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_sf_adapted ob x;
  Bi_outbuf.contents ob
let read_sf_adapted = (
  Atdgen_runtime.Oj_run.read_with_adapter Atdgen_runtime.Json_adapter.One_field.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            match Yojson.Safe.read_ident p lb with
              | "a" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `A x
              | "b" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `B x
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "a" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                `A x
              | "b" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                `B x
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let sf_adapted_of_string s =
  read_sf_adapted (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_sample_open_enum = (
  fun ob x ->
    match x with
      | `Alpha -> Bi_outbuf.add_string ob "\"Alpha\""
      | `Beta -> Bi_outbuf.add_string ob "\"Beta\""
      | `Other x -> (
          Yojson.Safe.write_string
        ) ob x;
)
let string_of_sample_open_enum ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_sample_open_enum ob x;
  Bi_outbuf.contents ob
let read_sample_open_enum = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "Alpha" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Alpha
            | "Beta" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Beta
            | x ->
              `Other x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "Alpha" ->
              `Alpha
            | "Beta" ->
              `Beta
            | x ->
              `Other x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let sample_open_enum_of_string s =
  read_sample_open_enum (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__6 = (
  Atdgen_runtime.Oj_run.write_list (
    write_sample_open_enum
  )
)
let string_of__6 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__6 ob x;
  Bi_outbuf.contents ob
let read__6 = (
  Atdgen_runtime.Oj_run.read_list (
    read_sample_open_enum
  )
)
let _6_of_string s =
  read__6 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_sample_open_enums = (
  write__6
)
let string_of_sample_open_enums ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_sample_open_enums ob x;
  Bi_outbuf.contents ob
let read_sample_open_enums = (
  read__6
)
let sample_open_enums_of_string s =
  read_sample_open_enums (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__2 = (
  Atdgen_runtime.Oj_run.write_nullable (
    Yojson.Safe.write_int
  )
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    (if Yojson.Safe.read_null_if_possible p lb then None
    else Some ((
      Atdgen_runtime.Oj_run.read_int
    ) p lb) : _ option)
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  Atdgen_runtime.Oj_run.write_std_option (
    write__2
  )
)
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let read__3 = (
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
                  read__2
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
                  read__2
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_patch : _ -> patch -> _ = (
  fun ob (x : patch) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    (match x.patch1 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"patch1\":";
      (
        write__2
      )
        ob x;
    );
    (match x.patch2 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"patch2\":";
      (
        write__2
      )
        ob x;
    );
    (match x.patch3 with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"patch3\":";
      (
        write__2
      )
        ob x;
    );
    Bi_outbuf.add_char ob '}';
)
let string_of_patch ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_patch ob x;
  Bi_outbuf.contents ob
let read_patch = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_patch1 = ref (None) in
    let field_patch2 = ref (None) in
    let field_patch3 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 6 && String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'h' then (
            match String.unsafe_get s (pos+5) with
              | '1' -> (
                  0
                )
              | '2' -> (
                  1
                )
              | '3' -> (
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
            field_patch1 := (
              Some (
                (
                  read__2
                ) p lb
              )
            );
          | 1 ->
            field_patch2 := (
              Some (
                (
                  read__2
                ) p lb
              )
            );
          | 2 ->
            field_patch3 := (
              Some (
                (
                  read__2
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
            if len = 6 && String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'h' then (
              match String.unsafe_get s (pos+5) with
                | '1' -> (
                    0
                  )
                | '2' -> (
                    1
                  )
                | '3' -> (
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
              field_patch1 := (
                Some (
                  (
                    read__2
                  ) p lb
                )
              );
            | 1 ->
              field_patch2 := (
                Some (
                  (
                    read__2
                  ) p lb
                )
              );
            | 2 ->
              field_patch3 := (
                Some (
                  (
                    read__2
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
            patch1 = !field_patch1;
            patch2 = !field_patch2;
            patch3 = !field_patch3;
          }
         : patch)
      )
)
let patch_of_string s =
  read_patch (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_b : _ -> b -> _ = (
  fun ob (x : b) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"thing\":";
    (
      Yojson.Safe.write_int
    )
      ob x.thing;
    Bi_outbuf.add_char ob '}';
)
let string_of_b ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_b ob x;
  Bi_outbuf.contents ob
let read_b = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_thing = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 5 && String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'g' then (
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
            field_thing := (
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
            if len = 5 && String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'g' then (
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
              field_thing := (
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
            thing = (match !field_thing with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "thing");
          }
         : b)
      )
)
let b_of_string s =
  read_b (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_a : _ -> a -> _ = (
  fun ob (x : a) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"thing\":";
    (
      Yojson.Safe.write_string
    )
      ob x.thing;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"other_thing\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.other_thing;
    Bi_outbuf.add_char ob '}';
)
let string_of_a ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_a ob x;
  Bi_outbuf.contents ob
let read_a = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_thing = ref (None) in
    let field_other_thing = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 5 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'g' then (
                  0
                )
                else (
                  -1
                )
              )
            | 11 -> (
                if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'h' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'h' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 'g' then (
                  1
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
            field_thing := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_other_thing := (
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
            match len with
              | 5 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'g' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 11 -> (
                  if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'h' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'h' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 'g' then (
                    1
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
              field_thing := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_other_thing := (
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
            thing = (match !field_thing with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "thing");
            other_thing = (match !field_other_thing with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "other_thing");
          }
         : a)
      )
)
let a_of_string s =
  read_a (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_adapted = (
  Atdgen_runtime.Oj_run.write_with_adapter Atdgen_runtime.Json_adapter.Type_field.restore (
    fun ob x ->
      match x with
        | `A x ->
          Bi_outbuf.add_string ob "[\"a\",";
          (
            write_a
          ) ob x;
          Bi_outbuf.add_char ob ']'
        | `B x ->
          Bi_outbuf.add_string ob "[\"b\",";
          (
            write_b
          ) ob x;
          Bi_outbuf.add_char ob ']'
  )
)
let string_of_adapted ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_adapted ob x;
  Bi_outbuf.contents ob
let read_adapted = (
  Atdgen_runtime.Oj_run.read_with_adapter Atdgen_runtime.Json_adapter.Type_field.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            match Yojson.Safe.read_ident p lb with
              | "a" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_a
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `A x
              | "b" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_b
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                `B x
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "a" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_a
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                `A x
              | "b" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_b
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                `B x
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let adapted_of_string s =
  read_adapted (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
