(* Auto-generated from "test_ambiguous_record.atd" *)
[@@@ocaml.warning "-27-32-35-39"]
open Test_ambiguous_record_t

let write_ambiguous' : _ -> ambiguous' -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter Json_adapters.Identity.restore (
    fun ob (x : ambiguous') ->
      Bi_outbuf.add_char ob '{';
      let is_first = ref true in
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"ambiguous\":";
      (
        Yojson.Safe.write_string
      )
        ob x.ambiguous;
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"not_ambiguous2\":";
      (
        Yojson.Safe.write_int
      )
        ob x.not_ambiguous2;
      Bi_outbuf.add_char ob '}';
  )
)
let string_of_ambiguous' ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_ambiguous' ob x;
  Bi_outbuf.contents ob
let read_ambiguous' = (
  Atdgen_runtime.Oj_run.read_with_adapter Json_adapters.Identity.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_lcurl p lb;
      let field_ambiguous = ref (None) in
      let field_not_ambiguous2 = ref (None) in
      try
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_end lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 9 -> (
                  if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = 'u' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 14 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'b' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'g' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = '2' then (
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
              field_ambiguous := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_not_ambiguous2 := (
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
              match len with
                | 9 -> (
                    if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = 'u' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 's' then (
                      0
                    )
                    else (
                      -1
                    )
                  )
                | 14 -> (
                    if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'b' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'g' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = '2' then (
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
                field_ambiguous := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_string
                    ) p lb
                  )
                );
              | 1 ->
                field_not_ambiguous2 := (
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
              ambiguous = (match !field_ambiguous with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "ambiguous");
              not_ambiguous2 = (match !field_not_ambiguous2 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "not_ambiguous2");
            }
           : ambiguous')
        )
  )
)
let ambiguous'_of_string s =
  read_ambiguous' (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_ambiguous : _ -> ambiguous -> _ = (
  fun ob (x : ambiguous) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"ambiguous\":";
    (
      Yojson.Safe.write_string
    )
      ob x.ambiguous;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"not_ambiguous1\":";
    (
      Yojson.Safe.write_int
    )
      ob x.not_ambiguous1;
    Bi_outbuf.add_char ob '}';
)
let string_of_ambiguous ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_ambiguous ob x;
  Bi_outbuf.contents ob
let read_ambiguous = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_ambiguous = ref (None) in
    let field_not_ambiguous1 = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 9 -> (
                if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = 'u' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | 14 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'b' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'g' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = '1' then (
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
            field_ambiguous := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_not_ambiguous1 := (
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
            match len with
              | 9 -> (
                  if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = 'u' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 14 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'm' && String.unsafe_get s (pos+6) = 'b' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'g' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = '1' then (
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
              field_ambiguous := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_not_ambiguous1 := (
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
            ambiguous = (match !field_ambiguous with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "ambiguous");
            not_ambiguous1 = (match !field_not_ambiguous1 with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "not_ambiguous1");
          }
         : ambiguous)
      )
)
let ambiguous_of_string s =
  read_ambiguous (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let create_ambiguous' 
  ~ambiguous
  ~not_ambiguous2
  () : ambiguous' =
  {
    ambiguous = ambiguous;
    not_ambiguous2 = not_ambiguous2;
  }
let create_ambiguous 
  ~ambiguous
  ~not_ambiguous1
  () : ambiguous =
  {
    ambiguous = ambiguous;
    not_ambiguous1 = not_ambiguous1;
  }
