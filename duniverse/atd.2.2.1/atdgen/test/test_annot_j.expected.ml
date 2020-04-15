(* Auto-generated from "test_annot.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type pointC = ProtoC_t.pointC = { f: float }

type pointB = ProtoD_t.pointB = { f: float }

type pointA = ProtoA_t.pointA = { f: float }

let write_pointC : _ -> pointC -> _ = (
  fun ob (x : pointC) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"f\":";
    (
      Yojson.Safe.write_std_float
    )
      ob x.f;
    Bi_outbuf.add_char ob '}';
)
let string_of_pointC ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_pointC ob x;
  Bi_outbuf.contents ob
let read_pointC = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_f = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 1 && String.unsafe_get s pos = 'f' then (
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
            field_f := (
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
            if len = 1 && String.unsafe_get s pos = 'f' then (
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
              field_f := (
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
            f = (match !field_f with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "f");
          }
         : pointC)
      )
)
let pointC_of_string s =
  read_pointC (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_pointB : _ -> pointB -> _ = (
  fun ob (x : pointB) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"f\":";
    (
      Yojson.Safe.write_std_float
    )
      ob x.f;
    Bi_outbuf.add_char ob '}';
)
let string_of_pointB ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_pointB ob x;
  Bi_outbuf.contents ob
let read_pointB = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_f = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 1 && String.unsafe_get s pos = 'f' then (
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
            field_f := (
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
            if len = 1 && String.unsafe_get s pos = 'f' then (
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
              field_f := (
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
            f = (match !field_f with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "f");
          }
         : pointB)
      )
)
let pointB_of_string s =
  read_pointB (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_pointA : _ -> pointA -> _ = (
  fun ob (x : pointA) ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"f\":";
    (
      Yojson.Safe.write_std_float
    )
      ob x.f;
    Bi_outbuf.add_char ob '}';
)
let string_of_pointA ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_pointA ob x;
  Bi_outbuf.contents ob
let read_pointA = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_f = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 1 && String.unsafe_get s pos = 'f' then (
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
            field_f := (
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
            if len = 1 && String.unsafe_get s pos = 'f' then (
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
              field_f := (
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
            f = (match !field_f with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "f");
          }
         : pointA)
      )
)
let pointA_of_string s =
  read_pointA (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
