(* Auto-generated from "github_org.atd" *)


type org = Github_org_t.org = {
  login: string;
  id: int;
  url: string;
  name: string option;
  blog: string option;
  email: string option;
  public_repos: int
}

let write__1 = (
  Ag_oj_run.write_option (
    Yojson.Safe.write_string
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 then (
                  match String.unsafe_get s pos with
                    | 'N' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | 'S' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          1
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | _ -> (
                        raise (Exit)
                      )
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              None
            | 1 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  Ag_oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              Some x
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'N' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              None
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Ag_oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              Some x
            | _ -> (
                assert false
              )
        )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_org = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"login\":";
    (
      Yojson.Safe.write_string
    )
      ob x.login;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"id\":";
    (
      Yojson.Safe.write_int
    )
      ob x.id;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"url\":";
    (
      Yojson.Safe.write_string
    )
      ob x.url;
    (match x.name with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"name\":";
      (
        Yojson.Safe.write_string
      )
        ob x;
    );
    (match x.blog with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"blog\":";
      (
        Yojson.Safe.write_string
      )
        ob x;
    );
    (match x.email with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"email\":";
      (
        Yojson.Safe.write_string
      )
        ob x;
    );
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"public_repos\":";
    (
      Yojson.Safe.write_int
    )
      ob x.public_repos;
    Bi_outbuf.add_char ob '}';
)
let string_of_org ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_org ob x;
  Bi_outbuf.contents ob
let read_org = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let x =
      {
        login = Obj.magic 0.0;
        id = Obj.magic 0.0;
        url = Obj.magic 0.0;
        name = None;
        blog = None;
        email = None;
        public_repos = Obj.magic 0.0;
      }
    in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 2 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'd' then (
                  1
                )
                else (
                  -1
                )
              )
            | 3 -> (
                if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'l' then (
                  2
                )
                else (
                  -1
                )
              )
            | 4 -> (
                match String.unsafe_get s pos with
                  | 'b' -> (
                      if String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'g' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | 'n' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                        3
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 5 -> (
                match String.unsafe_get s pos with
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'l' then (
                        5
                      )
                      else (
                        -1
                      )
                    )
                  | 'l' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'n' then (
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
            | 12 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'p' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 's' then (
                  6
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
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                Ag_oj_run.read_int
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | 2 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            bits0 := !bits0 lor 0x4;
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                Some (
                  (
                    Ag_oj_run.read_string
                  ) p lb
                )
              in
              Obj.set_field (Obj.repr x) 3 (Obj.repr v);
            )
          | 4 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                Some (
                  (
                    Ag_oj_run.read_string
                  ) p lb
                )
              in
              Obj.set_field (Obj.repr x) 4 (Obj.repr v);
            )
          | 5 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              let v =
                Some (
                  (
                    Ag_oj_run.read_string
                  ) p lb
                )
              in
              Obj.set_field (Obj.repr x) 5 (Obj.repr v);
            )
          | 6 ->
            let v =
              (
                Ag_oj_run.read_int
              ) p lb
            in
            Obj.set_field (Obj.repr x) 6 (Obj.repr v);
            bits0 := !bits0 lor 0x8;
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
              | 2 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'd' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 3 -> (
                  if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'l' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 4 -> (
                  match String.unsafe_get s pos with
                    | 'b' -> (
                        if String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'g' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | 'n' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 5 -> (
                  match String.unsafe_get s pos with
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'l' then (
                          5
                        )
                        else (
                          -1
                        )
                      )
                    | 'l' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'n' then (
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
              | 12 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'p' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 's' then (
                    6
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
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  Ag_oj_run.read_int
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | 2 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              bits0 := !bits0 lor 0x4;
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  Some (
                    (
                      Ag_oj_run.read_string
                    ) p lb
                  )
                in
                Obj.set_field (Obj.repr x) 3 (Obj.repr v);
              )
            | 4 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  Some (
                    (
                      Ag_oj_run.read_string
                    ) p lb
                  )
                in
                Obj.set_field (Obj.repr x) 4 (Obj.repr v);
              )
            | 5 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                let v =
                  Some (
                    (
                      Ag_oj_run.read_string
                    ) p lb
                  )
                in
                Obj.set_field (Obj.repr x) 5 (Obj.repr v);
              )
            | 6 ->
              let v =
                (
                  Ag_oj_run.read_int
                ) p lb
              in
              Obj.set_field (Obj.repr x) 6 (Obj.repr v);
              bits0 := !bits0 lor 0x8;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0xf then Ag_oj_run.missing_fields [| !bits0 |] [| "login"; "id"; "url"; "public_repos" |];
        Ag_oj_run.identity x
      )
)
let org_of_string s =
  read_org (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
