(*
  OCaml code generator for the biniou format.
*)


open Atd.Import
open Indent

open Atd.Ast
open Mapping
open Ob_mapping

(*
  OCaml code generator (biniou readers and writers)
*)


let make_ocaml_biniou_intf ~with_create buf deref defs =
  List.concat_map snd defs
  |> List.filter Ox_emit.include_intf
  |> List.iter (fun x ->
    let full_name = Ox_emit.get_full_type_name x in
    let writer_params =
      String.concat "" (
        List.map
          (fun s ->
             sprintf "\n  Bi_io.node_tag ->\
                      \n  (Bi_outbuf.t -> '%s -> unit) ->\
                      \n  (Bi_outbuf.t -> '%s -> unit) ->" s s)
          x.def_param
      )
    in
    let reader_params =
      String.concat "" (
        List.map (
          fun s ->
            sprintf
              "\n  (Bi_io.node_tag -> (Bi_inbuf.t -> '%s)) ->\
               \n  (Bi_inbuf.t -> '%s) ->" s s
        )
          x.def_param
      )
    in
    bprintf buf "(* Writers for type %s *)\n\n" x.def_name;

    bprintf buf "\
val %s_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!%s}.
      Readers may support more than just this tag. *)

" x.def_name x.def_name;

    bprintf buf "\
val write_untagged_%s :%s
  Bi_outbuf.t -> %s -> unit
  (** Output an untagged biniou value of type {!%s}. *)

" x.def_name writer_params full_name x.def_name;

    bprintf buf "\
val write_%s :%s
  Bi_outbuf.t -> %s -> unit
  (** Output a biniou value of type {!%s}. *)

" x.def_name writer_params full_name x.def_name;

    bprintf buf "\
val string_of_%s :%s
  ?len:int -> %s -> string
  (** Serialize a value of type {!%s} into
      a biniou string. *)

" x.def_name writer_params full_name x.def_name;

    bprintf buf "(* Readers for type %s *)\n\n" x.def_name;

    bprintf buf "\
val get_%s_reader :%s
  Bi_io.node_tag -> (Bi_inbuf.t -> %s)
  (** Return a function that reads an untagged
      biniou value of type {!%s}. *)

" x.def_name reader_params full_name x.def_name;

    bprintf buf "\
val read_%s :%s
  Bi_inbuf.t -> %s
  (** Input a tagged biniou value of type {!%s}. *)

" x.def_name reader_params full_name x.def_name;

    bprintf buf "\
val %s_of_string :%s
  ?pos:int -> string -> %s
  (** Deserialize a biniou value of type {!%s}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

" x.def_name reader_params full_name x.def_name;

    Ox_emit.maybe_write_creator_intf ~with_create deref buf x
  )


let rec get_biniou_tag (x : ob_mapping) =
  match x with
    Unit (_, Unit, Unit) -> "Bi_io.unit_tag"
  | Bool (_, Bool, Bool) -> "Bi_io.bool_tag"
  | Int (_, Int _, Int b) ->
      (match b with
         `Uvint -> "Bi_io.uvint_tag"
       | `Svint -> "Bi_io.svint_tag"
       | `Int8 -> "Bi_io.int8_tag"
       | `Int16 -> "Bi_io.int16_tag"
       | `Int32 -> "Bi_io.int32_tag"
       | `Int64 -> "Bi_io.int64_tag"
      )
  | Float (_, Float, Float b) ->
      (match b with
         `Float32 -> "Bi_io.float32_tag"
       | `Float64 -> "Bi_io.float64_tag"
      )
  | String (_, String, String) -> "Bi_io.string_tag"
  | Sum (_, _, Sum _, Sum) -> "Bi_io.variant_tag"
  | Record (_, _, Record _, Record) -> "Bi_io.record_tag"
  | Tuple (_, _, Tuple, Tuple) -> "Bi_io.tuple_tag"
  | List (_, _, List _, List b) ->
      (match b with
         `Array -> "Bi_io.array_tag"
       | `Table -> "Bi_io.table_tag"
      )
  | Option (_, _, Option, Option)
  | Nullable (_, _, Nullable, Nullable) -> "Bi_io.num_variant_tag"
  | Wrap (_, x, Wrap _, Wrap) -> get_biniou_tag x

  | Name (_, s, _, None, None) -> sprintf "%s_tag" s
  | External (_, _, _,
              External (_, main_module, ext_name),
              External) ->
      sprintf "%s.%s_tag" main_module ext_name
  | Tvar (_, s) -> sprintf "%s_tag" (Ox_emit.name_of_var s)
  | _ -> assert false

let get_fields deref a =
  List.map (fun x ->
    let (ocamlf, binf) =
      match x.f_arepr, x.f_brepr with
      | Ocaml.Repr.Field o, Biniou.Field b -> o, b
      | _, _ -> assert false
    in
    let ocaml_default = Ox_emit.default_value x deref in
    (x, ocamlf.Ocaml.ocaml_fname , ocaml_default
    , (not (Atd.Ast.is_required x.f_kind))
    , binf.Biniou.biniou_unwrapped)
  ) (Array.to_list a)

let rec get_writer_name
    ?(paren = false)
    ?name_f
    ~tagged
    (x : ob_mapping) : string =

  let name_f =
    match name_f with
      Some f -> f
    | None ->
        if tagged then
          (fun s -> "write_" ^ s)
        else
          (fun s -> "write_untagged_" ^ s)
  in

  let un = if tagged then "" else "untagged_" in
  match x with
    Unit (_, Unit, Unit) ->
      sprintf "Bi_io.write_%sunit" un
  | Bool (_, Bool, Bool) ->
      sprintf "Bi_io.write_%sbool" un
  | Int (loc, Int o, Int b) ->
      (match o, b with
         Int, `Uvint -> sprintf "Bi_io.write_%suvint" un
       | Int, `Svint -> sprintf "Bi_io.write_%ssvint" un
       | Char, `Int8 -> sprintf "Bi_io.write_%schar" un
       | Int, `Int8 -> sprintf "Bi_io.write_%sint8" un
       | Int, `Int16 -> sprintf "Bi_io.write_%sint16" un
       | Int32, `Int32 -> sprintf "Bi_io.write_%sint32" un
       | Int64, `Int64 -> sprintf "Bi_io.write_%sint64" un
       | _ ->
           Error.error loc "Unsupported combination of OCaml/Biniou int types"
      )

  | Float (_, Float, Float b) ->
      (match b with
         `Float32 -> sprintf "Bi_io.write_%sfloat32" un
       | `Float64 -> sprintf "Bi_io.write_%sfloat64" un
      )
  | String (_, String, String) ->
      sprintf "Bi_io.write_%sstring" un

  | Tvar (_, s) ->
      sprintf "write_%s%s" un (Ox_emit.name_of_var s)

  | Name (_, s, args, None, None) ->
      let l = List.map get_writer_names args in
      let s = String.concat " " (name_f s :: l) in
      if paren && l <> [] then "(" ^ s ^ ")"
      else s

  | External (_, _, args,
              External (_, main_module, ext_name),
              External) ->
      let f = main_module ^ "." ^ name_f ext_name in
      let l = List.map get_writer_names args in
      let s = String.concat " " (f :: l) in
      if paren && l <> [] then "(" ^ s ^ ")"
      else s

  | _ -> assert false

and get_writer_names x =
  let tag = get_biniou_tag x in
  let write_untagged = get_writer_name ~paren:true ~tagged:false x in
  let write = get_writer_name ~paren:true ~tagged:true x in
  String.concat " " [ tag; write_untagged; write ]


let get_left_writer_name ~tagged name param =
  let args = List.map (fun s -> Tvar (dummy_loc, s)) param in
  get_writer_name ~tagged
    (Name (dummy_loc, name, args, None, None))

let get_left_to_string_name name param =
  let name_f s = "string_of_" ^ s in
  let args = List.map (fun s -> Tvar (dummy_loc, s)) param in
  get_writer_name ~tagged:true ~name_f
    (Name (dummy_loc, name, args, None, None))

(*
let make_writer_name tagged loc name args =
  let un = if tagged then "" else "untagged_" in
  let f = sprintf "write_%s%s" un name in
  let l =
    List.map (
      function
          `Tvar (loc, s) ->
            let name = name_of_var s in
            (* TODO (incomplete) *)
            [ sprintf "%s_tag" name;
              sprintf "write_%s" name ]
        | _ -> assert false
    ) args
  in
  String.concat " " (f :: List.flatten l)
*)

let rec get_reader_name
    ?(paren = false)
    ?name_f
    ~tagged
    (x : ob_mapping) : string =

  let name_f =
    match name_f with
      Some f -> f
    | None ->
        if tagged then
          (fun s -> "read_" ^ s)
        else
          (fun s -> sprintf "get_%s_reader" s)
  in

  let xreader s =
    if tagged then
      sprintf "Atdgen_runtime.Ob_run.read_%s" s
    else
      sprintf "Atdgen_runtime.Ob_run.get_%s_reader" s
  in
  match x with
    Unit (_, Unit, Unit) -> xreader "unit"

  | Bool (_, Bool, Bool) -> xreader "bool"

  | Int (loc, Int o, Int b) ->
      (match o, b with
         Int, `Uvint
       | Int, `Svint
       | Int, `Int8
       | Int, `Int16 -> xreader "int"
       | Char, `Int8 -> xreader "char"
       | Int32, `Int32 -> xreader "int32"
       | Int64, `Int64 -> xreader "int64"
       | _ ->
           Error.error loc "Unsupported combination of OCaml/Biniou int types"
      )

  | Float (_, Float, Float b) ->
      (match b with
         `Float32 -> xreader "float32"
       | `Float64 -> xreader "float64"
      )

  | String (_, String, String) -> xreader "string"

  | Tvar (_, s) ->
      let name = Ox_emit.name_of_var s in
      if tagged then
        sprintf "read_%s" name
      else
        sprintf "get_%s_reader" name

  | Name (_, s, args, None, None) ->
      let l = List.map get_reader_names args in
      let s = String.concat " " (name_f s :: l) in
      if paren && l <> [] then "(" ^ s ^ ")"
      else s

  | External (_, _, args,
              External (_, main_module, ext_name),
              External) ->
      let f = main_module ^ "." ^ name_f ext_name in
      let l = List.map get_reader_names args in
      let s = String.concat " " (f :: l) in
      if paren && l <> [] then "(" ^ s ^ ")"
      else s

  | _ -> assert false

and get_reader_names x =
  let get_reader = get_reader_name ~paren:true ~tagged:false x in
  let reader = get_reader_name ~paren:true ~tagged:true x in
  String.concat " " [ get_reader; reader ]


let get_left_reader_name ~tagged name param =
  let args = List.map (fun s -> Tvar (dummy_loc, s)) param in
  get_reader_name ~tagged (Name (dummy_loc, name, args, None, None))

let get_left_of_string_name name param =
  let name_f s = s ^ "_of_string" in
  let args = List.map (fun s -> Tvar (dummy_loc, s)) param in
  get_reader_name ~name_f ~tagged:true
    (Name (dummy_loc, name, args, None, None))


let rec make_writer ~tagged deref (x : ob_mapping) : Indent.t list =
  let un = if tagged then "" else "untagged_" in
  match x with
    Unit _
  | Bool _
  | Int _
  | Float _
  | String _
  | Name _
  | External _
  | Tvar _ -> [ Line (get_writer_name ~tagged x) ]

  | Sum (_, a, Sum x, Sum) ->
      let tick = Ocaml.tick x in
      let match_ =
        [
          Line "match x with";
          Block (
            Array.to_list (
              Array.map
                (fun x -> Inline (make_variant_writer deref tick x))
                a
            )
          )
        ]
      in
      let body =
        if tagged then
          Line "Bi_io.write_tag ob Bi_io.variant_tag;" :: match_
        else
          match_
      in
      [
        Annot ("fun", Line "fun ob x ->");
        Block body;
      ]

  | Record (_, a, Record o, Record) ->
      let body = make_record_writer deref tagged a o in
      [
        Annot ("fun", Line "fun ob x ->");
        Block body;
      ]

  | Tuple (_, a, Tuple, Tuple) ->
      let main =
        let len = Array.length a in
        let a =
          Array.mapi (
            fun i x ->
              [
                Line "(";
                Block [
                  Line (sprintf "let %s = x in (" (Ox_emit.nth "x" i len));
                  Block (make_writer ~tagged:true deref x.cel_value);
                  Line ") ob x";
                ];
                Line ");"
              ]
          ) a
        in
        [
          Line (sprintf "Bi_vint.write_uvint ob %i;" len);
          Inline (List.flatten (Array.to_list a))
        ]
      in
      let body =
        if tagged then
          Line "Bi_io.write_tag ob Bi_io.tuple_tag;" :: main
        else
          main
      in
      [
        Annot ("fun", Line "fun ob x ->");
        Block body;
      ]

  | List (_, x, List o, List b) ->
      (match o, b with
         List, `Array ->
           let tag = get_biniou_tag x in
           [
             Line (sprintf "Atdgen_runtime.Ob_run.write_%slist" un);
             Block [
               Line tag;
               Line "(";
               Block (make_writer ~tagged:false deref x);
               Line ")";
             ]
           ]
       | Array, `Array ->
           let tag = get_biniou_tag x in
           [
             Line (sprintf "Atdgen_runtime.Ob_run.write_%sarray" un);
             Block [
               Line tag;
               Line "(";
               Block (make_writer ~tagged deref x);
               Line ")";
             ]
           ]
       | list_kind, `Table ->
           let body = make_table_writer deref tagged list_kind x in
           [
             Annot ("fun", Line "fun ob x ->");
             Block body;
           ]
      )

  | Option (_, x, Option, Option)
  | Nullable (_, x, Nullable, Nullable) ->
      [
        Line (sprintf "Atdgen_runtime.Ob_run.write_%soption (" un);
        Block (make_writer ~tagged:true deref x);
        Line ")";
      ]

  | Wrap (_, x, Wrap o, Wrap) ->
      let simple_writer = make_writer ~tagged deref x in
      (match o with
         None -> simple_writer
       | Some { Ocaml.ocaml_unwrap; _ } ->
           [
             Line "fun ob x -> (";
             Block [
               Line (sprintf "let x = ( %s ) x in (" ocaml_unwrap);
               Block simple_writer;
               Line ") ob x)";
             ]
           ]
      )

  | _ -> assert false



and make_variant_writer deref tick x : Indent.t list =
  let o =
    match x.var_arepr, x.var_brepr with
      Variant o, Variant -> o
    | _ -> assert false
  in
  let ocaml_cons = o.Ocaml.ocaml_cons in
  match x.var_arg with
    None ->
      let h = Bi_io.string_of_hashtag (Bi_io.hash_name x.var_cons) false in
      [ Line (sprintf "| %s%s -> Bi_outbuf.add_char4 ob %C %C %C %C"
                tick ocaml_cons
                h.[0] h.[1] h.[2] h.[3]) ]
  | Some v ->
      let h = Bi_io.string_of_hashtag (Bi_io.hash_name x.var_cons) true in
      [
        Line (sprintf "| %s%s x ->" tick ocaml_cons);
        Block [
          Line (sprintf "Bi_outbuf.add_char4 ob %C %C %C %C;"
                  h.[0] h.[1] h.[2] h.[3]);
          Line "(";
          Block (make_writer ~tagged:true deref v);
          Line ") ob x"
        ]
      ]

and make_record_writer deref tagged a record_kind =
  let dot = Ocaml.dot record_kind in
  let fields = get_fields deref a in
  let write_length =
    (* count the number of defined optional fields in order
       to determine the length of the record *)
    let min_len =
      List.fold_left
        (fun n (_, _, _, opt, _) -> if opt then n else n + 1) 0 fields
    in
    let max_len = List.length fields in
    if min_len = max_len then
      [ Line (sprintf "Bi_vint.write_uvint ob %i;" max_len) ]
    else
      [
        (* Using a ref because many "let len = ... len + 1 in"
           cause ocamlopt to take a very long time to finish *)
        Line (sprintf "let len = ref %i in" min_len);
        Inline (
          List.fold_right (
            fun (_, ocaml_fname, default, opt, _) l ->
              if opt then
                let getfield =
                  sprintf "let x_%s = x%s%s in" ocaml_fname dot ocaml_fname in
                let setlen =
                  sprintf "if x_%s != %s then incr len;"
                    ocaml_fname (Option.value_exn default)
                in
                Line getfield :: Line setlen :: l
              else l
          ) fields []
        );
        Line "Bi_vint.write_uvint ob !len;"
      ]
  in

  let write_fields =
    List.concat_map (fun (x, ocaml_fname, ocaml_default, optional, unwrapped) ->
      let f_value =
        if unwrapped then Ocaml.unwrap_option (deref x.f_value)
        else x.f_value
      in
      let write_field_tag =
        let s = Bi_io.string_of_hashtag (Bi_io.hash_name x.f_name) true in
        sprintf "Bi_outbuf.add_char4 ob %C %C %C %C;"
          s.[0] s.[1] s.[2] s.[3]
      in
      let app v =
        [
          Line write_field_tag;
          Line "(";
          Block (make_writer ~tagged:true deref f_value);
          Line (sprintf ") ob %s;" v);
        ]
      in
      let v =
        if optional then
          sprintf "x_%s" ocaml_fname
        else
          sprintf "x%s%s" dot ocaml_fname
      in
      if unwrapped then
        [
          Line (sprintf "(match %s with None -> () | Some x ->" v);
          Block (app "x");
          Line ");"
        ]
      else if optional then
        [
          Line (sprintf "if %s != %s then (" v (Option.value_exn ocaml_default));
          Block (app v);
          Line ");"
        ]
      else
        app v
    ) fields in

  let main = write_length @ write_fields in

  if tagged then
    Line "Bi_io.write_tag ob Bi_io.record_tag;" :: main
  else
    main



and make_table_writer deref tagged list_kind x =
  let a, record_kind =
    match deref x with
      Record (_, a, Record record_kind, Record) -> a, record_kind
    | _ ->
        Error.error (loc_of_mapping x) "Not a record type"
  in
  let dot = Ocaml.dot record_kind in
  let let_len =
    match list_kind with
      List -> Line "let len = List.length x in"
    | Array -> Line "let len = Array.length x in"
  in
  let iter2 =
    match list_kind with
      List -> "Atdgen_runtime.Ob_run.list_iter2"
    | Array -> "Atdgen_runtime.Ob_run.array_iter2"
  in
  let l = Array.to_list a in
  let write_header =
    Line (sprintf "Bi_vint.write_uvint ob %i;" (Array.length a)) ::
    List.concat_map (
      fun x ->
        [ Line (sprintf "Bi_io.write_hashtag ob (%i) true;"
                  (Bi_io.hash_name x.f_name));
          Line (sprintf "Bi_io.write_tag ob %s;"
                  (get_biniou_tag x.f_value)) ]
    ) l
  in
  let write_record =
    List.concat_map (fun x ->
      [ Line "(";
        Block (make_writer ~tagged:false deref x.f_value);
        Line ")";
        Block [ Line (sprintf "ob x%s%s;" dot x.f_name) ] ]
    ) l
  in
  let write_items =
    [ Line (iter2 ^ " (fun ob x ->");
      Block write_record;
      Line ") ob x;" ]
  in
  let main =
    [
      let_len;
      Line "Bi_vint.write_uvint ob len;";
      Line "if len > 0 then (";
      Block (write_header @ write_items);
      Line ");"
    ]
  in
  if tagged then
    Line "Bi_io.write_tag ob Bi_io.table_tag;" :: main
  else
    main


let study_record ~ocaml_version fields =
  let field_assignments =
    List.fold_right (
      fun (_, name, default, opt, _) field_assignments ->
        let v =
          match default with
            None ->
              assert (not opt);
              begin match ocaml_version with
                | Some (maj, min) when (maj > 4 || maj = 4 && min >= 3) ->
                    "Obj.magic (Sys.opaque_identity 0.0)"
                | _ -> "Obj.magic 0.0"
              end
          | Some s ->
              s
        in
        let init = Line (sprintf "let field_%s = ref (%s) in" name v) in
        let create = Line (sprintf "%s = !field_%s;" name name) in
        (init, create) :: field_assignments
    ) fields []
  in
  let init_fields, create_record_fields = List.split field_assignments in
  let n, mapping =
    List.fold_left (
      fun (i, acc) (_, _, _, opt, _) ->
        if not opt then
          (i+1, (Some i :: acc))
        else
          (i, (None :: acc))
    ) (0, []) fields
  in
  let mapping = Array.of_list (List.rev mapping) in

  let create_record = [ Line "{"; Block create_record_fields; Line "}" ] in

  let k = n / 31 + (if n mod 31 > 0 then 1 else 0) in
  let init_bits =
    List.init k (fun i -> Line (sprintf "let bits%i = ref 0 in" i)) in
  let final_bits = Array.make k 0 in
  for z0 = 0 to List.length fields - 1 do
    match mapping.(z0) with
      None -> ()
    | Some z ->
        let i = z / 31 in
        let j = z mod 31 in
        final_bits.(i) <- final_bits.(i) lor (1 lsl j);
  done;
  let set_bit z0 =
    match mapping.(z0) with
      None -> []
    | Some z ->
        let i = z / 31 in
        let j = z mod 31 in
        [ Line (sprintf "bits%i := !bits%i lor 0x%x;" i i (1 lsl j)) ]
  in
  let check_bits =
    let bool_expr =
      Array.mapi (fun i x -> sprintf "!bits%i <> 0x%x" i x) final_bits
      |> Array.to_list
      |> String.concat " || "
    in
    let bit_fields =
      let a = Array.init k (fun i -> sprintf "!bits%i" i) in
      sprintf "[| %s |]" (String.concat "; " (Array.to_list a))
    in
    let field_names =
      let l =
        List.fold_right (
          fun (x, _, _, opt, _) acc ->
            if not opt then
              sprintf "%S" x.f_name :: acc
            else
              acc
        ) fields []
      in
      sprintf "[| %s |]" (String.concat "; " l)
    in
    if k = 0 then []
    else
      [ Line (sprintf "if %s then Atdgen_runtime.Ob_run.missing_fields %s %s;"
                bool_expr bit_fields field_names) ]
  in
  init_fields, init_bits, set_bit, check_bits, create_record


let wrap_body ~tagged expected_tag body =
  if tagged then
    [
      Annot ("fun", Line "fun ib ->");
      Block [
        Line (sprintf "if Bi_io.read_tag ib <> %i then \
                       Atdgen_runtime.Ob_run.read_error_at ib;"
                expected_tag);
        Inline body;
      ]
    ]
  else
    [
      Annot ("fun", Line "fun tag ->");
      Block [
        Line (sprintf "if tag <> %i then \
                       Atdgen_runtime.Ob_run.read_error () else"
                expected_tag);
        Block [
          Line "fun ib ->";
          Block body;
        ]
      ]
    ]

let wrap_bodies ~tagged l =
  if tagged then
    let cases =
      List.map (
        fun (expected_tag, body) ->
          Inline [
            Line (sprintf "| %i -> " expected_tag);
            Block body;
          ]
      ) l
    in
    [
      Line "fun ib ->";
      Block [
        Line "match Bi_io.read_tag ib with";
        Block [
          Inline cases;
          Line "| _ -> Atdgen_runtime.Ob_run.read_error_at ib"
        ]
      ]
    ]
  else
    let cases =
      List.map (
        fun (expected_tag, body) ->
          Inline [
            Line (sprintf "| %i -> " expected_tag);
            Block [
              Line "(fun ib ->";
              Block body;
              Line ")";
            ]
          ]
      ) l
    in
    [
      Line "function";
      Block [
        Inline cases;
        Line "| _ -> Atdgen_runtime.Ob_run.read_error ()"
      ]
    ]


let rec make_reader
    deref ~tagged ~ocaml_version ?type_annot (x : ob_mapping)
  : Indent.t list =
  match x with
    Unit _
  | Bool _
  | Int _
  | Float _
  | String _
  | Name _
  | External _
  | Tvar _ -> [ Line (get_reader_name ~tagged x) ]

  | Sum (_, a, Sum x, Sum) ->
      let tick = Ocaml.tick x in
      let body =
        [
          Line "Bi_io.read_hashtag ib (fun ib h has_arg ->";
          Block [
            Line "match h, has_arg with";
            Block [
              Inline (
                Array.to_list (
                  Array.map
                    (fun x ->
                       Inline (make_variant_reader ~ocaml_version
                                 deref type_annot tick x)
                    )
                    a
                )
              );
              Line "| _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg";
            ]
          ];
          Line ")"
        ]
      in
      wrap_body ~tagged Bi_io.variant_tag body

  | Record (loc, a, Record o, Record) ->
      Ocaml.obj_unimplemented loc o;
      let body = make_record_reader deref ~ocaml_version type_annot a in
      wrap_body ~tagged Bi_io.record_tag body

  | Tuple (_, a, Tuple, Tuple) ->
      let body = make_tuple_reader deref ~ocaml_version a in
      wrap_body ~tagged Bi_io.tuple_tag body

  | List (loc, x, List o, List b) ->
      (match o, b with
         List, `Array ->
           let f =
             if tagged then "Atdgen_runtime.Ob_run.read_list"
             else "Atdgen_runtime.Ob_run.get_list_reader"
           in
           [
             Line (f ^ " (");
             Block (make_reader deref ~ocaml_version ~tagged:false x);
             Line ")";
           ]
       | Array, `Array ->
           let f =
             if tagged then "Atdgen_runtime.Ob_run.read_array"
             else "Atdgen_runtime.Ob_run.get_array_reader"
           in
           [
             Line (f ^ " (");
             Block (make_reader deref ~ocaml_version ~tagged:false x);
             Line ")";
           ]
       | list_kind, `Table ->
           (* Support table format and regular array format *)
           let body1 =
             make_table_reader ~ocaml_version deref loc list_kind x in
           let body2 =
             let f =
               match list_kind with
                 List -> "Atdgen_runtime.Ob_run.read_list_value"
               | Array -> "Atdgen_runtime.Ob_run.read_array_value"
             in
             [
               Line (f ^ " (");
               Block (make_reader deref ~tagged:false ~ocaml_version x);
               Line ") ib";
             ]
           in
           wrap_bodies ~tagged [ Bi_io.table_tag, body1;
                                 Bi_io.array_tag, body2 ]
      )

  | Option (_, x, Option, Option)
  | Nullable (_, x, Nullable, Nullable) ->
      let body = [
        Line "match Char.code (Bi_inbuf.read_char ib) with";
        Block [
          Line "| 0 -> None";
          Line "| 0x80 ->";
          Block [
            Line "Some (";
            Block [
              Line "(";
              Block (make_reader deref ~tagged:true ~ocaml_version x);
              Line ")";
              Block [ Line "ib"];
            ];
            Line ")"
          ];
          Line "| _ -> Atdgen_runtime.Ob_run.read_error_at ib";
        ]
      ]
      in
      wrap_body ~tagged Bi_io.num_variant_tag body

  | Wrap (_, x, Wrap o, Wrap) ->
      let simple_reader = make_reader deref ~tagged ~ocaml_version x in
      (match o with
         None -> simple_reader
       | Some { Ocaml.ocaml_wrap ; _ } ->
           if tagged then
             [
               Line "fun ib ->";
               Block [
                 Line (sprintf "( %s ) ((" ocaml_wrap);
                 Block simple_reader;
                 Line ") ib)";
               ]
             ]
           else
             [
               Line "fun tag ib ->";
               Block [
                 Line (sprintf "( %s ) ((" ocaml_wrap);
                 Block simple_reader;
                 Line ") tag ib)";
               ]
             ]
      )
  | _ -> assert false


and make_variant_reader ~ocaml_version deref type_annot tick x : Indent.t list =
  let o =
    match x.var_arepr, x.var_brepr with
      Variant o, Variant -> o
    | _ -> assert false
  in
  let ocaml_cons = o.Ocaml.ocaml_cons in
  match x.var_arg with
    None ->
      let h = Bi_io.hash_name x.var_cons in
      let typed_cons = Ox_emit.opt_annot type_annot (tick ^ ocaml_cons) in
      [ Line (sprintf "| %i, false -> %s" h typed_cons) ]
  | Some v ->
      let h = Bi_io.hash_name x.var_cons in
      [
        Line (sprintf "| %i, true -> (%s%s (" h tick ocaml_cons);
        Block [
          Block [
            Line "(";
            Block (make_reader deref ~tagged:true ~ocaml_version v);
            Line ") ib";
          ];
          Line (sprintf ")%s)" (Ox_emit.insert_annot type_annot));
        ];
      ]

and make_record_reader
    deref ~ocaml_version type_annot
    a =
  let fields = get_fields deref a in
  let init_fields, init_bits, set_bit, check_bits, create_record =
    study_record ~ocaml_version fields
  in

  let body =
    let a = Array.of_list fields in
    let cases =
      Array.mapi (
        fun i (x, name, _, _, unwrapped) ->
          let f_value =
            if unwrapped then Ocaml.unwrap_option (deref x.f_value)
            else x.f_value
          in
          let wrap l =
            if unwrapped then
              [
                Line "Some (";
                Block l;
                Line ")"
              ]
            else l
          in
          let read_value =
            [
              Line "(";
              Block (make_reader deref ~tagged:true ~ocaml_version f_value);
              Line ") ib"
            ]
          in
          Inline [
            Line (sprintf "| %i ->" (Bi_io.hash_name x.f_name));
            Block [
              Line (sprintf "field_%s := (" name);
              Block (wrap read_value);
              Line ");";
              Inline (set_bit i);
            ];
          ]
      ) a
    in
    [
      Line "match Bi_io.read_field_hashtag ib with";
      Block [
        Inline (Array.to_list cases);
        Line "| _ -> Bi_io.skip ib";
      ]
    ]
  in

  [
    Inline init_fields;
    Inline init_bits;
    Line "let len = Bi_vint.read_uvint ib in";
    Line "for i = 1 to len do";
    Block body;
    Line "done;";
    Inline check_bits;
    Line "(";
    Block create_record;
    Line (sprintf "%s)" (Ox_emit.insert_annot type_annot));
  ]


and make_tuple_reader deref ~ocaml_version a =
  let cells =
    Array.map (
      fun x ->
        match x.cel_arepr with
          Ocaml.Repr.Cell f -> x, f.Ocaml.ocaml_default
        | _ -> assert false
    ) a
  in
  let min_length =
    let n = ref (Array.length cells) in
    (try
       for i = Array.length cells - 1 downto 0 do
         let _, default = cells.(i) in
         if default = None then (
           n := i + 1;
           raise Exit
         )
       done
     with Exit -> ());
    !n
  in
  let tup_len = Array.length a in

  let read_cells =
    List.flatten (
      Array.to_list (
        Array.mapi (
          fun i (x, default) ->
            let read_value =
              make_reader deref ~ocaml_version ~tagged:true
                x.cel_value in
            let get_value =
              if i < min_length then
                [
                  Line "(";
                  Block read_value;
                  Line ") ib";
                ]
              else
                [
                  Line (sprintf "if len >= %i then (" (i+1));
                  Block read_value;
                  Line ") ib";
                  Line "else";
                  Block [
                    Line
                      (match default with None -> assert false | Some s -> s)
                  ]
                ]
            in
            [
              Line (sprintf "let x%i =" i);
              Block get_value;
              Line "in"
            ]
        ) cells
      )
    )
  in

  let make_tuple =
    sprintf "(%s)"
      (String.concat ", "
         (Array.to_list (Array.mapi (fun i _ -> sprintf "x%i" i) a)))
  in
  let req_fields =
    let acc = ref [] in
    for i = Array.length cells - 1 downto 0 do
      let _, default = cells.(i) in
      if default = None then
        acc := string_of_int i :: !acc
    done;
    sprintf "[ %s ]" (String.concat "; " !acc)
  in
  [
    Line "let len = Bi_vint.read_uvint ib in";
    Line (sprintf
            "if len < %i then Atdgen_runtime.Ob_run.missing_tuple_fields len %s;"
            min_length req_fields);
    Inline read_cells;
    Line (sprintf "for i = %i to len - 1 do Bi_io.skip ib done;" tup_len);
    Line make_tuple
  ]


and make_table_reader deref ~ocaml_version loc list_kind x =
  let empty_list, to_list =
    match list_kind with
      List -> "[ ]", (fun s -> "Array.to_list " ^ s)
    | Array -> "[| |]", (fun s -> s)
  in
  let fields =
    match deref x with
      Record (loc, a, Record o, Record) ->
        Ocaml.obj_unimplemented loc o;
        get_fields deref a
    | _ ->
        Error.error loc "Not a list or array of records"
  in
  let init_fields, init_bits, set_bit, check_bits, create_record =
    study_record ~ocaml_version fields
  in
  let cases =
    Array.to_list (
      Array.mapi (
        fun i (x, name, _, _, _) ->
          Inline [
            Line (sprintf "| %i ->" (Bi_io.hash_name x.f_name));
            Block [
              Inline (set_bit i);
              Line "let read =";
              Block [
                Line "(";
                Block (make_reader deref ~tagged:false ~ocaml_version x.f_value);
                Line ")";
                Block [ Line "tag" ]
              ];
              Line "in";
              Line (sprintf "(fun ib -> field_%s := read ib)" name);
            ]
          ]
      ) (Array.of_list fields)
    )
  in
  [
    Line "let row_num = Bi_vint.read_uvint ib in";
    Line ("if row_num = 0 then " ^ empty_list);
    Line "else";
    Block [
      Line "let col_num = Bi_vint.read_uvint ib in";
      Inline init_fields;
      Inline init_bits;
      Line "let readers =";
      Block [
        Line "Atdgen_runtime.Ob_run.array_init2 col_num ib (";
        Block [
          Line "fun col ib ->";
          Block [
            Line "let h = Bi_io.read_field_hashtag ib in";
            Line "let tag = Bi_io.read_tag ib in";
            Line "match h with";
            Block cases;
            Block [ Line "| _ -> (fun ib -> Bi_io.skip ib)" ]
          ]
        ];
        Line ")";
      ];
      Line "in";
      Inline check_bits;
      Line "let a = Array.make row_num (Obj.magic 0) in";
      Line "for row = 0 to row_num - 1 do";
      Block [
        Line "for i = 0 to Array.length readers - 1 do";
        Block [ Line "readers.(i) ib" ];
        Line "done;";
        Line "a.(row) <-";
        Block create_record;
      ];
      Line "done;";
      Line (to_list "a")
    ]
  ]

let make_ocaml_biniou_writer ~original_types deref is_rec let1 let2 def =
  let x = Option.value_exn def.def_value in
  let name = def.def_name in
  let type_constraint = Ox_emit.get_type_constraint ~original_types def in
  let param = def.def_param in
  let tag = get_biniou_tag (deref x) in
  let write_untagged = get_left_writer_name ~tagged:false name param in
  let write = get_left_writer_name ~tagged:true name param in
  let to_string = get_left_to_string_name name param in
  let write_untagged_expr = make_writer deref ~tagged:false x in
  let eta_expand = is_rec && not (Ox_emit.is_lambda write_untagged_expr) in
  let needs_annot = Ox_emit.needs_type_annot x in
  let extra_param, extra_args =
    match eta_expand, needs_annot with
    | true, false -> " ob x", " ob x"
    | true, true -> sprintf " ob (x : %s)" type_constraint, " ob x"
    | false, false -> "", ""
    | false, true -> "", ""
  in
  let type_annot =
    match Ox_emit.needs_type_annot x with
    | true -> Some (sprintf "Bi_outbuf.t -> %s -> unit" type_constraint)
    | false -> None
  in
  [
    Line (sprintf "%s %s_tag = %s" let1 name tag);
    Line (sprintf "%s %s = ("
            let2
            (Ox_emit.opt_annot_def type_annot
               (write_untagged ^ extra_param)));
    Block (List.map Indent.strip write_untagged_expr);
    Line (sprintf ")%s" extra_args);
    Line (sprintf "%s %s ob x =" let2 write);
    Block [
      Line (sprintf "Bi_io.write_tag ob %s;" tag);
      Line (sprintf "%s ob x" write_untagged);
    ];
    Line (sprintf "%s %s ?(len = 1024) x =" let2 to_string);
    Block [
      Line "let ob = Bi_outbuf.create len in";
      Line (sprintf "%s ob x;" write);
      Line "Bi_outbuf.contents ob"
    ]
  ]

let make_ocaml_biniou_reader ~original_types ~ocaml_version
    deref is_rec let1 let2 def =
  let x = Option.value_exn def.def_value in
  let name = def.def_name in
  let type_constraint = Ox_emit.get_type_constraint ~original_types def in
  let param = def.def_param in
  let get_reader = get_left_reader_name ~tagged:false name param in
  let read = get_left_reader_name ~tagged:true name param in
  let of_string = get_left_of_string_name name param in
  let type_annot =
    match Ox_emit.needs_type_annot x with
    | true -> Some type_constraint
    | false -> None
  in
  let get_reader_expr =
    make_reader deref ~tagged:false ~ocaml_version ?type_annot x in
  let read_expr = make_reader deref ~tagged:true ~ocaml_version ?type_annot x in
  let eta_expand1 = is_rec && not (Ox_emit.is_lambda get_reader_expr) in
  let eta_expand2 = is_rec && not (Ox_emit.is_lambda read_expr) in
  let extra_param1, extra_args1 =
    if eta_expand1 then " tag", " tag"
    else "", ""
  in
  let extra_param2, extra_args2 =
    if eta_expand2 then " ib", " ib"
    else "", ""
  in
  [
    Line (sprintf "%s %s%s = (" let1 get_reader extra_param1);
    Block (List.map Indent.strip get_reader_expr);
    Line (sprintf ")%s" extra_args1);
    Line (sprintf "%s %s%s = (" let2 read extra_param2);
    Block (List.map Indent.strip read_expr);
    Line (sprintf ")%s" extra_args2);
    Line (sprintf "%s %s ?pos s =" let2 of_string);
    Block [
      Line (sprintf "%s (Bi_inbuf.from_string ?pos s)" read)
    ]
  ]

let make_ocaml_biniou_impl ~with_create ~original_types ~ocaml_version
    buf deref defs =
  defs
  |> List.concat_map (fun (is_rec, l) ->
    let l = List.filter (fun x -> x.def_value <> None) l in
    let writers =
      List.map_first (fun ~is_first def ->
          let let1, let2 = Ox_emit.get_let ~is_rec ~is_first in
          make_ocaml_biniou_writer
            ~original_types deref is_rec let1 let2 def
      ) l
    in
    let readers =
      List.map_first (fun ~is_first def ->
          let let1, let2 = Ox_emit.get_let ~is_rec ~is_first in
          make_ocaml_biniou_reader ~ocaml_version
            ~original_types deref is_rec let1 let2 def
      ) l
    in
    List.flatten (writers @ readers))
  |> Indent.to_buffer buf;
  Ox_emit.maybe_write_creator_impl ~with_create deref buf defs


(*
  Glue
*)

let make_mli
    ~header ~opens ~with_typedefs ~with_create ~with_fundefs
    ocaml_typedefs deref defs =
  let buf = Buffer.create 1000 in
  bprintf buf "%s\n" header;
  Ox_emit.write_opens buf opens;
  if with_typedefs then
    bprintf buf "%s\n" ocaml_typedefs;
  if with_typedefs && with_fundefs then
    bprintf buf "\n";
  if with_fundefs then
    make_ocaml_biniou_intf ~with_create buf deref defs;
  Buffer.contents buf

let make_ml
    ~header ~opens ~with_typedefs ~with_create ~with_fundefs ~original_types
    ~ocaml_version
    ocaml_typedefs deref defs =
  let buf = Buffer.create 1000 in
  bprintf buf "%s\n" header;
  Ox_emit.write_opens buf opens;
  if with_typedefs then
    bprintf buf "%s\n" ocaml_typedefs;
  if with_typedefs && with_fundefs then
    bprintf buf "\n";
  if with_fundefs then
    make_ocaml_biniou_impl
      ~with_create ~original_types ~ocaml_version
      buf deref defs;
  Buffer.contents buf

let make_ocaml_files
    ~opens
    ~with_typedefs
    ~with_create
    ~with_fundefs
    ~all_rec
    ~pos_fname
    ~pos_lnum
    ~type_aliases
    ~force_defaults:_
    ~ocaml_version
    ~pp_convs
    atd_file out =
  let ((head, m0), _) =
    match atd_file with
      Some file ->
        Atd.Util.load_file
          ~expand:false ~inherit_fields:true ~inherit_variants:true
          ?pos_fname ?pos_lnum
          file
    | None ->
        Atd.Util.read_channel
          ~expand:false ~inherit_fields:true ~inherit_variants:true
          ?pos_fname ?pos_lnum
          stdin
  in
  let tsort =
    if all_rec then
      function m -> [ (true, m) ]
    else
      Atd.Util.tsort
  in
  let m1 = tsort m0 in
  let defs1 = defs_of_atd_modules m1 in
  Xb_emit.check defs1;
  let (m1', original_types) =
    Atd.Expand.expand_module_body ~keep_poly:true m0
  in
  let m2 = tsort m1' in
  (* m0 = original type definitions
     m1 = original type definitions after dependency analysis
     m2 = monomorphic type definitions after dependency analysis *)
  let ocaml_typedefs =
    Ocaml.ocaml_of_atd ~pp_convs ~target:Biniou ~type_aliases (head, m1) in
  let defs = defs_of_atd_modules m2 in
  let header =
    let src =
      match atd_file with
        None -> "stdin"
      | Some path -> sprintf "%S" (Filename.basename path)
    in
    sprintf {|(* Auto-generated from %s *)
              [@@@ocaml.warning "-27-32-35-39"]|} src
  in
  let mli =
    make_mli ~header ~opens ~with_typedefs ~with_create ~with_fundefs
      ocaml_typedefs (Mapping.make_deref defs1) defs1
  in
  let ml =
    make_ml ~header ~opens ~with_typedefs ~with_create ~with_fundefs
      ~original_types ~ocaml_version ocaml_typedefs
      (Mapping.make_deref defs) defs
  in
  Ox_emit.write_ocaml out mli ml
