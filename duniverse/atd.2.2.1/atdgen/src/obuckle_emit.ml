open Atd.Import
open Indent

type param =
  { deref
    : (Ocaml.Repr.t, Json.json_repr) Mapping.mapping
      -> (Ocaml.Repr.t, Json.json_repr) Mapping.mapping;
  }

let target : Ocaml.target = Bucklescript

let open_enum_not_supported () =
  failwith "open_enum is not supported in bucklescript mode"

let runtime_module = "Atdgen_codec_runtime"

let decoder_ident = sprintf "%s.Decode.%s" runtime_module

let encoder_ident = sprintf "%s.Encode.%s" runtime_module

let decoder_make = decoder_ident "make"

let encoder_make = encoder_ident "make"

let decoder_t s = sprintf "%s %s" s (decoder_ident "t")

let encoder_t s = sprintf "%s %s" s (encoder_ident "t")

let make_json_string s = Yojson.Safe.to_string (`String s)

let type_annot_str = Option.value ~default:"_"

let destruct_sum (x : Oj_mapping.t) =
  let open Mapping in
  match x with
  | Sum (_, a, Sum x, Sum j) ->
      let tick = Ocaml.tick x in
      if j.json_open_enum then open_enum_not_supported ();
      tick, a
  | Unit _ -> Error.error (loc_of_mapping x) "Cannot destruct unit"
  | Bool _ -> Error.error (loc_of_mapping x) "Cannot destruct bool"
  | Int _ -> Error.error (loc_of_mapping x) "Cannot destruct int"
  | Float _ -> Error.error (loc_of_mapping x) "Cannot destruct float"
  | String _ -> Error.error (loc_of_mapping x) "Cannot destruct string"
  | Name (_,name,_,_,_) ->
      Error.error (loc_of_mapping x) ("Cannot destruct name " ^ name)
  | External _ -> Error.error (loc_of_mapping x) "Cannot destruct external"
  | Tvar _ -> Error.error (loc_of_mapping x) "Cannot destruct tvar"
  | Record _ -> Error.error (loc_of_mapping x) "Cannot destruct record"
  | Tuple _ -> Error.error (loc_of_mapping x) "Cannot destruct tuple"
  | List _ -> Error.error (loc_of_mapping x) "Cannot destruct list"
  | Option _ -> Error.error (loc_of_mapping x) "Cannot destruct option"
  | Nullable _ -> Error.error (loc_of_mapping x) "Cannot destruct nullable"
  | Wrap _ -> Error.error (loc_of_mapping x) "Cannot destruct wrap"
  | _ -> Error.error (loc_of_mapping x) "Cannot destruct unknown type"


let make_ocaml_bs_intf ~with_create buf deref defs =
  List.concat_map snd defs
  |> List.filter Ox_emit.include_intf
  |> List.iter (fun (x : (_, _) Mapping.def) ->
    let s = x.def_name in
    let full_name = Ox_emit.get_full_type_name x in
    let params t =
      String.concat " " (
        List.map (fun s -> sprintf "%s ->" (t ("'" ^ s))) x.def_param
      ) in
    let read_params = params decoder_t in
    let write_params = params encoder_t in
    bprintf buf "val read_%s : %s %s\n\n"
      s read_params (decoder_t full_name);
    bprintf buf "val write_%s : %s %s\n\n"
      s write_params (encoder_t full_name);
    Ox_emit.maybe_write_creator_intf ~with_create deref buf x
  )

let unwrap_f_value { Ox_emit.mapping; unwrapped; _} (p : param) =
  if unwrapped then
    Ocaml.unwrap_option (p.deref mapping.f_value)
  else
    mapping.f_value

let rec get_reader_name
    ?(paren = false)
    ?(name_f = fun s -> "read_" ^ s)
    p (x : Oj_mapping.t) : string =
  match x with
    Unit (_, Unit, Unit) -> decoder_ident "unit"
  | Bool (_, Bool, Bool) -> decoder_ident "bool"
  | Int (_, Int o, Int) ->
      decoder_ident (
        match o with
        | Int -> "int"
        | Char ->  "char"
        | Int32 -> "int32"
        | Int64 -> "int64"
        | Float -> "float"
      )
  | Float (_, Float, Float _) -> decoder_ident "float"
  | String (_, String, String) -> decoder_ident "string"
  | Tvar (_, s) -> "read_" ^ Ox_emit.name_of_var s

  | Name (_, s, args, None, None) ->
      let l = List.map (get_reader_name ~paren:true p) args in
      let s = String.concat " " (name_f s :: l) in
      if paren && l <> [] then "(" ^ s ^ ")"
      else s

  | External (_, _, args,
              External (_, main_module, ext_name),
              External) ->
      let f = main_module ^ "." ^ name_f ext_name in
      let l = List.map (get_reader_name ~paren:true p) args in
      let s = String.concat " " (f :: l) in
      if paren && l <> [] then "(" ^ s ^ ")"
      else s

  | _ -> assert false

let read_with_adapter adapter reader =
  match adapter.Json.ocaml_adapter with
  | None -> reader
  | Some adapter_path ->
      let normalize =
        Oj_mapping.json_normalizer_of_adapter_path adapter_path
      in
      [
        Line (
          sprintf "%s %s (" (decoder_ident "adapter") normalize
        );
        Block reader;
        Line ")";
      ]

let rec make_reader ?type_annot p (x : Oj_mapping.t) : Indent.t list =
  match x with
    Unit _
  | Bool _
  | Int _
  | Float _
  | String _
  | Name _
  | External _
  | Tvar _ -> [ Indent.Line (get_reader_name p x) ]
  | Record (loc, a, Record o, Record j) ->
      let reader =
        [ Line (sprintf "%s (fun json ->" decoder_make)
        ; Block (make_record_reader ?type_annot p loc a j)
        ; Line ")"
        ]
      in
      let adapter = j.json_record_adapter in
      read_with_adapter adapter reader
  | Tuple (_, a, Tuple, Tuple) ->
      [ Line (decoder_ident (sprintf "tuple%d" (Array.length a)))
      ; Block (
          a
          |> Array.to_list
          |> List.map (fun (cm : (_, _) Mapping.cell_mapping) ->
            Block (make_reader p cm.cel_value)
            |> Indent.paren
          )
        )
      ]
  | List (loc, x, List o, List j) ->
      (match j with
         Array ->
           [ Line (sprintf "%s ("
                     (match o with
                      | List -> decoder_ident "list"
                      | Array -> decoder_ident "array"))
           ; Block (make_reader p x)
           ; Line ")"
           ]
       | Object ->
           let _k, v = Ox_emit.get_assoc_type p.deref loc x in (* TODO key wrap *)
           [ Line (sprintf "%s ("
                     (match o with
                      | List -> decoder_ident "obj_list"
                      | Array -> decoder_ident "obj_array"))
           ; Block (make_reader p v)
           ; Line ")"
           ]
      )
  | Sum (_, a, Sum osum, Sum j) ->
      if j.json_open_enum then open_enum_not_supported ();
      let cases =
        Array.to_list a
        |> List.map
          (fun (r : (Ocaml.Repr.t, Json.json_repr) Mapping.variant_mapping) ->
             let (o, j) =
               match r.var_arepr, r.var_brepr with
               | Ocaml.Repr.Variant o, Json.Variant j -> o, j
               | _ -> assert false in
             ( r.var_arg
             , j.json_cons
             , o.ocaml_cons
             )
          ) in
      let cases =
        let tick = Ocaml.tick osum in
        cases
        |> List.concat_map (fun (arg, j, o) ->
          let codec_cons =
            match arg with
            | None ->
                [Line (sprintf "`Single (%s%s)" tick o)]
            | Some v ->
                [ Line "`Decode ("
                ; Inline (make_reader p v)
                ; Line (
                    sprintf "|> %s (fun x -> ((%s%s x) : %s))"
                      (decoder_ident "map")
                      tick o (type_annot_str type_annot))
                ; Line ")"
                ]
          in
          [Block
             [ Line "("
             ; Line (sprintf "%S" j)
             ; Line ","
             ; Block codec_cons
             ; Line ")"
             ]
          ])
        |> Indent.concat (Line ";")
      in
      let standard_reader =
        [ Line (decoder_ident "enum")
        ; Line "["
        ; Block cases
        ; Line "]"
        ]
      in
      let adapter = j.json_sum_adapter in
      read_with_adapter adapter standard_reader
  | Wrap (_, x, Wrap o, Wrap) ->
      (match o with
       | None -> make_reader p x
       | Some w ->
           [ Line "("
           ; Block (make_reader p x)
           ; Line (sprintf ") |> (%s (%s))" (decoder_ident "map") w.ocaml_wrap)
           ])
  | Option (_, x, Option, Option) ->
      [ Line (sprintf "%s (" (decoder_ident "option_as_constr"))
      ; Block (make_reader p x)
      ; Line ")"
      ]
  | Nullable (_, x, Nullable, Nullable) ->
      [ Line (sprintf "%s (" (decoder_ident "nullable"))
      ; Block (make_reader p x)
      ; Line ")"
      ]
  | _ -> failwith "TODO: make reader"

and make_record_reader ?type_annot
    (p : param)
    _loc
    (a : (Ocaml.Repr.t, Json.json_repr) Mapping.field_mapping array)
    _json_options
  =
  let create_record =
    Ox_emit.get_fields p.deref a
    |> List.map (function ({ Ox_emit. mapping; ocaml_fname; json_fname
                           ; ocaml_default ; optional ; unwrapped
                           } as field) ->
        let f_value = unwrap_f_value field p in
        Block
          [ Line (sprintf "%s =" ocaml_fname)
          ; Block
              [ Line (decoder_ident "decode")
              ; Line "("
              ; Block
                  [ Inline (
                      make_reader p (
                        if unwrapped then
                          f_value
                        else
                          mapping.f_value
                      )
                    )
                  ; Line (
                      sprintf "|> %s"
                        (match unwrapped, optional, ocaml_default with
                         | true, true, Some _
                         | true, true, None ->
                             sprintf "%s \"%s\""
                               (decoder_ident "fieldOptional")
                               json_fname
                         | false, true, Some default
                         | false, false, Some default ->
                             sprintf "%s \"%s\" %s"
                               (decoder_ident "fieldDefault")
                               json_fname
                               default
                         | false, false, None
                         | true, false, None
                         | false, true, None ->
                             sprintf "%s \"%s\""
                               (decoder_ident "field")
                               json_fname
                         | true, false, _ -> assert false
                        )
                    )
                  ]
              ; Line ") json;"
              ]
          ])
  in
  [ Line "("
  ; Block
      [ Line "({"
      ; Block create_record
      ; Line (sprintf "} : %s)" (type_annot_str type_annot))
      ]
  ; Line ")"
  ]

let get_left_reader_name p name param =
  let args = List.map (fun s -> Mapping.Tvar (Atd.Ast.dummy_loc, s)) param in
  get_reader_name p (Mapping.Name (Atd.Ast.dummy_loc, name, args, None, None))

let make_ocaml_bs_reader p ~original_types is_rec let1 _let2
    (def : (_, _) Mapping.def) =
  let x = Option.value_exn def.def_value in
  let name = def.def_name in
  let param = def.def_param in
  let read = get_left_reader_name p name param in
  let type_annot =
    if Ox_emit.needs_type_annot x then (
      Some (Ox_emit.get_type_constraint ~original_types def)
    ) else (
      None
    )
  in
  let reader_expr = make_reader ?type_annot p x in
  let eta_expand = is_rec && not (Ox_emit.is_lambda reader_expr) in
  let extra_param, extra_args =
    if eta_expand then " js", " js"
    else "", ""
  in
  [
    Line (sprintf "%s %s%s = (" let1 read extra_param);
    Block (List.map Indent.strip reader_expr);
    Line (sprintf ")%s" extra_args);
  ]

let rec get_writer_name
    ?(paren = false)
    ?(name_f = fun s -> "write_" ^ s)
    (p : param) (x : Oj_mapping.t) : string =
  match x with
  | Unit (_, Ocaml.Repr.Unit, Unit) ->
      encoder_ident "unit"
  | Bool (_, Bool, Bool) ->
      encoder_ident "bool"
  | Int (_, Int o, Int) ->
      encoder_ident (
        match o with
        | Int -> "int"
        | Char ->  "char"
        | Int32 -> "int32"
        | Int64 -> "int64"
        | Float -> "float"
      )
  | Float (_, Float, Float j) ->
      encoder_ident (
        match j with
        | Float None -> "float"
        (* TODO *)
        | Float (Some _precision) -> sprintf "float"
        | Int -> "float"
      )

  | String (_, String, String) -> encoder_ident "string"

  | Tvar (_, s) -> "write_" ^ (Ox_emit.name_of_var s)

  | Name (_, s, args, None, None) ->
      let l = List.map (get_writer_name ~paren:true p) args in
      let s = String.concat " " (name_f s :: l) in
      if paren && l <> [] then "(" ^ s ^ ")"
      else s

  | External (_, _, args,
              External (_, main_module, ext_name),
              External) ->
      let f = main_module ^ "." ^ name_f ext_name in
      let l = List.map (get_writer_name ~paren:true p) args in
      let s = String.concat " " (f :: l) in
      if paren && l <> [] then "(" ^ s ^ ")"
      else s

  | _ -> assert false

let write_with_adapter adapter writer =
  match adapter.Json.ocaml_adapter with
  | None -> writer
  | Some adapter_path ->
      let restore = Oj_mapping.json_restorer_of_adapter_path adapter_path in
      [
        Line (
          sprintf "%s %s (" (encoder_ident "adapter") restore
        );
        Block writer;
        Line ")";
      ]

let get_left_writer_name p name param =
  let args = List.map (fun s -> Mapping.Tvar (Atd.Ast.dummy_loc, s)) param in
  get_writer_name p (Name (Atd.Ast.dummy_loc, name, args, None, None))

let rec make_writer ?type_annot p (x : Oj_mapping.t) : Indent.t list =
  match x with
    Unit _
  | Bool _
  | Int _
  | Float _
  | String _
  | Name _
  | External _
  | Tvar _ -> [ Line (get_writer_name p x) ]
  | Tuple (_, a, Tuple, Tuple) ->
      [ Line (encoder_ident (sprintf "tuple%d" (Array.length a)))
      ; Block (
          Array.to_list a
          |> List.map (fun (cm : (_, _) Mapping.cell_mapping) ->
            Block (make_writer p cm.cel_value)
            |> Indent.paren
          )
        )
      ]
  | List (loc, x, List o, List j) ->
      (match j with
         Array ->
           [ Line (sprintf "%s ("
                     (match o with
                      | List -> encoder_ident "list"
                      | Array -> encoder_ident "array"))
           ; Block (make_writer p x)
           ; Line ")"
           ]
       | Object ->
           let _k, v = Ox_emit.get_assoc_type p.deref loc x in
           [ Line (sprintf "%s (fun (t : %s) ->"
               encoder_make (type_annot_str type_annot))
           ; Block
               [ Line (sprintf "%s |>"
                         (match o with
                          | List -> "t"
                          | Array -> "Array.to_list t"))
               ; Line "List.map ("
               ; Block
                   [ Line "fun (key, value) ->"
                   ; Block
                       [ Line (encoder_ident "field")
                       ; Block
                           [ Line "("
                           ; Block (make_writer p v)
                           ; Line ")"
                           ]
                       ; Block
                           [ Line "~name:key" (* TODO unwrap keys? *)
                           ; Line "value";
                           ]
                       ]
                   ]
               ; Line ") |>"
               ; Line (encoder_ident "obj")
               ]
           ; Line ")"
           ]
      )
  | Record (_, a, Record o, Record j) ->
      let writer =
        [ Line (sprintf "%s (fun (t : %s) ->"
                  encoder_make (type_annot_str type_annot))
        ; Block (make_record_writer p a o)
        ; Line ")"
        ]
      in
      write_with_adapter j.json_record_adapter writer
  | Sum (_, _a, Sum _osum, Sum j) ->
      if j.json_open_enum then open_enum_not_supported ();
      let standard_writer = make_sum_writer ?type_annot p x in
      let adapter = j.json_sum_adapter in
      write_with_adapter adapter standard_writer

  | Wrap (_, x, Wrap o, Wrap) ->
      begin match o with
        | None -> make_writer p x
        | Some { Ocaml.ocaml_unwrap ; ocaml_wrap_t = _ ; ocaml_wrap = _ } ->
            [ Block (make_writer p x)
            ; Line (sprintf "|> %s (%s)" (encoder_ident "contramap")
                      ocaml_unwrap)
            ]
      end
  | Nullable (_, x, Nullable, Nullable) ->
      [ Line (sprintf "%s (" (encoder_ident "nullable"))
      ; Block (make_writer p x)
      ; Line ")"
      ]
  | Option (_, x, Option, Option) ->
      [ Line (sprintf "%s (" (encoder_ident "option_as_constr"))
      ; Block (make_writer p x)
      ; Line ")"
      ]
  | _ -> []

and make_record_writer p a _record_kind =
  let write_record =
    Ox_emit.get_fields p.deref a
    |> List.map
      (fun ({ Ox_emit. mapping; ocaml_fname; json_fname
            ; unwrapped ; optional ; ocaml_default } as field) ->
        let f_value = unwrap_f_value field p in
        Block
          [ Line (
              match unwrapped, optional, ocaml_default with
              | true, true, None
              | true, true, Some _ ->
                  encoder_ident "field_o"
              | false, false, Some default ->
                  encoder_ident (sprintf "field ~default:%s" default)
              | false, false, None
              | false, true, _ ->
                  encoder_ident "field"
              | true, false, _ ->
                  assert false
            )
          ; Block
              [ Line "("
              ; Inline (make_writer p (
                  if unwrapped then
                    f_value
                  else
                    mapping.f_value
                ))
              ; Line ")"
              ]
          ; Line (sprintf "~name:%S" json_fname)
          ; Line (sprintf "t.%s" ocaml_fname)
          ]
      )
    |> Indent.concat (Line ";") in
  [ Line "("
  ; Line (encoder_ident "obj")
  ; Block
      [ Line "["
      ; Block write_record
      ; Line "]"
      ]
  ; Line ")"
  ]

and make_sum_writer ?type_annot (p : param)
    (sum : (Ocaml.Repr.t, Json.json_repr) Mapping.mapping) =
  let tick, a = destruct_sum (p.deref sum) in
  let cases =
    a
    |> Array.map (
      fun (x : (Ocaml.Repr.t, Json.json_repr) Mapping.variant_mapping) ->
        let o, j =
          match x.var_arepr, x.var_brepr with
          | Ocaml.Repr.Variant o, Json.Variant j -> o, j
          | _ -> assert false in
        let ocaml_cons = o.Ocaml.ocaml_cons in
        let json_cons = j.Json.json_cons in
        Inline (
          begin match x.var_arg with
            | None ->
                [ Line (sprintf "| %s%s ->" tick ocaml_cons)
                ; Line (sprintf "%s %s" (encoder_ident "constr0")
                          (make_json_string json_cons))
                ]
            | Some v ->
                [ Line (sprintf "| %s%s x ->" tick ocaml_cons)
                ; Line (sprintf "%s %s (" (encoder_ident "constr1")
                          (make_json_string json_cons))
                ; Block (make_writer p v)
                ; Line ") x"
                ]
          end)
    )
    |> Array.to_list
  in
  [ Line (sprintf "%s (fun (x : %s) -> match x with"
            (encoder_ident "make") (type_annot_str type_annot))
  ; Block cases
  ; Line ")"]

let make_ocaml_bs_writer p ~original_types is_rec let1 _let2
    (def : (_, _) Mapping.def) =
  let x = Option.value_exn def.def_value in
  let name = def.def_name in
  let type_annot =
    if Ox_emit.needs_type_annot x then (
      Some (Ox_emit.get_type_constraint ~original_types def)
    ) else (
      None
    )
  in
  let param = def.def_param in
  let write = get_left_writer_name p name param in
  let writer_expr = make_writer ?type_annot p x in
  let eta_expand = is_rec && not (Ox_emit.is_lambda writer_expr) in
  let extra_param, extra_args =
    if eta_expand then " js", " js"
    else "", ""
  in
  [
    Line (sprintf "%s %s%s = (" let1 write extra_param);
    Block (List.map Indent.strip writer_expr);
    Line (sprintf ")%s" extra_args);
  ]

let make_ocaml_bs_impl
    ~with_create
    ~original_types
    buf deref defs =
  let p = {deref = deref;} in
  defs
  |> List.concat_map (fun (is_rec, l) ->
    let l = List.filter
        (fun (x : (Ocaml.Repr.t, Json.json_repr) Mapping.def) ->
           x.def_value <> None) l in
    let writers =
      List.map_first (fun ~is_first def ->
        let let1, let2 = Ox_emit.get_let ~is_rec ~is_first in
        make_ocaml_bs_writer p ~original_types is_rec let1 let2 def
      ) l in
    let readers =
      List.map_first (fun ~is_first def ->
        let let1, let2 = Ox_emit.get_let ~is_rec ~is_first in
        make_ocaml_bs_reader p ~original_types is_rec let1 let2 def
      ) l
    in
    List.flatten (writers @ readers))
  |> Indent.to_buffer buf;
  Ox_emit.maybe_write_creator_impl ~with_create deref buf defs

let make_ml
    ~opens
    ~header
    ~with_typedefs
    ~with_create
    ~with_fundefs
    ~original_types
    ocaml_typedefs deref defs =
  let buf = Buffer.create 1000 in
  bprintf buf "%s\n" header;
  Ox_emit.write_opens buf opens;
  if with_typedefs then
    bprintf buf "%s\n" ocaml_typedefs;
  if with_typedefs && with_fundefs then
    bprintf buf "\n";
  if with_fundefs then
    make_ocaml_bs_impl ~with_create ~original_types buf deref defs;
  Buffer.contents buf

let make_mli
    ~opens
    ~header
    ~with_typedefs
    ~with_create
    ~with_fundefs
    ~original_types:_
    ocaml_typedefs deref defs =
  let buf = Buffer.create 1000 in
  bprintf buf "%s\n" header;
  Ox_emit.write_opens buf opens;
  if with_typedefs then
    bprintf buf "%s\n" ocaml_typedefs;
  if with_typedefs && with_fundefs then
    bprintf buf "\n";
  if with_fundefs then
    make_ocaml_bs_intf ~with_create buf deref defs;
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
    ~pp_convs:_
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
  let defs1 = Oj_mapping.defs_of_atd_modules m1 ~target in
  let (m1', original_types) =
    Atd.Expand.expand_module_body ~keep_poly:true m0
  in
  let m2 = tsort m1' in
  (* m0 = original type definitions
     m1 = original type definitions after dependency analysis
     m2 = monomorphic type definitions after dependency analysis *)
  let ocaml_typedefs =
    Ocaml.ocaml_of_atd ~pp_convs:(Ppx []) ~target
      ~type_aliases (head, m1) in
  let defs = Oj_mapping.defs_of_atd_modules m2 ~target in
  let header =
    let src =
      match atd_file with
        None -> "stdin"
      | Some path -> sprintf "%S" (Filename.basename path)
    in
    sprintf {|(* Auto-generated from %s *)
              [@@@ocaml.warning "-27-32-35-39"]|} src
  in
  let ml =
    make_ml ~opens ~header ~with_typedefs ~with_create ~with_fundefs ~original_types
      ocaml_typedefs (Mapping.make_deref defs) defs
  in
  let mli =
    make_mli ~opens ~header ~with_typedefs ~with_create ~with_fundefs ~original_types
      ocaml_typedefs (Mapping.make_deref defs1) defs1
  in
  Ox_emit.write_ocaml out mli ml
