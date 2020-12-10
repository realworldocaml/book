(*
  Validators of OCaml data whose types are defined using ATD.
*)

open Atd.Import
open Indent

open Atd.Ast
open Mapping
open Ov_mapping

let make_ocaml_validate_intf ~with_create buf deref defs =
  List.concat_map snd defs
  |> List.iter (fun x ->
    if with_create && Ox_emit.is_exportable x then (
      let create_record_intf, _ =
        Ox_emit.make_record_creator deref x
      in
      bprintf buf "%s" create_record_intf;
    );

    let full_name = Ox_emit.get_full_type_name x in
    let validator_params =
      String.concat "" (
        List.map
          (fun s ->
             sprintf "\n  (Atdgen_runtime.Util.Validation.path -> '%s -> \
                      Atdgen_runtime.Util.Validation.error option) ->" s)
          x.def_param
      )
    in
    let s = x.def_name in
    if Ox_emit.is_exportable x then (
      bprintf buf "\
val validate_%s :%s
  Atdgen_runtime.Util.Validation.path -> %s -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!%s}. *)

"
        s validator_params
        full_name
        s
    )
  )

let get_fields a =
  let all =
    List.map (
      fun x ->
        match x.f_arepr with
          Ocaml.Repr.Field o -> (x, o.Ocaml.ocaml_fname)
        | _ -> assert false
    )
      (Array.to_list a)
  in
  List.filter (
    function
      { f_brepr = (None, shallow) ; _ }, _ -> not shallow
    | _ -> assert false
  ) all

let rec forall : Indent.t list -> Indent.t list = function
  | [] -> []
  | [x] -> [x]
  | x :: l ->
      [
        Line "match";
        Block [x];
        Line "with";
        Block [
          Line "| Some _ as err -> err";
          Line "| None ->";
          Block (forall l);
        ]
      ]

let return_true_paren = "(fun _ _ -> None)"

let opt_validator = function
    None -> [ Line "fun _ _ -> None" ]
  | Some s -> [ Line s ]

let opt_validator_s = function
    None -> "(fun _ _ -> None)"
  | Some s -> sprintf "( %s )" s


let prepend_validator opt l =
  match opt with
    None -> l
  | Some s ->
      [
        Line (sprintf "match ( %s ) path x with" s);
        Block [
          Line "| Some _ as err -> err";
          Line "| None ->";
          Block l;
        ]
      ]

let prepend_validator_s v s2 =
  match v with
    None -> s2
  | Some s1 ->
      sprintf "(fun path x -> \
               match ( %s ) path x with \
               | Some _ as err -> err \
               | None -> (%s) path x)" s1 s2

let prepend_validator_f v l =
  match v with
    None -> l
  | Some s ->
      [
        Line "(fun path x ->";
        Block [
          Line (sprintf "(match ( %s ) path x with" s);
          Block [
            Line "| Some _ as err -> err";
            Line "| None -> (";
            Block [
              Block l;
              Line ") path x";
            ]
          ];
          Line ")";
        ];
        Line ")";
      ]

(*
  ('a, 'b) t ->
    validate_t validate__a validate__b
  ('a, foo) t ->
    validate_t validate__a validate_foo
  ('a, (foo, 'b) bar) t ->
    validate_t validate__a (validate_bar validate_foo validate__b)
*)
let rec get_validator_name
    ?(paren = false)
    ?(name_f = fun s -> "validate_" ^ s)
    (x : ov_mapping) : string =

  match x with
    Unit (_, Unit, v)
  | Bool (_, Bool, v)
  | Int (_, Int _, v)
  | Float (_, Float, v)
  | String (_, String, v) ->
      (match v with
         (None, true) -> return_true_paren
       | (Some s, true) -> s
       | (_, false) -> assert false
      )
  | Tvar (_, s) -> "validate_" ^ (Ox_emit.name_of_var s)

  | Name (_, s, args, None, opt) ->
      let v1 =
        let l =
          List.map (get_validator_name ~paren:true) args in
        let s = String.concat " " (name_f s :: l) in
        if paren && l <> [] then "(" ^ s ^ ")"
        else s
      in
      (match opt with
         None -> v1
       | Some (o, false) -> prepend_validator_s o v1
       | Some (o, true) -> opt_validator_s o
      )

  | External (_, _, args,
              External (_, main_module, ext_name),
              v) ->
      (match v with
         (o, false) ->
           prepend_validator_s o (
             let f = main_module ^ "." ^ name_f ext_name in
             let l = List.map (get_validator_name ~paren:true) args in
             let s = String.concat " " (f :: l) in
             if paren && l <> [] then "(" ^ s ^ ")"
             else s
           )
       | (_, true) -> assert false
      )

  | _ -> assert false


let get_left_validator_name name param =
  let args = List.map (fun s -> Tvar (dummy_loc, s)) param in
  get_validator_name (Name (dummy_loc, name, args, None, None))

let rec make_validator (x : ov_mapping) : Indent.t list =
  match x with
    Unit _
  | Bool _
  | Int _
  | Float _
  | String _
  | Name _
  | External _
  | Tvar _ -> [ Line (get_validator_name x) ]

  | Sum (_, a, Sum x, (v, shallow)) ->
      if shallow then
        opt_validator v
      else
        let tick = Ocaml.tick x in
        let body : Indent.t list =
          [
            Line "match x with";
            Block (
              Array.to_list (
                Array.map
                  (fun x -> Inline (make_variant_validator tick x))
                  a
              )
            )
          ]
        in
        [
          Annot ("fun", Line "fun path x ->");
          Block (prepend_validator v body);
        ]

  | Record (_, a, Record o, (v, shallow)) ->
      if shallow then
        opt_validator v
      else
        [
          Annot ("fun", Line "fun path x ->");
          Block (prepend_validator v (make_record_validator a o));
        ]

  | Tuple (_, a, Tuple, (v, shallow)) ->
      if shallow then
        opt_validator v
      else
        let len = Array.length a in
        let l = Array.to_list (Array.mapi (fun i x -> (i, x)) a) in
        let l = List.filter (fun (_, x) -> not (snd x.cel_brepr)) l in
        let l =
          List.map (
            fun (i, x) ->
              Inline [
                Line (sprintf "(let %s = x in" (Ox_emit.nth "x" i len));
                Line "(";
                Block (make_validator x.cel_value);
                Line (sprintf ") (`Index %i :: path) x" i);
                Line ")"
              ]
          ) l
        in
        let l = forall l
        in
        [
          Annot ("fun", Line "fun path x ->");
          Block (prepend_validator v l);
        ]

  | List (_, x, List o, (v, shallow)) ->
      if shallow then
        opt_validator v
      else
        let validate =
          match o with
            List -> "Atdgen_runtime.Ov_run.validate_list ("
          | Array -> "Atdgen_runtime.Ov_run.validate_array ("
        in
        prepend_validator_f v [
          Line validate;
          Block (make_validator x);
          Line ")";
        ]

  | Option (_, x, Option, (v, shallow))
  | Nullable (_, x, Nullable, (v, shallow)) ->
      if shallow then
        opt_validator v
      else
        prepend_validator_f v [
          Line "Atdgen_runtime.Ov_run.validate_option (";
          Block (make_validator x);
          Line ")";
        ]

  | Wrap (_, x, Wrap _, (v, shallow)) ->
      if shallow then
        opt_validator v
      else
        prepend_validator_f v (make_validator x)

  | _ -> assert false



and make_variant_validator tick x :
  Indent.t list =
  let o =
    match x.var_arepr, x.var_brepr with
      Variant o, (None, _) -> o
    | _ -> assert false
  in
  let ocaml_cons = o.Ocaml.ocaml_cons in
  match x.var_arg with
    None ->
      [
        Line (sprintf "| %s%s -> None" tick ocaml_cons)
      ]
  | Some v ->
      [
        Line (sprintf "| %s%s x ->" tick ocaml_cons);
        Block [
          Line "(";
          Block (make_validator v);
          Line ") path x"
        ]
      ]

and make_record_validator a record_kind =
  let dot = Ocaml.dot record_kind in
  let fields = get_fields a in
  assert (fields <> []);
  let validate_fields : Indent.t list =
    List.map (
      fun (x, ocaml_fname) ->
        Inline [
          Line "(";
          Block (make_validator x.Mapping.f_value);
          Line (sprintf
                  ") (`Field %S :: path) x%s%s" ocaml_fname dot ocaml_fname);
        ]
    ) fields
  in
  forall validate_fields

let make_ocaml_validator ~original_types is_rec let1 def =
  let x = Option.value_exn def.def_value in
  let name = def.def_name in
  let type_constraint = Ox_emit.get_type_constraint ~original_types def in
  let param = def.def_param in
  let validate = get_left_validator_name name param in
  let validator_expr = make_validator x in
  let eta_expand = is_rec && not (Ox_emit.is_lambda validator_expr) in
  let needs_annot = Ox_emit.needs_type_annot x in
  let extra_param, extra_args, type_annot =
    match eta_expand, needs_annot with
    | true, false -> " path x", " path x", None
    | true, true -> sprintf " path (x : %s)" type_constraint, " path x", None
    | false, false -> "", "", None
    | false, true -> "", "", Some (sprintf "_ -> %s -> _" type_constraint)
  in
  [
    Line (sprintf "%s %s = ("
            let1
            (Ox_emit.opt_annot_def type_annot (validate ^ extra_param)));
    Block (List.map Indent.strip validator_expr);
    Line (sprintf ")%s" extra_args);
  ]


let make_ocaml_validate_impl ~with_create ~original_types buf deref defs =
  defs
  |> List.concat_map (fun (is_rec, l) ->
    let l = List.filter (fun x -> x.def_value <> None) l in
    let validators =
      List.map_first (fun ~is_first def ->
        let let1, _ = Ox_emit.get_let ~is_rec ~is_first in
        make_ocaml_validator ~original_types is_rec let1 def
      ) l
    in
    List.flatten validators)
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
    make_ocaml_validate_intf ~with_create buf deref defs;
  Buffer.contents buf

let make_ml
    ~header ~opens ~with_typedefs ~with_create ~with_fundefs
    ~original_types ocaml_typedefs deref defs =
  let buf = Buffer.create 1000 in
  bprintf buf "%s\n" header;
  Ox_emit.write_opens buf opens;
  if with_typedefs then
    bprintf buf "%s\n" ocaml_typedefs;
  if with_typedefs && with_fundefs then
    bprintf buf "\n";
  if with_fundefs then
    make_ocaml_validate_impl ~with_create ~original_types buf deref defs;
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
    ~ocaml_version:_
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
  let m1 = tsort m0
  in
  let defs1 = Ov_mapping.defs_of_atd_modules m1 in
  let (m1', original_types) =
    Atd.Expand.expand_module_body ~keep_poly:true m0
  in
  let m2 = tsort m1' in
  (* m0 = original type definitions
     m1 = original type definitions after dependency analysis
     m2 = monomorphic type definitions after dependency analysis *)
  let ocaml_typedefs =
    Ocaml.ocaml_of_atd ~pp_convs ~target:Validate ~type_aliases (head, m1) in
  let defs = Ov_mapping.defs_of_atd_modules m2 in
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
      ~original_types ocaml_typedefs (Mapping.make_deref defs) defs
  in
  Ox_emit.write_ocaml out mli ml
