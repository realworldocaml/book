open Ppxlib
open Ast_builder.Default

type maybe_drop =
  | Keep
  | Deadcode
  | Remove

let drop_benches = ref Keep
let () =
  Driver.add_arg "-bench-drop"
    (Unit (fun () -> drop_benches := Remove))
    ~doc:" Drop inline benchmarks";
  Driver.add_arg "-bench-drop-with-deadcode"
    (Unit (fun () -> drop_benches := Deadcode))
    ~doc:" Drop inline benchmarks by wrapping them inside deadcode to prevent unused variable warnings."


let () =
  Driver.Cookies.add_simple_handler "inline-bench"
    Ast_pattern.(pexp_ident (lident __'))
    ~f:(function
      | None -> ()
      | Some id ->
        match id.txt with
        | "drop" -> drop_benches := Remove
        | "drop_with_deadcode" -> drop_benches := Deadcode
        | s ->
          Location.raise_errorf ~loc:id.loc
            "invalid 'inline-bench' cookie (%s), expected one of: \
             drop, drop_with_deadcode"
            s)

let maybe_drop loc code =
  match !drop_benches with
  | Keep     -> [%str let () = [%e code]]
  | Deadcode -> [%str let () = if false then [%e code] else ()]
  | Remove   -> Attribute.explicitly_drop#expression code; [%str ]

let descr (loc : Location.t) ?(inner_loc=loc) () =
  let filename  = File_path.get_default_path loc in
  let line      = loc.loc_start.pos_lnum  in
  let start_pos = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let end_pos   = inner_loc.Location.loc_end.pos_cnum - loc.loc_start.pos_bol in
  (estring ~loc filename,
   eint    ~loc line,
   eint    ~loc start_pos,
   eint    ~loc end_pos)

let apply_to_descr_bench type_conv_path lid loc ?inner_loc e_opt ?name_suffix name more_arg =
  let filename, line, start_pos, end_pos = descr loc ?inner_loc () in
  let s = match e_opt with
    | None   -> ""
    | Some e -> Pprintast.string_of_expression e
  in
  let descr = estring ~loc s in
  let name =
    let base_name = estring ~loc name in
    match name_suffix with
    | None -> base_name
    | Some name_suffix ->
      [%expr [%e base_name] ^ [%e name_suffix]]
  in
  let type_conv_path = estring ~loc type_conv_path in
  maybe_drop loc
    [%expr
      if Ppx_bench_lib.Benchmark_accumulator.add_environment_var then
        [%e evar ~loc ("Ppx_bench_lib.Benchmark_accumulator." ^ lid)]
          ~name:[%e name]
          ~code:[%e descr]
          ~type_conv_path:[%e type_conv_path]
          ~filename:[%e filename]
          ~line:[%e line]
          ~startpos:[%e start_pos]
          ~endpos:[%e end_pos]
          [%e more_arg]
    ]

type bench_kind = Bench | Bench_fun

let thunk_bench kind e = match kind with
  | Bench_fun -> e
  | Bench -> let loc = e.pexp_loc in [%expr fun () -> [%e e]]

let enabled () =
  match Ppx_inline_test_libname.get () with
  | None   -> false
  | Some _ -> true

let assert_enabled loc =
  if not (enabled ()) then
    Location.raise_errorf ~loc
      "ppx_bench: extension is disabled as no -inline-test-lib was given"

let expand_bench_exp ~loc ~path kind index name e =
  assert_enabled loc;
  match index with
  | None ->
    (* Here and in the other cases below, because functions given to pa_bench can return
       any 'a, we add a dead call to ignore so we can get a warning if the user code
       mistakenly gives a partial application. *)
    apply_to_descr_bench path "add_bench" loc (Some e) name
      [%expr
        let f `init = [%e thunk_bench kind e] in begin
          if false then Ppx_bench_lib.Export.ignore (f `init ()) else ();
          Ppx_bench_lib.Benchmark_accumulator.Entry.Regular_thunk f
        end
      ]
  | Some (var_name, args) ->
    apply_to_descr_bench path "add_bench" loc (Some e) name
      [%expr
        let arg_values = [%e args]
        and f = fun [%p pvar ~loc var_name] -> [%e thunk_bench kind e] in begin
          if false then Ppx_bench_lib.Export.ignore (f 0 ()) else ();
          Ppx_bench_lib.Benchmark_accumulator.Entry.Indexed_thunk
            { Ppx_bench_lib.Benchmark_accumulator.Entry.arg_name =
                [%e estring ~loc var_name]
            ; Ppx_bench_lib.Benchmark_accumulator.Entry.arg_values
            ; Ppx_bench_lib.Benchmark_accumulator.Entry.thunk = f
            }
        end
      ]

let expand_bench_module ~loc ~path name_suffix name m =
  assert_enabled loc;
  apply_to_descr_bench
    path "add_bench_module" loc ~inner_loc:m.pmod_loc None ?name_suffix name
    (pexp_fun ~loc Nolabel None (punit ~loc)
       (pexp_letmodule ~loc (Located.mk ~loc "M")
          m
          (eunit ~loc)))

module E = struct
  let indexed =
    Attribute.declare
      "bench.indexed"
      Attribute.Context.pattern
      Ast_pattern.
        (single_expr_payload (pexp_apply (pexp_ident (lident (string "=")))
                                (no_label (pexp_ident (lident __))
                                 ^:: no_label __
                                 ^:: nil)))
      (fun var values -> (var, values))

  let name_suffix =
    Attribute.declare
      "bench.name_suffix"
      Attribute.Context.pattern
      Ast_pattern.(single_expr_payload __)
      (fun a -> a)

  let simple =
    let open Ast_pattern in
    pstr (pstr_value nonrecursive
            (value_binding ~pat:(Attribute.pattern indexed (pstring __)) ~expr:__
             ^:: nil)
          ^:: nil)

  let bench =
    Extension.declare_inline "bench" Extension.Context.structure_item
      simple (expand_bench_exp Bench)

  let bench_fun =
    Extension.declare_inline "bench_fun" Extension.Context.structure_item
      simple (expand_bench_exp Bench_fun)

  let bench_module =
    Extension.declare_inline "bench_module" Extension.Context.structure_item
      Ast_pattern.(
        pstr (pstr_value nonrecursive (value_binding
                                         ~pat:(Attribute.pattern name_suffix (pstring __))
                                         ~expr:(pexp_pack __)
                                       ^:: nil)
              ^:: nil)
      )
      expand_bench_module

  let all =
    [ bench
    ; bench_fun
    ; bench_module
    ]
end

let () =
  Driver.register_transformation "bench"
    ~extensions:E.all
    ~enclose_impl:(fun loc ->
      match loc, Ppx_inline_test_libname.get () with
      | None, _ | _, None -> ([], [])
      | Some loc, Some (libname, _) ->
        (* See comment in benchmark_accumulator.ml *)
        let header =
          let loc = { loc with loc_end = loc.loc_start } in
          maybe_drop loc
            [%expr Ppx_bench_lib.Benchmark_accumulator.Current_libname.set
                     [%e estring ~loc libname]]
        and footer =
          let loc = { loc with loc_start = loc.loc_end } in
          maybe_drop loc
            [%expr Ppx_bench_lib.Benchmark_accumulator.Current_libname.unset ()]
        in
        (header, footer)
    )
;;
