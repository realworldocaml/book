open Base
open Ppxlib
open Ast_builder.Default

(* Generated code should depend on the environment in scope as little as
   possible.  E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the
   use of [=].  It is especially important to not use polymorphic comparisons, since we
   are moving more and more to code that doesn't have them in scope. *)


type maybe_drop =
  | Keep
  | Drop_with_deadcode
  | Drop

let maybe_drop_mode = ref Keep

let set_default_maybe_drop x = maybe_drop_mode := x

let () =
  Driver.add_arg "-inline-test-drop"
    (Unit (fun () -> maybe_drop_mode := Drop))
    ~doc:" Drop unit tests";
  Driver.add_arg "-inline-test-drop-with-deadcode"
    (Unit (fun () -> maybe_drop_mode := Drop_with_deadcode))
    ~doc:" Drop unit tests by wrapping them inside deadcode to prevent \
          unused variable warnings.";
;;

let () =
  Driver.Cookies.add_simple_handler "inline-test"
    Ast_pattern.(pexp_ident (lident __'))
    ~f:(function
      | None -> ()
      | Some id ->
        match id.txt with
        | "drop" -> maybe_drop_mode := Drop
        | "drop_with_deadcode" -> maybe_drop_mode := Drop_with_deadcode
        | s ->
          Location.raise_errorf ~loc:id.loc
            "invalid 'inline-test' cookie (%s), expected one of: drop, drop_with_deadcode"
            s)

(* Same as above, but for the Dune setting *)
let () =
  Driver.Cookies.add_simple_handler "inline_tests"
    Ast_pattern.(estring __')
    ~f:(function
      | None -> ()
      | Some id ->
        match id.txt with
        | "enabled" -> maybe_drop_mode := Keep
        | "disabled" -> maybe_drop_mode := Drop
        | "ignored" -> maybe_drop_mode := Drop_with_deadcode
        | s ->
          Location.raise_errorf ~loc:id.loc
            "invalid 'inline_tests' cookie (%s), expected one of: enabled, disabled or ignored"
            s)
;;

let maybe_drop loc code =
  match !maybe_drop_mode with
  | Keep               -> [%str let () = [%e code]]
  | Drop_with_deadcode -> [%str let () = if false then [%e code] else ()]
  | Drop               -> Attribute.explicitly_drop#expression code; [%str ]

let rec short_desc_of_expr ~max_len e =
  match e.pexp_desc with
  | Pexp_let (_, _, e) | Pexp_letmodule (_, _, e) ->
    short_desc_of_expr ~max_len e
  | _ ->
    let s = Pprintast.string_of_expression e in
    let res =
      if String.length s >= max_len then
        let s_short = String.sub s ~pos:0 ~len:(max_len - 5) in
        s_short ^ "[...]"
      else s
    in
    String.map res ~f:(function
      | '\n' -> ' '
      | c -> c)
;;

let descr ~(loc:Location.t) ?(inner_loc=loc) e_opt id_opt =
  let filename  = File_path.get_default_path loc                 in
  let line      = loc.loc_start.pos_lnum                         in
  let start_pos = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let end_pos   = inner_loc.Location.loc_end.pos_cnum - loc.loc_start.pos_bol in
  let descr =
    match id_opt, e_opt with
    | None, None -> ""
    | None, Some e -> ": <<" ^ short_desc_of_expr ~max_len:50 e ^ ">>"
    | Some id, _ -> ": " ^ id
  in
  (estring ~loc descr,
   estring ~loc filename,
   eint ~loc line,
   eint ~loc start_pos,
   eint ~loc end_pos)
;;

let apply_to_descr lid ~loc ?inner_loc e_opt id_opt tags more_arg =
  let descr, filename, line, start_pos, end_pos = descr ~loc ?inner_loc e_opt id_opt in
  let expr =
    pexp_apply ~loc (evar ~loc ("Ppx_inline_test_lib.Runtime." ^ lid))
      [ Labelled "config", [%expr (module Inline_test_config)]
      ; Labelled "descr", descr
      ; Labelled "tags", elist ~loc (List.map ~f:(estring ~loc) tags)
      ; Labelled "filename", filename
      ; Labelled "line_number", line
      ; Labelled "start_pos", start_pos
      ; Labelled "end_pos", end_pos
      ; Nolabel, more_arg ]
  in
  maybe_drop loc expr
;;

let can_use_test_extensions () =
  match !maybe_drop_mode, Ppx_inline_test_libname.get () with
  | Keep, None -> false
  | (Drop | Drop_with_deadcode), _ | _, Some _ -> true
;;

(* Set to [true] when we see a [let%test] or [let%expect_test] etc extension. *)
module Has_tests =
  Driver.Create_file_property
    (struct let name = "ppx_inline_test.has_tests" end)
    (Bool)

let all_tags =
  [ "no-js"
  ; "js-only"
  ; "64-bits-only"
  ; "32-bits-only"
  ; "fast-flambda"
  ; "x-library-inlining-sensitive"
  ]

let validate_tag tag =
  if not (List.mem all_tags tag ~equal:String.equal)
  then
    Error (Spellcheck.spellcheck all_tags tag)
  else
    Ok ()

let validate_extension_point_exn ~name_of_ppx_rewriter ~loc ~tags =
  Has_tests.set true;
  if not (can_use_test_extensions ()) then
    Location.raise_errorf ~loc
      "%s: extension is disabled because the tests would be ignored \
       (the build system didn't pass -inline-test-lib)" name_of_ppx_rewriter;
  List.iter tags ~f:(fun tag ->
    match validate_tag tag with
    | Ok () -> ()
    | Error hint ->
      let hint = match hint with
        | None      -> ""
        | Some hint -> "\n"^hint
      in
      Location.raise_errorf ~loc
        "%s: %S is not a valid tag for inline tests.%s" name_of_ppx_rewriter tag hint
  )
;;

let name_of_ppx_rewriter = "ppx_inline_test"

let expand_test ~loc ~path:_ ~name:id ~tags e =
  validate_extension_point_exn ~name_of_ppx_rewriter ~loc ~tags;
  apply_to_descr "test" ~loc (Some e) id tags (pexp_fun ~loc Nolabel None (punit ~loc) e)
;;

let expand_test_unit ~loc ~path:_ ~name:id ~tags e =
  validate_extension_point_exn ~name_of_ppx_rewriter ~loc ~tags;
  apply_to_descr "test_unit" ~loc (Some e) id tags (pexp_fun ~loc Nolabel None (punit ~loc) e)
;;

let expand_test_module ~loc ~path:_ ~name:id ~tags m =
  validate_extension_point_exn ~name_of_ppx_rewriter ~loc ~tags;
  apply_to_descr "test_module" ~loc ~inner_loc:m.pmod_loc None id tags
    (pexp_fun ~loc Nolabel None (punit ~loc)
       (pexp_letmodule ~loc (Located.mk ~loc (Some "M"))
          m
          (eunit ~loc)))
;;

module E = struct
  open Ast_pattern

  let tags =
    Attribute.declare
      "tags"
      Attribute.Context.pattern
      (single_expr_payload (
         pexp_tuple (many (estring __))
         |||  map (estring __) ~f:(fun f x -> f [x])))
      (fun x -> x)

  let list_of_option = function
    | None -> []
    | Some x -> x

  let opt_name () =
         map (pstring __) ~f:(fun f x -> f (Some x))
     ||| map ppat_any     ~f:(fun f   -> f None)

  let opt_name_and_expr expr =
    pstr ((
      pstr_value nonrecursive (
        (value_binding
           ~pat:(
             map
               (Attribute.pattern tags (opt_name ()))
               ~f:(fun f attributes name_opt ->
                 f ~name:name_opt ~tags:(list_of_option attributes)))
           ~expr)
        ^:: nil)
    ) ^:: nil)

  let test =
    Extension.declare_inline "inline_test.test"
      Extension.Context.structure_item
      (opt_name_and_expr __)
      expand_test

  let test_unit =
    Extension.declare_inline "inline_test.test_unit"
      Extension.Context.structure_item
      (opt_name_and_expr __)
      expand_test_unit

  let test_module =
    Extension.declare_inline "inline_test.test_module"
      Extension.Context.structure_item
      (opt_name_and_expr  (pexp_pack __))
      expand_test_module

  let all =
    [ test
    ; test_unit
    ; test_module
    ]
end

let tags = E.tags
let opt_name_and_expr = E.opt_name_and_expr

let () =
  Driver.register_transformation "inline-test"
    ~extensions:E.all
    ~enclose_impl:(fun loc ->
      match loc, Ppx_inline_test_libname.get () with
      | None, _ | _, None -> ([], [])
      | Some loc, Some (libname, partition) ->
        (* See comment in benchmark_accumulator.ml *)
        let header =
          let loc = { loc with loc_end = loc.loc_start } in
          maybe_drop loc [%expr Ppx_inline_test_lib.Runtime.set_lib_and_partition
                                  [%e estring ~loc libname] [%e estring ~loc partition]]
        and footer =
          let loc = { loc with loc_start = loc.loc_end } in
          maybe_drop loc [%expr Ppx_inline_test_lib.Runtime.unset_lib
                                  [%e estring ~loc libname]]
        in
        (header, footer)
    )
;;
