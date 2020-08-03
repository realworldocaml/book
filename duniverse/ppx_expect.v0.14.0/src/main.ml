open Expect_test_common
open Base
open Ppxlib
open Ast_builder.Default

let lift_location
      ~loc
      ({ filename; line_number; line_start; start_pos; end_pos } : File.Location.t)
  =
  Merlin_helpers.hide_expression
    [%expr
      ({ filename =
           Expect_test_common.File.Name.of_string
             [%e estring ~loc (File.Name.to_string filename)]
       ; line_number = [%e eint ~loc line_number]
       ; line_start = [%e eint ~loc line_start]
       ; start_pos = [%e eint ~loc start_pos]
       ; end_pos = [%e eint ~loc end_pos]
       }
       : Expect_test_common.File.Location.t)]
;;

let eoption ~loc x =
  match x with
  | None -> pexp_construct ~loc (Located.mk ~loc (lident "None")) None
  | Some e -> pexp_construct ~loc (Located.mk ~loc (lident "Some")) (Some e)
;;

let estring_option ~loc x = eoption ~loc (Option.map x ~f:(estring ~loc))

let lift_expectation
      ~loc
      ({ tag; body; extid_location; body_location } : _ Expectation.t)
  =
  Merlin_helpers.hide_expression
    [%expr
      ({ tag = [%e estring_option ~loc tag]
       ; body =
           [%e
             match body with
             | Exact string -> [%expr Exact [%e estring ~loc string]]
             | Output -> [%expr Output]
             | Pretty string -> [%expr Pretty [%e estring ~loc string]]
             | Unreachable -> [%expr Unreachable]]
       ; extid_location = [%e lift_location ~loc extid_location]
       ; body_location = [%e lift_location ~loc body_location]
       }
       : string Expect_test_common.Expectation.t)]
;;

(* Grab a list of all the output expressions *)
let collect_expectations =
  object
    inherit [(Location.t * Expectation.Raw.t) list] Ast_traverse.fold as super

    method! expression expr acc =
      match Expect_extension.match_expectation expr with
      | None -> super#expression expr acc
      | Some ext ->
        assert_no_attributes expr.pexp_attributes;
        (expr.pexp_loc, ext) :: acc
  end
;;

let replace_expects =
  object
    inherit Ast_traverse.map as super

    method! expression ({ pexp_attributes; pexp_loc = loc; _ } as expr) =
      match Expect_extension.match_expectation expr with
      | None -> super#expression expr
      | Some ext ->
        let f_var =
          match ext.body with
          | Exact _ | Pretty _ | Unreachable -> "Expect_test_collector.save_output"
          | Output -> "Expect_test_collector.save_and_return_output"
        in
        let expr =
          [%expr [%e evar ~loc f_var] [%e lift_location ~loc ext.extid_location]]
        in
        { expr with pexp_attributes }
  end
;;

let file_digest =
  let cache = Hashtbl.create (module String) ~size:32 in
  fun fname ->
    Hashtbl.find_or_add cache fname ~default:(fun () ->
      Caml.Digest.file fname |> Caml.Digest.to_hex)
;;

let rewrite_test_body ~descr ~tags ~uncaught_exn pstr_loc body =
  let loc = pstr_loc in
  let expectations =
    List.map (collect_expectations#expression body []) ~f:(fun (loc, expect_extension) ->
      lift_expectation ~loc expect_extension)
    |> elist ~loc
  in
  let uncaught_exn =
    Option.map uncaught_exn ~f:(fun (loc, expectation) ->
      lift_expectation ~loc expectation)
    |> eoption ~loc
  in
  let body = replace_expects#expression body in
  let absolute_filename =
    Ppx_here_expander.expand_filename pstr_loc.loc_start.pos_fname
  in
  let hash = file_digest loc.loc_start.pos_fname in
  [%expr
    let module Expect_test_collector = Expect_test_collector.Make (Expect_test_config) in
    Expect_test_collector.run
      ~file_digest:(Expect_test_common.File.Digest.of_string [%e estring ~loc hash])
      ~location:[%e lift_location ~loc (Ppx_expect_payload.transl_loc pstr_loc)]
      ~absolute_filename:[%e estring ~loc absolute_filename]
      ~description:[%e estring_option ~loc descr]
      ~tags:[%e elist ~loc (List.map tags ~f:(estring ~loc))]
      ~expectations:[%e expectations]
      ~uncaught_exn_expectation:[%e uncaught_exn]
      ~inline_test_config:(module Inline_test_config)
      (fun () -> [%e body])]
;;

module P = struct
  open Ast_pattern

  let uncaught_exn =
    Attribute.declare_with_name_loc
      "@expect.uncaught_exn"
      Attribute.Context.value_binding
      (map1' (Ppx_expect_payload.pattern ()) ~f:(fun loc x -> loc, x))
      (fun ~name_loc (loc, x) ->
         loc, Ppx_expect_payload.make x ~kind:Normal ~extension_id_loc:name_loc)
  ;;

  let opt_name () =
    map (pstring __) ~f:(fun f x -> f (Some x)) ||| map ppat_any ~f:(fun f -> f None)
  ;;

  let pattern () =
    pstr
      (pstr_value
         nonrecursive
         (Attribute.pattern
            uncaught_exn
            (value_binding
               ~pat:
                 (map
                    (Attribute.pattern Ppx_inline_test.tags (opt_name ()))
                    ~f:(fun f attributes name_opt ->
                      f
                        ~name:name_opt
                        ~tags:
                          (match attributes with
                           | None -> []
                           | Some x -> x)))
               ~expr:__)
          ^:: nil)
       ^:: nil)
  ;;
end

(* Set to [true] when we see a [%expect_test] extension *)
module Has_tests =
  Driver.Create_file_property
    (struct
      let name = "ppx_expect.has_tests"
    end)
    (Bool)

let expect_test =
  Extension.declare_inline
    "expect_test"
    Structure_item
    (P.pattern ())
    (fun ~loc ~path:_ uncaught_exn ~name ~tags code ->
       Has_tests.set true;
       Ppx_inline_test.validate_extension_point_exn
         ~name_of_ppx_rewriter:"ppx_expect"
         ~loc
         ~tags;
       rewrite_test_body ~descr:name ~tags ~uncaught_exn loc code
       |> Ppx_inline_test.maybe_drop loc)
;;

let () =
  Driver.register_transformation
    "expect_test"
    ~rules:[ Context_free.Rule.extension expect_test ]
    ~enclose_impl:(fun whole_loc ->
      match whole_loc, Ppx_inline_test_libname.get () with
      | None, _ | _, None -> [], []
      | Some loc, Some _ ->
        let maybe_drop = Ppx_inline_test.maybe_drop in
        let absolute_filename =
          Ppx_here_expander.expand_filename loc.loc_start.pos_fname
        in
        let header =
          let loc = { loc with loc_end = loc.loc_start } in
          maybe_drop
            loc
            [%expr
              Expect_test_collector.Current_file.set
                ~absolute_filename:[%e estring ~loc absolute_filename]]
        and footer =
          let loc = { loc with loc_start = loc.loc_end } in
          maybe_drop loc [%expr Expect_test_collector.Current_file.unset ()]
        in
        header, footer)
;;
