open! Base
open Ppxlib
open Ast_builder.Default

(* Avoids adding timing definitions in toplevel contexts like utop (which uses the
   filename "//toplevel//") and toplevel expect tests (which have ".mlt" files).

   This is necessary to preserve interactions like the following:
   {v
     utop # 1;;
     - : int = 1
   v}

   To print the type and value of a result, the toplevel has to detect lone expressions.
   Wrapping an expression in module timing definitions spoils this special case. So we
   turn off this rewriter in toplevel contexts. *)
let is_ocaml_file string =
  String.is_suffix string ~suffix:".ml" || String.is_suffix string ~suffix:".mll"
;;

let enclose_impl = function
  | Some (loc : Location.t) when is_ocaml_file loc.loc_start.pos_fname ->
    let prefix =
      let loc = { loc with loc_end = loc.loc_start; loc_ghost = true } in
      [%str
        let () =
          Ppx_module_timer_runtime.record_start Ppx_module_timer_runtime.__MODULE__
        ;;]
    in
    let suffix =
      let loc = { loc with loc_start = loc.loc_end; loc_ghost = true } in
      [%str
        let () =
          Ppx_module_timer_runtime.record_until Ppx_module_timer_runtime.__MODULE__
        ;;]
    in
    prefix, suffix
  | _ -> [], []
;;

module Time_individual_definitions = struct
  let attribute =
    Attribute.Floating.declare
      "ppx_module_timer.pay_overhead_to_time_individual_definitions"
      Attribute.Floating.Context.structure_item
      (Ast_pattern.pstr Ast_pattern.nil)
      ()
  ;;

  let rec module_expr_is_compound module_expr =
    match module_expr.pmod_desc with
    | Pmod_structure _ -> true
    | Pmod_constraint (body, _) -> module_expr_is_compound body
    | _ -> false
  ;;

  let structure_item_is_compound item =
    match item.pstr_desc with
    | Pstr_recmodule module_bindings ->
      List.for_all module_bindings ~f:(fun module_binding ->
        module_expr_is_compound module_binding.pmb_expr)
    | Pstr_module module_binding -> module_expr_is_compound module_binding.pmb_expr
    | _ -> false
  ;;

  let obj =
    object (self)
      inherit Ast_traverse.map

      method! structure structure =
        List.concat_map structure ~f:(fun item ->
          if structure_item_is_compound item
          then [ self#structure_item item ]
          else (
            let loc = { item.pstr_loc with loc_ghost = true } in
            let name =
              Location.print Caml.Format.str_formatter loc;
              Caml.Format.flush_str_formatter () |> String.chop_suffix_exn ~suffix:":"
            in
            [%str
              let () =
                Ppx_module_timer_runtime.record_definition_start [%e estring ~loc name]
              ;;

              [%%i
                item]

              let () =
                Ppx_module_timer_runtime.record_definition_until [%e estring ~loc name]
              ;;]))
    end
  ;;
end

let structure_item_is_attribute item =
  match item.pstr_desc with
  | Pstr_attribute _ -> true
  | _ -> false
;;

let impl structure_with_initial_attributes =
  let initial_attributes, structure =
    List.split_while structure_with_initial_attributes ~f:structure_item_is_attribute
  in
  let prefix, suffix =
    let loc =
      Option.both (List.hd structure) (List.last structure)
      |> Option.map ~f:(fun (first, last) ->
        { first.pstr_loc with loc_end = last.pstr_loc.loc_end })
    in
    enclose_impl loc
  in
  let middle =
    match
      List.find_map structure ~f:(fun item ->
        match item.pstr_desc with
        | Pstr_attribute _ ->
          Attribute.Floating.convert [ Time_individual_definitions.attribute ] item
        | _ -> None)
    with
    | None -> structure
    | Some () -> Time_individual_definitions.obj#structure structure
  in
  initial_attributes @ prefix @ middle @ suffix
;;

let () = Driver.register_transformation "module_timer" ~impl
