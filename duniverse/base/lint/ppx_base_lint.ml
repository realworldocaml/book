open Ppxlib
open Base

let error ~loc fmt = Location.raise_errorf ~loc (Caml.( ^^ ) "ppx_base_lint:" fmt)

type suspicious_id = Caml_submodule of string

let rec iter_suspicious (id : Longident.t) ~f =
  match id with
  | Ldot (Lident "Caml", s)
    when String.( <> ) s ""
         &&
         match s.[0] with
         | 'A' .. 'Z' -> true
         | _ -> false -> f (Caml_submodule s)
  | Ldot (x, _) -> iter_suspicious x ~f
  | Lapply (a, b) ->
    iter_suspicious a ~f;
    iter_suspicious b ~f
  | Lident _ -> ()
;;

let zero_modules () =
  Caml.Sys.readdir "."
  |> Array.to_list
  |> List.filter ~f:(fun fn -> Caml.Filename.check_suffix fn "0.ml")
  |> List.map ~f:(fun fn ->
    String.capitalize (String.sub fn ~pos:0 ~len:(String.length fn - 4)))
  |> Set.of_list (module String)
;;

let check_open (id : Longident.t Asttypes.loc) =
  match id.txt with
  | Lident "Caml" -> error ~loc:id.loc "you are not allowed to open Caml inside Base"
  | _ -> ()
;;

let rec is_caml_dot_something : Longident.t -> bool = function
  | Ldot (Lident "Caml", _) -> true
  | Ldot (id, _) -> is_caml_dot_something id
  | _ -> false
;;

let print_payload ppf = function
  | PStr x -> Pprintast.structure ppf x
  | PSig x -> Pprintast.signature ppf x
  | PTyp x -> Pprintast.core_type ppf x
  | PPat (x, None) -> Pprintast.pattern ppf x
  | PPat (x, Some w) ->
    Caml.Format.fprintf ppf "%a@ when@ %a" Pprintast.pattern x Pprintast.expression w
;;

let remove_loc =
  object
    inherit Ast_traverse.map

    method! location _ = Location.none

    method! location_stack _ = []
  end
;;

(* Disable this check given that in base we replace [@cold] by [@cold] [@inline never]
   ... in the source code *)
let () = Ppx_js_style.cold_instead_of_inline_never := false

let check current_module =
  let zero_modules = zero_modules () in
  object
    inherit Ast_traverse.iter as super

    method! longident_loc { txt = id; loc } =
      (* Note: we don't distinguish between module identifiers and constructors
         names. Since there is no [Caml.String], [Caml.Array], ... constructors this is
         not a problem. *)
      iter_suspicious id ~f:(function Caml_submodule m ->
        if not (Set.mem zero_modules m)
        then () (* We are allowed to use Caml modules that don't have a Foo0 version *)
        else if String.equal (m ^ "0") current_module
        then () (* Foo0 is allowed to use Caml.Foo *)
        else (
          match current_module with
          | "Import0" | "Base" -> ()
          | _ -> error ~loc "you cannot use [Caml.%s] here, use [%s0] instead" m m))

    method! expression e =
      super#expression e;
      match e.pexp_desc with
      | Pexp_open ({ popen_expr = { pmod_desc = Pmod_ident id; _ }; _ }, _) ->
        check_open id
      | _ -> ()

    method! open_description op =
      super#open_description op;
      check_open op.popen_expr

    method! module_binding mb =
      super#module_binding mb;
      match current_module with
      | "Import0" -> ()
      | _ ->
        (match mb.pmb_expr.pmod_desc with
         | Pmod_ident { txt = id; _ } when is_caml_dot_something id ->
           error ~loc:mb.pmb_loc "you cannot alias [Caml] sub-modules, use them directly"
         | _ -> ())

    method! attributes attrs =
      super#attributes attrs;
      let is_cold attr = String.equal attr.attr_name.txt "cold" in
      match List.find attrs ~f:is_cold with
      | None -> ()
      | Some attr ->
        let expansion =
          Ppx_cold.expand_cold_attribute attr
          |> List.map ~f:(fun a ->
            { a with
              attr_name =
                { a.attr_name with
                  txt =
                    String.chop_prefix a.attr_name.txt ~prefix:"ocaml."
                    |> Option.value ~default:a.attr_name.txt
                }
            })
        in
        let is_part_of_expansion attr =
          List.exists expansion ~f:(fun a ->
            String.equal a.attr_name.txt attr.attr_name.txt
            || String.equal ("ocaml." ^ a.attr_name.txt) attr.attr_name.txt)
        in
        let new_attrs =
          List.concat_map attrs ~f:(fun a ->
            if is_cold a
            then a :: expansion
            else if is_part_of_expansion a
            then []
            else [ a ])
        in
        if not
             (Poly.equal (remove_loc#attributes attrs) (remove_loc#attributes new_attrs))
        then (
          (* Remove attributes written by the user that correspond to attributes in the
             expansion *)
          List.iter attrs ~f:(fun a ->
            if is_part_of_expansion a
            then Driver.register_correction ~loc:a.attr_loc ~repl:"");
          let attribute_level =
            String.make
              (attr.attr_name.loc.loc_start.pos_cnum
               - attr.attr_loc.loc_start.pos_cnum
               - 1)
              '@'
          in
          let repl =
            Caml.Format.asprintf
              "@[<h>%a@]"
              (Caml.Format.pp_print_list (fun ppf x ->
                 Caml.Format.fprintf
                   ppf
                   "[%s%s@ %a]"
                   attribute_level
                   x.attr_name.txt
                   print_payload
                   x.attr_payload))
              (attr :: expansion)
          in
          Driver.register_correction ~loc:attr.attr_loc ~repl)
  end
;;

let module_of_loc (loc : Location.t) =
  String.capitalize
    (Caml.Filename.chop_extension (Caml.Filename.basename loc.loc_start.pos_fname))
;;

let () =
  Ppxlib.Driver.register_transformation
    "base_lint"
    ~impl:(function
      | [] -> []
      | { pstr_loc = loc; _ } :: _ as st ->
        (check (module_of_loc loc))#structure st;
        st)
    ~intf:(function
      | [] -> []
      | { psig_loc = loc; _ } :: _ as sg ->
        (check (module_of_loc loc))#signature sg;
        sg)
;;
