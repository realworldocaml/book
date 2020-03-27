(*
 * Copyright (c) 2015 Leo White <lpw25@cl.cam.ac.uk>
 * Copyright (c) 2016, 2017 Thomas Refis <trefis@janestreet.com>
 *
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)



class lookup = object
  val env = Name_env.empty

  inherit Odoc_model.Maps.types as super

  method root x = x
  method path_type x = x
  method path_module_type x = x
  method path_module x = x
  method path_class_type x = x
  method identifier_value x = x
  method identifier_type x = x
  method identifier_module_type x = x
  method identifier_module x = x
  method identifier_method x = x
  method identifier_label x = x
  method identifier_page x = x
  method identifier_instance_variable x = x
  method identifier_field x = x
  method identifier_extension x = x
  method identifier_exception x = x
  method identifier_constructor x = x
  method identifier_class_type x = x
  method identifier_class x = x
  method identifier_signature x = x
  method identifier x = x
  method fragment_type x = x
  method fragment_module x = x

  method reference_module x =
    Name_env.lookup_module env x
  method reference_module_type x =
    Name_env.lookup_module_type env x
  method reference_type x =
    Name_env.lookup_type env x
  method reference_constructor x =
    Name_env.lookup_constructor env x
  method reference_field x =
    Name_env.lookup_field env x
  method reference_extension x =
    Name_env.lookup_extension env x
  method reference_exception x =
    Name_env.lookup_exception env x
  method reference_value x =
    Name_env.lookup_value env x
  method reference_class x =
    Name_env.lookup_class env x
  method reference_class_type x =
    Name_env.lookup_class_type env x
  method reference_method x =
    Name_env.lookup_method env x
  method reference_instance_variable x =
    Name_env.lookup_instance_variable env x
  method reference_label x =
    Name_env.lookup_label env x
  method reference_any x =
    Name_env.lookup_element env x

  method super_module md = super#module_ md

  method! module_ md =
    (* let open Module in *)
    let env = Name_env.add_module_decl_items md.type_ env in
    let this = {< env = env >} in
      this#super_module md

  method super_module_type mty = super#module_type mty

  method! module_type mty =
    (* let open ModuleType in *)
    let env =
      match mty.expr with
      | None -> env
      | Some expr -> Name_env.add_module_type_expr_items expr env
    in
    let this = {< env = env >} in
      this#super_module_type mty

  method super_unit unt = super#unit unt

  method! unit unt =
    let env = Name_env.add_unit unt env in
    let env =
      match unt.content with
      | Module items -> Name_env.add_signature_items items env
      | Pack _ -> env
    in
    let this = {< env = env >} in
      this#super_unit unt

  method super_page page = super#page page

  method! page page =
    let env = Name_env.add_page page env in
    let this = {< env = env >} in
      this#super_page page

  method super_class cl = super#class_ cl

  method! class_ cl =
    let env = Name_env.add_class_decl_items cl.type_ env in
    let this = {< env = env >} in
      this#super_class cl

  method super_class_type cltyp = super#class_type cltyp

  method! class_type cltyp =
    let env = Name_env.add_class_type_expr_items cltyp.expr env in
    let this = {< env = env >} in
      this#super_class_type cltyp

  method! documentation_reference r =
    let (path, elements) = super#documentation_reference r in
    match path, elements with
    | `Resolved
        (`Identifier (`Label _) | `Label _ as rr), [] ->
      begin match Name_env.lookup_section_title env rr with
      | None -> (path, elements)
      | Some elements' -> (path, elements')
      end
    | _ ->
      (path, elements)

end

let lookup x =
  let obj = new lookup in
  obj#unit x

let lookup_page x =
  let obj = new lookup in
  obj#page x
