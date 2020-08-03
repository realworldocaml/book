(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2016 Anton Bachin
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110-1301, USA.
*)

let parse loc (language, element_name) attributes =
  let (module Reflected) =
    Namespace.get language in

  (* For prefix ["prefix"] and attribute names ["prefix-foo"], evaluates to
     [Some "foo"], otherwise evaluates to [None].

     Used to parse user-data attributes (prefixed by "data-") and ARIA
     attributes (prefixed by "aria-").
  *)
  let parse_prefixed prefix name =
    let length = String.length prefix in

    let is_prefixed =
      try String.sub name 0 length = prefix
      with Invalid_argument _ -> false
    in

    if not is_prefixed then None
    else Some (String.sub name length (String.length name - length))
  in

  (* Applied to each attribute. Accumulates individually labeled attributes,
     such as img/src, in "labeled," and attributes passed in ~a in "regular." *)
  let parse_attribute (labeled, regular) ((_, local_name), value) =
    (* Convert the markup name of the attribute to a TyXML name without regard
       to renamed attributes such as "a_input_max." Renaming will be accounted
       for later. *)
    let tyxml_name = Name_convention.attrib local_name in

    let test_labeled (e, a, _) = e = element_name && a = local_name in
    let test_blacklisted (a, _, _) = a = tyxml_name in
    let test_renamed (_, a, es) = a = local_name && List.mem element_name es in

    let unknown () =
      Common.error loc "Unknown attribute in %s element: %s"
        (Common.lang language) local_name
    in

    (* Check whether this attribute is individually labeled. Parse its argument
       and accumulate the attribute if so. *)
    match Common.find test_labeled Reflected.labeled_attributes with
    | Some (_, label, parser) ->
      let e =
        match parser language loc local_name value with
        | None ->
          Common.error loc
            "Internal error: labeled attribute %s without an argument" label
        | Some e -> e
      in

      (Common.Label.labelled label, e)::labeled, regular

    | None ->
      (* The attribute is not individually labeled, so it is passed in ~a.

         First, check if the default TyXML name of this attribute collides with
         the TyXML name of a renamed attribute. For example, if the language is
         HTML, and this attribute has markup name "input-max" (which is
         invalid), then its default TyXML name will be "a_input_max", which is a
         *valid* value in TyXML. We want to avoid mapping "input-max" to
         "a_input_max", because "input-max" is invalid, and because
         "a_input_max" maps to "max" instead. *)
      if List.exists test_blacklisted Reflected.renamed_attributes then
        unknown ()
      else
        let parse_prefixed_attribute tag tyxml_name =
          let parser =
            try List.assoc tyxml_name Reflected.attribute_parsers
            with Not_found ->
              Common.error loc "Internal error: no parser for %s" tyxml_name
          in

          let identifier = Common.make ~loc language tyxml_name in
          let tag = Common.string loc tag in

          let e =
            match parser language loc local_name value with
            | Some e' -> [%expr [%e identifier] [%e tag] [%e e']] [@metaloc loc]
            | None ->
              Common.error loc "Internal error: no expression for %s"
                tyxml_name
          in

          labeled, e::regular
        in

        (* Check if this is a "data-foo" or "aria-foo" attribute. Parse the
           attribute value, and accumulate it in the list of attributes passed
           in ~a. *)
        match parse_prefixed "data-" local_name,
              parse_prefixed "aria-" local_name
        with
        | Some tag, _ -> parse_prefixed_attribute tag "a_user_data"
        | _, Some tag -> parse_prefixed_attribute tag "a_aria"
        | None, None ->
          let tyxml_name =
            match Common.find test_renamed Reflected.renamed_attributes with
            | Some (name, _, _) -> name
            | None -> tyxml_name
          in

          let parser =
            try List.assoc tyxml_name Reflected.attribute_parsers
            with Not_found -> unknown ()
          in

          let identifier = Common.make ~loc language tyxml_name in

          let e =
            match parser language loc local_name value with
            | None -> identifier
            | Some e' -> [%expr [%e identifier] [%e e']] [@metaloc loc]
          in

          labeled, e::regular
  in

  let labeled, regular =
    List.fold_left parse_attribute ([], []) attributes in

  (* If there are any attributes to pass in ~a, assemble them into a parse tree
     for a list, and prefix that with the ~a label. *)
  if regular = [] then List.rev labeled
  else
    let regular =
      Common.Label.labelled "a",
      Common.list loc (List.rev regular)
    in
    List.rev (regular::labeled)
