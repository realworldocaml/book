open Result

(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
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



module Paths = Odoc_model.Paths



let empty_body = []

let empty : Odoc_model.Comment.docs = empty_body



let load_payload : Parsetree.payload -> (string * Location.t) option = function
  | PStr [{pstr_desc =
      Pstr_eval ({pexp_desc =
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
        Pexp_constant (Const_string (text, _))
#elif OCAML_MAJOR = 4 && OCAML_MINOR < 11
        Pexp_constant (Pconst_string (text, _))
#else
        Pexp_constant (Pconst_string (text, _, _))
#endif
   ; pexp_loc = loc; _}, _); _}] ->
    Some (text, loc)
  | _ ->
    None

let attached parent attrs =
  let ocaml_deprecated = ref None in
  let rec loop first nb_deprecated acc
      : _ -> (Odoc_model.Comment.docs, Odoc_model.Error.t) result =
    function
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
    | {Parsetree.attr_name = { Location.txt =
          ("doc" | "ocaml.doc"); loc = _loc}; attr_payload; _ } :: rest -> begin
#else
    | ({Location.txt =
          ("doc" | "ocaml.doc"); loc = _loc}, attr_payload) :: rest -> begin
#endif
        match load_payload attr_payload with
        | Some (str, loc) -> begin
            let start_pos = loc.Location.loc_start in
            let start_pos =
              {start_pos with pos_cnum = start_pos.pos_cnum + 3} in
            let parsed =
              Odoc_parser.parse_comment
                ~sections_allowed:`All
                ~containing_definition:parent
                ~location:start_pos
                ~text:str
              |> Odoc_model.Error.shed_warnings
            in
            loop false 0 (acc @ parsed) rest
          end
        | None -> (* TODO *) assert false
      end
    | _ :: rest -> loop first nb_deprecated acc rest
    | [] -> begin
        match nb_deprecated, !ocaml_deprecated with
        | 0, Some _tag -> Ok acc
        | _, _ -> Ok acc
      end
  in
    loop true 0 empty_body attrs
  |> Odoc_model.Error.to_exception

let read_string parent loc str : Odoc_model.Comment.docs_or_stop =
  let start_pos = loc.Location.loc_start in
  let doc : Odoc_model.Comment.docs =
    Odoc_parser.parse_comment
      ~sections_allowed:`All
      ~containing_definition:parent
      ~location:start_pos
      ~text:str
    |> Odoc_model.Error.shed_warnings
  in
  `Docs doc

let page = read_string

let standalone parent
    : Parsetree.attribute -> Odoc_model.Comment.docs_or_stop option =

  function
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
  | { attr_name = { Location.txt =
        ("text" | "ocaml.text"); loc = _loc}; attr_payload; _ } -> begin
#else
  | ({Location.txt =
        ("text" | "ocaml.text"); loc = _loc}, attr_payload) -> begin
#endif
      match load_payload attr_payload with
      | Some ("/*", _loc) -> Some `Stop
      | Some (str, loc) ->
        let loc' =
          { loc with
            loc_start = { loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + 3 } }
        in
        Some (read_string parent loc' str)
      | None ->
        (* TODO *)
        assert false
          (* let doc : Odoc_model.Comment.t =
            Error (invalid_attribute_error parent loc) in
            Some (Documentation doc) *)
    end
  | _ -> None

let standalone_multiple parent attrs =
  let coms =
    List.fold_left
      (fun acc attr ->
        match standalone parent attr  with
         | None -> acc
         | Some com -> com :: acc)
      [] attrs
  in
    List.rev coms
