(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common

let escape_attribute s =
  let buffer = Buffer.create (String.length s) in
  Uutf.String.fold_utf_8 (fun () _ -> function
    | `Malformed _ -> ()
    | `Uchar c ->
      let c = Uchar.to_int c in
      match c with
      | 0x0026 -> Buffer.add_string buffer "&amp;"
      | 0x00A0 -> Buffer.add_string buffer "&nbsp;"
      | 0x0022 -> Buffer.add_string buffer "&quot;"
      | _ -> add_utf_8 buffer c)
    () s;
  Buffer.contents buffer

let escape_text s =
  let buffer = Buffer.create (String.length s) in
  Uutf.String.fold_utf_8 (fun () _ -> function
    | `Malformed _ -> ()
    | `Uchar c ->
      let c = Uchar.to_int c in
      match c with
      | 0x0026 -> Buffer.add_string buffer "&amp;"
      | 0x00A0 -> Buffer.add_string buffer "&nbsp;"
      | 0x003C -> Buffer.add_string buffer "&lt;"
      | 0x003E -> Buffer.add_string buffer "&gt;"
      | _ -> add_utf_8 buffer c)
    () s;
  Buffer.contents buffer

let void_elements =
  ["area"; "base"; "basefont"; "bgsound"; "br"; "col"; "embed"; "frame"; "hr";
   "img"; "input"; "keygen"; "link"; "meta"; "param"; "source"; "track"; "wbr"]

let prepend_newline_for = ["pre"; "textarea"; "listing"]

let rec starts_with_newline = function
  | [] -> false
  | s::more ->
    if String.length s = 0 then starts_with_newline more
    else s.[0] = '\x0A'

open Kstream

let literal_text_elements =
  ["style"; "script"; "xmp"; "iframe"; "noembed"; "noframes"; "plaintext"]

let write ?(escape_attribute=escape_attribute) ?(escape_text=escape_text) signals =
  let open_elements = ref [] in

  let in_literal_text_element () =
    match !open_elements with
      | element :: _ -> List.mem element literal_text_elements
      | _ -> false in

  let rec queue = ref next_signal

  and emit_list l throw e k =
    match l with
    | [] -> next_signal throw e k
    | s::more ->
      queue := emit_list more;
      k s

  and next_signal throw e k =
    next signals throw e begin function
      | `Start_element ((ns, name') as name, attributes) ->
        let tag_name =
          match name with
          | ns, local_name
              when list_mem_string ns [html_ns; svg_ns; mathml_ns] ->
            local_name
          | ns, local_name when ns = xml_ns -> "xml:" ^ local_name
          | ns, local_name when ns = xmlns_ns -> "xmlns:" ^ local_name
          | ns, local_name when ns = xlink_ns -> "xlink:" ^ local_name
          | _, local_name -> (* An error. *) local_name
        in

        let attributes =
          attributes |> List.map (fun ((ns, local_name) as name, value) ->
            let name =
              match name with
              | "", _ -> local_name
              | _ when ns = xml_ns -> "xml:" ^ local_name
              | _, "xmlns" when ns = xmlns_ns -> "xmlns"
              | _ when ns = xmlns_ns -> "xmlns:" ^ local_name
              | _ when ns = xlink_ns -> "xlink:" ^ local_name
              | _ -> (* An error. *) local_name
            in
            name, value)
        in

        let rec prepend_attributes words = function
          | [] -> words
          | (name, value)::more ->
            prepend_attributes
              (" "::name::"=\""::(escape_attribute value)::"\""::words) more
        in

        let tag =
          "<"::tag_name::(prepend_attributes [">"] (List.rev attributes)) in

        let is_void = ns = html_ns && list_mem_string name' void_elements in

        if is_void then
          peek signals throw (fun () -> emit_list tag throw e k) (function
            | `End_element ->
              next_option signals throw (fun _ ->
              emit_list tag throw e k)
            | `Start_element _ | `Text _ | `Comment _ | `PI _ | `Xml _
            | `Doctype _ ->
              open_elements := tag_name::!open_elements;
              emit_list tag throw e k)
        else begin
          open_elements := tag_name::!open_elements;

          if ns = html_ns && list_mem_string name' prepend_newline_for then
            peek_option signals throw (function
              | Some (`Text ss) when starts_with_newline ss ->
                emit_list (tag @ ["\n"]) throw e k
              | Some (`Text _ | `Start_element _ | `End_element | `Comment _ |
                      `PI _ | `Doctype _ | `Xml _)
              | None -> emit_list tag throw e k)
          else
            emit_list tag throw e k
        end

      | `End_element ->
        begin match !open_elements with
        | [] -> next_signal throw e k
        | name::rest ->
          open_elements := rest;
          emit_list ["</"; name; ">"] throw e k
        end

      | `Text ss ->
        if List.for_all (fun s -> String.length s = 0) ss then
          next_signal throw e k
        else if in_literal_text_element () then
          emit_list ss throw e k
        else
          emit_list (List.map escape_text ss) throw e k

      | `Comment s ->
        emit_list ["<!--"; s; "-->"] throw e k

      | `PI (target, s) ->
        emit_list ["<?"; target; " "; s; ">"] throw e k

      | `Doctype {doctype_name = Some name} ->
        emit_list ["<!DOCTYPE "; name; ">"] throw e k

      | `Doctype _ | `Xml _ ->
        next_signal throw e k
    end

  in

  (fun throw e k -> !queue throw e k) |> make
