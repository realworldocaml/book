(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)



let escape s =
  let buffer = Buffer.create (String.length s) in
  String.iter (function
    | '"' -> Buffer.add_string buffer "&quot;"
    | '&' -> Buffer.add_string buffer "&amp;"
    | '\'' -> Buffer.add_string buffer "&apos;"
    | '<' -> Buffer.add_string buffer "&lt;"
    | '>' -> Buffer.add_string buffer "&gt;"
    | c -> Buffer.add_char buffer c)
    s;
  Buffer.contents buffer

let attribute_strings end_ attributes =
  let rec prepend_attributes words = function
    | [] -> words
    | (name, value)::more ->
      prepend_attributes
        (" "::name::"=\""::(escape value)::"\""::words) more
  in

  prepend_attributes [end_] (List.rev attributes)

open Common
open Kstream

let write report prefix signals =
  let signals = enumerate signals in

  let open_elements = ref [] in
  let namespaces = Namespace.Writing.init prefix in

  let rec queue = ref next_signal

  and emit_list l throw e k =
    match l with
    | [] -> next_signal throw e k
    | s::more ->
      queue := emit_list more;
      k s

  and next_signal throw e k =
    next signals throw e begin function
      | i, (`Start_element (name, attributes) as signal) ->
        (fun k' ->
          next signals throw (fun () -> k' false) (fun s ->
            match s with
            | _, `End_element -> k' true
            | _, (`Text _ | `Start_element _ | `Comment _ | `PI _ | `Doctype _ |
                  `Xml _) -> push signals s; k' false))
        (fun self_closing ->
          Namespace.Writing.push (fun () -> report (signal, i))
            namespaces name attributes
            throw (fun (formatted_name, formatted_attributes) ->

          open_elements := formatted_name::!open_elements;

          if self_closing then begin
            Namespace.Writing.pop namespaces;
            open_elements :=
              match !open_elements with
              | [] -> []
              | _::rest -> rest
          end;

          let end_ = if self_closing then "/>" else ">" in

          let tag =
            "<"::formatted_name::(attribute_strings end_ formatted_attributes)
          in

          emit_list tag throw e k))

      | _, `End_element ->
        Namespace.Writing.pop namespaces;
        begin match !open_elements with
        | [] -> next_signal throw e k
        | name::rest ->
          open_elements := rest;
          emit_list ["</"; name; ">"] throw e k
        end

      | _, `Text ss ->
        if List.for_all (fun s -> String.length s = 0) ss then
          next_signal throw e k
        else
          emit_list (List.map escape ss) throw e k

      | _, `Xml {version; encoding; standalone} ->
        let attributes =
          match standalone with
          | None -> []
          | Some true -> ["standalone", "yes"]
          | Some false -> ["standalone", "no"]
        in

        let attributes =
          match encoding with
          | None -> attributes
          | Some encoding -> ("encoding", encoding)::attributes
        in

        let attributes = ("version", version)::attributes in

        let declaration = "<?xml"::(attribute_strings "?>" attributes) in

        emit_list declaration throw e k

      | _, `Doctype {raw_text} ->
        begin match raw_text with
        | None -> next_signal throw e k
        | Some text -> emit_list ["<!DOCTYPE "; text; ">"] throw e k
        end

      | _, `PI (target, s) ->
        emit_list ["<?"; target; " "; s; "?>"] throw e k

      | _, `Comment s ->
        emit_list ["<!--"; s; "-->"] throw e k
    end

  in

  (fun throw e k -> !queue throw e k) |> make
