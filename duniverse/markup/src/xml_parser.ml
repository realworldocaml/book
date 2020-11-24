(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common
open Kstream
open Token_tag

let is_whitespace_only strings = List.for_all is_whitespace_only strings

let parse context namespace report tokens =
  let open_elements = ref [] in
  let namespaces = Namespace.Parsing.init namespace in
  let is_fragment = ref false in
  let fragment_allowed = ref true in

  let throw = ref (fun _ -> ()) in
  let ended = ref (fun _ -> ()) in
  let output = ref (fun _ -> ()) in

  let rec current_state = ref (fun () ->
    match context with
    | None -> initial_state []
    | Some `Document ->
      fragment_allowed := false;
      document_state ()
    | Some `Fragment ->
      is_fragment := false;
      content_state ())

  and emit l signal state = current_state := state; !output (l, signal)

  and push_and_emit l {name = raw_name; attributes} state =
    Namespace.Parsing.push (fun () -> report l) namespaces raw_name attributes
      !throw (fun (expanded_name, attributes) ->

    let rec deduplicate acc attributes k =
      match attributes with
      | [] -> k (List.rev acc)
      | ((n, _) as attr)::more ->
        if acc |> List.exists (fun (n', _) -> n' = n) then
          report l (`Bad_token (snd n, "tag", "duplicate attribute")) !throw
            (fun () -> deduplicate acc more k)
        else
          deduplicate (attr::acc) more k
    in

    deduplicate [] attributes (fun attributes ->
    open_elements := (l, expanded_name, raw_name)::!open_elements;
    emit l (`Start_element (expanded_name, attributes)) state))

  and pop l state =
    match !open_elements with
    | [] -> state ()
    | _::more ->
      Namespace.Parsing.pop namespaces;
      open_elements := more;
      emit l `End_element state

  and emit_end () =
    current_state := (fun () -> !ended ());
    !ended ()

  and initial_state leading =
    next_expected tokens !throw begin function
      | _, (`Xml _ | `Doctype _ | `Start _ | `End _) as v ->
        push tokens v;
        push_list tokens (List.rev leading);
        document_state ()

      | _, `Chars s as v when is_whitespace_only s ->
        initial_state (v::leading)

      | _, (`Comment _ | `PI _) as v ->
        initial_state (v::leading)

      | _, (`Chars _ | `EOF) as v ->
        is_fragment := true;
        push tokens v;
        push_list tokens (List.rev leading);
        content_state ()
    end

  and document_state () =
    next_expected tokens !throw begin function
      | l, `Xml declaration ->
        fragment_allowed := false;
        emit l (`Xml declaration) doctype_state

      | v ->
        push tokens v;
        doctype_state ()
    end

  and doctype_state () =
    next_expected tokens !throw begin function
      | l, `Doctype d ->
        fragment_allowed := false;
        emit l (`Doctype d) root_state

      | _, `Chars s when is_whitespace_only s ->
        doctype_state ()

      | l, `Comment s ->
        emit l (`Comment s) doctype_state

      | l, `PI s ->
        emit l (`PI s) doctype_state

      | l, `Xml _ ->
        report l (`Bad_document "XML declaration must be first") !throw
          doctype_state

      | l, `Chars _ ->
        report l (`Bad_document "text at top level") !throw doctype_state

      | v ->
        push tokens v;
        root_state ()
    end

  and root_state () =
    next_expected tokens !throw begin function
      | l, `Start t ->
        if t.self_closing then
          push_and_emit l t (fun () ->
          pop l after_root_state)
        else
          push_and_emit l t content_state

      | _, `Chars s when is_whitespace_only s ->
        root_state ()

      | l, `Comment s ->
        emit l (`Comment s) root_state

      | l, `PI s ->
        emit l (`PI s) root_state

      | l, `Xml _ ->
        report l (`Bad_document "XML declaration must be first") !throw
          root_state

      | l, `EOF ->
        report l (`Unexpected_eoi "document before root element") !throw
          emit_end

      | l, _ ->
        report l (`Bad_document "expected root element") !throw root_state
    end

  and after_root_state () =
    next_expected tokens !throw begin function
      | _, `Chars s when is_whitespace_only s ->
        after_root_state ()

      | l, `Comment s ->
        emit l (`Comment s) after_root_state

      | l, `PI s ->
        emit l (`PI s) after_root_state

      | _, `EOF ->
        emit_end ()

      | _, (`Chars _ | `Start _ | `End _) as v when !fragment_allowed ->
        is_fragment := true;
        push tokens v;
        content_state ()

      | l, _ as v ->
        report l (`Bad_document "not allowed after root element") !throw
          (fun () ->
        is_fragment := true;
        push tokens v;
        content_state ())
    end

  and content_state () =
    next_expected tokens !throw begin function
      | l, `Start t ->
        if t.self_closing then
          push_and_emit l t (fun () ->
          pop l content_state)
        else
          push_and_emit l t content_state

      | l, `End {name = raw_name} ->
        Namespace.Parsing.expand_element (fun () -> report l) namespaces
          raw_name !throw (fun expanded_name ->

        let is_on_stack =
          !open_elements
          |> List.exists (fun (_, name, _) -> name = expanded_name)
        in

        if not is_on_stack then
          report l (`Unmatched_end_tag raw_name) !throw content_state
        else
          let rec pop_until_match () =
            match !open_elements with
            | (_, name, _)::_ when name = expanded_name ->
              pop l (fun () ->
              match !open_elements with
              | [] when not !is_fragment -> after_root_state ()
              | _ -> content_state ())

            | (l', _, name)::_ ->
              report l' (`Unmatched_start_tag name) !throw (fun () ->
              pop l pop_until_match)

            | _ -> failwith "impossible"
          in
          pop_until_match ())

      | l, `Chars s ->
        emit l (`Text s) content_state

      | l, `PI s ->
        emit l (`PI s) content_state

      | l, `Comment s ->
        emit l (`Comment s) content_state

      | l, `EOF ->
        let rec pop_stack () =
          match !open_elements with
          | [] -> emit_end ()
          | (l', _, raw_name)::_ ->
            report l' (`Unmatched_start_tag raw_name) !throw (fun () ->
            pop l pop_stack)
        in
        pop_stack ()

      | l, `Xml _ ->
        report l (`Bad_document "XML declaration should be at top level") !throw
          content_state

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be at top level") !throw
          content_state
    end

  in

  (fun throw_ e k ->
    throw := throw_;
    ended := e;
    output := k;
    !current_state ())
  |> make
