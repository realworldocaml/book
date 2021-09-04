(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

type token =
  [ `Xml of xml_declaration
  | `Doctype of doctype
  | `Start of Token_tag.t
  | `End of Token_tag.t
  | `Chars of string list
  | `PI of string * string
  | `Comment of string
  | `EOF ]

let is_name_start_char c =
  is_in_range 0x0041 0x005A c
  || is_in_range 0x0061 0x007A c
  || c = 0x003A
  || c = 0x005F
  || is_in_range 0x00C0 0x00D6 c
  || is_in_range 0x00D8 0x00F6 c
  || is_in_range 0x00F8 0x02FF c
  || is_in_range 0x0370 0x037D c
  || is_in_range 0x037F 0x1FFF c
  || is_in_range 0x200C 0x200D c
  || is_in_range 0x2070 0x218F c
  || is_in_range 0x2C00 0x2FEF c
  || is_in_range 0x3001 0xD7EF c
  || is_in_range 0xF900 0xFDCF c
  || is_in_range 0xFDF0 0xFFFD c
  || is_in_range 0x10000 0xEFFFF c

let is_name_char c =
  is_name_start_char c
  || is_in_range 0x0030 0x0039 c
  || c = 0x002D
  || c = 0x002E
  || c = 0x00B7
  || is_in_range 0x0300 0x036F c
  || is_in_range 0x203F 0x2040 c

let resolve_builtin_reference = function
  | "quot" -> Some "\""
  | "amp" -> Some "&"
  | "apos" -> Some "'"
  | "lt" -> Some "<"
  | "gt" -> Some ">"
  | _ -> None

open Kstream
open Common.Token_tag

let tokenize report resolve_reference (input, get_location) =
  let resolve_reference s =
    match resolve_builtin_reference s with
    | Some _ as v -> v
    | None -> resolve_reference s
  in

  let report_if = Error.report_if report in

  let throw = ref (fun _ -> ()) in
  let ended = ref (fun _ -> ()) in
  let output = ref (fun _ -> ()) in

  let parse_reference l' k =
    let input, restore = checkpoint input in
    let unresolved () = restore (); k None in
    let k s = k (Some s) in

    let unexpected_eoi () =
      report (get_location ()) (`Unexpected_eoi "reference") !throw (fun () ->
      unresolved ())
    in

    let character_reference filter notation_prefix reference_prefix =
      let buffer = Buffer.create 32 in
      let rec read () =
        next input !throw unexpected_eoi begin function
          | _, 0x003B ->
            if Buffer.length buffer = 0 then
              report l' (`Bad_token
                (Printf.sprintf "&#%s;" reference_prefix, "reference",
                 "empty character reference")) !throw unresolved

            else
              let s = Buffer.contents buffer in
              let maybe_n =
                try Some (int_of_string (notation_prefix ^ s))
                with Failure _ -> None
              in

              begin match maybe_n with
              | None ->
                report l' (`Bad_token
                  (Printf.sprintf "&#%s%s;" reference_prefix s, "reference",
                   "number out of range")) !throw unresolved

              | Some n ->
                let utf_8_encoded = Buffer.create 8 in
                add_utf_8 utf_8_encoded n;
                k (Buffer.contents utf_8_encoded)
              end

          | _, c when filter c ->
            add_utf_8 buffer c;
            read ()

          | l, c ->
            report l (`Bad_token (char c, "reference", "expected digit")) !throw
              unresolved
        end
      in
      read ()
    in

    next input !throw unexpected_eoi begin function
      | _, 0x003B ->
        report l'
          (`Bad_token ("&;", "reference", "empty reference")) !throw unresolved

      | _, 0x0023 ->
        next input !throw unexpected_eoi begin function
          | _, 0x0078 ->
            character_reference is_hex_digit "0x" "x"

          | _, c as v when is_digit c || c = 0x003B ->
            push input v;
            character_reference is_digit "" ""

          | l, c ->
            report l (`Bad_token (char c, "reference", "expected digit")) !throw
              unresolved
        end

      | _, c when is_name_start_char c ->
        let buffer = Buffer.create 32 in
        add_utf_8 buffer c;
        let rec read () =
          next input !throw unexpected_eoi begin function
            | _, 0x003B ->
              let s = Buffer.contents buffer in
              begin match resolve_reference s with
              | Some s -> k s
              | None ->
                report l' (`Bad_token (s, "reference", "unknown entity")) !throw
                  unresolved
              end

            | _, c when is_name_char c ->
              add_utf_8 buffer c;
              read ()

            | l, c ->
              report l
                (`Bad_token (char c, "reference", "invalid name character"))
                !throw unresolved
          end
        in
        read ()

      | l, c ->
        report l (`Bad_token (char c, "reference", "invalid start character"))
          !throw unresolved
    end
  in

  let extra_whitespace where l c k =
    report l (`Bad_token (char c, where, "whitespace not allowed here"))
      !throw k
  in

  let rec consume_whitespace k =
    next input !throw k (function
      | _, c when is_whitespace c -> consume_whitespace k
      | v -> push input v; k ())
  in

  let parse_attribute with_references terminators l k' =
    let name_buffer = Buffer.create 32 in
    let value_buffer = Buffer.create 256 in
    let quote_opened = ref false in
    let quote_closed = ref false in

    let finish () =
      if Buffer.length name_buffer = 0 then k' None
      else
        let emit () =
          k' (Some (Buffer.contents name_buffer, Buffer.contents value_buffer))
        in

        if !quote_opened then
          if not !quote_closed then
            report (get_location ()) (`Unexpected_eoi "attribute value") !throw
              emit
          else
            emit ()
        else
          if Buffer.length value_buffer = 0 then
            report l (`Bad_token
              (Buffer.contents name_buffer, "attribute", "has no value")) !throw
              emit
          else
            emit ()
    in

    let next' f =
      next input !throw finish begin function
        | _, c as v when List.mem c terminators ->
          push input v;
          finish ();

        | v -> f v
      end
    in

    let rec name_start_state () =
      next' begin function
        | l, c ->
          report_if (not @@ is_name_start_char c) l (fun () ->
            `Bad_token (char c, "attribute", "invalid start character"))
            !throw (fun () ->
          add_utf_8 name_buffer c;
          name_state ())
      end

    and name_state () =
      next' begin function
        | _, 0x003D ->
          value_state ()

        | l, c when is_whitespace c ->
          extra_whitespace "attribute" l c (fun () ->
          consume_whitespace equals_state)

        | l, c ->
          report_if (not @@ is_name_char c) l (fun () ->
            `Bad_token (char c, "attribute", "invalid name character"))
            !throw (fun () ->
          add_utf_8 name_buffer c;
          name_state ())
      end

    and equals_state () =
      next' begin function
        | _, 0x003D ->
          value_state ()

        | v ->
          push input v;
          finish ()
      end

    and value_state () =
      next' begin function
        | l, c when is_whitespace c ->
          extra_whitespace "attribute" l c (fun () ->
          consume_whitespace value_state)

        | _, (0x0022 | 0x0027 as c) ->
          quote_opened := true;
          quoted_value_state c

        | l, c as v ->
          push input v;
          report l (`Bad_token (char c, "attribute", "unquoted value"))
            !throw unquoted_value_state
      end

    and handle_ampersand l state =
      parse_reference l begin function
        | Some s ->
          Buffer.add_string value_buffer s;
          state ()

        | None ->
          report l
            (`Bad_token ("&", "attribute", "replace with '&amp;'"))
            !throw (fun () ->
          add_utf_8 value_buffer 0x0026;
          state ())
      end

    and handle_lt l state =
      report l (`Bad_token ("<", "attribute", "replace with '&lt;'")) !throw
        (fun () ->
      add_utf_8 value_buffer 0x003C;
      state ())

    and quoted_value_state quote =
      next input !throw finish begin function
        | _, c when c = quote ->
          quote_closed := true;
          finish ()

        | l, 0x0026 when with_references ->
          handle_ampersand l (fun () ->
          quoted_value_state quote)

        | l, 0x003C ->
          handle_lt l (fun () ->
          quoted_value_state quote)

        | _, c ->
          add_utf_8 value_buffer c;
          quoted_value_state quote
      end

    and unquoted_value_state () =
      next' begin function
        | _, c as v when is_whitespace c ->
          push input v;
          finish ()

        | l, 0x0026 when with_references ->
          handle_ampersand l unquoted_value_state

        | l, 0x003C ->
          handle_lt l unquoted_value_state

        | _, c ->
          add_utf_8 value_buffer c;
          unquoted_value_state ()
      end

    in

    name_start_state ()
  in

  let parse_declaration_or_processing_instruction l k =
    let pi = "processing instruction" in
    let xml = "xml declaration" in

    let target_buffer = Buffer.create 32 in
    let text_buffer = Buffer.create 512 in
    let attributes = ref [] in

    let next' context finish f =
      let rec initial_state () =
        next input !throw (fun () ->
          report (get_location ()) (`Unexpected_eoi context) !throw finish)
        begin function
          | l, 0x003F ->
            question_mark_state l

          | v ->
            f v
        end

      and question_mark_state l =
        next input !throw (fun () ->
          report (get_location ()) (`Unexpected_eoi context) !throw finish)
        begin function
          | _, 0x003E ->
            finish ()

          | v ->
            push input v;
            f (l, 0x003F)
        end

      in
      initial_state ()
    in

    let rec target_start_state () =
      next' pi finish_pi begin function
        | l, c when is_whitespace c ->
          extra_whitespace pi l c (fun () ->
          consume_whitespace target_start_state)

        | l, c ->
          report_if (not @@ is_name_start_char c) l (fun () ->
            `Bad_token (char c, pi, "invalid start character")) !throw
            (fun () ->
          add_utf_8 target_buffer c;
          target_state ())
      end

    and target_state () =
      next' pi finish_pi begin function
        | _, c when is_whitespace c ->
          if String.lowercase (Buffer.contents target_buffer) = "xml" then
            xml_declaration_state ()
          else
            text_state ()

        | l, c ->
          report_if (not @@ is_name_char c) l (fun () ->
            `Bad_token (char c, pi, "invalid name character")) !throw
            (fun () ->
          add_utf_8 target_buffer c;
          target_state ())
      end

    and text_state () =
      next' pi finish_pi (fun (_, c) ->
        add_utf_8 text_buffer c;
        text_state ())

    and xml_declaration_state () =
      next' xml finish_xml begin function
        | _, c when is_whitespace c ->
          xml_declaration_state ()

        | _, 0x003F ->
          xml_declaration_state ()

        | l, _ as v ->
          push input v;
          parse_attribute false [0x003F] l (function
            | None -> xml_declaration_state ()
            | Some (name, value) ->
              attributes := (l, name, value)::!attributes;
              xml_declaration_state ())
      end

    and finish_pi () =
      if Buffer.length target_buffer = 0 then
        report l (`Bad_token ("<?...", pi, "empty")) !throw (fun () ->
        k None)
      else
        if String.lowercase (Buffer.contents target_buffer) = "xml" then
          finish_xml ()
        else
          k (Some
            (`PI (Buffer.contents target_buffer, Buffer.contents text_buffer)))

    and finish_xml () =
      let split f l =
        let rec scan prefix = function
          | x::suffix when f x -> Some (List.rev prefix, x, suffix)
          | x::suffix -> scan (x::prefix) suffix
          | [] -> None
        in
        scan [] l
      in

      let matches s (_, name, _) = String.lowercase name = s in

      let version_valid s =
        String.length s = 3 &&
        s.[0] = '1' && s.[1] = '.' && is_digit (Char.code s.[2])
      in

      let rec check_name attributes =
        let target = Buffer.contents target_buffer in
        report_if (target <> "xml") l (fun () ->
          `Bad_token (target, xml, "must be 'xml'")) !throw (fun () ->
        version_state attributes)

      and version_state attributes =
        match split (matches "version") attributes with
        | None ->
          report l (`Bad_token ("<?xml...", xml, "missing version")) !throw
            (fun () ->
          encoding_state "1.0" attributes)

        | Some (prefix, (l, name, value), suffix) ->
          report_if (name <> "version") l (fun () ->
            `Bad_token (name, xml, "must be 'version'")) !throw (fun () ->
          report_if (List.length prefix <> 0) l (fun () ->
            `Bad_token (name, xml, "must be first")) !throw (fun () ->
          report_if (not @@ version_valid value) l (fun () ->
            `Bad_token (value, xml, "must match 1.x")) !throw (fun () ->
          encoding_state value (prefix @ suffix))))

      and encoding_state version attributes =
        match split (matches "encoding") attributes with
        | None ->
          standalone_state version None 0 attributes

        | Some (prefix, (l, name, value), suffix) ->
          report_if (name <> "encoding") l (fun () ->
            `Bad_token (name, xml, "must be 'encoding'")) !throw (fun () ->
          standalone_state
            version (Some value) (List.length prefix) (prefix @ suffix))

      and standalone_state version encoding encoding_index attributes =
        match split (matches "standalone") attributes with
        | None ->
          final_state version encoding None attributes

        | Some (prefix, (l, name, value), suffix) ->
          report_if (name <> "standalone") l (fun () ->
            `Bad_token (name, xml, "must be 'standalone'")) !throw (fun () ->
          report_if (List.length prefix < encoding_index) l (fun () ->
            `Bad_token (name, xml, "must come after 'encoding'")) !throw
            (fun () ->

          (fun k ->
            match value with
            | "yes" -> k (Some true)
            | "no" -> k (Some false)
            | _ ->
              report l
                (`Bad_token (value, xml, "must be 'yes' or 'no'")) !throw
                (fun () ->
              match String.lowercase value with
              | "yes" -> k (Some true)
              | "no" -> k (Some false)
              | _ -> k None))
          (fun v ->
            final_state version encoding v (prefix @ suffix))))

      and final_state version encoding standalone attributes =
        (fun k ->
          match attributes with
          | (l, name, _)::_ ->
            report l (`Bad_token (name, xml, "not allowed here")) !throw k
          | [] -> k ())
        (fun () ->
          k (Some (`Xml {version; encoding; standalone})))

      in
      check_name (List.rev !attributes)
    in

    target_start_state ()
  in

  let text = Text.prepare () in
  let note_character_location = Text.note_location text in
  let add_character = Text.add text in
  let add_string = Text.add_string text in

  let rec current_state = ref initial_state

  and emit' l t s = current_state := s; !output (l, t)

  and emit_chars state =
    match Text.emit text with
    | None -> state ()
    | Some (l, strings) ->
      emit' l (`Chars strings) state

  and emit l t state =
    emit_chars (fun () ->
    emit' l t state)

  and emit_eoi ?during () =
    let l = get_location () in
    emit_chars (fun () ->
      (fun k' ->
        match during with
        | None -> k' ()
        | Some production ->
          report l (`Unexpected_eoi production) !throw k')
      (fun () ->
        emit' l `EOF (fun () -> !ended ())))

  and emit_start l name self_closing attributes state =
    let tag = {name = name; self_closing; attributes = List.rev attributes} in
    emit l (`Start tag) state

  and emit_end l name state =
    let tag = {name = name; self_closing = false; attributes = []} in
    emit l (`End tag) state

  and emit_doctype l buffer s =
    let doctype =
      {doctype_name      = None;
       public_identifier = None;
       system_identifier = None;
       raw_text          = Some (Buffer.contents buffer);
       force_quirks      = false}
    in
    emit l (`Doctype doctype) s

  and lt_in_text l k =
    report l (`Bad_token ("<", "text", "replace with '&lt;'")) !throw k

  and initial_state () =
    next input !throw (fun () -> emit_eoi ()) begin function
      | l, (0x005D as c) ->
        add_character l c;
        one_bracket_state l

      | l, 0x003C ->
        begin_markup_state l

      | l, (0x0026 as c) ->
        parse_reference l (function
          | None ->
            report l (`Bad_token (char c, "text", "replace with '&amp;'"))
              !throw (fun () ->
            add_character l c;
            initial_state ())

          | Some s ->
            add_string l s;
            initial_state ())

      | l, c ->
        add_character l c;
        initial_state ()
    end

  and one_bracket_state l' =
    next_option input !throw begin function
      | Some (l, (0x005D as c)) ->
        add_character l c;
        two_brackets_state l' l

      | v ->
        push_option input v;
        initial_state ()
    end

  and two_brackets_state l' l'' =
    next_option input !throw begin function
      | Some (l, (0x003E as c)) ->
        report l' (`Bad_token ("]]>", "text", "must end a CDATA section"))
          !throw (fun () ->
        add_character l c;
        initial_state ())

      | Some (l, (0x005D as c)) ->
        add_character l c;
        two_brackets_state l'' l

      | v ->
        push_option input v;
        initial_state ()
    end

  and begin_markup_state l' =
    let recover v =
      lt_in_text l' (fun () ->
      add_character l' 0x003C;
      push_option input v;
      initial_state ())
    in

    next input !throw (fun () ->
      report (get_location ()) (`Unexpected_eoi "tag") !throw
      (fun () -> recover None))
    begin function
      | _, 0x0021 ->
        comment_cdata_or_doctype_state l'

      | _, 0x003F ->
        parse_declaration_or_processing_instruction l' (function
          | None -> initial_state ()
          | Some token -> emit l' token initial_state)

      | _, 0x002F ->
        end_tag_state l'

      | _, c when is_name_start_char c ->
        let tag_name_buffer = Buffer.create 32 in
        add_utf_8 tag_name_buffer c;
        start_tag_state l' tag_name_buffer

      | l, c as v ->
        report l (`Bad_token (char c, "tag", "invalid start character"))
          !throw (fun () ->
        recover (Some v))
    end

  and start_tag_state l' buffer =
    let recover v =
      lt_in_text l' (fun () ->
      add_character l' 0x003C;
      add_string l' (Buffer.contents buffer);
      push_option input v;
      initial_state ())
    in

    next input !throw (fun () ->
      report (get_location ()) (`Unexpected_eoi "tag") !throw (fun () ->
      recover None))
    begin function
      | _, 0x003E ->
        emit_start l' (Buffer.contents buffer) false [] initial_state

      | l, 0x002F ->
        close_empty_element_state l' l (Buffer.contents buffer) []

      | _, c when is_whitespace c ->
        attributes_state l' (Buffer.contents buffer) []

      | _, c when is_name_char c ->
        add_utf_8 buffer c;
        start_tag_state l' buffer

      | l, c as v ->
        report l (`Bad_token (char c, "tag", "invalid name character"))
          !throw (fun () ->
        recover (Some v))
    end

  and attributes_state l' tag_name attributes =
    next input !throw begin fun () ->
      emit_start l' tag_name false attributes (fun () ->
      emit_eoi ~during:"tag" ())
    end
    begin function
      | _, c when is_whitespace c ->
        attributes_state l' tag_name attributes

      | _, 0x003E ->
        emit_start l' tag_name false attributes initial_state

      | l, 0x002F ->
        close_empty_element_state l' l tag_name attributes

      | l, _ as v ->
        push input v;
        parse_attribute true [0x003E; 0x002F] l (function
          | None -> attributes_state l' tag_name attributes
          | Some (name, value) ->
            attributes_state l' tag_name ((name, value)::attributes))
    end

  and close_empty_element_state l' l'' name attributes =
    next input !throw begin fun () ->
      emit_start l' name true attributes (fun () ->
      emit_eoi ~during:"tag" ())
    end
    begin function
      | _, 0x003E ->
        emit_start l' name true attributes initial_state

      | v ->
        report l'' (`Bad_token (char 0x002F, "tag", "should be part of '/>'"))
          !throw (fun () ->
        push input v;
        attributes_state l' name attributes)
    end

  and end_tag_state l' =
    let recover v =
      lt_in_text l' (fun () ->
      add_character l' 0x003C;
      add_character l' 0x002F;
      push_option input v;
      initial_state ())
    in

    next input !throw (fun () ->
      report (get_location ()) (`Unexpected_eoi "tag") !throw (fun () ->
      recover None))
    begin function
      | _, c when is_name_start_char c ->
        let name_buffer = Buffer.create 32 in
        add_utf_8 name_buffer c;
        end_tag_name_state l' name_buffer

      | l, c as v ->
        report l (`Bad_token (char c, "tag", "invalid start character"))
          !throw (fun () ->
        recover (Some v))
    end

  and end_tag_name_state l' buffer =
    let recover v =
      lt_in_text l' (fun () ->
      add_character l' 0x003C;
      add_character l' 0x002F;
      add_string l' (Buffer.contents buffer);
      push_option input v;
      initial_state ())
    in

    next input !throw (fun () ->
      report (get_location ()) (`Unexpected_eoi "tag") !throw (fun () ->
      recover None))
    begin function
      | _, 0x003E ->
        emit_end l' (Buffer.contents buffer) initial_state

      | _, c when is_whitespace c ->
        end_tag_whitespace_state false l' (Buffer.contents buffer)

      | _, c when is_name_char c ->
        add_utf_8 buffer c;
        end_tag_name_state l' buffer

      | l, c as v ->
        report l (`Bad_token (char c, "tag", "invalid name character"))
          !throw (fun () ->
        recover (Some v))
    end

  and end_tag_whitespace_state reported l' name =
    next input !throw begin fun () ->
      emit_end l' name (fun () ->
      emit_eoi ~during:"tag" ())
    end
    begin function
      | _, 0x003E ->
        emit_end l' name initial_state

      | _, c when is_whitespace c ->
        end_tag_whitespace_state reported l' name

      | l, c ->
        if not reported then
          report l (`Bad_token (char c, "tag", "attribute in end tag"))
            !throw (fun () ->
          end_tag_whitespace_state true l' name)
        else
          end_tag_whitespace_state reported l' name
    end

  and bad_comment_start s l k' =
    report l (`Bad_token (s, "comment", "should start with '<!--'"))
      !throw (fun () ->
    lt_in_text l k')

  and comment_cdata_or_doctype_state l' =
    next_option input !throw begin function
      | Some (_, 0x002D) ->
        comment_start_state l'

      | Some (_, 0x005B) ->
        cdata_start_state l'

      | Some (_, 0x0044) ->
        doctype_start_state l'

      | v ->
        bad_comment_start "<!" l' (fun () ->
        add_character l' 0x003C;
        add_character l' 0x0021;
        push_option input v;
        initial_state ())
    end

  and comment_start_state l' =
    next_option input !throw begin function
      | Some (_, 0x002D) ->
        comment_state l' (Buffer.create 256)

      | v ->
        bad_comment_start "<!-" l' (fun () ->
        add_character l' 0x003C;
        add_character l' 0x0021;
        add_character l' 0x002D;
        push_option input v;
        initial_state ())
    end

  and unterminated_comment l buffer =
    emit l (`Comment (Buffer.contents buffer)) (fun () ->
    emit_eoi ~during:"comment" ())

  and comment_state l' buffer =
    next input !throw (fun () -> unterminated_comment l' buffer)
    begin function
      | l, 0x002D ->
        comment_one_dash_state l' l buffer

      | _, c ->
        add_utf_8 buffer c;
        comment_state l' buffer
    end

  and comment_one_dash_state l' l'' buffer =
    next input !throw (fun () -> unterminated_comment l' buffer)
    begin function
      | _, 0x002D ->
        comment_two_dashes_state false l' l'' buffer

      | _, c ->
        add_utf_8 buffer 0x002D;
        add_utf_8 buffer c;
        comment_state l' buffer
    end

  and comment_two_dashes_state reported l' l'' buffer =
    let recover k' =
      if reported then k' ()
      else
        report l''
          (`Bad_token ("--", "comment", "should be followed by '>'")) !throw k'
    in

    next input !throw (fun () -> unterminated_comment l' buffer)
    begin function
      | _, 0x003E ->
        emit l' (`Comment (Buffer.contents buffer)) initial_state

      | _, 0x002D ->
        recover (fun () ->
        add_utf_8 buffer 0x002D;
        comment_two_dashes_state true l' l'' buffer)

      | _, c ->
        recover (fun () ->
        add_utf_8 buffer 0x002D;
        add_utf_8 buffer 0x002D;
        add_utf_8 buffer c;
        comment_state l' buffer)
    end

  and cdata_start_state l' =
    next_n 6 input !throw begin function
      | [_, 0x43; _, 0x44; _, 0x41; _, 0x54; _, 0x41; _, 0x005B] ->
        note_character_location l';
        cdata_state l'

      | cs ->
        report l' (`Bad_token ("<![", "cdata", "should start with '<![CDATA['"))
          !throw (fun () ->
        lt_in_text l' (fun () ->
        push_list input cs;
        add_character l' 0x003C;
        add_character l' 0x0021;
        add_character l' 0x005B;
        initial_state ()))
    end

  and cdata_state l' =
    next input !throw (fun () -> emit_eoi ~during:"cdata" ())
    begin function
      | l, 0x005D ->
        cdata_one_bracket_state l' l

      | l, c ->
        add_character l c;
        cdata_state l'
    end

  and cdata_one_bracket_state l' l'' =
    next input !throw (fun () -> emit_eoi ~during:"cdata" ())
    begin function
      | l, 0x005D ->
        cdata_two_brackets_state l' l'' l

      | l, c ->
        add_character l'' 0x005D;
        add_character l c;
        cdata_state l'
    end

  and cdata_two_brackets_state l' l'' l''' =
    next input !throw (fun () -> emit_eoi ~during:"cdata" ())
    begin function
      | _, 0x003E ->
        initial_state ()

      | l, 0x005D ->
        add_character l'' 0x005D;
        cdata_two_brackets_state l' l''' l

      | l, c ->
        add_character l'' 0x005D;
        add_character l''' 0x005D;
        add_character l c;
        cdata_state l'
    end

  and doctype_start_state l' =
    next_n 7 input !throw begin function
      | [_, 0x4F; _, 0x43; _, 0x54; _, 0x59; _, 0x50; _, 0x45; _, c]
          when is_whitespace c ->
        doctype_state l' (Buffer.create 512)

      | cs ->
        report l'
          (`Bad_token ("<!D", "doctype", "should start with '<!DOCTYPE '"))
          !throw (fun () ->
        lt_in_text l' (fun () ->
        push_list input cs;
        add_character l' 0x003C;
        add_character l' 0x0021;
        add_character l' 0x0044;
        initial_state ()))
    end

  and unterminated_doctype l buffer =
    emit_doctype l buffer (fun () ->
    emit_eoi ~during:"doctype" ())

  and doctype_state l' buffer =
    next input !throw (fun () -> unterminated_doctype l' buffer)
    begin function
      | _, 0x003E ->
        emit_doctype l' buffer initial_state

      | _, (0x0022 | 0x0027 as c) ->
        add_utf_8 buffer c;
        doctype_quoted_state (fun () -> doctype_state l' buffer) c l' buffer

      | _, (0x003C as c) ->
        add_utf_8 buffer c;
        doctype_item_state (fun () -> doctype_state l' buffer) l' buffer

      | _, c ->
        add_utf_8 buffer c;
        doctype_state l' buffer
    end

  and doctype_quoted_state state quote l' buffer =
    next input !throw (fun () -> unterminated_doctype l' buffer)
    begin function
      | _, c when c = quote ->
        add_utf_8 buffer c;
        state ()

      | _, c ->
        add_utf_8 buffer c;
        doctype_quoted_state state quote l' buffer
    end

  and doctype_item_state state l' buffer =
    next input !throw (fun () -> unterminated_doctype l' buffer)
    begin function
      | _, (0x0021 as c) ->
        add_utf_8 buffer c;
        doctype_declaration_state state l' buffer

      | l, (0x003F as c) ->
        add_utf_8 buffer c;
        let undo = tap (fun (_, c) -> add_utf_8 buffer c) input in
        parse_declaration_or_processing_instruction l (fun _ ->
        undo ();
        state ())

      | _, c ->
        add_utf_8 buffer c;
        state ()
    end

  and doctype_declaration_state state l' buffer =
    next input !throw (fun () -> unterminated_doctype l' buffer)
    begin function
      | _, (0x003E as c) ->
        add_utf_8 buffer c;
        state ()

      | _, (0x0022 | 0x0027 as c) ->
        add_utf_8 buffer c;
        doctype_quoted_state
          (fun () -> doctype_declaration_state state l' buffer) c l' buffer

      | _, c ->
        add_utf_8 buffer c;
        doctype_declaration_state state l' buffer
    end

  in

  (fun throw_ e k ->
    throw := throw_;
    ended := e;
    output := k;
    !current_state ())
  |> make
