(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common
open Token_tag

type token =
  [ `Doctype of doctype
  | `Start of Token_tag.t
  | `End of Token_tag.t
  | `Char of int
  | `Comment of string
  | `EOF ]

type state = [ `Data | `RCDATA | `RAWTEXT | `Script_data | `PLAINTEXT ]

let replace_windows_1252_entity = function
  | 0x80 -> 0x20AC
  | 0x82 -> 0x201A
  | 0x83 -> 0x0192
  | 0x84 -> 0x201E
  | 0x85 -> 0x2026
  | 0x86 -> 0x2020
  | 0x87 -> 0x2021
  | 0x88 -> 0x02C6
  | 0x89 -> 0x2030
  | 0x8A -> 0x0160
  | 0x8B -> 0x2039
  | 0x8C -> 0x0152
  | 0x8E -> 0x017D
  | 0x91 -> 0x2018
  | 0x92 -> 0x2019
  | 0x93 -> 0x201C
  | 0x94 -> 0x201D
  | 0x95 -> 0x2022
  | 0x96 -> 0x2013
  | 0x97 -> 0x2014
  | 0x98 -> 0x02DC
  | 0x99 -> 0x2122
  | 0x9A -> 0x0161
  | 0x9B -> 0x203A
  | 0x9C -> 0x0153
  | 0x9E -> 0x017E
  | 0x9F -> 0x0178
  | c -> c

let named_entity_trie =
  lazy begin
    let trie = Trie.create () in
    Array.fold_left (fun trie (name, characters) ->
      Trie.add name characters trie)
      trie
      Entities.entities
  end

type doctype_buffers =
  {mutable doctype_name      : Buffer.t option;
   mutable public_identifier : Buffer.t option;
   mutable system_identifier : Buffer.t option;
   mutable force_quirks      : bool}

module Doctype_buffers =
struct
  type t = doctype_buffers =
    {mutable doctype_name      : Buffer.t option;
     mutable public_identifier : Buffer.t option;
     mutable system_identifier : Buffer.t option;
     mutable force_quirks      : bool}
end

let add_doctype_char buffer c =
  let buffer =
    match buffer with
    | None -> Buffer.create 32
    | Some buffer -> buffer
  in
  add_utf_8 buffer c;
  Some buffer

type tag_buffers =
  {mutable start        : bool;
   tag_name             : Buffer.t;
   mutable self_closing : bool;
   mutable attributes   : (string * string) list}

module Tag_buffers =
struct
  type t = tag_buffers =
    {mutable start        : bool;
     tag_name             : Buffer.t;
     mutable self_closing : bool;
     mutable attributes   : (string * string) list}
end

let sequence_to_lowercase = List.map (fun (l, c) -> l, to_lowercase c)

open Kstream

let tokenize report (input, get_location) =
  let foreign = ref (fun () -> false) in

  let last_start_tag_name : string option ref = ref None in

  let is_appropriate_end_tag name_buffer =
    match !last_start_tag_name with
    | None -> false
    | Some name -> Buffer.contents name_buffer = name
  in

  let throw = ref (fun _ -> ()) in
  let ended = ref (fun _ -> ()) in
  let output = ref (fun _ -> ()) in

  let rec current_state = ref data_state

  and emit t s = current_state := s; !output t

  and emit_character l c s = emit (l, `Char c) s

  and emit_characters cs s =
    match cs with
    | [] -> s ()
    | (l, c)::cs -> emit_character l c (fun () -> emit_characters cs s)

  and emit_eof () =
    emit (get_location (), `EOF) (fun () -> !ended ())

  and emit_tag l tag' =
    let rec rev_deduplicate accumulator seen attributes k =
      match attributes with
      | [] -> k accumulator
      | (n, v)::more ->
        if list_mem_string n seen then
          report l (`Bad_token (n, "tag", "duplicate attribute")) !throw
            (fun () ->
          rev_deduplicate accumulator seen more k)
        else rev_deduplicate ((n, v)::accumulator) (n::seen) more k
    in

    rev_deduplicate [] [] (List.rev tag'.Tag_buffers.attributes)
      (fun attributes ->

    let tag =
      {Token_tag.name = Buffer.contents tag'.tag_name;
       self_closing   = tag'.self_closing;
       attributes     = List.rev attributes}
    in

    (fun k ->
      if tag'.start then begin
        last_start_tag_name := Some tag.name;
        k (`Start tag)
      end
      else
        (fun k ->
          match attributes with
          | (n, _)::_ ->
            report l (`Bad_token (n, "tag", "end tag with attributes")) !throw k
          | _ -> k ())
        @@ (fun k () ->
          if tag.Token_tag.self_closing then
            report l (`Bad_token ("/>", "tag",
                                  "end tag cannot be self-closing")) !throw k
          else k ())
        @@ (fun () -> k (`End tag)))

    (fun token ->
      emit (l, token) data_state))

  and emit_comment l buffer =
    emit (l, `Comment (Buffer.contents buffer)) data_state

  and emit_doctype ?(quirks = false) l doctype =
    if quirks then doctype.Doctype_buffers.force_quirks <- true;

    let if_not_missing = function
      | None -> None
      | Some buffer -> Some (Buffer.contents buffer)
    in

    let doctype =
      {Common.doctype_name = if_not_missing doctype.doctype_name;
       public_identifier   = if_not_missing doctype.public_identifier;
       system_identifier   = if_not_missing doctype.system_identifier;
       raw_text            = None;
       force_quirks        = doctype.force_quirks}
    in

    emit (l, `Doctype doctype) data_state

  (* Implementation of 8.2.4.69 Tokenizing character references. *)
  and consume_character_reference in_attribute additional location k =
    peek_option input !throw (function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020 | 0x003C | 0x0026))
      | None ->
        k None

      | Some (_, c) when Some c = additional -> k None

      | Some (_, 0x0023 as pound) ->
        let consume_digits filter k =
          let buffer = Buffer.create 8 in

          let rec iterate () =
            next_option input !throw (function
              | Some (_, c) when filter c ->
                Buffer.add_char buffer (Char.chr c); iterate ()
              | v ->
                push_option input v;
                if Buffer.length buffer = 0 then k None
                else k (Some (Buffer.contents buffer)))
          in
          iterate ()
        in

        let finish_digits prefix text s =
          let consume_semicolon k =
            next_option input !throw begin function
              | Some (_, 0x003B) -> k ";"

              | v ->
                push_option input v;
                report location
                  (`Bad_token (prefix ^ text, "character reference",
                               "missing ';' at end")) !throw (fun () ->
                k "")
            end
          in

          let convert s semicolon k' =
            let maybe_n =
              try Some (int_of_string s)
              with Failure _ -> None
            in

            match maybe_n with
            | Some n -> k' n
            | None ->
              report location
                (`Bad_token (prefix ^ text ^ semicolon, "character reference",
                             "out of range")) !throw (fun () ->
              k (Some (`One u_rep)))
          in

          consume_semicolon begin fun semicolon ->
            convert s semicolon begin fun n' ->
              let n = replace_windows_1252_entity n' in

              if n <> n' then
                report location
                  (`Bad_token (prefix ^ text ^ semicolon, "character reference",
                               "Windows-1252 character")) !throw (fun () ->
                k (Some (`One n)))

              else
                match n with
                | n when not @@ is_scalar n || n = 0 ->
                  report location
                    (`Bad_token (prefix ^ text ^ semicolon,
                                 "character reference", "out of range"))
                    !throw (fun () ->
                  k (Some (`One u_rep)))

                | n when is_control_character n || is_non_character n ->
                  report location
                    (`Bad_token (prefix ^ text ^ semicolon,
                                 "character reference",
                                 "invalid HTML character")) !throw (fun () ->
                  k (Some (`One n)))

                | n -> k (Some (`One n))
              end
            end
        in

        next_expected input !throw (fun _ ->
          peek_option input !throw (function
            | Some (_, (0x0078 | 0x0058 as c) as x) ->
              let prefix = Printf.sprintf "&#%c" (Char.chr c) in

              next_expected input !throw (fun _ ->
                consume_digits is_hex_digit (function
                  | None ->
                    push_list input [pound; x];

                    report location (`Bad_token
                      (prefix, "character reference", "expected digits"))
                      !throw (fun () ->
                    k None)

                  | Some s -> finish_digits prefix s ("0x" ^ s)))

            | _ ->
              let prefix = "&#" in

              consume_digits is_digit (function
                | None ->
                  push input pound;

                  report location (`Bad_token
                    (prefix, "character reference", "expected digits"))
                    !throw (fun () ->
                  k None)

                | Some s -> finish_digits prefix s s)))

      | _ ->
        let is_entity_like k =
          let finish replace text =
            push_list input (List.rev replace); k text in

          let buffer = Buffer.create 16 in

          let rec iterate replace =
            next_option input !throw (function
              | None -> finish replace None
              | Some ((_, c) as v) when is_alphanumeric c ->
                Buffer.add_char buffer (Char.chr c); iterate (v::replace)
              | Some ((_, 0x003B) as v) ->
                finish (v::replace) (Some (Buffer.contents buffer))
              | Some v -> finish (v::replace) None)
          in
          iterate []
        in

        let finish best matched replace =
          push_list input (List.rev replace);
          match best with
          | None ->
            is_entity_like (function
              | None -> k None
              | Some s ->
                report location
                  (`Bad_token ("&" ^ s ^ ";", "entity reference",
                               "no such entity")) !throw (fun () ->
                k None))
          | Some (text, code_points) ->
            next_option input !throw (function
              | Some (_, 0x003B) -> k (Some code_points)
              | maybe_v ->
                let unterminated () =
                  push_option input maybe_v;

                  report location
                    (`Bad_token ("&" ^ text, "entity reference",
                     "missing ';' at end")) !throw (fun () ->
                  k (Some code_points))
                in

                if not in_attribute then unterminated ()
                else
                  match maybe_v with
                  | Some ((_, c) as v) when is_alphanumeric c ->
                    push_list input (List.rev (v::matched));
                    k None
                  | Some ((_, 0x003D) as v) ->
                    push_list input (List.rev (v::matched));

                    report location
                      (`Bad_token ("&" ^ text ^ "=", "attribute",
                        "unterminated entity reference followed by '='"))
                      !throw(fun () ->
                    k None)
                  | _ -> unterminated ())
        in

        let rec match_named best matched replace trie text =
          next_option input !throw (function
            | None ->
              finish best matched replace
            | Some ((_, c) as v) ->
              let trie = Trie.advance c trie in
              add_utf_8 text c;
              match Trie.matches trie with
              | Trie.No ->
                finish best matched (v::replace)
              | Trie.Prefix ->
                match_named best matched (v::replace) trie text
              | Trie.Multiple m ->
                let w = Buffer.contents text in
                match_named (Some (w, m)) (v::(replace @ matched)) [] trie text
              | Trie.Yes m ->
                let w = Buffer.contents text in
                finish (Some (w, m)) (v::(replace @ matched)) [])
        in
        match_named
          None [] [] (Lazy.force named_entity_trie) (Buffer.create 16))

  (* 8.2.4.1. *)
  and data_state () =
    next_option input !throw begin function
      | Some (l, 0x0026) ->
        character_reference_state data_state l

      | Some (l, 0x003C) ->
        tag_open_state l

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "content", "null")) !throw (fun () ->
        emit (l, `Char 0) data_state)

      | None ->
        emit_eof ()

      | Some (l, c) ->
        emit (l, `Char c) data_state
    end

  (* 8.2.4.2, 8.2.4.4. *)
  and character_reference_state state l =
    consume_character_reference false None l begin function
      | None ->
        emit (l, `Char 0x0026) state

      | Some (`One c) ->
        emit (l, `Char c) state

      | Some (`Two (c, c')) ->
        emit (l, `Char c) (fun () ->
        emit (l, `Char c') state)
    end

  (* 8.2.4.3. *)
  and rcdata_state () =
    next_option input !throw begin function
      | Some (l, 0x0026) ->
        character_reference_state rcdata_state l

      | Some (l, 0x003C as v) ->
        text_less_than_sign_state rcdata_state l [v]

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "content", "null")) !throw (fun () ->
        emit (l, `Char u_rep) rcdata_state)

      | None ->
        emit_eof ()

      | Some (l, c) ->
        emit (l, `Char c) rcdata_state
    end

  (* 8.2.4.5. *)
  and rawtext_state () =
    next_option input !throw begin function
      | Some (l, 0x003C as v) ->
        text_less_than_sign_state rawtext_state l [v]

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "content", "null")) !throw (fun () ->
        emit (l, `Char u_rep) rawtext_state)

      | None ->
        emit_eof ()

      | Some (l, c) ->
        emit (l, `Char c) rawtext_state
    end

  (* 8.2.4.6. *)
  and script_data_state () =
    next_option input !throw begin function
      | Some (l, 0x003C as v) ->
        script_data_less_than_sign_state l [v]

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "content", "null")) !throw (fun () ->
        emit_character l u_rep script_data_state)

      | None ->
        emit_eof ()

      | Some (l, c) ->
        emit_character l c script_data_state
    end

  (* 8.2.4.7. *)
  and plaintext_state () =
    next_option input !throw begin function
      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "content", "null")) !throw (fun () ->
        emit (l, `Char u_rep) plaintext_state)

      | None ->
        emit_eof ()

      | Some (l, c) ->
        emit (l, `Char c) plaintext_state
    end

  (* 8.2.4.8. *)
  and tag_open_state l' =
    let tag =
      {start        = true;
       tag_name     = Buffer.create 16;
       self_closing = false;
       attributes   = []}
    in

    next_option input !throw begin function
      | Some (_, 0x0021) ->
        markup_declaration_open_state l'

      | Some (_, 0x002F) ->
        end_tag_open_state l' tag

      | Some (_, c) when is_alphabetic c ->
        add_utf_8 tag.tag_name (to_lowercase c);
        tag_name_state l' tag

      | Some (_, 0x003F) ->
        report l'
          (`Bad_token ("<?", "content",
                       "HTML does not have processing instructions"))
          !throw (fun () ->
        bogus_comment_state l')

      | Some ((l, c) as v) ->
        report l
          (`Bad_token (char c, "tag",
                       "invalid start character")) !throw (fun () ->
        push input v;
        emit_character l' 0x003C data_state)

      | None ->
        report (get_location ()) (`Unexpected_eoi "tag") !throw (fun () ->
        emit_character l' 0x003C data_state)
    end

  (* 8.2.4.9. *)
  and end_tag_open_state l' tag =
    tag.start <- false;

    next_option input !throw begin function
      | Some (_, c) when is_alphabetic c ->
        add_utf_8 tag.tag_name (to_lowercase c);
        tag_name_state l' tag

      | Some (_, 0x003E) ->
        report l' (`Bad_token ("</>", "tag", "no tag name")) !throw data_state

      | None ->
        report (get_location ()) (`Unexpected_eoi "tag") !throw (fun () ->
        let line, column = l' in
        emit (l', `Char 0x003C) (fun () ->
        emit ((line, column + 1), `Char 0x002F) data_state))

      | Some (l, c) ->
        report l
          (`Bad_token (char c, "tag",
                       "invalid start character")) !throw (fun () ->
        bogus_comment_state l')
    end

  (* 8.2.4.10. *)
  and tag_name_state l' tag =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        before_attribute_name_state l' tag

      | Some (_, 0x002F) ->
        self_closing_start_tag_state l' tag

      | Some (_, 0x003E) ->
        emit_tag l' tag

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "tag name", "null")) !throw (fun () ->
        add_utf_8 tag.tag_name u_rep;
        tag_name_state l' tag)

      | None ->
        report (get_location ()) (`Unexpected_eoi "tag") !throw data_state

      | Some (_, c) ->
        add_utf_8 tag.tag_name (to_lowercase c);
        tag_name_state l' tag
    end

  (* 8.2.4.11, 8.2.4.14. *)
  and text_less_than_sign_state state l' cs =
    next_option input !throw begin function
      | Some (_, 0x002F as v) ->
        text_end_tag_open_state state l' (v::cs)

      | maybe_v ->
        push_option input maybe_v;
        emit_characters cs state
    end

  (* 8.2.4.12, 8.2.4.15, 8.2.4.18, 8.2.4.26. *)
  and text_end_tag_open_state state l' cs =
    next_option input !throw begin function
      | Some (_, c as v) when is_alphabetic c ->
        let name_buffer = Buffer.create 32 in
        add_utf_8 name_buffer (to_lowercase c);
        text_end_tag_name_state state l' (v::cs) name_buffer

      | maybe_v ->
        push_option input maybe_v;
        emit_characters (List.rev cs) state
    end

  (* 8.2.4.13, 8.2.4.16, 8.2.4.19, 8.2.4.27. *)
  and text_end_tag_name_state state l' cs name_buffer =
    let create_tag () =
      {start        = false;
       tag_name     = name_buffer;
       self_closing = false;
       attributes   = []}
    in

    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020))
          when is_appropriate_end_tag name_buffer ->
        before_attribute_name_state l' (create_tag ())

      | Some (_, 0x002F) when is_appropriate_end_tag name_buffer ->
        self_closing_start_tag_state l' (create_tag ())

      | Some (_, 0x003E) when is_appropriate_end_tag name_buffer ->
        emit_tag l' (create_tag ())

      | Some ((_, c) as v) when is_alphabetic c ->
        add_utf_8 name_buffer (to_lowercase c);
        text_end_tag_name_state state l' (v::cs) name_buffer

      | maybe_v ->
        push_option input maybe_v;
        emit_characters (List.rev cs) state
    end

  (* 8.2.4.17. *)
  and script_data_less_than_sign_state l' cs =
    next_option input !throw begin function
      | Some (_, 0x002F as v) ->
        text_end_tag_open_state script_data_state l' (v::cs)

      | Some (_, 0x0021 as v) ->
        emit_characters (List.rev (v::cs)) (fun () ->
        script_data_escape_start_state l')

      | maybe_v ->
        push_option input maybe_v; emit_characters cs script_data_state
    end

  (* 8.2.4.20. *)
  and script_data_escape_start_state l' =
    next_option input !throw begin function
      | Some (l, 0x002D) ->
        emit_character l 0x002D (fun () ->
        script_data_escape_start_dash_state l')

      | maybe_v ->
        push_option input maybe_v;
        script_data_state ()
    end

  (* 8.2.4.21. *)
  and script_data_escape_start_dash_state l' =
    next_option input !throw begin function
      | Some (l, 0x002D) ->
        emit_character l 0x002D (fun () ->
        script_data_escaped_dash_dash_state l')

      | maybe_v ->
        push_option input maybe_v;
        script_data_state ()
    end

  (* 8.2.4.22. *)
  and script_data_escaped_state l' =
    next_option input !throw begin function
      | Some (l, 0x002D) ->
        emit_character l 0x002D (fun () ->
        script_data_escaped_dash_state l')

      | Some ((l, 0x003C) as v) ->
        script_data_escaped_less_than_sign_state l' l [v]

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "script", "null")) !throw (fun () ->
        emit_character l u_rep (fun () ->
        script_data_escaped_state l'))

      | None ->
        report (get_location ()) (`Unexpected_eoi "script") !throw data_state

      | Some (l, c) ->
        emit_character l c (fun () ->
        script_data_escaped_state l')
    end

  (* 8.2.4.23. *)
  and script_data_escaped_dash_state l' =
    next_option input !throw begin function
      | Some (l, 0x002D) ->
        emit_character l 0x002D (fun () ->
        script_data_escaped_dash_dash_state l')

      | Some (l, 0x003C as v) ->
        script_data_escaped_less_than_sign_state l' l [v]

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "script", "null")) !throw (fun () ->
        emit_character l u_rep (fun () ->
        script_data_escaped_state l'))

      | None ->
        report (get_location ()) (`Unexpected_eoi "script") !throw data_state

      | Some (l, c) ->
        emit_character l c (fun () ->
        script_data_escaped_state l')
    end

  (* 8.2.4.24. *)
  and script_data_escaped_dash_dash_state l' =
    next_option input !throw begin function
      | Some (l, 0x002D) ->
        emit_character l 0x002D (fun () ->
        script_data_escaped_dash_dash_state l')

      | Some (l, 0x003C as v) ->
        script_data_escaped_less_than_sign_state l' l [v]

      | Some (l, 0x003E) ->
        emit_character l 0x003E script_data_state

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "script", "null")) !throw (fun () ->
        emit_character l u_rep (fun () ->
        script_data_escaped_state l'))

      | None ->
        report (get_location ()) (`Unexpected_eoi "script") !throw data_state

      | Some (l, c) ->
        emit_character l c (fun () ->
        script_data_escaped_state l')
    end

  (* 8.2.4.25. *)
  and script_data_escaped_less_than_sign_state l' l'' cs =
    next_option input !throw begin function
      | Some (_, 0x002F as v) ->
        text_end_tag_open_state (fun () ->
        script_data_escaped_state l') l'' (v::cs)

      | Some (_, c as v) when is_alphabetic c ->
        let tag_buffer = Buffer.create 32 in
        add_utf_8 tag_buffer (to_lowercase c);
        emit_characters (List.rev (v::cs)) (fun () ->
        script_data_double_escape_start_state l' tag_buffer)

      | maybe_v ->
        push_option input maybe_v;
        emit_characters cs (fun () ->
        script_data_escaped_state l')
    end

  (* 8.2.4.28. *)
  and script_data_double_escape_start_state l' tag_buffer =
    next_option input !throw begin function
      | Some (l, (0x0009 | 0x000A | 0x000C | 0x0020 | 0x002F | 0x003E as c)) ->
        emit_character l c (fun () ->
        if Buffer.contents tag_buffer = "script" then
          script_data_double_escaped_state l'
        else script_data_escaped_state l')

      | Some (l, c) when is_alphabetic c ->
        add_utf_8 tag_buffer (to_lowercase c);
        emit_character l c (fun () ->
        script_data_double_escape_start_state l' tag_buffer)

      | maybe_v ->
        push_option input maybe_v;
        script_data_escaped_state l'
    end

  (* 8.2.4.29. *)
  and script_data_double_escaped_state l' =
    next_option input !throw begin function
      | Some (l, 0x002D) ->
        emit_character l 0x002D (fun () ->
        script_data_double_escaped_dash_state l')

      | Some (l, 0x003C) ->
        emit_character l 0x003C (fun () ->
        script_data_double_escaped_less_than_sign_state l')

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "script", "null")) !throw (fun () ->
        emit_character l u_rep (fun () ->
        script_data_double_escaped_state l'))

      | None ->
        report (get_location ()) (`Unexpected_eoi "script") !throw data_state

      | Some (l, c) ->
        emit_character l c (fun () ->
        script_data_double_escaped_state l')
    end

  (* 8.2.4.30. *)
  and script_data_double_escaped_dash_state l' =
    next_option input !throw begin function
      | Some (l, 0x002D) ->
        emit_character l 0x002D (fun () ->
        script_data_double_escaped_dash_dash_state l')

      | Some (l, 0x003C) ->
        emit_character l 0x003C (fun () ->
        script_data_double_escaped_less_than_sign_state l')

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "script", "null")) !throw (fun () ->
        emit_character l u_rep (fun () ->
        script_data_double_escaped_state l'))

      | None ->
        report (get_location ()) (`Unexpected_eoi "script") !throw data_state

      | Some (l, c) ->
        emit_character l c (fun () ->
        script_data_double_escaped_state l')
    end

  (* 8.2.4.31. *)
  and script_data_double_escaped_dash_dash_state l' =
    next_option input !throw begin function
      | Some (l, 0x002D) ->
        emit_character l 0x002D (fun () ->
        script_data_double_escaped_dash_dash_state l')

      | Some (l, 0x003C) ->
        emit_character l 0x003C (fun () ->
        script_data_double_escaped_less_than_sign_state l')

      | Some (l, 0x003E) ->
        emit_character l 0x003E script_data_state

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "script", "null")) !throw (fun () ->
        emit_character l u_rep (fun () ->
        script_data_double_escaped_state l'))

      | None ->
        report (get_location ()) (`Unexpected_eoi "script") !throw data_state

      | Some (l, c) ->
        emit_character l c (fun () ->
        script_data_double_escaped_state l')
    end

  (* 8.2.4.32. *)
  and script_data_double_escaped_less_than_sign_state l' =
    next_option input !throw begin function
      | Some (l, 0x002F) ->
        let tag_buffer = Buffer.create 32 in
        emit_character l 0x002F (fun () ->
        script_data_double_escape_end_state l' tag_buffer)

      | maybe_v ->
        push_option input maybe_v;
        script_data_double_escaped_state l'
    end

  (* 8.2.4.33. *)
  and script_data_double_escape_end_state l' tag_buffer =
    next_option input !throw begin function
      | Some (l, (0x0009 | 0x000A | 0x000C | 0x0020 | 0x002F | 0x003E as c)) ->
        emit_character l c (fun () ->
        if Buffer.contents tag_buffer = "script" then
          script_data_escaped_state l'
        else script_data_double_escaped_state l')

      | Some (l, c) when is_alphabetic c ->
        add_utf_8 tag_buffer (to_lowercase c);
        emit_character l c (fun () ->
        script_data_double_escape_end_state l' tag_buffer)

      | maybe_v ->
        push_option input maybe_v;
        script_data_double_escaped_state l'
    end

  (* 8.2.4.34. *)
  and before_attribute_name_state l' tag =
    let start_attribute c =
      let name_buffer = Buffer.create 32 in
      add_utf_8 name_buffer c;
      attribute_name_state l' tag name_buffer
    in

    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        before_attribute_name_state l' tag

      | Some (_, 0x002F) ->
        self_closing_start_tag_state l' tag

      | Some (_, 0x003E) ->
        emit_tag l' tag

      | None ->
        report (get_location ()) (`Unexpected_eoi "tag") !throw data_state

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "attribute name", "null")) !throw
          (fun () ->
        start_attribute u_rep)

      | Some (l, (0x0022 | 0x0027 | 0x003C | 0x003D as c)) ->
        report l (`Bad_token (char c, "attribute name",
                              "invalid start character")) !throw (fun () ->
        start_attribute c)

      | Some (_, c) ->
        start_attribute (to_lowercase c)
    end

  (* 8.2.4.35. *)
  and attribute_name_state l' tag name_buffer =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        after_attribute_name_state l' tag (Buffer.contents name_buffer)

      | Some (_, 0x002F) ->
        tag.attributes <- (Buffer.contents name_buffer, "")::tag.attributes;
        self_closing_start_tag_state l' tag

      | Some (_, 0x003D) ->
        before_attribute_value_state l' tag (Buffer.contents name_buffer)

      | Some (_, 0x003E) ->
        tag.attributes <- (Buffer.contents name_buffer, "")::tag.attributes;
        emit_tag l' tag

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "attribute name", "null")) !throw
          (fun () ->
        add_utf_8 name_buffer u_rep;
        attribute_name_state l' tag name_buffer)

      | Some (l, (0x0022 | 0x0027 | 0x003C as c)) ->
        report l (`Bad_token (char c, "attribute name",
                              "invalid name character")) !throw (fun () ->
        add_utf_8 name_buffer c;
        attribute_name_state l' tag name_buffer)

      | None ->
        report (get_location ()) (`Unexpected_eoi "tag") !throw data_state

      | Some (_, c) ->
        add_utf_8 name_buffer (to_lowercase c);
        attribute_name_state l' tag name_buffer
    end

  (* 8.2.4.36. *)
  and after_attribute_name_state l' tag name =
    let start_next_attribute c =
      tag.attributes <- (name, "")::tag.attributes;
      let name_buffer = Buffer.create 32 in
      add_utf_8 name_buffer c;
      attribute_name_state l' tag name_buffer
    in

    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        after_attribute_name_state l' tag name

      | Some (_, 0x002F) ->
        tag.attributes <- (name, "")::tag.attributes;
        self_closing_start_tag_state l' tag

      | Some (_, 0x003D) ->
        before_attribute_value_state l' tag name

      | Some (_, 0x003E) ->
        tag.attributes <- (name, "")::tag.attributes;
        emit_tag l' tag

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "attribute name", "null")) !throw
          (fun () ->
        start_next_attribute u_rep)

      | Some (l, (0x0022 | 0x0027 | 0x003C as c)) ->
        report l (`Bad_token (char c, "attribute name",
                              "invalid start character")) !throw (fun () ->
        start_next_attribute c)

      | None ->
        report (get_location ()) (`Unexpected_eoi "tag") !throw data_state

      | Some (_, c) ->
        start_next_attribute (to_lowercase c)
    end

  (* 8.2.4.37. *)
  and before_attribute_value_state l' tag name =
    let start_value state maybe_c =
      let value_buffer = Buffer.create 32 in
      begin match maybe_c with
      | None -> ()
      | Some c -> add_utf_8 value_buffer c
      end;
      state l' tag name value_buffer
    in

    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        before_attribute_value_state l' tag name

      | Some (_, (0x0022 | 0x0027 as c)) ->
        start_value (attribute_value_quoted_state c) None

      | Some (_, 0x0026 as v) ->
        push input v;
        start_value attribute_value_unquoted_state None

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "attribute value", "null")) !throw
          (fun () ->
        start_value attribute_value_unquoted_state (Some u_rep))

      | Some (l, 0x003E) ->
        report l (`Bad_token (">", "tag", "expected attribute value after '='"))
          !throw (fun () ->
        tag.attributes <- (name, "")::tag.attributes;
        emit_tag l' tag)

      | Some (l, (0x003C | 0x003D | 0x0060 as c)) ->
        report l (`Bad_token (char c, "attribute value",
                              "invalid start character")) !throw (fun () ->
        start_value attribute_value_unquoted_state (Some c))

      | None ->
        report (get_location ()) (`Unexpected_eoi "tag") !throw data_state

      | Some (_, c) ->
        start_value attribute_value_unquoted_state (Some c)
    end

  (* 8.2.4.38 and 8.2.4.39. *)
  and attribute_value_quoted_state quote l' tag name value_buffer =
    next_option input !throw begin function
      | Some (_, c) when c = quote ->
        tag.attributes <-
          (name, Buffer.contents value_buffer)::tag.attributes;
        after_attribute_value_quoted_state l' tag

      | Some (l, 0x0026) ->
        character_reference_in_attribute quote l value_buffer (fun () ->
        attribute_value_quoted_state quote l' tag name value_buffer)

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "attribute value", "null")) !throw
          (fun () ->
        add_utf_8 value_buffer u_rep;
        attribute_value_quoted_state quote l' tag name value_buffer)

      | None ->
        report (get_location ()) (`Unexpected_eoi "attribute value") !throw
          data_state

      | Some (_, c) ->
        add_utf_8 value_buffer c;
        attribute_value_quoted_state quote l' tag name value_buffer
    end

  (* 8.2.4.40. *)
  and attribute_value_unquoted_state l' tag name value_buffer =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        tag.attributes <-
          (name, Buffer.contents value_buffer)::tag.attributes;
        before_attribute_name_state l' tag

      | Some (l, 0x0026) ->
        character_reference_in_attribute 0x003E l value_buffer (fun () ->
        attribute_value_unquoted_state l' tag name value_buffer)

      | Some (_, 0x003E) ->
        tag.attributes <-
          (name, Buffer.contents value_buffer)::tag.attributes;
        emit_tag l' tag

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "attribute value", "null")) !throw
          (fun () ->
        add_utf_8 value_buffer u_rep;
        attribute_value_unquoted_state l' tag name value_buffer)

      | Some (l, (0x0022 | 0x0027 | 0x003C | 0x003D | 0x0060 as c)) ->
        report l (`Bad_token (char c, "attribute value",
                              "invalid character")) !throw (fun () ->
        add_utf_8 value_buffer c;
        attribute_value_unquoted_state l' tag name value_buffer)

      | None ->
        report (get_location ()) (`Unexpected_eoi "tag") !throw data_state

      | Some (_, c) ->
        add_utf_8 value_buffer c;
        attribute_value_unquoted_state l' tag name value_buffer
    end

  (* 8.2.4.41. *)
  and character_reference_in_attribute allowed l value_buffer k =
    consume_character_reference true (Some allowed) l begin function
      | None ->
        add_utf_8 value_buffer 0x0026;
        k ()

      | Some (`One c) ->
        add_utf_8 value_buffer c;
        k ()

      | Some (`Two (c, c')) ->
        add_utf_8 value_buffer c;
        add_utf_8 value_buffer c';
        k ()
    end

  (* 8.2.4.42. *)
  and after_attribute_value_quoted_state l' tag =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        before_attribute_name_state l' tag

      | Some (_, 0x002F) ->
        self_closing_start_tag_state l' tag

      | Some (_, 0x003E) ->
        emit_tag l' tag

      | None ->
        report (get_location ()) (`Unexpected_eoi "tag") !throw data_state

      | Some (l, c as v) ->
        push input v;
        report l (`Bad_token (char c, "tag",
                              "expected whitespace before attribute"))
          !throw (fun () ->
        before_attribute_name_state l' tag)
    end

  (* 8.2.4.43. *)
  and self_closing_start_tag_state l' tag =
    next_option input !throw begin function
      | Some (_, 0x003E) ->
        tag.self_closing <- true;
        emit_tag l' tag

      | None ->
        report (get_location ()) (`Unexpected_eoi "tag") !throw data_state

      | Some (l, c as v) ->
        push input v;
        report l
          (`Bad_token (char c, "tag", "expected '/>'")) !throw (fun () ->
        before_attribute_name_state l' tag)
    end

  (* 8.2.4.44. *)
  and bogus_comment_state l' =
    let buffer = Buffer.create 256 in
    let rec consume () =
      next_option input !throw begin function
        | Some (_, 0x003E) ->
          emit_comment l' buffer

        | Some (_, 0) ->
          add_utf_8 buffer u_rep;
          consume ()

        | None ->
          emit_comment l' buffer

        | Some (_, c) ->
          add_utf_8 buffer c;
          consume ()
      end
    in
    consume ()

  (* 8.2.4.45. *)
  and markup_declaration_open_state l' =
    peek_n 2 input !throw begin function
      | [_, 0x002D; _, 0x002D] ->
        next_n 2 input !throw (fun _ ->
        comment_start_state l' (Buffer.create 64))

      | _ ->
        peek_n 7 input !throw begin fun l ->
          match sequence_to_lowercase l with
          | [_, 0x64; _, 0x6F; _, 0x63; _, 0x74; _, 0x79; _, 0x70; _, 0x65] ->
            next_n 7 input !throw (fun _ ->
            doctype_state l')

          | _ ->
            peek_n 7 input !throw (function
              | [_, 0x5B; _, 0x43; _, 0x44; _, 0x41;
                 _, 0x54; _, 0x41; _, 0x5B] ->
                if !foreign () then
                  next_n 7 input !throw (fun _ ->
                  cdata_section_state ())
                else
                  report l'
                    (`Bad_token ("<![CDATA[", "content",
                                 "CDATA sections not allowed in HTML"))
                    !throw (fun () ->
                  bogus_comment_state l')

              | _ ->
                report l'
                  (`Bad_token ("<!", "comment", "should begin with '<!--'"))
                  !throw (fun () ->
                bogus_comment_state l'))
        end
    end

  (* 8.2.4.46. *)
  and comment_start_state l' buffer =
    next_option input !throw begin function
      | Some (_, 0x002D) ->
        comment_start_dash_state l' buffer

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "comment", "null")) !throw (fun () ->
        add_utf_8 buffer u_rep;
        comment_state l' buffer)

      | Some (_, 0x003E) ->
        report l' (`Bad_token ("<!-->", "comment", "'-->' overlaps '<!--'"))
          !throw (fun () ->
        emit_comment l' buffer)

      | None ->
        report (get_location ()) (`Unexpected_eoi "comment") !throw (fun () ->
        emit_comment l' buffer)

      | Some (_, c) ->
        add_utf_8 buffer c;
        comment_state l' buffer
    end

  (* 8.2.4.47. *)
  and comment_start_dash_state l' buffer =
    next_option input !throw begin function
      | Some (_, 0x002D) ->
        comment_end_state l' buffer

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "comment", "null")) !throw (fun () ->
        Buffer.add_char buffer '-';
        add_utf_8 buffer u_rep;
        comment_state l' buffer)

      | Some (_, 0x003E) ->
        report l' (`Bad_token ("<!--->", "comment", "'-->' overlaps '<!--'"))
          !throw (fun () ->
        emit_comment l' buffer)

      | None ->
        report (get_location ()) (`Unexpected_eoi "comment") !throw (fun () ->
        emit_comment l' buffer)

      | Some (_, c) ->
        Buffer.add_char buffer '-';
        add_utf_8 buffer c;
        comment_state l' buffer
    end

  (* 8.2.4.48. *)
  and comment_state l' buffer =
    next_option input !throw begin function
      | Some (_, 0x002D) ->
        comment_end_dash_state l' buffer

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "comment", "null")) !throw (fun () ->
        add_utf_8 buffer u_rep;
        comment_state l' buffer)

      | None ->
        report (get_location ()) (`Unexpected_eoi "comment") !throw (fun () ->
        emit_comment l' buffer)

      | Some (_, c) ->
        add_utf_8 buffer c;
        comment_state l' buffer
    end

  (* 8.2.4.49. *)
  and comment_end_dash_state l' buffer =
    next_option input !throw begin function
      | Some (_, 0x002D) ->
        comment_end_state l' buffer

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "comment", "null")) !throw (fun () ->
        Buffer.add_char buffer '-';
        add_utf_8 buffer u_rep;
        comment_state l' buffer)

      | None ->
        report (get_location ()) (`Unexpected_eoi "comment") !throw (fun () ->
        emit_comment l' buffer)

      | Some (_, c) ->
        Buffer.add_char buffer '-';
        add_utf_8 buffer c;
        comment_state l' buffer
    end

  (* 8.2.4.50. *)
  and comment_end_state l' buffer =
    next_option input !throw begin function
      | Some (_, 0x003E) ->
        emit_comment l' buffer

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "comment", "null")) !throw (fun () ->
        Buffer.add_string buffer "--";
        add_utf_8 buffer u_rep;
        comment_state l' buffer)

      | Some (l, 0x0021) ->
        report l (`Bad_token ("--!", "comment", "'--' should be in '-->'"))
          !throw (fun () ->
        comment_end_bang_state l' buffer)

      | Some (l, 0x002D) ->
        report l (`Bad_token ("---", "comment", "'--' should be in '-->'"))
          !throw (fun () ->
        Buffer.add_char buffer '-';
        comment_end_state l' buffer)

      | None ->
        report (get_location ()) (`Unexpected_eoi "comment") !throw (fun () ->
        emit_comment l' buffer)

      | Some (l, c) ->
        report l (`Bad_token ("--" ^ (char c), "comment",
                              "'--' should be in '-->'")) !throw (fun () ->
        Buffer.add_string buffer "--";
        add_utf_8 buffer c;
        comment_state l' buffer)
    end

  (* 8.2.4.51. *)
  and comment_end_bang_state l' buffer =
    next_option input !throw begin function
      | Some (_, 0x002D) ->
        Buffer.add_string buffer "--!";
        comment_end_dash_state l' buffer

      | Some (_, 0x003E) ->
        emit_comment l' buffer

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "comment", "null")) !throw (fun () ->
        Buffer.add_string buffer "--!";
        add_utf_8 buffer u_rep;
        comment_state l' buffer)

      | None ->
        report (get_location ()) (`Unexpected_eoi "comment") !throw (fun () ->
        emit_comment l' buffer)

      | Some (_, c) ->
        Buffer.add_string buffer "--!";
        add_utf_8 buffer c;
        comment_state l' buffer
    end

  (* 8.2.5.52. *)
  and doctype_state l' =
    let doctype =
      {doctype_name      = None;
       public_identifier = None;
       system_identifier = None;
       force_quirks      = false}
    in

    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        before_doctype_name_state l' doctype

      | None ->
        report (get_location ()) (`Unexpected_eoi "doctype") !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | Some (l, c as v) ->
        report l (`Bad_token (char c, "doctype",
                              "expected whitespace")) !throw (fun () ->
        push input v;
        before_doctype_name_state l' doctype)
    end

  (* 8.2.5.53. *)
  and before_doctype_name_state l' doctype =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        before_doctype_name_state l' doctype

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "doctype", "null")) !throw (fun () ->
        doctype.doctype_name <- add_doctype_char doctype.doctype_name u_rep;
        doctype_name_state l' doctype)

      | Some (l, 0x003E) ->
        report l (`Bad_token (">", "doctype", "expected name")) !throw
          (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | None ->
        report (get_location ()) (`Unexpected_eoi "doctype") !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | Some (_, c) ->
        doctype.doctype_name <-
          add_doctype_char doctype.doctype_name (to_lowercase c);
        doctype_name_state l' doctype
    end

  (* 8.2.5.54. *)
  and doctype_name_state l' doctype =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        after_doctype_name_state l' doctype

      | Some (_, 0x003E) ->
        emit_doctype l' doctype

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "doctype", "null")) !throw (fun () ->
        doctype.doctype_name <-
          add_doctype_char doctype.doctype_name u_rep;
        doctype_name_state l' doctype)

      | None ->
        report (get_location ()) (`Unexpected_eoi "doctype") !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | Some (_, c) ->
        doctype.doctype_name <-
          add_doctype_char doctype.doctype_name (to_lowercase c);
        doctype_name_state l' doctype
    end

  (* 8.2.4.55. *)
  and after_doctype_name_state l' doctype =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        after_doctype_name_state l' doctype

      | Some (_, 0x003E) ->
        emit_doctype l' doctype

      | None ->
        report (get_location ()) (`Unexpected_eoi "doctype") !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | Some (l'', c as v) ->
        push input v;
        next_n 6 input !throw begin fun l ->
          match sequence_to_lowercase l with
          | [_, 0x70; _, 0x75; _, 0x62; _, 0x6C; _, 0x69; _, 0x63] ->
            after_doctype_public_keyword_state l' doctype

          | [_, 0x73; _, 0x79; _, 0x73; _, 0x74; _, 0x65; _, 0x6D] ->
            after_doctype_system_keyword_state l' doctype

          | vs ->
            push_list input vs;
            report l'' (`Bad_token (char c, "doctype",
                                    "expected 'PUBLIC' or 'SYSTEM'")) !throw
              (fun () ->
            doctype.force_quirks <- true;
            bogus_doctype_state l' doctype)
        end
    end

  (* Helper. *)
  and begin_public_identifier quote l' doctype =
    doctype.Doctype_buffers.public_identifier <- Some (Buffer.create 32);
    doctype_identifier_quoted_state
      (fun doctype c ->
        doctype.Doctype_buffers.public_identifier <-
          add_doctype_char doctype.Doctype_buffers.public_identifier c)
        quote after_doctype_public_identifier_state l' doctype

  (* Helper. *)
  and begin_system_identifier quote l' doctype =
    doctype.Doctype_buffers.system_identifier <- Some (Buffer.create 32);
    doctype_identifier_quoted_state
      (fun doctype c ->
        doctype.system_identifier <-
          add_doctype_char doctype.system_identifier c)
      quote after_doctype_system_identifier_state l' doctype

  (* 8.2.4.56. *)
  and after_doctype_public_keyword_state l' doctype =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        before_doctype_public_identifier_state l' doctype

      | Some (l, (0x0022 | 0x0027 as c)) ->
        report l (`Bad_token (char c, "doctype",
                              "expected whitespace")) !throw (fun () ->
        begin_public_identifier c l' doctype)

      | Some (l, 0x003E) ->
        report l (`Bad_token (">", "doctype", "expected public identifier"))
          !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | None ->
        report (get_location ()) (`Unexpected_eoi "doctype") !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | Some (l, c) ->
        report l (`Bad_token (char c, "doctype",
                              "expected whitespace")) !throw (fun () ->
        doctype.force_quirks <- true;
        bogus_doctype_state l' doctype)
    end

  (* 8.2.4.57. *)
  and before_doctype_public_identifier_state l' doctype =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        before_doctype_public_identifier_state l' doctype

      | Some (_, (0x0022 | 0x0027 as c)) ->
        begin_public_identifier c l' doctype

      | Some (l, 0x003E) ->
        report l (`Bad_token (">", "doctype", "expected public identifier"))
          !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | None ->
        report (get_location ()) (`Unexpected_eoi "doctype") !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | Some (l, c) ->
        report l (`Bad_token (char c, "doctype",
                              "public identifier must be quoted")) !throw
          (fun () ->
        doctype.force_quirks <- true;
        bogus_doctype_state l' doctype)
    end

  (* 8.2.4.58, 8.2.4.59, 8.2.4.64, 8.2.4.65. *)
  and doctype_identifier_quoted_state add quote next_state l' doctype =
    next_option input !throw begin function
      | Some (_, c) when c = quote ->
        next_state l' doctype

      | Some (l, 0) ->
        report l (`Bad_token ("U+0000", "doctype", "null")) !throw (fun () ->
        add doctype u_rep;
        doctype_identifier_quoted_state add quote next_state l' doctype)

      | Some (l, 0x003E) ->
        report l (`Bad_token (">", "doctype", "'>' in identifier")) !throw
          (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | None ->
        report (get_location ()) (`Unexpected_eoi "doctype") !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | Some (_, c) ->
        add doctype c;
        doctype_identifier_quoted_state add quote next_state l' doctype
    end

  (* 8.2.4.60. *)
  and after_doctype_public_identifier_state l' doctype =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        between_doctype_public_and_system_identifiers l' doctype

      | Some (_, 0x003E) -> emit_doctype l' doctype

      | Some (l, (0x0022 | 0x0027 as c)) ->
        report l (`Bad_token (char c, "doctype",
                              "expected whitespace")) !throw (fun () ->
        begin_system_identifier c l' doctype)

      | None ->
        report (get_location ()) (`Unexpected_eoi "doctype") !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | Some (l, c) ->
        report l (`Bad_token (char c, "doctype",
                              "system identifier must be quoted")) !throw
          (fun () ->
        doctype.force_quirks <- true;
        bogus_doctype_state l' doctype)
    end

  (* 8.2.4.61. *)
  and between_doctype_public_and_system_identifiers l' doctype =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        between_doctype_public_and_system_identifiers l' doctype

      | Some (_, 0x003E) ->
        emit_doctype l' doctype

      | Some (_, (0x0022 | 0x0027 as c)) ->
        begin_system_identifier c l' doctype

      | None ->
        report (get_location ()) (`Unexpected_eoi "doctype") !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | Some (l, c) ->
        report l (`Bad_token (char c, "doctype",
                              "system identifier must be quoted")) !throw
          (fun () ->
        doctype.force_quirks <- true;
        bogus_doctype_state l' doctype)
    end

  (* 8.2.4.62. *)
  and after_doctype_system_keyword_state l' doctype =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        before_doctype_system_identifier_state l' doctype

      | Some (l, (0x0022 | 0x0027 as c)) ->
        report l (`Bad_token (char c, "doctype",
                              "expected whitespace")) !throw (fun () ->
        begin_system_identifier c l' doctype)

      | Some (l, 0x003E) ->
        report l (`Bad_token (">", "doctype", "expected system identifier"))
          !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | None ->
        report (get_location ()) (`Unexpected_eoi "doctype") !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | Some (l, c) ->
        report l (`Bad_token (char c, "doctype",
                              "expected whitespace")) !throw (fun () ->
        doctype.force_quirks <- true;
        bogus_doctype_state l' doctype)
    end

  (* 8.2.4.63. *)
  and before_doctype_system_identifier_state l' doctype =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        before_doctype_system_identifier_state l' doctype

      | Some (_, (0x0022 | 0x0027 as c)) ->
        begin_system_identifier c l' doctype

      | Some (l, 0x003E) ->
        report l (`Bad_token (">", "doctype", "expected system identifier"))
          !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | None ->
        report (get_location ()) (`Unexpected_eoi "doctype") !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | Some (l, c) ->
        report l (`Bad_token (char c, "doctype",
                              "system identifier must be quoted")) !throw
          (fun () ->
        doctype.force_quirks <- true;
        bogus_doctype_state l' doctype)
    end

  (* 8.2.4.66. *)
  and after_doctype_system_identifier_state l' doctype =
    next_option input !throw begin function
      | Some (_, (0x0009 | 0x000A | 0x000C | 0x0020)) ->
        after_doctype_system_identifier_state l' doctype

      | Some (_, 0x003E) ->
        emit_doctype l' doctype

      | None ->
        report (get_location ()) (`Unexpected_eoi "doctype") !throw (fun () ->
        emit_doctype ~quirks:true l' doctype)

      | Some (l, c) ->
        report l (`Bad_token (char c, "doctype",
                              "junk after system identifier")) !throw (fun () ->
        bogus_doctype_state l' doctype)
    end

  (* 8.2.4.67. *)
  and bogus_doctype_state l' doctype =
    next_option input !throw begin function
      | Some (_, 0x003E) ->
        emit_doctype l' doctype

      | None ->
        emit_doctype l' doctype

      | _ ->
        bogus_doctype_state l' doctype
    end

  (* 8.2.4.68. *)
  and cdata_section_state () =
    next_option input !throw begin function
      | None ->
        data_state ()

      | Some (l, 0x005D) ->
        peek_n 2 input !throw begin function
          | [_, 0x005D; _, 0x003E] ->
            next_n 2 input !throw (fun _ ->
            data_state ())

          | _ ->
            emit (l, `Char 0x005D) cdata_section_state
        end

      | Some (l, c) ->
        emit (l, `Char c) cdata_section_state
    end

  in

  let stream =
    (fun throw_ e k ->
      throw := throw_;
      ended := e;
      output := k;
      !current_state ())
    |> make
  in

  let set_state = function
    | `Data -> current_state := data_state
    | `RCDATA -> current_state := rcdata_state
    | `RAWTEXT -> current_state := rawtext_state
    | `Script_data -> current_state := script_data_state
    | `PLAINTEXT -> current_state := plaintext_state
  in

  let set_foreign = (:=) foreign in

  stream, set_state, set_foreign
