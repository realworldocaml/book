(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common
open Kstream
open Encoding

let name_to_encoding = function
  | "utf-8" -> Some utf_8
  | "utf-16be" -> Some utf_16be
  | "utf-16le" -> Some utf_16le
  | "iso-8859-1" -> Some iso_8859_1
  | "iso-8859-15" -> Some iso_8859_15
  | "us-ascii" -> Some us_ascii
  | "windows-1251" -> Some windows_1251
  | "windows-1252" -> Some windows_1252
  | "ucs-4be" -> Some ucs_4be
  | "ucs-4le" -> Some ucs_4le
  | _ -> None

(* 8.2.2.2. *)
let guess_from_bom_html source throw k =
  peek_n 3 source throw (function
    | '\xFE'::'\xFF'::_ -> k (Some "utf-16be")
    | '\xFF'::'\xFE'::_ -> k (Some "utf-16le")
    | ['\xEF'; '\xBB'; '\xBF'] -> k (Some "utf-8")
    | _ -> k None)

(* Appendix F.1. *)
let guess_from_bom_xml source throw k =
  peek_n 4 source throw (function
    | ['\x00'; '\x00'; '\xFE'; '\xFF'] -> k (Some "ucs-4be")
    | ['\xFF'; '\xFE'; '\x00'; '\x00'] -> k (Some "ucs-4le")
    | ['\x00'; '\x00'; '\xFF'; '\xFE'] -> k (Some "ucs-4be-transposed")
    | ['\xFE'; '\xFF'; '\x00'; '\x00'] -> k (Some "ucs-4le-transposed")
    | '\xFE'::'\xFF'::_ -> k (Some "utf-16be")
    | '\xFF'::'\xFE'::_ -> k (Some "utf-16le")
    | '\xEF'::'\xBB'::'\xBF'::_ -> k (Some "utf-8")
    | _ -> k None)

(* Appendix F.1. *)
let guess_family_xml source throw k =
  peek_n 4 source throw (function
    | ['\x00'; '\x00'; '\x00'; '\x3C'] -> k (Some "ucs-4be")
    | ['\x3C'; '\x00'; '\x00'; '\x00'] -> k (Some "ucs-4le")
    | ['\x00'; '\x00'; '\x3C'; '\x00'] -> k (Some "ucs-4be-transposed")
    | ['\x00'; '\x3C'; '\x00'; '\x00'] -> k (Some "ucs-4le-transposed")
    | ['\x00'; '\x3C'; '\x00'; '\x3F'] -> k (Some "utf-16be")
    | ['\x3C'; '\x00'; '\x3F'; '\x00'] -> k (Some "utf-16le")
    | ['\x3C'; '\x3F'; '\x78'; '\x6D'] -> k (Some "utf-8")
    | ['\x4C'; '\x6F'; '\xA7'; '\x94'] -> k (Some "ebcdic")
    | _ -> k None)

(* 5.2 in the Encoding Candidate Recommendation. *)
let normalize_name for_html s =
  match String.lowercase (trim_string s) with
  | "unicode-1-1-utf-8" | "utf-8" | "utf8" ->
    "utf-8"

  | "866" | "cp866" | "csibm866" | "ibm866" ->
    "ibm866"

  | "csisolatin2" | "iso-8859-2" | "iso-ir-101" | "iso8859-2" | "iso88592"
  | "iso_8859-2" | "iso_8859-2:1987" | "l2" | "latin2" ->
    "iso-8859-2"

  | "csisolatin3" | "iso-8859-3" | "iso-ir-109" | "iso8859-3" | "iso88593"
  | "iso_8859-3" | "iso_8859-3:1988" | "l3" | "latin3" ->
    "iso-8859-3"

  | "csisolatin4" | "iso-8859-4" | "iso-ir-110" | "iso8859-4" | "iso88594"
  | "iso_8859-4" | "iso_8859-4:1988" | "l4" | "latin4" ->
    "iso-8859-4"

  | "csisolatincyrillic" | "cyrillic" | "iso-8859-5" | "iso-ir-144"
  | "iso8859-5" | "iso88595" | "iso_8859-5" | "iso_8859-5:1988" ->
    "iso-8859-5"

  | "arabic" | "asmo-708" | "csiso88596e" | "csiso88596i" | "csisolatinarabic"
  | "ecma-114" | "iso-8859-6" | "iso-8859-6-e" | "iso-8859-6-i" | "iso-ir-127"
  | "iso8859-6" | "iso88596" | "iso_8859-6" | "iso_8859-6:1987" ->
    "iso-8859-6"

  | "csisolatingreek" | "ecma-118" | "elot_928" | "greek" | "greek8"
  | "iso-8859-7" | "iso-ir-126" | "iso8859-7" | "iso88597" | "iso_8859-7"
  | "iso_8859-7:1987" | "sun_eu_greek" ->
    "iso-8859-7"

  | "csiso88598e" | "csisolatinhebrew" | "hebrew" | "iso-8859-8"
  | "iso-8859-8-e" | "iso-ir-138" | "iso8859-8" | "iso88598" | "iso_8859-8"
  | "iso_8859-8:1988" | "visual" ->
    "iso-8859-8"

  | "csiso88598i" | "iso-8859-8-i" | "logical" ->
    "iso-8859-8-i"

  | "csisolatin6" | "iso-8859-10" | "iso-ir-157" | "iso8859-10" | "iso885910"
  | "l6" | "latin6" ->
    "iso-8859-10"

  | "iso-8859-13" | "iso8859-13" | "iso885913" ->
    "iso-8859-13"

  | "iso-8859-14" | "iso8859-14" | "iso885914" ->
    "iso-8859-14"

  | "csisolatin9" | "iso-8859-15" | "iso8859-15" | "iso885915" | "iso_8859-15"
  | "l9" ->
    "iso-8859-15"

  | "iso-8859-16" ->
    "iso-8859-16"

  | "cskoi8r" | "koi" | "koi8" | "koi8-r" | "koi8_r" ->
    "koi8-r"

  | "koi8-ru" | "koi8-u" ->
    "koi8-u"

  | "csmacintosh" | "mac" | "macintosh" | "x-mac-roman" ->
    "macintosh"

  | "dos-874" | "iso-8859-11" | "iso8859-11" | "iso885911" | "tis-620"
  | "windows-874" ->
    "windows-874"

  | "cp1250" | "windows-1250" | "x-cp1250" ->
    "windows-1250"

  | "cp1251" | "windows-1251" | "x-cp1251" ->
    "windows-1251"

  | "ansi_x3.4-1968" | "ascii" | "us-ascii" ->
    if for_html then "windows-1252" else "us-ascii"

  | "cp819" | "csisolatin1" | "ibm819" | "iso-8859-1" | "iso-ir-100"
  | "iso8859-1" | "iso88591" | "iso_8859-1" | "iso_8859-1:1987" | "l1"
  | "latin1" ->
    if for_html then "windows-1252" else "iso-8859-1"

  | "cp1252" | "windows-1252" | "x-cp1252" ->
    "windows-1252"

  | "cp1253" | "windows-1253" | "x-cp1253" ->
    "windows-1253"

  | "cp1254" | "csisolatin5" | "iso-8859-9" | "iso-ir-148" | "iso8859-9"
  | "iso88599" | "iso_8859-9" | "iso_8859-9:1989" | "l5" | "latin5"
  | "windows-1254" | "x-cp1254" ->
    "windows-1254"

  | "cp1255" | "windows-1255" | "x-cp1255" ->
    "windows-1255"

  | "cp1256" | "windows-1256" | "x-cp1256" ->
    "windows-1256"

  | "cp1257" | "windows-1257" | "x-cp1257" ->
    "windows-1257"

  | "cp1258" | "windows-1258" | "x-cp1258" ->
    "windows-1258"

  | "x-mac-cyrillic" | "x-mac-ukrainian" ->
    "x-mac-cyrillic"

  | "chinese" | "csgb2312" | "csiso58gb231280" | "gb2312" | "gb_2312"
  | "gb_2312-80" | "gbk" | "iso-ir-58" | "x-gbk" ->
    "gbk"

  | "gb18030" ->
    "gb18030"

  | "big5" | "big5-hkscs" | "cn-big5" | "csbig5" | "x-x-big5" ->
    "big5"

  | "cseucpkdfmtjapanese" | "euc-jp" | "x-euc-jp" ->
    "euc-jp"

  | "csiso2022jp" | "iso-2022-jp" ->
    "iso-2022-jp"

  | "csshiftjis" | "ms932" | "ms_kanji" | "shift-jis" | "shift_jis" | "sjis"
  | "windows-31j" | "x-sjis" ->
    "shift_jis"

  | "cseuckr" | "csksc56011987" | "euc-kr" | "iso-ir-149" | "korean"
  | "ks_c_5601-1987" | "ks_c_5601-1989" | "ksc5601" | "ksc_5601"
  | "windows-949" ->
    "euc-kr"

  | "csiso2022kr" | "hz-gb-2312" | "iso-2022-cn" | "iso-2022-cn-ext"
  | "iso-2022-kr" ->
    "replacement"

  | "utf-16be" ->
    "utf-16be"

  | "utf-16" | "utf-16le" ->
    "utf-16le"

  | "x-user-defined" ->
    "x-user-defined"

  | s -> s

(* 8.2.2.2. *)
let meta_tag_prescan =
  let is_uppercase c = c >= 'A' && c <= 'Z' in
  let is_lowercase c = c >= 'a' && c <= 'z' in
  let is_letter c = is_uppercase c || is_lowercase c in
  let is_whitespace c = String.contains "\t\n\r\x0C " c in

  let rec skip_whitespace source throw k =
    next source throw k (function
      | c when is_whitespace c -> skip_whitespace source throw k
      | c -> push source c; k ())
  in

  let read_quoted_value quote source throw k =
    let buffer = Buffer.create 32 in

    let rec iterate () =
      next source throw (fun () -> k "") (function
        | c when c = quote -> k (Buffer.contents buffer)
        | c -> add_utf_8 buffer (Char.code (Char.lowercase c)); iterate ())
    in
    iterate ()
  in

  let read_unquoted_value terminator source throw k =
    let buffer = Buffer.create 32 in

    let rec iterate () =
      next source throw (fun () -> k (Buffer.contents buffer)) (function
        | c when is_whitespace c || c = terminator ->
          push source c;
          k (Buffer.contents buffer)
        | c ->
          add_utf_8 buffer (Char.code (Char.lowercase c));
          iterate ())
    in
    iterate ()
  in

  (* 2.6.5. *)
  let extract_encoding source throw k =
    let rec scan () =
      next source throw (fun () -> k None) begin function
        | 'c' ->
          next_n 6 source throw begin fun l ->
            match List.map Char.lowercase l with
            | ['h'; 'a'; 'r'; 's'; 'e'; 't'] ->
              skip_whitespace source throw (fun () ->
              next source throw (fun () -> k None) begin function
                | '=' ->
                  skip_whitespace source throw (fun () ->
                  next source throw (fun () -> k None) (fun c ->
                  let continue_with =
                    match c with
                    | '"' | '\'' as c -> read_quoted_value c source throw
                    | _ -> push source c; read_unquoted_value ';' source throw
                  in
                  continue_with (function
                    | "" -> k None
                    | s -> k (Some s))))

                | c ->
                  push source c;
                  scan ()
              end)
            | _ -> scan ()
          end
        | _ -> scan ()
      end
    in
    scan ()
  in

  let everything = fun _ k -> k true in

  fun ?(supported = everything) ?(limit = 1024) source throw k ->
    let source, restore = checkpoint source in
    let finish result = restore (); k result in

    let source =
      let count = ref 0 in
      (fun throw empty k ->
        if !count >= limit then empty ()
        else next source throw empty (fun c -> count := !count + 1; k c))
      |> make
    in

    let get_attribute k' =
      let rec skip_leading k =
        next source throw (fun () -> k' None) (function
          | c when is_whitespace c || c = '/' -> skip_leading k
          | c -> push source c; k ())
      in

      let read_name k =
        let buffer = Buffer.create 32 in

        let rec iterate () =
          next_option source throw begin function
            | Some ('=' as c) when Buffer.length buffer > 0 ->
              push source c;
              k (Buffer.contents buffer)

            | Some '/' | Some '>' | None as c ->
              push_option source c;
              if Buffer.length buffer = 0 then k' None
              else k' (Some (Buffer.contents buffer, ""))

            | Some c when is_whitespace c ->
              k (Buffer.contents buffer)

            | Some c ->
              add_utf_8 buffer (Char.code (Char.lowercase c));
              iterate ()
          end
        in
        iterate ()
      in

      skip_leading (fun () ->
      read_name (fun name ->
      skip_whitespace source throw (fun () ->
      next_option source throw begin function
        | Some '=' ->
          skip_whitespace source throw (fun () ->
          next_option source throw (fun maybe_c ->
          let continue_with =
            match maybe_c with
            | Some ('\'' | '"' as c) ->
              read_quoted_value c source throw
            | Some c ->
              push source c;
              read_unquoted_value '>' source throw
            | None ->
              read_unquoted_value '>' source throw
          in
          continue_with (fun value -> k' (Some (name, value)))))

        | c ->
          push_option source c;
          k' (Some (name, ""))
      end)))
    in

    let read_attributes k =
      let rec iterate names got_pragma need_pragma charset =
        get_attribute begin function
          | None -> k got_pragma need_pragma charset
          | Some (name, value) ->
            if list_mem_string name names then
              iterate names got_pragma need_pragma charset
            else
              let names = name::names in
              match name with
              | "http-equiv" ->
                if value = "content-type" then
                  iterate names true need_pragma charset
                else
                  iterate names got_pragma need_pragma charset

              | "content" ->
                if charset <> None then
                  iterate names got_pragma need_pragma charset
                else
                  extract_encoding (Stream_io.string value) throw begin function
                    | None -> iterate names got_pragma need_pragma charset
                    | Some encoding ->
                      iterate names got_pragma (Some true) (Some encoding)
                  end

              | "charset" ->
                if value = "" then
                  iterate names got_pragma need_pragma charset
                else
                  iterate names got_pragma (Some false) (Some value)

              | _ -> iterate names got_pragma need_pragma charset
        end
      in
      iterate [] false None None
    in

    let process_attributes got_pragma need_pragma charset k =
      match need_pragma with
      | None -> k None
      | Some need_pragma ->
        if need_pragma && (not got_pragma) then k None
        else
          match charset with
          | None -> k None
          | Some charset ->
            let charset =
              match normalize_name true charset with
              | "utf-16be" | "utf-16le" | "utf-16" -> "utf-8"
              | s -> s
            in
            supported charset (function
              | true -> k (Some charset)
              | false -> k None)
    in

    let process_meta_tag k =
      read_attributes (fun got_pragma need_pragma charset ->
      process_attributes got_pragma need_pragma charset (function
        | None -> k ()
        | v -> finish v))
    in

    let rec close_comment k =
      next source throw (fun () -> finish None) (function
        | '-' ->
          next_n 2 source throw (function
            | ['-'; '>'] -> k ()
            | l -> push_list source l; close_comment k)
        | _ -> close_comment k)
    in

    let close_tag k =
      let rec skip () =
        next source throw (fun () -> finish None) (function
          | c when is_whitespace c || c = '>' ->
            push source c;
            let rec drain_attributes () =
              get_attribute (function
                | None -> k ()
                | Some _ -> drain_attributes ())
            in
            drain_attributes ()

          | _ -> skip ())
      in
      skip ()
    in

    let rec close_tag_like k =
      next source throw (fun () -> finish None) (function
        | '>' -> k ()
        | _ -> close_tag_like k)
    in

    let rec scan () =
      next source throw (fun () -> finish None) begin function
        | '<' ->
          peek source throw (fun () -> finish None) begin function
            | '!' ->
              peek_n 3 source throw (function
                | ['!'; '-'; '-'] -> close_comment scan
                | _ -> close_tag_like scan)

            | '/' ->
              peek_n 2 source throw (function
                | ['/'; c] when is_letter c -> close_tag scan
                | _ -> close_tag_like scan)

            | '?' ->
              close_tag_like scan

            | 'm' ->
              peek_n 5 source throw (fun l ->
                match List.map Char.lowercase l with
                | ['m'; 'e'; 't'; 'a'; c] when is_whitespace c || c = '/' ->
                  next_n 4 source throw (fun _ ->
                  process_meta_tag scan)

                | _ ->
                  close_tag scan)

            | c when is_letter c ->
              close_tag scan

            | _ ->
              scan ()
          end

        | _ -> scan ()
      end
    in
    scan ()

let read_xml_encoding_declaration bytes (family : Encoding.t) throw k =
  let bytes, restore = Kstream.checkpoint bytes in
  let k v = restore (); k v in

  let tokens =
    bytes
    |> family
    |> Input.preprocess is_valid_xml_char Error.ignore_errors
    |> Xml_tokenizer.tokenize Error.ignore_errors (fun _ -> None)
  in

  let rec prescan () =
    Kstream.next tokens throw (fun () -> k None) begin function
      | _, `Xml {Common.encoding} -> k encoding
      | _, `Comment _ -> prescan ()
      | _, `Chars s when List.for_all is_whitespace_only s -> prescan ()
      | _ -> k None
    end
  in

  prescan ()

let name_to_encoding_or_utf_8 encoding =
  match name_to_encoding encoding with
  | Some e -> e
  | None -> utf_8

let select_html ?limit bytes throw k =
  guess_from_bom_html bytes throw (function
    | Some encoding -> k (name_to_encoding_or_utf_8 encoding)
    | None ->
      meta_tag_prescan ?limit bytes throw (function
        | Some encoding -> k (name_to_encoding_or_utf_8 encoding)
        | None -> k utf_8))

let select_xml bytes throw k =
  guess_from_bom_xml bytes throw (function
    | Some encoding -> k (name_to_encoding_or_utf_8 encoding)
    | None ->
      (fun k' ->
        guess_family_xml bytes throw (function
          | None -> k' "utf-8" utf_8
          | Some family -> k' family (name_to_encoding_or_utf_8 family)))
      (fun name family ->
        read_xml_encoding_declaration bytes family throw (function
          | None -> k (name_to_encoding_or_utf_8 name)
          | Some encoding ->
            match name, normalize_name false encoding with
            | "utf-8", "iso-8859-1" -> k iso_8859_1
            | "utf-8", "us-ascii" -> k us_ascii
            | "utf-8", "windows-1251" -> k windows_1251
            | "utf-8", "windows-1252" -> k windows_1252
            | _ -> k (name_to_encoding_or_utf_8 name))))
