(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(* Aliases for reducing the number of deprecation warnings. *)
module String =
struct
  include String
  let lowercase = lowercase [@ocaml.warning "-3"]
end

module Char =
struct
  include Char
  let lowercase = lowercase [@ocaml.warning "-3"]
end

type 'a cont = 'a -> unit
type 'a cps = exn cont -> 'a cont -> unit

type location = int * int

let compare_locations (line, column) (line', column') =
  match line - line' with
  | 0 -> column - column'
  | order -> order

type name = string * string

let xml_ns = "http://www.w3.org/XML/1998/namespace"
let xmlns_ns = "http://www.w3.org/2000/xmlns/"
let xlink_ns = "http://www.w3.org/1999/xlink"
let html_ns = "http://www.w3.org/1999/xhtml"
let svg_ns = "http://www.w3.org/2000/svg"
let mathml_ns = "http://www.w3.org/1998/Math/MathML"

module Token_tag =
struct
  type t =
    {name         : string;
     attributes   : (string * string) list;
     self_closing : bool}
end

type xml_declaration =
  {version    : string;
   encoding   : string option;
   standalone : bool option}

type doctype =
  {doctype_name      : string option;
   public_identifier : string option;
   system_identifier : string option;
   raw_text          : string option;
   force_quirks      : bool}

type signal =
  [ `Start_element of name * (name * string) list
  | `End_element
  | `Text of string list
  | `Xml of xml_declaration
  | `Doctype of doctype
  | `PI of string * string
  | `Comment of string ]

type content_signal =
  [ `Start_element of name * (name * string) list
  | `End_element
  | `Text of string list ]

type general_token =
  [ `Xml of xml_declaration
  | `Doctype of doctype
  | `Start of Token_tag.t
  | `End of Token_tag.t
  | `Chars of string list
  | `Char of int
  | `PI of string * string
  | `Comment of string
  | `EOF ]

let u_rep = Uchar.to_int Uutf.u_rep

let add_utf_8 buffer c =
  Uutf.Buffer.add_utf_8 buffer (Uchar.unsafe_of_int c)

let format_char = Printf.sprintf "U+%04X"

(* Type constraints are necessary to avoid polymorphic comparison, which would
   greatly reduce performance: https://github.com/aantron/markup.ml/pull/15. *)
let is_in_range (lower : int) (upper : int) c = c >= lower && c <= upper

(* HTML 8.2.2.5. *)
let is_control_character = function
  | 0x000B -> true
  | c when is_in_range 0x0001 0x0008 c -> true
  | c when is_in_range 0x000E 0x001F c -> true
  | c when is_in_range 0x007F 0x009F c -> true
  | _ -> false

(* HTML 8.2.2.5. *)
let is_non_character = function
  | c when is_in_range 0xFDD0 0xFDEF c -> true
  | c when (c land 0xFFFF = 0xFFFF) || (c land 0xFFFF = 0xFFFE) -> true
  | _ -> false

let is_digit = is_in_range 0x0030 0x0039

let is_hex_digit = function
  | c when is_digit c -> true
  | c when is_in_range 0x0041 0x0046 c -> true
  | c when is_in_range 0x0061 0x0066 c -> true
  | _ -> false

let is_scalar = function
  | c when (c >= 0x10FFFF) || ((c >= 0xD800) && (c <= 0xDFFF)) -> false
  | _ -> true

let is_uppercase = is_in_range 0x0041 0x005A

let is_lowercase = is_in_range 0x0061 0x007A

let is_alphabetic = function
  | c when is_uppercase c -> true
  | c when is_lowercase c -> true
  | _ -> false

let is_alphanumeric = function
  | c when is_alphabetic c -> true
  | c when is_digit c -> true
  | _ -> false

let is_whitespace c = c = 0x0020 || c = 0x000A || c = 0x0009 || c = 0x000D

let is_whitespace_only s =
  try
    s |> String.iter (fun c ->
      if is_whitespace (int_of_char c) then ()
      else raise Exit);
    true

  with Exit -> false

let to_lowercase = function
  | c when is_uppercase c -> c + 0x20
  | c -> c

let is_printable = is_in_range 0x0020 0x007E

let char c =
  if is_printable c then begin
    let buffer = Buffer.create 4 in
    add_utf_8 buffer c;
    Buffer.contents buffer
  end
  else
    format_char c

let is_valid_html_char c = not (is_control_character c || is_non_character c)

let is_valid_xml_char c =
  is_in_range 0x0020 0xD7FF c
  || c = 0x0009
  || c = 0x000A
  || c = 0x000D
  || is_in_range 0xE000 0xFFFD c
  || is_in_range 0x10000 0x10FFFF c

let signal_to_string = function
  | `Comment s ->
    Printf.sprintf "<!--%s-->" s

  | `Doctype d ->
    let text =
      match d.doctype_name with
      | None ->
        begin match d.raw_text with
        | None -> ""
        | Some s -> " " ^ s
        end
      | Some name ->
        match d.public_identifier, d.system_identifier with
        | None, None -> name
        | Some p, None -> Printf.sprintf " %s PUBLIC \"%s\"" name p
        | None, Some s -> Printf.sprintf " %s SYSTEM \"%s\"" name s
        | Some p, Some s -> Printf.sprintf " %s PUBLIC \"%s\" \"%s\"" name p s
    in
    Printf.sprintf "<!DOCTYPE %s>" text

  | `Start_element (name, attributes) ->
    let name_to_string = function
      | "", local_name -> local_name
      | ns, local_name -> ns ^ ":" ^ local_name
    in
    let attributes =
      attributes
      |> List.map (fun (name, value) ->
        Printf.sprintf " %s=\"%s\"" (name_to_string name) value)
      |> String.concat ""
    in
    Printf.sprintf "<%s%s>" (name_to_string name) attributes

  | `End_element ->
    "</...>"

  | `Text ss ->
    String.concat "" ss

  | `Xml x ->
    let s = Printf.sprintf "<?xml version=\"%s\">" x.version in
    let s =
      match x.encoding with
      | None -> s
      | Some encoding -> Printf.sprintf "%s encoding=\"%s\"" s encoding
    in
    let s =
      match x.standalone with
      | None -> s
      | Some standalone ->
        Printf.sprintf
          "%s standalone=\"%s\"" s (if standalone then "yes" else "no")
    in
    s ^ "?>"

  | `PI (target, s) ->
    Printf.sprintf "<?%s %s?>" target s

let token_to_string = function
  | `Xml x ->
    signal_to_string (`Xml x)

  | `Doctype d ->
    signal_to_string (`Doctype d)

  | `Start t ->
    let name = "", t.Token_tag.name in
    let attributes =
      t.Token_tag.attributes |> List.map (fun (n, v) -> ("", n), v) in
    let s = signal_to_string (`Start_element (name, attributes)) in
    if not t.Token_tag.self_closing then s
    else (String.sub s 0 (String.length s - 1)) ^ "/>"

  | `End t ->
    Printf.sprintf "</%s>" t.Token_tag.name

  | `Chars ss ->
    String.concat "" ss

  | `Char i ->
    char i

  | `PI v ->
    signal_to_string (`PI v)

  | `Comment s ->
    signal_to_string (`Comment s)

  | `EOF ->
    "EOF"

let whitespace_chars = " \t\n\r"

let whitespace_prefix_length s =
  let rec loop index =
    if index = String.length s then index
    else
      if String.contains whitespace_chars s.[index] then loop (index + 1)
      else index
  in
  loop 0

let whitespace_suffix_length s =
  let rec loop rindex =
    if rindex = String.length s then rindex
    else
      if String.contains whitespace_chars s.[String.length s - rindex - 1] then
        loop (rindex + 1)
      else rindex
  in
  loop 0

let trim_string_left s =
  let prefix_length = whitespace_prefix_length s in
  String.sub s prefix_length (String.length s - prefix_length)

let trim_string_right s =
  let suffix_length = whitespace_suffix_length s in
  String.sub s 0 (String.length s - suffix_length)

(* String.trim not available for OCaml < 4.00. *)
let trim_string s = s |> trim_string_left |> trim_string_right

(* Specialization of List.mem at string list, to avoid polymorphic
   comparison. *)
let list_mem_string (s : string) l = List.exists (fun s' -> s' = s) l
