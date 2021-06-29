(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

type t =
  [ `Decoding_error of string * string
  | `Bad_token of string * string * string
  | `Unexpected_eoi of string
  | `Bad_document of string
  | `Unmatched_start_tag of string
  | `Unmatched_end_tag of string
  | `Bad_namespace of string
  | `Misnested_tag of string * string * (string * string) list
  | `Bad_content of string ]

let explode_string s =
  let rec iterate index acc =
    if index >= String.length s then List.rev acc
    else iterate (index + 1) (s.[index]::acc)
  in
  iterate 0 []

let to_string ?location error =
  let fmt = Printf.sprintf in

  let message =
    match error with
    | `Decoding_error (bytes, encoding) ->
      begin match String.length bytes with
      | 0 ->
        fmt "bad bytes for encoding '%s'" encoding
      | 1 ->
        fmt "bad byte '0x%02X' for encoding '%s'" (Char.code bytes.[0]) encoding
      | _ ->
        fmt "bad bytes '%s' for encoding '%s'"
          (explode_string bytes
           |> List.map Char.code
           |> List.map (fmt "0x%02X")
           |> String.concat " ")
          encoding
      end

    | `Bad_token (s, production, reason) ->
      fmt "bad token '%s' in %s: %s" s production reason

    | `Unexpected_eoi in_ ->
      fmt "unexpected end of input in %s" in_

    | `Bad_document reason ->
      fmt "bad document: %s" reason

    | `Unmatched_start_tag s ->
      fmt "unmatched start tag '%s'" s

    | `Unmatched_end_tag s ->
      fmt "unmatched end tag '%s'" s

    | `Bad_namespace s ->
      fmt "unknown namespace '%s'" s

    | `Misnested_tag (s, in_, _attributes) ->
      fmt "misnested tag: '%s' in '%s'" s in_

    | `Bad_content s ->
      fmt "bad content in '%s'" s
  in

  match location with
  | None -> message
  | Some (line, column) -> fmt "line %i, column %i: %s" line column message

type 'a handler = 'a -> t -> unit cps
type parse_handler = location handler
type write_handler = (signal * int) handler

let ignore_errors _ _ _ resume = resume ()

let report_if report condition location detail throw k =
  if condition then report location (detail ()) throw k
  else k ()
