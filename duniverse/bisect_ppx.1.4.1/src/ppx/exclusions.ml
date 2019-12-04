(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



type t =
  | Regular_expression of Str.regexp
  | Exclude_file of Exclude.file

let excluded = ref []

let function_separator = Str.regexp "[ \t]*,[ \t]*"

let add s =
  let patterns = Str.split function_separator s in
  let patterns = List.map (fun x -> Regular_expression (Str.regexp x)) patterns in
  excluded := patterns @ !excluded

let add_file filename =
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  try
    let res = Exclude_parser.file Exclude_lexer.token lexbuf in
    let res = List.map (fun x -> Exclude_file x) res in
    excluded := res @ !excluded;
    close_in_noerr ch
  with
  | Exclude.Exception (line, msg) ->
      Printf.eprintf " *** error in file %S at line %d: %s\n"
        filename line msg;
      close_in_noerr ch;
      exit 1
  | e ->
      close_in_noerr ch;
      raise e

let match_pattern pattern name =
  Str.string_match pattern name 0 && Str.match_end () = String.length name

let file_name_matches exclusion file =
  match exclusion.Exclude.path with
  | Exclude.Name file' -> file = file'
  | Exclude.Regexp pattern -> match_pattern pattern file

let contains_value file name =
  List.exists
    (function
      | Regular_expression patt ->
          match_pattern patt name
      | Exclude_file ef ->
        let in_value_list () =
          match ef.Exclude.exclusions with
          | None -> true
          | Some exclusions ->
            exclusions |> List.exists (function
              | Exclude.Name en -> name = en
              | Exclude.Regexp patt -> match_pattern patt name)
        in
        file_name_matches ef file && in_value_list ())
    !excluded

let contains_file file =
  !excluded |> List.exists (function
    | Regular_expression _ -> false
    | Exclude_file exclusion ->
      exclusion.Exclude.exclusions = None && file_name_matches exclusion file)
