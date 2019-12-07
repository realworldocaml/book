(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

open Compat

type threechoices = Always | Never | Auto

type t = {
  i_base: int;
  i_type: int;
  i_in: int;
  i_with: int;
  i_match_clause: int;
  i_ppx_stritem_ext: int;
  i_max_indent: int option;
  i_strict_with: threechoices;
  i_strict_else: threechoices;
  i_strict_comments: bool;
  i_align_ops: bool;
  i_align_params: threechoices;
}

let default = {
  i_base = 2;
  i_type = 2;
  i_in = 0;
  i_with = 0;
  i_match_clause = 2;
  i_ppx_stritem_ext = 2;
  i_max_indent = Some 4;
  i_strict_with = Never;
  i_strict_else = Always;
  i_strict_comments = false;
  i_align_ops = true;
  i_align_params = Auto;
}

let presets = [
  "apprentice",
  { i_base = 2; i_type = 4; i_in = 2; i_with = 2; i_match_clause = 4;
    i_ppx_stritem_ext = 2;
    i_max_indent = None;
    i_strict_with = Never; i_strict_else = Always; i_strict_comments = false;
    i_align_ops = true; i_align_params = Always };
  "normal",
  default;
  "JaneStreet",
  { i_base = 2; i_type = 2; i_in = 0; i_with = 0; i_match_clause = 2;
    i_ppx_stritem_ext = 2;
    i_max_indent = Some 2;
    i_strict_with = Auto; i_strict_else = Always; i_strict_comments = true;
    i_align_ops = true; i_align_params = Always };
]

let threechoices_of_string = function
  | "always" -> Always
  | "never" -> Never
  | "auto" -> Auto
  | _ -> failwith "threechoices_of_string"

let string_of_threechoices = function
  | Always -> "always"
  | Never -> "never"
  | Auto -> "auto"

let intoption_of_string = function
  | "none" | "None" -> None
  | n ->
      try Some (int_of_string n)
      with Failure _ ->
          failwith "intoption_of_string"

let string_of_intoption = function
  | Some n -> string_of_int n
  | None -> "none"

let to_string ?(sep=",") indent =
  Printf.sprintf
    "base = %d%s\
     type = %d%s\
     in = %d%s\
     with = %d%s\
     match_clause = %d%s\
     ppx_stritem_ext = %d%s\
     max_indent = %s%s\
     strict_with = %s%s\
     strict_else = %s%s\
     strict_comments = %b%s\
     align_ops = %b%s\
     align_params = %s"
    indent.i_base sep
    indent.i_type sep
    indent.i_in sep
    indent.i_with sep
    indent.i_match_clause sep
    indent.i_ppx_stritem_ext sep
    (string_of_intoption indent.i_max_indent) sep
    (string_of_threechoices indent.i_strict_with) sep
    (string_of_threechoices indent.i_strict_else) sep
    indent.i_strict_comments sep
    indent.i_align_ops sep
    (string_of_threechoices indent.i_align_params)

let set ?(extra=fun _ -> None) t var_name value =
  try
    match var_name with
    | "base" -> {t with i_base = int_of_string value}
    | "type" -> {t with i_type = int_of_string value}
    | "in" -> {t with i_in = int_of_string value}
    | "with" -> {t with i_with = int_of_string value}
    | "match_clause" -> {t with i_match_clause = int_of_string value}
    | "ppx_stritem_ext" -> {t with i_ppx_stritem_ext = int_of_string value}
    | "max_indent" -> {t with i_max_indent = intoption_of_string value}
    | "strict_with" -> {t with i_strict_with = threechoices_of_string value}
    | "strict_else" -> {t with i_strict_else = threechoices_of_string value}
    | "with_never" -> (* backwards compat, don't document *)
        {t with i_strict_with = if bool_of_string value then Always else Never}
    | "strict_comments" -> {t with i_strict_comments = bool_of_string value}
    | "align_ops" -> {t with i_align_ops = bool_of_string value}
    | "align_params" -> {t with i_align_params = threechoices_of_string value}
    | var_name ->
        match extra var_name with
        | Some f -> f value; t
        | None ->
            let e = Printf.sprintf "unknown configuration key %S" var_name in
            raise (Invalid_argument e)
  with
  | Failure "int_of_string" ->
      let e = Printf.sprintf "%s should be an integer, not %S" var_name value in
      raise (Invalid_argument e)
  | Failure "bool_of_string" ->
      let e =
        Printf.sprintf "%s should be either \"true\" or \"false\", not %S"
          var_name value
      in
      raise (Invalid_argument e)
  | Failure "threechoices_of_string" ->
      let e =
        Printf.sprintf
          "%s should be either \"always\", \"never\" or \"auto\", not %S"
          var_name value
      in
      raise (Invalid_argument e)
  | Failure "intoption_of_string" ->
      let e =
        Printf.sprintf
          "%s should be either an integer or \"none\", not %S"
          var_name value
      in
      raise (Invalid_argument e)

let update_from_string ?extra indent s =
  List.fold_left
    (fun indent s -> match Util.string_split '=' s with
      | [] | [""] -> indent
      | [var;value] -> set ?extra indent (String.trim var) (String.trim value)
      | [preset] ->
          (try List.assoc (String.trim preset) presets with
             Not_found ->
               let e = Printf.sprintf "unknown preset %S" preset in
               raise (Invalid_argument e))
      | _ ->
          let e = Printf.sprintf "wrong \"param=value\" pair in %S" s in
          raise (Invalid_argument e))
    indent
    (Util.string_split_chars ",\n" s)

(* Remember to also document the template configuration file ../.ocp-indent *)

type man_block =
  [ `S of string | `P of string | `Pre of string | `I of string * string
  | `Noblank | `Blocks of man_block list ]

let man =
  let option_name name kind default =
    Printf.sprintf "$(b,%s)=%s (default=%s)" name kind default
  in
  let pre s =
    List.fold_right
      (fun line acc ->
         let i = ref 0 and line = Bytes.copy line in
         while !i < Bytes.length line && Bytes.get line (!i) = ' ' do
           Bytes.set line (!i) '\xa0'; incr i done;
         let line = Bytes.to_string line in
         `P line :: (if acc = [] then [] else `Noblank :: acc))
      (List.map Bytes.of_string (Util.string_split '\n' s)) []
  in
  [ `P "A configuration definition is a list of bindings in the form \
        $(i,NAME=VALUE) or of $(i,PRESET), separated by commas or newlines";
    `P "Syntax: $(b,[PRESET,]VAR=VALUE[,VAR=VALUE...])"
  ]
  @
    `I (option_name "base" "INT" (string_of_int default.i_base),
        "Indentation used when none of the following options applies.")
    :: pre "        let foo =\n\
           \        $(b,..)bar"
  @
    `I (option_name "type" "INT" (string_of_int default.i_type),
        "Indentation for type definitions.")
    :: pre "        type t =\n\
           \        $(b,..)int"
  @
    `I (option_name "in" "INT" (string_of_int default.i_in),
        "Indentation after `let ... in', unless followed by another `let'.")
    :: pre "        let foo = () in\n\
           \        $(b,..)bar"
  @
    `I (option_name "with" "INT" (string_of_int default.i_with),
        "Indentation after `match ... with', `try ... with' or `function'.")
    :: pre "        match foo with\n\
           \        $(b,..)| _ -> bar"
  @
    `I (option_name "match_clause" "INT" (string_of_int default.i_match_clause),
        "Indentation for clauses inside a pattern-match (after arrows).")
    :: pre "        match foo with\n\
           \        | _ ->\n\
           \        $(b,..)bar"
  @
    `I (option_name "ppx_stritem_ext" "INT" (string_of_int default.i_ppx_stritem_ext),
        "Indentation for items inside a [%%id ... ] extension node).")
    :: pre "        [%% id.id\n\
           \        $(b,..)let x = 3\
           \        ]"
  @
    `I (option_name "max_indent" "<INT|none>"
          (string_of_intoption default.i_max_indent),
        "When nesting expressions on the same line, their indentations are \
         stacked in some cases so that they remain correct if you close them \
         one per line. However, this can lead to large indentations in complex \
         code, so this parameter sets a maximum indentation. Note that it \
         only affects indentation after function arrows and opening parens at \
         the ends of lines.")
    :: pre "        let f = g (h (i (fun x ->\n\
           \        $(b,....)x)\n\
           \          )\n\
           \        )"
  @
    `I (option_name "strict_with" "<always|never|auto>"
          (string_of_threechoices default.i_strict_with),
        "If `never', match bars are indented, superseding `with', \
         whenever `match with' doesn't start its line.\n\
         If `auto', there are exceptions for constructs like \
         `begin match with'.\n\
         If `always', `with' is always strictly respected, and additionally \
         applies to variant types definition, for consistency.")
    :: pre "    Example with `strict_with=$(b,never),with=0':\n\
           \        begin match foo with\n\
           \        $(b,..)| _ -> bar\n\
           \        end"
  @
    `I (option_name "strict_else" "<always|never|auto>"
          (string_of_threechoices default.i_strict_else),
        "If `always', indent after the `else' keyword normally, like after \
         `then'.\n\
         If `auto', indent after `else' unless in a few \
         \"unclosable\" cases (`let .... in', `match', etc.).\n\
         If `never', the `else' keyword won't indent when followed \
         by a newline.")
    :: pre "    Example with `strict_else=$(b,auto)':\n\
           \        if cond then\n\
           \          foo\n\
           \        else\n\
           \        $(b,let) x = bar in\n\
           \        baz"
  @
    `I (option_name "strict_comments" "BOOL"
          (string_of_bool default.i_strict_comments),
        "In-comment indentation is normally preserved, as long as it respects \
         the left margin or the comments starts with a newline. Setting this \
         to `true' forces alignment within comments. Lines starting with `*' \
         are always aligned")
    :: []
  @
    `I (option_name "align_ops" "BOOL"
          (string_of_bool default.i_align_ops),
        "Toggles preference of column-alignment over line indentation for most \
         of the common operators and after mid-line opening parentheses.")
    :: pre "    Example with `align_ops=$(b,true)':\n\
           \        let f x = x\n\
           \                  + y\n\
           \ \n\
           \    Example with `align_ops=$(b,false)':\n\
           \        let f x = x\n\
           \          + y"
  @
    `I (option_name "align_params" "<always|never|auto>"
          (string_of_threechoices default.i_align_params),
        "If `never', function parameters are indented one level from the \
         line of the function. \
         If `always', they are aligned from the column of the function. \
         if `auto', alignment is chosen over indentation in a few cases, e.g. \
         after match arrows")
    :: pre "    Example with `align_params=$(b,never)':\n\
           \        match foo with\n\
           \        | _ -> some_fun\n\
           \          $(b,..)parameter\n\
           \ \n\
           \    Example with `align_params=$(b,always)' or `$(b,auto)':\n\
           \        match foo with\n\
           \        | _ -> some_fun\n\
           \               $(b,..)parameter"
  @ [
    `P "Available presets are `normal', the default, `apprentice' which may \
        make some aspects of the syntax more obvious for beginners, and \
        `JaneStreet'."
  ]


let save t file =
  try
    let oc = open_out file in
    output_string oc (to_string ~sep:"\n" t);
    output_char oc '\n';
    true
  with Sys_error _ ->
      Printf.eprintf
        "ocp-indent warning: could not open %S for writing configuration.\n%!"
        file;
      false

let syntax_ext syntax_list_ref dynlink_list_ref = function
  | "syntax" ->
      Some
        (fun syntaxes ->
           List.iter
             (fun syn ->
                (* if List.mem syn (IndentExt.available ()) then *)
                  syntax_list_ref := syn :: !syntax_list_ref
                (* else *)
                (*   let e = Printf.sprintf "unknown syntax extension %S" syn in *)
                (*   raise (Invalid_argument e) *)
             )
             (Util.string_split ' ' syntaxes))
  | "load" ->
      Some
        (fun pkgs ->
           List.iter
             (fun s ->
                let dl =
                  if Filename.check_suffix s ".cmo"
                  || Filename.check_suffix s ".cma"
                  || Filename.check_suffix s ".cmxs"
                  then `Mod s
                  else `Pkg s in
                dynlink_list_ref := dl :: !dynlink_list_ref)
             (Util.string_split ' ' pkgs))
  | _ -> None

let load ?(indent=default) file =
  try
    let ic = open_in file in
    let contents =
      let b = Buffer.create 512 in
      try while true do
          let s = input_line ic in
          let n = try String.index s '#' with Not_found -> String.length s in
          Buffer.add_substring b s 0 n;
          Buffer.add_char b '\n'
      done; assert false
      with End_of_file -> close_in ic; Buffer.contents b
    in
    let exts = ref [] in
    let dynlink = ref [] in
    let t = update_from_string ~extra:(syntax_ext exts dynlink) indent contents in
    t, !exts, !dynlink
  with
  | Sys_error _ ->
      Printf.eprintf
        "ocp-indent warning: could not open %S for reading configuration.\n%!"
        file;
      indent, [], []
  | Invalid_argument err ->
      Printf.eprintf
        "ocp-indent warning: error in configuration file %S:\n%s\n%!"
        file err;
      default, [], []

let conf_file_name = ".ocp-indent"

let rec find_conf_file path =
  let (/) = Filename.concat in
  if Sys.file_exists (path / conf_file_name)
  then Some (path / conf_file_name)
  else
    let path =
      if Filename.is_relative path then Sys.getcwd () / path
      else path
    in
    let parent = Filename.dirname path in
    if parent <> path then find_conf_file parent
    else None

let local_default ?(path=Sys.getcwd()) () =
  let conf = default in
  let conf, syn, dlink =
    try
      let (/) = Filename.concat in
      let xdg_path = ( match Sys.getenv "XDG_CONFIG_HOME" with
          | "" -> (Sys.getenv "HOME") / ".config"
          | exception Not_found -> (Sys.getenv "HOME") / ".config"
          | x -> x ) / "ocp" / "ocp-indent.conf" in
      if Sys.file_exists xdg_path
      then load ~indent:conf xdg_path
      else let legacy_path = (Sys.getenv "HOME") / ".ocp" / "ocp-indent.conf" in
        if  Sys.file_exists legacy_path
        then load ~indent:conf legacy_path
        else conf, [], []
    with Not_found -> conf, [], []
  in
  let conf, syn, dlink = match find_conf_file path with
    | Some c ->
        let conf, syn1, dlink1 = load ~indent:conf c in
        conf, syn1@syn, dlink1@dlink
    | None -> conf, syn, dlink
  in
  let conf =
    try update_from_string conf
          (Sys.getenv ("OCP_INDENT_CONFIG"))
    with
    | Not_found -> conf
    | Invalid_argument _ ->
        prerr_endline "Warning: invalid $OCP_INDENT_CONFIG";
        conf
  in
  conf, syn, dlink
