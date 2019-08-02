(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Astring

type section = int * string

type cram_value = { pad: int; tests: Cram.t list }

type value =
  | Raw
  | OCaml
  | Error of string list
  | Cram of cram_value
  | Toplevel of Toplevel.t list

type t = {
  line    : int;
  file    : string;
  section : section option;
  labels  :
    (string * ([`Eq | `Neq | `Le | `Lt | `Ge | `Gt] * string) option) list;
  header  : string option;
  contents: string list;
  value   : value;
}

let empty = {
  line=0;
  file="";
  section=None;
  labels=[];
  header=None;
  contents=[];
  value=Raw
}

let dump_string ppf s = Fmt.pf ppf "%S" s
let dump_section = Fmt.(Dump.pair int string)

let dump_value ppf = function
  | Raw -> Fmt.string ppf "Raw"
  | OCaml -> Fmt.string ppf "OCaml"
  | Error e -> Fmt.pf ppf "Error %a" Fmt.(Dump.list dump_string) e
  | Cram { pad; tests } ->
    Fmt.pf ppf "@[Cram@ {pad=%d;@ tests=%a}@]"
      pad Fmt.(Dump.list Cram.dump) tests
  | Toplevel tests ->
    Fmt.pf ppf "@[Toplevel %a@]" Fmt.(Dump.list Toplevel.dump) tests

let dump_relation ppf = function
  | `Eq -> Fmt.string ppf "="
  | `Neq -> Fmt.string ppf "<>"
  | `Gt -> Fmt.string ppf ">"
  | `Ge -> Fmt.string ppf ">="
  | `Lt -> Fmt.string ppf "<"
  | `Le -> Fmt.string ppf "<="

let dump_labels =
  Fmt.(Dump.(list (pair dump_string (option (pair dump_relation dump_string)))))

let dump ppf { file; line; section; labels; header; contents; value } =
  Fmt.pf ppf
    "{@[file: %s;@ line: %d;@ section: %a;@ labels: %a;@ header: %a;@
        contents: %a;@ value: %a@]}"
    file line
    Fmt.(Dump.option dump_section) section
    dump_labels labels
    Fmt.(Dump.option string) header
    Fmt.(Dump.list dump_string) contents
    dump_value value

let pp_lines syntax =
  let pp =
    match syntax with
    | Some Syntax.Cram -> Fmt.fmt "  %s"
    | _ -> Fmt.string
  in
  Fmt.(list ~sep:(unit "\n") pp)
let pp_contents ?syntax ppf t = Fmt.pf ppf "%a\n" (pp_lines syntax) t.contents
let pp_footer ?syntax ppf () =
  match syntax with
  | Some Syntax.Cram -> ()
  | _ -> Fmt.string ppf "```\n"

let pp_cmp ppf = function
  | `Eq -> Fmt.pf ppf "="
  | `Neq -> Fmt.pf ppf "<>"
  | `Lt -> Fmt.pf ppf "<"
  | `Le -> Fmt.pf ppf "<="
  | `Gt -> Fmt.pf ppf ">"
  | `Ge -> Fmt.pf ppf ">="

let pp_label ppf (k, v) = match v with
  | None   -> Fmt.string ppf k
  | Some (o, v) -> Fmt.pf ppf "%s%a%s" k pp_cmp o v

let pp_labels ppf = function
  | [] -> ()
  | l  -> Fmt.pf ppf " %a" Fmt.(list ~sep:(unit ",") pp_label) l

let pp_header ?syntax ppf t =
  match syntax with
  | Some Syntax.Cram ->
    assert (t.header = Syntax.cram_default_header);
    begin match t.labels with
    | [] -> ()
    | ["non-deterministic", Some (`Eq, choice)] ->
      Fmt.pf ppf "<-- non-deterministic %s\n" choice
    | _ ->
      let err =
        Fmt.strf "Block.pp_header: [ %a ]" pp_labels t.labels
      in
      invalid_arg err
    end
  | _ ->
    Fmt.pf ppf "```%a%a\n" Fmt.(option string) t.header pp_labels t.labels

let pp_error ppf b =
  match b.value with
  | Error e -> List.iter (fun e -> Fmt.pf ppf ">> @[<h>%a@]@." Fmt.words e) e
  | _ -> ()

let pp ?syntax ppf b =
  pp_header ?syntax ppf b;
  pp_error ppf b;
  pp_contents ?syntax ppf b;
  pp_footer ?syntax ppf ()

let labels = [
  `Label "dir"              , [`Any];
  `Label "source-tree"      , [`Any];
  `Label "file"             , [`Any];
  `Label "part"             , [`Any];
  `Label "env"              , [`Any];
  `Label "skip"             , [`None];
  `Label "non-deterministic", [`None; `Some "command"; `Some "output"];
  `Label "version"          , [`Any];
  `Label "require-package"  , [`Any];
  `Prefix "set-"            , [`Any];
  `Prefix "unset-"          , [`None];
]

let pp_value ppf = function
  | `Any   -> Fmt.string ppf "*"
  | `None   -> Fmt.string ppf "<none>"
  | `Some v -> dump_string ppf v

let match_label l p = match p, l with
  | `Any   , Some _ -> true
  | `None  , None   -> true
  | `Some p, Some (_, l) -> String.equal p l
  | _ -> false

let pp_v ppf = function
  | None   -> Fmt.string ppf "<none>"
  | Some (_, v) -> Fmt.string ppf v

let rec pp_list pp ppf = function
  | []    -> ()
  | [x]   -> pp ppf x
  | [x;y] -> Fmt.pf ppf "%a and %a" pp x pp y
  | h::t  -> Fmt.pf ppf "%a, %a" pp h (pp_list pp) t

let check_labels t =
  List.fold_left (fun acc (k, v) ->
      try
        let f = function
        | `Label s, _ -> s = k
        | `Prefix s, _ -> String.equal (String.with_range ~len:(String.length s) k) s in
        let _, vs = List.find f labels in
        if List.exists (match_label v) vs then acc
        else
          Fmt.strf "%a is not a valid value for label %S. \
                    Valid values are %a."
            pp_v v k (pp_list pp_value) vs
          :: acc
      with Not_found ->
        let f = function
        | `Label _, _ -> true
        | _ -> false in
        let g = function `Label s, _ | `Prefix s, _ -> s in
        let ls, ps = List.partition f labels in
        Fmt.strf "%S is not a valid label or prefix. \
                  Valid labels are %a and valid prefixes are %a."
          k (pp_list dump_string) (List.map g ls)
          (pp_list dump_string) (List.map g ps)
        :: acc
    ) [] t.labels
  |> function
  | [] -> Result.Ok ()
  | es -> Result.Error es

let get_label t label =
  try Some (List.assoc label t.labels)
  with Not_found -> None

let get_labels t label =
  List.fold_left (fun acc (k, v) ->
      if String.equal k label then match v with
        | None   -> assert false
        | Some v -> v ::acc
      else acc
    ) [] t.labels

let get_prefixed_labels t prefix =
  List.fold_left (fun acc (k, v) ->
      if String.equal (String.with_range ~len:(String.length prefix) k) prefix then match v with
        | None   -> (String.with_range ~first:(String.length prefix) k, None)::acc
        | Some (e, s) -> (String.with_range ~first:(String.length prefix) k, Some (e, s))::acc
      else acc
    ) [] t.labels

let directory t = match get_label t "dir" with
  | None   -> None
  | Some None -> None
  | Some (Some (`Eq, d)) -> Some d
  | Some (Some _) -> Fmt.failwith "invalid `dir` label value"

let file t = match get_label t "file" with
  | None   -> None
  | Some None -> None
  | Some (Some (`Eq, f)) -> Some f
  | Some (Some _) -> Fmt.failwith "invalid `file` label value"

let part t = match get_label t "part" with
  | None   -> None
  | Some None -> None
  | Some (Some (`Eq, l)) -> Some l
  | Some (Some _) -> Fmt.failwith "invalid `part` label value"

let version t = match get_label t "version" with
  | Some (Some (op, v)) ->
    let x, y, z = Misc.parse_version v in
    op, x, y, z
  | _ -> `Eq, None, None, None

let source_trees t =
  let f = function
    | `Eq, x -> x
    | _ -> Fmt.failwith "invalid `source-tree` label value"
  in
  List.map f (get_labels t "source-tree")

let mode t = match get_label t "non-deterministic" with
  | None                  -> `Normal
  | Some None
  | Some (Some (`Eq, "output"))  -> `Non_det `Output
  | Some (Some (`Eq, "command")) -> `Non_det `Command
  | Some (Some (`Eq, _))         -> `Normal
  | Some (Some _) -> Fmt.failwith "invalid `non-deterministic` label value"

let skip t = match get_label t "skip" with
  | Some None -> true
  | _ -> false

let environment t = match get_label t "env" with
  | None
  | Some None
  | Some (Some (`Eq, "default")) -> "default"
  | Some (Some (`Eq, s)) -> s
  | Some (Some _) -> Fmt.failwith "invalid `env` label value"

let set_variables t =
  let f = function
    | _, None -> Fmt.failwith "invalid env variable value (use '=' followed by the value)"
    | variable, Some (`Eq, value) -> variable, value
    | _ -> Fmt.failwith "invalid env variable operator (use '=' only)"
  in
  List.map f (get_prefixed_labels t "set-")

let unset_variables t =
  let f = function
    | variable, None -> variable
    | _ -> Fmt.failwith "invalid env variable operator (no operator allowed)"
  in
  List.map f (get_prefixed_labels t "unset-")

let required_packages t =
  let f = function
    | `Eq, "" ->
      Fmt.failwith "invalid `require-package` label value: requires a value"
    | `Eq, pkg -> pkg
    | _ -> Fmt.failwith "invalid `env` label value"
  in
  List.map f (get_labels t "require-package")

let require_re =
  let open Re in
  seq [str "#require \""; group (rep1 any); str "\""]

let require_from_line line =
  let open Rresult.R.Infix in
  let re = Re.compile require_re in
  match Re.exec_opt re line with
  | None -> Ok Library.Set.empty
  | Some group ->
    let matched = Re.Group.get group 1 in
    let libs_str = String.cuts ~sep:"," matched in
    Util.Result.List.map ~f:Library.from_string libs_str >>| fun libs ->
    List.fold_left (fun acc l -> Library.Set.add l acc) Library.Set.empty libs

let require_from_lines lines =
  let open Rresult.R.Infix in
  Util.Result.List.map ~f:require_from_line lines >>| fun libs ->
  List.fold_left Library.Set.union Library.Set.empty libs

let required_libraries t = require_from_lines t.contents

let value t = t.value
let section t = t.section
let header t = t.header

let cram lines =
  let pad, tests = Cram.of_lines lines in
  Cram { pad; tests }

let guess_ocaml_kind b =
  let rec aux = function
    | []     -> `Code
    | h :: t ->
      let h = String.trim h in
      if h = "" then aux t
      else if String.length h > 1 && h.[0] = '#' then `Toplevel
      else `Code
  in
  match b.header, b.contents with
  | Some "ocaml", t -> `OCaml (aux t)
  | _ -> `Other

let toplevel ~file ~line lines = Toplevel (Toplevel.of_lines ~line ~file lines)

let eval t =
  match check_labels t with
  | Error e -> { t with value = Error e }
  | Ok ()   ->
    match t.header with
    | Some ("sh" | "bash") ->
      let value = cram t.contents in
      { t with value }
    | Some "ocaml" ->
      (match guess_ocaml_kind t with
       | `OCaml `Code -> { t with value = OCaml }
       | `OCaml `Toplevel ->
         let value = toplevel ~file:t.file ~line:t.line t.contents in
         { t with value }
       | `Other -> { t with value = Raw })
    | _ -> t

let ends_by_semi_semi c = match List.rev c with
  | h::_ ->
    let len = String.length h in
    len > 2 && h.[len-1] = ';' && h.[len-2] = ';'
  | _ -> false

let pp_line_directive ppf (file, line) = Fmt.pf ppf "#%d %S" line file
let line_directive = Fmt.to_to_string pp_line_directive

let executable_contents b =
  let contents =
    match guess_ocaml_kind b with
    | `OCaml `Code -> b.contents
    | `OCaml `Toplevel | `Other ->
      match b.value with
      | Error _ | Raw | Cram _ -> []
      | OCaml -> line_directive (b.file, b.line) :: b.contents
      | Toplevel tests ->
        List.flatten (
          List.map (fun t ->
              match Toplevel.command t with
              | [] -> []
              | cs ->
                let mk s = String.v ~len:(t.hpad+2) (fun _ -> ' ') ^ s in
                line_directive (b.file, t.line) :: List.map mk cs
            ) tests)
  in
  if contents = [] || ends_by_semi_semi contents then contents
  else contents @ [";;"]

let split sep s op =
  match String.cut ~sep s with
  | None -> s, None (* operator does not matter here *)
  | Some (k, v) -> k, Some (op, v)

let ( ||| ) x y =
  match x with
  | _, None -> y
  | x -> x

let labels_of_string s =
  let labels = String.cuts ~empty:false ~sep:"," s in
  List.map (fun s ->
      split "<>" s `Neq |||
      split ">=" s `Ge |||
      split ">"  s `Gt |||
      split "<=" s `Le |||
      split "<"  s `Lt |||
      split "="  s `Eq
    ) labels

let compare_versions v1 v2 =
  match (v1, v2) with
  | (Some _, Some _, Some _), (None, _, _) -> 0
  | (Some x, Some _, Some _), (Some x', None, _) -> x - x'
  | (Some x, Some y, Some _), (Some x', Some y', None) ->
    if x = x' then y - y' else x - x'
  | (Some x, Some y, Some z), (Some x', Some y', Some z') ->
    if x = x' then
      if y = y' then z - z'
      else y - y'
    else x - x'
  | _ -> Fmt.failwith "incomplete OCaml version"

let compare = function
  | `Eq -> ( = )
  | `Neq -> ( <> )
  | `Lt -> ( < )
  | `Le -> ( <= )
  | `Gt -> ( > )
  | `Ge -> ( >= )

let version_enabled t =
  let curr_version = Misc.parse_version Sys.ocaml_version in
  let op, x, y, z = version t in
  (compare op) (compare_versions curr_version (x, y, z)) 0
