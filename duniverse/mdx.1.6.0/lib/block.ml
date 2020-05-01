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
open Result

module List = Compat.List

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
  labels  : Label.t list;
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

let dump ppf { file; line; section; labels; header; contents; value } =
  Fmt.pf ppf
    "{@[file: %s;@ line: %d;@ section: %a;@ labels: %a;@ header: %a;@
        contents: %a;@ value: %a@]}"
    file line
    Fmt.(Dump.option dump_section) section
    Fmt.Dump.(list Label.pp) labels
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

let pp_labels ppf = function
  | [] -> ()
  | l  -> Fmt.pf ppf " %a" Fmt.(list ~sep:(unit ",") Label.pp) l

let pp_header ?syntax ppf t =
  match syntax with
  | Some Syntax.Cram ->
    assert (t.header = Syntax.cram_default_header);
    begin match t.labels with
    | [] -> ()
    | [Non_det None] -> Fmt.pf ppf "<-- non-deterministic\n"
    | [Non_det (Some Nd_output)] -> Fmt.pf ppf "<-- non-deterministic output\n"
    | [Non_det (Some Nd_command)] -> Fmt.pf ppf "<-- non-deterministic command\n"
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

let get_label f t = Util.List.find_map f t.labels

let get_label_or f ~default t =
  Util.Option.value ~default (Util.List.find_map f t.labels)

let directory t = get_label (function Dir x -> Some x | _ -> None) t

let file t = get_label (function File x -> Some x | _ -> None) t

let part t = get_label (function Part x -> Some x | _ -> None) t

let version t = get_label (function Version (x, y) -> Some (x, y) | _ -> None) t

let source_trees t =
  List.filter_map (function Label.Source_tree x -> Some x | _ -> None) t.labels

let mode t =
  get_label_or
    (function
      | Non_det (Some mode) -> Some (`Non_det mode)
      | Non_det None -> Some (`Non_det Label.default_non_det)
      | _ -> None)
    ~default:`Normal
    t

let skip t = List.exists (function Label.Skip -> true | _ -> false) t.labels

let environment t =
  get_label_or (function Label.Env e -> Some e | _ -> None) ~default:"default" t

let set_variables t =
  List.filter_map
    (function Label.Set (v, x) -> Some (v, x) | _ -> None)
    t.labels

let unset_variables t =
  List.filter_map (function Label.Unset x -> Some x | _ -> None) t.labels

let explicit_required_packages t =
  List.filter_map
    (function Label.Require_package x -> Some x | _ -> None) t.labels

let require_re =
  let open Re in
  seq [str "#require \""; group (rep1 any); str "\""]

let require_from_line line =
  let open Util.Result.Infix in
  let re = Re.compile require_re in
  match Re.exec_opt re line with
  | None -> Ok Library.Set.empty
  | Some group ->
    let matched = Re.Group.get group 1 in
    let libs_str = String.cuts ~sep:"," matched in
    Util.Result.List.map ~f:Library.from_string libs_str >>| fun libs ->
    Library.Set.of_list libs

let require_from_lines lines =
  let open Util.Result.Infix in
  Util.Result.List.map ~f:require_from_line lines >>| fun libs ->
  List.fold_left Library.Set.union Library.Set.empty libs

let required_libraries = function
  | { value = Toplevel _; contents; _} -> require_from_lines contents
  | { value = (Raw | OCaml | Error _ | Cram _); _ } -> Ok Library.Set.empty

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
     | `Other -> Fmt.failwith "Dead code. todo: remove"
    )
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

let version_enabled t =
  match Ocaml_version.of_string Sys.ocaml_version with
  | Ok curr_version -> (
      match version t with
      | Some (op, v) ->
        Label.Relation.compare op (Ocaml_version.compare curr_version v) 0
      | None -> true )
  | Error (`Msg e) -> Fmt.failwith "invalid OCaml version: %s" e
