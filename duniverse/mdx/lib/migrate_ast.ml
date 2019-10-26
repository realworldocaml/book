(*
 *  This file originates from OCamlFormat.
 *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.
 *
 *  This source code is licensed under the MIT license.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 *  in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open Compat

(* original modules *)
module Asttypes_ = Asttypes
module Parsetree_ = Parsetree

include (
  Migrate_parsetree.Ast_406 :
  module type of struct
      include Migrate_parsetree.Ast_406
    end
    with module Location := Migrate_parsetree.Ast_406.Location
     and module Outcometree := Migrate_parsetree.Ast_406.Outcometree
     and module Asttypes := Migrate_parsetree.Ast_406.Asttypes
     and module Ast_helper := Migrate_parsetree.Ast_406.Ast_helper
     and module Parsetree := Migrate_parsetree.Ast_406.Parsetree
 )

module Asttypes = Migrate_parsetree.Ast_406.Asttypes
module Ast_helper = Migrate_parsetree.Ast_406.Ast_helper
module Parsetree = Migrate_parsetree.Ast_406.Parsetree

module Parse = struct
  open Migrate_parsetree

  let toplevel_phrase lexbuf =
    Parse.toplevel_phrase Versions.ocaml_406 lexbuf

  let implementation lexbuf =
    Parse.implementation Versions.ocaml_406 lexbuf

  let interface lexbuf =
    Parse.interface Versions.ocaml_406 lexbuf
end

let to_current =
  Migrate_parsetree.Versions.(migrate ocaml_406 ocaml_current)

let to_406 =
  Migrate_parsetree.Versions.(migrate ocaml_current ocaml_406)

module Typemod = struct
  open Typemod

  let type_structure e s l =
    type_structure e (to_current.copy_structure s) l
end

module Printast = struct
  open Printast

#if OCAML_MAJOR = 4 && OCAML_MINOR >= 8
  let copy_directive_argument (x : Parsetree.directive_argument) =
    let open Migrate_parsetree.Versions.OCaml_current.Ast.Parsetree in
    match x with
    | Pdir_none -> None
    | Pdir_string s -> Some ({ pdira_desc = Pdir_string s; pdira_loc = Location.none })
    | Pdir_int (s, c) -> Some ({ pdira_desc = Pdir_int (s, c); pdira_loc = Location.none })
    | Pdir_ident i -> Some ({ pdira_desc = Pdir_ident i; pdira_loc = Location.none })
    | Pdir_bool b -> Some ({ pdira_desc = Pdir_bool b; pdira_loc = Location.none })

  let top_phrase f (x : Parsetree.toplevel_phrase) =
    match x with
    | Ptop_def s ->
       top_phrase f (Ptop_def (to_current.copy_structure s))
    | Ptop_dir (d, a) ->
      top_phrase f (Ptop_dir { pdir_name = Location.mknoloc d
                             ; pdir_arg = copy_directive_argument a
                             ; pdir_loc = Location.none})
#else
  let copy_directive_argument (x : Parsetree.directive_argument) =
    let open Migrate_parsetree.Versions.OCaml_current.Ast.Parsetree in
    match x with
    | Pdir_none -> Pdir_none
    | Pdir_string s -> Pdir_string s
#if OCAML_MAJOR = 4 && OCAML_MINOR <= 2
    | Pdir_int (s, _) -> Pdir_int (int_of_string s)
#else
    | Pdir_int (s, c) -> Pdir_int (s, c)
#endif
    | Pdir_ident i -> Pdir_ident i
    | Pdir_bool b -> Pdir_bool b

  let top_phrase f (x : Parsetree.toplevel_phrase) =
    match x with
    | Ptop_def s ->
       top_phrase f (Ptop_def (to_current.copy_structure s))
    | Ptop_dir (d, a) ->
       top_phrase f (Ptop_dir (d, copy_directive_argument a))
#endif

end

module Pprintast = struct
  open Pprintast

  let top_phrase f x = top_phrase f (to_current.copy_toplevel_phrase x)
end

module Printtyp = struct
  include Printtyp

  let wrap_printing_env e f =
    wrap_printing_env
#if (OCAML_MAJOR = 4 && OCAML_MINOR >= 7) || OCAML_MAJOR > 4
      ~error:false
#endif
      e f
end

module Pparse = struct
  open Pparse

  let apply_rewriters_str ~tool_name s =
    apply_rewriters_str ~tool_name (to_current.copy_structure s)
    |> to_406.copy_structure
end

module Position = struct
  open Lexing

  let column {pos_fname=_; pos_lnum=_; pos_bol; pos_cnum} = pos_cnum - pos_bol

  let fmt fs {pos_fname=_; pos_lnum; pos_bol; pos_cnum} =
    if pos_lnum = -1 then Format.fprintf fs "[%d]" pos_cnum
    else Format.fprintf fs "[%d,%d+%d]" pos_lnum pos_bol (pos_cnum - pos_bol)

  let compare_col p1 p2 = (column p1) - (column p2)

  let equal p1 p2 =
    String.equal p1.pos_fname p2.pos_fname
    && p1.pos_lnum = p2.pos_lnum
    && p1.pos_bol = p2.pos_bol
    && p1.pos_cnum = p2.pos_cnum

  let compare p1 p2 =
    if equal p1 p2 then 0 else p1.pos_cnum - p2.pos_cnum

  let distance p1 p2 = p2.pos_cnum - p1.pos_cnum
end

module Location = struct
  include Migrate_parsetree.Ast_406.Location

  let fmt fs {loc_start; loc_end; loc_ghost} =
    Format.fprintf fs "(%a..%a)%s" Position.fmt loc_start Position.fmt
      loc_end
      (if loc_ghost then " ghost" else "")

  let to_string x = Format.asprintf "%a" fmt x

  let compare x y =
    let compare_start = Position.compare x.loc_start y.loc_start in
    if compare_start = 0 then Position.compare x.loc_end y.loc_end
    else compare_start

  let hash = Hashtbl.hash

  let is_single_line x = x.loc_start.pos_lnum = x.loc_end.pos_lnum

  let compare_start x y = Position.compare x.loc_start y.loc_start

  let compare_start_col x y = Position.compare_col x.loc_start y.loc_start

  let compare_end x y = Position.compare x.loc_end y.loc_end

  let compare_end_col x y = Position.compare_col x.loc_end y.loc_end

  let contains l1 l2 = compare_start l1 l2 <= 0 && compare_end l1 l2 >= 0

  let width x = Position.distance x.loc_start x.loc_end

  let compare_width_decreasing l1 l2 = (width l2) - (width l1)
end
