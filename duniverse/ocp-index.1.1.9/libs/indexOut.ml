(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Lesser GNU Public License version 3.0.                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

open IndexTypes

let option_iter opt f = match opt with
  | Some x -> f x
  | None -> ()

let rec string_index_from s i chars =
  if i >= String.length s then raise Not_found
  else if String.contains chars s.[i] then i
  else string_index_from s (i+1) chars

module IndexFormat = struct

  let list
      ?(paren=false) ?(left=fun _ -> ()) ?(right=fun _ -> ())
      pr sep fmt lst
    =
    let rec aux = function
      | [] -> ()
      | [x] -> pr fmt x
      | x::r -> pr fmt x; sep fmt (); aux r
    in
    match lst with
    | [] -> ()
    | [x] -> left fmt; pr fmt x; right fmt
    | _::_::_ ->
        if paren then Format.pp_print_char fmt '(';
        left fmt; aux lst; right fmt;
        if paren then Format.pp_print_char fmt ')'

  let lines ?(escaped=false) fmt str =
    let len = String.length str in
    let esc = if escaped then String.escaped else fun s -> s in
    let rec aux i =
      if i >= len then () else
        let j = try String.index_from str i '\n' with Not_found -> len in
        Format.pp_print_string fmt
          (esc (String.trim (String.sub str i (j - i))));
        if j < len - 1 then
          (Format.pp_force_newline fmt ();
           aux (j+1))
    in
    aux 0

  type coloriser =
    { f: 'a. kind ->
        ('a, Format.formatter, unit) format -> Format.formatter
        -> 'a }

  let color =
    let f kind fstr fmt =
      let colorcode = match kind with
        | OpenType
        | Type -> "\027[36m"
        | Value -> "\027[1m"
        | Exception -> "\027[33m"
        | Field _ | Variant _ -> "\027[34m"
        | Method _ -> "\027[1m"
        | Module | ModuleType -> "\027[31m"
        | Class | ClassType -> "\027[35m"
        | Keyword -> "\027[32m"
      in
      Format.pp_print_as fmt 0 colorcode;
      Format.kfprintf (fun fmt -> Format.pp_print_as fmt 0 "\027[m") fmt fstr
    in { f }

  let no_color =
    let f _ fstr fmt = Format.fprintf fmt fstr in
    { f }

  let name ?(colorise = no_color) fmt id =
    colorise.f id.kind "%s" fmt id.name

  let path ?(short = false) ?(colorise = no_color) fmt id =
    List.iter
      (Format.fprintf fmt "%a." (colorise.f Module "%s"))
      (if short then id.path else id.orig_path);
    name ~colorise fmt id

  let kind ?(colorise = no_color) fmt id =
    match id.kind with
    | OpenType -> Format.pp_print_string fmt "opentype"
    | Type -> Format.pp_print_string fmt "type"
    | Value -> Format.pp_print_string fmt "val"
    | Exception -> Format.pp_print_string fmt "exception"
    | Field parentty ->
        Format.fprintf fmt "field(%a)"
          (colorise.f parentty.kind "%s") parentty.name
    | Variant parentty ->
        Format.fprintf fmt "constr(%a)"
          (colorise.f parentty.kind "%s") parentty.name
    | Method parentclass ->
        Format.fprintf fmt "method(%a)"
          (colorise.f parentclass.kind "%s") parentclass.name
    | Module -> Format.pp_print_string fmt "module"
    | ModuleType -> Format.pp_print_string fmt "modtype"
    | Class -> Format.pp_print_string fmt "class"
    | ClassType -> Format.pp_print_string fmt "classtype"
    | Keyword -> Format.pp_print_string fmt "keyword"

  let rec tydecl fmt =
    let open Outcometree in
    function
    | Otyp_abstract -> Format.fprintf fmt "<abstract>"
    | Otyp_manifest (ty,_) -> tydecl fmt ty
    | Otyp_record fields ->
        let print_field fmt (name, mut, arg) =
          Format.fprintf fmt "@[<2>%s%s :@ @[%a@]@];"
            (if mut then "mutable " else "") name
            !Oprint.out_type arg
        in
        Format.fprintf fmt "@[<hv 2>{%a}@]"
          (list
             ~left:(fun fmt -> Format.pp_print_space fmt ())
             ~right:(fun fmt -> Format.pp_print_break fmt 1 (-2))
             print_field Format.pp_print_space)
          fields
    | Otyp_sum [] ->
        Format.pp_print_char fmt '-'
    | Otyp_sum constrs ->
        let print_variant fmt (name, tyl, ret_type_opt) =
          match ret_type_opt with
          | None ->
              if tyl = [] then Format.pp_print_string fmt name
              else
                Format.fprintf fmt "@[<2>%s of@ @[%a@]@]"
                  name
                  (list !Oprint.out_type
                     (fun fmt () -> Format.fprintf fmt " *@ "))
                  tyl
          | Some ret_type ->
              if tyl = [] then
                Format.fprintf fmt "@[<2>%s :@ @[%a@]@]" name
                  !Oprint.out_type ret_type
              else
                Format.fprintf fmt "@[<2>%s :@ @[%a -> @[%a@]@]@]"
                  name
                  (list !Oprint.out_type
                     (fun fmt () -> Format.fprintf fmt " *@ "))
                  tyl
                  !Oprint.out_type ret_type
        in
        list print_variant
          ~left:(fun fmt ->
              Format.pp_print_if_newline fmt (); Format.fprintf fmt "| ")
          (fun fmt () -> Format.fprintf fmt "@ | ")
          fmt constrs
    | ty ->
        !Oprint.out_type fmt ty

  let out_ty fmt ty =
    let open Outcometree in
    match ty with
    | Osig_class (_,_,_,ctyp,_)
    | Osig_class_type (_,_,_,ctyp,_) ->
        !Oprint.out_class_type fmt ctyp
    | Osig_typext ({ oext_args = [] }, _) ->
        Format.pp_print_char fmt '-'
    | Osig_typext ({ oext_args }, _) ->
        list ~paren:true
          !Oprint.out_type
          (fun fmt () ->
            Format.pp_print_char fmt ','; Format.pp_print_space fmt ())
          fmt
          oext_args
    | Osig_modtype (_,mtyp)
    | Osig_module (_,mtyp,_) ->
        !Oprint.out_module_type fmt mtyp
#if OCAML_VERSION >= "4.03"
    | Osig_type ({ otype_type },_) ->
        tydecl fmt otype_type
    | Osig_value {oval_type} ->
        !Oprint.out_type fmt oval_type
    | Osig_ellipsis ->
        Format.fprintf fmt "..."
#elif OCAML_VERSION >= "4.02"
    | Osig_type ({ otype_type },_) ->
        tydecl fmt otype_type
    | Osig_value (_,ty,_) ->
        !Oprint.out_type fmt ty
#else
    | Osig_type ((_,_,ty,_,_),_) ->
        tydecl fmt ty
    | Osig_value (_,ty,_) ->
        !Oprint.out_type fmt ty
#endif

  let ty ?(colorise = no_color) fmt id =
    option_iter id.ty
      (colorise.f Type "@[<hv>%a@]" fmt out_ty)

  let parent_ty ?colorise ?short fmt id =
    option_iter (IndexMisc.parent_type id)
      (fun id ->
         Format.fprintf fmt "@[<hv>%a =@ %a@]"
           (path ?colorise ?short) id
           (ty ?colorise) id)

  let doc ?escaped ?colorise:(_ = no_color) fmt id =
    option_iter (Lazy.force id.doc)
      (Format.fprintf fmt "@[<h>%a@]" (lines ?escaped))

  let loc ?root ?(intf=false) ?colorise:(_ = no_color) fmt id =
    let loc =
      if intf then Lazy.force id.loc_sig
      else Lazy.force id.loc_impl
    in
    if loc = Location.none then
      Format.fprintf fmt "@[<h><no location information>@]"
    else
      let pos = loc.Location.loc_start in
      let fname = match root with
        | Some r when Filename.is_relative pos.Lexing.pos_fname ->
            Filename.concat r pos.Lexing.pos_fname
        | _ -> pos.Lexing.pos_fname
      in
      Format.fprintf fmt "@[<h>%s:%d:%d@]"
        fname pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

  let file ?colorise:(_ = no_color) fmt id =
    Format.fprintf fmt "@[<h>%s@]"
      (match id.file with Cmt f | Cmi f | Cmti f -> f)

  let info ?(colorise = no_color) fmt id =
    let breakif n fmt = function
      | None -> ()
      | Some _ -> Format.pp_print_break fmt 1 n
    in
    Format.fprintf fmt "@[<v 2>@[<hov 2>%a@ %a%a%a@]%a%a@]@."
      (path ?short:None ~colorise) id
      (kind ~colorise) id
      (breakif 0) id.ty
      (ty ~colorise) id
      (breakif 2) (Lazy.force id.doc)
      (doc ?escaped:None ~colorise) id

  let handle_format_char ?root chr ?colorise fmt id = match chr with
    | 'n' -> name ?colorise fmt id
    | 'q' -> path ~short:true ?colorise fmt id
    | 'p' -> path ?colorise fmt id
    | 'k' -> kind ?colorise fmt id
    | 't' -> ty   ?colorise fmt id
    | 'd' -> doc  ?colorise fmt id
    | 'D' -> doc  ~escaped:true ?colorise fmt id
    | 'l' -> loc  ?root ?colorise fmt id
    | 's' -> loc  ?root ~intf:true ?colorise fmt id
    | 'f' -> file ?colorise fmt id
    | 'i' -> info ?colorise fmt id
    | 'e' -> parent_ty ?colorise fmt id
    | '%' -> Format.fprintf fmt "%%"
    | c   -> Format.fprintf fmt "%%%c" c

  let format ?root ?(separate=false) format ?colorise fmt id =
    let len = String.length format in
    let rec aux addsub ffmt flush i =
      let j = try string_index_from format i "%\\" with Not_found -> len in
      if j > i then addsub i (j - i);
      if j >= len - 1 then addsub j (len - j)
      else
        let fmt = ffmt () in
        begin match format.[j], format.[j+1] with
          | '%', c -> handle_format_char ?root c ?colorise fmt id
          | '\\', 'n' -> Format.pp_print_newline fmt ()
          | '\\', 't' -> Format.pp_print_char fmt '\t'
          | '\\', 'r' -> Format.pp_print_char fmt '\r'
          | '\\', c -> Format.pp_print_char fmt c
          | _ -> assert false
        end;
        flush fmt;
        aux addsub ffmt flush (j + 2)
    in
    if not separate then
      let addsub i len = Format.pp_print_string fmt (String.sub format i len) in
      let ffmt () = fmt in
      let flush _ = () in
      aux addsub ffmt flush 0
    else
      let b = Buffer.create 200 in
      let addsub = Buffer.add_substring b format in
      let ffmt () = Format.formatter_of_buffer b in
      let flush fmt = Format.pp_print_flush fmt () in
      aux addsub ffmt flush 0;
      Format.pp_print_string fmt (Buffer.contents b)

end

module Print = struct

  let disable_split_lines () =
    Format.pp_set_margin Format.str_formatter 1_000_000

  let make (f: ?colorise: IndexFormat.coloriser -> 'a) ?(color=false) id =
    let colorise =
      if color then IndexFormat.color else IndexFormat.no_color
    in
    f ~colorise Format.str_formatter id;
    Format.flush_str_formatter ()

  let name = make IndexFormat.name

  let path ?short = make (IndexFormat.path ?short)

  let kind = make IndexFormat.kind

  let ty = make IndexFormat.ty

  let doc ?escaped = make (IndexFormat.doc ?escaped)

  let loc ?root ?intf = make (IndexFormat.loc ?root ?intf)

  let file = make IndexFormat.file

  let info = make IndexFormat.info

  let format ?root ?separate format =
    make (IndexFormat.format ?root ?separate format)

end

module Format = IndexFormat
