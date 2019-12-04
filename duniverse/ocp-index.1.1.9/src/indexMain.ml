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

(** This module contains the run-time for the command-line ocp-index tool *)


open Cmdliner

let common_opts = IndexOptions.common_opts ()

let default_cmd =
  let man = [
    `S "DESCRIPTION";
    `P "ocp-index is a simple and light-weight documentation extractor for \
        OCaml, for command-line or integrated use (e.g. for completion). \
        It gathers information from .cmi (like ocamlbrowser) and \
        .cmt/cmti files, including structure, location, type, and ocamldoc \
        comments when available."
  ]
  in
  let doc = "Explore the interfaces of installed OCaml libraries." in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_opts)),
  Term.info "ocp-index" ~version:"1.1.5" ~doc ~man

let format_man =
  let formats = [
    "%n", "The name of the ident, e.g. \"map\"";
    "%q", "The qualified ident, e.g. \"List.map\", or possibly \"map\" if \
           the ident was accessed through an \"open\"";
    "%p", "The full path of the ident, e.g. \"List.map\"";
    "%k", "The kind of the ident, one of \"type\", \"val\", \"exception\", \
           \"field(<type>)\", \"constr(<type>)\", \"method(<class>)\", \
           \"module\", \"modtype\", \"class\", \"classtype\", \"keyword\"";
    "%t", "The type of the ident";
    "%d", "The ident documentation (ocamldoc comment, from a .cmt file)";
    "%D", "Same as %d, but as an escaped string";
    "%l", "The location of the declaration";
    "%s", "The location of the signature (as found in a .mli)";
    "%f", "The file where ocp-index found the definition (.cmi, .cmt)";
    "%i", "A short summary of some of this information";
    "%e", "The enclosing type definition, for field records, variants and \
           methods";
    "%%", "A single '%' character";
  ]
  in
  [ `S "FORMAT STRINGS";
    `P "Format strings are arbitrary strings that will be printed for every \
        match, with the usual '\\n', '\\t', '\\r' plus the following sequences \
        interpreted:" ] @
  List.map (fun (k, s) -> `I (Printf.sprintf "$(b,%s)" k, s)) formats

let man = [
  `S "COPYRIGHT";
  `P "Ocp-index is written by Louis Gesbert <louis.gesbert@ocamlpro.com>, \
      copyright OCamlPro 2013-2014, \
      distributed under the terms of the LGPL v3 with linking exception. \
      Full source available at $(i,https://github.com/OCamlPro/ocp-index)";
  `S "BUGS";
  `P "Bugs are tracked at $(i,https://github.com/OCamlPro/ocp-index/issues).";
  `S "SEE ALSO";
  `P "ocp-grep, ocp-browser";
]

let complete_cmd =
  let man = [
    `S "DESCRIPTION";
    `P "Searches for OCaml identifiers matching a given prefix in $(b,.cmi), \
        $(b,.cmt) and $(b,.cmti) files. Unless specified, the search path \
        includes the OCaml libraries prefix and the current project build \
        directory."
  ] @ format_man @ man
  in
  let doc = "Complete identifiers starting with prefix $(docv)." in
  let t =
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"STRING")
  in
  let sexpr: bool Term.t =
    let doc = "Output the result as a s-expression." in
    Arg.(value & flag & info ["sexp"] ~doc)
  in
  let format: string option Term.t =
    let doc = "Specify the output format. See section FORMAT STRINGS." in
    Arg.(value & opt (some string) None &
         info ["f";"format"] ~doc ~docv:"FORMAT")
  in
  let separate: bool Term.t =
    let doc = "When using a format string, format each escape independently \
               rather than as a whole. Useful when generating (elisp) code." in
    Arg.(value & flag & info ["separate"] ~doc)
  in
  let print_compl opts sexpr format separate query =
    let fmt = Format.std_formatter in
    let results =
      LibIndex.complete
        opts.IndexOptions.lib_info
        ~filter:(IndexOptions.filter opts)
        query
    in
    if sexpr then (
      if format <> None then
        raise (Invalid_argument "options --format and --sexp are incompatible");
      Format.fprintf fmt "(@[<v 2>@ ";
      List.iter (fun info ->
          let (!) f x = f ?colorise:None x in
          Format.fprintf fmt "(@[<v 2>\"%a\""
            !(LibIndex.Format.path ~short:true) info;
          Format.fprintf fmt "@ (:path . \"%a\")"
            !(LibIndex.Format.path ~short:false) info;
          Format.fprintf fmt "@ (:type . %S)" (LibIndex.Print.ty info);
          Format.fprintf fmt "@ (:kind . \"%a\")" !LibIndex.Format.kind info;
          (if Lazy.force info.LibIndex.doc <> None
           then Format.fprintf fmt "@ (:doc . %S)" (LibIndex.Print.doc info));
          Format.fprintf fmt "@]@ )@ "
        )
        results;
      Format.fprintf fmt "@]@ )@."
    ) else
      let colorise =
        if opts.IndexOptions.color then LibIndex.Format.color
        else LibIndex.Format.no_color
      in
      let print fmt i = match format with
        | None -> LibIndex.Format.info ~colorise fmt i
        | Some fstring ->
            LibIndex.Format.format ?root:opts.IndexOptions.project_root
              ~separate
              fstring ~colorise fmt i;
            Format.pp_print_newline fmt ()
      in
      List.iter (print fmt) results;
  in
  let doc = "Output completions for a given prefix." in
  Term.(pure print_compl $ common_opts $ sexpr $ format $ separate $ t),
  Term.info "complete" ~doc ~man

let type_cmd =
  let man = [
    `S "DESCRIPTION";
    `P "Prints the type(s) of an OCaml ident as found in $(b,.cmi), \
        $(b,.cmt) and $(b,.cmti) files. Unless specified, the search path \
        includes the OCaml libraries prefix and the current project build \
        directory. This is equivalent to `ocp-index print $(docv) \"%t\"'"
  ] @ man
  in
  let doc = "Print the type of OCaml identifier $(docv)." in
  let t =
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"STRING")
  in
  let print_ty opts query =
    try
      let id = LibIndex.get opts.IndexOptions.lib_info query in
      print_endline (LibIndex.Print.ty id)
    with Not_found -> exit 2
  in
  let doc = "Print the type of an identifier." in
  Term.(pure print_ty $ common_opts $ t),
  Term.info "type" ~doc ~man

let locate_cmd =
  let man = [
    `S "DESCRIPTION";
    `P "Prints the source location(s) of an OCaml ident as found in $(b,.cmt), \
        or $(b,.cmti) files. Unless specified, the search path \
        includes the OCaml libraries prefix and the current project build \
        directory."
  ] @ man
  in
  let doc = "Get the location of definition of $(docv)." in
  let t =
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"STRING")
  in
  let interface: bool Term.t =
    let doc =
      "Lookup the interface instead of the implementation, if it exists" in
    Arg.(value & flag & info ["i";"interface"] ~doc)
  in
  let print_loc opts intf query =
    let ids0 = LibIndex.get_all opts.IndexOptions.lib_info query in
    let filter_ids intf =
      List.filter (fun id ->
          intf && Lazy.force id.LibIndex.loc_sig <> Location.none
          || not intf && Lazy.force id.LibIndex.loc_impl <> Location.none)
        ids0
    in
    let ids, intf = match filter_ids intf with
      | [] ->
          filter_ids (not intf), not intf
      | ids -> ids, intf
    in
    let loc_as_string id =
      LibIndex.Print.loc ?root:opts.IndexOptions.project_root ~intf id
    in
    match ids with
    | [] -> exit 2
    | _ -> List.iter (fun id -> print_endline (loc_as_string id)) ids
  in
  let doc = "Get the location where an identifier was defined." in
  Term.(pure print_loc $ common_opts $ interface $ t),
  Term.info "locate" ~doc ~man

let print_cmd =
  let man = [
    `S "DESCRIPTION";
    `P "Searches for an OCaml identifier in $(b,.cmi), \
        $(b,.cmt) and $(b,.cmti) files. Unless specified, the search path \
        includes the OCaml libraries prefix and the current project build \
        directory."
  ] @ format_man @ man
  in
  let query =
    let doc = "The identifier to lookup" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"STRING")
  in
  let format =
    let doc = "Specify the output format. See section FORMAT STRINGS." in
    Arg.(value & pos 1 string "%i" & info [] ~doc ~docv:"FORMAT")
  in
  let separate: bool Term.t =
    let doc = "Format each escape in the format string independently \
               rather than as a whole. Useful when generating (elisp) code." in
    Arg.(value & flag & info ["separate"] ~doc)
  in
  let print opts query format separate =
    let ids = LibIndex.get_all opts.IndexOptions.lib_info query in
    let root = opts.IndexOptions.project_root in
    if ids = [] then exit 2;
    List.iter
      (fun id ->
         let s = LibIndex.Print.format ?root ~separate format id in
         if s <> "" then print_endline s)
      ids
  in
  let doc = "Print information about an identifier with a custom format." in
  Term.(pure print $ common_opts $ query $ format $ separate),
  Term.info "print" ~doc ~man

let () =
  match
    try
      Term.eval_choice ~catch:false
        default_cmd [complete_cmd; type_cmd; locate_cmd; print_cmd]
    with
    | LibIndex.Bad_format f ->
        Printf.eprintf
          "[ERROR] %S can't be read.\n\
           It's likely that it belongs to a version of OCaml different from \
           the one ocp-index was compiled against.\n"
          f;
        exit 4
    | e ->
        IndexMisc.debug "%s\n" (Printexc.to_string e);
        if Printexc.backtrace_status () then
          IndexMisc.debug "%s\n" (Printexc.get_backtrace ());
        `Error `Exn
  with
  | `Error _ -> exit 1
  | _ -> exit 0
