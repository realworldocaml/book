(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012,2013 OCamlPro                                          *)
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

module Args = IndentArgs

let indent_channel ic args config out perm =
  let oc, need_close = match out with
    | None | Some "-" -> stdout, false
    | Some file -> open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] perm file, true
  in
  let output = {
    IndentPrinter.
    debug = args.Args.debug;
    config = config;
    in_lines = args.Args.in_lines;
    indent_empty = args.Args.indent_empty;
    adaptive = true;
    kind = args.Args.indent_printer oc;
  }
  in
  let stream = Nstream.of_channel ic in
  IndentPrinter.proceed output stream IndentBlock.empty ();
  flush oc;
  if need_close then close_out oc

let config_syntaxes syntaxes =
  Approx_lexer.disable_extensions ();
  List.iter (fun stx ->
      try
        Approx_lexer.enable_extension stx
      with IndentExtend.Syntax_not_found name ->
        Format.eprintf "Warning: unknown syntax extension %S@." name)
    syntaxes

let indent_file args = function
  | Args.InChannel ic ->
      let config, syntaxes, dlink = IndentConfig.local_default () in
      IndentLoader.load ~debug:args.Args.debug (dlink @ args.Args.dynlink);
      config_syntaxes (syntaxes @ args.Args.syntax_exts);
      let config =
        List.fold_left
          IndentConfig.update_from_string
          config
          args.Args.indent_config
      in
      indent_channel ic args config args.Args.file_out 0o644 (* won't be used *)
  | Args.File path ->
      let config, syntaxes, dlink =
        IndentConfig.local_default ~path:(Filename.dirname path) ()
      in
      IndentLoader.load ~debug:args.Args.debug (dlink @ args.Args.dynlink);
      config_syntaxes (syntaxes @ args.Args.syntax_exts);
      let config =
        List.fold_left
          IndentConfig.update_from_string
          config
          args.Args.indent_config
      in
      let out, perm, need_move =
        if args.Args.inplace then
          let tmp_file = path ^ ".ocp-indent-tmp" in
          let rec get_true_file path =
            let open Unix in
            match lstat path with
            | { st_kind = S_REG ; st_perm } -> Some tmp_file, st_perm, Some path
            | { st_kind = S_LNK ; } -> get_true_file @@ readlink path
            | { st_kind = _ ; } -> failwith "invalid file type"
          in get_true_file path
        else
          args.Args.file_out, 0o644, None
      in
      let ic = open_in path in
      try
        indent_channel ic args config out perm;
        match out, need_move with
        | Some src, Some dst -> Sys.rename src dst
        | _, _ -> ()
      with e ->
          close_in ic; raise e

let main =
  Cmdliner.Term.(
    pure (fun (args,files) -> List.iter (indent_file args) files)
    $ Args.options
  ),
  Args.info

let _ =
  match Cmdliner.Term.eval main with
  | `Error _ -> exit 1
  | _ -> exit 0
