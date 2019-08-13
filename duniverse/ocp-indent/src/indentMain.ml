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

let indent_channel ic args config out =
  let oc, need_close = match out with
    | None | Some "-" -> stdout, false
    | Some file -> open_out file, true
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
      indent_channel ic args config args.Args.file_out
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
      let out, need_move =
        if args.Args.inplace then
          let tmp_file = path ^ ".ocp-indent-tmp" in
          Some tmp_file, Some path
        else
          args.Args.file_out, None
      in
      let ic = open_in path in
      try
        indent_channel ic args config out;
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
