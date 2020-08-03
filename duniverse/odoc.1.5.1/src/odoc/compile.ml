open Or_error

(*
 * Copyright (c) 2014 Leo White <leo@lpw25.net>
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



let resolve_and_substitute ~env ~output ~warn_error input_file read_file =
  Odoc_model.Error.set_warn_error warn_error;
  let filename = Fs.File.to_string input_file in
  match read_file ~filename:filename with
  | Error e -> Error (`Msg (Odoc_model.Error.to_string e))
  | Ok unit ->
    let unit = Odoc_xref.Lookup.lookup unit in
    if not unit.Odoc_model.Lang.Compilation_unit.interface then (
      Printf.eprintf "WARNING: not processing the \"interface\" file.%s\n%!"
        (if not (Filename.check_suffix filename "cmt") then "" (* ? *)
         else
          Printf.sprintf
            " Using %S while you should use the .cmti file" filename)
    );
    let resolve_env = Env.build env (`Unit unit) in
    Odoc_xref.resolve (Env.resolver resolve_env) unit >>= fun resolved ->
    (* [expand unit] fetches [unit] from [env] to get the expansion of local, previously
       defined, elements. We'd rather it got back the resolved bit so we rebuild an
       environment with the resolved unit.
       Note that this is bad and once rewritten expand should not fetch the unit it is
       working on. *)
    let expand_env = Env.build env (`Unit resolved) in
    Odoc_xref.expand (Env.expander expand_env) resolved >>= fun expanded ->
    Compilation_unit.save output expanded;
    Ok ()

let root_of_compilation_unit ~package ~hidden ~module_name ~digest =
  let file_representation : Odoc_model.Root.Odoc_file.t =
    Odoc_model.Root.Odoc_file.create_unit ~force_hidden:hidden module_name in
  {Odoc_model.Root.package; file = file_representation; digest}

let cmti ~env ~package ~hidden ~output ~warn_error input =
  let make_root = root_of_compilation_unit ~package ~hidden in
  let read_file = Odoc_loader.read_cmti ~make_root in
  resolve_and_substitute ~env ~output ~warn_error input read_file

let cmt ~env ~package ~hidden ~output ~warn_error input =
  let make_root = root_of_compilation_unit ~package ~hidden in
  let read_file = Odoc_loader.read_cmt ~make_root in
  resolve_and_substitute ~env ~output ~warn_error input read_file

let cmi ~env ~package ~hidden ~output ~warn_error input =
  let make_root = root_of_compilation_unit ~package ~hidden in
  let read_file = Odoc_loader.read_cmi ~make_root in
  resolve_and_substitute ~env ~output ~warn_error input read_file

(* TODO: move most of this to doc-ock. *)
let mld ~env ~package ~output ~warn_error input =
  Odoc_model.Error.set_warn_error warn_error;
  let root_name =
    let page_dash_root =
      Filename.chop_extension (Fs.File.(to_string @@ basename output))
    in
    String.sub page_dash_root (String.length "page-")
      (String.length page_dash_root - String.length "page-")
  in
  let digest = Digest.file (Fs.File.to_string input) in
  let root =
    let file = Odoc_model.Root.Odoc_file.create_page root_name in
    {Odoc_model.Root.package; file; digest}
  in
  let name = `Page (root, Odoc_model.Names.PageName.of_string root_name) in
  let location =
    let pos =
      Lexing.{
        pos_fname = Fs.File.to_string input;
        pos_lnum = 0;
        pos_cnum = 0;
        pos_bol = 0
      }
    in
    Location.{ loc_start = pos; loc_end = pos; loc_ghost = true }
  in
  let resolve content =
    (* This is a mess. *)
    let page = Odoc_model.Lang.Page.{ name; content; digest } in
    let page = Odoc_xref.Lookup.lookup_page page in
    let env = Env.build env (`Page page) in
    Odoc_xref.resolve_page (Env.resolver env) page >>= fun resolved ->
    Page.save output resolved;
    Ok ()
  in
  Fs.File.read input >>= fun str ->
  match Odoc_loader.read_string name location str with
  | Error e -> Error (`Msg (Odoc_model.Error.to_string e))
  | Ok `Stop -> resolve [] (* TODO: Error? *)
  | Ok (`Docs content) -> resolve content
