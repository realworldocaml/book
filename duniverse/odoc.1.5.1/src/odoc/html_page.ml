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

open StdLabels
open Or_error

let to_html_tree_page ?theme_uri ~syntax v =
  match syntax with
  | Odoc_html.Tree.Reason -> Odoc_html.Generator.Reason.page ?theme_uri v
  | Odoc_html.Tree.OCaml -> Odoc_html.Generator.ML.page ?theme_uri v

let to_html_tree_compilation_unit ?theme_uri ~syntax v =
  match syntax with
  | Odoc_html.Tree.Reason -> Odoc_html.Generator.Reason.compilation_unit ?theme_uri v
  | Odoc_html.Tree.OCaml -> Odoc_html.Generator.ML.compilation_unit ?theme_uri v

let from_odoc ~env ?(syntax=Odoc_html.Tree.OCaml) ?theme_uri ~output:root_dir input =
  Root.read input >>= fun root ->
  match root.file with
  | Page page_name ->
    Page.load input >>= fun page ->
    let resolve_env = Env.build env (`Page page) in
    Odoc_xref.resolve_page (Env.resolver resolve_env) page >>= fun odoctree ->
    let pkg_name = root.package in
    let pages = to_html_tree_page ?theme_uri ~syntax odoctree in
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir pkg_name in
    Fs.Directory.mkdir_p pkg_dir;
    Odoc_html.Tree.traverse pages ~f:(fun ~parents _pkg_name content ->
      assert (parents = []);
      let oc =
        let f = Fs.File.create ~directory:pkg_dir ~name:(page_name ^ ".html") in
        open_out (Fs.File.to_string f)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a@?" (Tyxml.Html.pp ()) content;
      close_out oc
    );
    Ok ()
  | Compilation_unit {hidden = _; _} ->
    (* If hidden, we should not generate HTML. See
         https://github.com/ocaml/odoc/issues/99. *)
    Compilation_unit.load input >>= fun unit ->
    let unit = Odoc_xref.Lookup.lookup unit in
    begin
      (* See comment in compile for explanation regarding the env duplication. *)
      let resolve_env = Env.build env (`Unit unit) in
      Odoc_xref.resolve (Env.resolver resolve_env) unit >>= fun resolved ->
      let expand_env = Env.build env (`Unit resolved) in
      Odoc_xref.expand (Env.expander expand_env) resolved >>= fun expanded ->
      Odoc_xref.Lookup.lookup expanded
      |> Odoc_xref.resolve (Env.resolver expand_env) (* Yes, again. *)
    end >>= fun odoctree ->
    let pkg_dir =
      Fs.Directory.reach_from ~dir:root_dir root.package
    in
    let pages = to_html_tree_compilation_unit ?theme_uri ~syntax odoctree in
    Odoc_html.Tree.traverse pages ~f:(fun ~parents name content ->
      let directory =
        let dir =
          List.fold_right ~f:(fun name dir -> Fs.Directory.reach_from ~dir name)
            parents ~init:pkg_dir
        in
        Fs.Directory.reach_from ~dir name
      in
      let oc =
        Fs.Directory.mkdir_p directory;
        let file = Fs.File.create ~directory ~name:"index.html" in
        open_out (Fs.File.to_string file)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a@?" (Tyxml.Html.pp ()) content;
      close_out oc
    );
    Ok ()

(* Used only for [--index-for] which is deprecated and available only for
   backward compatibility. It should be removed whenever. *)
let from_mld ~env ?(syntax=Odoc_html.Tree.OCaml) ~package ~output:root_dir ~warn_error input =
  Odoc_model.Error.set_warn_error warn_error;
  let root_name = "index" in
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
  let to_html content =
    (* This is a mess. *)
    let page = Odoc_model.Lang.Page.{ name; content; digest } in
    let page = Odoc_xref.Lookup.lookup_page page in
    let env = Env.build env (`Page page) in
    Odoc_xref.resolve_page (Env.resolver env) page >>= fun resolved ->
    let pages = to_html_tree_page ~syntax resolved in
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir root.package in
    Fs.Directory.mkdir_p pkg_dir;
    Odoc_html.Tree.traverse pages ~f:(fun ~parents _pkg_name content ->
      assert (parents = []);
      let oc =
        let f = Fs.File.create ~directory:pkg_dir ~name:"index.html" in
        open_out (Fs.File.to_string f)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a@?" (Tyxml.Html.pp ()) content;
      close_out oc
    );
    Ok ()
  in
  match Fs.File.read input with
  | Error _ as e -> e
  | Ok str ->
    match Odoc_loader.read_string name location str with
    | Error e -> Error (`Msg (Odoc_model.Error.to_string e))
    | Ok (`Docs content) -> to_html content
    | Ok `Stop -> to_html [] (* TODO: Error? *)
