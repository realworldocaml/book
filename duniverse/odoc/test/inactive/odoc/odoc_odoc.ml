(*
 * Copyright (c) 2016 Daniel C. Bünzli
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

(* odoc odoc's lib. Invoke from the root of the distribution. *)

open Topkg (* we just want the nicer OS interface *)

let build_dir = "_build"
let lib_src = "lib"
let pkg = "odoc"
let dst_dir = Fpath.(build_dir // pkg)

let mod_file m ext = String.uncapitalize_ascii m ^ ext
let sorted_lib_modules () =
  OS.File.read Fpath.(lib_src // "odoc.mllib")
  >>= fun mllib -> Ok (String.cuts ~empty:false ~sep:'\n' mllib)
  >>= fun lines -> Ok (List.filter (fun s -> s.[0] <> '#') lines)

let rec iter_mods cmd = function
| m :: ms -> cmd m >>= fun () -> iter_mods cmd ms
| [] -> Ok ()

let odoc = Cmd.(v @@ p Fpath.(build_dir // "bin/main.native"))

let odoc_compile_mod m =
  let cmti = Fpath.(build_dir // lib_src // mod_file m ".cmti" ) in
  let odocf = Fpath.(dst_dir // mod_file m ".odoc") in
  Log.info (fun msg -> msg "Compiling %s to odoc file" m);
  OS.Cmd.run @@
  Cmd.(odoc % "compile" % "-I" % dst_dir % "--pkg" % pkg % "-o" % odocf % cmti)

let odoc_html_mod m =
  let odocf = Fpath.(dst_dir // mod_file m ".odoc") in
  Log.info (fun msg -> msg "Writing HTML for %s" m);
  OS.Cmd.run @@
  Cmd.(odoc % "html" % "-I" % dst_dir % "-o" % build_dir % odocf)

let odoc_html_index mods =
  Log.info (fun msg -> msg "Writing package index.html (dst_dir %S)" dst_dir);
  let mod_list = String.concat " " mods in
  let page =
    Printf.sprintf
      "{%%html: \
       <nav>No up link</nav>\n\
       <header><h1 id=\"pkg\">Package odoc</h1></header>%%}\n\
       {!modules: %s}\n"
      mod_list
  in
  OS.File.tmp ()
  >>= fun tmp_file ->
  OS.File.write tmp_file page
  >>= fun () ->
  OS.Cmd.run Cmd.(odoc % "html" % "-I" % dst_dir % "-o" % build_dir
                  % "--index-for" % "odoc" % tmp_file)
  >>= fun () ->
  OS.File.delete tmp_file

let odoc_css () =
  Log.info (fun msg -> msg "Writing CSS");
  OS.Cmd.run Cmd.(odoc % "css" % "-o" % build_dir)

let odoc_odoc ~browse =
  begin
    OS.Cmd.run Cmd.(v "mkdir" % "-p" % dst_dir)
    >>= fun () -> sorted_lib_modules ()
    >>= fun mods -> iter_mods odoc_compile_mod mods
    >>= fun () -> iter_mods odoc_html_mod mods
    >>= fun () -> odoc_html_index mods
    >>= fun () -> odoc_css ()
    >>= fun () ->
    Log.app (fun m -> m "Generated odoc docs in %s" dst_dir);
    match browse with
    | true -> OS.Cmd.run Cmd.(v "browse" % "-p" % dst_dir)
    | false -> Ok ()
  end
  |> Log.on_error_msg ~use:(fun _ -> ())

let main () =
  Topkg.Private.disable_main ();
  Topkg.Log.(set_level (Some Info));
  let browse = match Sys.argv with [|_; "-b" |] -> true | _ -> false in
  odoc_odoc ~browse

let () = main ()
