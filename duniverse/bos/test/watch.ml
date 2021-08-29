(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

(* Watch a directory for changes. First run will create a database
   watchdb in the directory with modification times. Subsquent runs
   will check files against that database. *)

module Db = struct
  let db_file = Fpath.v "watchdb"
  let exists () = OS.File.exists db_file
  let scan () =             (* returns list of (path, modification time) *)
    let add p acc =
      (OS.Path.stat p >>= fun stats ->
       if stats.Unix.st_kind <> Unix.S_REG then Ok acc else
       Ok ((p, stats.Unix.st_mtime) :: acc))
      |> Logs.on_error_msg ~use:(fun _ -> acc)
    in
    Logs.app (fun m -> m "Scanning files");
    OS.Dir.current () >>= fun dir ->
    OS.Dir.fold_contents ~dotfiles:true ~elements:`Files add [] dir

  let dump oc db = Ok (Marshal.(to_channel oc db [No_sharing; Compat_32]))
  let slurp ic () = (Marshal.from_channel ic : float Fpath.Map.t)

  let create files =
    Logs.app (fun m -> m "Writing modification time database %a"
                 Fpath.pp db_file);
    let count = ref 0 in
    let add acc (f, time) = incr count; Fpath.Map.add f time acc in
    let db = List.fold_left add Fpath.Map.empty files in
    R.join @@ OS.File.with_oc db_file dump db >>= fun () -> Ok !count

  let check files =
    let count = ref 0 in
    let changes db (f, time) = match (incr count; Fpath.Map.find f db) with
    | None ->
        Logs.app (fun m -> m "New file: %a" Fpath.pp f)
    | Some stamp when stamp <> time ->
        Logs.app (fun m -> m "File changed: %a" Fpath.pp f)
    | _ -> ()
    in
    Logs.app (fun m -> m "Checking against %a" Fpath.pp db_file);
    OS.File.with_ic db_file slurp ()
    >>= fun db -> List.iter (changes db) files; Ok !count
end

let watch () =
  Db.scan ()
  >>= fun files -> Db.exists ()
  >>= fun exists -> if exists then Db.check files else Db.create files

let main () =
  let c = Mtime_clock.counter () in
  let count = watch () |> Logs.on_error_msg ~use:(fun _ -> 0) in
  Logs.app (fun m -> m "Watch completed for %d files in %a"
               count Mtime.Span.pp (Mtime_clock.count c))

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
