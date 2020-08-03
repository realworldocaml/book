(*{{{ Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
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
 *
  }}}*)

let _debug_active = ref false
let debug_active () = !_debug_active

let default_reporter () =
  (* Note: we want the logging operation to not block the other operation.
   * Hence, the reporter creates Lwt promises. *)
  let fmtr, fmtr_flush =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like:Fmt.stdout b
    , fun () ->
      let m = Buffer.contents b in Buffer.reset b;
      m ) in
  let report _src _level ~over k msgf =
    let k _ =
      let write () = Lwt_io.write Lwt_io.stderr (fmtr_flush ()) in
      let unblock () = over (); Lwt.return_unit in
      Lwt.ignore_result @@ Lwt.catch
        (fun () -> (Lwt.finalize write unblock : unit Lwt.t))
        (fun e  ->
          Logs.warn (fun f ->
            f "Flushing stderr failed: %s" (Printexc.to_string e));
          Lwt.return_unit
        );
      k ()
    in
    msgf @@ fun ?header:_ ?tags:_ fmt ->
    Format.kfprintf k fmtr ("@[" ^^ fmt ^^ "@]@.")
  in
  { Logs.report = report }

let set_log = lazy (
  (* If no reporter has been set by the application, set default one
     that prints to stderr *)
  if (Logs.reporter ()) == Logs.nop_reporter
  then
    Logs.set_level @@ Some Logs.Debug;
    Logs.set_reporter (default_reporter ());
)

let activate_debug () =
  Lazy.force set_log;
  _debug_active := true;
  Logs.debug (fun f -> f "Cohttp debugging output is active")

let () =
  try (
   match Sys.getenv "COHTTP_DEBUG" with
   | "false" | "0" -> ()
   | _ -> activate_debug ()
  ) with Not_found -> ()
