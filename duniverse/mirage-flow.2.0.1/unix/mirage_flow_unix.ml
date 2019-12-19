(*
 * Copyright (c) 2015-present Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

let src = Logs.Src.create "mirage-flow-unix"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (F: Mirage_flow.S) = struct

  let reader t =
    let frag = ref (Cstruct.create 0) in
    let rec aux buf ofs len =
      if len = 0
      then Lwt.return 0
      else
        let available = Cstruct.len !frag in
        if available = 0 then begin
          F.read t >>= function
          | Ok (`Data b) ->
            frag := b;
            aux buf ofs len
          | Ok `Eof -> Lwt.return 0
          | Error e ->
            Lwt.fail_with @@ Fmt.strf "Lwt_io_flow.reader: %a" F.pp_error e
        end else begin
          let n = min available len in
          Cstruct.blit !frag 0 (Cstruct.of_bigarray buf) ofs n;
          frag := Cstruct.shift !frag n;
          Lwt.return n
        end in
    aux

  let writer t buf ofs len =
    let b = Cstruct.sub (Cstruct.of_bigarray buf) ofs len in
    F.write t b >>= function
    | Ok ()          -> Lwt.return len
    | Error `Closed  -> Lwt.return 0
    | Error e        ->
      Lwt.fail_with @@ Fmt.strf "Lwt_io_flow.writer: %a" F.pp_write_error e

  let ic ?(buffer_size=1024) ?(close=true) t =
    let close () = if close then F.close t else Lwt.return_unit in
    let buffer = Lwt_bytes.create buffer_size in
    Lwt_io.make ~buffer ~mode:Lwt_io.input ~close (reader t)

  let oc ?(buffer_size=1024) ?(close=false) t =
    let close () = if close then F.close t else Lwt.return_unit in
    let buffer = Lwt_bytes.create buffer_size in
    Lwt_io.make ~buffer ~mode:Lwt_io.output ~close (writer t)

end

module Fd = struct

  type error = [`Msg of string]
  type write_error = [ Mirage_flow.write_error | error ]

  let pp_error ppf (`Msg s) = Fmt.string ppf s

  let pp_write_error ppf = function
    | #Mirage_flow.write_error as e -> Mirage_flow.pp_write_error ppf e
    | #error as e                   -> pp_error ppf e

  type flow = Lwt_unix.file_descr

  let err e =  Lwt.return (Error (`Msg (Printexc.to_string e)))

  let failf fmt = Fmt.kstrf Lwt.fail_with fmt

  let pp_fd ppf (t:Lwt_unix.file_descr) =
    Fmt.int ppf (Obj.magic (Lwt_unix.unix_file_descr t): int)

  let rec really_write fd buf off len =
    match len with
    | 0   -> Lwt.return_unit
    | len ->
       Log.debug (fun l -> l "really_write %a off=%d len=%d" pp_fd fd off len);
       Lwt_unix.write fd buf off len >>= fun n ->
       if n = 0 then Lwt.fail_with "write 0"
       else really_write fd buf (off+n) (len-n)

  let write_all fd buf = really_write fd buf 0 (Bytes.length buf)

  let read_all fd =
    Log.debug (fun l -> l "read_all %a" pp_fd fd);
    let len = 16 * 1024 in
    let buf = Bytes.create len in
    let rec loop acc =
      Lwt_unix.read fd buf 0 len >>= fun n ->
      if n = 0 then failf "read %a: 0" pp_fd fd
      else
        let acc = Bytes.sub buf 0 n :: acc in
        if n <= len then Lwt.return (List.rev acc)
        else loop acc
    in
    loop [] >|= fun bufs ->
    Bytes.concat (Bytes.create 0) bufs

  let read t =
    Lwt.catch (fun () ->
        read_all t >|= fun buf -> Ok (`Data (Cstruct.of_bytes buf))
      ) (function Failure _ -> Lwt.return (Ok `Eof) | e -> err e)

  let write t b =
    Lwt.catch (fun () ->
        write_all t (Cstruct.to_bytes b) >|= fun () -> Ok ()
      ) (fun e  -> err e)

  let close t = Lwt_unix.close t

  let writev t bs =
    Lwt.catch (fun () ->
        Lwt_list.iter_s (fun b -> write_all t (Cstruct.to_bytes b)) bs
        >|= fun () -> Ok ()
      ) (fun e -> err e)

end
