(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

type 'a log = ('a, unit Lwt.t) Logs.msgf -> unit Lwt.t

let kmsg k ?(src = Logs.default) level msgf = match Logs.Src.level src with
| None -> k ()
| Some level' when level > level' ->
    (if level = Logs.Error then Logs.incr_err_count () else
     if level = Logs.Warning then Logs.incr_warn_count () else ());
    (k ())
| Some _ ->
    (if level = Logs.Error then Logs.incr_err_count () else
     if level = Logs.Warning then Logs.incr_warn_count () else ());
    let (ret, unblock) = Lwt.wait () in
    let k () = Lwt.bind ret k in
    let over () = Lwt.wakeup unblock () in
    Logs.report src level ~over k msgf

let kunit _ = Lwt.return ()
let msg ?src level msgf = kmsg kunit ?src level msgf
let app ?src msgf = kmsg kunit ?src Logs.App msgf
let err ?src msgf = kmsg kunit ?src Logs.Error msgf
let warn ?src msgf = kmsg kunit ?src Logs.Warning msgf
let info ?src msgf = kmsg kunit ?src Logs.Info msgf
let debug ?src msgf = kmsg kunit ?src Logs.Debug msgf

let on_error ?src ?(level = Logs.Error) ?header ?tags ~pp ~use t =
  Lwt.bind t @@ function
  | Ok v -> Lwt.return v
  | Error e ->
      kmsg (fun () -> use e) ?src level @@ fun m ->
      m ?header ?tags "@[%a@]" pp e

let on_error_msg ?src ?(level = Logs.Error) ?header ?tags ~use t =
  Lwt.bind t @@ function
  | Ok v -> Lwt.return v
  | Error (`Msg e) ->
      kmsg use ?src level @@ fun m ->
      m ?header ?tags "@[%a@]" Logs.pp_print_text e

(* Source specific functions *)

module type LOG = sig
  val msg : Logs.level -> 'a log
  val app : 'a log
  val err : 'a log
  val warn : 'a log
  val info : 'a log
  val debug : 'a log
  val kmsg : ?over:(unit -> unit) -> (unit -> 'b Lwt.t) ->
    Logs.level -> ('a, 'b Lwt.t) Logs.msgf -> 'b Lwt.t

  val on_error : ?level:Logs.level -> ?header:string -> ?tags:Logs.Tag.set ->
    pp:(Format.formatter -> 'b -> unit) -> use:('b -> 'a Lwt.t) ->
    ('a, 'b) result Lwt.t -> 'a Lwt.t

  val on_error_msg : ?level:Logs.level -> ?header:string ->
    ?tags:Logs.Tag.set -> use:(unit -> 'a Lwt.t) ->
    ('a, [`Msg of string]) result Lwt.t -> 'a Lwt.t
end

let src_log src =
  let module Log = struct
    let msg level msgf = msg ~src level msgf
    let kmsg ?over k level msgf = kmsg k ~src level msgf
    let app msgf = msg Logs.App msgf
    let err msgf = msg Logs.Error msgf
    let warn msgf = msg Logs.Warning msgf
    let info msgf = msg Logs.Info msgf
    let debug msgf = msg Logs.Debug msgf
    let on_error ?level ?header ?tags ~pp ~use =
      on_error ~src ?level ?header ?tags ~pp ~use

    let on_error_msg ?level ?header ?tags ~use =
      on_error_msg ~src ?level ?header ?tags ~use
  end
  in
  (module Log : LOG)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers

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
