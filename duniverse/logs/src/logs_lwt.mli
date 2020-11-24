(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Lwt} logging.

    The log functions of this module return [Lwt] threads that proceed
    only when the log operation is over, as defined by the current
    {!Logs.reporter}.

    See a {{!report_ex}cooperative reporter example}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Log functions} *)

type 'a log = ('a, unit Lwt.t) Logs.msgf -> unit Lwt.t
(** The type for Lwt log functions. The returned thread only proceeds
    once the log operation is over. See {!Logs.log}. *)

val msg : ?src:Logs.src -> Logs.level -> 'a log
(** See {!Logs.msg}. *)

val app : ?src:Logs.src -> 'a log
(** See {!Logs.app}. *)

val err : ?src:Logs.src -> 'a log
(** See {!Logs.err}. *)

val warn : ?src:Logs.src -> 'a log
(** See {!Logs.warn}. *)

val info : ?src:Logs.src -> 'a log
(** See {!Logs.info}. *)

val debug : ?src:Logs.src -> 'a log
(** See {!Logs.debug}. *)

val kmsg : (unit -> 'b Lwt.t) -> ?src:Logs.src ->
  Logs.level -> ('a, 'b Lwt.t) Logs.msgf -> 'b Lwt.t
(** See {!Logs.kmsg}. *)

(** {2 Logging {!result} value [Error]s} *)

val on_error : ?src:Logs.src -> ?level:Logs.level -> ?header:string ->
  ?tags:Logs.Tag.set -> pp:(Format.formatter -> 'b -> unit) ->
  use:('b -> 'a Lwt.t) -> ('a, 'b) result Lwt.t -> 'a Lwt.t
(** See {!Logs.on_error}. *)

val on_error_msg : ?src:Logs.src -> ?level:Logs.level -> ?header:string ->
  ?tags:Logs.Tag.set -> use:(unit -> 'a Lwt.t) ->
  ('a, [`Msg of string]) result Lwt.t -> 'a Lwt.t
(** See {!Logs.on_error_msg}. *)

(** {1 Source specific log functions} *)

module type LOG = sig
  val msg : Logs.level -> 'a log
  (** See {!Logs.msg}. *)

  val app : 'a log
  (** See {!Logs.app}. *)

  val err : 'a log
  (** See {!Logs.err}. *)

  val warn : 'a log
  (** See {!Logs.warn}. *)

  val info : 'a log
  (** See {!Logs.info}. *)

  val debug : 'a log
  (** See {!Logs.debug}. *)

  val kmsg : ?over:(unit -> unit) -> (unit -> 'b Lwt.t) ->
    Logs.level -> ('a, 'b Lwt.t) Logs.msgf -> 'b Lwt.t
  (** See {!Logs.kmsg}. *)

  (** {2 Logging {!result} value [Error]s} *)

  val on_error : ?level:Logs.level -> ?header:string ->
    ?tags:Logs.Tag.set -> pp:(Format.formatter -> 'b -> unit) ->
    use:('b -> 'a Lwt.t) -> ('a, 'b) result Lwt.t -> 'a Lwt.t
  (** See {!Logs.on_error}. *)

  val on_error_msg : ?level:Logs.level -> ?header:string ->
    ?tags:Logs.Tag.set -> use:(unit -> 'a Lwt.t) -> ('a, [`Msg of
    string]) result Lwt.t -> 'a Lwt.t
  (** See {!Logs.on_error_msg}. *)
end

val src_log : Logs.src -> (module LOG)
(** [src_log src] is a {{!LOG}set of logging functions} for [src]. *)

(** {1:report_ex Cooperative reporter example}

    The following reporter will play nice with [Lwt]'s runtime, it
    will behave synchronously for the log functions of this module and
    asynchronously for those of the {!Logs} module (see {!Logs.sync}).

    It reuses {!Logs_fmt.reporter} and will produce colorful output if
    the standard formatters are setup to do so. For example it can be
    used instead of {!Logs_fmt.reporter} in the {{!Logs_cli.ex}full
    setup example}.
{[
let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
      | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
      | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }
]}
*)

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
