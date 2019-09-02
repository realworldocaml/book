(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Logging.

    [Logs] provides a basic logging infrastructure. {{!func}Logging}
    is performed on {{!srcs}sources} whose reporting
    {{!type:level}level} can be set independently. Log message
    report is decoupled from logging and handled by a
    {{!reporters}reporter}.

    See the {{!basics}basics}, a few {{!usage}usage conventions} to
    respect and a note on {{!sync}synchronous logging}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:levels Reporting levels} *)

(** The type for reporting levels. For level semantics see the
    {{!usage}usage conventions}.

    Log {{!srcs}sources} have an optional {{!Src.level}reporting level}. If
    the level is [Some l] then any message whose level is smaller or
    equal to [l] is reported. If the level is [None] no message is
    ever reported. *)
type level = App | Error | Warning | Info | Debug

val level : unit -> level option
(** [level ()] is the reporting level given to {{!Src.create}new sources}. *)

val set_level : ?all:bool -> level option -> unit
(** [set_level ?all l] sets the reporting level given to
    {{!Src.create}new sources}. If [all] is [true] (default), also
    sets the reporting level of all {{!Src.list}existing sources}. Use
    {!Src.set_level} to only affect a specific source. Only applications
    should use this function directly see {{!usage}usage conventions}. *)

val pp_level : Format.formatter -> level -> unit
(** [pp_level ppf l] prints an unspecified representation of [l] on
    [ppf]. *)

val level_to_string : level option -> string
(** [level_to_string l] converts [l] to an US-ASCII string that can be
    parsed back by {!level_of_string} and by the [LEVEL] option
    argument of {!Logs_cli.level}. *)

val level_of_string : string -> (level option, [`Msg of string]) result
(** [level_of_string s] parses the representation of {!level_to_string}
    from [s]. *)

(** {1:srcs Log sources} *)

type src
(** The type for log sources. A source defines a named unit of logging
    whose reporting level can be set independently. *)

val default : src
(** [default] is a logging source that is reserved for use by
    applications. See {{!usage}usage conventions}. *)

(** Sources. *)
module Src : sig

  (** {1 Sources} *)

  type t = src
  (** The type for log sources. *)

  val create : ?doc:string -> string -> src
  (** [create ?doc name] is a new log source. [name] is the name of
      the source; it doesn't need to be unique but it is good practice
      to prefix the name with the name of your package or library
      (e.g. ["mypkg.network"]). [doc] is a documentation string
      describing the source, defaults to ["undocumented"]. The initial
      reporting level of the source is defined by {!Logs.level}. *)

  val name : src -> string
  (** [name] is [src]'s name. *)

  val doc : src -> string
  (** [doc src] is [src]'s documentation string. *)

  val level : src -> level option
  (** [level src] is the report level of [src] (if any).  *)

  val set_level : src -> level option -> unit
  (** [set_level src l] sets the report level of [src] to [l]. Only
      applications should use this function directly, see {{!usage}usage
      conventions}. *)

  val equal : src -> src -> bool
  (** [equal src src'] is [true] iff [src] and [src'] are the same source. *)

  val compare : src -> src -> int
  (** [compare src src'] is a total order on sources. *)

  val pp : Format.formatter -> src -> unit
  (** [pp ppf src] prints an unspecified representation of [src] on
      [ppf]. *)

  val list : unit -> src list
  (** [list ()] is the current exisiting source list. *)
end

(** {1:func Log functions} *)

(** Message tags.

    Message tags are arbitrary named and typed values that can be
    associated to log messages. See an {{!ex1}example}. *)
module Tag : sig

  (** {1 Tag definitions} *)

  type 'a def
  (** The type for tag definitions. The type ['a] is the type of the
      tag. The definition specifies a name for the tag, a pretty-printer
      for the type of the tag and a documentation string. See {!val:def}. *)

  (** The type for existential tag definitions. *)
  type def_e = Def : 'a def -> def_e

  val def : ?doc:string -> string -> (Format.formatter -> 'a -> unit) -> 'a def
  (** [def ~doc name pp] is a tag definition. [name] is the name of
      the tag, it doesn't need to be unique. [pp] is a printer for the
      type of the tag. [doc] is a documentation string describing
      the tag (defaults to ["undocumented"]). *)

  val name : 'a def -> string
  (** [name d] is [d]'s name. *)

  val doc : 'a def -> string
  (** [doc d] is [d]'s documentation string. *)

  val printer : 'a def -> (Format.formatter -> 'a -> unit)
  (** [printer d] is [d]'s type pretty-printer. *)

  val pp_def : Format.formatter -> 'a def -> unit
  (** [pp_def ppf d] prints an unspecified representation of [d] on [ppf]. *)

  val list : unit -> def_e list
  (** [tag_list ()] is the list of currently existing tag definitions. *)

  (** {1 Tags} *)

  (** The type for tags. Tuples the tag definition and its value. *)
  type t = V : 'a def * 'a -> t

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints an unspecified representation of [t] on [ppf]. *)

  (** {1 Tag sets} *)

  type set
  (** The type for tag sets. A tag set contains at most one tag per
      tag definition. *)

  val empty : set
  (** [empty] is the empty set. *)

  val is_empty : set -> bool
  (** [is_empty s] is [true] iff [s] is empty. *)

  val mem : 'a def -> set -> bool
  (** [mem d s] is [true] iff [s] has a tag with definition [d]. *)

  val add : 'a def -> 'a -> set -> set
  (** [add d v s] is [s] with the tag [(V (d, v))] added. If there was a tag
      with definition [d] in [s] it is replaced. *)

  val rem : 'a def -> set -> set
  (** [rem d s] is [s] without the tag defined by [d] (if there was one). *)

  val find : 'a def -> set -> 'a option
  (** [find d s] is the tag value with definition [d] in [s] (if any). *)

  val get : 'a def -> set -> 'a
  (** [get d s] is like [find d s] but @raise Invalid_argument if there
      is no tag with definition [d] in [s]. *)

  val fold : (t -> 'a -> 'a) -> set -> 'a -> 'a
  (** [fold f s acc] is the result of folding [f] over the tags
      of [s] starting with [acc]. *)

  val pp_set : Format.formatter -> set -> unit
  (** [pp_set ppf s] prints an unspecified representation of s on [ppf]. *)
end

type ('a, 'b) msgf =
  (?header:string -> ?tags:Tag.set ->
   ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
(** The type for client specified message formatting functions.

    Message formatting functions are called with a message
    construction function whenever a message needs to be reported. The
    message formatting function must call the given message
    construction function with a format string and its arguments to
    define the message contents, see the {{!logging}basics} for examples.
    The optional arguments of the message construction function are:
    {ul
    {- [header], an optional printable message header. Default to [None].}
    {- [tags], a set of tags to attach to the message. Defaults
       {!Tag.empty}.}} *)

type 'a log = ('a, unit) msgf -> unit
(** The type for log functions. See the {{!logging}basics} to understand
    how to use log functions. *)

val msg : ?src:src -> level -> 'a log
(** [msg ?src l (fun m -> m fmt ...)] logs with level [l] on the source
    [src] (defaults to {!default}) a message formatted with [fmt]. For the
    semantics of levels see the {{!usage}the usage conventions}. *)

val app : ?src:src -> 'a log
(** [app] is [msg App]. *)

val err : ?src:src -> 'a log
(** [err] is [msg Error]. *)

val warn : ?src:src -> 'a log
(** [warn] is [msg Warning]. *)

val info : ?src:src -> 'a log
(** [info] is [msg Info]. *)

val debug : ?src:src -> 'a log
(** [debug] is [msg Debug]. *)

val kmsg : (unit -> 'b) -> ?src:src -> level -> ('a, 'b) msgf -> 'b
(** [kmsg k] is like {!msg} but calls [k] for returning. *)

(** {2:result Logging [result] value [Error]s} *)

val on_error : ?src:src -> ?level:level -> ?header:string -> ?tags:Tag.set ->
  pp:(Format.formatter -> 'b -> unit) -> use:('b -> 'a) -> ('a, 'b) result -> 'a
(** [on_error ~level ~pp ~use r] is:
    {ul
    {- [v] if [r = Ok v]}
    {- [use e] if [r = Error e]. As a side effect [msg] is logged
       with [pp] on level [level] (defaults to {!Logs.Error}).}} *)

val on_error_msg : ?src:src -> ?level:level -> ?header:string ->
  ?tags:Tag.set -> use:(unit -> 'a) ->
  ('a, [`Msg of string]) result -> 'a
(** [on_error_msg] is like {!on_error} but uses
    {!Format.pp_print_text} to format the message. *)

(** {1:srcfunc Source specific log functions} *)

(** The type for source specific logging functions. *)
module type LOG = sig

  (** {1:func Log functions} *)

  val msg : level -> 'a log
  (** See {!Logs.msg}. *)

  val app : 'a log
  (** [app] is [msg App]. *)

  val err : 'a log
  (** [err] is [msg Error]. *)

  val warn : 'a log
  (** [warn] is [msg Warning]. *)

  val info : 'a log
  (** [info] is [msg Info]. *)

  val debug : 'a log
  (** [debug] is [msg Debug]. *)

  val kmsg : (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b
  (** See {!Logs.kmsg}. *)

  (** {2:result Logging [result] value [Error]s} *)

  val on_error : ?level:level -> ?header:string -> ?tags:Tag.set ->
    pp:(Format.formatter -> 'b -> unit) -> use:('b -> 'a) -> ('a, 'b) result ->
    'a
  (** See {!Logs.on_error}. *)

  val on_error_msg : ?level:level -> ?header:string -> ?tags:Tag.set ->
    use:(unit -> 'a) -> ('a, [`Msg of string]) result -> 'a
  (** See {!Logs.on_error_msg}. *)
end

val src_log : src -> (module LOG)
(** [src_log src] is a {{!LOG}set of logging functions} for [src]. *)

(** {1:reporters Reporters} *)

type reporter =
  { report : 'a 'b. src -> level -> over:(unit -> unit) -> (unit -> 'b) ->
      ('a, 'b) msgf -> 'b }
(** The type for reporters.

    A reporter formats and handles log messages that get
    reported. Whenever a {{!func}log function} gets called on a source
    with a level equal or smaller to the {{!Src.level}source's reporting
    level}, the {{!reporter}current reporter}'s field [r.report]
    gets called as [r.report src level ~over k msgf]
    where:
    {ul
    {- [src] is the logging source.}
    {- [level] is the reporting level.}
    {- [over] must be called by the reporter once the logging operation is
       over from the reporter's perspective. This may happen before or
       after [k] is called.}
    {- [k] is the function to invoke to return.}
    {- [msgf] is the {{!msgf}message formatting function} to call.}}
    See an {{!ex1}example}. *)

val nop_reporter : reporter
(** [nop_reporter] is the initial reporter returned by {!reporter}, it
    does nothing if a log message gets reported. *)

val format_reporter :
  ?pp_header:(Format.formatter -> (level * string option) -> unit) ->
  ?app:Format.formatter -> ?dst:Format.formatter -> unit -> reporter
(** [format_reporter ~pp_header ~app ~dst ()] is a reporter that reports
    {!App} level messages on [app] (defauts to {!Format.std_formatter})
    and all other level on [dst] (defaults to {!Format.err_formatter}).

    [pp_header] determines how message headers are rendered. The default
    prefixes the program name and renders the header with {!pp_header}.
    Use {!Logs_fmt.reporter} if you want colored headers rendering.

    The reporter does not process or render information about message
    sources or tags.

    {b Important.} This is a synchronous reporter it considers the log
    operation to be over once the message was formatted and before
    calling the continuation (see the {{!Logs.sync}note on synchronous
    logging}). In particular if the formatters are backed by channels,
    it will block until the message has been formatted on the channel
    before proceeding which may not be suitable in a cooperative
    concurrency setting like {!Lwt}. *)

val reporter : unit -> reporter
(** [reporter ()] is the current repporter. *)

val set_reporter : reporter -> unit
(** [set_reporter r] sets the current reporter to [r]. *)

val set_reporter_mutex : lock:(unit -> unit) -> unlock:(unit -> unit) -> unit
(** [set_reporter_mutex ~lock ~unlock] sets the mutex primitives used
    to access the reporter. [lock] is called before invoking the
    reporter and [unlock] after it returns. Initially both [lock] and
    [unlock] are [fun () -> ()]. *)

(**/**)
val report : src -> level -> over:(unit -> unit) -> (unit -> 'b) ->
  ('a, 'b) msgf -> 'b
val incr_err_count : unit -> unit
val incr_warn_count : unit -> unit
val pp_print_text : Format.formatter -> string -> unit
(**/**)

val pp_header : Format.formatter -> (level * string option) -> unit
(** [pp_header ppf (l, h)] prints an unspecified representation
    of log header [h] for level [l]. *)

(** {1:monitoring Logs monitoring} *)

val err_count : unit -> int
(** [err_count ()] is the number of messages logged with level [Error]
    across all sources. *)

val warn_count : unit -> int
(** [warn_count ()] is the number of messages logged with level
    [Warning] across all sources. *)

(** {1:basics Basics}

    {2:logging Logging}

    In order to minimize the overhead whenever a log message is not reported,
    message formatting only occurs on actual message report via the
    {{!msgf}message formatting function} you provide to log functions. This
    leads to the following logging structure:
{[
let k, v = ... in
Logs.err (fun m -> m "invalid kv (%a,%a)" pp_key k pp_val v);
Logs.err (fun m -> m "NO CARRIER");
]}
    The pattern is quite simple: it is as if you were formatting with
    a [printf]-like function except you get this function in the [m]
    argument of the function you give to the logging function.

    If you want to abstract a repeated log report it is better to keep
    the message formatting function structure for the arguments of the
    messages. Here's how the above examples can be abstracted and
    invoked:
{[
let err_invalid_kv args =
  Logs.err @@ fun m ->
  args (fun k v -> m "invalid kv (%a,%a)" pp_key k pp_val v)

let err_no_carrier args =
  Logs.err @@ fun m -> args (m "NO CARRIER")

let () =
  err_invalid_kv (fun args -> args "key" "value");
  err_no_carrier (fun () -> ());
  ()
]}
    Note that log messages are formatted and hit the reporter only if
    they have not been filtered out by the current
    {{!Src.level}reporting level} of the source you log on. See also
    the log source and reporting level {{!usage}usage conventions}.

    {2:setupreporter Reporter setup}

    If you are writing an application you must remember to
    {{!set_reporter}set} the reporter before any logging operation
    takes place otherwise no messages will be reported. For example if
    you are using the {{!Logs_fmt}formatter reporter}, logging
    can be setup as follows:
{[
let main () =
  Logs.set_reporter (Logs_fmt.reporter ());
  ...
  exit (if Logs.err_count () > 0 then 1 else 0);
  ()
]}
    If you have logging code that is performed in the toplevel
    initialization code of modules (not a good idea) or you depend on
    (bad) libraries that do so, you must call and link the reporter
    install code before these initialization bits are being executed
    otherwise you will miss these messages.

    In multi-threaded programs you likely want to ensure mutual
    exclusion on reporter access. This can be done by invoking
    {!Logs.set_reporter_mutex} with suitable mutual exclusion
    primitives. If you use OCaml {!Thread}s simply calling
    {!Logs_threaded.enable} with handle that for you.

    If you need to use multiple reporters in your program see this
    {{!ex2}sample code}.

    The documentation of {!Logs_cli} module has a {{!Logs_cli.ex}full setup
    example} that includes command line options to control color and log
    reporting level.

    If you are writing a library you should neither install reporters, nor
    set the reporting level of sources, nor log on the {!default} source or
    at the [App] level; follow the {{!usage}the usage conventions}. A
    library should simply log on another existing source or define
    its own source like in the example below:
{[
let src = Logs.Src.create "mylib.network" ~doc:"logs mylib's network events"
module Log = (val Logs.src_log src : Logs.LOG)
]}
    The [Log] module defines logging functions that are specific to the
    source [src].

    {1:usage Usage conventions}

    A library should never log on the {!default} source or at the
    [App] level these are reserved for use by the application. It
    should either create a source for itself or log on the source
    defined by one of its dependencies. It should also never set the
    reporting level of the sources it deals with or install reporters since
    control over this must be left to the application.

    The semantics of {{!type:level}reporting levels} should be understood
    as follows:
    {ul
    {- [App], this level can be used for the standard output or console
       of an application. It should never be used by libraries.}
    {- [Error], error condition that prevent the program from
       running normally.}
    {- [Warning], suspicious condition that does not prevent the
       program from running normally but may eventually lead to an
       error condition.}
    {- [Info], condition that allows the program {e user} to get a better
       understanding of what the program is doing.}
    {- [Debug], condition that allows the program {e developer} to get a
       better undersanding of what the program is doing.}}

    {1:sync Note on synchronous logging}

    In synchronous logging, a client call to a log function proceeds
    only once the reporter has finished the report operation. In
    [Logs] this depends both on the reporter and the log functions
    that the client uses.

    Whenever the client uses a log function that results in a report,
    it gives the reporter a continuation that defines the result type
    of the log function and a callback to be called whenever the log
    operation is over from the reporter's perspective (see {!type:reporter}).
    The typical use of the callback is to unblock the continuation given
    to the reporter. This is used by {!Logs_lwt}'s log functions to make
    sure that the threads they return proceed only once the report is over.
    In the functions of {!Logs} however the callback does nothing as there
    is no way to block the polymorphic continuation.

    Now considering reporters, at the extreme we have:
    {ul
    {- A completely asynchronous reporter. This reporter formats the
       message in memory and immediately invoke the callback followed
       by the continuation. This provides no guarantee of persistency
       in case a crash occurs. All log functions behave asynchronously
       and return as soon as possible.}
    {- A completely synchronous reporter. This reporter formats the
       message, persist it, invoke the client callback followed by the
       continuation. All log functions behave synchronously. An
       example of such a reporter is {!Logs_fmt.reporter} with
       formatters baked by channels: when formatting returns the
       message has been written on the channel.}}

    However a purely synchronous reporter like {!Logs_fmt.reporter}
    acting on channels does not play well with [Lwt]'s cooperative
    runtime system. It is possible to reuse {!Logs_fmt.reporter} to
    define a cooperative reporter, see {{!Logs_lwt.report_ex}this
    example}. However while this reporter makes {!Logs_lwt}'s log
    functions synchronous, those of {!Logs} behave asynchronously. For
    now it seems it that this is unfortunately the best we can do if
    we want to preserve the ability to use [Logs] with or without
    cooperative concurency.

    {1:ex1 Example with custom reporter and tags}

    This example uses a {{!Tag}tag} to attach {!Mtime} time spans in
    log messages. The custom reporter uses these time spans to format
    relative timings for runs of a given function. Note that as done
    below the timings do include logging time.
{[
let stamp_tag : Mtime.span Logs.Tag.def =
  Logs.Tag.def "stamp" ~doc:"Relative monotonic time stamp" Mtime.Span.pp

let stamp c = Logs.Tag.(empty |> add stamp_tag (Mtime_clock.count c))

let run () =
  let rec wait n = if n = 0 then () else wait (n - 1) in
  let c = Mtime_clock.counter () in
  Logs.info (fun m -> m "Starting run");
  let delay1, delay2, delay3 = 10_000, 20_000, 40_000 in
  Logs.info (fun m -> m "Start action 1 (%d)." delay1 ~tags:(stamp c));
  wait delay1;
  Logs.info (fun m -> m "Start action 2 (%d)." delay2 ~tags:(stamp c));
  wait delay2;
  Logs.info (fun m -> m "Start action 3 (%d)." delay3 ~tags:(stamp c));
  wait delay3;
  Logs.info (fun m -> m "Done." ?header:None ~tags:(stamp c));
  ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_stamp h tags k ppf fmt =
      let stamp = match tags with
      | None -> None
      | Some tags -> Logs.Tag.find stamp_tag tags
      in
      let dt = match stamp with None -> 0. | Some s -> Mtime.Span.to_us s in
      Format.kfprintf k ppf ("%a[%0+04.0fus] @[" ^^ fmt ^^ "@]@.")
        Logs.pp_header (level, h) dt
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
  in
  { Logs.report = report }

let main () =
  Logs.set_reporter (reporter (Format.std_formatter));
  Logs.set_level (Some Logs.Info);
  run ();
  run ();
  ()

let () = main ()
]}
Here is the standard output of a sample run of the program:
{v
[INFO][+000us] Starting run
[INFO][+168us] Start action 1 (10000).
[INFO][+206us] Start action 2 (20000).
[INFO][+243us] Start action 3 (40000).
[INFO][+303us] Done.
[INFO][+000us] Starting run
[INFO][+012us] Start action 1 (10000).
[INFO][+038us] Start action 2 (20000).
[INFO][+074us] Start action 3 (40000).
[INFO][+133us] Done.
v}

    {1:ex2 Logging to multiple reporters}

    Logging to multiple reporters can be achieved by defining a new reporter
    that simply forwards to them. The following example combines
    two reporters:
{[
let combine r1 r2 =
  let report = fun src level ~over k msgf ->
    let v = r1.Logs.report src level ~over:(fun () -> ()) k msgf in
    r2.Logs.report src level ~over (fun () -> v) msgf
  in
  { Logs.report }

let () =
  let r1 = Logs.format_reporter () in
  let r2 = Logs_fmt.reporter () in
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (combine r1 r2);
  Logs.err (fun m -> m "HEY HO!");
  ()
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
