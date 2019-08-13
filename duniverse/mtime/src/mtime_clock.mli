(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Monotonic time clock.

    [Mtime_clock] provides access to a system monotonic clock. This
    time increases monotonically and is not subject to operating
    system calendar time adjustments.

    Only use {!Mtime_clock.now} if you need inter-process time
    correlation, otherwise prefer {!Mtime_clock.elapsed} and
    {{!Mtime_clock.counters}counters}.

    Consult important information about {{!err}error handling}
    and {{!platform_support}platform support}.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:clock Monotonic clock} *)

val elapsed : unit -> Mtime.span
(** [elapsed ()] is the monotonic time span elapsed since the
    beginning of the program.

    @raise Sys_error see {{!err}error handling} *)

val now : unit -> Mtime.t
(** [now ()] is the current system-relative monotonic timestamp. Its
    absolute value is meaningless.

    @raise Sys_error see {{!err}error handling} *)

val period : unit -> Mtime.span option
(** [period ()] is the clock's period as a monotonic time span (if
    available). *)

(** {1:counters Time counters} *)

type counter
(** The type for monotonic wall-clock time counters. *)

val counter : unit -> counter
(** [counter ()] is a counter counting from now on.

    @raise Sys_error see {{!err}error handling} *)

val count : counter -> Mtime.span
(** [count c] is the monotonic time span elapsed since [c] was created. *)

(** {1:raw Monotonic clock raw interface} *)

val elapsed_ns : unit -> int64
(** [now_ns ()] is the {e unsigned} 64-bit integer nanosecond monotonic
     time span elapsed since the beginning of the program.

    @raise Sys_error see {{!err}error handling} *)

val now_ns : unit -> int64
(** [now_ns ()] is an {e unsigned} 64-bit integer nanosecond
     system-relative monotonic timestamp. The absolute value is
     meaningless.

    @raise Sys_error see {{!err}error handling} *)

val period_ns : unit -> int64 option
(** [period_ns ()] is the clock's period as an {e unsigned} 64-bit
    integer nanosecond monotonic time span (if available). *)

(** {1:err Error handling}

    The functions {!elapsed}, {!now}, {!counter}, {!elapsed_ns} and
    {!now_ns} raise [Sys_error] whenever they can't determine the
    current time or that it doesn't fit in [Mtime]'s range. Ususally
    this exception should only be catched at the toplevel of your
    program to log it and abort the program. It indicates a serious
    error condition in the system.

    All the other functions, whose functionality is less essential,
    simply silently return [None] if they can't determine the
    information either because it is unavailable or because an error
    occured.

    {1:platform_support Platform support}

    {ul
    {- Platforms with a POSIX clock (includes Linux) use
       {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/clock_gettime.html}[clock_gettime]}
       with CLOCK_MONOTONIC.}
    {- Darwin uses
       {{:https://developer.apple.com/library/mac/qa/qa1398/_index.html}[mach_absolute_time]}.}
    {- Windows is TODO, use
       {{:https://msdn.microsoft.com/en-us/library/windows/desktop/aa373083%28v=vs.85%29.aspx}Performance counters}. }
    {- JavaScript uses
       {{:http://www.w3.org/TR/hr-time/}[performance.now]} (consult
       {{:http://caniuse.com/#feat=high-resolution-time}availability})
       which returns a
       {{:http://www.w3.org/TR/hr-time/#sec-DOMHighResTimeStamp}double
       floating point value} in milliseconds with
       resolution up to the microsecond.}}
*)


(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli

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
