(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** POSIX time clock.

    [Ptime_clock] provides access to a system POSIX time clock and to
    the system's current time zone offset.

    This time does not increase monotically and is subject to system
    calendar time adjustments. Use {!Mtime} if you need monotonic
    wall-clock time to measure time spans.

    Consult important information about {{!err}error handling}
    and {{!platform_support}platform support}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:clock POSIX clock} *)

val now : unit -> Ptime.t
(** [now ()] is the current POSIX time, by definition always on the
    UTC timeline.

    @raise Sys_error see {{!err}error handling}. *)

val period : unit -> Ptime.span option
(** [period ()] is a positive POSIX time span representing
    the clock's period (if available). *)

(** {1:tz System time zone offset} *)

val current_tz_offset_s : unit -> Ptime.tz_offset_s option
(** [current_tz_offset_s ()] is the system's current local time zone
    offset to UTC in seconds, if known. This is the duration local
    time - UTC time in seconds. *)

(** {1:raw POSIX clock raw interface} *)

val now_d_ps : unit -> int * int64
(** [now_d_ps ()] is [(d, ps)] representing POSIX time occuring at
    [d] * 86'400e12 + [ps] POSIX picoseconds from the epoch
    1970-01-01 00:00:00 UTC. [ps] is in the range
    \[[0];[86_399_999_999_999_999L]\].

    @raise Sys_error see {{!err}error handling} *)

val period_d_ps : unit -> (int * int64) option
(** [period_d_ps ()] is if available [Some (d, ps)] representing the
    clock's picosecond period [d] * 86'400e12 + [ps]. [ps] is in the
    range \[[0];[86_399_999_999_999_999L]\]. *)

(** {1:err Error handling}

    The functions {!now} and {!now_d_ps} raise [Sys_error] whenever
    they can't determine the current time or that it doesn't fit in
    [Ptime]'s well-defined {{!Ptime.t}range}. This exception should
    only be catched at the toplevel of your program to log it and
    abort the program. It indicates a serious error condition in the
    system.

    All the other functions, whose functionality is less essential,
    simply silently return [None] if they can't determine the
    information either because it is unavailable or because an error
    occured.

    {1:platform_support Platform support}

    {ul
    {- Platforms with a POSIX clock (includes Linux) use
       {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/clock_gettime.html}[clock_gettime]} with [CLOCK_REALTIME].}
    {- On Darwin {{:http://pubs.opengroup.org/onlinepubs/9699919799/}
                  [gettimeofday]} is used.}
    {- On Windows
       {{:https://msdn.microsoft.com/en-us/library/windows/desktop/ms724390(v=vs.85).aspx}[GetSystemTime]}
       and
       {{:https://msdn.microsoft.com/en-us/library/windows/desktop/ms724421(v=vs.85).aspx}[GetTimeZoneInformation]}
       are used.}
    {- On JavaScript
       {{:http://www.ecma-international.org/ecma-262/6.0/index.html#sec-date.now}[Date.now ()]} and
       {{:http://www.ecma-international.org/ecma-262/6.0/index.html#sec-date.prototype.gettimezoneoffset}[Date.prototype.getTimezoneOffset]} are used.}} *)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers

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
