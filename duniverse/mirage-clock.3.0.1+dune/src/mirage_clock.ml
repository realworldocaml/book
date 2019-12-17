(*
 * Copyright (c) 2015 Matt Gray <matthew.thomas.gray@gmail.com>
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
(** Clock devices for MirageOS

    This module define clock devices signatures for MirageOS.

    {e Release %%VERSION%% } *)

(** {2 POSIX clock}

    Clock counting time since the Unix epoch. Subject to adjustment by
    e.g. NTP. *)
module type PCLOCK = sig

  val now_d_ps : unit -> int * int64
  (** [now_d_ps ()] is [(d, ps)] representing the POSIX time occurring
      at [d] * 86'400e12 + [ps] POSIX picoseconds from the epoch
      1970-01-01 00:00:00 UTC. [ps] is in the range
      \[[0];[86_399_999_999_999_999L]\]. *)

  val current_tz_offset_s : unit -> int option
  (** [current_tz_offset_s ()] is the clock's current local time zone
      offset to UTC in seconds, if known. This is the duration local
      time - UTC time in seconds. *)

  val period_d_ps : unit -> (int * int64) option
  (** [period_d_ps ()] is [Some (d, ps)] representing the clock's
      picosecond period [d] * 86'400e12 + [ps], if known. [ps] is in
      the range \[[0];[86_399_999_999_999_999L]\]. *)

end

(** {2 Monotonic clock}

    Clock returning monotonic time since an arbitrary point. To be
    used for e.g. profiling. *)
module type MCLOCK = sig

  val elapsed_ns : unit -> int64
  (** [elapsed_ns ()] is a monotonically increasing count of
      nanoseconds elapsed since some arbitrary point *)

  val period_ns : unit -> int64 option
  (** [period_ns ()] is [Some ns] representing the clock's nanosecond
      period [ns], if known *)

end
