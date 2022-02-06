(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fmt programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [Fmt] TTY setup.

    [Fmt_tty] provides simple automatic setup on channel formatters for:
    {ul
    {- {!Fmt.set_style_renderer}. [`Ansi_tty] is used if the channel
       {{!Unix.isatty}is a tty} and the environment variable
       [TERM] is defined and its value is not ["dumb"]. [`None] is
       used otherwise.}
    {- {!Fmt.set_utf_8}. [true] is used if one of the following
       environment variables has ["UTF-8"] as a case insensitive
       substring: [LANG], [LC_ALL], [LC_CTYPE].}} *)

(** {1:tty_setup TTY setup} *)

val setup : ?style_renderer:Fmt.style_renderer -> ?utf_8:bool ->
  out_channel -> Format.formatter
(** [setup ?style_renderer ?utf_8 outc] is a formatter for [outc] with
    {!Fmt.set_style_renderer} and {!Fmt.set_utf_8} correctly setup. If
    [style_renderer] or [utf_8] are specified they override the automatic
    setup.

    If [outc] is {!stdout}, {!Fmt.stdout} is returned. If [outc] is
    {!stderr}, {!Fmt.stderr} is returned. *)

val setup_std_outputs : ?style_renderer:Fmt.style_renderer -> ?utf_8:bool ->
  unit -> unit
(** [setup_std_outputs ?style_renderer ?utf_8 ()] applies {!setup}
    on {!stdout} and {!stderr}. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fmt programmers

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
