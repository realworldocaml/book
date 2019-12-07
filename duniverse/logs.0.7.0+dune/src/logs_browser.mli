(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Web browser reporters for {!Logs}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Reporters} *)

val console_reporter : unit -> Logs.reporter
(** [console_reporter ()] logs message using the
    {{:https://github.com/DeveloperToolsWG/console-object/blob/master/api.md}
    browser console object} at the corresponding level and uses
    [console.log] for the [App] level.

    The reporter does not process or render information about
    message sources or tags.

    Consult the {{:http://caniuse.com/#search=console}browser support}. *)

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
