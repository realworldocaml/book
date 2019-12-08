(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

(** Stream functions based on [Lwt_io].

    This module contains additional functions over {!Markup_lwt}.

    [Markup_lwt_unix] is available if Markup.ml is installed when Lwt is
    installed, i.e.

{[
opam install lwt markup
]}

    To link with this module, depend on the findlib package [markup.lwt.unix]
    instead of [markup] or [markup.lwt]. *)

open Markup

val channel : Lwt_io.input Lwt_io.channel -> (char, async) stream
(** Evaluates to a stream that retrieves successive bytes from the given
    channel. If the channel cannot be read, the next read of the stream results
    in the thread failing with an exception, as specified in [Lwt_io]. *)

val file : string -> (char, async) stream * (unit -> unit Lwt.t)
(** Evaluates to a pair [s, close], where reading from stream [s] retrieves
    successive bytes from the given file, and completing [close ()] closes the
    file. If the file cannot be opened, the first read of the stream results in
    failure with an exception, as specified in [Lwt_io]. If the file cannot be
    read, reading the stream results in the reading thread failing with an
    exception, also as in [Lwt_io]. *)

val to_channel : Lwt_io.output Lwt_io.channel -> (char, _) stream -> unit Lwt.t
(** Writes bytes from the given stream to the given channel. *)

val to_file : string -> (char, _) stream -> unit Lwt.t
(** Writes bytes from the given stream to the given file. *)
