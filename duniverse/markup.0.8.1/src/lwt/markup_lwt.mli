(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

(** Lwt interface to Markup.ml.

    The majority of the functions in this interface are listed in the signature
    {!Markup.ASYNCHRONOUS}, and are not directly included on this page. There
    are also additional Lwt functions in module {!Markup_lwt_unix}. Those are
    based on [Lwt_io], and have been separated to make this module [Markup_lwt]
    usable on [js_of_ocaml], which does not support [Lwt_io].

    This module is available if Markup.ml is installed when Lwt is installed,
    i.e.

{[
opam install lwt markup
]}

    To link with this module, depend on the findlib package [markup.lwt] instead
    of package [markup]. *)

open Markup

include Markup.ASYNCHRONOUS with type 'a io := 'a Lwt.t

val lwt_stream : 'a Lwt_stream.t -> ('a, async) stream
(** Adapts an Lwt stream to a Markup.ml stream. *)

val to_lwt_stream : ('a, _) stream -> 'a Lwt_stream.t
(** Adapts a Markup.ml stream to an Lwt stream. *)

val ensure_tail_calls : ?hook:((exn -> unit) ref) -> unit -> unit
(** @deprecated Not necessary since Markup.ml 0.7.4. *)

(**/**)

val to_cps : (unit -> 'a Lwt.t) -> (exn -> unit) -> ('a -> unit) -> unit

(**/**)
