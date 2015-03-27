(** Book processing. The book's files are in HTMLBook format with some
    custom variations, e.g. we support <link rel="import"> nodes to
    enable inclusion of code blocks in external files. This module
    supports the processing of these files.
*)
open Core.Std
open Async.Std

(** Source from which to build an HTML page.

    - [`Chapter file] - The file for a chapter. Path must be relative
    to current working directory.

    - [`Frontpage] - No other information needed. We generate the
    whole page programmatically. Output file is named "index.html".
*)
type src = [
| `Chapter of string
| `Frontpage
| `Toc_page
| `FAQs
| `Install
]

(** Make an HTML page from the given [src] and save it to [out_dir]. *)
val make
  :  ?run_pygmentize:bool
  -> ?repo_root:string
  -> out_dir:string
  -> src
  -> unit Deferred.t


(******************************************************************************)
(** {2 Parts, Chapters, and Sections *)
(******************************************************************************)
type part_info = {
  number : int;
  title : string;
}

type section = {
  id : string;
  title : string;
}

(** Interpret as n-ary tree with depth 3. *)
type sections = (section * (section * section list) list) list

type chapter = {
  number : int;
  filename : string; (** basename *)
  title : string;
  part_info : part_info option;
  sections : sections;
}

type part = {
  info : part_info option;
  chapters : chapter list
}

(** Return all chapter numbers and names, ordered by chapter
    number. *)
val chapters : ?repo_root:string -> unit -> chapter list Deferred.t

val group_chapters_by_part : chapter list -> part list

(** [get_sections filename html] returns the section structure within
    the chapter of the given file, to depth 3. The [filename] is only
    for error messages. *)
val get_sections : string -> Rwo_html.t -> sections

(** Useful for debugging. *)
val flatten_sections : sections -> section list


(******************************************************************************)
(** {2 indexterm nodes} *)
(******************************************************************************)
val indexterm_to_idx : Rwo_html.t -> Rwo_html.t
val idx_to_indexterm : Rwo_html.t -> Rwo_html.t
