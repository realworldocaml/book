(** Table of contents. Representation of the hierarchical structure of
    the book.
*)
open! Core
open Async

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
  name : string; (** basename with .html *)
  title : string;
  part_info : part_info option;
  sections : sections;
}

type part = {
  info : part_info option;
  chapters : chapter list
}

type t = part list

val get : ?repo_root:string -> unit -> t Deferred.t
val of_chapters : chapter list -> part list

(** Return all chapter numbers and names, ordered by chapter
    number. *)
val get_chapters : ?repo_root:string -> unit -> chapter list Deferred.t

val get_next_chapter : chapter list -> chapter -> chapter option

val find: name:string -> t -> chapter option

(** [get_sections filename html] returns the section structure within
    the chapter of the given file, to depth 3. The [filename] is only
    for error messages. *)
val get_sections : filename:string -> Html.t -> sections

(** Useful for debugging. *)
val flatten_sections : sections -> section list

(** Return list of all files within the book/code directory. *)
val code_files : ?repo_root:string -> unit -> string list Deferred.t
