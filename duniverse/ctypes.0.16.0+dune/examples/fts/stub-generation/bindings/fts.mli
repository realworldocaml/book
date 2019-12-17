(*
 * Copyright (c)  Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open Fts_types

(* The fts functions are provided for traversing file hierarchies.  A simple
   overview is that the fts_open() function returns a "handle" on a file
   hierarchy, which is then supplied to the other fts functions.  The function
   fts_read() returns a pointer to a structure describing one of the files in the
   file hierarchy.  The function fts_children() returns a pointer to a linked
   list of structures, each of which describes one of the files contained in a
   directory in the hierarchy.  In general, directories are visited two
   distinguishable times; in preorder (before any of their descendants are
   visited) and in postorder (after all of their descendants have been visited).
   Files are visited once.  It is possible to walk the hierarchy "logically"
   (ignoring symbolic links) or physically (visiting symbolic links), order the
   walk of the hierarchy or prune and/or revisit portions of the hierarchy.  *)

module Bindings (F : sig val foreign : string -> 'a fn -> unit end) : sig
  (* The fts_open() function takes a list of strings naming one or more paths
     which make up a logical file hierarchy to be traversed.

     There are a number of options, at least one of which (either FTS_LOGICAL
     or FTS_PHYSICAL) must be specified.

     The argument compar() specifies a user-defined function which may be used
     to order the traversal of the hierarchy.  It takes two pointers to
     pointers to FTSENT structures as arguments and should return a negative
     value, zero, or a positive value to indicate if the file referenced by
     its first argument comes before, in any order with respect to, or after,
     the file referenced by its second argument.  The fts_accpath, fts_path
     and fts_pathlen fields of the FTSENT structures may never be used in this
     comparison.  If the fts_info field is set to FTS_NS or FTS_NSOK, the
     fts_statp field may not either.  If the compar() argument is NULL, the
     directory traversal order is in the order listed in path_argv for the
     root paths, and in the order listed in the directory for everything
     else.  *)
  val fts_open :
    path_argv:string list ->
    ?compar:(FTSENT.t ptr -> FTSENT.t ptr -> int) ->
    options:fts_open_option list ->
    FTS.t

  (* The fts_children() function returns a pointer to an FTSENT structure
     describing the first entry in a NULL-terminated linked list of the files
     in the directory represented by the FTSENT structure most recently
     returned by fts_read().  The list is linked through the fts_link field of
     the FTSENT struc‐ ture, and is ordered by the user-specified comparison
     function, if any.  Repeated calls to fts_children() will recreate this
     linked list.

     As a special case, if fts_read() has not yet been called for a hierarchy,
     fts_children() will return a pointer to the files in the logical
     directory specified to fts_open(), that is, the arguments specified to
     fts_open().  Otherwise, if the FTSENT structure most recently returned by
     fts_read() is not a directory being visited in preorder, or the directory
     does not contain any files, fts_children() returns NULL and sets errno to
     zero.  If an error occurs, fts_children() returns NULL and sets errno
     appropriately.

     The FTSENT structures returned by fts_children() may be overwritten after
     a call to fts_children(), fts_close() or fts_read() on the same file
     hierarchy stream.

     The name_only option indicates that only the names of the files are
     needed.  The contents of all the fields in the returned linked list of
     structures are undefined with the exception of the fts_name and
     fts_namelen fields.  *)
  val fts_children :
    ftsp:FTS.t ->
    name_only:bool ->
    FTSENT.t

  (* The fts_read() function returns a pointer to an FTSENT structure
     describing a file in the hierarchy.  Directories (that are readable and
     do not cause cycles) are visited at least twice, once in preorder and
     once in postorder.  All other files are visited at least once.  (Hard
     links between directories that do not cause cycles or symbolic links to
     symbolic links may cause files to be visited more than once, or
     directories more than twice.)

     The FTSENT structures returned by fts_read() may be overwritten after a
     call to fts_close() on the same file hierarchy stream, or, after a call
     to fts_read() on the same file hierarchy stream unless they represent a
     file of type directory, in which case they will not be overwritten until
     after a call to fts_read() after the FTSENT structure has been returned
     by the function fts_read() in postorder.  *)
  val fts_read : FTS.t -> FTSENT.t option

  (* The function fts_set() allows the user application to determine further
     processing for the file f of the stream ftsp.  *)
  val fts_set :
    ftsp:FTS.t ->
    f:FTSENT.t ->
    options:fts_set_option list ->
    unit

  (* The fts_close() function closes a file hierarchy stream ftsp and restores
     the current directory to the directory from which fts_open() was called
     to open ftsp. *)
  val fts_close : FTS.t -> unit
end
