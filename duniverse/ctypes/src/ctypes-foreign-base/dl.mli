(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Bindings to the dlopen / dlsym interface. *)

type library
(** The type of dynamic libraries, as returned by {!dlopen}. *)

exception DL_error of string
(** An error condition occurred when calling {!dlopen}, {!dlclose} or
    {!dlsym}.  The argument is the string returned by the [dlerror]
    function. *)

(** Flags for {!dlopen}

Note for windows users: Only [RTLD_NOLOAD] and [RTLD_NODELETE] are supported.
Passing no or any other flags to {!dlopen} will result in standard behaviour:
just LoadLibrary is called. If [RTLD_NOLOAD] is specified and the module is
not already loaded, a {!DL_error} with the string "library not loaded" is
thrown; there is however no test, if such a library exists at all (like under
linux).
*)
type flag = 
    RTLD_LAZY
  | RTLD_NOW
  | RTLD_GLOBAL
  | RTLD_LOCAL
  | RTLD_NODELETE
  | RTLD_NOLOAD
  | RTLD_DEEPBIND

val dlopen : ?filename:string -> flags:flag list -> library
(** Open a dynamic library.

Note for windows users: the filename must be encoded in UTF-8 *)

val dlclose : handle:library -> unit
(** Close a dynamic library. *)

val dlsym : ?handle:library -> symbol:string -> nativeint
(** Look up a symbol in a dynamic library. *)
