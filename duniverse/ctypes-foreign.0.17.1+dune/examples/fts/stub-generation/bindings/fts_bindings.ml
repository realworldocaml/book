(*
 * Copyright (c)  Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open Fts_types

open FTSENT
open FTS

module Bindings (F : Ctypes.FOREIGN) =
struct
  open F

  (* FTS *fts_open(char * const *path_argv, int options,
     int ( *compar)(const FTSENT **, const FTSENT ** ));
  *)
  let _fts_open = foreign "fts_open"
    (ptr (ptr char) @-> int @-> compar_typ_opt @-> returning (ptr fts))

  (* FTSENT *fts_read(FTS *ftsp); *)
  let _fts_read = foreign "fts_read" (* ~check_errno:true *)
    (ptr fts @-> returning (ptr ftsent))

  (* FTSENT *fts_children(FTS *ftsp, int options); *)
  let _fts_children = foreign "fts_children"
    (ptr fts @-> int @-> returning (ptr ftsent))

  (* int fts_set(FTS *ftsp, FTSENT *f, int options); *)
  let _fts_set = foreign "fts_set" (* ~check_errno:true *)
    (ptr fts @-> ptr (ftsent) @-> int @-> returning int)

  (* int fts_close(FTS *ftsp); *)
  let _fts_close = foreign "fts_close" (* ~check_errno:true *)
    (ptr fts @-> returning int)

  let _strdup = foreign "strdup"
    (string @-> returning (ptr char))

  let _free = foreign "free"
    (ptr char @-> returning void)
end
