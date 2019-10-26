(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open Fts_types
open FTS

module N = Fts_bindings.Bindings(Fts_generated)
open N

let crush_options f : 'a list -> int = List.fold_left (fun i o -> i lor (f o)) 0

let fts_read fts =
  let p = _fts_read fts.ptr in
  if to_voidp p = null then None
  else Some p

let fts_close ftsp =
  ignore (_fts_close ftsp.ptr)
      
let fts_set ~ftsp ~f ~options =
  ignore (_fts_set ftsp.ptr f (crush_options fts_set_option_value options))

let fts_children ~ftsp ~name_only =
  _fts_children ftsp.ptr (fts_children_option_of_bool name_only)

let null_terminated_array_of_ptr_list typ list =
  let nitems = List.length list in
  let arr = CArray.make typ (1 + nitems) in
  List.iteri (CArray.set arr) list;
  (coerce (ptr typ) (ptr (ptr void)) (CArray.start arr +@ nitems)) <-@ null;
  arr

let fts_open ~path_argv ?compar ~options = 
  let path_argv_cpointers = List.map _strdup path_argv in
  let paths = null_terminated_array_of_ptr_list (ptr char) path_argv_cpointers in
  let options = crush_options fts_open_option_value options in
  let r = { ptr = _fts_open (CArray.start paths) options compar; compar } in
  List.iter _free path_argv_cpointers;
  r
