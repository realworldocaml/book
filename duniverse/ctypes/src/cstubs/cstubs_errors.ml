(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Exception definitions *)

exception Cstubs_internal_error of string

let internal_error fmt = 
  Format.ksprintf (fun s -> raise (Cstubs_internal_error s)) fmt
