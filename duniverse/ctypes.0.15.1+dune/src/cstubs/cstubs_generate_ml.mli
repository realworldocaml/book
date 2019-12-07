(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* ML stub generation *)

val extern : concurrency:[ `Sequential | `Lwt_jobs | `Lwt_preemptive | `Unlocked ] ->
         errno:[ `Ignore_errno | `Return_errno ] ->
         stub_name:string -> external_name:string -> Format.formatter ->
         ('a -> 'b) Ctypes.fn -> unit

val case : concurrency:[ `Sequential | `Lwt_jobs | `Lwt_preemptive | `Unlocked ] ->
         errno:[ `Ignore_errno | `Return_errno ] ->
         stub_name:string -> external_name:string -> Format.formatter ->
         ('a -> 'b) Ctypes.fn -> unit

val val_case : stub_name:string -> external_name:string -> Format.formatter ->
         'a Ctypes.typ -> unit

val constructor_decl : concurrency:[ `Sequential | `Lwt_jobs | `Lwt_preemptive | `Unlocked ] ->
  errno:[ `Ignore_errno | `Return_errno ] ->
  string -> 'a Ctypes.fn -> Format.formatter -> unit

val inverse_case : register_name:string -> constructor:string -> string ->
         Format.formatter -> ('a -> 'b) Ctypes.fn -> unit
