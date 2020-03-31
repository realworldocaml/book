(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module type Abstract =
sig
  type t
  val t : t Ctypes.typ
end

let mkAbstract : 'a. 'a Ctypes.typ -> (module Abstract)
  = fun (type a) (ty : a Ctypes.typ) ->
    (module
     struct
       type t = a
       let t = ty
     end : Abstract)

let mkAbstractSized : name:string -> size:int -> alignment:int -> (module Abstract)
  = fun ~name ~size ~alignment:a ->
    (module
     struct
       open Ctypes
       type t = unit Ctypes.abstract
       let t = abstract ~name ~size ~alignment:a
     end : Abstract)

let mkArithmetic_abstract = 
  let open Ctypes in function
    Ctypes_static.Int8   -> mkAbstract int8_t
  | Ctypes_static.Int16  -> mkAbstract int16_t
  | Ctypes_static.Int32  -> mkAbstract int32_t
  | Ctypes_static.Int64  -> mkAbstract int64_t
  | Ctypes_static.Uint8  -> mkAbstract uint8_t
  | Ctypes_static.Uint16 -> mkAbstract uint16_t
  | Ctypes_static.Uint32 -> mkAbstract uint32_t
  | Ctypes_static.Uint64 -> mkAbstract uint64_t
  | Ctypes_static.Float  -> mkAbstract float
  | Ctypes_static.Double -> mkAbstract double

let mkSigned name = function
  | Ctypes_static.Int8  -> Ctypes_std_views.signed_typedef name ~size:1
  | Ctypes_static.Int16 -> Ctypes_std_views.signed_typedef name ~size:2
  | Ctypes_static.Int32 -> Ctypes_std_views.signed_typedef name ~size:4
  | Ctypes_static.Int64 -> Ctypes_std_views.signed_typedef name ~size:8
  | _ -> assert false

let mkUnsigned name = function
  | Ctypes_static.Uint8  -> Ctypes_std_views.unsigned_typedef name ~size:1
  | Ctypes_static.Uint16 -> Ctypes_std_views.unsigned_typedef name ~size:2
  | Ctypes_static.Uint32 -> Ctypes_std_views.unsigned_typedef name ~size:4
  | Ctypes_static.Uint64 -> Ctypes_std_views.unsigned_typedef name ~size:8
  | _ -> assert false

let mkArithmetic name : _ -> (module Ctypes_std_views.Unsigned_type) =
  let open Ctypes_static in function
  | Uint8 | Uint16 | Uint32 | Uint64 as u ->
    let module U = (val mkUnsigned name u) in (module U)
  | Int8 | Int16 | Int32 | Int64 as u ->
    let module S = (val mkSigned name u) in (module S)
  | _ -> assert false

(* Arithmetic types *)
external typeof_clock_t : unit -> Ctypes_static.arithmetic = "ctypes_typeof_clock_t"
external typeof_dev_t : unit -> Ctypes_static.arithmetic = "ctypes_typeof_dev_t"
external typeof_ino_t : unit -> Ctypes_static.arithmetic = "ctypes_typeof_ino_t"
external typeof_mode_t : unit -> Ctypes_static.arithmetic = "ctypes_typeof_mode_t"
external typeof_nlink_t : unit -> Ctypes_static.arithmetic = "ctypes_typeof_nlink_t"
external typeof_off_t : unit -> Ctypes_static.arithmetic = "ctypes_typeof_off_t"
external typeof_pid_t : unit -> Ctypes_static.arithmetic = "ctypes_typeof_pid_t"
external typeof_ssize_t : unit -> Ctypes_static.arithmetic = "ctypes_typeof_ssize_t"
external typeof_time_t : unit -> Ctypes_static.arithmetic = "ctypes_typeof_time_t"
external typeof_useconds_t : unit -> Ctypes_static.arithmetic = "ctypes_typeof_useconds_t"

module Clock = (val mkArithmetic_abstract (typeof_clock_t ()) : Abstract)
module Dev = (val mkArithmetic "dev_t" (typeof_dev_t ()))
module Ino = (val mkArithmetic "ino_t" (typeof_ino_t ()))
module Mode = (val mkArithmetic "mode_t" (typeof_mode_t ()))
module Nlink = (val mkArithmetic "nlink_t" (typeof_nlink_t ()))
module Off = (val mkSigned "off_t" (typeof_off_t ()))
module Pid = (val mkSigned "pid_t" (typeof_pid_t ()))
module Size =
struct
  type t = Unsigned.size_t
  let t = Ctypes.size_t
end
module Ssize = (val mkSigned "ssize_t" (typeof_ssize_t ()))
module Time = (val mkArithmetic "time_t" (typeof_time_t ()))
module Useconds = (val mkArithmetic_abstract (typeof_useconds_t ()) : Abstract)

type clock_t = Clock.t
type dev_t = Dev.t
type ino_t = Ino.t
type mode_t = Mode.t
type nlink_t = Nlink.t
type off_t = Off.t
type pid_t = Pid.t
type size_t = Size.t
type ssize_t = Ssize.t
type time_t = Time.t
type useconds_t = Useconds.t

let clock_t = Clock.t
let dev_t = Dev.t
let ino_t = Ino.t
let mode_t = Mode.t
let nlink_t = Nlink.t
let off_t = Off.t
let pid_t = Pid.t
let size_t = Size.t
let ssize_t = Ssize.t
let time_t = Time.t
let useconds_t = Useconds.t

(* Non-arithmetic types *)

external sizeof_sigset_t : unit -> int = "ctypes_sizeof_sigset_t"

external alignmentof_sigset_t : unit -> int = "ctypes_alignmentof_sigset_t"

module Sigset = (val mkAbstractSized ~name:"sigset_t" ~size:(sizeof_sigset_t ()) ~alignment:(alignmentof_sigset_t ()) : Abstract)

type sigset_t = Sigset.t

let sigset_t = Sigset.t
