(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Support for various ABIs. *)

type abi

val aix : abi
val darwin : abi
val eabi : abi
val fastcall : abi
val gcc_sysv : abi
val linux : abi
val linux64 : abi
val linux_soft_float : abi
val ms_cdecl : abi
val n32 : abi
val n32_soft_float : abi
val n64 : abi
val n64_soft_float : abi
val o32 : abi
val o32_soft_float : abi
val osf : abi
val pa32 : abi
val stdcall : abi
val sysv : abi
val thiscall : abi
val unix : abi
val unix64 : abi
val v8 : abi
val v8plus : abi
val v9 : abi
val vfp : abi

val default_abi : abi

val abi_code : abi -> int
