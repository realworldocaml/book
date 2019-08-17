(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* For internal use only, and really only for use with Closure_properties_base.
   A mutex for synchronizing between the GC (i.e. finalisers) and the single
   mutator thread.  Provides very few guarantees.  Since the program is
   single-threaded, there is no waiting; locking either succeeds or fails
   immediately.
*)

exception MutexError of string

type t = { mutable locked: bool }

let create () = { locked = false }

(* the only allocation below is exception raising *) 

let lock m =
  if m.locked then raise (MutexError "Locking locked mutex")
  else m.locked <- true

let try_lock m = 
  if m.locked then false
  else (m.locked <- true; true)

let unlock m = 
  if not m.locked then raise (MutexError "Unlocking unlocked mutex")
  else m.locked <- false
