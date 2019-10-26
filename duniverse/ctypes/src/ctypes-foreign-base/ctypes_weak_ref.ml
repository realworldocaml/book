(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

exception EmptyWeakReference

type 'a t = 'a Weak.t

let empty () = raise EmptyWeakReference
let make v = Weak.(let a = create 1 in set a 0 (Some v); a)
let set r v = Weak.set r 0 (Some v)
let get r = match Weak.get r 0 with Some v -> v | None -> empty ()
let is_empty r = Weak.check r 0
