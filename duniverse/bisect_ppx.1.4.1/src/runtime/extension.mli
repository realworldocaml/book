(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



(** Bisect output files extension. *)

val value : string
(** Output file extension. This is [out], except when built as [Meta_bisect] for
    self-instrumentation. Then, it is [out.meta]. *)
