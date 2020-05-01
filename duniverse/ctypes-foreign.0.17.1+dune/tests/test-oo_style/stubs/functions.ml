(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the OO-style tests. *)

open Ctypes

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F

  let cast base p = from_voidp base (to_voidp p)

  (* We'll build part of the hierarchy in C and part in OCaml.

        animal
        ^    ^
        |    |
   chorse   camel
  *)

  (** Create the base class and its method table **)
  type animal and animal_methods

  let animal_methods : animal_methods structure typ = structure "animal methods"
  and animal : animal structure typ = structure "animal"

  (* class layout (vtable pointer, no instance variables) *)
  let animal_vtable = field animal "animal_vtable" (ptr animal_methods)
  let () = seal animal

  (* method table layout (two virtual methods) *)
  let (-:) ty label = field animal_methods label ty
  let say = Foreign.funptr Ctypes.(ptr animal @-> returning string)      -: "say"
  let identify = Foreign.funptr Ctypes.(ptr animal @-> returning string) -: "identify"
  let () = seal animal_methods

  let call_say cinstance =
    !@((getf (!@cinstance) animal_vtable) |-> say) cinstance

  let call_identify cinstance =
    !@((getf (!@cinstance) animal_vtable) |-> identify) cinstance

  (* constructor *)
  class animalc ~cinstance = object
    method say : string = call_say cinstance
    method identify : string = call_identify cinstance
    method cinstance = cinstance
  end

  (** Create a sub class and its method table **)
  type camel and camel_methods
  let camel_methods : camel_methods structure typ = structure "camel methods"
  and camel : camel structure typ = structure "camel"

  (* class layout (vtable pointer, one instance variable) *)
  let (-:) ty label = field camel label ty
  let camel_vtable = ptr camel_methods -: "camel_vtable"
  let nhumps       = int               -: "nhumps"
  let () = seal camel

  (* method table layout (one additional virtual method) *)
  let (-:) ty label = field camel_methods label ty
  let _     = animal_methods                       -: "_"
  let humps = Foreign.funptr Ctypes.(ptr camel @-> returning int) -: "humps"
  let () = seal camel_methods

  let call_humps cinstance =
    !@((getf (!@cinstance) camel_vtable) |-> humps) cinstance

  (* constructor *)
  class camelc ~cinstance = object
    inherit animalc ~cinstance:(cast animal cinstance)
    method humps : int = call_humps cinstance
  end

  let check_name = foreign "check_name"
    (ptr animal @-> string @-> returning int)

    let new_chorse = foreign "new_chorse"
      (int @-> returning (ptr animal))
end
