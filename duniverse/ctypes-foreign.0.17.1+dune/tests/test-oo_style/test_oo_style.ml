(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes

[@@@warning "-6-27-37"]

module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (* 
     Establish a hierarchy of "classes", create some "objects" and call some
     "methods".
  *)
  let test_oo_hierarchy _ =
    let module M = struct

      let camel_vtable_singleton = make camel_methods

      let idfn = (fun animal ->
          let n = call_humps (cast camel animal) in
          Printf.sprintf "%d-hump camel" n)

      let humpsfn = (fun camel -> !@(camel |-> nhumps))

      let sayfn = (fun animal -> "humph")

      let () = begin
        let vt = camel_vtable_singleton in
        let base_vt = !@(cast animal_methods (addr vt)) in
        (* say *)
        setf base_vt say sayfn;

        (* identify *)
        setf base_vt identify idfn;

        (* humps *)
        setf vt humps humpsfn;
      end

      let new_camel ~humps =
        let c = make camel in begin
          setf c camel_vtable (addr camel_vtable_singleton);
          setf c nhumps humps
        end;
        new camelc ~cinstance:(addr c)

      let () =
        let c = new_camel ~humps:3 in begin
          (* Test that we can call a virtual method in an OCaml-created subclass
             from C *)
          assert_equal 1 (check_name (cast animal c#cinstance) "3-hump camel");

          (* Test that we can call virtual methods in an OCaml-created subclass
             from OCaml *)
          assert_equal c#identify "3-hump camel";
          assert_equal c#say "humph";
          assert_equal c#humps 3;
        end

      let _ = Ctypes_memory_stubs.use_value (idfn, humpsfn, sayfn)

      (* Test that we can call a virtual method in a C-created subclass from
         OCaml *)
      type colour = White | Red | Black | Pale
      let colour_num = function
      White -> 0 | Red -> 1 | Black -> 2 | Pale -> 3

      class chorse ~colour =
      object
        inherit animalc (new_chorse(colour_num colour))
      end

      let () =
        let red_horse = new chorse ~colour:Red
        and pale_horse = new chorse ~colour:Pale in begin
          assert_equal "red horse" red_horse#identify;
          assert_equal "pale horse" pale_horse#identify;
          assert_equal "neigh" pale_horse#say;
        end
    end in ()
end

module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)


let suite = "OO-style tests" >:::
  ["OO style (foreign)"
    >:: Foreign_tests.test_oo_hierarchy;

   "OO style (stubs)"
    >:: Stub_tests.test_oo_hierarchy;
  ]


let _ =
  run_test_tt_main suite
