module Generator = Test_generator
module Observer = Test_observer
module Shrinker = Test_shrinker
module Test = Test_test

(* This module contains only a module type. *)
module With_basic_types = Base_quickcheck.With_basic_types

(* This module contains only aliases to values already tested above. *)
module Export = Base_quickcheck.Export
include Export
module Private = Base_quickcheck.Private
