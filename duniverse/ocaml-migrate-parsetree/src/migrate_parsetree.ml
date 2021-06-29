(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                   Jérémie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*$ open Cinaps_helpers
    open Printf
$*)

(* Shared definitions.
   Mostly errors about features missing in older versions. *)
module Def = Migrate_parsetree_def

(* Copy of OCaml parsetrees *)
(*$foreach_version (fun suffix _ ->
    printf "module Ast_%s = Ast_%s\n" suffix suffix
  )*)
module Ast_402 = Ast_402
module Ast_403 = Ast_403
module Ast_404 = Ast_404
module Ast_405 = Ast_405
module Ast_406 = Ast_406
module Ast_407 = Ast_407
module Ast_408 = Ast_408
module Ast_409 = Ast_409
module Ast_410 = Ast_410
module Ast_411 = Ast_411
module Ast_412 = Ast_412
(*$*)

(* Manual migration between versions *)
(*$foreach_version_pair (fun x y ->
    printf "module Migrate_%s_%s = Migrate_%s_%s\n" x y x y;
    printf "module Migrate_%s_%s = Migrate_%s_%s\n" y x y x;
  )*)
module Migrate_402_403 = Migrate_402_403
module Migrate_403_402 = Migrate_403_402
module Migrate_403_404 = Migrate_403_404
module Migrate_404_403 = Migrate_404_403
module Migrate_404_405 = Migrate_404_405
module Migrate_405_404 = Migrate_405_404
module Migrate_405_406 = Migrate_405_406
module Migrate_406_405 = Migrate_406_405
module Migrate_406_407 = Migrate_406_407
module Migrate_407_406 = Migrate_407_406
module Migrate_407_408 = Migrate_407_408
module Migrate_408_407 = Migrate_408_407
module Migrate_408_409 = Migrate_408_409
module Migrate_409_408 = Migrate_409_408
module Migrate_409_410 = Migrate_409_410
module Migrate_410_409 = Migrate_410_409
module Migrate_410_411 = Migrate_410_411
module Migrate_411_410 = Migrate_411_410
module Migrate_411_412 = Migrate_411_412
module Migrate_412_411 = Migrate_412_411
(*$*)

(* Aliases for compiler-libs modules that might be shadowed *)
module Compiler_libs = struct
  module Location = Location
  module Longident = Longident

  module type Asttypes = module type of struct include Asttypes end
  module rec Asttypes : Asttypes = Asttypes

  module type Parsetree = module type of struct include Parsetree end
  module rec Parsetree : Parsetree = Parsetree

  module Docstrings = Docstrings
  module Ast_helper = Ast_helper
  module Ast_mapper = Ast_mapper
end
