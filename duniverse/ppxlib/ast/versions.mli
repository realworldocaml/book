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

(*$ open Ast_cinaps_helpers $*)

(** {1 Abstracting an OCaml frontend} *)

(** Abstract view of a version of an OCaml Ast *)
module type Ast = sig
  (*$ foreach_module (fun m types ->
      printf "  module %s : sig\n" m;
      List.iter types ~f:(printf "    type %s\n");
      printf "  end\n"
    )
  *)
  module Parsetree : sig
    type structure
    type signature
    type toplevel_phrase
    type core_type
    type expression
    type pattern
    type case
    type type_declaration
    type type_extension
    type extension_constructor
  end
(*$*)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
end

(* Shortcuts for talking about ast types outside of the module language *)

type 'a _types = 'a constraint 'a
  = <
    (*$ foreach_type (fun _ s -> printf "    %-21s : _;\n" s) *)
    structure             : _;
    signature             : _;
    toplevel_phrase       : _;
    core_type             : _;
    expression            : _;
    pattern               : _;
    case                  : _;
    type_declaration      : _;
    type_extension        : _;
    extension_constructor : _;
(*$*)
  >
;;

(** A version of the OCaml frontend packs the ast with type witnesses
    so that equalities can be recovered dynamically. *)
type _ witnesses (*IF_AT_LEAST 406 = private ..*)

(** [migration_info] is an opaque type that is used to generate migration
    functions. *)
type _ migration_info

(** An OCaml frontend versions an Ast, version number and some witnesses for
    conversion. *)
module type OCaml_version = sig

  (** Ast definition for this version *)
  module Ast : Ast

  (* Version number as an integer, 402, 403, 404, ... *)
  val version : int

  (* Version number as a user-friendly string *)
  val string_version : string (* 4.02, 4.03, 4.04, ... *)

  (** Shortcut for talking about Ast types *)
  type types = <
    (*$ foreach_type (fun m s -> printf "    %-21s : Ast.%s.%s;\n" s m s) *)
    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
(*$*)
  > _types

  (** A construtor for recovering type equalities between two arbitrary
      versions. *)
  type _ witnesses += Version : types witnesses

  (** Information used to derive migration functions, see below *)
  val migration_info : types migration_info
end

(** {1 Concrete frontend instances} *)

(*$foreach_version (fun n _ ->
    printf "module OCaml_%d : OCaml_version with module Ast = Astlib.Ast_%d\n"
      n n
  )*)
module OCaml_402 : OCaml_version with module Ast = Astlib.Ast_402
module OCaml_403 : OCaml_version with module Ast = Astlib.Ast_403
module OCaml_404 : OCaml_version with module Ast = Astlib.Ast_404
module OCaml_405 : OCaml_version with module Ast = Astlib.Ast_405
module OCaml_406 : OCaml_version with module Ast = Astlib.Ast_406
module OCaml_407 : OCaml_version with module Ast = Astlib.Ast_407
module OCaml_408 : OCaml_version with module Ast = Astlib.Ast_408
module OCaml_409 : OCaml_version with module Ast = Astlib.Ast_409
module OCaml_410 : OCaml_version with module Ast = Astlib.Ast_410
module OCaml_411 : OCaml_version with module Ast = Astlib.Ast_411
module OCaml_412 : OCaml_version with module Ast = Astlib.Ast_412
module OCaml_413 : OCaml_version with module Ast = Astlib.Ast_413
module OCaml_414 : OCaml_version with module Ast = Astlib.Ast_414
module OCaml_500 : OCaml_version with module Ast = Astlib.Ast_500
(*$*)

(* An alias to the current compiler version *)
module OCaml_current = OCaml_OCAML_VERSION

(* The list of all supported versions *)
val all_versions : (module OCaml_version) list

(** {1 Convenience definitions} *)

(** Module level migration *)
module Convert (A : OCaml_version) (B : OCaml_version) : sig
  (*$ foreach_type (fun m s ->
      let fq = sprintf "%s.%s" m s in
      printf "  val copy_%-21s : A.Ast.%-31s -> B.Ast.%s\n" s fq fq) *)
  val copy_structure             : A.Ast.Parsetree.structure             -> B.Ast.Parsetree.structure
  val copy_signature             : A.Ast.Parsetree.signature             -> B.Ast.Parsetree.signature
  val copy_toplevel_phrase       : A.Ast.Parsetree.toplevel_phrase       -> B.Ast.Parsetree.toplevel_phrase
  val copy_core_type             : A.Ast.Parsetree.core_type             -> B.Ast.Parsetree.core_type
  val copy_expression            : A.Ast.Parsetree.expression            -> B.Ast.Parsetree.expression
  val copy_pattern               : A.Ast.Parsetree.pattern               -> B.Ast.Parsetree.pattern
  val copy_case                  : A.Ast.Parsetree.case                  -> B.Ast.Parsetree.case
  val copy_type_declaration      : A.Ast.Parsetree.type_declaration      -> B.Ast.Parsetree.type_declaration
  val copy_type_extension        : A.Ast.Parsetree.type_extension        -> B.Ast.Parsetree.type_extension
  val copy_extension_constructor : A.Ast.Parsetree.extension_constructor -> B.Ast.Parsetree.extension_constructor
(*$*)
end

(** Helper to find the frontend corresponding to a given magic number *)
module Find_version : sig
  type t = Impl of (module OCaml_version) | Intf of (module OCaml_version) | Unknown

  val from_magic : string -> t
end
