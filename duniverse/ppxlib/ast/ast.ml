(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Definition of the OCaml AST *)

open Import

(* This file is obtained by:

   - copying a subset of the corresponding ast_xxx.ml file from Astlib
   (sub-modules Asttypes and Parsetree)
   - adding the type definitions for position, location, loc and longident
   - flattening all the modules
   - removing Asttypes.constant (unused and conflicts with Parsetree.constant)
   - renaming a few types:
   - - Location.t -> location
   - - Longident.t -> longident
   - adding a type longident_loc = longident loc and replacing all the occurrences of the
   latter by the former. This is so that we can override iteration an the level of a
   longident loc
   - adding a type cases = case list
   - replacing all the (*IF_CURRENT = Foo.bar*) by: = Foo.bar
   - removing the extra values at the end of the file
   - replacing app [type ...] by [and ...] to make everything one recursive block
   - adding [@@deriving_inline traverse][@@@end] at the end

   To update it to a newer OCaml version, create a new module with the above from the
   latest compiler and add the following module definitions and opens to get it to
   compile:
   [{
    module Ast = Versions.OCaml_4xx
    open Ast.Ast
    module Location   = Ocaml_common.Location
    module Longident = Ocaml_common.Longident
   }]

   Once you have generated the inlined derived traversal classes by running
   [{ dune build @lint }] you can replace the above mentioned module definitions by a
   [open Import] and update [Import] so that the [Js] module points to
   [Versions.OCaml_4xx].
*)

(* Source code locations (ranges of positions), used in parsetree. *)

type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

and location = Location.t = {
  loc_start : position;
  loc_end : position;
  loc_ghost : bool;
}

and location_stack = location list

(* Note on the use of Lexing.position in this module.
   If [pos_fname = ""], then use [!input_name] instead.
   If [pos_lnum = -1], then [pos_bol = 0]. Use [pos_cnum] and
   re-parse the file to get the line and character numbers.
   Else all fields are correct.
*)
and 'a loc = 'a Location.loc = { txt : 'a; loc : location }

(* Long identifiers, used in parsetree. *)
and longident = Longident.t =
  | Lident of string
  | Ldot of longident * string
  | Lapply of longident * longident

and longident_loc = longident loc

(** Auxiliary AST types used by parsetree and typedtree. *)

and rec_flag = Asttypes.rec_flag = Nonrecursive | Recursive

and direction_flag = Asttypes.direction_flag = Upto | Downto

(* Order matters, used in polymorphic comparison *)
and private_flag = Asttypes.private_flag = Private | Public

and mutable_flag = Asttypes.mutable_flag = Immutable | Mutable

and virtual_flag = Asttypes.virtual_flag = Virtual | Concrete

and override_flag = Asttypes.override_flag = Override | Fresh

and closed_flag = Asttypes.closed_flag = Closed | Open

and label = string

and arg_label = Asttypes.arg_label =
  | Nolabel
  | Labelled of string (*  label:T -> ... *)
  | Optional of string
(* ?label:T -> ... *)

and variance = Asttypes.variance = Covariant | Contravariant | NoVariance

and injectivity = Asttypes.injectivity = Injective | NoInjectivity

(** Abstract syntax tree produced by parsing *)

and constant = Parsetree.constant =
  | Pconst_integer of string * char option
  (* 3 3l 3L 3n

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes except 'l', 'L' and 'n' are rejected by the typechecker
  *)
  | Pconst_char of char
  (* 'c' *)
  | Pconst_string of string * location * string option
  (* "constant"
     {delim|other constant|delim}

     The location span the content of the string, without the delimiters.
  *)
  | Pconst_float of string * char option
(* 3.4 2e5 1.4e-4

   Suffixes [g-z][G-Z] are accepted by the parser.
   Suffixes are rejected by the typechecker.
*)

(** {1 Extension points} *)

and attribute = Parsetree.attribute = {
  attr_name : string loc;
  attr_payload : payload;
  attr_loc : location;
}
(* [@id ARG]
   [@@id ARG]

   Metadata containers passed around within the AST.
   The compiler ignores unknown attributes.
*)

and extension = string loc * payload
(* [%id ARG]
   [%%id ARG]

   Sub-language placeholder -- rejected by the typechecker.
*)

and attributes = attribute list

and payload = Parsetree.payload =
  | PStr of structure
  | PSig of signature (* : SIG *)
  | PTyp of core_type (* : T *)
  | PPat of pattern * expression option
(* ? P  or  ? P when E *)

(* Type expressions *)

(** {1 Core language} *)

and core_type = Parsetree.core_type = {
  ptyp_desc : core_type_desc;
  ptyp_loc : location;
  ptyp_loc_stack : location_stack;
  ptyp_attributes : attributes; (* ... [@id1] [@id2] *)
}

and core_type_desc = Parsetree.core_type_desc =
  | Ptyp_any
  (*  _ *)
  | Ptyp_var of string
  (* 'a *)
  | Ptyp_arrow of arg_label * core_type * core_type
  (* T1 -> T2       Simple
     ~l:T1 -> T2    Labelled
     ?l:T1 -> T2    Optional
  *)
  | Ptyp_tuple of core_type list
  (* T1 * ... * Tn

     Invariant: n >= 2
  *)
  | Ptyp_constr of longident_loc * core_type list
  (* tconstr
     T tconstr
     (T1, ..., Tn) tconstr
  *)
  | Ptyp_object of object_field list * closed_flag
  (* < l1:T1; ...; ln:Tn >     (flag = Closed)
     < l1:T1; ...; ln:Tn; .. > (flag = Open)
  *)
  | Ptyp_class of longident_loc * core_type list
  (* #tconstr
     T #tconstr
     (T1, ..., Tn) #tconstr
  *)
  | Ptyp_alias of core_type * string
  (* T as 'a *)
  | Ptyp_variant of row_field list * closed_flag * label list option
  (* [ `A|`B ]         (flag = Closed; labels = None)
     [> `A|`B ]        (flag = Open;   labels = None)
     [< `A|`B ]        (flag = Closed; labels = Some [])
     [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
  *)
  | Ptyp_poly of string loc list * core_type (* 'a1 ... 'an. T

                                                Can only appear in the following context:

                                                - As the core_type of a Ppat_constraint node corresponding
                                                to a constraint on a let-binding: let x : 'a1 ... 'an. T
                                                = e ...

                                                - Under Cfk_virtual for methods (not values).

                                                - As the core_type of a Pctf_method node.

                                                - As the core_type of a Pexp_poly node.

                                                - As the pld_type field of a label_declaration.

                                                - As a core_type of a Ptyp_object node.
                                             *)
  | Ptyp_package of package_type
  (* (module S) *)
  | Ptyp_extension of extension
(* [%id] *)

and package_type = longident_loc * (longident_loc * core_type) list
(*
   (module S)
   (module S with type t1 = T1 and ... and tn = Tn)
*)

and row_field = Parsetree.row_field = {
  prf_desc : row_field_desc;
  prf_loc : location;
  prf_attributes : attributes;
}

and row_field_desc = Parsetree.row_field_desc =
  | Rtag of label loc * bool * core_type list
  (* [`A]                   ( true,  [] )
     [`A of T]              ( false, [T] )
     [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
     [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

     - The 2nd field is true if the tag contains a
     constant (empty) constructor.
     - '&' occurs when several types are used for the same constructor
     (see 4.2 in the manual)

     - TODO: switch to a record representation, and keep location
  *)
  | Rinherit of core_type
(* [ | t ] *)

and object_field = Parsetree.object_field = {
  pof_desc : object_field_desc;
  pof_loc : location;
  pof_attributes : attributes;
}

and object_field_desc = Parsetree.object_field_desc =
  | Otag of label loc * core_type
  | Oinherit of core_type

(* Patterns *)
and pattern = Parsetree.pattern = {
  ppat_desc : pattern_desc;
  ppat_loc : location;
  ppat_loc_stack : location_stack;
  ppat_attributes : attributes; (* ... [@id1] [@id2] *)
}

and pattern_desc = Parsetree.pattern_desc =
  | Ppat_any
  (* _ *)
  | Ppat_var of string loc
  (* x *)
  | Ppat_alias of pattern * string loc
  (* P as 'a *)
  | Ppat_constant of constant
  (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Ppat_interval of constant * constant
  (* 'a'..'z'

     Other forms of interval are recognized by the parser
     but rejected by the type-checker. *)
  | Ppat_tuple of pattern list
  (* (P1, ..., Pn)

     Invariant: n >= 2
  *)
  | Ppat_construct of longident_loc * pattern option
  (* C                None
     C P              Some P
     C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
  *)
  | Ppat_variant of label * pattern option
  (* `A             (None)
     `A P           (Some P)
  *)
  | Ppat_record of (longident_loc * pattern) list * closed_flag
  (* { l1=P1; ...; ln=Pn }     (flag = Closed)
     { l1=P1; ...; ln=Pn; _}   (flag = Open)

     Invariant: n > 0
  *)
  | Ppat_array of pattern list
  (* [| P1; ...; Pn |] *)
  | Ppat_or of pattern * pattern
  (* P1 | P2 *)
  | Ppat_constraint of pattern * core_type
  (* (P : T) *)
  | Ppat_type of longident_loc
  (* #tconst *)
  | Ppat_lazy of pattern
  (* lazy P *)
  | Ppat_unpack of string option loc
  (* (module P)
     Note: (module P : S) is represented as
     Ppat_constraint(Ppat_unpack, Ptyp_package)
  *)
  | Ppat_exception of pattern
  (* exception P *)
  | Ppat_extension of extension
  (* [%id] *)
  | Ppat_open of longident_loc * pattern
(* M.(P) *)

(* Value expressions *)
and expression = Parsetree.expression = {
  pexp_desc : expression_desc;
  pexp_loc : location;
  pexp_loc_stack : location_stack;
  pexp_attributes : attributes; (* ... [@id1] [@id2] *)
}

and expression_desc = Parsetree.expression_desc =
  | Pexp_ident of longident_loc
  (* x
     M.x
  *)
  | Pexp_constant of constant
  (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Pexp_let of rec_flag * value_binding list * expression
  (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
     let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
  *)
  | Pexp_function of cases
  (* function P1 -> E1 | ... | Pn -> En *)
  | Pexp_fun of arg_label * expression option * pattern * expression
  (* fun P -> E1                          (Simple, None)
     fun ~l:P -> E1                       (Labelled l, None)
     fun ?l:P -> E1                       (Optional l, None)
     fun ?l:(P = E0) -> E1                (Optional l, Some E0)

     Notes:
     - If E0 is provided, only Optional is allowed.
     - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
     - "let f P = E" is represented using Pexp_fun.
  *)
  | Pexp_apply of expression * (arg_label * expression) list
  (* E0 ~l1:E1 ... ~ln:En
     li can be empty (non labeled argument) or start with '?'
     (optional argument).

     Invariant: n > 0
  *)
  | Pexp_match of expression * cases
  (* match E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_try of expression * cases
  (* try E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_tuple of expression list
  (* (E1, ..., En)

     Invariant: n >= 2
  *)
  | Pexp_construct of longident_loc * expression option
  (* C                None
     C E              Some E
     C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
  *)
  | Pexp_variant of label * expression option
  (* `A             (None)
     `A E           (Some E)
  *)
  | Pexp_record of (longident_loc * expression) list * expression option
  (* { l1=P1; ...; ln=Pn }     (None)
     { E0 with l1=P1; ...; ln=Pn }   (Some E0)

     Invariant: n > 0
  *)
  | Pexp_field of expression * longident_loc
  (* E.l *)
  | Pexp_setfield of expression * longident_loc * expression
  (* E1.l <- E2 *)
  | Pexp_array of expression list
  (* [| E1; ...; En |] *)
  | Pexp_ifthenelse of expression * expression * expression option
  (* if E1 then E2 else E3 *)
  | Pexp_sequence of expression * expression
  (* E1; E2 *)
  | Pexp_while of expression * expression
  (* while E1 do E2 done *)
  | Pexp_for of pattern * expression * expression * direction_flag * expression
  (* for i = E1 to E2 do E3 done      (flag = Upto)
     for i = E1 downto E2 do E3 done  (flag = Downto)
  *)
  | Pexp_constraint of expression * core_type
  (* (E : T) *)
  | Pexp_coerce of expression * core_type option * core_type
  (* (E :> T)        (None, T)
     (E : T0 :> T)   (Some T0, T)
  *)
  | Pexp_send of expression * label loc
  (*  E # m *)
  | Pexp_new of longident_loc
  (* new M.c *)
  | Pexp_setinstvar of label loc * expression
  (* x <- 2 *)
  | Pexp_override of (label loc * expression) list
  (* {< x1 = E1; ...; Xn = En >} *)
  | Pexp_letmodule of string option loc * module_expr * expression
  (* let module M = ME in E *)
  | Pexp_letexception of extension_constructor * expression
  (* let exception C in E *)
  | Pexp_assert of expression
  (* assert E
     Note: "assert false" is treated in a special way by the
     type-checker. *)
  | Pexp_lazy of expression
  (* lazy E *)
  | Pexp_poly of expression * core_type option
  (* Used for method bodies.

     Can only be used as the expression under Cfk_concrete
     for methods (not values). *)
  | Pexp_object of class_structure
  (* object ... end *)
  | Pexp_newtype of string loc * expression
  (* fun (type t) -> E *)
  | Pexp_pack of module_expr
  (* (module ME)

     (module ME : S) is represented as
     Pexp_constraint(Pexp_pack, Ptyp_package S) *)
  | Pexp_open of open_declaration * expression
  (* M.(E)
     let open M in E
     let! open M in E *)
  | Pexp_letop of letop
  (* let* P = E in E
     let* P = E and* P = E in E *)
  | Pexp_extension of extension
  (* [%id] *)
  | Pexp_unreachable
(* . *)

and case = Parsetree.case = {
  (* (P -> E) or (P when E0 -> E) *)
  pc_lhs : pattern;
  pc_guard : expression option;
  pc_rhs : expression;
}

and letop = Parsetree.letop = {
  let_ : binding_op;
  ands : binding_op list;
  body : expression;
}

and binding_op = Parsetree.binding_op = {
  pbop_op : string loc;
  pbop_pat : pattern;
  pbop_exp : expression;
  pbop_loc : location;
}

(* Value descriptions *)
and value_description = Parsetree.value_description = {
  pval_name : string loc;
  pval_type : core_type;
  pval_prim : string list;
  pval_attributes : attributes;
  (* ... [@@id1] [@@id2] *)
  pval_loc : location;
}

(*
   val x: T                            (prim = [])
   external x: T = "s1" ... "sn"       (prim = ["s1";..."sn"])
*)

(* Type declarations *)
and type_declaration = Parsetree.type_declaration = {
  ptype_name : string loc;
  ptype_params : (core_type * (variance * injectivity)) list;
  (* ('a1,...'an) t; None represents  _*)
  ptype_cstrs : (core_type * core_type * location) list;
  (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
  ptype_kind : type_kind;
  ptype_private : private_flag;
  (* = private ... *)
  ptype_manifest : core_type option;
  (* = T *)
  ptype_attributes : attributes;
  (* ... [@@id1] [@@id2] *)
  ptype_loc : location;
}

(*
   type t                     (abstract, no manifest)
   type t = T0                (abstract, manifest=T0)
   type t = C of T | ...      (variant,  no manifest)
   type t = T0 = C of T | ... (variant,  manifest=T0)
   type t = {l: T; ...}       (record,   no manifest)
   type t = T0 = {l : T; ...} (record,   manifest=T0)
   type t = ..                (open,     no manifest)
*)
and type_kind = Parsetree.type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
  | Ptype_record of label_declaration list
  (* Invariant: non-empty list *)
  | Ptype_open

and label_declaration = Parsetree.label_declaration = {
  pld_name : string loc;
  pld_mutable : mutable_flag;
  pld_type : core_type;
  pld_loc : location;
  pld_attributes : attributes; (* l : T [@id1] [@id2] *)
}

(* { ...; l: T; ... }            (mutable=Immutable)
   { ...; mutable l: T; ... }    (mutable=Mutable)

   Note: T can be a Ptyp_poly.
*)
and constructor_declaration = Parsetree.constructor_declaration = {
  pcd_name : string loc;
  pcd_args : constructor_arguments;
  pcd_res : core_type option;
  pcd_loc : location;
  pcd_attributes : attributes; (* C of ... [@id1] [@id2] *)
}

and constructor_arguments = Parsetree.constructor_arguments =
  | Pcstr_tuple of core_type list
  | Pcstr_record of label_declaration list

(*
   | C of T1 * ... * Tn     (res = None,    args = Pcstr_tuple [])
   | C: T0                  (res = Some T0, args = [])
   | C: T1 * ... * Tn -> T0 (res = Some T0, args = Pcstr_tuple)
   | C of {...}             (res = None,    args = Pcstr_record)
   | C: {...} -> T0         (res = Some T0, args = Pcstr_record)
   | C of {...} as t        (res = None,    args = Pcstr_record)
*)
and type_extension = Parsetree.type_extension = {
  ptyext_path : longident_loc;
  ptyext_params : (core_type * (variance * injectivity)) list;
  ptyext_constructors : extension_constructor list;
  ptyext_private : private_flag;
  ptyext_loc : location;
  ptyext_attributes : attributes; (* ... [@@id1] [@@id2] *)
}
(*
   type t += ...
*)

and extension_constructor = Parsetree.extension_constructor = {
  pext_name : string loc;
  pext_kind : extension_constructor_kind;
  pext_loc : location;
  pext_attributes : attributes; (* C of ... [@id1] [@id2] *)
}

and type_exception = Parsetree.type_exception = {
  ptyexn_constructor : extension_constructor;
  ptyexn_loc : location;
  ptyexn_attributes : attributes;
}

and extension_constructor_kind = Parsetree.extension_constructor_kind =
  | Pext_decl of constructor_arguments * core_type option
  (*
     | C of T1 * ... * Tn     ([T1; ...; Tn], None)
     | C: T0                  ([], Some T0)
     | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
  *)
  | Pext_rebind of longident_loc
(*
     | C = D
  *)

(* Type expressions for the class language *)

(** {1 Class language} *)

and class_type = Parsetree.class_type = {
  pcty_desc : class_type_desc;
  pcty_loc : location;
  pcty_attributes : attributes; (* ... [@id1] [@id2] *)
}

and class_type_desc = Parsetree.class_type_desc =
  | Pcty_constr of longident_loc * core_type list
  (* c
     ['a1, ..., 'an] c *)
  | Pcty_signature of class_signature
  (* object ... end *)
  | Pcty_arrow of arg_label * core_type * class_type
  (* T -> CT       Simple
     ~l:T -> CT    Labelled l
     ?l:T -> CT    Optional l
  *)
  | Pcty_extension of extension
  (* [%id] *)
  | Pcty_open of open_description * class_type
(* let open M in CT *)

and class_signature = Parsetree.class_signature = {
  pcsig_self : core_type;
  pcsig_fields : class_type_field list;
}
(* object('selfpat) ... end
   object ... end             (self = Ptyp_any)
*)

and class_type_field = Parsetree.class_type_field = {
  pctf_desc : class_type_field_desc;
  pctf_loc : location;
  pctf_attributes : attributes; (* ... [@@id1] [@@id2] *)
}

and class_type_field_desc = Parsetree.class_type_field_desc =
  | Pctf_inherit of class_type
  (* inherit CT *)
  | Pctf_val of (label loc * mutable_flag * virtual_flag * core_type)
  (* val x: T *)
  | Pctf_method of (label loc * private_flag * virtual_flag * core_type)
  (* method x: T

     Note: T can be a Ptyp_poly.
  *)
  | Pctf_constraint of (core_type * core_type)
  (* constraint T1 = T2 *)
  | Pctf_attribute of attribute
  (* [@@@id] *)
  | Pctf_extension of extension
(* [%%id] *)

and 'a class_infos = 'a Parsetree.class_infos = {
  pci_virt : virtual_flag;
  pci_params : (core_type * (variance * injectivity)) list;
  pci_name : string loc;
  pci_expr : 'a;
  pci_loc : location;
  pci_attributes : attributes; (* ... [@@id1] [@@id2] *)
}
(* class c = ...
   class ['a1,...,'an] c = ...
   class virtual c = ...

   Also used for "class type" declaration.
*)

and class_description = class_type class_infos

and class_type_declaration = class_type class_infos

(* Value expressions for the class language *)
and class_expr = Parsetree.class_expr = {
  pcl_desc : class_expr_desc;
  pcl_loc : location;
  pcl_attributes : attributes; (* ... [@id1] [@id2] *)
}

and class_expr_desc = Parsetree.class_expr_desc =
  | Pcl_constr of longident_loc * core_type list
  (* c
     ['a1, ..., 'an] c *)
  | Pcl_structure of class_structure
  (* object ... end *)
  | Pcl_fun of arg_label * expression option * pattern * class_expr
  (* fun P -> CE                          (Simple, None)
     fun ~l:P -> CE                       (Labelled l, None)
     fun ?l:P -> CE                       (Optional l, None)
     fun ?l:(P = E0) -> CE                (Optional l, Some E0)
  *)
  | Pcl_apply of class_expr * (arg_label * expression) list
  (* CE ~l1:E1 ... ~ln:En
     li can be empty (non labeled argument) or start with '?'
     (optional argument).

     Invariant: n > 0
  *)
  | Pcl_let of rec_flag * value_binding list * class_expr
  (* let P1 = E1 and ... and Pn = EN in CE      (flag = Nonrecursive)
     let rec P1 = E1 and ... and Pn = EN in CE  (flag = Recursive)
  *)
  | Pcl_constraint of class_expr * class_type
  (* (CE : CT) *)
  | Pcl_extension of extension
  (* [%id] *)
  | Pcl_open of open_description * class_expr
(* let open M in CE *)

and class_structure = Parsetree.class_structure = {
  pcstr_self : pattern;
  pcstr_fields : class_field list;
}
(* object(selfpat) ... end
   object ... end           (self = Ppat_any)
*)

and class_field = Parsetree.class_field = {
  pcf_desc : class_field_desc;
  pcf_loc : location;
  pcf_attributes : attributes; (* ... [@@id1] [@@id2] *)
}

and class_field_desc = Parsetree.class_field_desc =
  | Pcf_inherit of override_flag * class_expr * string loc option
  (* inherit CE
     inherit CE as x
     inherit! CE
     inherit! CE as x
  *)
  | Pcf_val of (label loc * mutable_flag * class_field_kind)
  (* val x = E
     val virtual x: T
  *)
  | Pcf_method of (label loc * private_flag * class_field_kind)
  (* method x = E            (E can be a Pexp_poly)
     method virtual x: T     (T can be a Ptyp_poly)
  *)
  | Pcf_constraint of (core_type * core_type)
  (* constraint T1 = T2 *)
  | Pcf_initializer of expression
  (* initializer E *)
  | Pcf_attribute of attribute
  (* [@@@id] *)
  | Pcf_extension of extension
(* [%%id] *)

and class_field_kind = Parsetree.class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag * expression

and class_declaration = class_expr class_infos
(* Type expressions for the module language *)

(** {1 Module language} *)

and module_type = Parsetree.module_type = {
  pmty_desc : module_type_desc;
  pmty_loc : location;
  pmty_attributes : attributes; (* ... [@id1] [@id2] *)
}

and module_type_desc = Parsetree.module_type_desc =
  | Pmty_ident of longident_loc
  (* S *)
  | Pmty_signature of signature
  (* sig ... end *)
  | Pmty_functor of functor_parameter * module_type
  (* functor(X : MT1) -> MT2 *)
  | Pmty_with of module_type * with_constraint list
  (* MT with ... *)
  | Pmty_typeof of module_expr
  (* module type of ME *)
  | Pmty_extension of extension
  (* [%id] *)
  | Pmty_alias of longident_loc
(* (module M) *)

and functor_parameter = Parsetree.functor_parameter =
  | Unit
  (* () *)
  | Named of string option loc * module_type
(* (X : MT)          Some X, MT
   (_ : MT)          None, MT *)

and signature = signature_item list

and signature_item = Parsetree.signature_item = {
  psig_desc : signature_item_desc;
  psig_loc : location;
}

and signature_item_desc = Parsetree.signature_item_desc =
  | Psig_value of value_description
  (*
     val x: T
     external x: T = "s1" ... "sn"
  *)
  | Psig_type of rec_flag * type_declaration list
  (* type t1 = ... and ... and tn = ... *)
  | Psig_typesubst of type_declaration list
  (* type t1 := ... and ... and tn := ...  *)
  | Psig_typext of type_extension
  (* type t1 += ... *)
  | Psig_exception of type_exception
  (* exception C of T *)
  | Psig_module of module_declaration
  (* module X : MT *)
  | Psig_modsubst of module_substitution
  (* module X := M *)
  | Psig_recmodule of module_declaration list
  (* module rec X1 : MT1 and ... and Xn : MTn *)
  | Psig_modtype of module_type_declaration
  (* module type S = MT
     module type S *)
  | Psig_open of open_description
  (* open X *)
  | Psig_include of include_description
  (* include MT *)
  | Psig_class of class_description list
  (* class c1 : ... and ... and cn : ... *)
  | Psig_class_type of class_type_declaration list
  (* class type ct1 = ... and ... and ctn = ... *)
  | Psig_attribute of attribute
  (* [@@@id] *)
  | Psig_extension of extension * attributes
(* [%%id] *)

and module_declaration = Parsetree.module_declaration = {
  pmd_name : string option loc;
  pmd_type : module_type;
  pmd_attributes : attributes;
  (* ... [@@id1] [@@id2] *)
  pmd_loc : location;
}
(* S : MT *)

and module_substitution = Parsetree.module_substitution = {
  pms_name : string loc;
  pms_manifest : longident_loc;
  pms_attributes : attributes;
  pms_loc : location;
}

and module_type_declaration = Parsetree.module_type_declaration = {
  pmtd_name : string loc;
  pmtd_type : module_type option;
  pmtd_attributes : attributes;
  (* ... [@@id1] [@@id2] *)
  pmtd_loc : location;
}
(* S = MT
   S       (abstract module type declaration, pmtd_type = None)
*)

and 'a open_infos = 'a Parsetree.open_infos = {
  popen_expr : 'a;
  popen_override : override_flag;
  popen_loc : location;
  popen_attributes : attributes;
}

and open_description = longident_loc open_infos
(* open! X - popen_override = Override (silences the 'used identifier
   shadowing' warning)
   open  X - popen_override = Fresh
*)

and open_declaration = module_expr open_infos

and 'a include_infos = 'a Parsetree.include_infos = {
  pincl_mod : 'a;
  pincl_loc : location;
  pincl_attributes : attributes;
}

and include_description = module_type include_infos
(* include MT *)

and include_declaration = module_expr include_infos
(* include ME *)

and with_constraint = Parsetree.with_constraint =
  | Pwith_type of longident_loc * type_declaration
  (* with type X.t = ...

     Note: the last component of the longident must match
     the name of the type_declaration. *)
  | Pwith_module of longident_loc * longident_loc
  (* with module X.Y = Z *)
  | Pwith_typesubst of longident_loc * type_declaration
  (* with type X.t := ..., same format as [Pwith_type] *)
  | Pwith_modsubst of longident_loc * longident_loc
(* with module X.Y := Z *)

(* Value expressions for the module language *)
and module_expr = Parsetree.module_expr = {
  pmod_desc : module_expr_desc;
  pmod_loc : location;
  pmod_attributes : attributes; (* ... [@id1] [@id2] *)
}

and module_expr_desc = Parsetree.module_expr_desc =
  | Pmod_ident of longident_loc
  (* X *)
  | Pmod_structure of structure
  (* struct ... end *)
  | Pmod_functor of functor_parameter * module_expr
  (* functor(X : MT1) -> ME *)
  | Pmod_apply of module_expr * module_expr
  (* ME1(ME2) *)
  | Pmod_constraint of module_expr * module_type
  (* (ME : MT) *)
  | Pmod_unpack of expression
  (* (val E) *)
  | Pmod_extension of extension
(* [%id] *)

and structure = structure_item list

and structure_item = Parsetree.structure_item = {
  pstr_desc : structure_item_desc;
  pstr_loc : location;
}

and structure_item_desc = Parsetree.structure_item_desc =
  | Pstr_eval of expression * attributes
  (* E *)
  | Pstr_value of rec_flag * value_binding list
  (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
     let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
  *)
  | Pstr_primitive of value_description
  (* val x: T
     external x: T = "s1" ... "sn" *)
  | Pstr_type of rec_flag * type_declaration list
  (* type t1 = ... and ... and tn = ... *)
  | Pstr_typext of type_extension
  (* type t1 += ... *)
  | Pstr_exception of type_exception
  (* exception C of T
     exception C = M.X *)
  | Pstr_module of module_binding
  (* module X = ME *)
  | Pstr_recmodule of module_binding list
  (* module rec X1 = ME1 and ... and Xn = MEn *)
  | Pstr_modtype of module_type_declaration
  (* module type S = MT *)
  | Pstr_open of open_declaration
  (* open X *)
  | Pstr_class of class_declaration list
  (* class c1 = ... and ... and cn = ... *)
  | Pstr_class_type of class_type_declaration list
  (* class type ct1 = ... and ... and ctn = ... *)
  | Pstr_include of include_declaration
  (* include ME *)
  | Pstr_attribute of attribute
  (* [@@@id] *)
  | Pstr_extension of extension * attributes
(* [%%id] *)

and value_binding = Parsetree.value_binding = {
  pvb_pat : pattern;
  pvb_expr : expression;
  pvb_attributes : attributes;
  pvb_loc : location;
}

and module_binding = Parsetree.module_binding = {
  pmb_name : string option loc;
  pmb_expr : module_expr;
  pmb_attributes : attributes;
  pmb_loc : location;
}
(* X = ME *)

(* Toplevel phrases *)

(** {1 Toplevel} *)

and toplevel_phrase = Parsetree.toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of toplevel_directive
(* #use, #load ... *)

and toplevel_directive = Parsetree.toplevel_directive = {
  pdir_name : string loc;
  pdir_arg : directive_argument option;
  pdir_loc : location;
}

and directive_argument = Parsetree.directive_argument = {
  pdira_desc : directive_argument_desc;
  pdira_loc : location;
}

and directive_argument_desc = Parsetree.directive_argument_desc =
  | Pdir_string of string
  | Pdir_int of string * char option
  | Pdir_ident of longident
  | Pdir_bool of bool

and cases = case list [@@deriving_inline traverse]

class virtual map =
  object (self)
    method virtual bool : bool -> bool

    method virtual char : char -> char

    method virtual int : int -> int

    method virtual list : 'a. ('a -> 'a) -> 'a list -> 'a list

    method virtual option : 'a. ('a -> 'a) -> 'a option -> 'a option

    method virtual string : string -> string

    method position : position -> position =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
        let pos_fname = self#string pos_fname in
        let pos_lnum = self#int pos_lnum in
        let pos_bol = self#int pos_bol in
        let pos_cnum = self#int pos_cnum in
        { pos_fname; pos_lnum; pos_bol; pos_cnum }

    method location : location -> location =
      fun { loc_start; loc_end; loc_ghost } ->
        let loc_start = self#position loc_start in
        let loc_end = self#position loc_end in
        let loc_ghost = self#bool loc_ghost in
        { loc_start; loc_end; loc_ghost }

    method location_stack : location_stack -> location_stack =
      self#list self#location

    method loc : 'a. ('a -> 'a) -> 'a loc -> 'a loc =
      fun _a { txt; loc } ->
        let txt = _a txt in
        let loc = self#location loc in
        { txt; loc }

    method longident : longident -> longident =
      fun x ->
        match x with
        | Lident a ->
            let a = self#string a in
            Lident a
        | Ldot (a, b) ->
            let a = self#longident a in
            let b = self#string b in
            Ldot (a, b)
        | Lapply (a, b) ->
            let a = self#longident a in
            let b = self#longident b in
            Lapply (a, b)

    method longident_loc : longident_loc -> longident_loc =
      self#loc self#longident

    method rec_flag : rec_flag -> rec_flag = fun x -> x

    method direction_flag : direction_flag -> direction_flag = fun x -> x

    method private_flag : private_flag -> private_flag = fun x -> x

    method mutable_flag : mutable_flag -> mutable_flag = fun x -> x

    method virtual_flag : virtual_flag -> virtual_flag = fun x -> x

    method override_flag : override_flag -> override_flag = fun x -> x

    method closed_flag : closed_flag -> closed_flag = fun x -> x

    method label : label -> label = self#string

    method arg_label : arg_label -> arg_label =
      fun x ->
        match x with
        | Nolabel -> Nolabel
        | Labelled a ->
            let a = self#string a in
            Labelled a
        | Optional a ->
            let a = self#string a in
            Optional a

    method variance : variance -> variance = fun x -> x

    method injectivity : injectivity -> injectivity = fun x -> x

    method constant : constant -> constant =
      fun x ->
        match x with
        | Pconst_integer (a, b) ->
            let a = self#string a in
            let b = self#option self#char b in
            Pconst_integer (a, b)
        | Pconst_char a ->
            let a = self#char a in
            Pconst_char a
        | Pconst_string (a, b, c) ->
            let a = self#string a in
            let b = self#location b in
            let c = self#option self#string c in
            Pconst_string (a, b, c)
        | Pconst_float (a, b) ->
            let a = self#string a in
            let b = self#option self#char b in
            Pconst_float (a, b)

    method attribute : attribute -> attribute =
      fun { attr_name; attr_payload; attr_loc } ->
        let attr_name = self#loc self#string attr_name in
        let attr_payload = self#payload attr_payload in
        let attr_loc = self#location attr_loc in
        { attr_name; attr_payload; attr_loc }

    method extension : extension -> extension =
      fun (a, b) ->
        let a = self#loc self#string a in
        let b = self#payload b in
        (a, b)

    method attributes : attributes -> attributes = self#list self#attribute

    method payload : payload -> payload =
      fun x ->
        match x with
        | PStr a ->
            let a = self#structure a in
            PStr a
        | PSig a ->
            let a = self#signature a in
            PSig a
        | PTyp a ->
            let a = self#core_type a in
            PTyp a
        | PPat (a, b) ->
            let a = self#pattern a in
            let b = self#option self#expression b in
            PPat (a, b)

    method core_type : core_type -> core_type =
      fun { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes } ->
        let ptyp_desc = self#core_type_desc ptyp_desc in
        let ptyp_loc = self#location ptyp_loc in
        let ptyp_loc_stack = self#location_stack ptyp_loc_stack in
        let ptyp_attributes = self#attributes ptyp_attributes in
        { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }

    method core_type_desc : core_type_desc -> core_type_desc =
      fun x ->
        match x with
        | Ptyp_any -> Ptyp_any
        | Ptyp_var a ->
            let a = self#string a in
            Ptyp_var a
        | Ptyp_arrow (a, b, c) ->
            let a = self#arg_label a in
            let b = self#core_type b in
            let c = self#core_type c in
            Ptyp_arrow (a, b, c)
        | Ptyp_tuple a ->
            let a = self#list self#core_type a in
            Ptyp_tuple a
        | Ptyp_constr (a, b) ->
            let a = self#longident_loc a in
            let b = self#list self#core_type b in
            Ptyp_constr (a, b)
        | Ptyp_object (a, b) ->
            let a = self#list self#object_field a in
            let b = self#closed_flag b in
            Ptyp_object (a, b)
        | Ptyp_class (a, b) ->
            let a = self#longident_loc a in
            let b = self#list self#core_type b in
            Ptyp_class (a, b)
        | Ptyp_alias (a, b) ->
            let a = self#core_type a in
            let b = self#string b in
            Ptyp_alias (a, b)
        | Ptyp_variant (a, b, c) ->
            let a = self#list self#row_field a in
            let b = self#closed_flag b in
            let c = self#option (self#list self#label) c in
            Ptyp_variant (a, b, c)
        | Ptyp_poly (a, b) ->
            let a = self#list (self#loc self#string) a in
            let b = self#core_type b in
            Ptyp_poly (a, b)
        | Ptyp_package a ->
            let a = self#package_type a in
            Ptyp_package a
        | Ptyp_extension a ->
            let a = self#extension a in
            Ptyp_extension a

    method package_type : package_type -> package_type =
      fun (a, b) ->
        let a = self#longident_loc a in
        let b =
          self#list
            (fun (a, b) ->
              let a = self#longident_loc a in
              let b = self#core_type b in
              (a, b))
            b
        in
        (a, b)

    method row_field : row_field -> row_field =
      fun { prf_desc; prf_loc; prf_attributes } ->
        let prf_desc = self#row_field_desc prf_desc in
        let prf_loc = self#location prf_loc in
        let prf_attributes = self#attributes prf_attributes in
        { prf_desc; prf_loc; prf_attributes }

    method row_field_desc : row_field_desc -> row_field_desc =
      fun x ->
        match x with
        | Rtag (a, b, c) ->
            let a = self#loc self#label a in
            let b = self#bool b in
            let c = self#list self#core_type c in
            Rtag (a, b, c)
        | Rinherit a ->
            let a = self#core_type a in
            Rinherit a

    method object_field : object_field -> object_field =
      fun { pof_desc; pof_loc; pof_attributes } ->
        let pof_desc = self#object_field_desc pof_desc in
        let pof_loc = self#location pof_loc in
        let pof_attributes = self#attributes pof_attributes in
        { pof_desc; pof_loc; pof_attributes }

    method object_field_desc : object_field_desc -> object_field_desc =
      fun x ->
        match x with
        | Otag (a, b) ->
            let a = self#loc self#label a in
            let b = self#core_type b in
            Otag (a, b)
        | Oinherit a ->
            let a = self#core_type a in
            Oinherit a

    method pattern : pattern -> pattern =
      fun { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes } ->
        let ppat_desc = self#pattern_desc ppat_desc in
        let ppat_loc = self#location ppat_loc in
        let ppat_loc_stack = self#location_stack ppat_loc_stack in
        let ppat_attributes = self#attributes ppat_attributes in
        { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes }

    method pattern_desc : pattern_desc -> pattern_desc =
      fun x ->
        match x with
        | Ppat_any -> Ppat_any
        | Ppat_var a ->
            let a = self#loc self#string a in
            Ppat_var a
        | Ppat_alias (a, b) ->
            let a = self#pattern a in
            let b = self#loc self#string b in
            Ppat_alias (a, b)
        | Ppat_constant a ->
            let a = self#constant a in
            Ppat_constant a
        | Ppat_interval (a, b) ->
            let a = self#constant a in
            let b = self#constant b in
            Ppat_interval (a, b)
        | Ppat_tuple a ->
            let a = self#list self#pattern a in
            Ppat_tuple a
        | Ppat_construct (a, b) ->
            let a = self#longident_loc a in
            let b = self#option self#pattern b in
            Ppat_construct (a, b)
        | Ppat_variant (a, b) ->
            let a = self#label a in
            let b = self#option self#pattern b in
            Ppat_variant (a, b)
        | Ppat_record (a, b) ->
            let a =
              self#list
                (fun (a, b) ->
                  let a = self#longident_loc a in
                  let b = self#pattern b in
                  (a, b))
                a
            in
            let b = self#closed_flag b in
            Ppat_record (a, b)
        | Ppat_array a ->
            let a = self#list self#pattern a in
            Ppat_array a
        | Ppat_or (a, b) ->
            let a = self#pattern a in
            let b = self#pattern b in
            Ppat_or (a, b)
        | Ppat_constraint (a, b) ->
            let a = self#pattern a in
            let b = self#core_type b in
            Ppat_constraint (a, b)
        | Ppat_type a ->
            let a = self#longident_loc a in
            Ppat_type a
        | Ppat_lazy a ->
            let a = self#pattern a in
            Ppat_lazy a
        | Ppat_unpack a ->
            let a = self#loc (self#option self#string) a in
            Ppat_unpack a
        | Ppat_exception a ->
            let a = self#pattern a in
            Ppat_exception a
        | Ppat_extension a ->
            let a = self#extension a in
            Ppat_extension a
        | Ppat_open (a, b) ->
            let a = self#longident_loc a in
            let b = self#pattern b in
            Ppat_open (a, b)

    method expression : expression -> expression =
      fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
        let pexp_desc = self#expression_desc pexp_desc in
        let pexp_loc = self#location pexp_loc in
        let pexp_loc_stack = self#location_stack pexp_loc_stack in
        let pexp_attributes = self#attributes pexp_attributes in
        { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes }

    method expression_desc : expression_desc -> expression_desc =
      fun x ->
        match x with
        | Pexp_ident a ->
            let a = self#longident_loc a in
            Pexp_ident a
        | Pexp_constant a ->
            let a = self#constant a in
            Pexp_constant a
        | Pexp_let (a, b, c) ->
            let a = self#rec_flag a in
            let b = self#list self#value_binding b in
            let c = self#expression c in
            Pexp_let (a, b, c)
        | Pexp_function a ->
            let a = self#cases a in
            Pexp_function a
        | Pexp_fun (a, b, c, d) ->
            let a = self#arg_label a in
            let b = self#option self#expression b in
            let c = self#pattern c in
            let d = self#expression d in
            Pexp_fun (a, b, c, d)
        | Pexp_apply (a, b) ->
            let a = self#expression a in
            let b =
              self#list
                (fun (a, b) ->
                  let a = self#arg_label a in
                  let b = self#expression b in
                  (a, b))
                b
            in
            Pexp_apply (a, b)
        | Pexp_match (a, b) ->
            let a = self#expression a in
            let b = self#cases b in
            Pexp_match (a, b)
        | Pexp_try (a, b) ->
            let a = self#expression a in
            let b = self#cases b in
            Pexp_try (a, b)
        | Pexp_tuple a ->
            let a = self#list self#expression a in
            Pexp_tuple a
        | Pexp_construct (a, b) ->
            let a = self#longident_loc a in
            let b = self#option self#expression b in
            Pexp_construct (a, b)
        | Pexp_variant (a, b) ->
            let a = self#label a in
            let b = self#option self#expression b in
            Pexp_variant (a, b)
        | Pexp_record (a, b) ->
            let a =
              self#list
                (fun (a, b) ->
                  let a = self#longident_loc a in
                  let b = self#expression b in
                  (a, b))
                a
            in
            let b = self#option self#expression b in
            Pexp_record (a, b)
        | Pexp_field (a, b) ->
            let a = self#expression a in
            let b = self#longident_loc b in
            Pexp_field (a, b)
        | Pexp_setfield (a, b, c) ->
            let a = self#expression a in
            let b = self#longident_loc b in
            let c = self#expression c in
            Pexp_setfield (a, b, c)
        | Pexp_array a ->
            let a = self#list self#expression a in
            Pexp_array a
        | Pexp_ifthenelse (a, b, c) ->
            let a = self#expression a in
            let b = self#expression b in
            let c = self#option self#expression c in
            Pexp_ifthenelse (a, b, c)
        | Pexp_sequence (a, b) ->
            let a = self#expression a in
            let b = self#expression b in
            Pexp_sequence (a, b)
        | Pexp_while (a, b) ->
            let a = self#expression a in
            let b = self#expression b in
            Pexp_while (a, b)
        | Pexp_for (a, b, c, d, e) ->
            let a = self#pattern a in
            let b = self#expression b in
            let c = self#expression c in
            let d = self#direction_flag d in
            let e = self#expression e in
            Pexp_for (a, b, c, d, e)
        | Pexp_constraint (a, b) ->
            let a = self#expression a in
            let b = self#core_type b in
            Pexp_constraint (a, b)
        | Pexp_coerce (a, b, c) ->
            let a = self#expression a in
            let b = self#option self#core_type b in
            let c = self#core_type c in
            Pexp_coerce (a, b, c)
        | Pexp_send (a, b) ->
            let a = self#expression a in
            let b = self#loc self#label b in
            Pexp_send (a, b)
        | Pexp_new a ->
            let a = self#longident_loc a in
            Pexp_new a
        | Pexp_setinstvar (a, b) ->
            let a = self#loc self#label a in
            let b = self#expression b in
            Pexp_setinstvar (a, b)
        | Pexp_override a ->
            let a =
              self#list
                (fun (a, b) ->
                  let a = self#loc self#label a in
                  let b = self#expression b in
                  (a, b))
                a
            in
            Pexp_override a
        | Pexp_letmodule (a, b, c) ->
            let a = self#loc (self#option self#string) a in
            let b = self#module_expr b in
            let c = self#expression c in
            Pexp_letmodule (a, b, c)
        | Pexp_letexception (a, b) ->
            let a = self#extension_constructor a in
            let b = self#expression b in
            Pexp_letexception (a, b)
        | Pexp_assert a ->
            let a = self#expression a in
            Pexp_assert a
        | Pexp_lazy a ->
            let a = self#expression a in
            Pexp_lazy a
        | Pexp_poly (a, b) ->
            let a = self#expression a in
            let b = self#option self#core_type b in
            Pexp_poly (a, b)
        | Pexp_object a ->
            let a = self#class_structure a in
            Pexp_object a
        | Pexp_newtype (a, b) ->
            let a = self#loc self#string a in
            let b = self#expression b in
            Pexp_newtype (a, b)
        | Pexp_pack a ->
            let a = self#module_expr a in
            Pexp_pack a
        | Pexp_open (a, b) ->
            let a = self#open_declaration a in
            let b = self#expression b in
            Pexp_open (a, b)
        | Pexp_letop a ->
            let a = self#letop a in
            Pexp_letop a
        | Pexp_extension a ->
            let a = self#extension a in
            Pexp_extension a
        | Pexp_unreachable -> Pexp_unreachable

    method case : case -> case =
      fun { pc_lhs; pc_guard; pc_rhs } ->
        let pc_lhs = self#pattern pc_lhs in
        let pc_guard = self#option self#expression pc_guard in
        let pc_rhs = self#expression pc_rhs in
        { pc_lhs; pc_guard; pc_rhs }

    method letop : letop -> letop =
      fun { let_; ands; body } ->
        let let_ = self#binding_op let_ in
        let ands = self#list self#binding_op ands in
        let body = self#expression body in
        { let_; ands; body }

    method binding_op : binding_op -> binding_op =
      fun { pbop_op; pbop_pat; pbop_exp; pbop_loc } ->
        let pbop_op = self#loc self#string pbop_op in
        let pbop_pat = self#pattern pbop_pat in
        let pbop_exp = self#expression pbop_exp in
        let pbop_loc = self#location pbop_loc in
        { pbop_op; pbop_pat; pbop_exp; pbop_loc }

    method value_description : value_description -> value_description =
      fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } ->
        let pval_name = self#loc self#string pval_name in
        let pval_type = self#core_type pval_type in
        let pval_prim = self#list self#string pval_prim in
        let pval_attributes = self#attributes pval_attributes in
        let pval_loc = self#location pval_loc in
        { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }

    method type_declaration : type_declaration -> type_declaration =
      fun {
            ptype_name;
            ptype_params;
            ptype_cstrs;
            ptype_kind;
            ptype_private;
            ptype_manifest;
            ptype_attributes;
            ptype_loc;
          } ->
        let ptype_name = self#loc self#string ptype_name in
        let ptype_params =
          self#list
            (fun (a, b) ->
              let a = self#core_type a in
              let b =
                (fun (a, b) ->
                  let a = self#variance a in
                  let b = self#injectivity b in
                  (a, b))
                  b
              in
              (a, b))
            ptype_params
        in
        let ptype_cstrs =
          self#list
            (fun (a, b, c) ->
              let a = self#core_type a in
              let b = self#core_type b in
              let c = self#location c in
              (a, b, c))
            ptype_cstrs
        in
        let ptype_kind = self#type_kind ptype_kind in
        let ptype_private = self#private_flag ptype_private in
        let ptype_manifest = self#option self#core_type ptype_manifest in
        let ptype_attributes = self#attributes ptype_attributes in
        let ptype_loc = self#location ptype_loc in
        {
          ptype_name;
          ptype_params;
          ptype_cstrs;
          ptype_kind;
          ptype_private;
          ptype_manifest;
          ptype_attributes;
          ptype_loc;
        }

    method type_kind : type_kind -> type_kind =
      fun x ->
        match x with
        | Ptype_abstract -> Ptype_abstract
        | Ptype_variant a ->
            let a = self#list self#constructor_declaration a in
            Ptype_variant a
        | Ptype_record a ->
            let a = self#list self#label_declaration a in
            Ptype_record a
        | Ptype_open -> Ptype_open

    method label_declaration : label_declaration -> label_declaration =
      fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } ->
        let pld_name = self#loc self#string pld_name in
        let pld_mutable = self#mutable_flag pld_mutable in
        let pld_type = self#core_type pld_type in
        let pld_loc = self#location pld_loc in
        let pld_attributes = self#attributes pld_attributes in
        { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }

    method constructor_declaration
        : constructor_declaration -> constructor_declaration =
      fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } ->
        let pcd_name = self#loc self#string pcd_name in
        let pcd_args = self#constructor_arguments pcd_args in
        let pcd_res = self#option self#core_type pcd_res in
        let pcd_loc = self#location pcd_loc in
        let pcd_attributes = self#attributes pcd_attributes in
        { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }

    method constructor_arguments
        : constructor_arguments -> constructor_arguments =
      fun x ->
        match x with
        | Pcstr_tuple a ->
            let a = self#list self#core_type a in
            Pcstr_tuple a
        | Pcstr_record a ->
            let a = self#list self#label_declaration a in
            Pcstr_record a

    method type_extension : type_extension -> type_extension =
      fun {
            ptyext_path;
            ptyext_params;
            ptyext_constructors;
            ptyext_private;
            ptyext_loc;
            ptyext_attributes;
          } ->
        let ptyext_path = self#longident_loc ptyext_path in
        let ptyext_params =
          self#list
            (fun (a, b) ->
              let a = self#core_type a in
              let b =
                (fun (a, b) ->
                  let a = self#variance a in
                  let b = self#injectivity b in
                  (a, b))
                  b
              in
              (a, b))
            ptyext_params
        in
        let ptyext_constructors =
          self#list self#extension_constructor ptyext_constructors
        in
        let ptyext_private = self#private_flag ptyext_private in
        let ptyext_loc = self#location ptyext_loc in
        let ptyext_attributes = self#attributes ptyext_attributes in
        {
          ptyext_path;
          ptyext_params;
          ptyext_constructors;
          ptyext_private;
          ptyext_loc;
          ptyext_attributes;
        }

    method extension_constructor
        : extension_constructor -> extension_constructor =
      fun { pext_name; pext_kind; pext_loc; pext_attributes } ->
        let pext_name = self#loc self#string pext_name in
        let pext_kind = self#extension_constructor_kind pext_kind in
        let pext_loc = self#location pext_loc in
        let pext_attributes = self#attributes pext_attributes in
        { pext_name; pext_kind; pext_loc; pext_attributes }

    method type_exception : type_exception -> type_exception =
      fun { ptyexn_constructor; ptyexn_loc; ptyexn_attributes } ->
        let ptyexn_constructor =
          self#extension_constructor ptyexn_constructor
        in
        let ptyexn_loc = self#location ptyexn_loc in
        let ptyexn_attributes = self#attributes ptyexn_attributes in
        { ptyexn_constructor; ptyexn_loc; ptyexn_attributes }

    method extension_constructor_kind
        : extension_constructor_kind -> extension_constructor_kind =
      fun x ->
        match x with
        | Pext_decl (a, b) ->
            let a = self#constructor_arguments a in
            let b = self#option self#core_type b in
            Pext_decl (a, b)
        | Pext_rebind a ->
            let a = self#longident_loc a in
            Pext_rebind a

    method class_type : class_type -> class_type =
      fun { pcty_desc; pcty_loc; pcty_attributes } ->
        let pcty_desc = self#class_type_desc pcty_desc in
        let pcty_loc = self#location pcty_loc in
        let pcty_attributes = self#attributes pcty_attributes in
        { pcty_desc; pcty_loc; pcty_attributes }

    method class_type_desc : class_type_desc -> class_type_desc =
      fun x ->
        match x with
        | Pcty_constr (a, b) ->
            let a = self#longident_loc a in
            let b = self#list self#core_type b in
            Pcty_constr (a, b)
        | Pcty_signature a ->
            let a = self#class_signature a in
            Pcty_signature a
        | Pcty_arrow (a, b, c) ->
            let a = self#arg_label a in
            let b = self#core_type b in
            let c = self#class_type c in
            Pcty_arrow (a, b, c)
        | Pcty_extension a ->
            let a = self#extension a in
            Pcty_extension a
        | Pcty_open (a, b) ->
            let a = self#open_description a in
            let b = self#class_type b in
            Pcty_open (a, b)

    method class_signature : class_signature -> class_signature =
      fun { pcsig_self; pcsig_fields } ->
        let pcsig_self = self#core_type pcsig_self in
        let pcsig_fields = self#list self#class_type_field pcsig_fields in
        { pcsig_self; pcsig_fields }

    method class_type_field : class_type_field -> class_type_field =
      fun { pctf_desc; pctf_loc; pctf_attributes } ->
        let pctf_desc = self#class_type_field_desc pctf_desc in
        let pctf_loc = self#location pctf_loc in
        let pctf_attributes = self#attributes pctf_attributes in
        { pctf_desc; pctf_loc; pctf_attributes }

    method class_type_field_desc
        : class_type_field_desc -> class_type_field_desc =
      fun x ->
        match x with
        | Pctf_inherit a ->
            let a = self#class_type a in
            Pctf_inherit a
        | Pctf_val a ->
            let a =
              (fun (a, b, c, d) ->
                let a = self#loc self#label a in
                let b = self#mutable_flag b in
                let c = self#virtual_flag c in
                let d = self#core_type d in
                (a, b, c, d))
                a
            in
            Pctf_val a
        | Pctf_method a ->
            let a =
              (fun (a, b, c, d) ->
                let a = self#loc self#label a in
                let b = self#private_flag b in
                let c = self#virtual_flag c in
                let d = self#core_type d in
                (a, b, c, d))
                a
            in
            Pctf_method a
        | Pctf_constraint a ->
            let a =
              (fun (a, b) ->
                let a = self#core_type a in
                let b = self#core_type b in
                (a, b))
                a
            in
            Pctf_constraint a
        | Pctf_attribute a ->
            let a = self#attribute a in
            Pctf_attribute a
        | Pctf_extension a ->
            let a = self#extension a in
            Pctf_extension a

    method class_infos : 'a. ('a -> 'a) -> 'a class_infos -> 'a class_infos =
      fun _a
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
        let pci_virt = self#virtual_flag pci_virt in
        let pci_params =
          self#list
            (fun (a, b) ->
              let a = self#core_type a in
              let b =
                (fun (a, b) ->
                  let a = self#variance a in
                  let b = self#injectivity b in
                  (a, b))
                  b
              in
              (a, b))
            pci_params
        in
        let pci_name = self#loc self#string pci_name in
        let pci_expr = _a pci_expr in
        let pci_loc = self#location pci_loc in
        let pci_attributes = self#attributes pci_attributes in
        { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }

    method class_description : class_description -> class_description =
      self#class_infos self#class_type

    method class_type_declaration
        : class_type_declaration -> class_type_declaration =
      self#class_infos self#class_type

    method class_expr : class_expr -> class_expr =
      fun { pcl_desc; pcl_loc; pcl_attributes } ->
        let pcl_desc = self#class_expr_desc pcl_desc in
        let pcl_loc = self#location pcl_loc in
        let pcl_attributes = self#attributes pcl_attributes in
        { pcl_desc; pcl_loc; pcl_attributes }

    method class_expr_desc : class_expr_desc -> class_expr_desc =
      fun x ->
        match x with
        | Pcl_constr (a, b) ->
            let a = self#longident_loc a in
            let b = self#list self#core_type b in
            Pcl_constr (a, b)
        | Pcl_structure a ->
            let a = self#class_structure a in
            Pcl_structure a
        | Pcl_fun (a, b, c, d) ->
            let a = self#arg_label a in
            let b = self#option self#expression b in
            let c = self#pattern c in
            let d = self#class_expr d in
            Pcl_fun (a, b, c, d)
        | Pcl_apply (a, b) ->
            let a = self#class_expr a in
            let b =
              self#list
                (fun (a, b) ->
                  let a = self#arg_label a in
                  let b = self#expression b in
                  (a, b))
                b
            in
            Pcl_apply (a, b)
        | Pcl_let (a, b, c) ->
            let a = self#rec_flag a in
            let b = self#list self#value_binding b in
            let c = self#class_expr c in
            Pcl_let (a, b, c)
        | Pcl_constraint (a, b) ->
            let a = self#class_expr a in
            let b = self#class_type b in
            Pcl_constraint (a, b)
        | Pcl_extension a ->
            let a = self#extension a in
            Pcl_extension a
        | Pcl_open (a, b) ->
            let a = self#open_description a in
            let b = self#class_expr b in
            Pcl_open (a, b)

    method class_structure : class_structure -> class_structure =
      fun { pcstr_self; pcstr_fields } ->
        let pcstr_self = self#pattern pcstr_self in
        let pcstr_fields = self#list self#class_field pcstr_fields in
        { pcstr_self; pcstr_fields }

    method class_field : class_field -> class_field =
      fun { pcf_desc; pcf_loc; pcf_attributes } ->
        let pcf_desc = self#class_field_desc pcf_desc in
        let pcf_loc = self#location pcf_loc in
        let pcf_attributes = self#attributes pcf_attributes in
        { pcf_desc; pcf_loc; pcf_attributes }

    method class_field_desc : class_field_desc -> class_field_desc =
      fun x ->
        match x with
        | Pcf_inherit (a, b, c) ->
            let a = self#override_flag a in
            let b = self#class_expr b in
            let c = self#option (self#loc self#string) c in
            Pcf_inherit (a, b, c)
        | Pcf_val a ->
            let a =
              (fun (a, b, c) ->
                let a = self#loc self#label a in
                let b = self#mutable_flag b in
                let c = self#class_field_kind c in
                (a, b, c))
                a
            in
            Pcf_val a
        | Pcf_method a ->
            let a =
              (fun (a, b, c) ->
                let a = self#loc self#label a in
                let b = self#private_flag b in
                let c = self#class_field_kind c in
                (a, b, c))
                a
            in
            Pcf_method a
        | Pcf_constraint a ->
            let a =
              (fun (a, b) ->
                let a = self#core_type a in
                let b = self#core_type b in
                (a, b))
                a
            in
            Pcf_constraint a
        | Pcf_initializer a ->
            let a = self#expression a in
            Pcf_initializer a
        | Pcf_attribute a ->
            let a = self#attribute a in
            Pcf_attribute a
        | Pcf_extension a ->
            let a = self#extension a in
            Pcf_extension a

    method class_field_kind : class_field_kind -> class_field_kind =
      fun x ->
        match x with
        | Cfk_virtual a ->
            let a = self#core_type a in
            Cfk_virtual a
        | Cfk_concrete (a, b) ->
            let a = self#override_flag a in
            let b = self#expression b in
            Cfk_concrete (a, b)

    method class_declaration : class_declaration -> class_declaration =
      self#class_infos self#class_expr

    method module_type : module_type -> module_type =
      fun { pmty_desc; pmty_loc; pmty_attributes } ->
        let pmty_desc = self#module_type_desc pmty_desc in
        let pmty_loc = self#location pmty_loc in
        let pmty_attributes = self#attributes pmty_attributes in
        { pmty_desc; pmty_loc; pmty_attributes }

    method module_type_desc : module_type_desc -> module_type_desc =
      fun x ->
        match x with
        | Pmty_ident a ->
            let a = self#longident_loc a in
            Pmty_ident a
        | Pmty_signature a ->
            let a = self#signature a in
            Pmty_signature a
        | Pmty_functor (a, b) ->
            let a = self#functor_parameter a in
            let b = self#module_type b in
            Pmty_functor (a, b)
        | Pmty_with (a, b) ->
            let a = self#module_type a in
            let b = self#list self#with_constraint b in
            Pmty_with (a, b)
        | Pmty_typeof a ->
            let a = self#module_expr a in
            Pmty_typeof a
        | Pmty_extension a ->
            let a = self#extension a in
            Pmty_extension a
        | Pmty_alias a ->
            let a = self#longident_loc a in
            Pmty_alias a

    method functor_parameter : functor_parameter -> functor_parameter =
      fun x ->
        match x with
        | Unit -> Unit
        | Named (a, b) ->
            let a = self#loc (self#option self#string) a in
            let b = self#module_type b in
            Named (a, b)

    method signature : signature -> signature = self#list self#signature_item

    method signature_item : signature_item -> signature_item =
      fun { psig_desc; psig_loc } ->
        let psig_desc = self#signature_item_desc psig_desc in
        let psig_loc = self#location psig_loc in
        { psig_desc; psig_loc }

    method signature_item_desc : signature_item_desc -> signature_item_desc =
      fun x ->
        match x with
        | Psig_value a ->
            let a = self#value_description a in
            Psig_value a
        | Psig_type (a, b) ->
            let a = self#rec_flag a in
            let b = self#list self#type_declaration b in
            Psig_type (a, b)
        | Psig_typesubst a ->
            let a = self#list self#type_declaration a in
            Psig_typesubst a
        | Psig_typext a ->
            let a = self#type_extension a in
            Psig_typext a
        | Psig_exception a ->
            let a = self#type_exception a in
            Psig_exception a
        | Psig_module a ->
            let a = self#module_declaration a in
            Psig_module a
        | Psig_modsubst a ->
            let a = self#module_substitution a in
            Psig_modsubst a
        | Psig_recmodule a ->
            let a = self#list self#module_declaration a in
            Psig_recmodule a
        | Psig_modtype a ->
            let a = self#module_type_declaration a in
            Psig_modtype a
        | Psig_open a ->
            let a = self#open_description a in
            Psig_open a
        | Psig_include a ->
            let a = self#include_description a in
            Psig_include a
        | Psig_class a ->
            let a = self#list self#class_description a in
            Psig_class a
        | Psig_class_type a ->
            let a = self#list self#class_type_declaration a in
            Psig_class_type a
        | Psig_attribute a ->
            let a = self#attribute a in
            Psig_attribute a
        | Psig_extension (a, b) ->
            let a = self#extension a in
            let b = self#attributes b in
            Psig_extension (a, b)

    method module_declaration : module_declaration -> module_declaration =
      fun { pmd_name; pmd_type; pmd_attributes; pmd_loc } ->
        let pmd_name = self#loc (self#option self#string) pmd_name in
        let pmd_type = self#module_type pmd_type in
        let pmd_attributes = self#attributes pmd_attributes in
        let pmd_loc = self#location pmd_loc in
        { pmd_name; pmd_type; pmd_attributes; pmd_loc }

    method module_substitution : module_substitution -> module_substitution =
      fun { pms_name; pms_manifest; pms_attributes; pms_loc } ->
        let pms_name = self#loc self#string pms_name in
        let pms_manifest = self#longident_loc pms_manifest in
        let pms_attributes = self#attributes pms_attributes in
        let pms_loc = self#location pms_loc in
        { pms_name; pms_manifest; pms_attributes; pms_loc }

    method module_type_declaration
        : module_type_declaration -> module_type_declaration =
      fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } ->
        let pmtd_name = self#loc self#string pmtd_name in
        let pmtd_type = self#option self#module_type pmtd_type in
        let pmtd_attributes = self#attributes pmtd_attributes in
        let pmtd_loc = self#location pmtd_loc in
        { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }

    method open_infos : 'a. ('a -> 'a) -> 'a open_infos -> 'a open_infos =
      fun _a { popen_expr; popen_override; popen_loc; popen_attributes } ->
        let popen_expr = _a popen_expr in
        let popen_override = self#override_flag popen_override in
        let popen_loc = self#location popen_loc in
        let popen_attributes = self#attributes popen_attributes in
        { popen_expr; popen_override; popen_loc; popen_attributes }

    method open_description : open_description -> open_description =
      self#open_infos self#longident_loc

    method open_declaration : open_declaration -> open_declaration =
      self#open_infos self#module_expr

    method include_infos
        : 'a. ('a -> 'a) -> 'a include_infos -> 'a include_infos =
      fun _a { pincl_mod; pincl_loc; pincl_attributes } ->
        let pincl_mod = _a pincl_mod in
        let pincl_loc = self#location pincl_loc in
        let pincl_attributes = self#attributes pincl_attributes in
        { pincl_mod; pincl_loc; pincl_attributes }

    method include_description : include_description -> include_description =
      self#include_infos self#module_type

    method include_declaration : include_declaration -> include_declaration =
      self#include_infos self#module_expr

    method with_constraint : with_constraint -> with_constraint =
      fun x ->
        match x with
        | Pwith_type (a, b) ->
            let a = self#longident_loc a in
            let b = self#type_declaration b in
            Pwith_type (a, b)
        | Pwith_module (a, b) ->
            let a = self#longident_loc a in
            let b = self#longident_loc b in
            Pwith_module (a, b)
        | Pwith_typesubst (a, b) ->
            let a = self#longident_loc a in
            let b = self#type_declaration b in
            Pwith_typesubst (a, b)
        | Pwith_modsubst (a, b) ->
            let a = self#longident_loc a in
            let b = self#longident_loc b in
            Pwith_modsubst (a, b)

    method module_expr : module_expr -> module_expr =
      fun { pmod_desc; pmod_loc; pmod_attributes } ->
        let pmod_desc = self#module_expr_desc pmod_desc in
        let pmod_loc = self#location pmod_loc in
        let pmod_attributes = self#attributes pmod_attributes in
        { pmod_desc; pmod_loc; pmod_attributes }

    method module_expr_desc : module_expr_desc -> module_expr_desc =
      fun x ->
        match x with
        | Pmod_ident a ->
            let a = self#longident_loc a in
            Pmod_ident a
        | Pmod_structure a ->
            let a = self#structure a in
            Pmod_structure a
        | Pmod_functor (a, b) ->
            let a = self#functor_parameter a in
            let b = self#module_expr b in
            Pmod_functor (a, b)
        | Pmod_apply (a, b) ->
            let a = self#module_expr a in
            let b = self#module_expr b in
            Pmod_apply (a, b)
        | Pmod_constraint (a, b) ->
            let a = self#module_expr a in
            let b = self#module_type b in
            Pmod_constraint (a, b)
        | Pmod_unpack a ->
            let a = self#expression a in
            Pmod_unpack a
        | Pmod_extension a ->
            let a = self#extension a in
            Pmod_extension a

    method structure : structure -> structure = self#list self#structure_item

    method structure_item : structure_item -> structure_item =
      fun { pstr_desc; pstr_loc } ->
        let pstr_desc = self#structure_item_desc pstr_desc in
        let pstr_loc = self#location pstr_loc in
        { pstr_desc; pstr_loc }

    method structure_item_desc : structure_item_desc -> structure_item_desc =
      fun x ->
        match x with
        | Pstr_eval (a, b) ->
            let a = self#expression a in
            let b = self#attributes b in
            Pstr_eval (a, b)
        | Pstr_value (a, b) ->
            let a = self#rec_flag a in
            let b = self#list self#value_binding b in
            Pstr_value (a, b)
        | Pstr_primitive a ->
            let a = self#value_description a in
            Pstr_primitive a
        | Pstr_type (a, b) ->
            let a = self#rec_flag a in
            let b = self#list self#type_declaration b in
            Pstr_type (a, b)
        | Pstr_typext a ->
            let a = self#type_extension a in
            Pstr_typext a
        | Pstr_exception a ->
            let a = self#type_exception a in
            Pstr_exception a
        | Pstr_module a ->
            let a = self#module_binding a in
            Pstr_module a
        | Pstr_recmodule a ->
            let a = self#list self#module_binding a in
            Pstr_recmodule a
        | Pstr_modtype a ->
            let a = self#module_type_declaration a in
            Pstr_modtype a
        | Pstr_open a ->
            let a = self#open_declaration a in
            Pstr_open a
        | Pstr_class a ->
            let a = self#list self#class_declaration a in
            Pstr_class a
        | Pstr_class_type a ->
            let a = self#list self#class_type_declaration a in
            Pstr_class_type a
        | Pstr_include a ->
            let a = self#include_declaration a in
            Pstr_include a
        | Pstr_attribute a ->
            let a = self#attribute a in
            Pstr_attribute a
        | Pstr_extension (a, b) ->
            let a = self#extension a in
            let b = self#attributes b in
            Pstr_extension (a, b)

    method value_binding : value_binding -> value_binding =
      fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } ->
        let pvb_pat = self#pattern pvb_pat in
        let pvb_expr = self#expression pvb_expr in
        let pvb_attributes = self#attributes pvb_attributes in
        let pvb_loc = self#location pvb_loc in
        { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }

    method module_binding : module_binding -> module_binding =
      fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc } ->
        let pmb_name = self#loc (self#option self#string) pmb_name in
        let pmb_expr = self#module_expr pmb_expr in
        let pmb_attributes = self#attributes pmb_attributes in
        let pmb_loc = self#location pmb_loc in
        { pmb_name; pmb_expr; pmb_attributes; pmb_loc }

    method toplevel_phrase : toplevel_phrase -> toplevel_phrase =
      fun x ->
        match x with
        | Ptop_def a ->
            let a = self#structure a in
            Ptop_def a
        | Ptop_dir a ->
            let a = self#toplevel_directive a in
            Ptop_dir a

    method toplevel_directive : toplevel_directive -> toplevel_directive =
      fun { pdir_name; pdir_arg; pdir_loc } ->
        let pdir_name = self#loc self#string pdir_name in
        let pdir_arg = self#option self#directive_argument pdir_arg in
        let pdir_loc = self#location pdir_loc in
        { pdir_name; pdir_arg; pdir_loc }

    method directive_argument : directive_argument -> directive_argument =
      fun { pdira_desc; pdira_loc } ->
        let pdira_desc = self#directive_argument_desc pdira_desc in
        let pdira_loc = self#location pdira_loc in
        { pdira_desc; pdira_loc }

    method directive_argument_desc
        : directive_argument_desc -> directive_argument_desc =
      fun x ->
        match x with
        | Pdir_string a ->
            let a = self#string a in
            Pdir_string a
        | Pdir_int (a, b) ->
            let a = self#string a in
            let b = self#option self#char b in
            Pdir_int (a, b)
        | Pdir_ident a ->
            let a = self#longident a in
            Pdir_ident a
        | Pdir_bool a ->
            let a = self#bool a in
            Pdir_bool a

    method cases : cases -> cases = self#list self#case
  end

class virtual iter =
  object (self)
    method virtual bool : bool -> unit

    method virtual char : char -> unit

    method virtual int : int -> unit

    method virtual list : 'a. ('a -> unit) -> 'a list -> unit

    method virtual option : 'a. ('a -> unit) -> 'a option -> unit

    method virtual string : string -> unit

    method position : position -> unit =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
        self#string pos_fname;
        self#int pos_lnum;
        self#int pos_bol;
        self#int pos_cnum

    method location : location -> unit =
      fun { loc_start; loc_end; loc_ghost } ->
        self#position loc_start;
        self#position loc_end;
        self#bool loc_ghost

    method location_stack : location_stack -> unit = self#list self#location

    method loc : 'a. ('a -> unit) -> 'a loc -> unit =
      fun _a { txt; loc } ->
        _a txt;
        self#location loc

    method longident : longident -> unit =
      fun x ->
        match x with
        | Lident a -> self#string a
        | Ldot (a, b) ->
            self#longident a;
            self#string b
        | Lapply (a, b) ->
            self#longident a;
            self#longident b

    method longident_loc : longident_loc -> unit = self#loc self#longident

    method rec_flag : rec_flag -> unit = fun _ -> ()

    method direction_flag : direction_flag -> unit = fun _ -> ()

    method private_flag : private_flag -> unit = fun _ -> ()

    method mutable_flag : mutable_flag -> unit = fun _ -> ()

    method virtual_flag : virtual_flag -> unit = fun _ -> ()

    method override_flag : override_flag -> unit = fun _ -> ()

    method closed_flag : closed_flag -> unit = fun _ -> ()

    method label : label -> unit = self#string

    method arg_label : arg_label -> unit =
      fun x ->
        match x with
        | Nolabel -> ()
        | Labelled a -> self#string a
        | Optional a -> self#string a

    method variance : variance -> unit = fun _ -> ()

    method injectivity : injectivity -> unit = fun _ -> ()

    method constant : constant -> unit =
      fun x ->
        match x with
        | Pconst_integer (a, b) ->
            self#string a;
            self#option self#char b
        | Pconst_char a -> self#char a
        | Pconst_string (a, b, c) ->
            self#string a;
            self#location b;
            self#option self#string c
        | Pconst_float (a, b) ->
            self#string a;
            self#option self#char b

    method attribute : attribute -> unit =
      fun { attr_name; attr_payload; attr_loc } ->
        self#loc self#string attr_name;
        self#payload attr_payload;
        self#location attr_loc

    method extension : extension -> unit =
      fun (a, b) ->
        self#loc self#string a;
        self#payload b

    method attributes : attributes -> unit = self#list self#attribute

    method payload : payload -> unit =
      fun x ->
        match x with
        | PStr a -> self#structure a
        | PSig a -> self#signature a
        | PTyp a -> self#core_type a
        | PPat (a, b) ->
            self#pattern a;
            self#option self#expression b

    method core_type : core_type -> unit =
      fun { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes } ->
        self#core_type_desc ptyp_desc;
        self#location ptyp_loc;
        self#location_stack ptyp_loc_stack;
        self#attributes ptyp_attributes

    method core_type_desc : core_type_desc -> unit =
      fun x ->
        match x with
        | Ptyp_any -> ()
        | Ptyp_var a -> self#string a
        | Ptyp_arrow (a, b, c) ->
            self#arg_label a;
            self#core_type b;
            self#core_type c
        | Ptyp_tuple a -> self#list self#core_type a
        | Ptyp_constr (a, b) ->
            self#longident_loc a;
            self#list self#core_type b
        | Ptyp_object (a, b) ->
            self#list self#object_field a;
            self#closed_flag b
        | Ptyp_class (a, b) ->
            self#longident_loc a;
            self#list self#core_type b
        | Ptyp_alias (a, b) ->
            self#core_type a;
            self#string b
        | Ptyp_variant (a, b, c) ->
            self#list self#row_field a;
            self#closed_flag b;
            self#option (self#list self#label) c
        | Ptyp_poly (a, b) ->
            self#list (self#loc self#string) a;
            self#core_type b
        | Ptyp_package a -> self#package_type a
        | Ptyp_extension a -> self#extension a

    method package_type : package_type -> unit =
      fun (a, b) ->
        self#longident_loc a;
        self#list
          (fun (a, b) ->
            self#longident_loc a;
            self#core_type b)
          b

    method row_field : row_field -> unit =
      fun { prf_desc; prf_loc; prf_attributes } ->
        self#row_field_desc prf_desc;
        self#location prf_loc;
        self#attributes prf_attributes

    method row_field_desc : row_field_desc -> unit =
      fun x ->
        match x with
        | Rtag (a, b, c) ->
            self#loc self#label a;
            self#bool b;
            self#list self#core_type c
        | Rinherit a -> self#core_type a

    method object_field : object_field -> unit =
      fun { pof_desc; pof_loc; pof_attributes } ->
        self#object_field_desc pof_desc;
        self#location pof_loc;
        self#attributes pof_attributes

    method object_field_desc : object_field_desc -> unit =
      fun x ->
        match x with
        | Otag (a, b) ->
            self#loc self#label a;
            self#core_type b
        | Oinherit a -> self#core_type a

    method pattern : pattern -> unit =
      fun { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes } ->
        self#pattern_desc ppat_desc;
        self#location ppat_loc;
        self#location_stack ppat_loc_stack;
        self#attributes ppat_attributes

    method pattern_desc : pattern_desc -> unit =
      fun x ->
        match x with
        | Ppat_any -> ()
        | Ppat_var a -> self#loc self#string a
        | Ppat_alias (a, b) ->
            self#pattern a;
            self#loc self#string b
        | Ppat_constant a -> self#constant a
        | Ppat_interval (a, b) ->
            self#constant a;
            self#constant b
        | Ppat_tuple a -> self#list self#pattern a
        | Ppat_construct (a, b) ->
            self#longident_loc a;
            self#option self#pattern b
        | Ppat_variant (a, b) ->
            self#label a;
            self#option self#pattern b
        | Ppat_record (a, b) ->
            self#list
              (fun (a, b) ->
                self#longident_loc a;
                self#pattern b)
              a;
            self#closed_flag b
        | Ppat_array a -> self#list self#pattern a
        | Ppat_or (a, b) ->
            self#pattern a;
            self#pattern b
        | Ppat_constraint (a, b) ->
            self#pattern a;
            self#core_type b
        | Ppat_type a -> self#longident_loc a
        | Ppat_lazy a -> self#pattern a
        | Ppat_unpack a -> self#loc (self#option self#string) a
        | Ppat_exception a -> self#pattern a
        | Ppat_extension a -> self#extension a
        | Ppat_open (a, b) ->
            self#longident_loc a;
            self#pattern b

    method expression : expression -> unit =
      fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
        self#expression_desc pexp_desc;
        self#location pexp_loc;
        self#location_stack pexp_loc_stack;
        self#attributes pexp_attributes

    method expression_desc : expression_desc -> unit =
      fun x ->
        match x with
        | Pexp_ident a -> self#longident_loc a
        | Pexp_constant a -> self#constant a
        | Pexp_let (a, b, c) ->
            self#rec_flag a;
            self#list self#value_binding b;
            self#expression c
        | Pexp_function a -> self#cases a
        | Pexp_fun (a, b, c, d) ->
            self#arg_label a;
            self#option self#expression b;
            self#pattern c;
            self#expression d
        | Pexp_apply (a, b) ->
            self#expression a;
            self#list
              (fun (a, b) ->
                self#arg_label a;
                self#expression b)
              b
        | Pexp_match (a, b) ->
            self#expression a;
            self#cases b
        | Pexp_try (a, b) ->
            self#expression a;
            self#cases b
        | Pexp_tuple a -> self#list self#expression a
        | Pexp_construct (a, b) ->
            self#longident_loc a;
            self#option self#expression b
        | Pexp_variant (a, b) ->
            self#label a;
            self#option self#expression b
        | Pexp_record (a, b) ->
            self#list
              (fun (a, b) ->
                self#longident_loc a;
                self#expression b)
              a;
            self#option self#expression b
        | Pexp_field (a, b) ->
            self#expression a;
            self#longident_loc b
        | Pexp_setfield (a, b, c) ->
            self#expression a;
            self#longident_loc b;
            self#expression c
        | Pexp_array a -> self#list self#expression a
        | Pexp_ifthenelse (a, b, c) ->
            self#expression a;
            self#expression b;
            self#option self#expression c
        | Pexp_sequence (a, b) ->
            self#expression a;
            self#expression b
        | Pexp_while (a, b) ->
            self#expression a;
            self#expression b
        | Pexp_for (a, b, c, d, e) ->
            self#pattern a;
            self#expression b;
            self#expression c;
            self#direction_flag d;
            self#expression e
        | Pexp_constraint (a, b) ->
            self#expression a;
            self#core_type b
        | Pexp_coerce (a, b, c) ->
            self#expression a;
            self#option self#core_type b;
            self#core_type c
        | Pexp_send (a, b) ->
            self#expression a;
            self#loc self#label b
        | Pexp_new a -> self#longident_loc a
        | Pexp_setinstvar (a, b) ->
            self#loc self#label a;
            self#expression b
        | Pexp_override a ->
            self#list
              (fun (a, b) ->
                self#loc self#label a;
                self#expression b)
              a
        | Pexp_letmodule (a, b, c) ->
            self#loc (self#option self#string) a;
            self#module_expr b;
            self#expression c
        | Pexp_letexception (a, b) ->
            self#extension_constructor a;
            self#expression b
        | Pexp_assert a -> self#expression a
        | Pexp_lazy a -> self#expression a
        | Pexp_poly (a, b) ->
            self#expression a;
            self#option self#core_type b
        | Pexp_object a -> self#class_structure a
        | Pexp_newtype (a, b) ->
            self#loc self#string a;
            self#expression b
        | Pexp_pack a -> self#module_expr a
        | Pexp_open (a, b) ->
            self#open_declaration a;
            self#expression b
        | Pexp_letop a -> self#letop a
        | Pexp_extension a -> self#extension a
        | Pexp_unreachable -> ()

    method case : case -> unit =
      fun { pc_lhs; pc_guard; pc_rhs } ->
        self#pattern pc_lhs;
        self#option self#expression pc_guard;
        self#expression pc_rhs

    method letop : letop -> unit =
      fun { let_; ands; body } ->
        self#binding_op let_;
        self#list self#binding_op ands;
        self#expression body

    method binding_op : binding_op -> unit =
      fun { pbop_op; pbop_pat; pbop_exp; pbop_loc } ->
        self#loc self#string pbop_op;
        self#pattern pbop_pat;
        self#expression pbop_exp;
        self#location pbop_loc

    method value_description : value_description -> unit =
      fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } ->
        self#loc self#string pval_name;
        self#core_type pval_type;
        self#list self#string pval_prim;
        self#attributes pval_attributes;
        self#location pval_loc

    method type_declaration : type_declaration -> unit =
      fun {
            ptype_name;
            ptype_params;
            ptype_cstrs;
            ptype_kind;
            ptype_private;
            ptype_manifest;
            ptype_attributes;
            ptype_loc;
          } ->
        self#loc self#string ptype_name;
        self#list
          (fun (a, b) ->
            self#core_type a;
            (fun (a, b) ->
              self#variance a;
              self#injectivity b)
              b)
          ptype_params;
        self#list
          (fun (a, b, c) ->
            self#core_type a;
            self#core_type b;
            self#location c)
          ptype_cstrs;
        self#type_kind ptype_kind;
        self#private_flag ptype_private;
        self#option self#core_type ptype_manifest;
        self#attributes ptype_attributes;
        self#location ptype_loc

    method type_kind : type_kind -> unit =
      fun x ->
        match x with
        | Ptype_abstract -> ()
        | Ptype_variant a -> self#list self#constructor_declaration a
        | Ptype_record a -> self#list self#label_declaration a
        | Ptype_open -> ()

    method label_declaration : label_declaration -> unit =
      fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } ->
        self#loc self#string pld_name;
        self#mutable_flag pld_mutable;
        self#core_type pld_type;
        self#location pld_loc;
        self#attributes pld_attributes

    method constructor_declaration : constructor_declaration -> unit =
      fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } ->
        self#loc self#string pcd_name;
        self#constructor_arguments pcd_args;
        self#option self#core_type pcd_res;
        self#location pcd_loc;
        self#attributes pcd_attributes

    method constructor_arguments : constructor_arguments -> unit =
      fun x ->
        match x with
        | Pcstr_tuple a -> self#list self#core_type a
        | Pcstr_record a -> self#list self#label_declaration a

    method type_extension : type_extension -> unit =
      fun {
            ptyext_path;
            ptyext_params;
            ptyext_constructors;
            ptyext_private;
            ptyext_loc;
            ptyext_attributes;
          } ->
        self#longident_loc ptyext_path;
        self#list
          (fun (a, b) ->
            self#core_type a;
            (fun (a, b) ->
              self#variance a;
              self#injectivity b)
              b)
          ptyext_params;
        self#list self#extension_constructor ptyext_constructors;
        self#private_flag ptyext_private;
        self#location ptyext_loc;
        self#attributes ptyext_attributes

    method extension_constructor : extension_constructor -> unit =
      fun { pext_name; pext_kind; pext_loc; pext_attributes } ->
        self#loc self#string pext_name;
        self#extension_constructor_kind pext_kind;
        self#location pext_loc;
        self#attributes pext_attributes

    method type_exception : type_exception -> unit =
      fun { ptyexn_constructor; ptyexn_loc; ptyexn_attributes } ->
        self#extension_constructor ptyexn_constructor;
        self#location ptyexn_loc;
        self#attributes ptyexn_attributes

    method extension_constructor_kind : extension_constructor_kind -> unit =
      fun x ->
        match x with
        | Pext_decl (a, b) ->
            self#constructor_arguments a;
            self#option self#core_type b
        | Pext_rebind a -> self#longident_loc a

    method class_type : class_type -> unit =
      fun { pcty_desc; pcty_loc; pcty_attributes } ->
        self#class_type_desc pcty_desc;
        self#location pcty_loc;
        self#attributes pcty_attributes

    method class_type_desc : class_type_desc -> unit =
      fun x ->
        match x with
        | Pcty_constr (a, b) ->
            self#longident_loc a;
            self#list self#core_type b
        | Pcty_signature a -> self#class_signature a
        | Pcty_arrow (a, b, c) ->
            self#arg_label a;
            self#core_type b;
            self#class_type c
        | Pcty_extension a -> self#extension a
        | Pcty_open (a, b) ->
            self#open_description a;
            self#class_type b

    method class_signature : class_signature -> unit =
      fun { pcsig_self; pcsig_fields } ->
        self#core_type pcsig_self;
        self#list self#class_type_field pcsig_fields

    method class_type_field : class_type_field -> unit =
      fun { pctf_desc; pctf_loc; pctf_attributes } ->
        self#class_type_field_desc pctf_desc;
        self#location pctf_loc;
        self#attributes pctf_attributes

    method class_type_field_desc : class_type_field_desc -> unit =
      fun x ->
        match x with
        | Pctf_inherit a -> self#class_type a
        | Pctf_val a ->
            (fun (a, b, c, d) ->
              self#loc self#label a;
              self#mutable_flag b;
              self#virtual_flag c;
              self#core_type d)
              a
        | Pctf_method a ->
            (fun (a, b, c, d) ->
              self#loc self#label a;
              self#private_flag b;
              self#virtual_flag c;
              self#core_type d)
              a
        | Pctf_constraint a ->
            (fun (a, b) ->
              self#core_type a;
              self#core_type b)
              a
        | Pctf_attribute a -> self#attribute a
        | Pctf_extension a -> self#extension a

    method class_infos : 'a. ('a -> unit) -> 'a class_infos -> unit =
      fun _a
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
        self#virtual_flag pci_virt;
        self#list
          (fun (a, b) ->
            self#core_type a;
            (fun (a, b) ->
              self#variance a;
              self#injectivity b)
              b)
          pci_params;
        self#loc self#string pci_name;
        _a pci_expr;
        self#location pci_loc;
        self#attributes pci_attributes

    method class_description : class_description -> unit =
      self#class_infos self#class_type

    method class_type_declaration : class_type_declaration -> unit =
      self#class_infos self#class_type

    method class_expr : class_expr -> unit =
      fun { pcl_desc; pcl_loc; pcl_attributes } ->
        self#class_expr_desc pcl_desc;
        self#location pcl_loc;
        self#attributes pcl_attributes

    method class_expr_desc : class_expr_desc -> unit =
      fun x ->
        match x with
        | Pcl_constr (a, b) ->
            self#longident_loc a;
            self#list self#core_type b
        | Pcl_structure a -> self#class_structure a
        | Pcl_fun (a, b, c, d) ->
            self#arg_label a;
            self#option self#expression b;
            self#pattern c;
            self#class_expr d
        | Pcl_apply (a, b) ->
            self#class_expr a;
            self#list
              (fun (a, b) ->
                self#arg_label a;
                self#expression b)
              b
        | Pcl_let (a, b, c) ->
            self#rec_flag a;
            self#list self#value_binding b;
            self#class_expr c
        | Pcl_constraint (a, b) ->
            self#class_expr a;
            self#class_type b
        | Pcl_extension a -> self#extension a
        | Pcl_open (a, b) ->
            self#open_description a;
            self#class_expr b

    method class_structure : class_structure -> unit =
      fun { pcstr_self; pcstr_fields } ->
        self#pattern pcstr_self;
        self#list self#class_field pcstr_fields

    method class_field : class_field -> unit =
      fun { pcf_desc; pcf_loc; pcf_attributes } ->
        self#class_field_desc pcf_desc;
        self#location pcf_loc;
        self#attributes pcf_attributes

    method class_field_desc : class_field_desc -> unit =
      fun x ->
        match x with
        | Pcf_inherit (a, b, c) ->
            self#override_flag a;
            self#class_expr b;
            self#option (self#loc self#string) c
        | Pcf_val a ->
            (fun (a, b, c) ->
              self#loc self#label a;
              self#mutable_flag b;
              self#class_field_kind c)
              a
        | Pcf_method a ->
            (fun (a, b, c) ->
              self#loc self#label a;
              self#private_flag b;
              self#class_field_kind c)
              a
        | Pcf_constraint a ->
            (fun (a, b) ->
              self#core_type a;
              self#core_type b)
              a
        | Pcf_initializer a -> self#expression a
        | Pcf_attribute a -> self#attribute a
        | Pcf_extension a -> self#extension a

    method class_field_kind : class_field_kind -> unit =
      fun x ->
        match x with
        | Cfk_virtual a -> self#core_type a
        | Cfk_concrete (a, b) ->
            self#override_flag a;
            self#expression b

    method class_declaration : class_declaration -> unit =
      self#class_infos self#class_expr

    method module_type : module_type -> unit =
      fun { pmty_desc; pmty_loc; pmty_attributes } ->
        self#module_type_desc pmty_desc;
        self#location pmty_loc;
        self#attributes pmty_attributes

    method module_type_desc : module_type_desc -> unit =
      fun x ->
        match x with
        | Pmty_ident a -> self#longident_loc a
        | Pmty_signature a -> self#signature a
        | Pmty_functor (a, b) ->
            self#functor_parameter a;
            self#module_type b
        | Pmty_with (a, b) ->
            self#module_type a;
            self#list self#with_constraint b
        | Pmty_typeof a -> self#module_expr a
        | Pmty_extension a -> self#extension a
        | Pmty_alias a -> self#longident_loc a

    method functor_parameter : functor_parameter -> unit =
      fun x ->
        match x with
        | Unit -> ()
        | Named (a, b) ->
            self#loc (self#option self#string) a;
            self#module_type b

    method signature : signature -> unit = self#list self#signature_item

    method signature_item : signature_item -> unit =
      fun { psig_desc; psig_loc } ->
        self#signature_item_desc psig_desc;
        self#location psig_loc

    method signature_item_desc : signature_item_desc -> unit =
      fun x ->
        match x with
        | Psig_value a -> self#value_description a
        | Psig_type (a, b) ->
            self#rec_flag a;
            self#list self#type_declaration b
        | Psig_typesubst a -> self#list self#type_declaration a
        | Psig_typext a -> self#type_extension a
        | Psig_exception a -> self#type_exception a
        | Psig_module a -> self#module_declaration a
        | Psig_modsubst a -> self#module_substitution a
        | Psig_recmodule a -> self#list self#module_declaration a
        | Psig_modtype a -> self#module_type_declaration a
        | Psig_open a -> self#open_description a
        | Psig_include a -> self#include_description a
        | Psig_class a -> self#list self#class_description a
        | Psig_class_type a -> self#list self#class_type_declaration a
        | Psig_attribute a -> self#attribute a
        | Psig_extension (a, b) ->
            self#extension a;
            self#attributes b

    method module_declaration : module_declaration -> unit =
      fun { pmd_name; pmd_type; pmd_attributes; pmd_loc } ->
        self#loc (self#option self#string) pmd_name;
        self#module_type pmd_type;
        self#attributes pmd_attributes;
        self#location pmd_loc

    method module_substitution : module_substitution -> unit =
      fun { pms_name; pms_manifest; pms_attributes; pms_loc } ->
        self#loc self#string pms_name;
        self#longident_loc pms_manifest;
        self#attributes pms_attributes;
        self#location pms_loc

    method module_type_declaration : module_type_declaration -> unit =
      fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } ->
        self#loc self#string pmtd_name;
        self#option self#module_type pmtd_type;
        self#attributes pmtd_attributes;
        self#location pmtd_loc

    method open_infos : 'a. ('a -> unit) -> 'a open_infos -> unit =
      fun _a { popen_expr; popen_override; popen_loc; popen_attributes } ->
        _a popen_expr;
        self#override_flag popen_override;
        self#location popen_loc;
        self#attributes popen_attributes

    method open_description : open_description -> unit =
      self#open_infos self#longident_loc

    method open_declaration : open_declaration -> unit =
      self#open_infos self#module_expr

    method include_infos : 'a. ('a -> unit) -> 'a include_infos -> unit =
      fun _a { pincl_mod; pincl_loc; pincl_attributes } ->
        _a pincl_mod;
        self#location pincl_loc;
        self#attributes pincl_attributes

    method include_description : include_description -> unit =
      self#include_infos self#module_type

    method include_declaration : include_declaration -> unit =
      self#include_infos self#module_expr

    method with_constraint : with_constraint -> unit =
      fun x ->
        match x with
        | Pwith_type (a, b) ->
            self#longident_loc a;
            self#type_declaration b
        | Pwith_module (a, b) ->
            self#longident_loc a;
            self#longident_loc b
        | Pwith_typesubst (a, b) ->
            self#longident_loc a;
            self#type_declaration b
        | Pwith_modsubst (a, b) ->
            self#longident_loc a;
            self#longident_loc b

    method module_expr : module_expr -> unit =
      fun { pmod_desc; pmod_loc; pmod_attributes } ->
        self#module_expr_desc pmod_desc;
        self#location pmod_loc;
        self#attributes pmod_attributes

    method module_expr_desc : module_expr_desc -> unit =
      fun x ->
        match x with
        | Pmod_ident a -> self#longident_loc a
        | Pmod_structure a -> self#structure a
        | Pmod_functor (a, b) ->
            self#functor_parameter a;
            self#module_expr b
        | Pmod_apply (a, b) ->
            self#module_expr a;
            self#module_expr b
        | Pmod_constraint (a, b) ->
            self#module_expr a;
            self#module_type b
        | Pmod_unpack a -> self#expression a
        | Pmod_extension a -> self#extension a

    method structure : structure -> unit = self#list self#structure_item

    method structure_item : structure_item -> unit =
      fun { pstr_desc; pstr_loc } ->
        self#structure_item_desc pstr_desc;
        self#location pstr_loc

    method structure_item_desc : structure_item_desc -> unit =
      fun x ->
        match x with
        | Pstr_eval (a, b) ->
            self#expression a;
            self#attributes b
        | Pstr_value (a, b) ->
            self#rec_flag a;
            self#list self#value_binding b
        | Pstr_primitive a -> self#value_description a
        | Pstr_type (a, b) ->
            self#rec_flag a;
            self#list self#type_declaration b
        | Pstr_typext a -> self#type_extension a
        | Pstr_exception a -> self#type_exception a
        | Pstr_module a -> self#module_binding a
        | Pstr_recmodule a -> self#list self#module_binding a
        | Pstr_modtype a -> self#module_type_declaration a
        | Pstr_open a -> self#open_declaration a
        | Pstr_class a -> self#list self#class_declaration a
        | Pstr_class_type a -> self#list self#class_type_declaration a
        | Pstr_include a -> self#include_declaration a
        | Pstr_attribute a -> self#attribute a
        | Pstr_extension (a, b) ->
            self#extension a;
            self#attributes b

    method value_binding : value_binding -> unit =
      fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } ->
        self#pattern pvb_pat;
        self#expression pvb_expr;
        self#attributes pvb_attributes;
        self#location pvb_loc

    method module_binding : module_binding -> unit =
      fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc } ->
        self#loc (self#option self#string) pmb_name;
        self#module_expr pmb_expr;
        self#attributes pmb_attributes;
        self#location pmb_loc

    method toplevel_phrase : toplevel_phrase -> unit =
      fun x ->
        match x with
        | Ptop_def a -> self#structure a
        | Ptop_dir a -> self#toplevel_directive a

    method toplevel_directive : toplevel_directive -> unit =
      fun { pdir_name; pdir_arg; pdir_loc } ->
        self#loc self#string pdir_name;
        self#option self#directive_argument pdir_arg;
        self#location pdir_loc

    method directive_argument : directive_argument -> unit =
      fun { pdira_desc; pdira_loc } ->
        self#directive_argument_desc pdira_desc;
        self#location pdira_loc

    method directive_argument_desc : directive_argument_desc -> unit =
      fun x ->
        match x with
        | Pdir_string a -> self#string a
        | Pdir_int (a, b) ->
            self#string a;
            self#option self#char b
        | Pdir_ident a -> self#longident a
        | Pdir_bool a -> self#bool a

    method cases : cases -> unit = self#list self#case
  end

class virtual ['acc] fold =
  object (self)
    method virtual bool : bool -> 'acc -> 'acc

    method virtual char : char -> 'acc -> 'acc

    method virtual int : int -> 'acc -> 'acc

    method virtual list : 'a. ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc

    method virtual option
        : 'a. ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc

    method virtual string : string -> 'acc -> 'acc

    method position : position -> 'acc -> 'acc =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum } acc ->
        let acc = self#string pos_fname acc in
        let acc = self#int pos_lnum acc in
        let acc = self#int pos_bol acc in
        let acc = self#int pos_cnum acc in
        acc

    method location : location -> 'acc -> 'acc =
      fun { loc_start; loc_end; loc_ghost } acc ->
        let acc = self#position loc_start acc in
        let acc = self#position loc_end acc in
        let acc = self#bool loc_ghost acc in
        acc

    method location_stack : location_stack -> 'acc -> 'acc =
      self#list self#location

    method loc : 'a. ('a -> 'acc -> 'acc) -> 'a loc -> 'acc -> 'acc =
      fun _a { txt; loc } acc ->
        let acc = _a txt acc in
        let acc = self#location loc acc in
        acc

    method longident : longident -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Lident a -> self#string a acc
        | Ldot (a, b) ->
            let acc = self#longident a acc in
            let acc = self#string b acc in
            acc
        | Lapply (a, b) ->
            let acc = self#longident a acc in
            let acc = self#longident b acc in
            acc

    method longident_loc : longident_loc -> 'acc -> 'acc =
      self#loc self#longident

    method rec_flag : rec_flag -> 'acc -> 'acc = fun _ acc -> acc

    method direction_flag : direction_flag -> 'acc -> 'acc = fun _ acc -> acc

    method private_flag : private_flag -> 'acc -> 'acc = fun _ acc -> acc

    method mutable_flag : mutable_flag -> 'acc -> 'acc = fun _ acc -> acc

    method virtual_flag : virtual_flag -> 'acc -> 'acc = fun _ acc -> acc

    method override_flag : override_flag -> 'acc -> 'acc = fun _ acc -> acc

    method closed_flag : closed_flag -> 'acc -> 'acc = fun _ acc -> acc

    method label : label -> 'acc -> 'acc = self#string

    method arg_label : arg_label -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Nolabel -> acc
        | Labelled a -> self#string a acc
        | Optional a -> self#string a acc

    method variance : variance -> 'acc -> 'acc = fun _ acc -> acc

    method injectivity : injectivity -> 'acc -> 'acc = fun _ acc -> acc

    method constant : constant -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pconst_integer (a, b) ->
            let acc = self#string a acc in
            let acc = self#option self#char b acc in
            acc
        | Pconst_char a -> self#char a acc
        | Pconst_string (a, b, c) ->
            let acc = self#string a acc in
            let acc = self#location b acc in
            let acc = self#option self#string c acc in
            acc
        | Pconst_float (a, b) ->
            let acc = self#string a acc in
            let acc = self#option self#char b acc in
            acc

    method attribute : attribute -> 'acc -> 'acc =
      fun { attr_name; attr_payload; attr_loc } acc ->
        let acc = self#loc self#string attr_name acc in
        let acc = self#payload attr_payload acc in
        let acc = self#location attr_loc acc in
        acc

    method extension : extension -> 'acc -> 'acc =
      fun (a, b) acc ->
        let acc = self#loc self#string a acc in
        let acc = self#payload b acc in
        acc

    method attributes : attributes -> 'acc -> 'acc = self#list self#attribute

    method payload : payload -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | PStr a -> self#structure a acc
        | PSig a -> self#signature a acc
        | PTyp a -> self#core_type a acc
        | PPat (a, b) ->
            let acc = self#pattern a acc in
            let acc = self#option self#expression b acc in
            acc

    method core_type : core_type -> 'acc -> 'acc =
      fun { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes } acc ->
        let acc = self#core_type_desc ptyp_desc acc in
        let acc = self#location ptyp_loc acc in
        let acc = self#location_stack ptyp_loc_stack acc in
        let acc = self#attributes ptyp_attributes acc in
        acc

    method core_type_desc : core_type_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Ptyp_any -> acc
        | Ptyp_var a -> self#string a acc
        | Ptyp_arrow (a, b, c) ->
            let acc = self#arg_label a acc in
            let acc = self#core_type b acc in
            let acc = self#core_type c acc in
            acc
        | Ptyp_tuple a -> self#list self#core_type a acc
        | Ptyp_constr (a, b) ->
            let acc = self#longident_loc a acc in
            let acc = self#list self#core_type b acc in
            acc
        | Ptyp_object (a, b) ->
            let acc = self#list self#object_field a acc in
            let acc = self#closed_flag b acc in
            acc
        | Ptyp_class (a, b) ->
            let acc = self#longident_loc a acc in
            let acc = self#list self#core_type b acc in
            acc
        | Ptyp_alias (a, b) ->
            let acc = self#core_type a acc in
            let acc = self#string b acc in
            acc
        | Ptyp_variant (a, b, c) ->
            let acc = self#list self#row_field a acc in
            let acc = self#closed_flag b acc in
            let acc = self#option (self#list self#label) c acc in
            acc
        | Ptyp_poly (a, b) ->
            let acc = self#list (self#loc self#string) a acc in
            let acc = self#core_type b acc in
            acc
        | Ptyp_package a -> self#package_type a acc
        | Ptyp_extension a -> self#extension a acc

    method package_type : package_type -> 'acc -> 'acc =
      fun (a, b) acc ->
        let acc = self#longident_loc a acc in
        let acc =
          self#list
            (fun (a, b) acc ->
              let acc = self#longident_loc a acc in
              let acc = self#core_type b acc in
              acc)
            b acc
        in
        acc

    method row_field : row_field -> 'acc -> 'acc =
      fun { prf_desc; prf_loc; prf_attributes } acc ->
        let acc = self#row_field_desc prf_desc acc in
        let acc = self#location prf_loc acc in
        let acc = self#attributes prf_attributes acc in
        acc

    method row_field_desc : row_field_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Rtag (a, b, c) ->
            let acc = self#loc self#label a acc in
            let acc = self#bool b acc in
            let acc = self#list self#core_type c acc in
            acc
        | Rinherit a -> self#core_type a acc

    method object_field : object_field -> 'acc -> 'acc =
      fun { pof_desc; pof_loc; pof_attributes } acc ->
        let acc = self#object_field_desc pof_desc acc in
        let acc = self#location pof_loc acc in
        let acc = self#attributes pof_attributes acc in
        acc

    method object_field_desc : object_field_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Otag (a, b) ->
            let acc = self#loc self#label a acc in
            let acc = self#core_type b acc in
            acc
        | Oinherit a -> self#core_type a acc

    method pattern : pattern -> 'acc -> 'acc =
      fun { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes } acc ->
        let acc = self#pattern_desc ppat_desc acc in
        let acc = self#location ppat_loc acc in
        let acc = self#location_stack ppat_loc_stack acc in
        let acc = self#attributes ppat_attributes acc in
        acc

    method pattern_desc : pattern_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Ppat_any -> acc
        | Ppat_var a -> self#loc self#string a acc
        | Ppat_alias (a, b) ->
            let acc = self#pattern a acc in
            let acc = self#loc self#string b acc in
            acc
        | Ppat_constant a -> self#constant a acc
        | Ppat_interval (a, b) ->
            let acc = self#constant a acc in
            let acc = self#constant b acc in
            acc
        | Ppat_tuple a -> self#list self#pattern a acc
        | Ppat_construct (a, b) ->
            let acc = self#longident_loc a acc in
            let acc = self#option self#pattern b acc in
            acc
        | Ppat_variant (a, b) ->
            let acc = self#label a acc in
            let acc = self#option self#pattern b acc in
            acc
        | Ppat_record (a, b) ->
            let acc =
              self#list
                (fun (a, b) acc ->
                  let acc = self#longident_loc a acc in
                  let acc = self#pattern b acc in
                  acc)
                a acc
            in
            let acc = self#closed_flag b acc in
            acc
        | Ppat_array a -> self#list self#pattern a acc
        | Ppat_or (a, b) ->
            let acc = self#pattern a acc in
            let acc = self#pattern b acc in
            acc
        | Ppat_constraint (a, b) ->
            let acc = self#pattern a acc in
            let acc = self#core_type b acc in
            acc
        | Ppat_type a -> self#longident_loc a acc
        | Ppat_lazy a -> self#pattern a acc
        | Ppat_unpack a -> self#loc (self#option self#string) a acc
        | Ppat_exception a -> self#pattern a acc
        | Ppat_extension a -> self#extension a acc
        | Ppat_open (a, b) ->
            let acc = self#longident_loc a acc in
            let acc = self#pattern b acc in
            acc

    method expression : expression -> 'acc -> 'acc =
      fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } acc ->
        let acc = self#expression_desc pexp_desc acc in
        let acc = self#location pexp_loc acc in
        let acc = self#location_stack pexp_loc_stack acc in
        let acc = self#attributes pexp_attributes acc in
        acc

    method expression_desc : expression_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pexp_ident a -> self#longident_loc a acc
        | Pexp_constant a -> self#constant a acc
        | Pexp_let (a, b, c) ->
            let acc = self#rec_flag a acc in
            let acc = self#list self#value_binding b acc in
            let acc = self#expression c acc in
            acc
        | Pexp_function a -> self#cases a acc
        | Pexp_fun (a, b, c, d) ->
            let acc = self#arg_label a acc in
            let acc = self#option self#expression b acc in
            let acc = self#pattern c acc in
            let acc = self#expression d acc in
            acc
        | Pexp_apply (a, b) ->
            let acc = self#expression a acc in
            let acc =
              self#list
                (fun (a, b) acc ->
                  let acc = self#arg_label a acc in
                  let acc = self#expression b acc in
                  acc)
                b acc
            in
            acc
        | Pexp_match (a, b) ->
            let acc = self#expression a acc in
            let acc = self#cases b acc in
            acc
        | Pexp_try (a, b) ->
            let acc = self#expression a acc in
            let acc = self#cases b acc in
            acc
        | Pexp_tuple a -> self#list self#expression a acc
        | Pexp_construct (a, b) ->
            let acc = self#longident_loc a acc in
            let acc = self#option self#expression b acc in
            acc
        | Pexp_variant (a, b) ->
            let acc = self#label a acc in
            let acc = self#option self#expression b acc in
            acc
        | Pexp_record (a, b) ->
            let acc =
              self#list
                (fun (a, b) acc ->
                  let acc = self#longident_loc a acc in
                  let acc = self#expression b acc in
                  acc)
                a acc
            in
            let acc = self#option self#expression b acc in
            acc
        | Pexp_field (a, b) ->
            let acc = self#expression a acc in
            let acc = self#longident_loc b acc in
            acc
        | Pexp_setfield (a, b, c) ->
            let acc = self#expression a acc in
            let acc = self#longident_loc b acc in
            let acc = self#expression c acc in
            acc
        | Pexp_array a -> self#list self#expression a acc
        | Pexp_ifthenelse (a, b, c) ->
            let acc = self#expression a acc in
            let acc = self#expression b acc in
            let acc = self#option self#expression c acc in
            acc
        | Pexp_sequence (a, b) ->
            let acc = self#expression a acc in
            let acc = self#expression b acc in
            acc
        | Pexp_while (a, b) ->
            let acc = self#expression a acc in
            let acc = self#expression b acc in
            acc
        | Pexp_for (a, b, c, d, e) ->
            let acc = self#pattern a acc in
            let acc = self#expression b acc in
            let acc = self#expression c acc in
            let acc = self#direction_flag d acc in
            let acc = self#expression e acc in
            acc
        | Pexp_constraint (a, b) ->
            let acc = self#expression a acc in
            let acc = self#core_type b acc in
            acc
        | Pexp_coerce (a, b, c) ->
            let acc = self#expression a acc in
            let acc = self#option self#core_type b acc in
            let acc = self#core_type c acc in
            acc
        | Pexp_send (a, b) ->
            let acc = self#expression a acc in
            let acc = self#loc self#label b acc in
            acc
        | Pexp_new a -> self#longident_loc a acc
        | Pexp_setinstvar (a, b) ->
            let acc = self#loc self#label a acc in
            let acc = self#expression b acc in
            acc
        | Pexp_override a ->
            self#list
              (fun (a, b) acc ->
                let acc = self#loc self#label a acc in
                let acc = self#expression b acc in
                acc)
              a acc
        | Pexp_letmodule (a, b, c) ->
            let acc = self#loc (self#option self#string) a acc in
            let acc = self#module_expr b acc in
            let acc = self#expression c acc in
            acc
        | Pexp_letexception (a, b) ->
            let acc = self#extension_constructor a acc in
            let acc = self#expression b acc in
            acc
        | Pexp_assert a -> self#expression a acc
        | Pexp_lazy a -> self#expression a acc
        | Pexp_poly (a, b) ->
            let acc = self#expression a acc in
            let acc = self#option self#core_type b acc in
            acc
        | Pexp_object a -> self#class_structure a acc
        | Pexp_newtype (a, b) ->
            let acc = self#loc self#string a acc in
            let acc = self#expression b acc in
            acc
        | Pexp_pack a -> self#module_expr a acc
        | Pexp_open (a, b) ->
            let acc = self#open_declaration a acc in
            let acc = self#expression b acc in
            acc
        | Pexp_letop a -> self#letop a acc
        | Pexp_extension a -> self#extension a acc
        | Pexp_unreachable -> acc

    method case : case -> 'acc -> 'acc =
      fun { pc_lhs; pc_guard; pc_rhs } acc ->
        let acc = self#pattern pc_lhs acc in
        let acc = self#option self#expression pc_guard acc in
        let acc = self#expression pc_rhs acc in
        acc

    method letop : letop -> 'acc -> 'acc =
      fun { let_; ands; body } acc ->
        let acc = self#binding_op let_ acc in
        let acc = self#list self#binding_op ands acc in
        let acc = self#expression body acc in
        acc

    method binding_op : binding_op -> 'acc -> 'acc =
      fun { pbop_op; pbop_pat; pbop_exp; pbop_loc } acc ->
        let acc = self#loc self#string pbop_op acc in
        let acc = self#pattern pbop_pat acc in
        let acc = self#expression pbop_exp acc in
        let acc = self#location pbop_loc acc in
        acc

    method value_description : value_description -> 'acc -> 'acc =
      fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } acc ->
        let acc = self#loc self#string pval_name acc in
        let acc = self#core_type pval_type acc in
        let acc = self#list self#string pval_prim acc in
        let acc = self#attributes pval_attributes acc in
        let acc = self#location pval_loc acc in
        acc

    method type_declaration : type_declaration -> 'acc -> 'acc =
      fun {
            ptype_name;
            ptype_params;
            ptype_cstrs;
            ptype_kind;
            ptype_private;
            ptype_manifest;
            ptype_attributes;
            ptype_loc;
          } acc ->
        let acc = self#loc self#string ptype_name acc in
        let acc =
          self#list
            (fun (a, b) acc ->
              let acc = self#core_type a acc in
              let acc =
                (fun (a, b) acc ->
                  let acc = self#variance a acc in
                  let acc = self#injectivity b acc in
                  acc)
                  b acc
              in
              acc)
            ptype_params acc
        in
        let acc =
          self#list
            (fun (a, b, c) acc ->
              let acc = self#core_type a acc in
              let acc = self#core_type b acc in
              let acc = self#location c acc in
              acc)
            ptype_cstrs acc
        in
        let acc = self#type_kind ptype_kind acc in
        let acc = self#private_flag ptype_private acc in
        let acc = self#option self#core_type ptype_manifest acc in
        let acc = self#attributes ptype_attributes acc in
        let acc = self#location ptype_loc acc in
        acc

    method type_kind : type_kind -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Ptype_abstract -> acc
        | Ptype_variant a -> self#list self#constructor_declaration a acc
        | Ptype_record a -> self#list self#label_declaration a acc
        | Ptype_open -> acc

    method label_declaration : label_declaration -> 'acc -> 'acc =
      fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } acc ->
        let acc = self#loc self#string pld_name acc in
        let acc = self#mutable_flag pld_mutable acc in
        let acc = self#core_type pld_type acc in
        let acc = self#location pld_loc acc in
        let acc = self#attributes pld_attributes acc in
        acc

    method constructor_declaration : constructor_declaration -> 'acc -> 'acc =
      fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } acc ->
        let acc = self#loc self#string pcd_name acc in
        let acc = self#constructor_arguments pcd_args acc in
        let acc = self#option self#core_type pcd_res acc in
        let acc = self#location pcd_loc acc in
        let acc = self#attributes pcd_attributes acc in
        acc

    method constructor_arguments : constructor_arguments -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pcstr_tuple a -> self#list self#core_type a acc
        | Pcstr_record a -> self#list self#label_declaration a acc

    method type_extension : type_extension -> 'acc -> 'acc =
      fun {
            ptyext_path;
            ptyext_params;
            ptyext_constructors;
            ptyext_private;
            ptyext_loc;
            ptyext_attributes;
          } acc ->
        let acc = self#longident_loc ptyext_path acc in
        let acc =
          self#list
            (fun (a, b) acc ->
              let acc = self#core_type a acc in
              let acc =
                (fun (a, b) acc ->
                  let acc = self#variance a acc in
                  let acc = self#injectivity b acc in
                  acc)
                  b acc
              in
              acc)
            ptyext_params acc
        in
        let acc =
          self#list self#extension_constructor ptyext_constructors acc
        in
        let acc = self#private_flag ptyext_private acc in
        let acc = self#location ptyext_loc acc in
        let acc = self#attributes ptyext_attributes acc in
        acc

    method extension_constructor : extension_constructor -> 'acc -> 'acc =
      fun { pext_name; pext_kind; pext_loc; pext_attributes } acc ->
        let acc = self#loc self#string pext_name acc in
        let acc = self#extension_constructor_kind pext_kind acc in
        let acc = self#location pext_loc acc in
        let acc = self#attributes pext_attributes acc in
        acc

    method type_exception : type_exception -> 'acc -> 'acc =
      fun { ptyexn_constructor; ptyexn_loc; ptyexn_attributes } acc ->
        let acc = self#extension_constructor ptyexn_constructor acc in
        let acc = self#location ptyexn_loc acc in
        let acc = self#attributes ptyexn_attributes acc in
        acc

    method extension_constructor_kind
        : extension_constructor_kind -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pext_decl (a, b) ->
            let acc = self#constructor_arguments a acc in
            let acc = self#option self#core_type b acc in
            acc
        | Pext_rebind a -> self#longident_loc a acc

    method class_type : class_type -> 'acc -> 'acc =
      fun { pcty_desc; pcty_loc; pcty_attributes } acc ->
        let acc = self#class_type_desc pcty_desc acc in
        let acc = self#location pcty_loc acc in
        let acc = self#attributes pcty_attributes acc in
        acc

    method class_type_desc : class_type_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pcty_constr (a, b) ->
            let acc = self#longident_loc a acc in
            let acc = self#list self#core_type b acc in
            acc
        | Pcty_signature a -> self#class_signature a acc
        | Pcty_arrow (a, b, c) ->
            let acc = self#arg_label a acc in
            let acc = self#core_type b acc in
            let acc = self#class_type c acc in
            acc
        | Pcty_extension a -> self#extension a acc
        | Pcty_open (a, b) ->
            let acc = self#open_description a acc in
            let acc = self#class_type b acc in
            acc

    method class_signature : class_signature -> 'acc -> 'acc =
      fun { pcsig_self; pcsig_fields } acc ->
        let acc = self#core_type pcsig_self acc in
        let acc = self#list self#class_type_field pcsig_fields acc in
        acc

    method class_type_field : class_type_field -> 'acc -> 'acc =
      fun { pctf_desc; pctf_loc; pctf_attributes } acc ->
        let acc = self#class_type_field_desc pctf_desc acc in
        let acc = self#location pctf_loc acc in
        let acc = self#attributes pctf_attributes acc in
        acc

    method class_type_field_desc : class_type_field_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pctf_inherit a -> self#class_type a acc
        | Pctf_val a ->
            (fun (a, b, c, d) acc ->
              let acc = self#loc self#label a acc in
              let acc = self#mutable_flag b acc in
              let acc = self#virtual_flag c acc in
              let acc = self#core_type d acc in
              acc)
              a acc
        | Pctf_method a ->
            (fun (a, b, c, d) acc ->
              let acc = self#loc self#label a acc in
              let acc = self#private_flag b acc in
              let acc = self#virtual_flag c acc in
              let acc = self#core_type d acc in
              acc)
              a acc
        | Pctf_constraint a ->
            (fun (a, b) acc ->
              let acc = self#core_type a acc in
              let acc = self#core_type b acc in
              acc)
              a acc
        | Pctf_attribute a -> self#attribute a acc
        | Pctf_extension a -> self#extension a acc

    method class_infos
        : 'a. ('a -> 'acc -> 'acc) -> 'a class_infos -> 'acc -> 'acc =
      fun _a
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
          acc ->
        let acc = self#virtual_flag pci_virt acc in
        let acc =
          self#list
            (fun (a, b) acc ->
              let acc = self#core_type a acc in
              let acc =
                (fun (a, b) acc ->
                  let acc = self#variance a acc in
                  let acc = self#injectivity b acc in
                  acc)
                  b acc
              in
              acc)
            pci_params acc
        in
        let acc = self#loc self#string pci_name acc in
        let acc = _a pci_expr acc in
        let acc = self#location pci_loc acc in
        let acc = self#attributes pci_attributes acc in
        acc

    method class_description : class_description -> 'acc -> 'acc =
      self#class_infos self#class_type

    method class_type_declaration : class_type_declaration -> 'acc -> 'acc =
      self#class_infos self#class_type

    method class_expr : class_expr -> 'acc -> 'acc =
      fun { pcl_desc; pcl_loc; pcl_attributes } acc ->
        let acc = self#class_expr_desc pcl_desc acc in
        let acc = self#location pcl_loc acc in
        let acc = self#attributes pcl_attributes acc in
        acc

    method class_expr_desc : class_expr_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pcl_constr (a, b) ->
            let acc = self#longident_loc a acc in
            let acc = self#list self#core_type b acc in
            acc
        | Pcl_structure a -> self#class_structure a acc
        | Pcl_fun (a, b, c, d) ->
            let acc = self#arg_label a acc in
            let acc = self#option self#expression b acc in
            let acc = self#pattern c acc in
            let acc = self#class_expr d acc in
            acc
        | Pcl_apply (a, b) ->
            let acc = self#class_expr a acc in
            let acc =
              self#list
                (fun (a, b) acc ->
                  let acc = self#arg_label a acc in
                  let acc = self#expression b acc in
                  acc)
                b acc
            in
            acc
        | Pcl_let (a, b, c) ->
            let acc = self#rec_flag a acc in
            let acc = self#list self#value_binding b acc in
            let acc = self#class_expr c acc in
            acc
        | Pcl_constraint (a, b) ->
            let acc = self#class_expr a acc in
            let acc = self#class_type b acc in
            acc
        | Pcl_extension a -> self#extension a acc
        | Pcl_open (a, b) ->
            let acc = self#open_description a acc in
            let acc = self#class_expr b acc in
            acc

    method class_structure : class_structure -> 'acc -> 'acc =
      fun { pcstr_self; pcstr_fields } acc ->
        let acc = self#pattern pcstr_self acc in
        let acc = self#list self#class_field pcstr_fields acc in
        acc

    method class_field : class_field -> 'acc -> 'acc =
      fun { pcf_desc; pcf_loc; pcf_attributes } acc ->
        let acc = self#class_field_desc pcf_desc acc in
        let acc = self#location pcf_loc acc in
        let acc = self#attributes pcf_attributes acc in
        acc

    method class_field_desc : class_field_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pcf_inherit (a, b, c) ->
            let acc = self#override_flag a acc in
            let acc = self#class_expr b acc in
            let acc = self#option (self#loc self#string) c acc in
            acc
        | Pcf_val a ->
            (fun (a, b, c) acc ->
              let acc = self#loc self#label a acc in
              let acc = self#mutable_flag b acc in
              let acc = self#class_field_kind c acc in
              acc)
              a acc
        | Pcf_method a ->
            (fun (a, b, c) acc ->
              let acc = self#loc self#label a acc in
              let acc = self#private_flag b acc in
              let acc = self#class_field_kind c acc in
              acc)
              a acc
        | Pcf_constraint a ->
            (fun (a, b) acc ->
              let acc = self#core_type a acc in
              let acc = self#core_type b acc in
              acc)
              a acc
        | Pcf_initializer a -> self#expression a acc
        | Pcf_attribute a -> self#attribute a acc
        | Pcf_extension a -> self#extension a acc

    method class_field_kind : class_field_kind -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Cfk_virtual a -> self#core_type a acc
        | Cfk_concrete (a, b) ->
            let acc = self#override_flag a acc in
            let acc = self#expression b acc in
            acc

    method class_declaration : class_declaration -> 'acc -> 'acc =
      self#class_infos self#class_expr

    method module_type : module_type -> 'acc -> 'acc =
      fun { pmty_desc; pmty_loc; pmty_attributes } acc ->
        let acc = self#module_type_desc pmty_desc acc in
        let acc = self#location pmty_loc acc in
        let acc = self#attributes pmty_attributes acc in
        acc

    method module_type_desc : module_type_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pmty_ident a -> self#longident_loc a acc
        | Pmty_signature a -> self#signature a acc
        | Pmty_functor (a, b) ->
            let acc = self#functor_parameter a acc in
            let acc = self#module_type b acc in
            acc
        | Pmty_with (a, b) ->
            let acc = self#module_type a acc in
            let acc = self#list self#with_constraint b acc in
            acc
        | Pmty_typeof a -> self#module_expr a acc
        | Pmty_extension a -> self#extension a acc
        | Pmty_alias a -> self#longident_loc a acc

    method functor_parameter : functor_parameter -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Unit -> acc
        | Named (a, b) ->
            let acc = self#loc (self#option self#string) a acc in
            let acc = self#module_type b acc in
            acc

    method signature : signature -> 'acc -> 'acc = self#list self#signature_item

    method signature_item : signature_item -> 'acc -> 'acc =
      fun { psig_desc; psig_loc } acc ->
        let acc = self#signature_item_desc psig_desc acc in
        let acc = self#location psig_loc acc in
        acc

    method signature_item_desc : signature_item_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Psig_value a -> self#value_description a acc
        | Psig_type (a, b) ->
            let acc = self#rec_flag a acc in
            let acc = self#list self#type_declaration b acc in
            acc
        | Psig_typesubst a -> self#list self#type_declaration a acc
        | Psig_typext a -> self#type_extension a acc
        | Psig_exception a -> self#type_exception a acc
        | Psig_module a -> self#module_declaration a acc
        | Psig_modsubst a -> self#module_substitution a acc
        | Psig_recmodule a -> self#list self#module_declaration a acc
        | Psig_modtype a -> self#module_type_declaration a acc
        | Psig_open a -> self#open_description a acc
        | Psig_include a -> self#include_description a acc
        | Psig_class a -> self#list self#class_description a acc
        | Psig_class_type a -> self#list self#class_type_declaration a acc
        | Psig_attribute a -> self#attribute a acc
        | Psig_extension (a, b) ->
            let acc = self#extension a acc in
            let acc = self#attributes b acc in
            acc

    method module_declaration : module_declaration -> 'acc -> 'acc =
      fun { pmd_name; pmd_type; pmd_attributes; pmd_loc } acc ->
        let acc = self#loc (self#option self#string) pmd_name acc in
        let acc = self#module_type pmd_type acc in
        let acc = self#attributes pmd_attributes acc in
        let acc = self#location pmd_loc acc in
        acc

    method module_substitution : module_substitution -> 'acc -> 'acc =
      fun { pms_name; pms_manifest; pms_attributes; pms_loc } acc ->
        let acc = self#loc self#string pms_name acc in
        let acc = self#longident_loc pms_manifest acc in
        let acc = self#attributes pms_attributes acc in
        let acc = self#location pms_loc acc in
        acc

    method module_type_declaration : module_type_declaration -> 'acc -> 'acc =
      fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } acc ->
        let acc = self#loc self#string pmtd_name acc in
        let acc = self#option self#module_type pmtd_type acc in
        let acc = self#attributes pmtd_attributes acc in
        let acc = self#location pmtd_loc acc in
        acc

    method open_infos
        : 'a. ('a -> 'acc -> 'acc) -> 'a open_infos -> 'acc -> 'acc =
      fun _a { popen_expr; popen_override; popen_loc; popen_attributes } acc ->
        let acc = _a popen_expr acc in
        let acc = self#override_flag popen_override acc in
        let acc = self#location popen_loc acc in
        let acc = self#attributes popen_attributes acc in
        acc

    method open_description : open_description -> 'acc -> 'acc =
      self#open_infos self#longident_loc

    method open_declaration : open_declaration -> 'acc -> 'acc =
      self#open_infos self#module_expr

    method include_infos
        : 'a. ('a -> 'acc -> 'acc) -> 'a include_infos -> 'acc -> 'acc =
      fun _a { pincl_mod; pincl_loc; pincl_attributes } acc ->
        let acc = _a pincl_mod acc in
        let acc = self#location pincl_loc acc in
        let acc = self#attributes pincl_attributes acc in
        acc

    method include_description : include_description -> 'acc -> 'acc =
      self#include_infos self#module_type

    method include_declaration : include_declaration -> 'acc -> 'acc =
      self#include_infos self#module_expr

    method with_constraint : with_constraint -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pwith_type (a, b) ->
            let acc = self#longident_loc a acc in
            let acc = self#type_declaration b acc in
            acc
        | Pwith_module (a, b) ->
            let acc = self#longident_loc a acc in
            let acc = self#longident_loc b acc in
            acc
        | Pwith_typesubst (a, b) ->
            let acc = self#longident_loc a acc in
            let acc = self#type_declaration b acc in
            acc
        | Pwith_modsubst (a, b) ->
            let acc = self#longident_loc a acc in
            let acc = self#longident_loc b acc in
            acc

    method module_expr : module_expr -> 'acc -> 'acc =
      fun { pmod_desc; pmod_loc; pmod_attributes } acc ->
        let acc = self#module_expr_desc pmod_desc acc in
        let acc = self#location pmod_loc acc in
        let acc = self#attributes pmod_attributes acc in
        acc

    method module_expr_desc : module_expr_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pmod_ident a -> self#longident_loc a acc
        | Pmod_structure a -> self#structure a acc
        | Pmod_functor (a, b) ->
            let acc = self#functor_parameter a acc in
            let acc = self#module_expr b acc in
            acc
        | Pmod_apply (a, b) ->
            let acc = self#module_expr a acc in
            let acc = self#module_expr b acc in
            acc
        | Pmod_constraint (a, b) ->
            let acc = self#module_expr a acc in
            let acc = self#module_type b acc in
            acc
        | Pmod_unpack a -> self#expression a acc
        | Pmod_extension a -> self#extension a acc

    method structure : structure -> 'acc -> 'acc = self#list self#structure_item

    method structure_item : structure_item -> 'acc -> 'acc =
      fun { pstr_desc; pstr_loc } acc ->
        let acc = self#structure_item_desc pstr_desc acc in
        let acc = self#location pstr_loc acc in
        acc

    method structure_item_desc : structure_item_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pstr_eval (a, b) ->
            let acc = self#expression a acc in
            let acc = self#attributes b acc in
            acc
        | Pstr_value (a, b) ->
            let acc = self#rec_flag a acc in
            let acc = self#list self#value_binding b acc in
            acc
        | Pstr_primitive a -> self#value_description a acc
        | Pstr_type (a, b) ->
            let acc = self#rec_flag a acc in
            let acc = self#list self#type_declaration b acc in
            acc
        | Pstr_typext a -> self#type_extension a acc
        | Pstr_exception a -> self#type_exception a acc
        | Pstr_module a -> self#module_binding a acc
        | Pstr_recmodule a -> self#list self#module_binding a acc
        | Pstr_modtype a -> self#module_type_declaration a acc
        | Pstr_open a -> self#open_declaration a acc
        | Pstr_class a -> self#list self#class_declaration a acc
        | Pstr_class_type a -> self#list self#class_type_declaration a acc
        | Pstr_include a -> self#include_declaration a acc
        | Pstr_attribute a -> self#attribute a acc
        | Pstr_extension (a, b) ->
            let acc = self#extension a acc in
            let acc = self#attributes b acc in
            acc

    method value_binding : value_binding -> 'acc -> 'acc =
      fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } acc ->
        let acc = self#pattern pvb_pat acc in
        let acc = self#expression pvb_expr acc in
        let acc = self#attributes pvb_attributes acc in
        let acc = self#location pvb_loc acc in
        acc

    method module_binding : module_binding -> 'acc -> 'acc =
      fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc } acc ->
        let acc = self#loc (self#option self#string) pmb_name acc in
        let acc = self#module_expr pmb_expr acc in
        let acc = self#attributes pmb_attributes acc in
        let acc = self#location pmb_loc acc in
        acc

    method toplevel_phrase : toplevel_phrase -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Ptop_def a -> self#structure a acc
        | Ptop_dir a -> self#toplevel_directive a acc

    method toplevel_directive : toplevel_directive -> 'acc -> 'acc =
      fun { pdir_name; pdir_arg; pdir_loc } acc ->
        let acc = self#loc self#string pdir_name acc in
        let acc = self#option self#directive_argument pdir_arg acc in
        let acc = self#location pdir_loc acc in
        acc

    method directive_argument : directive_argument -> 'acc -> 'acc =
      fun { pdira_desc; pdira_loc } acc ->
        let acc = self#directive_argument_desc pdira_desc acc in
        let acc = self#location pdira_loc acc in
        acc

    method directive_argument_desc : directive_argument_desc -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pdir_string a -> self#string a acc
        | Pdir_int (a, b) ->
            let acc = self#string a acc in
            let acc = self#option self#char b acc in
            acc
        | Pdir_ident a -> self#longident a acc
        | Pdir_bool a -> self#bool a acc

    method cases : cases -> 'acc -> 'acc = self#list self#case
  end

class virtual ['acc] fold_map =
  object (self)
    method virtual bool : bool -> 'acc -> bool * 'acc

    method virtual char : char -> 'acc -> char * 'acc

    method virtual int : int -> 'acc -> int * 'acc

    method virtual list
        : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a list -> 'acc -> 'a list * 'acc

    method virtual option
        : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a option -> 'acc -> 'a option * 'acc

    method virtual string : string -> 'acc -> string * 'acc

    method position : position -> 'acc -> position * 'acc =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum } acc ->
        let pos_fname, acc = self#string pos_fname acc in
        let pos_lnum, acc = self#int pos_lnum acc in
        let pos_bol, acc = self#int pos_bol acc in
        let pos_cnum, acc = self#int pos_cnum acc in
        ({ pos_fname; pos_lnum; pos_bol; pos_cnum }, acc)

    method location : location -> 'acc -> location * 'acc =
      fun { loc_start; loc_end; loc_ghost } acc ->
        let loc_start, acc = self#position loc_start acc in
        let loc_end, acc = self#position loc_end acc in
        let loc_ghost, acc = self#bool loc_ghost acc in
        ({ loc_start; loc_end; loc_ghost }, acc)

    method location_stack : location_stack -> 'acc -> location_stack * 'acc =
      self#list self#location

    method loc
        : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a loc -> 'acc -> 'a loc * 'acc =
      fun _a { txt; loc } acc ->
        let txt, acc = _a txt acc in
        let loc, acc = self#location loc acc in
        ({ txt; loc }, acc)

    method longident : longident -> 'acc -> longident * 'acc =
      fun x acc ->
        match x with
        | Lident a ->
            let a, acc = self#string a acc in
            (Lident a, acc)
        | Ldot (a, b) ->
            let a, acc = self#longident a acc in
            let b, acc = self#string b acc in
            (Ldot (a, b), acc)
        | Lapply (a, b) ->
            let a, acc = self#longident a acc in
            let b, acc = self#longident b acc in
            (Lapply (a, b), acc)

    method longident_loc : longident_loc -> 'acc -> longident_loc * 'acc =
      self#loc self#longident

    method rec_flag : rec_flag -> 'acc -> rec_flag * 'acc = fun x acc -> (x, acc)

    method direction_flag : direction_flag -> 'acc -> direction_flag * 'acc =
      fun x acc -> (x, acc)

    method private_flag : private_flag -> 'acc -> private_flag * 'acc =
      fun x acc -> (x, acc)

    method mutable_flag : mutable_flag -> 'acc -> mutable_flag * 'acc =
      fun x acc -> (x, acc)

    method virtual_flag : virtual_flag -> 'acc -> virtual_flag * 'acc =
      fun x acc -> (x, acc)

    method override_flag : override_flag -> 'acc -> override_flag * 'acc =
      fun x acc -> (x, acc)

    method closed_flag : closed_flag -> 'acc -> closed_flag * 'acc =
      fun x acc -> (x, acc)

    method label : label -> 'acc -> label * 'acc = self#string

    method arg_label : arg_label -> 'acc -> arg_label * 'acc =
      fun x acc ->
        match x with
        | Nolabel -> (Nolabel, acc)
        | Labelled a ->
            let a, acc = self#string a acc in
            (Labelled a, acc)
        | Optional a ->
            let a, acc = self#string a acc in
            (Optional a, acc)

    method variance : variance -> 'acc -> variance * 'acc = fun x acc -> (x, acc)

    method injectivity : injectivity -> 'acc -> injectivity * 'acc =
      fun x acc -> (x, acc)

    method constant : constant -> 'acc -> constant * 'acc =
      fun x acc ->
        match x with
        | Pconst_integer (a, b) ->
            let a, acc = self#string a acc in
            let b, acc = self#option self#char b acc in
            (Pconst_integer (a, b), acc)
        | Pconst_char a ->
            let a, acc = self#char a acc in
            (Pconst_char a, acc)
        | Pconst_string (a, b, c) ->
            let a, acc = self#string a acc in
            let b, acc = self#location b acc in
            let c, acc = self#option self#string c acc in
            (Pconst_string (a, b, c), acc)
        | Pconst_float (a, b) ->
            let a, acc = self#string a acc in
            let b, acc = self#option self#char b acc in
            (Pconst_float (a, b), acc)

    method attribute : attribute -> 'acc -> attribute * 'acc =
      fun { attr_name; attr_payload; attr_loc } acc ->
        let attr_name, acc = self#loc self#string attr_name acc in
        let attr_payload, acc = self#payload attr_payload acc in
        let attr_loc, acc = self#location attr_loc acc in
        ({ attr_name; attr_payload; attr_loc }, acc)

    method extension : extension -> 'acc -> extension * 'acc =
      fun (a, b) acc ->
        let a, acc = self#loc self#string a acc in
        let b, acc = self#payload b acc in
        ((a, b), acc)

    method attributes : attributes -> 'acc -> attributes * 'acc =
      self#list self#attribute

    method payload : payload -> 'acc -> payload * 'acc =
      fun x acc ->
        match x with
        | PStr a ->
            let a, acc = self#structure a acc in
            (PStr a, acc)
        | PSig a ->
            let a, acc = self#signature a acc in
            (PSig a, acc)
        | PTyp a ->
            let a, acc = self#core_type a acc in
            (PTyp a, acc)
        | PPat (a, b) ->
            let a, acc = self#pattern a acc in
            let b, acc = self#option self#expression b acc in
            (PPat (a, b), acc)

    method core_type : core_type -> 'acc -> core_type * 'acc =
      fun { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes } acc ->
        let ptyp_desc, acc = self#core_type_desc ptyp_desc acc in
        let ptyp_loc, acc = self#location ptyp_loc acc in
        let ptyp_loc_stack, acc = self#location_stack ptyp_loc_stack acc in
        let ptyp_attributes, acc = self#attributes ptyp_attributes acc in
        ({ ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }, acc)

    method core_type_desc : core_type_desc -> 'acc -> core_type_desc * 'acc =
      fun x acc ->
        match x with
        | Ptyp_any -> (Ptyp_any, acc)
        | Ptyp_var a ->
            let a, acc = self#string a acc in
            (Ptyp_var a, acc)
        | Ptyp_arrow (a, b, c) ->
            let a, acc = self#arg_label a acc in
            let b, acc = self#core_type b acc in
            let c, acc = self#core_type c acc in
            (Ptyp_arrow (a, b, c), acc)
        | Ptyp_tuple a ->
            let a, acc = self#list self#core_type a acc in
            (Ptyp_tuple a, acc)
        | Ptyp_constr (a, b) ->
            let a, acc = self#longident_loc a acc in
            let b, acc = self#list self#core_type b acc in
            (Ptyp_constr (a, b), acc)
        | Ptyp_object (a, b) ->
            let a, acc = self#list self#object_field a acc in
            let b, acc = self#closed_flag b acc in
            (Ptyp_object (a, b), acc)
        | Ptyp_class (a, b) ->
            let a, acc = self#longident_loc a acc in
            let b, acc = self#list self#core_type b acc in
            (Ptyp_class (a, b), acc)
        | Ptyp_alias (a, b) ->
            let a, acc = self#core_type a acc in
            let b, acc = self#string b acc in
            (Ptyp_alias (a, b), acc)
        | Ptyp_variant (a, b, c) ->
            let a, acc = self#list self#row_field a acc in
            let b, acc = self#closed_flag b acc in
            let c, acc = self#option (self#list self#label) c acc in
            (Ptyp_variant (a, b, c), acc)
        | Ptyp_poly (a, b) ->
            let a, acc = self#list (self#loc self#string) a acc in
            let b, acc = self#core_type b acc in
            (Ptyp_poly (a, b), acc)
        | Ptyp_package a ->
            let a, acc = self#package_type a acc in
            (Ptyp_package a, acc)
        | Ptyp_extension a ->
            let a, acc = self#extension a acc in
            (Ptyp_extension a, acc)

    method package_type : package_type -> 'acc -> package_type * 'acc =
      fun (a, b) acc ->
        let a, acc = self#longident_loc a acc in
        let b, acc =
          self#list
            (fun (a, b) acc ->
              let a, acc = self#longident_loc a acc in
              let b, acc = self#core_type b acc in
              ((a, b), acc))
            b acc
        in
        ((a, b), acc)

    method row_field : row_field -> 'acc -> row_field * 'acc =
      fun { prf_desc; prf_loc; prf_attributes } acc ->
        let prf_desc, acc = self#row_field_desc prf_desc acc in
        let prf_loc, acc = self#location prf_loc acc in
        let prf_attributes, acc = self#attributes prf_attributes acc in
        ({ prf_desc; prf_loc; prf_attributes }, acc)

    method row_field_desc : row_field_desc -> 'acc -> row_field_desc * 'acc =
      fun x acc ->
        match x with
        | Rtag (a, b, c) ->
            let a, acc = self#loc self#label a acc in
            let b, acc = self#bool b acc in
            let c, acc = self#list self#core_type c acc in
            (Rtag (a, b, c), acc)
        | Rinherit a ->
            let a, acc = self#core_type a acc in
            (Rinherit a, acc)

    method object_field : object_field -> 'acc -> object_field * 'acc =
      fun { pof_desc; pof_loc; pof_attributes } acc ->
        let pof_desc, acc = self#object_field_desc pof_desc acc in
        let pof_loc, acc = self#location pof_loc acc in
        let pof_attributes, acc = self#attributes pof_attributes acc in
        ({ pof_desc; pof_loc; pof_attributes }, acc)

    method object_field_desc
        : object_field_desc -> 'acc -> object_field_desc * 'acc =
      fun x acc ->
        match x with
        | Otag (a, b) ->
            let a, acc = self#loc self#label a acc in
            let b, acc = self#core_type b acc in
            (Otag (a, b), acc)
        | Oinherit a ->
            let a, acc = self#core_type a acc in
            (Oinherit a, acc)

    method pattern : pattern -> 'acc -> pattern * 'acc =
      fun { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes } acc ->
        let ppat_desc, acc = self#pattern_desc ppat_desc acc in
        let ppat_loc, acc = self#location ppat_loc acc in
        let ppat_loc_stack, acc = self#location_stack ppat_loc_stack acc in
        let ppat_attributes, acc = self#attributes ppat_attributes acc in
        ({ ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes }, acc)

    method pattern_desc : pattern_desc -> 'acc -> pattern_desc * 'acc =
      fun x acc ->
        match x with
        | Ppat_any -> (Ppat_any, acc)
        | Ppat_var a ->
            let a, acc = self#loc self#string a acc in
            (Ppat_var a, acc)
        | Ppat_alias (a, b) ->
            let a, acc = self#pattern a acc in
            let b, acc = self#loc self#string b acc in
            (Ppat_alias (a, b), acc)
        | Ppat_constant a ->
            let a, acc = self#constant a acc in
            (Ppat_constant a, acc)
        | Ppat_interval (a, b) ->
            let a, acc = self#constant a acc in
            let b, acc = self#constant b acc in
            (Ppat_interval (a, b), acc)
        | Ppat_tuple a ->
            let a, acc = self#list self#pattern a acc in
            (Ppat_tuple a, acc)
        | Ppat_construct (a, b) ->
            let a, acc = self#longident_loc a acc in
            let b, acc = self#option self#pattern b acc in
            (Ppat_construct (a, b), acc)
        | Ppat_variant (a, b) ->
            let a, acc = self#label a acc in
            let b, acc = self#option self#pattern b acc in
            (Ppat_variant (a, b), acc)
        | Ppat_record (a, b) ->
            let a, acc =
              self#list
                (fun (a, b) acc ->
                  let a, acc = self#longident_loc a acc in
                  let b, acc = self#pattern b acc in
                  ((a, b), acc))
                a acc
            in
            let b, acc = self#closed_flag b acc in
            (Ppat_record (a, b), acc)
        | Ppat_array a ->
            let a, acc = self#list self#pattern a acc in
            (Ppat_array a, acc)
        | Ppat_or (a, b) ->
            let a, acc = self#pattern a acc in
            let b, acc = self#pattern b acc in
            (Ppat_or (a, b), acc)
        | Ppat_constraint (a, b) ->
            let a, acc = self#pattern a acc in
            let b, acc = self#core_type b acc in
            (Ppat_constraint (a, b), acc)
        | Ppat_type a ->
            let a, acc = self#longident_loc a acc in
            (Ppat_type a, acc)
        | Ppat_lazy a ->
            let a, acc = self#pattern a acc in
            (Ppat_lazy a, acc)
        | Ppat_unpack a ->
            let a, acc = self#loc (self#option self#string) a acc in
            (Ppat_unpack a, acc)
        | Ppat_exception a ->
            let a, acc = self#pattern a acc in
            (Ppat_exception a, acc)
        | Ppat_extension a ->
            let a, acc = self#extension a acc in
            (Ppat_extension a, acc)
        | Ppat_open (a, b) ->
            let a, acc = self#longident_loc a acc in
            let b, acc = self#pattern b acc in
            (Ppat_open (a, b), acc)

    method expression : expression -> 'acc -> expression * 'acc =
      fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } acc ->
        let pexp_desc, acc = self#expression_desc pexp_desc acc in
        let pexp_loc, acc = self#location pexp_loc acc in
        let pexp_loc_stack, acc = self#location_stack pexp_loc_stack acc in
        let pexp_attributes, acc = self#attributes pexp_attributes acc in
        ({ pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes }, acc)

    method expression_desc : expression_desc -> 'acc -> expression_desc * 'acc =
      fun x acc ->
        match x with
        | Pexp_ident a ->
            let a, acc = self#longident_loc a acc in
            (Pexp_ident a, acc)
        | Pexp_constant a ->
            let a, acc = self#constant a acc in
            (Pexp_constant a, acc)
        | Pexp_let (a, b, c) ->
            let a, acc = self#rec_flag a acc in
            let b, acc = self#list self#value_binding b acc in
            let c, acc = self#expression c acc in
            (Pexp_let (a, b, c), acc)
        | Pexp_function a ->
            let a, acc = self#cases a acc in
            (Pexp_function a, acc)
        | Pexp_fun (a, b, c, d) ->
            let a, acc = self#arg_label a acc in
            let b, acc = self#option self#expression b acc in
            let c, acc = self#pattern c acc in
            let d, acc = self#expression d acc in
            (Pexp_fun (a, b, c, d), acc)
        | Pexp_apply (a, b) ->
            let a, acc = self#expression a acc in
            let b, acc =
              self#list
                (fun (a, b) acc ->
                  let a, acc = self#arg_label a acc in
                  let b, acc = self#expression b acc in
                  ((a, b), acc))
                b acc
            in
            (Pexp_apply (a, b), acc)
        | Pexp_match (a, b) ->
            let a, acc = self#expression a acc in
            let b, acc = self#cases b acc in
            (Pexp_match (a, b), acc)
        | Pexp_try (a, b) ->
            let a, acc = self#expression a acc in
            let b, acc = self#cases b acc in
            (Pexp_try (a, b), acc)
        | Pexp_tuple a ->
            let a, acc = self#list self#expression a acc in
            (Pexp_tuple a, acc)
        | Pexp_construct (a, b) ->
            let a, acc = self#longident_loc a acc in
            let b, acc = self#option self#expression b acc in
            (Pexp_construct (a, b), acc)
        | Pexp_variant (a, b) ->
            let a, acc = self#label a acc in
            let b, acc = self#option self#expression b acc in
            (Pexp_variant (a, b), acc)
        | Pexp_record (a, b) ->
            let a, acc =
              self#list
                (fun (a, b) acc ->
                  let a, acc = self#longident_loc a acc in
                  let b, acc = self#expression b acc in
                  ((a, b), acc))
                a acc
            in
            let b, acc = self#option self#expression b acc in
            (Pexp_record (a, b), acc)
        | Pexp_field (a, b) ->
            let a, acc = self#expression a acc in
            let b, acc = self#longident_loc b acc in
            (Pexp_field (a, b), acc)
        | Pexp_setfield (a, b, c) ->
            let a, acc = self#expression a acc in
            let b, acc = self#longident_loc b acc in
            let c, acc = self#expression c acc in
            (Pexp_setfield (a, b, c), acc)
        | Pexp_array a ->
            let a, acc = self#list self#expression a acc in
            (Pexp_array a, acc)
        | Pexp_ifthenelse (a, b, c) ->
            let a, acc = self#expression a acc in
            let b, acc = self#expression b acc in
            let c, acc = self#option self#expression c acc in
            (Pexp_ifthenelse (a, b, c), acc)
        | Pexp_sequence (a, b) ->
            let a, acc = self#expression a acc in
            let b, acc = self#expression b acc in
            (Pexp_sequence (a, b), acc)
        | Pexp_while (a, b) ->
            let a, acc = self#expression a acc in
            let b, acc = self#expression b acc in
            (Pexp_while (a, b), acc)
        | Pexp_for (a, b, c, d, e) ->
            let a, acc = self#pattern a acc in
            let b, acc = self#expression b acc in
            let c, acc = self#expression c acc in
            let d, acc = self#direction_flag d acc in
            let e, acc = self#expression e acc in
            (Pexp_for (a, b, c, d, e), acc)
        | Pexp_constraint (a, b) ->
            let a, acc = self#expression a acc in
            let b, acc = self#core_type b acc in
            (Pexp_constraint (a, b), acc)
        | Pexp_coerce (a, b, c) ->
            let a, acc = self#expression a acc in
            let b, acc = self#option self#core_type b acc in
            let c, acc = self#core_type c acc in
            (Pexp_coerce (a, b, c), acc)
        | Pexp_send (a, b) ->
            let a, acc = self#expression a acc in
            let b, acc = self#loc self#label b acc in
            (Pexp_send (a, b), acc)
        | Pexp_new a ->
            let a, acc = self#longident_loc a acc in
            (Pexp_new a, acc)
        | Pexp_setinstvar (a, b) ->
            let a, acc = self#loc self#label a acc in
            let b, acc = self#expression b acc in
            (Pexp_setinstvar (a, b), acc)
        | Pexp_override a ->
            let a, acc =
              self#list
                (fun (a, b) acc ->
                  let a, acc = self#loc self#label a acc in
                  let b, acc = self#expression b acc in
                  ((a, b), acc))
                a acc
            in
            (Pexp_override a, acc)
        | Pexp_letmodule (a, b, c) ->
            let a, acc = self#loc (self#option self#string) a acc in
            let b, acc = self#module_expr b acc in
            let c, acc = self#expression c acc in
            (Pexp_letmodule (a, b, c), acc)
        | Pexp_letexception (a, b) ->
            let a, acc = self#extension_constructor a acc in
            let b, acc = self#expression b acc in
            (Pexp_letexception (a, b), acc)
        | Pexp_assert a ->
            let a, acc = self#expression a acc in
            (Pexp_assert a, acc)
        | Pexp_lazy a ->
            let a, acc = self#expression a acc in
            (Pexp_lazy a, acc)
        | Pexp_poly (a, b) ->
            let a, acc = self#expression a acc in
            let b, acc = self#option self#core_type b acc in
            (Pexp_poly (a, b), acc)
        | Pexp_object a ->
            let a, acc = self#class_structure a acc in
            (Pexp_object a, acc)
        | Pexp_newtype (a, b) ->
            let a, acc = self#loc self#string a acc in
            let b, acc = self#expression b acc in
            (Pexp_newtype (a, b), acc)
        | Pexp_pack a ->
            let a, acc = self#module_expr a acc in
            (Pexp_pack a, acc)
        | Pexp_open (a, b) ->
            let a, acc = self#open_declaration a acc in
            let b, acc = self#expression b acc in
            (Pexp_open (a, b), acc)
        | Pexp_letop a ->
            let a, acc = self#letop a acc in
            (Pexp_letop a, acc)
        | Pexp_extension a ->
            let a, acc = self#extension a acc in
            (Pexp_extension a, acc)
        | Pexp_unreachable -> (Pexp_unreachable, acc)

    method case : case -> 'acc -> case * 'acc =
      fun { pc_lhs; pc_guard; pc_rhs } acc ->
        let pc_lhs, acc = self#pattern pc_lhs acc in
        let pc_guard, acc = self#option self#expression pc_guard acc in
        let pc_rhs, acc = self#expression pc_rhs acc in
        ({ pc_lhs; pc_guard; pc_rhs }, acc)

    method letop : letop -> 'acc -> letop * 'acc =
      fun { let_; ands; body } acc ->
        let let_, acc = self#binding_op let_ acc in
        let ands, acc = self#list self#binding_op ands acc in
        let body, acc = self#expression body acc in
        ({ let_; ands; body }, acc)

    method binding_op : binding_op -> 'acc -> binding_op * 'acc =
      fun { pbop_op; pbop_pat; pbop_exp; pbop_loc } acc ->
        let pbop_op, acc = self#loc self#string pbop_op acc in
        let pbop_pat, acc = self#pattern pbop_pat acc in
        let pbop_exp, acc = self#expression pbop_exp acc in
        let pbop_loc, acc = self#location pbop_loc acc in
        ({ pbop_op; pbop_pat; pbop_exp; pbop_loc }, acc)

    method value_description
        : value_description -> 'acc -> value_description * 'acc =
      fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } acc ->
        let pval_name, acc = self#loc self#string pval_name acc in
        let pval_type, acc = self#core_type pval_type acc in
        let pval_prim, acc = self#list self#string pval_prim acc in
        let pval_attributes, acc = self#attributes pval_attributes acc in
        let pval_loc, acc = self#location pval_loc acc in
        ({ pval_name; pval_type; pval_prim; pval_attributes; pval_loc }, acc)

    method type_declaration
        : type_declaration -> 'acc -> type_declaration * 'acc =
      fun {
            ptype_name;
            ptype_params;
            ptype_cstrs;
            ptype_kind;
            ptype_private;
            ptype_manifest;
            ptype_attributes;
            ptype_loc;
          } acc ->
        let ptype_name, acc = self#loc self#string ptype_name acc in
        let ptype_params, acc =
          self#list
            (fun (a, b) acc ->
              let a, acc = self#core_type a acc in
              let b, acc =
                (fun (a, b) acc ->
                  let a, acc = self#variance a acc in
                  let b, acc = self#injectivity b acc in
                  ((a, b), acc))
                  b acc
              in
              ((a, b), acc))
            ptype_params acc
        in
        let ptype_cstrs, acc =
          self#list
            (fun (a, b, c) acc ->
              let a, acc = self#core_type a acc in
              let b, acc = self#core_type b acc in
              let c, acc = self#location c acc in
              ((a, b, c), acc))
            ptype_cstrs acc
        in
        let ptype_kind, acc = self#type_kind ptype_kind acc in
        let ptype_private, acc = self#private_flag ptype_private acc in
        let ptype_manifest, acc =
          self#option self#core_type ptype_manifest acc
        in
        let ptype_attributes, acc = self#attributes ptype_attributes acc in
        let ptype_loc, acc = self#location ptype_loc acc in
        ( {
            ptype_name;
            ptype_params;
            ptype_cstrs;
            ptype_kind;
            ptype_private;
            ptype_manifest;
            ptype_attributes;
            ptype_loc;
          },
          acc )

    method type_kind : type_kind -> 'acc -> type_kind * 'acc =
      fun x acc ->
        match x with
        | Ptype_abstract -> (Ptype_abstract, acc)
        | Ptype_variant a ->
            let a, acc = self#list self#constructor_declaration a acc in
            (Ptype_variant a, acc)
        | Ptype_record a ->
            let a, acc = self#list self#label_declaration a acc in
            (Ptype_record a, acc)
        | Ptype_open -> (Ptype_open, acc)

    method label_declaration
        : label_declaration -> 'acc -> label_declaration * 'acc =
      fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } acc ->
        let pld_name, acc = self#loc self#string pld_name acc in
        let pld_mutable, acc = self#mutable_flag pld_mutable acc in
        let pld_type, acc = self#core_type pld_type acc in
        let pld_loc, acc = self#location pld_loc acc in
        let pld_attributes, acc = self#attributes pld_attributes acc in
        ({ pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }, acc)

    method constructor_declaration
        : constructor_declaration -> 'acc -> constructor_declaration * 'acc =
      fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } acc ->
        let pcd_name, acc = self#loc self#string pcd_name acc in
        let pcd_args, acc = self#constructor_arguments pcd_args acc in
        let pcd_res, acc = self#option self#core_type pcd_res acc in
        let pcd_loc, acc = self#location pcd_loc acc in
        let pcd_attributes, acc = self#attributes pcd_attributes acc in
        ({ pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }, acc)

    method constructor_arguments
        : constructor_arguments -> 'acc -> constructor_arguments * 'acc =
      fun x acc ->
        match x with
        | Pcstr_tuple a ->
            let a, acc = self#list self#core_type a acc in
            (Pcstr_tuple a, acc)
        | Pcstr_record a ->
            let a, acc = self#list self#label_declaration a acc in
            (Pcstr_record a, acc)

    method type_extension : type_extension -> 'acc -> type_extension * 'acc =
      fun {
            ptyext_path;
            ptyext_params;
            ptyext_constructors;
            ptyext_private;
            ptyext_loc;
            ptyext_attributes;
          } acc ->
        let ptyext_path, acc = self#longident_loc ptyext_path acc in
        let ptyext_params, acc =
          self#list
            (fun (a, b) acc ->
              let a, acc = self#core_type a acc in
              let b, acc =
                (fun (a, b) acc ->
                  let a, acc = self#variance a acc in
                  let b, acc = self#injectivity b acc in
                  ((a, b), acc))
                  b acc
              in
              ((a, b), acc))
            ptyext_params acc
        in
        let ptyext_constructors, acc =
          self#list self#extension_constructor ptyext_constructors acc
        in
        let ptyext_private, acc = self#private_flag ptyext_private acc in
        let ptyext_loc, acc = self#location ptyext_loc acc in
        let ptyext_attributes, acc = self#attributes ptyext_attributes acc in
        ( {
            ptyext_path;
            ptyext_params;
            ptyext_constructors;
            ptyext_private;
            ptyext_loc;
            ptyext_attributes;
          },
          acc )

    method extension_constructor
        : extension_constructor -> 'acc -> extension_constructor * 'acc =
      fun { pext_name; pext_kind; pext_loc; pext_attributes } acc ->
        let pext_name, acc = self#loc self#string pext_name acc in
        let pext_kind, acc = self#extension_constructor_kind pext_kind acc in
        let pext_loc, acc = self#location pext_loc acc in
        let pext_attributes, acc = self#attributes pext_attributes acc in
        ({ pext_name; pext_kind; pext_loc; pext_attributes }, acc)

    method type_exception : type_exception -> 'acc -> type_exception * 'acc =
      fun { ptyexn_constructor; ptyexn_loc; ptyexn_attributes } acc ->
        let ptyexn_constructor, acc =
          self#extension_constructor ptyexn_constructor acc
        in
        let ptyexn_loc, acc = self#location ptyexn_loc acc in
        let ptyexn_attributes, acc = self#attributes ptyexn_attributes acc in
        ({ ptyexn_constructor; ptyexn_loc; ptyexn_attributes }, acc)

    method extension_constructor_kind
        : extension_constructor_kind ->
          'acc ->
          extension_constructor_kind * 'acc =
      fun x acc ->
        match x with
        | Pext_decl (a, b) ->
            let a, acc = self#constructor_arguments a acc in
            let b, acc = self#option self#core_type b acc in
            (Pext_decl (a, b), acc)
        | Pext_rebind a ->
            let a, acc = self#longident_loc a acc in
            (Pext_rebind a, acc)

    method class_type : class_type -> 'acc -> class_type * 'acc =
      fun { pcty_desc; pcty_loc; pcty_attributes } acc ->
        let pcty_desc, acc = self#class_type_desc pcty_desc acc in
        let pcty_loc, acc = self#location pcty_loc acc in
        let pcty_attributes, acc = self#attributes pcty_attributes acc in
        ({ pcty_desc; pcty_loc; pcty_attributes }, acc)

    method class_type_desc : class_type_desc -> 'acc -> class_type_desc * 'acc =
      fun x acc ->
        match x with
        | Pcty_constr (a, b) ->
            let a, acc = self#longident_loc a acc in
            let b, acc = self#list self#core_type b acc in
            (Pcty_constr (a, b), acc)
        | Pcty_signature a ->
            let a, acc = self#class_signature a acc in
            (Pcty_signature a, acc)
        | Pcty_arrow (a, b, c) ->
            let a, acc = self#arg_label a acc in
            let b, acc = self#core_type b acc in
            let c, acc = self#class_type c acc in
            (Pcty_arrow (a, b, c), acc)
        | Pcty_extension a ->
            let a, acc = self#extension a acc in
            (Pcty_extension a, acc)
        | Pcty_open (a, b) ->
            let a, acc = self#open_description a acc in
            let b, acc = self#class_type b acc in
            (Pcty_open (a, b), acc)

    method class_signature : class_signature -> 'acc -> class_signature * 'acc =
      fun { pcsig_self; pcsig_fields } acc ->
        let pcsig_self, acc = self#core_type pcsig_self acc in
        let pcsig_fields, acc =
          self#list self#class_type_field pcsig_fields acc
        in
        ({ pcsig_self; pcsig_fields }, acc)

    method class_type_field
        : class_type_field -> 'acc -> class_type_field * 'acc =
      fun { pctf_desc; pctf_loc; pctf_attributes } acc ->
        let pctf_desc, acc = self#class_type_field_desc pctf_desc acc in
        let pctf_loc, acc = self#location pctf_loc acc in
        let pctf_attributes, acc = self#attributes pctf_attributes acc in
        ({ pctf_desc; pctf_loc; pctf_attributes }, acc)

    method class_type_field_desc
        : class_type_field_desc -> 'acc -> class_type_field_desc * 'acc =
      fun x acc ->
        match x with
        | Pctf_inherit a ->
            let a, acc = self#class_type a acc in
            (Pctf_inherit a, acc)
        | Pctf_val a ->
            let a, acc =
              (fun (a, b, c, d) acc ->
                let a, acc = self#loc self#label a acc in
                let b, acc = self#mutable_flag b acc in
                let c, acc = self#virtual_flag c acc in
                let d, acc = self#core_type d acc in
                ((a, b, c, d), acc))
                a acc
            in
            (Pctf_val a, acc)
        | Pctf_method a ->
            let a, acc =
              (fun (a, b, c, d) acc ->
                let a, acc = self#loc self#label a acc in
                let b, acc = self#private_flag b acc in
                let c, acc = self#virtual_flag c acc in
                let d, acc = self#core_type d acc in
                ((a, b, c, d), acc))
                a acc
            in
            (Pctf_method a, acc)
        | Pctf_constraint a ->
            let a, acc =
              (fun (a, b) acc ->
                let a, acc = self#core_type a acc in
                let b, acc = self#core_type b acc in
                ((a, b), acc))
                a acc
            in
            (Pctf_constraint a, acc)
        | Pctf_attribute a ->
            let a, acc = self#attribute a acc in
            (Pctf_attribute a, acc)
        | Pctf_extension a ->
            let a, acc = self#extension a acc in
            (Pctf_extension a, acc)

    method class_infos
        : 'a.
          ('a -> 'acc -> 'a * 'acc) ->
          'a class_infos ->
          'acc ->
          'a class_infos * 'acc =
      fun _a
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
          acc ->
        let pci_virt, acc = self#virtual_flag pci_virt acc in
        let pci_params, acc =
          self#list
            (fun (a, b) acc ->
              let a, acc = self#core_type a acc in
              let b, acc =
                (fun (a, b) acc ->
                  let a, acc = self#variance a acc in
                  let b, acc = self#injectivity b acc in
                  ((a, b), acc))
                  b acc
              in
              ((a, b), acc))
            pci_params acc
        in
        let pci_name, acc = self#loc self#string pci_name acc in
        let pci_expr, acc = _a pci_expr acc in
        let pci_loc, acc = self#location pci_loc acc in
        let pci_attributes, acc = self#attributes pci_attributes acc in
        ( { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes },
          acc )

    method class_description
        : class_description -> 'acc -> class_description * 'acc =
      self#class_infos self#class_type

    method class_type_declaration
        : class_type_declaration -> 'acc -> class_type_declaration * 'acc =
      self#class_infos self#class_type

    method class_expr : class_expr -> 'acc -> class_expr * 'acc =
      fun { pcl_desc; pcl_loc; pcl_attributes } acc ->
        let pcl_desc, acc = self#class_expr_desc pcl_desc acc in
        let pcl_loc, acc = self#location pcl_loc acc in
        let pcl_attributes, acc = self#attributes pcl_attributes acc in
        ({ pcl_desc; pcl_loc; pcl_attributes }, acc)

    method class_expr_desc : class_expr_desc -> 'acc -> class_expr_desc * 'acc =
      fun x acc ->
        match x with
        | Pcl_constr (a, b) ->
            let a, acc = self#longident_loc a acc in
            let b, acc = self#list self#core_type b acc in
            (Pcl_constr (a, b), acc)
        | Pcl_structure a ->
            let a, acc = self#class_structure a acc in
            (Pcl_structure a, acc)
        | Pcl_fun (a, b, c, d) ->
            let a, acc = self#arg_label a acc in
            let b, acc = self#option self#expression b acc in
            let c, acc = self#pattern c acc in
            let d, acc = self#class_expr d acc in
            (Pcl_fun (a, b, c, d), acc)
        | Pcl_apply (a, b) ->
            let a, acc = self#class_expr a acc in
            let b, acc =
              self#list
                (fun (a, b) acc ->
                  let a, acc = self#arg_label a acc in
                  let b, acc = self#expression b acc in
                  ((a, b), acc))
                b acc
            in
            (Pcl_apply (a, b), acc)
        | Pcl_let (a, b, c) ->
            let a, acc = self#rec_flag a acc in
            let b, acc = self#list self#value_binding b acc in
            let c, acc = self#class_expr c acc in
            (Pcl_let (a, b, c), acc)
        | Pcl_constraint (a, b) ->
            let a, acc = self#class_expr a acc in
            let b, acc = self#class_type b acc in
            (Pcl_constraint (a, b), acc)
        | Pcl_extension a ->
            let a, acc = self#extension a acc in
            (Pcl_extension a, acc)
        | Pcl_open (a, b) ->
            let a, acc = self#open_description a acc in
            let b, acc = self#class_expr b acc in
            (Pcl_open (a, b), acc)

    method class_structure : class_structure -> 'acc -> class_structure * 'acc =
      fun { pcstr_self; pcstr_fields } acc ->
        let pcstr_self, acc = self#pattern pcstr_self acc in
        let pcstr_fields, acc = self#list self#class_field pcstr_fields acc in
        ({ pcstr_self; pcstr_fields }, acc)

    method class_field : class_field -> 'acc -> class_field * 'acc =
      fun { pcf_desc; pcf_loc; pcf_attributes } acc ->
        let pcf_desc, acc = self#class_field_desc pcf_desc acc in
        let pcf_loc, acc = self#location pcf_loc acc in
        let pcf_attributes, acc = self#attributes pcf_attributes acc in
        ({ pcf_desc; pcf_loc; pcf_attributes }, acc)

    method class_field_desc
        : class_field_desc -> 'acc -> class_field_desc * 'acc =
      fun x acc ->
        match x with
        | Pcf_inherit (a, b, c) ->
            let a, acc = self#override_flag a acc in
            let b, acc = self#class_expr b acc in
            let c, acc = self#option (self#loc self#string) c acc in
            (Pcf_inherit (a, b, c), acc)
        | Pcf_val a ->
            let a, acc =
              (fun (a, b, c) acc ->
                let a, acc = self#loc self#label a acc in
                let b, acc = self#mutable_flag b acc in
                let c, acc = self#class_field_kind c acc in
                ((a, b, c), acc))
                a acc
            in
            (Pcf_val a, acc)
        | Pcf_method a ->
            let a, acc =
              (fun (a, b, c) acc ->
                let a, acc = self#loc self#label a acc in
                let b, acc = self#private_flag b acc in
                let c, acc = self#class_field_kind c acc in
                ((a, b, c), acc))
                a acc
            in
            (Pcf_method a, acc)
        | Pcf_constraint a ->
            let a, acc =
              (fun (a, b) acc ->
                let a, acc = self#core_type a acc in
                let b, acc = self#core_type b acc in
                ((a, b), acc))
                a acc
            in
            (Pcf_constraint a, acc)
        | Pcf_initializer a ->
            let a, acc = self#expression a acc in
            (Pcf_initializer a, acc)
        | Pcf_attribute a ->
            let a, acc = self#attribute a acc in
            (Pcf_attribute a, acc)
        | Pcf_extension a ->
            let a, acc = self#extension a acc in
            (Pcf_extension a, acc)

    method class_field_kind
        : class_field_kind -> 'acc -> class_field_kind * 'acc =
      fun x acc ->
        match x with
        | Cfk_virtual a ->
            let a, acc = self#core_type a acc in
            (Cfk_virtual a, acc)
        | Cfk_concrete (a, b) ->
            let a, acc = self#override_flag a acc in
            let b, acc = self#expression b acc in
            (Cfk_concrete (a, b), acc)

    method class_declaration
        : class_declaration -> 'acc -> class_declaration * 'acc =
      self#class_infos self#class_expr

    method module_type : module_type -> 'acc -> module_type * 'acc =
      fun { pmty_desc; pmty_loc; pmty_attributes } acc ->
        let pmty_desc, acc = self#module_type_desc pmty_desc acc in
        let pmty_loc, acc = self#location pmty_loc acc in
        let pmty_attributes, acc = self#attributes pmty_attributes acc in
        ({ pmty_desc; pmty_loc; pmty_attributes }, acc)

    method module_type_desc
        : module_type_desc -> 'acc -> module_type_desc * 'acc =
      fun x acc ->
        match x with
        | Pmty_ident a ->
            let a, acc = self#longident_loc a acc in
            (Pmty_ident a, acc)
        | Pmty_signature a ->
            let a, acc = self#signature a acc in
            (Pmty_signature a, acc)
        | Pmty_functor (a, b) ->
            let a, acc = self#functor_parameter a acc in
            let b, acc = self#module_type b acc in
            (Pmty_functor (a, b), acc)
        | Pmty_with (a, b) ->
            let a, acc = self#module_type a acc in
            let b, acc = self#list self#with_constraint b acc in
            (Pmty_with (a, b), acc)
        | Pmty_typeof a ->
            let a, acc = self#module_expr a acc in
            (Pmty_typeof a, acc)
        | Pmty_extension a ->
            let a, acc = self#extension a acc in
            (Pmty_extension a, acc)
        | Pmty_alias a ->
            let a, acc = self#longident_loc a acc in
            (Pmty_alias a, acc)

    method functor_parameter
        : functor_parameter -> 'acc -> functor_parameter * 'acc =
      fun x acc ->
        match x with
        | Unit -> (Unit, acc)
        | Named (a, b) ->
            let a, acc = self#loc (self#option self#string) a acc in
            let b, acc = self#module_type b acc in
            (Named (a, b), acc)

    method signature : signature -> 'acc -> signature * 'acc =
      self#list self#signature_item

    method signature_item : signature_item -> 'acc -> signature_item * 'acc =
      fun { psig_desc; psig_loc } acc ->
        let psig_desc, acc = self#signature_item_desc psig_desc acc in
        let psig_loc, acc = self#location psig_loc acc in
        ({ psig_desc; psig_loc }, acc)

    method signature_item_desc
        : signature_item_desc -> 'acc -> signature_item_desc * 'acc =
      fun x acc ->
        match x with
        | Psig_value a ->
            let a, acc = self#value_description a acc in
            (Psig_value a, acc)
        | Psig_type (a, b) ->
            let a, acc = self#rec_flag a acc in
            let b, acc = self#list self#type_declaration b acc in
            (Psig_type (a, b), acc)
        | Psig_typesubst a ->
            let a, acc = self#list self#type_declaration a acc in
            (Psig_typesubst a, acc)
        | Psig_typext a ->
            let a, acc = self#type_extension a acc in
            (Psig_typext a, acc)
        | Psig_exception a ->
            let a, acc = self#type_exception a acc in
            (Psig_exception a, acc)
        | Psig_module a ->
            let a, acc = self#module_declaration a acc in
            (Psig_module a, acc)
        | Psig_modsubst a ->
            let a, acc = self#module_substitution a acc in
            (Psig_modsubst a, acc)
        | Psig_recmodule a ->
            let a, acc = self#list self#module_declaration a acc in
            (Psig_recmodule a, acc)
        | Psig_modtype a ->
            let a, acc = self#module_type_declaration a acc in
            (Psig_modtype a, acc)
        | Psig_open a ->
            let a, acc = self#open_description a acc in
            (Psig_open a, acc)
        | Psig_include a ->
            let a, acc = self#include_description a acc in
            (Psig_include a, acc)
        | Psig_class a ->
            let a, acc = self#list self#class_description a acc in
            (Psig_class a, acc)
        | Psig_class_type a ->
            let a, acc = self#list self#class_type_declaration a acc in
            (Psig_class_type a, acc)
        | Psig_attribute a ->
            let a, acc = self#attribute a acc in
            (Psig_attribute a, acc)
        | Psig_extension (a, b) ->
            let a, acc = self#extension a acc in
            let b, acc = self#attributes b acc in
            (Psig_extension (a, b), acc)

    method module_declaration
        : module_declaration -> 'acc -> module_declaration * 'acc =
      fun { pmd_name; pmd_type; pmd_attributes; pmd_loc } acc ->
        let pmd_name, acc = self#loc (self#option self#string) pmd_name acc in
        let pmd_type, acc = self#module_type pmd_type acc in
        let pmd_attributes, acc = self#attributes pmd_attributes acc in
        let pmd_loc, acc = self#location pmd_loc acc in
        ({ pmd_name; pmd_type; pmd_attributes; pmd_loc }, acc)

    method module_substitution
        : module_substitution -> 'acc -> module_substitution * 'acc =
      fun { pms_name; pms_manifest; pms_attributes; pms_loc } acc ->
        let pms_name, acc = self#loc self#string pms_name acc in
        let pms_manifest, acc = self#longident_loc pms_manifest acc in
        let pms_attributes, acc = self#attributes pms_attributes acc in
        let pms_loc, acc = self#location pms_loc acc in
        ({ pms_name; pms_manifest; pms_attributes; pms_loc }, acc)

    method module_type_declaration
        : module_type_declaration -> 'acc -> module_type_declaration * 'acc =
      fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } acc ->
        let pmtd_name, acc = self#loc self#string pmtd_name acc in
        let pmtd_type, acc = self#option self#module_type pmtd_type acc in
        let pmtd_attributes, acc = self#attributes pmtd_attributes acc in
        let pmtd_loc, acc = self#location pmtd_loc acc in
        ({ pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }, acc)

    method open_infos
        : 'a.
          ('a -> 'acc -> 'a * 'acc) ->
          'a open_infos ->
          'acc ->
          'a open_infos * 'acc =
      fun _a { popen_expr; popen_override; popen_loc; popen_attributes } acc ->
        let popen_expr, acc = _a popen_expr acc in
        let popen_override, acc = self#override_flag popen_override acc in
        let popen_loc, acc = self#location popen_loc acc in
        let popen_attributes, acc = self#attributes popen_attributes acc in
        ({ popen_expr; popen_override; popen_loc; popen_attributes }, acc)

    method open_description
        : open_description -> 'acc -> open_description * 'acc =
      self#open_infos self#longident_loc

    method open_declaration
        : open_declaration -> 'acc -> open_declaration * 'acc =
      self#open_infos self#module_expr

    method include_infos
        : 'a.
          ('a -> 'acc -> 'a * 'acc) ->
          'a include_infos ->
          'acc ->
          'a include_infos * 'acc =
      fun _a { pincl_mod; pincl_loc; pincl_attributes } acc ->
        let pincl_mod, acc = _a pincl_mod acc in
        let pincl_loc, acc = self#location pincl_loc acc in
        let pincl_attributes, acc = self#attributes pincl_attributes acc in
        ({ pincl_mod; pincl_loc; pincl_attributes }, acc)

    method include_description
        : include_description -> 'acc -> include_description * 'acc =
      self#include_infos self#module_type

    method include_declaration
        : include_declaration -> 'acc -> include_declaration * 'acc =
      self#include_infos self#module_expr

    method with_constraint : with_constraint -> 'acc -> with_constraint * 'acc =
      fun x acc ->
        match x with
        | Pwith_type (a, b) ->
            let a, acc = self#longident_loc a acc in
            let b, acc = self#type_declaration b acc in
            (Pwith_type (a, b), acc)
        | Pwith_module (a, b) ->
            let a, acc = self#longident_loc a acc in
            let b, acc = self#longident_loc b acc in
            (Pwith_module (a, b), acc)
        | Pwith_typesubst (a, b) ->
            let a, acc = self#longident_loc a acc in
            let b, acc = self#type_declaration b acc in
            (Pwith_typesubst (a, b), acc)
        | Pwith_modsubst (a, b) ->
            let a, acc = self#longident_loc a acc in
            let b, acc = self#longident_loc b acc in
            (Pwith_modsubst (a, b), acc)

    method module_expr : module_expr -> 'acc -> module_expr * 'acc =
      fun { pmod_desc; pmod_loc; pmod_attributes } acc ->
        let pmod_desc, acc = self#module_expr_desc pmod_desc acc in
        let pmod_loc, acc = self#location pmod_loc acc in
        let pmod_attributes, acc = self#attributes pmod_attributes acc in
        ({ pmod_desc; pmod_loc; pmod_attributes }, acc)

    method module_expr_desc
        : module_expr_desc -> 'acc -> module_expr_desc * 'acc =
      fun x acc ->
        match x with
        | Pmod_ident a ->
            let a, acc = self#longident_loc a acc in
            (Pmod_ident a, acc)
        | Pmod_structure a ->
            let a, acc = self#structure a acc in
            (Pmod_structure a, acc)
        | Pmod_functor (a, b) ->
            let a, acc = self#functor_parameter a acc in
            let b, acc = self#module_expr b acc in
            (Pmod_functor (a, b), acc)
        | Pmod_apply (a, b) ->
            let a, acc = self#module_expr a acc in
            let b, acc = self#module_expr b acc in
            (Pmod_apply (a, b), acc)
        | Pmod_constraint (a, b) ->
            let a, acc = self#module_expr a acc in
            let b, acc = self#module_type b acc in
            (Pmod_constraint (a, b), acc)
        | Pmod_unpack a ->
            let a, acc = self#expression a acc in
            (Pmod_unpack a, acc)
        | Pmod_extension a ->
            let a, acc = self#extension a acc in
            (Pmod_extension a, acc)

    method structure : structure -> 'acc -> structure * 'acc =
      self#list self#structure_item

    method structure_item : structure_item -> 'acc -> structure_item * 'acc =
      fun { pstr_desc; pstr_loc } acc ->
        let pstr_desc, acc = self#structure_item_desc pstr_desc acc in
        let pstr_loc, acc = self#location pstr_loc acc in
        ({ pstr_desc; pstr_loc }, acc)

    method structure_item_desc
        : structure_item_desc -> 'acc -> structure_item_desc * 'acc =
      fun x acc ->
        match x with
        | Pstr_eval (a, b) ->
            let a, acc = self#expression a acc in
            let b, acc = self#attributes b acc in
            (Pstr_eval (a, b), acc)
        | Pstr_value (a, b) ->
            let a, acc = self#rec_flag a acc in
            let b, acc = self#list self#value_binding b acc in
            (Pstr_value (a, b), acc)
        | Pstr_primitive a ->
            let a, acc = self#value_description a acc in
            (Pstr_primitive a, acc)
        | Pstr_type (a, b) ->
            let a, acc = self#rec_flag a acc in
            let b, acc = self#list self#type_declaration b acc in
            (Pstr_type (a, b), acc)
        | Pstr_typext a ->
            let a, acc = self#type_extension a acc in
            (Pstr_typext a, acc)
        | Pstr_exception a ->
            let a, acc = self#type_exception a acc in
            (Pstr_exception a, acc)
        | Pstr_module a ->
            let a, acc = self#module_binding a acc in
            (Pstr_module a, acc)
        | Pstr_recmodule a ->
            let a, acc = self#list self#module_binding a acc in
            (Pstr_recmodule a, acc)
        | Pstr_modtype a ->
            let a, acc = self#module_type_declaration a acc in
            (Pstr_modtype a, acc)
        | Pstr_open a ->
            let a, acc = self#open_declaration a acc in
            (Pstr_open a, acc)
        | Pstr_class a ->
            let a, acc = self#list self#class_declaration a acc in
            (Pstr_class a, acc)
        | Pstr_class_type a ->
            let a, acc = self#list self#class_type_declaration a acc in
            (Pstr_class_type a, acc)
        | Pstr_include a ->
            let a, acc = self#include_declaration a acc in
            (Pstr_include a, acc)
        | Pstr_attribute a ->
            let a, acc = self#attribute a acc in
            (Pstr_attribute a, acc)
        | Pstr_extension (a, b) ->
            let a, acc = self#extension a acc in
            let b, acc = self#attributes b acc in
            (Pstr_extension (a, b), acc)

    method value_binding : value_binding -> 'acc -> value_binding * 'acc =
      fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } acc ->
        let pvb_pat, acc = self#pattern pvb_pat acc in
        let pvb_expr, acc = self#expression pvb_expr acc in
        let pvb_attributes, acc = self#attributes pvb_attributes acc in
        let pvb_loc, acc = self#location pvb_loc acc in
        ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc }, acc)

    method module_binding : module_binding -> 'acc -> module_binding * 'acc =
      fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc } acc ->
        let pmb_name, acc = self#loc (self#option self#string) pmb_name acc in
        let pmb_expr, acc = self#module_expr pmb_expr acc in
        let pmb_attributes, acc = self#attributes pmb_attributes acc in
        let pmb_loc, acc = self#location pmb_loc acc in
        ({ pmb_name; pmb_expr; pmb_attributes; pmb_loc }, acc)

    method toplevel_phrase : toplevel_phrase -> 'acc -> toplevel_phrase * 'acc =
      fun x acc ->
        match x with
        | Ptop_def a ->
            let a, acc = self#structure a acc in
            (Ptop_def a, acc)
        | Ptop_dir a ->
            let a, acc = self#toplevel_directive a acc in
            (Ptop_dir a, acc)

    method toplevel_directive
        : toplevel_directive -> 'acc -> toplevel_directive * 'acc =
      fun { pdir_name; pdir_arg; pdir_loc } acc ->
        let pdir_name, acc = self#loc self#string pdir_name acc in
        let pdir_arg, acc = self#option self#directive_argument pdir_arg acc in
        let pdir_loc, acc = self#location pdir_loc acc in
        ({ pdir_name; pdir_arg; pdir_loc }, acc)

    method directive_argument
        : directive_argument -> 'acc -> directive_argument * 'acc =
      fun { pdira_desc; pdira_loc } acc ->
        let pdira_desc, acc = self#directive_argument_desc pdira_desc acc in
        let pdira_loc, acc = self#location pdira_loc acc in
        ({ pdira_desc; pdira_loc }, acc)

    method directive_argument_desc
        : directive_argument_desc -> 'acc -> directive_argument_desc * 'acc =
      fun x acc ->
        match x with
        | Pdir_string a ->
            let a, acc = self#string a acc in
            (Pdir_string a, acc)
        | Pdir_int (a, b) ->
            let a, acc = self#string a acc in
            let b, acc = self#option self#char b acc in
            (Pdir_int (a, b), acc)
        | Pdir_ident a ->
            let a, acc = self#longident a acc in
            (Pdir_ident a, acc)
        | Pdir_bool a ->
            let a, acc = self#bool a acc in
            (Pdir_bool a, acc)

    method cases : cases -> 'acc -> cases * 'acc = self#list self#case
  end

class virtual ['ctx] map_with_context =
  object (self)
    method virtual bool : 'ctx -> bool -> bool

    method virtual char : 'ctx -> char -> char

    method virtual int : 'ctx -> int -> int

    method virtual list : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list

    method virtual option
        : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option

    method virtual string : 'ctx -> string -> string

    method position : 'ctx -> position -> position =
      fun ctx { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
        let pos_fname = self#string ctx pos_fname in
        let pos_lnum = self#int ctx pos_lnum in
        let pos_bol = self#int ctx pos_bol in
        let pos_cnum = self#int ctx pos_cnum in
        { pos_fname; pos_lnum; pos_bol; pos_cnum }

    method location : 'ctx -> location -> location =
      fun ctx { loc_start; loc_end; loc_ghost } ->
        let loc_start = self#position ctx loc_start in
        let loc_end = self#position ctx loc_end in
        let loc_ghost = self#bool ctx loc_ghost in
        { loc_start; loc_end; loc_ghost }

    method location_stack : 'ctx -> location_stack -> location_stack =
      self#list self#location

    method loc : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a loc -> 'a loc =
      fun _a ctx { txt; loc } ->
        let txt = _a ctx txt in
        let loc = self#location ctx loc in
        { txt; loc }

    method longident : 'ctx -> longident -> longident =
      fun ctx x ->
        match x with
        | Lident a ->
            let a = self#string ctx a in
            Lident a
        | Ldot (a, b) ->
            let a = self#longident ctx a in
            let b = self#string ctx b in
            Ldot (a, b)
        | Lapply (a, b) ->
            let a = self#longident ctx a in
            let b = self#longident ctx b in
            Lapply (a, b)

    method longident_loc : 'ctx -> longident_loc -> longident_loc =
      self#loc self#longident

    method rec_flag : 'ctx -> rec_flag -> rec_flag = fun _ctx x -> x

    method direction_flag : 'ctx -> direction_flag -> direction_flag =
      fun _ctx x -> x

    method private_flag : 'ctx -> private_flag -> private_flag = fun _ctx x -> x

    method mutable_flag : 'ctx -> mutable_flag -> mutable_flag = fun _ctx x -> x

    method virtual_flag : 'ctx -> virtual_flag -> virtual_flag = fun _ctx x -> x

    method override_flag : 'ctx -> override_flag -> override_flag =
      fun _ctx x -> x

    method closed_flag : 'ctx -> closed_flag -> closed_flag = fun _ctx x -> x

    method label : 'ctx -> label -> label = self#string

    method arg_label : 'ctx -> arg_label -> arg_label =
      fun ctx x ->
        match x with
        | Nolabel -> Nolabel
        | Labelled a ->
            let a = self#string ctx a in
            Labelled a
        | Optional a ->
            let a = self#string ctx a in
            Optional a

    method variance : 'ctx -> variance -> variance = fun _ctx x -> x

    method injectivity : 'ctx -> injectivity -> injectivity = fun _ctx x -> x

    method constant : 'ctx -> constant -> constant =
      fun ctx x ->
        match x with
        | Pconst_integer (a, b) ->
            let a = self#string ctx a in
            let b = self#option self#char ctx b in
            Pconst_integer (a, b)
        | Pconst_char a ->
            let a = self#char ctx a in
            Pconst_char a
        | Pconst_string (a, b, c) ->
            let a = self#string ctx a in
            let b = self#location ctx b in
            let c = self#option self#string ctx c in
            Pconst_string (a, b, c)
        | Pconst_float (a, b) ->
            let a = self#string ctx a in
            let b = self#option self#char ctx b in
            Pconst_float (a, b)

    method attribute : 'ctx -> attribute -> attribute =
      fun ctx { attr_name; attr_payload; attr_loc } ->
        let attr_name = self#loc self#string ctx attr_name in
        let attr_payload = self#payload ctx attr_payload in
        let attr_loc = self#location ctx attr_loc in
        { attr_name; attr_payload; attr_loc }

    method extension : 'ctx -> extension -> extension =
      fun ctx (a, b) ->
        let a = self#loc self#string ctx a in
        let b = self#payload ctx b in
        (a, b)

    method attributes : 'ctx -> attributes -> attributes =
      self#list self#attribute

    method payload : 'ctx -> payload -> payload =
      fun ctx x ->
        match x with
        | PStr a ->
            let a = self#structure ctx a in
            PStr a
        | PSig a ->
            let a = self#signature ctx a in
            PSig a
        | PTyp a ->
            let a = self#core_type ctx a in
            PTyp a
        | PPat (a, b) ->
            let a = self#pattern ctx a in
            let b = self#option self#expression ctx b in
            PPat (a, b)

    method core_type : 'ctx -> core_type -> core_type =
      fun ctx { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes } ->
        let ptyp_desc = self#core_type_desc ctx ptyp_desc in
        let ptyp_loc = self#location ctx ptyp_loc in
        let ptyp_loc_stack = self#location_stack ctx ptyp_loc_stack in
        let ptyp_attributes = self#attributes ctx ptyp_attributes in
        { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }

    method core_type_desc : 'ctx -> core_type_desc -> core_type_desc =
      fun ctx x ->
        match x with
        | Ptyp_any -> Ptyp_any
        | Ptyp_var a ->
            let a = self#string ctx a in
            Ptyp_var a
        | Ptyp_arrow (a, b, c) ->
            let a = self#arg_label ctx a in
            let b = self#core_type ctx b in
            let c = self#core_type ctx c in
            Ptyp_arrow (a, b, c)
        | Ptyp_tuple a ->
            let a = self#list self#core_type ctx a in
            Ptyp_tuple a
        | Ptyp_constr (a, b) ->
            let a = self#longident_loc ctx a in
            let b = self#list self#core_type ctx b in
            Ptyp_constr (a, b)
        | Ptyp_object (a, b) ->
            let a = self#list self#object_field ctx a in
            let b = self#closed_flag ctx b in
            Ptyp_object (a, b)
        | Ptyp_class (a, b) ->
            let a = self#longident_loc ctx a in
            let b = self#list self#core_type ctx b in
            Ptyp_class (a, b)
        | Ptyp_alias (a, b) ->
            let a = self#core_type ctx a in
            let b = self#string ctx b in
            Ptyp_alias (a, b)
        | Ptyp_variant (a, b, c) ->
            let a = self#list self#row_field ctx a in
            let b = self#closed_flag ctx b in
            let c = self#option (self#list self#label) ctx c in
            Ptyp_variant (a, b, c)
        | Ptyp_poly (a, b) ->
            let a = self#list (self#loc self#string) ctx a in
            let b = self#core_type ctx b in
            Ptyp_poly (a, b)
        | Ptyp_package a ->
            let a = self#package_type ctx a in
            Ptyp_package a
        | Ptyp_extension a ->
            let a = self#extension ctx a in
            Ptyp_extension a

    method package_type : 'ctx -> package_type -> package_type =
      fun ctx (a, b) ->
        let a = self#longident_loc ctx a in
        let b =
          self#list
            (fun ctx (a, b) ->
              let a = self#longident_loc ctx a in
              let b = self#core_type ctx b in
              (a, b))
            ctx b
        in
        (a, b)

    method row_field : 'ctx -> row_field -> row_field =
      fun ctx { prf_desc; prf_loc; prf_attributes } ->
        let prf_desc = self#row_field_desc ctx prf_desc in
        let prf_loc = self#location ctx prf_loc in
        let prf_attributes = self#attributes ctx prf_attributes in
        { prf_desc; prf_loc; prf_attributes }

    method row_field_desc : 'ctx -> row_field_desc -> row_field_desc =
      fun ctx x ->
        match x with
        | Rtag (a, b, c) ->
            let a = self#loc self#label ctx a in
            let b = self#bool ctx b in
            let c = self#list self#core_type ctx c in
            Rtag (a, b, c)
        | Rinherit a ->
            let a = self#core_type ctx a in
            Rinherit a

    method object_field : 'ctx -> object_field -> object_field =
      fun ctx { pof_desc; pof_loc; pof_attributes } ->
        let pof_desc = self#object_field_desc ctx pof_desc in
        let pof_loc = self#location ctx pof_loc in
        let pof_attributes = self#attributes ctx pof_attributes in
        { pof_desc; pof_loc; pof_attributes }

    method object_field_desc : 'ctx -> object_field_desc -> object_field_desc =
      fun ctx x ->
        match x with
        | Otag (a, b) ->
            let a = self#loc self#label ctx a in
            let b = self#core_type ctx b in
            Otag (a, b)
        | Oinherit a ->
            let a = self#core_type ctx a in
            Oinherit a

    method pattern : 'ctx -> pattern -> pattern =
      fun ctx { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes } ->
        let ppat_desc = self#pattern_desc ctx ppat_desc in
        let ppat_loc = self#location ctx ppat_loc in
        let ppat_loc_stack = self#location_stack ctx ppat_loc_stack in
        let ppat_attributes = self#attributes ctx ppat_attributes in
        { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes }

    method pattern_desc : 'ctx -> pattern_desc -> pattern_desc =
      fun ctx x ->
        match x with
        | Ppat_any -> Ppat_any
        | Ppat_var a ->
            let a = self#loc self#string ctx a in
            Ppat_var a
        | Ppat_alias (a, b) ->
            let a = self#pattern ctx a in
            let b = self#loc self#string ctx b in
            Ppat_alias (a, b)
        | Ppat_constant a ->
            let a = self#constant ctx a in
            Ppat_constant a
        | Ppat_interval (a, b) ->
            let a = self#constant ctx a in
            let b = self#constant ctx b in
            Ppat_interval (a, b)
        | Ppat_tuple a ->
            let a = self#list self#pattern ctx a in
            Ppat_tuple a
        | Ppat_construct (a, b) ->
            let a = self#longident_loc ctx a in
            let b = self#option self#pattern ctx b in
            Ppat_construct (a, b)
        | Ppat_variant (a, b) ->
            let a = self#label ctx a in
            let b = self#option self#pattern ctx b in
            Ppat_variant (a, b)
        | Ppat_record (a, b) ->
            let a =
              self#list
                (fun ctx (a, b) ->
                  let a = self#longident_loc ctx a in
                  let b = self#pattern ctx b in
                  (a, b))
                ctx a
            in
            let b = self#closed_flag ctx b in
            Ppat_record (a, b)
        | Ppat_array a ->
            let a = self#list self#pattern ctx a in
            Ppat_array a
        | Ppat_or (a, b) ->
            let a = self#pattern ctx a in
            let b = self#pattern ctx b in
            Ppat_or (a, b)
        | Ppat_constraint (a, b) ->
            let a = self#pattern ctx a in
            let b = self#core_type ctx b in
            Ppat_constraint (a, b)
        | Ppat_type a ->
            let a = self#longident_loc ctx a in
            Ppat_type a
        | Ppat_lazy a ->
            let a = self#pattern ctx a in
            Ppat_lazy a
        | Ppat_unpack a ->
            let a = self#loc (self#option self#string) ctx a in
            Ppat_unpack a
        | Ppat_exception a ->
            let a = self#pattern ctx a in
            Ppat_exception a
        | Ppat_extension a ->
            let a = self#extension ctx a in
            Ppat_extension a
        | Ppat_open (a, b) ->
            let a = self#longident_loc ctx a in
            let b = self#pattern ctx b in
            Ppat_open (a, b)

    method expression : 'ctx -> expression -> expression =
      fun ctx { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
        let pexp_desc = self#expression_desc ctx pexp_desc in
        let pexp_loc = self#location ctx pexp_loc in
        let pexp_loc_stack = self#location_stack ctx pexp_loc_stack in
        let pexp_attributes = self#attributes ctx pexp_attributes in
        { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes }

    method expression_desc : 'ctx -> expression_desc -> expression_desc =
      fun ctx x ->
        match x with
        | Pexp_ident a ->
            let a = self#longident_loc ctx a in
            Pexp_ident a
        | Pexp_constant a ->
            let a = self#constant ctx a in
            Pexp_constant a
        | Pexp_let (a, b, c) ->
            let a = self#rec_flag ctx a in
            let b = self#list self#value_binding ctx b in
            let c = self#expression ctx c in
            Pexp_let (a, b, c)
        | Pexp_function a ->
            let a = self#cases ctx a in
            Pexp_function a
        | Pexp_fun (a, b, c, d) ->
            let a = self#arg_label ctx a in
            let b = self#option self#expression ctx b in
            let c = self#pattern ctx c in
            let d = self#expression ctx d in
            Pexp_fun (a, b, c, d)
        | Pexp_apply (a, b) ->
            let a = self#expression ctx a in
            let b =
              self#list
                (fun ctx (a, b) ->
                  let a = self#arg_label ctx a in
                  let b = self#expression ctx b in
                  (a, b))
                ctx b
            in
            Pexp_apply (a, b)
        | Pexp_match (a, b) ->
            let a = self#expression ctx a in
            let b = self#cases ctx b in
            Pexp_match (a, b)
        | Pexp_try (a, b) ->
            let a = self#expression ctx a in
            let b = self#cases ctx b in
            Pexp_try (a, b)
        | Pexp_tuple a ->
            let a = self#list self#expression ctx a in
            Pexp_tuple a
        | Pexp_construct (a, b) ->
            let a = self#longident_loc ctx a in
            let b = self#option self#expression ctx b in
            Pexp_construct (a, b)
        | Pexp_variant (a, b) ->
            let a = self#label ctx a in
            let b = self#option self#expression ctx b in
            Pexp_variant (a, b)
        | Pexp_record (a, b) ->
            let a =
              self#list
                (fun ctx (a, b) ->
                  let a = self#longident_loc ctx a in
                  let b = self#expression ctx b in
                  (a, b))
                ctx a
            in
            let b = self#option self#expression ctx b in
            Pexp_record (a, b)
        | Pexp_field (a, b) ->
            let a = self#expression ctx a in
            let b = self#longident_loc ctx b in
            Pexp_field (a, b)
        | Pexp_setfield (a, b, c) ->
            let a = self#expression ctx a in
            let b = self#longident_loc ctx b in
            let c = self#expression ctx c in
            Pexp_setfield (a, b, c)
        | Pexp_array a ->
            let a = self#list self#expression ctx a in
            Pexp_array a
        | Pexp_ifthenelse (a, b, c) ->
            let a = self#expression ctx a in
            let b = self#expression ctx b in
            let c = self#option self#expression ctx c in
            Pexp_ifthenelse (a, b, c)
        | Pexp_sequence (a, b) ->
            let a = self#expression ctx a in
            let b = self#expression ctx b in
            Pexp_sequence (a, b)
        | Pexp_while (a, b) ->
            let a = self#expression ctx a in
            let b = self#expression ctx b in
            Pexp_while (a, b)
        | Pexp_for (a, b, c, d, e) ->
            let a = self#pattern ctx a in
            let b = self#expression ctx b in
            let c = self#expression ctx c in
            let d = self#direction_flag ctx d in
            let e = self#expression ctx e in
            Pexp_for (a, b, c, d, e)
        | Pexp_constraint (a, b) ->
            let a = self#expression ctx a in
            let b = self#core_type ctx b in
            Pexp_constraint (a, b)
        | Pexp_coerce (a, b, c) ->
            let a = self#expression ctx a in
            let b = self#option self#core_type ctx b in
            let c = self#core_type ctx c in
            Pexp_coerce (a, b, c)
        | Pexp_send (a, b) ->
            let a = self#expression ctx a in
            let b = self#loc self#label ctx b in
            Pexp_send (a, b)
        | Pexp_new a ->
            let a = self#longident_loc ctx a in
            Pexp_new a
        | Pexp_setinstvar (a, b) ->
            let a = self#loc self#label ctx a in
            let b = self#expression ctx b in
            Pexp_setinstvar (a, b)
        | Pexp_override a ->
            let a =
              self#list
                (fun ctx (a, b) ->
                  let a = self#loc self#label ctx a in
                  let b = self#expression ctx b in
                  (a, b))
                ctx a
            in
            Pexp_override a
        | Pexp_letmodule (a, b, c) ->
            let a = self#loc (self#option self#string) ctx a in
            let b = self#module_expr ctx b in
            let c = self#expression ctx c in
            Pexp_letmodule (a, b, c)
        | Pexp_letexception (a, b) ->
            let a = self#extension_constructor ctx a in
            let b = self#expression ctx b in
            Pexp_letexception (a, b)
        | Pexp_assert a ->
            let a = self#expression ctx a in
            Pexp_assert a
        | Pexp_lazy a ->
            let a = self#expression ctx a in
            Pexp_lazy a
        | Pexp_poly (a, b) ->
            let a = self#expression ctx a in
            let b = self#option self#core_type ctx b in
            Pexp_poly (a, b)
        | Pexp_object a ->
            let a = self#class_structure ctx a in
            Pexp_object a
        | Pexp_newtype (a, b) ->
            let a = self#loc self#string ctx a in
            let b = self#expression ctx b in
            Pexp_newtype (a, b)
        | Pexp_pack a ->
            let a = self#module_expr ctx a in
            Pexp_pack a
        | Pexp_open (a, b) ->
            let a = self#open_declaration ctx a in
            let b = self#expression ctx b in
            Pexp_open (a, b)
        | Pexp_letop a ->
            let a = self#letop ctx a in
            Pexp_letop a
        | Pexp_extension a ->
            let a = self#extension ctx a in
            Pexp_extension a
        | Pexp_unreachable -> Pexp_unreachable

    method case : 'ctx -> case -> case =
      fun ctx { pc_lhs; pc_guard; pc_rhs } ->
        let pc_lhs = self#pattern ctx pc_lhs in
        let pc_guard = self#option self#expression ctx pc_guard in
        let pc_rhs = self#expression ctx pc_rhs in
        { pc_lhs; pc_guard; pc_rhs }

    method letop : 'ctx -> letop -> letop =
      fun ctx { let_; ands; body } ->
        let let_ = self#binding_op ctx let_ in
        let ands = self#list self#binding_op ctx ands in
        let body = self#expression ctx body in
        { let_; ands; body }

    method binding_op : 'ctx -> binding_op -> binding_op =
      fun ctx { pbop_op; pbop_pat; pbop_exp; pbop_loc } ->
        let pbop_op = self#loc self#string ctx pbop_op in
        let pbop_pat = self#pattern ctx pbop_pat in
        let pbop_exp = self#expression ctx pbop_exp in
        let pbop_loc = self#location ctx pbop_loc in
        { pbop_op; pbop_pat; pbop_exp; pbop_loc }

    method value_description : 'ctx -> value_description -> value_description =
      fun ctx { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } ->
        let pval_name = self#loc self#string ctx pval_name in
        let pval_type = self#core_type ctx pval_type in
        let pval_prim = self#list self#string ctx pval_prim in
        let pval_attributes = self#attributes ctx pval_attributes in
        let pval_loc = self#location ctx pval_loc in
        { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }

    method type_declaration : 'ctx -> type_declaration -> type_declaration =
      fun ctx
          {
            ptype_name;
            ptype_params;
            ptype_cstrs;
            ptype_kind;
            ptype_private;
            ptype_manifest;
            ptype_attributes;
            ptype_loc;
          } ->
        let ptype_name = self#loc self#string ctx ptype_name in
        let ptype_params =
          self#list
            (fun ctx (a, b) ->
              let a = self#core_type ctx a in
              let b =
                (fun ctx (a, b) ->
                  let a = self#variance ctx a in
                  let b = self#injectivity ctx b in
                  (a, b))
                  ctx b
              in
              (a, b))
            ctx ptype_params
        in
        let ptype_cstrs =
          self#list
            (fun ctx (a, b, c) ->
              let a = self#core_type ctx a in
              let b = self#core_type ctx b in
              let c = self#location ctx c in
              (a, b, c))
            ctx ptype_cstrs
        in
        let ptype_kind = self#type_kind ctx ptype_kind in
        let ptype_private = self#private_flag ctx ptype_private in
        let ptype_manifest = self#option self#core_type ctx ptype_manifest in
        let ptype_attributes = self#attributes ctx ptype_attributes in
        let ptype_loc = self#location ctx ptype_loc in
        {
          ptype_name;
          ptype_params;
          ptype_cstrs;
          ptype_kind;
          ptype_private;
          ptype_manifest;
          ptype_attributes;
          ptype_loc;
        }

    method type_kind : 'ctx -> type_kind -> type_kind =
      fun ctx x ->
        match x with
        | Ptype_abstract -> Ptype_abstract
        | Ptype_variant a ->
            let a = self#list self#constructor_declaration ctx a in
            Ptype_variant a
        | Ptype_record a ->
            let a = self#list self#label_declaration ctx a in
            Ptype_record a
        | Ptype_open -> Ptype_open

    method label_declaration : 'ctx -> label_declaration -> label_declaration =
      fun ctx { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } ->
        let pld_name = self#loc self#string ctx pld_name in
        let pld_mutable = self#mutable_flag ctx pld_mutable in
        let pld_type = self#core_type ctx pld_type in
        let pld_loc = self#location ctx pld_loc in
        let pld_attributes = self#attributes ctx pld_attributes in
        { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }

    method constructor_declaration
        : 'ctx -> constructor_declaration -> constructor_declaration =
      fun ctx { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } ->
        let pcd_name = self#loc self#string ctx pcd_name in
        let pcd_args = self#constructor_arguments ctx pcd_args in
        let pcd_res = self#option self#core_type ctx pcd_res in
        let pcd_loc = self#location ctx pcd_loc in
        let pcd_attributes = self#attributes ctx pcd_attributes in
        { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }

    method constructor_arguments
        : 'ctx -> constructor_arguments -> constructor_arguments =
      fun ctx x ->
        match x with
        | Pcstr_tuple a ->
            let a = self#list self#core_type ctx a in
            Pcstr_tuple a
        | Pcstr_record a ->
            let a = self#list self#label_declaration ctx a in
            Pcstr_record a

    method type_extension : 'ctx -> type_extension -> type_extension =
      fun ctx
          {
            ptyext_path;
            ptyext_params;
            ptyext_constructors;
            ptyext_private;
            ptyext_loc;
            ptyext_attributes;
          } ->
        let ptyext_path = self#longident_loc ctx ptyext_path in
        let ptyext_params =
          self#list
            (fun ctx (a, b) ->
              let a = self#core_type ctx a in
              let b =
                (fun ctx (a, b) ->
                  let a = self#variance ctx a in
                  let b = self#injectivity ctx b in
                  (a, b))
                  ctx b
              in
              (a, b))
            ctx ptyext_params
        in
        let ptyext_constructors =
          self#list self#extension_constructor ctx ptyext_constructors
        in
        let ptyext_private = self#private_flag ctx ptyext_private in
        let ptyext_loc = self#location ctx ptyext_loc in
        let ptyext_attributes = self#attributes ctx ptyext_attributes in
        {
          ptyext_path;
          ptyext_params;
          ptyext_constructors;
          ptyext_private;
          ptyext_loc;
          ptyext_attributes;
        }

    method extension_constructor
        : 'ctx -> extension_constructor -> extension_constructor =
      fun ctx { pext_name; pext_kind; pext_loc; pext_attributes } ->
        let pext_name = self#loc self#string ctx pext_name in
        let pext_kind = self#extension_constructor_kind ctx pext_kind in
        let pext_loc = self#location ctx pext_loc in
        let pext_attributes = self#attributes ctx pext_attributes in
        { pext_name; pext_kind; pext_loc; pext_attributes }

    method type_exception : 'ctx -> type_exception -> type_exception =
      fun ctx { ptyexn_constructor; ptyexn_loc; ptyexn_attributes } ->
        let ptyexn_constructor =
          self#extension_constructor ctx ptyexn_constructor
        in
        let ptyexn_loc = self#location ctx ptyexn_loc in
        let ptyexn_attributes = self#attributes ctx ptyexn_attributes in
        { ptyexn_constructor; ptyexn_loc; ptyexn_attributes }

    method extension_constructor_kind
        : 'ctx -> extension_constructor_kind -> extension_constructor_kind =
      fun ctx x ->
        match x with
        | Pext_decl (a, b) ->
            let a = self#constructor_arguments ctx a in
            let b = self#option self#core_type ctx b in
            Pext_decl (a, b)
        | Pext_rebind a ->
            let a = self#longident_loc ctx a in
            Pext_rebind a

    method class_type : 'ctx -> class_type -> class_type =
      fun ctx { pcty_desc; pcty_loc; pcty_attributes } ->
        let pcty_desc = self#class_type_desc ctx pcty_desc in
        let pcty_loc = self#location ctx pcty_loc in
        let pcty_attributes = self#attributes ctx pcty_attributes in
        { pcty_desc; pcty_loc; pcty_attributes }

    method class_type_desc : 'ctx -> class_type_desc -> class_type_desc =
      fun ctx x ->
        match x with
        | Pcty_constr (a, b) ->
            let a = self#longident_loc ctx a in
            let b = self#list self#core_type ctx b in
            Pcty_constr (a, b)
        | Pcty_signature a ->
            let a = self#class_signature ctx a in
            Pcty_signature a
        | Pcty_arrow (a, b, c) ->
            let a = self#arg_label ctx a in
            let b = self#core_type ctx b in
            let c = self#class_type ctx c in
            Pcty_arrow (a, b, c)
        | Pcty_extension a ->
            let a = self#extension ctx a in
            Pcty_extension a
        | Pcty_open (a, b) ->
            let a = self#open_description ctx a in
            let b = self#class_type ctx b in
            Pcty_open (a, b)

    method class_signature : 'ctx -> class_signature -> class_signature =
      fun ctx { pcsig_self; pcsig_fields } ->
        let pcsig_self = self#core_type ctx pcsig_self in
        let pcsig_fields = self#list self#class_type_field ctx pcsig_fields in
        { pcsig_self; pcsig_fields }

    method class_type_field : 'ctx -> class_type_field -> class_type_field =
      fun ctx { pctf_desc; pctf_loc; pctf_attributes } ->
        let pctf_desc = self#class_type_field_desc ctx pctf_desc in
        let pctf_loc = self#location ctx pctf_loc in
        let pctf_attributes = self#attributes ctx pctf_attributes in
        { pctf_desc; pctf_loc; pctf_attributes }

    method class_type_field_desc
        : 'ctx -> class_type_field_desc -> class_type_field_desc =
      fun ctx x ->
        match x with
        | Pctf_inherit a ->
            let a = self#class_type ctx a in
            Pctf_inherit a
        | Pctf_val a ->
            let a =
              (fun ctx (a, b, c, d) ->
                let a = self#loc self#label ctx a in
                let b = self#mutable_flag ctx b in
                let c = self#virtual_flag ctx c in
                let d = self#core_type ctx d in
                (a, b, c, d))
                ctx a
            in
            Pctf_val a
        | Pctf_method a ->
            let a =
              (fun ctx (a, b, c, d) ->
                let a = self#loc self#label ctx a in
                let b = self#private_flag ctx b in
                let c = self#virtual_flag ctx c in
                let d = self#core_type ctx d in
                (a, b, c, d))
                ctx a
            in
            Pctf_method a
        | Pctf_constraint a ->
            let a =
              (fun ctx (a, b) ->
                let a = self#core_type ctx a in
                let b = self#core_type ctx b in
                (a, b))
                ctx a
            in
            Pctf_constraint a
        | Pctf_attribute a ->
            let a = self#attribute ctx a in
            Pctf_attribute a
        | Pctf_extension a ->
            let a = self#extension ctx a in
            Pctf_extension a

    method class_infos
        : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a class_infos -> 'a class_infos =
      fun _a ctx
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
        let pci_virt = self#virtual_flag ctx pci_virt in
        let pci_params =
          self#list
            (fun ctx (a, b) ->
              let a = self#core_type ctx a in
              let b =
                (fun ctx (a, b) ->
                  let a = self#variance ctx a in
                  let b = self#injectivity ctx b in
                  (a, b))
                  ctx b
              in
              (a, b))
            ctx pci_params
        in
        let pci_name = self#loc self#string ctx pci_name in
        let pci_expr = _a ctx pci_expr in
        let pci_loc = self#location ctx pci_loc in
        let pci_attributes = self#attributes ctx pci_attributes in
        { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }

    method class_description : 'ctx -> class_description -> class_description =
      self#class_infos self#class_type

    method class_type_declaration
        : 'ctx -> class_type_declaration -> class_type_declaration =
      self#class_infos self#class_type

    method class_expr : 'ctx -> class_expr -> class_expr =
      fun ctx { pcl_desc; pcl_loc; pcl_attributes } ->
        let pcl_desc = self#class_expr_desc ctx pcl_desc in
        let pcl_loc = self#location ctx pcl_loc in
        let pcl_attributes = self#attributes ctx pcl_attributes in
        { pcl_desc; pcl_loc; pcl_attributes }

    method class_expr_desc : 'ctx -> class_expr_desc -> class_expr_desc =
      fun ctx x ->
        match x with
        | Pcl_constr (a, b) ->
            let a = self#longident_loc ctx a in
            let b = self#list self#core_type ctx b in
            Pcl_constr (a, b)
        | Pcl_structure a ->
            let a = self#class_structure ctx a in
            Pcl_structure a
        | Pcl_fun (a, b, c, d) ->
            let a = self#arg_label ctx a in
            let b = self#option self#expression ctx b in
            let c = self#pattern ctx c in
            let d = self#class_expr ctx d in
            Pcl_fun (a, b, c, d)
        | Pcl_apply (a, b) ->
            let a = self#class_expr ctx a in
            let b =
              self#list
                (fun ctx (a, b) ->
                  let a = self#arg_label ctx a in
                  let b = self#expression ctx b in
                  (a, b))
                ctx b
            in
            Pcl_apply (a, b)
        | Pcl_let (a, b, c) ->
            let a = self#rec_flag ctx a in
            let b = self#list self#value_binding ctx b in
            let c = self#class_expr ctx c in
            Pcl_let (a, b, c)
        | Pcl_constraint (a, b) ->
            let a = self#class_expr ctx a in
            let b = self#class_type ctx b in
            Pcl_constraint (a, b)
        | Pcl_extension a ->
            let a = self#extension ctx a in
            Pcl_extension a
        | Pcl_open (a, b) ->
            let a = self#open_description ctx a in
            let b = self#class_expr ctx b in
            Pcl_open (a, b)

    method class_structure : 'ctx -> class_structure -> class_structure =
      fun ctx { pcstr_self; pcstr_fields } ->
        let pcstr_self = self#pattern ctx pcstr_self in
        let pcstr_fields = self#list self#class_field ctx pcstr_fields in
        { pcstr_self; pcstr_fields }

    method class_field : 'ctx -> class_field -> class_field =
      fun ctx { pcf_desc; pcf_loc; pcf_attributes } ->
        let pcf_desc = self#class_field_desc ctx pcf_desc in
        let pcf_loc = self#location ctx pcf_loc in
        let pcf_attributes = self#attributes ctx pcf_attributes in
        { pcf_desc; pcf_loc; pcf_attributes }

    method class_field_desc : 'ctx -> class_field_desc -> class_field_desc =
      fun ctx x ->
        match x with
        | Pcf_inherit (a, b, c) ->
            let a = self#override_flag ctx a in
            let b = self#class_expr ctx b in
            let c = self#option (self#loc self#string) ctx c in
            Pcf_inherit (a, b, c)
        | Pcf_val a ->
            let a =
              (fun ctx (a, b, c) ->
                let a = self#loc self#label ctx a in
                let b = self#mutable_flag ctx b in
                let c = self#class_field_kind ctx c in
                (a, b, c))
                ctx a
            in
            Pcf_val a
        | Pcf_method a ->
            let a =
              (fun ctx (a, b, c) ->
                let a = self#loc self#label ctx a in
                let b = self#private_flag ctx b in
                let c = self#class_field_kind ctx c in
                (a, b, c))
                ctx a
            in
            Pcf_method a
        | Pcf_constraint a ->
            let a =
              (fun ctx (a, b) ->
                let a = self#core_type ctx a in
                let b = self#core_type ctx b in
                (a, b))
                ctx a
            in
            Pcf_constraint a
        | Pcf_initializer a ->
            let a = self#expression ctx a in
            Pcf_initializer a
        | Pcf_attribute a ->
            let a = self#attribute ctx a in
            Pcf_attribute a
        | Pcf_extension a ->
            let a = self#extension ctx a in
            Pcf_extension a

    method class_field_kind : 'ctx -> class_field_kind -> class_field_kind =
      fun ctx x ->
        match x with
        | Cfk_virtual a ->
            let a = self#core_type ctx a in
            Cfk_virtual a
        | Cfk_concrete (a, b) ->
            let a = self#override_flag ctx a in
            let b = self#expression ctx b in
            Cfk_concrete (a, b)

    method class_declaration : 'ctx -> class_declaration -> class_declaration =
      self#class_infos self#class_expr

    method module_type : 'ctx -> module_type -> module_type =
      fun ctx { pmty_desc; pmty_loc; pmty_attributes } ->
        let pmty_desc = self#module_type_desc ctx pmty_desc in
        let pmty_loc = self#location ctx pmty_loc in
        let pmty_attributes = self#attributes ctx pmty_attributes in
        { pmty_desc; pmty_loc; pmty_attributes }

    method module_type_desc : 'ctx -> module_type_desc -> module_type_desc =
      fun ctx x ->
        match x with
        | Pmty_ident a ->
            let a = self#longident_loc ctx a in
            Pmty_ident a
        | Pmty_signature a ->
            let a = self#signature ctx a in
            Pmty_signature a
        | Pmty_functor (a, b) ->
            let a = self#functor_parameter ctx a in
            let b = self#module_type ctx b in
            Pmty_functor (a, b)
        | Pmty_with (a, b) ->
            let a = self#module_type ctx a in
            let b = self#list self#with_constraint ctx b in
            Pmty_with (a, b)
        | Pmty_typeof a ->
            let a = self#module_expr ctx a in
            Pmty_typeof a
        | Pmty_extension a ->
            let a = self#extension ctx a in
            Pmty_extension a
        | Pmty_alias a ->
            let a = self#longident_loc ctx a in
            Pmty_alias a

    method functor_parameter : 'ctx -> functor_parameter -> functor_parameter =
      fun ctx x ->
        match x with
        | Unit -> Unit
        | Named (a, b) ->
            let a = self#loc (self#option self#string) ctx a in
            let b = self#module_type ctx b in
            Named (a, b)

    method signature : 'ctx -> signature -> signature =
      self#list self#signature_item

    method signature_item : 'ctx -> signature_item -> signature_item =
      fun ctx { psig_desc; psig_loc } ->
        let psig_desc = self#signature_item_desc ctx psig_desc in
        let psig_loc = self#location ctx psig_loc in
        { psig_desc; psig_loc }

    method signature_item_desc
        : 'ctx -> signature_item_desc -> signature_item_desc =
      fun ctx x ->
        match x with
        | Psig_value a ->
            let a = self#value_description ctx a in
            Psig_value a
        | Psig_type (a, b) ->
            let a = self#rec_flag ctx a in
            let b = self#list self#type_declaration ctx b in
            Psig_type (a, b)
        | Psig_typesubst a ->
            let a = self#list self#type_declaration ctx a in
            Psig_typesubst a
        | Psig_typext a ->
            let a = self#type_extension ctx a in
            Psig_typext a
        | Psig_exception a ->
            let a = self#type_exception ctx a in
            Psig_exception a
        | Psig_module a ->
            let a = self#module_declaration ctx a in
            Psig_module a
        | Psig_modsubst a ->
            let a = self#module_substitution ctx a in
            Psig_modsubst a
        | Psig_recmodule a ->
            let a = self#list self#module_declaration ctx a in
            Psig_recmodule a
        | Psig_modtype a ->
            let a = self#module_type_declaration ctx a in
            Psig_modtype a
        | Psig_open a ->
            let a = self#open_description ctx a in
            Psig_open a
        | Psig_include a ->
            let a = self#include_description ctx a in
            Psig_include a
        | Psig_class a ->
            let a = self#list self#class_description ctx a in
            Psig_class a
        | Psig_class_type a ->
            let a = self#list self#class_type_declaration ctx a in
            Psig_class_type a
        | Psig_attribute a ->
            let a = self#attribute ctx a in
            Psig_attribute a
        | Psig_extension (a, b) ->
            let a = self#extension ctx a in
            let b = self#attributes ctx b in
            Psig_extension (a, b)

    method module_declaration : 'ctx -> module_declaration -> module_declaration
        =
      fun ctx { pmd_name; pmd_type; pmd_attributes; pmd_loc } ->
        let pmd_name = self#loc (self#option self#string) ctx pmd_name in
        let pmd_type = self#module_type ctx pmd_type in
        let pmd_attributes = self#attributes ctx pmd_attributes in
        let pmd_loc = self#location ctx pmd_loc in
        { pmd_name; pmd_type; pmd_attributes; pmd_loc }

    method module_substitution
        : 'ctx -> module_substitution -> module_substitution =
      fun ctx { pms_name; pms_manifest; pms_attributes; pms_loc } ->
        let pms_name = self#loc self#string ctx pms_name in
        let pms_manifest = self#longident_loc ctx pms_manifest in
        let pms_attributes = self#attributes ctx pms_attributes in
        let pms_loc = self#location ctx pms_loc in
        { pms_name; pms_manifest; pms_attributes; pms_loc }

    method module_type_declaration
        : 'ctx -> module_type_declaration -> module_type_declaration =
      fun ctx { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } ->
        let pmtd_name = self#loc self#string ctx pmtd_name in
        let pmtd_type = self#option self#module_type ctx pmtd_type in
        let pmtd_attributes = self#attributes ctx pmtd_attributes in
        let pmtd_loc = self#location ctx pmtd_loc in
        { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }

    method open_infos
        : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a open_infos -> 'a open_infos =
      fun _a ctx { popen_expr; popen_override; popen_loc; popen_attributes } ->
        let popen_expr = _a ctx popen_expr in
        let popen_override = self#override_flag ctx popen_override in
        let popen_loc = self#location ctx popen_loc in
        let popen_attributes = self#attributes ctx popen_attributes in
        { popen_expr; popen_override; popen_loc; popen_attributes }

    method open_description : 'ctx -> open_description -> open_description =
      self#open_infos self#longident_loc

    method open_declaration : 'ctx -> open_declaration -> open_declaration =
      self#open_infos self#module_expr

    method include_infos
        : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a include_infos -> 'a include_infos
        =
      fun _a ctx { pincl_mod; pincl_loc; pincl_attributes } ->
        let pincl_mod = _a ctx pincl_mod in
        let pincl_loc = self#location ctx pincl_loc in
        let pincl_attributes = self#attributes ctx pincl_attributes in
        { pincl_mod; pincl_loc; pincl_attributes }

    method include_description
        : 'ctx -> include_description -> include_description =
      self#include_infos self#module_type

    method include_declaration
        : 'ctx -> include_declaration -> include_declaration =
      self#include_infos self#module_expr

    method with_constraint : 'ctx -> with_constraint -> with_constraint =
      fun ctx x ->
        match x with
        | Pwith_type (a, b) ->
            let a = self#longident_loc ctx a in
            let b = self#type_declaration ctx b in
            Pwith_type (a, b)
        | Pwith_module (a, b) ->
            let a = self#longident_loc ctx a in
            let b = self#longident_loc ctx b in
            Pwith_module (a, b)
        | Pwith_typesubst (a, b) ->
            let a = self#longident_loc ctx a in
            let b = self#type_declaration ctx b in
            Pwith_typesubst (a, b)
        | Pwith_modsubst (a, b) ->
            let a = self#longident_loc ctx a in
            let b = self#longident_loc ctx b in
            Pwith_modsubst (a, b)

    method module_expr : 'ctx -> module_expr -> module_expr =
      fun ctx { pmod_desc; pmod_loc; pmod_attributes } ->
        let pmod_desc = self#module_expr_desc ctx pmod_desc in
        let pmod_loc = self#location ctx pmod_loc in
        let pmod_attributes = self#attributes ctx pmod_attributes in
        { pmod_desc; pmod_loc; pmod_attributes }

    method module_expr_desc : 'ctx -> module_expr_desc -> module_expr_desc =
      fun ctx x ->
        match x with
        | Pmod_ident a ->
            let a = self#longident_loc ctx a in
            Pmod_ident a
        | Pmod_structure a ->
            let a = self#structure ctx a in
            Pmod_structure a
        | Pmod_functor (a, b) ->
            let a = self#functor_parameter ctx a in
            let b = self#module_expr ctx b in
            Pmod_functor (a, b)
        | Pmod_apply (a, b) ->
            let a = self#module_expr ctx a in
            let b = self#module_expr ctx b in
            Pmod_apply (a, b)
        | Pmod_constraint (a, b) ->
            let a = self#module_expr ctx a in
            let b = self#module_type ctx b in
            Pmod_constraint (a, b)
        | Pmod_unpack a ->
            let a = self#expression ctx a in
            Pmod_unpack a
        | Pmod_extension a ->
            let a = self#extension ctx a in
            Pmod_extension a

    method structure : 'ctx -> structure -> structure =
      self#list self#structure_item

    method structure_item : 'ctx -> structure_item -> structure_item =
      fun ctx { pstr_desc; pstr_loc } ->
        let pstr_desc = self#structure_item_desc ctx pstr_desc in
        let pstr_loc = self#location ctx pstr_loc in
        { pstr_desc; pstr_loc }

    method structure_item_desc
        : 'ctx -> structure_item_desc -> structure_item_desc =
      fun ctx x ->
        match x with
        | Pstr_eval (a, b) ->
            let a = self#expression ctx a in
            let b = self#attributes ctx b in
            Pstr_eval (a, b)
        | Pstr_value (a, b) ->
            let a = self#rec_flag ctx a in
            let b = self#list self#value_binding ctx b in
            Pstr_value (a, b)
        | Pstr_primitive a ->
            let a = self#value_description ctx a in
            Pstr_primitive a
        | Pstr_type (a, b) ->
            let a = self#rec_flag ctx a in
            let b = self#list self#type_declaration ctx b in
            Pstr_type (a, b)
        | Pstr_typext a ->
            let a = self#type_extension ctx a in
            Pstr_typext a
        | Pstr_exception a ->
            let a = self#type_exception ctx a in
            Pstr_exception a
        | Pstr_module a ->
            let a = self#module_binding ctx a in
            Pstr_module a
        | Pstr_recmodule a ->
            let a = self#list self#module_binding ctx a in
            Pstr_recmodule a
        | Pstr_modtype a ->
            let a = self#module_type_declaration ctx a in
            Pstr_modtype a
        | Pstr_open a ->
            let a = self#open_declaration ctx a in
            Pstr_open a
        | Pstr_class a ->
            let a = self#list self#class_declaration ctx a in
            Pstr_class a
        | Pstr_class_type a ->
            let a = self#list self#class_type_declaration ctx a in
            Pstr_class_type a
        | Pstr_include a ->
            let a = self#include_declaration ctx a in
            Pstr_include a
        | Pstr_attribute a ->
            let a = self#attribute ctx a in
            Pstr_attribute a
        | Pstr_extension (a, b) ->
            let a = self#extension ctx a in
            let b = self#attributes ctx b in
            Pstr_extension (a, b)

    method value_binding : 'ctx -> value_binding -> value_binding =
      fun ctx { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } ->
        let pvb_pat = self#pattern ctx pvb_pat in
        let pvb_expr = self#expression ctx pvb_expr in
        let pvb_attributes = self#attributes ctx pvb_attributes in
        let pvb_loc = self#location ctx pvb_loc in
        { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }

    method module_binding : 'ctx -> module_binding -> module_binding =
      fun ctx { pmb_name; pmb_expr; pmb_attributes; pmb_loc } ->
        let pmb_name = self#loc (self#option self#string) ctx pmb_name in
        let pmb_expr = self#module_expr ctx pmb_expr in
        let pmb_attributes = self#attributes ctx pmb_attributes in
        let pmb_loc = self#location ctx pmb_loc in
        { pmb_name; pmb_expr; pmb_attributes; pmb_loc }

    method toplevel_phrase : 'ctx -> toplevel_phrase -> toplevel_phrase =
      fun ctx x ->
        match x with
        | Ptop_def a ->
            let a = self#structure ctx a in
            Ptop_def a
        | Ptop_dir a ->
            let a = self#toplevel_directive ctx a in
            Ptop_dir a

    method toplevel_directive : 'ctx -> toplevel_directive -> toplevel_directive
        =
      fun ctx { pdir_name; pdir_arg; pdir_loc } ->
        let pdir_name = self#loc self#string ctx pdir_name in
        let pdir_arg = self#option self#directive_argument ctx pdir_arg in
        let pdir_loc = self#location ctx pdir_loc in
        { pdir_name; pdir_arg; pdir_loc }

    method directive_argument : 'ctx -> directive_argument -> directive_argument
        =
      fun ctx { pdira_desc; pdira_loc } ->
        let pdira_desc = self#directive_argument_desc ctx pdira_desc in
        let pdira_loc = self#location ctx pdira_loc in
        { pdira_desc; pdira_loc }

    method directive_argument_desc
        : 'ctx -> directive_argument_desc -> directive_argument_desc =
      fun ctx x ->
        match x with
        | Pdir_string a ->
            let a = self#string ctx a in
            Pdir_string a
        | Pdir_int (a, b) ->
            let a = self#string ctx a in
            let b = self#option self#char ctx b in
            Pdir_int (a, b)
        | Pdir_ident a ->
            let a = self#longident ctx a in
            Pdir_ident a
        | Pdir_bool a ->
            let a = self#bool ctx a in
            Pdir_bool a

    method cases : 'ctx -> cases -> cases = self#list self#case
  end

class virtual ['res] lift =
  object (self)
    method virtual record : (string * 'res) list -> 'res

    method virtual constr : string -> 'res list -> 'res

    method virtual tuple : 'res list -> 'res

    method virtual bool : bool -> 'res

    method virtual char : char -> 'res

    method virtual int : int -> 'res

    method virtual list : 'a. ('a -> 'res) -> 'a list -> 'res

    method virtual option : 'a. ('a -> 'res) -> 'a option -> 'res

    method virtual string : string -> 'res

    method position : position -> 'res =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
        let pos_fname = self#string pos_fname in
        let pos_lnum = self#int pos_lnum in
        let pos_bol = self#int pos_bol in
        let pos_cnum = self#int pos_cnum in
        self#record
          [
            ("pos_fname", pos_fname);
            ("pos_lnum", pos_lnum);
            ("pos_bol", pos_bol);
            ("pos_cnum", pos_cnum);
          ]

    method location : location -> 'res =
      fun { loc_start; loc_end; loc_ghost } ->
        let loc_start = self#position loc_start in
        let loc_end = self#position loc_end in
        let loc_ghost = self#bool loc_ghost in
        self#record
          [
            ("loc_start", loc_start);
            ("loc_end", loc_end);
            ("loc_ghost", loc_ghost);
          ]

    method location_stack : location_stack -> 'res = self#list self#location

    method loc : 'a. ('a -> 'res) -> 'a loc -> 'res =
      fun _a { txt; loc } ->
        let txt = _a txt in
        let loc = self#location loc in
        self#record [ ("txt", txt); ("loc", loc) ]

    method longident : longident -> 'res =
      fun x ->
        match x with
        | Lident a ->
            let a = self#string a in
            self#constr "Lident" [ a ]
        | Ldot (a, b) ->
            let a = self#longident a in
            let b = self#string b in
            self#constr "Ldot" [ a; b ]
        | Lapply (a, b) ->
            let a = self#longident a in
            let b = self#longident b in
            self#constr "Lapply" [ a; b ]

    method longident_loc : longident_loc -> 'res = self#loc self#longident

    method rec_flag : rec_flag -> 'res =
      fun x ->
        match x with
        | Nonrecursive -> self#constr "Nonrecursive" []
        | Recursive -> self#constr "Recursive" []

    method direction_flag : direction_flag -> 'res =
      fun x ->
        match x with
        | Upto -> self#constr "Upto" []
        | Downto -> self#constr "Downto" []

    method private_flag : private_flag -> 'res =
      fun x ->
        match x with
        | Private -> self#constr "Private" []
        | Public -> self#constr "Public" []

    method mutable_flag : mutable_flag -> 'res =
      fun x ->
        match x with
        | Immutable -> self#constr "Immutable" []
        | Mutable -> self#constr "Mutable" []

    method virtual_flag : virtual_flag -> 'res =
      fun x ->
        match x with
        | Virtual -> self#constr "Virtual" []
        | Concrete -> self#constr "Concrete" []

    method override_flag : override_flag -> 'res =
      fun x ->
        match x with
        | Override -> self#constr "Override" []
        | Fresh -> self#constr "Fresh" []

    method closed_flag : closed_flag -> 'res =
      fun x ->
        match x with
        | Closed -> self#constr "Closed" []
        | Open -> self#constr "Open" []

    method label : label -> 'res = self#string

    method arg_label : arg_label -> 'res =
      fun x ->
        match x with
        | Nolabel -> self#constr "Nolabel" []
        | Labelled a ->
            let a = self#string a in
            self#constr "Labelled" [ a ]
        | Optional a ->
            let a = self#string a in
            self#constr "Optional" [ a ]

    method variance : variance -> 'res =
      fun x ->
        match x with
        | Covariant -> self#constr "Covariant" []
        | Contravariant -> self#constr "Contravariant" []
        | NoVariance -> self#constr "NoVariance" []

    method injectivity : injectivity -> 'res =
      fun x ->
        match x with
        | Injective -> self#constr "Injective" []
        | NoInjectivity -> self#constr "NoInjectivity" []

    method constant : constant -> 'res =
      fun x ->
        match x with
        | Pconst_integer (a, b) ->
            let a = self#string a in
            let b = self#option self#char b in
            self#constr "Pconst_integer" [ a; b ]
        | Pconst_char a ->
            let a = self#char a in
            self#constr "Pconst_char" [ a ]
        | Pconst_string (a, b, c) ->
            let a = self#string a in
            let b = self#location b in
            let c = self#option self#string c in
            self#constr "Pconst_string" [ a; b; c ]
        | Pconst_float (a, b) ->
            let a = self#string a in
            let b = self#option self#char b in
            self#constr "Pconst_float" [ a; b ]

    method attribute : attribute -> 'res =
      fun { attr_name; attr_payload; attr_loc } ->
        let attr_name = self#loc self#string attr_name in
        let attr_payload = self#payload attr_payload in
        let attr_loc = self#location attr_loc in
        self#record
          [
            ("attr_name", attr_name);
            ("attr_payload", attr_payload);
            ("attr_loc", attr_loc);
          ]

    method extension : extension -> 'res =
      fun (a, b) ->
        let a = self#loc self#string a in
        let b = self#payload b in
        self#tuple [ a; b ]

    method attributes : attributes -> 'res = self#list self#attribute

    method payload : payload -> 'res =
      fun x ->
        match x with
        | PStr a ->
            let a = self#structure a in
            self#constr "PStr" [ a ]
        | PSig a ->
            let a = self#signature a in
            self#constr "PSig" [ a ]
        | PTyp a ->
            let a = self#core_type a in
            self#constr "PTyp" [ a ]
        | PPat (a, b) ->
            let a = self#pattern a in
            let b = self#option self#expression b in
            self#constr "PPat" [ a; b ]

    method core_type : core_type -> 'res =
      fun { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes } ->
        let ptyp_desc = self#core_type_desc ptyp_desc in
        let ptyp_loc = self#location ptyp_loc in
        let ptyp_loc_stack = self#location_stack ptyp_loc_stack in
        let ptyp_attributes = self#attributes ptyp_attributes in
        self#record
          [
            ("ptyp_desc", ptyp_desc);
            ("ptyp_loc", ptyp_loc);
            ("ptyp_loc_stack", ptyp_loc_stack);
            ("ptyp_attributes", ptyp_attributes);
          ]

    method core_type_desc : core_type_desc -> 'res =
      fun x ->
        match x with
        | Ptyp_any -> self#constr "Ptyp_any" []
        | Ptyp_var a ->
            let a = self#string a in
            self#constr "Ptyp_var" [ a ]
        | Ptyp_arrow (a, b, c) ->
            let a = self#arg_label a in
            let b = self#core_type b in
            let c = self#core_type c in
            self#constr "Ptyp_arrow" [ a; b; c ]
        | Ptyp_tuple a ->
            let a = self#list self#core_type a in
            self#constr "Ptyp_tuple" [ a ]
        | Ptyp_constr (a, b) ->
            let a = self#longident_loc a in
            let b = self#list self#core_type b in
            self#constr "Ptyp_constr" [ a; b ]
        | Ptyp_object (a, b) ->
            let a = self#list self#object_field a in
            let b = self#closed_flag b in
            self#constr "Ptyp_object" [ a; b ]
        | Ptyp_class (a, b) ->
            let a = self#longident_loc a in
            let b = self#list self#core_type b in
            self#constr "Ptyp_class" [ a; b ]
        | Ptyp_alias (a, b) ->
            let a = self#core_type a in
            let b = self#string b in
            self#constr "Ptyp_alias" [ a; b ]
        | Ptyp_variant (a, b, c) ->
            let a = self#list self#row_field a in
            let b = self#closed_flag b in
            let c = self#option (self#list self#label) c in
            self#constr "Ptyp_variant" [ a; b; c ]
        | Ptyp_poly (a, b) ->
            let a = self#list (self#loc self#string) a in
            let b = self#core_type b in
            self#constr "Ptyp_poly" [ a; b ]
        | Ptyp_package a ->
            let a = self#package_type a in
            self#constr "Ptyp_package" [ a ]
        | Ptyp_extension a ->
            let a = self#extension a in
            self#constr "Ptyp_extension" [ a ]

    method package_type : package_type -> 'res =
      fun (a, b) ->
        let a = self#longident_loc a in
        let b =
          self#list
            (fun (a, b) ->
              let a = self#longident_loc a in
              let b = self#core_type b in
              self#tuple [ a; b ])
            b
        in
        self#tuple [ a; b ]

    method row_field : row_field -> 'res =
      fun { prf_desc; prf_loc; prf_attributes } ->
        let prf_desc = self#row_field_desc prf_desc in
        let prf_loc = self#location prf_loc in
        let prf_attributes = self#attributes prf_attributes in
        self#record
          [
            ("prf_desc", prf_desc);
            ("prf_loc", prf_loc);
            ("prf_attributes", prf_attributes);
          ]

    method row_field_desc : row_field_desc -> 'res =
      fun x ->
        match x with
        | Rtag (a, b, c) ->
            let a = self#loc self#label a in
            let b = self#bool b in
            let c = self#list self#core_type c in
            self#constr "Rtag" [ a; b; c ]
        | Rinherit a ->
            let a = self#core_type a in
            self#constr "Rinherit" [ a ]

    method object_field : object_field -> 'res =
      fun { pof_desc; pof_loc; pof_attributes } ->
        let pof_desc = self#object_field_desc pof_desc in
        let pof_loc = self#location pof_loc in
        let pof_attributes = self#attributes pof_attributes in
        self#record
          [
            ("pof_desc", pof_desc);
            ("pof_loc", pof_loc);
            ("pof_attributes", pof_attributes);
          ]

    method object_field_desc : object_field_desc -> 'res =
      fun x ->
        match x with
        | Otag (a, b) ->
            let a = self#loc self#label a in
            let b = self#core_type b in
            self#constr "Otag" [ a; b ]
        | Oinherit a ->
            let a = self#core_type a in
            self#constr "Oinherit" [ a ]

    method pattern : pattern -> 'res =
      fun { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes } ->
        let ppat_desc = self#pattern_desc ppat_desc in
        let ppat_loc = self#location ppat_loc in
        let ppat_loc_stack = self#location_stack ppat_loc_stack in
        let ppat_attributes = self#attributes ppat_attributes in
        self#record
          [
            ("ppat_desc", ppat_desc);
            ("ppat_loc", ppat_loc);
            ("ppat_loc_stack", ppat_loc_stack);
            ("ppat_attributes", ppat_attributes);
          ]

    method pattern_desc : pattern_desc -> 'res =
      fun x ->
        match x with
        | Ppat_any -> self#constr "Ppat_any" []
        | Ppat_var a ->
            let a = self#loc self#string a in
            self#constr "Ppat_var" [ a ]
        | Ppat_alias (a, b) ->
            let a = self#pattern a in
            let b = self#loc self#string b in
            self#constr "Ppat_alias" [ a; b ]
        | Ppat_constant a ->
            let a = self#constant a in
            self#constr "Ppat_constant" [ a ]
        | Ppat_interval (a, b) ->
            let a = self#constant a in
            let b = self#constant b in
            self#constr "Ppat_interval" [ a; b ]
        | Ppat_tuple a ->
            let a = self#list self#pattern a in
            self#constr "Ppat_tuple" [ a ]
        | Ppat_construct (a, b) ->
            let a = self#longident_loc a in
            let b = self#option self#pattern b in
            self#constr "Ppat_construct" [ a; b ]
        | Ppat_variant (a, b) ->
            let a = self#label a in
            let b = self#option self#pattern b in
            self#constr "Ppat_variant" [ a; b ]
        | Ppat_record (a, b) ->
            let a =
              self#list
                (fun (a, b) ->
                  let a = self#longident_loc a in
                  let b = self#pattern b in
                  self#tuple [ a; b ])
                a
            in
            let b = self#closed_flag b in
            self#constr "Ppat_record" [ a; b ]
        | Ppat_array a ->
            let a = self#list self#pattern a in
            self#constr "Ppat_array" [ a ]
        | Ppat_or (a, b) ->
            let a = self#pattern a in
            let b = self#pattern b in
            self#constr "Ppat_or" [ a; b ]
        | Ppat_constraint (a, b) ->
            let a = self#pattern a in
            let b = self#core_type b in
            self#constr "Ppat_constraint" [ a; b ]
        | Ppat_type a ->
            let a = self#longident_loc a in
            self#constr "Ppat_type" [ a ]
        | Ppat_lazy a ->
            let a = self#pattern a in
            self#constr "Ppat_lazy" [ a ]
        | Ppat_unpack a ->
            let a = self#loc (self#option self#string) a in
            self#constr "Ppat_unpack" [ a ]
        | Ppat_exception a ->
            let a = self#pattern a in
            self#constr "Ppat_exception" [ a ]
        | Ppat_extension a ->
            let a = self#extension a in
            self#constr "Ppat_extension" [ a ]
        | Ppat_open (a, b) ->
            let a = self#longident_loc a in
            let b = self#pattern b in
            self#constr "Ppat_open" [ a; b ]

    method expression : expression -> 'res =
      fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
        let pexp_desc = self#expression_desc pexp_desc in
        let pexp_loc = self#location pexp_loc in
        let pexp_loc_stack = self#location_stack pexp_loc_stack in
        let pexp_attributes = self#attributes pexp_attributes in
        self#record
          [
            ("pexp_desc", pexp_desc);
            ("pexp_loc", pexp_loc);
            ("pexp_loc_stack", pexp_loc_stack);
            ("pexp_attributes", pexp_attributes);
          ]

    method expression_desc : expression_desc -> 'res =
      fun x ->
        match x with
        | Pexp_ident a ->
            let a = self#longident_loc a in
            self#constr "Pexp_ident" [ a ]
        | Pexp_constant a ->
            let a = self#constant a in
            self#constr "Pexp_constant" [ a ]
        | Pexp_let (a, b, c) ->
            let a = self#rec_flag a in
            let b = self#list self#value_binding b in
            let c = self#expression c in
            self#constr "Pexp_let" [ a; b; c ]
        | Pexp_function a ->
            let a = self#cases a in
            self#constr "Pexp_function" [ a ]
        | Pexp_fun (a, b, c, d) ->
            let a = self#arg_label a in
            let b = self#option self#expression b in
            let c = self#pattern c in
            let d = self#expression d in
            self#constr "Pexp_fun" [ a; b; c; d ]
        | Pexp_apply (a, b) ->
            let a = self#expression a in
            let b =
              self#list
                (fun (a, b) ->
                  let a = self#arg_label a in
                  let b = self#expression b in
                  self#tuple [ a; b ])
                b
            in
            self#constr "Pexp_apply" [ a; b ]
        | Pexp_match (a, b) ->
            let a = self#expression a in
            let b = self#cases b in
            self#constr "Pexp_match" [ a; b ]
        | Pexp_try (a, b) ->
            let a = self#expression a in
            let b = self#cases b in
            self#constr "Pexp_try" [ a; b ]
        | Pexp_tuple a ->
            let a = self#list self#expression a in
            self#constr "Pexp_tuple" [ a ]
        | Pexp_construct (a, b) ->
            let a = self#longident_loc a in
            let b = self#option self#expression b in
            self#constr "Pexp_construct" [ a; b ]
        | Pexp_variant (a, b) ->
            let a = self#label a in
            let b = self#option self#expression b in
            self#constr "Pexp_variant" [ a; b ]
        | Pexp_record (a, b) ->
            let a =
              self#list
                (fun (a, b) ->
                  let a = self#longident_loc a in
                  let b = self#expression b in
                  self#tuple [ a; b ])
                a
            in
            let b = self#option self#expression b in
            self#constr "Pexp_record" [ a; b ]
        | Pexp_field (a, b) ->
            let a = self#expression a in
            let b = self#longident_loc b in
            self#constr "Pexp_field" [ a; b ]
        | Pexp_setfield (a, b, c) ->
            let a = self#expression a in
            let b = self#longident_loc b in
            let c = self#expression c in
            self#constr "Pexp_setfield" [ a; b; c ]
        | Pexp_array a ->
            let a = self#list self#expression a in
            self#constr "Pexp_array" [ a ]
        | Pexp_ifthenelse (a, b, c) ->
            let a = self#expression a in
            let b = self#expression b in
            let c = self#option self#expression c in
            self#constr "Pexp_ifthenelse" [ a; b; c ]
        | Pexp_sequence (a, b) ->
            let a = self#expression a in
            let b = self#expression b in
            self#constr "Pexp_sequence" [ a; b ]
        | Pexp_while (a, b) ->
            let a = self#expression a in
            let b = self#expression b in
            self#constr "Pexp_while" [ a; b ]
        | Pexp_for (a, b, c, d, e) ->
            let a = self#pattern a in
            let b = self#expression b in
            let c = self#expression c in
            let d = self#direction_flag d in
            let e = self#expression e in
            self#constr "Pexp_for" [ a; b; c; d; e ]
        | Pexp_constraint (a, b) ->
            let a = self#expression a in
            let b = self#core_type b in
            self#constr "Pexp_constraint" [ a; b ]
        | Pexp_coerce (a, b, c) ->
            let a = self#expression a in
            let b = self#option self#core_type b in
            let c = self#core_type c in
            self#constr "Pexp_coerce" [ a; b; c ]
        | Pexp_send (a, b) ->
            let a = self#expression a in
            let b = self#loc self#label b in
            self#constr "Pexp_send" [ a; b ]
        | Pexp_new a ->
            let a = self#longident_loc a in
            self#constr "Pexp_new" [ a ]
        | Pexp_setinstvar (a, b) ->
            let a = self#loc self#label a in
            let b = self#expression b in
            self#constr "Pexp_setinstvar" [ a; b ]
        | Pexp_override a ->
            let a =
              self#list
                (fun (a, b) ->
                  let a = self#loc self#label a in
                  let b = self#expression b in
                  self#tuple [ a; b ])
                a
            in
            self#constr "Pexp_override" [ a ]
        | Pexp_letmodule (a, b, c) ->
            let a = self#loc (self#option self#string) a in
            let b = self#module_expr b in
            let c = self#expression c in
            self#constr "Pexp_letmodule" [ a; b; c ]
        | Pexp_letexception (a, b) ->
            let a = self#extension_constructor a in
            let b = self#expression b in
            self#constr "Pexp_letexception" [ a; b ]
        | Pexp_assert a ->
            let a = self#expression a in
            self#constr "Pexp_assert" [ a ]
        | Pexp_lazy a ->
            let a = self#expression a in
            self#constr "Pexp_lazy" [ a ]
        | Pexp_poly (a, b) ->
            let a = self#expression a in
            let b = self#option self#core_type b in
            self#constr "Pexp_poly" [ a; b ]
        | Pexp_object a ->
            let a = self#class_structure a in
            self#constr "Pexp_object" [ a ]
        | Pexp_newtype (a, b) ->
            let a = self#loc self#string a in
            let b = self#expression b in
            self#constr "Pexp_newtype" [ a; b ]
        | Pexp_pack a ->
            let a = self#module_expr a in
            self#constr "Pexp_pack" [ a ]
        | Pexp_open (a, b) ->
            let a = self#open_declaration a in
            let b = self#expression b in
            self#constr "Pexp_open" [ a; b ]
        | Pexp_letop a ->
            let a = self#letop a in
            self#constr "Pexp_letop" [ a ]
        | Pexp_extension a ->
            let a = self#extension a in
            self#constr "Pexp_extension" [ a ]
        | Pexp_unreachable -> self#constr "Pexp_unreachable" []

    method case : case -> 'res =
      fun { pc_lhs; pc_guard; pc_rhs } ->
        let pc_lhs = self#pattern pc_lhs in
        let pc_guard = self#option self#expression pc_guard in
        let pc_rhs = self#expression pc_rhs in
        self#record
          [ ("pc_lhs", pc_lhs); ("pc_guard", pc_guard); ("pc_rhs", pc_rhs) ]

    method letop : letop -> 'res =
      fun { let_; ands; body } ->
        let let_ = self#binding_op let_ in
        let ands = self#list self#binding_op ands in
        let body = self#expression body in
        self#record [ ("let_", let_); ("ands", ands); ("body", body) ]

    method binding_op : binding_op -> 'res =
      fun { pbop_op; pbop_pat; pbop_exp; pbop_loc } ->
        let pbop_op = self#loc self#string pbop_op in
        let pbop_pat = self#pattern pbop_pat in
        let pbop_exp = self#expression pbop_exp in
        let pbop_loc = self#location pbop_loc in
        self#record
          [
            ("pbop_op", pbop_op);
            ("pbop_pat", pbop_pat);
            ("pbop_exp", pbop_exp);
            ("pbop_loc", pbop_loc);
          ]

    method value_description : value_description -> 'res =
      fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } ->
        let pval_name = self#loc self#string pval_name in
        let pval_type = self#core_type pval_type in
        let pval_prim = self#list self#string pval_prim in
        let pval_attributes = self#attributes pval_attributes in
        let pval_loc = self#location pval_loc in
        self#record
          [
            ("pval_name", pval_name);
            ("pval_type", pval_type);
            ("pval_prim", pval_prim);
            ("pval_attributes", pval_attributes);
            ("pval_loc", pval_loc);
          ]

    method type_declaration : type_declaration -> 'res =
      fun {
            ptype_name;
            ptype_params;
            ptype_cstrs;
            ptype_kind;
            ptype_private;
            ptype_manifest;
            ptype_attributes;
            ptype_loc;
          } ->
        let ptype_name = self#loc self#string ptype_name in
        let ptype_params =
          self#list
            (fun (a, b) ->
              let a = self#core_type a in
              let b =
                (fun (a, b) ->
                  let a = self#variance a in
                  let b = self#injectivity b in
                  self#tuple [ a; b ])
                  b
              in
              self#tuple [ a; b ])
            ptype_params
        in
        let ptype_cstrs =
          self#list
            (fun (a, b, c) ->
              let a = self#core_type a in
              let b = self#core_type b in
              let c = self#location c in
              self#tuple [ a; b; c ])
            ptype_cstrs
        in
        let ptype_kind = self#type_kind ptype_kind in
        let ptype_private = self#private_flag ptype_private in
        let ptype_manifest = self#option self#core_type ptype_manifest in
        let ptype_attributes = self#attributes ptype_attributes in
        let ptype_loc = self#location ptype_loc in
        self#record
          [
            ("ptype_name", ptype_name);
            ("ptype_params", ptype_params);
            ("ptype_cstrs", ptype_cstrs);
            ("ptype_kind", ptype_kind);
            ("ptype_private", ptype_private);
            ("ptype_manifest", ptype_manifest);
            ("ptype_attributes", ptype_attributes);
            ("ptype_loc", ptype_loc);
          ]

    method type_kind : type_kind -> 'res =
      fun x ->
        match x with
        | Ptype_abstract -> self#constr "Ptype_abstract" []
        | Ptype_variant a ->
            let a = self#list self#constructor_declaration a in
            self#constr "Ptype_variant" [ a ]
        | Ptype_record a ->
            let a = self#list self#label_declaration a in
            self#constr "Ptype_record" [ a ]
        | Ptype_open -> self#constr "Ptype_open" []

    method label_declaration : label_declaration -> 'res =
      fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } ->
        let pld_name = self#loc self#string pld_name in
        let pld_mutable = self#mutable_flag pld_mutable in
        let pld_type = self#core_type pld_type in
        let pld_loc = self#location pld_loc in
        let pld_attributes = self#attributes pld_attributes in
        self#record
          [
            ("pld_name", pld_name);
            ("pld_mutable", pld_mutable);
            ("pld_type", pld_type);
            ("pld_loc", pld_loc);
            ("pld_attributes", pld_attributes);
          ]

    method constructor_declaration : constructor_declaration -> 'res =
      fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } ->
        let pcd_name = self#loc self#string pcd_name in
        let pcd_args = self#constructor_arguments pcd_args in
        let pcd_res = self#option self#core_type pcd_res in
        let pcd_loc = self#location pcd_loc in
        let pcd_attributes = self#attributes pcd_attributes in
        self#record
          [
            ("pcd_name", pcd_name);
            ("pcd_args", pcd_args);
            ("pcd_res", pcd_res);
            ("pcd_loc", pcd_loc);
            ("pcd_attributes", pcd_attributes);
          ]

    method constructor_arguments : constructor_arguments -> 'res =
      fun x ->
        match x with
        | Pcstr_tuple a ->
            let a = self#list self#core_type a in
            self#constr "Pcstr_tuple" [ a ]
        | Pcstr_record a ->
            let a = self#list self#label_declaration a in
            self#constr "Pcstr_record" [ a ]

    method type_extension : type_extension -> 'res =
      fun {
            ptyext_path;
            ptyext_params;
            ptyext_constructors;
            ptyext_private;
            ptyext_loc;
            ptyext_attributes;
          } ->
        let ptyext_path = self#longident_loc ptyext_path in
        let ptyext_params =
          self#list
            (fun (a, b) ->
              let a = self#core_type a in
              let b =
                (fun (a, b) ->
                  let a = self#variance a in
                  let b = self#injectivity b in
                  self#tuple [ a; b ])
                  b
              in
              self#tuple [ a; b ])
            ptyext_params
        in
        let ptyext_constructors =
          self#list self#extension_constructor ptyext_constructors
        in
        let ptyext_private = self#private_flag ptyext_private in
        let ptyext_loc = self#location ptyext_loc in
        let ptyext_attributes = self#attributes ptyext_attributes in
        self#record
          [
            ("ptyext_path", ptyext_path);
            ("ptyext_params", ptyext_params);
            ("ptyext_constructors", ptyext_constructors);
            ("ptyext_private", ptyext_private);
            ("ptyext_loc", ptyext_loc);
            ("ptyext_attributes", ptyext_attributes);
          ]

    method extension_constructor : extension_constructor -> 'res =
      fun { pext_name; pext_kind; pext_loc; pext_attributes } ->
        let pext_name = self#loc self#string pext_name in
        let pext_kind = self#extension_constructor_kind pext_kind in
        let pext_loc = self#location pext_loc in
        let pext_attributes = self#attributes pext_attributes in
        self#record
          [
            ("pext_name", pext_name);
            ("pext_kind", pext_kind);
            ("pext_loc", pext_loc);
            ("pext_attributes", pext_attributes);
          ]

    method type_exception : type_exception -> 'res =
      fun { ptyexn_constructor; ptyexn_loc; ptyexn_attributes } ->
        let ptyexn_constructor =
          self#extension_constructor ptyexn_constructor
        in
        let ptyexn_loc = self#location ptyexn_loc in
        let ptyexn_attributes = self#attributes ptyexn_attributes in
        self#record
          [
            ("ptyexn_constructor", ptyexn_constructor);
            ("ptyexn_loc", ptyexn_loc);
            ("ptyexn_attributes", ptyexn_attributes);
          ]

    method extension_constructor_kind : extension_constructor_kind -> 'res =
      fun x ->
        match x with
        | Pext_decl (a, b) ->
            let a = self#constructor_arguments a in
            let b = self#option self#core_type b in
            self#constr "Pext_decl" [ a; b ]
        | Pext_rebind a ->
            let a = self#longident_loc a in
            self#constr "Pext_rebind" [ a ]

    method class_type : class_type -> 'res =
      fun { pcty_desc; pcty_loc; pcty_attributes } ->
        let pcty_desc = self#class_type_desc pcty_desc in
        let pcty_loc = self#location pcty_loc in
        let pcty_attributes = self#attributes pcty_attributes in
        self#record
          [
            ("pcty_desc", pcty_desc);
            ("pcty_loc", pcty_loc);
            ("pcty_attributes", pcty_attributes);
          ]

    method class_type_desc : class_type_desc -> 'res =
      fun x ->
        match x with
        | Pcty_constr (a, b) ->
            let a = self#longident_loc a in
            let b = self#list self#core_type b in
            self#constr "Pcty_constr" [ a; b ]
        | Pcty_signature a ->
            let a = self#class_signature a in
            self#constr "Pcty_signature" [ a ]
        | Pcty_arrow (a, b, c) ->
            let a = self#arg_label a in
            let b = self#core_type b in
            let c = self#class_type c in
            self#constr "Pcty_arrow" [ a; b; c ]
        | Pcty_extension a ->
            let a = self#extension a in
            self#constr "Pcty_extension" [ a ]
        | Pcty_open (a, b) ->
            let a = self#open_description a in
            let b = self#class_type b in
            self#constr "Pcty_open" [ a; b ]

    method class_signature : class_signature -> 'res =
      fun { pcsig_self; pcsig_fields } ->
        let pcsig_self = self#core_type pcsig_self in
        let pcsig_fields = self#list self#class_type_field pcsig_fields in
        self#record
          [ ("pcsig_self", pcsig_self); ("pcsig_fields", pcsig_fields) ]

    method class_type_field : class_type_field -> 'res =
      fun { pctf_desc; pctf_loc; pctf_attributes } ->
        let pctf_desc = self#class_type_field_desc pctf_desc in
        let pctf_loc = self#location pctf_loc in
        let pctf_attributes = self#attributes pctf_attributes in
        self#record
          [
            ("pctf_desc", pctf_desc);
            ("pctf_loc", pctf_loc);
            ("pctf_attributes", pctf_attributes);
          ]

    method class_type_field_desc : class_type_field_desc -> 'res =
      fun x ->
        match x with
        | Pctf_inherit a ->
            let a = self#class_type a in
            self#constr "Pctf_inherit" [ a ]
        | Pctf_val a ->
            let a =
              (fun (a, b, c, d) ->
                let a = self#loc self#label a in
                let b = self#mutable_flag b in
                let c = self#virtual_flag c in
                let d = self#core_type d in
                self#tuple [ a; b; c; d ])
                a
            in
            self#constr "Pctf_val" [ a ]
        | Pctf_method a ->
            let a =
              (fun (a, b, c, d) ->
                let a = self#loc self#label a in
                let b = self#private_flag b in
                let c = self#virtual_flag c in
                let d = self#core_type d in
                self#tuple [ a; b; c; d ])
                a
            in
            self#constr "Pctf_method" [ a ]
        | Pctf_constraint a ->
            let a =
              (fun (a, b) ->
                let a = self#core_type a in
                let b = self#core_type b in
                self#tuple [ a; b ])
                a
            in
            self#constr "Pctf_constraint" [ a ]
        | Pctf_attribute a ->
            let a = self#attribute a in
            self#constr "Pctf_attribute" [ a ]
        | Pctf_extension a ->
            let a = self#extension a in
            self#constr "Pctf_extension" [ a ]

    method class_infos : 'a. ('a -> 'res) -> 'a class_infos -> 'res =
      fun _a
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
        let pci_virt = self#virtual_flag pci_virt in
        let pci_params =
          self#list
            (fun (a, b) ->
              let a = self#core_type a in
              let b =
                (fun (a, b) ->
                  let a = self#variance a in
                  let b = self#injectivity b in
                  self#tuple [ a; b ])
                  b
              in
              self#tuple [ a; b ])
            pci_params
        in
        let pci_name = self#loc self#string pci_name in
        let pci_expr = _a pci_expr in
        let pci_loc = self#location pci_loc in
        let pci_attributes = self#attributes pci_attributes in
        self#record
          [
            ("pci_virt", pci_virt);
            ("pci_params", pci_params);
            ("pci_name", pci_name);
            ("pci_expr", pci_expr);
            ("pci_loc", pci_loc);
            ("pci_attributes", pci_attributes);
          ]

    method class_description : class_description -> 'res =
      self#class_infos self#class_type

    method class_type_declaration : class_type_declaration -> 'res =
      self#class_infos self#class_type

    method class_expr : class_expr -> 'res =
      fun { pcl_desc; pcl_loc; pcl_attributes } ->
        let pcl_desc = self#class_expr_desc pcl_desc in
        let pcl_loc = self#location pcl_loc in
        let pcl_attributes = self#attributes pcl_attributes in
        self#record
          [
            ("pcl_desc", pcl_desc);
            ("pcl_loc", pcl_loc);
            ("pcl_attributes", pcl_attributes);
          ]

    method class_expr_desc : class_expr_desc -> 'res =
      fun x ->
        match x with
        | Pcl_constr (a, b) ->
            let a = self#longident_loc a in
            let b = self#list self#core_type b in
            self#constr "Pcl_constr" [ a; b ]
        | Pcl_structure a ->
            let a = self#class_structure a in
            self#constr "Pcl_structure" [ a ]
        | Pcl_fun (a, b, c, d) ->
            let a = self#arg_label a in
            let b = self#option self#expression b in
            let c = self#pattern c in
            let d = self#class_expr d in
            self#constr "Pcl_fun" [ a; b; c; d ]
        | Pcl_apply (a, b) ->
            let a = self#class_expr a in
            let b =
              self#list
                (fun (a, b) ->
                  let a = self#arg_label a in
                  let b = self#expression b in
                  self#tuple [ a; b ])
                b
            in
            self#constr "Pcl_apply" [ a; b ]
        | Pcl_let (a, b, c) ->
            let a = self#rec_flag a in
            let b = self#list self#value_binding b in
            let c = self#class_expr c in
            self#constr "Pcl_let" [ a; b; c ]
        | Pcl_constraint (a, b) ->
            let a = self#class_expr a in
            let b = self#class_type b in
            self#constr "Pcl_constraint" [ a; b ]
        | Pcl_extension a ->
            let a = self#extension a in
            self#constr "Pcl_extension" [ a ]
        | Pcl_open (a, b) ->
            let a = self#open_description a in
            let b = self#class_expr b in
            self#constr "Pcl_open" [ a; b ]

    method class_structure : class_structure -> 'res =
      fun { pcstr_self; pcstr_fields } ->
        let pcstr_self = self#pattern pcstr_self in
        let pcstr_fields = self#list self#class_field pcstr_fields in
        self#record
          [ ("pcstr_self", pcstr_self); ("pcstr_fields", pcstr_fields) ]

    method class_field : class_field -> 'res =
      fun { pcf_desc; pcf_loc; pcf_attributes } ->
        let pcf_desc = self#class_field_desc pcf_desc in
        let pcf_loc = self#location pcf_loc in
        let pcf_attributes = self#attributes pcf_attributes in
        self#record
          [
            ("pcf_desc", pcf_desc);
            ("pcf_loc", pcf_loc);
            ("pcf_attributes", pcf_attributes);
          ]

    method class_field_desc : class_field_desc -> 'res =
      fun x ->
        match x with
        | Pcf_inherit (a, b, c) ->
            let a = self#override_flag a in
            let b = self#class_expr b in
            let c = self#option (self#loc self#string) c in
            self#constr "Pcf_inherit" [ a; b; c ]
        | Pcf_val a ->
            let a =
              (fun (a, b, c) ->
                let a = self#loc self#label a in
                let b = self#mutable_flag b in
                let c = self#class_field_kind c in
                self#tuple [ a; b; c ])
                a
            in
            self#constr "Pcf_val" [ a ]
        | Pcf_method a ->
            let a =
              (fun (a, b, c) ->
                let a = self#loc self#label a in
                let b = self#private_flag b in
                let c = self#class_field_kind c in
                self#tuple [ a; b; c ])
                a
            in
            self#constr "Pcf_method" [ a ]
        | Pcf_constraint a ->
            let a =
              (fun (a, b) ->
                let a = self#core_type a in
                let b = self#core_type b in
                self#tuple [ a; b ])
                a
            in
            self#constr "Pcf_constraint" [ a ]
        | Pcf_initializer a ->
            let a = self#expression a in
            self#constr "Pcf_initializer" [ a ]
        | Pcf_attribute a ->
            let a = self#attribute a in
            self#constr "Pcf_attribute" [ a ]
        | Pcf_extension a ->
            let a = self#extension a in
            self#constr "Pcf_extension" [ a ]

    method class_field_kind : class_field_kind -> 'res =
      fun x ->
        match x with
        | Cfk_virtual a ->
            let a = self#core_type a in
            self#constr "Cfk_virtual" [ a ]
        | Cfk_concrete (a, b) ->
            let a = self#override_flag a in
            let b = self#expression b in
            self#constr "Cfk_concrete" [ a; b ]

    method class_declaration : class_declaration -> 'res =
      self#class_infos self#class_expr

    method module_type : module_type -> 'res =
      fun { pmty_desc; pmty_loc; pmty_attributes } ->
        let pmty_desc = self#module_type_desc pmty_desc in
        let pmty_loc = self#location pmty_loc in
        let pmty_attributes = self#attributes pmty_attributes in
        self#record
          [
            ("pmty_desc", pmty_desc);
            ("pmty_loc", pmty_loc);
            ("pmty_attributes", pmty_attributes);
          ]

    method module_type_desc : module_type_desc -> 'res =
      fun x ->
        match x with
        | Pmty_ident a ->
            let a = self#longident_loc a in
            self#constr "Pmty_ident" [ a ]
        | Pmty_signature a ->
            let a = self#signature a in
            self#constr "Pmty_signature" [ a ]
        | Pmty_functor (a, b) ->
            let a = self#functor_parameter a in
            let b = self#module_type b in
            self#constr "Pmty_functor" [ a; b ]
        | Pmty_with (a, b) ->
            let a = self#module_type a in
            let b = self#list self#with_constraint b in
            self#constr "Pmty_with" [ a; b ]
        | Pmty_typeof a ->
            let a = self#module_expr a in
            self#constr "Pmty_typeof" [ a ]
        | Pmty_extension a ->
            let a = self#extension a in
            self#constr "Pmty_extension" [ a ]
        | Pmty_alias a ->
            let a = self#longident_loc a in
            self#constr "Pmty_alias" [ a ]

    method functor_parameter : functor_parameter -> 'res =
      fun x ->
        match x with
        | Unit -> self#constr "Unit" []
        | Named (a, b) ->
            let a = self#loc (self#option self#string) a in
            let b = self#module_type b in
            self#constr "Named" [ a; b ]

    method signature : signature -> 'res = self#list self#signature_item

    method signature_item : signature_item -> 'res =
      fun { psig_desc; psig_loc } ->
        let psig_desc = self#signature_item_desc psig_desc in
        let psig_loc = self#location psig_loc in
        self#record [ ("psig_desc", psig_desc); ("psig_loc", psig_loc) ]

    method signature_item_desc : signature_item_desc -> 'res =
      fun x ->
        match x with
        | Psig_value a ->
            let a = self#value_description a in
            self#constr "Psig_value" [ a ]
        | Psig_type (a, b) ->
            let a = self#rec_flag a in
            let b = self#list self#type_declaration b in
            self#constr "Psig_type" [ a; b ]
        | Psig_typesubst a ->
            let a = self#list self#type_declaration a in
            self#constr "Psig_typesubst" [ a ]
        | Psig_typext a ->
            let a = self#type_extension a in
            self#constr "Psig_typext" [ a ]
        | Psig_exception a ->
            let a = self#type_exception a in
            self#constr "Psig_exception" [ a ]
        | Psig_module a ->
            let a = self#module_declaration a in
            self#constr "Psig_module" [ a ]
        | Psig_modsubst a ->
            let a = self#module_substitution a in
            self#constr "Psig_modsubst" [ a ]
        | Psig_recmodule a ->
            let a = self#list self#module_declaration a in
            self#constr "Psig_recmodule" [ a ]
        | Psig_modtype a ->
            let a = self#module_type_declaration a in
            self#constr "Psig_modtype" [ a ]
        | Psig_open a ->
            let a = self#open_description a in
            self#constr "Psig_open" [ a ]
        | Psig_include a ->
            let a = self#include_description a in
            self#constr "Psig_include" [ a ]
        | Psig_class a ->
            let a = self#list self#class_description a in
            self#constr "Psig_class" [ a ]
        | Psig_class_type a ->
            let a = self#list self#class_type_declaration a in
            self#constr "Psig_class_type" [ a ]
        | Psig_attribute a ->
            let a = self#attribute a in
            self#constr "Psig_attribute" [ a ]
        | Psig_extension (a, b) ->
            let a = self#extension a in
            let b = self#attributes b in
            self#constr "Psig_extension" [ a; b ]

    method module_declaration : module_declaration -> 'res =
      fun { pmd_name; pmd_type; pmd_attributes; pmd_loc } ->
        let pmd_name = self#loc (self#option self#string) pmd_name in
        let pmd_type = self#module_type pmd_type in
        let pmd_attributes = self#attributes pmd_attributes in
        let pmd_loc = self#location pmd_loc in
        self#record
          [
            ("pmd_name", pmd_name);
            ("pmd_type", pmd_type);
            ("pmd_attributes", pmd_attributes);
            ("pmd_loc", pmd_loc);
          ]

    method module_substitution : module_substitution -> 'res =
      fun { pms_name; pms_manifest; pms_attributes; pms_loc } ->
        let pms_name = self#loc self#string pms_name in
        let pms_manifest = self#longident_loc pms_manifest in
        let pms_attributes = self#attributes pms_attributes in
        let pms_loc = self#location pms_loc in
        self#record
          [
            ("pms_name", pms_name);
            ("pms_manifest", pms_manifest);
            ("pms_attributes", pms_attributes);
            ("pms_loc", pms_loc);
          ]

    method module_type_declaration : module_type_declaration -> 'res =
      fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } ->
        let pmtd_name = self#loc self#string pmtd_name in
        let pmtd_type = self#option self#module_type pmtd_type in
        let pmtd_attributes = self#attributes pmtd_attributes in
        let pmtd_loc = self#location pmtd_loc in
        self#record
          [
            ("pmtd_name", pmtd_name);
            ("pmtd_type", pmtd_type);
            ("pmtd_attributes", pmtd_attributes);
            ("pmtd_loc", pmtd_loc);
          ]

    method open_infos : 'a. ('a -> 'res) -> 'a open_infos -> 'res =
      fun _a { popen_expr; popen_override; popen_loc; popen_attributes } ->
        let popen_expr = _a popen_expr in
        let popen_override = self#override_flag popen_override in
        let popen_loc = self#location popen_loc in
        let popen_attributes = self#attributes popen_attributes in
        self#record
          [
            ("popen_expr", popen_expr);
            ("popen_override", popen_override);
            ("popen_loc", popen_loc);
            ("popen_attributes", popen_attributes);
          ]

    method open_description : open_description -> 'res =
      self#open_infos self#longident_loc

    method open_declaration : open_declaration -> 'res =
      self#open_infos self#module_expr

    method include_infos : 'a. ('a -> 'res) -> 'a include_infos -> 'res =
      fun _a { pincl_mod; pincl_loc; pincl_attributes } ->
        let pincl_mod = _a pincl_mod in
        let pincl_loc = self#location pincl_loc in
        let pincl_attributes = self#attributes pincl_attributes in
        self#record
          [
            ("pincl_mod", pincl_mod);
            ("pincl_loc", pincl_loc);
            ("pincl_attributes", pincl_attributes);
          ]

    method include_description : include_description -> 'res =
      self#include_infos self#module_type

    method include_declaration : include_declaration -> 'res =
      self#include_infos self#module_expr

    method with_constraint : with_constraint -> 'res =
      fun x ->
        match x with
        | Pwith_type (a, b) ->
            let a = self#longident_loc a in
            let b = self#type_declaration b in
            self#constr "Pwith_type" [ a; b ]
        | Pwith_module (a, b) ->
            let a = self#longident_loc a in
            let b = self#longident_loc b in
            self#constr "Pwith_module" [ a; b ]
        | Pwith_typesubst (a, b) ->
            let a = self#longident_loc a in
            let b = self#type_declaration b in
            self#constr "Pwith_typesubst" [ a; b ]
        | Pwith_modsubst (a, b) ->
            let a = self#longident_loc a in
            let b = self#longident_loc b in
            self#constr "Pwith_modsubst" [ a; b ]

    method module_expr : module_expr -> 'res =
      fun { pmod_desc; pmod_loc; pmod_attributes } ->
        let pmod_desc = self#module_expr_desc pmod_desc in
        let pmod_loc = self#location pmod_loc in
        let pmod_attributes = self#attributes pmod_attributes in
        self#record
          [
            ("pmod_desc", pmod_desc);
            ("pmod_loc", pmod_loc);
            ("pmod_attributes", pmod_attributes);
          ]

    method module_expr_desc : module_expr_desc -> 'res =
      fun x ->
        match x with
        | Pmod_ident a ->
            let a = self#longident_loc a in
            self#constr "Pmod_ident" [ a ]
        | Pmod_structure a ->
            let a = self#structure a in
            self#constr "Pmod_structure" [ a ]
        | Pmod_functor (a, b) ->
            let a = self#functor_parameter a in
            let b = self#module_expr b in
            self#constr "Pmod_functor" [ a; b ]
        | Pmod_apply (a, b) ->
            let a = self#module_expr a in
            let b = self#module_expr b in
            self#constr "Pmod_apply" [ a; b ]
        | Pmod_constraint (a, b) ->
            let a = self#module_expr a in
            let b = self#module_type b in
            self#constr "Pmod_constraint" [ a; b ]
        | Pmod_unpack a ->
            let a = self#expression a in
            self#constr "Pmod_unpack" [ a ]
        | Pmod_extension a ->
            let a = self#extension a in
            self#constr "Pmod_extension" [ a ]

    method structure : structure -> 'res = self#list self#structure_item

    method structure_item : structure_item -> 'res =
      fun { pstr_desc; pstr_loc } ->
        let pstr_desc = self#structure_item_desc pstr_desc in
        let pstr_loc = self#location pstr_loc in
        self#record [ ("pstr_desc", pstr_desc); ("pstr_loc", pstr_loc) ]

    method structure_item_desc : structure_item_desc -> 'res =
      fun x ->
        match x with
        | Pstr_eval (a, b) ->
            let a = self#expression a in
            let b = self#attributes b in
            self#constr "Pstr_eval" [ a; b ]
        | Pstr_value (a, b) ->
            let a = self#rec_flag a in
            let b = self#list self#value_binding b in
            self#constr "Pstr_value" [ a; b ]
        | Pstr_primitive a ->
            let a = self#value_description a in
            self#constr "Pstr_primitive" [ a ]
        | Pstr_type (a, b) ->
            let a = self#rec_flag a in
            let b = self#list self#type_declaration b in
            self#constr "Pstr_type" [ a; b ]
        | Pstr_typext a ->
            let a = self#type_extension a in
            self#constr "Pstr_typext" [ a ]
        | Pstr_exception a ->
            let a = self#type_exception a in
            self#constr "Pstr_exception" [ a ]
        | Pstr_module a ->
            let a = self#module_binding a in
            self#constr "Pstr_module" [ a ]
        | Pstr_recmodule a ->
            let a = self#list self#module_binding a in
            self#constr "Pstr_recmodule" [ a ]
        | Pstr_modtype a ->
            let a = self#module_type_declaration a in
            self#constr "Pstr_modtype" [ a ]
        | Pstr_open a ->
            let a = self#open_declaration a in
            self#constr "Pstr_open" [ a ]
        | Pstr_class a ->
            let a = self#list self#class_declaration a in
            self#constr "Pstr_class" [ a ]
        | Pstr_class_type a ->
            let a = self#list self#class_type_declaration a in
            self#constr "Pstr_class_type" [ a ]
        | Pstr_include a ->
            let a = self#include_declaration a in
            self#constr "Pstr_include" [ a ]
        | Pstr_attribute a ->
            let a = self#attribute a in
            self#constr "Pstr_attribute" [ a ]
        | Pstr_extension (a, b) ->
            let a = self#extension a in
            let b = self#attributes b in
            self#constr "Pstr_extension" [ a; b ]

    method value_binding : value_binding -> 'res =
      fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } ->
        let pvb_pat = self#pattern pvb_pat in
        let pvb_expr = self#expression pvb_expr in
        let pvb_attributes = self#attributes pvb_attributes in
        let pvb_loc = self#location pvb_loc in
        self#record
          [
            ("pvb_pat", pvb_pat);
            ("pvb_expr", pvb_expr);
            ("pvb_attributes", pvb_attributes);
            ("pvb_loc", pvb_loc);
          ]

    method module_binding : module_binding -> 'res =
      fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc } ->
        let pmb_name = self#loc (self#option self#string) pmb_name in
        let pmb_expr = self#module_expr pmb_expr in
        let pmb_attributes = self#attributes pmb_attributes in
        let pmb_loc = self#location pmb_loc in
        self#record
          [
            ("pmb_name", pmb_name);
            ("pmb_expr", pmb_expr);
            ("pmb_attributes", pmb_attributes);
            ("pmb_loc", pmb_loc);
          ]

    method toplevel_phrase : toplevel_phrase -> 'res =
      fun x ->
        match x with
        | Ptop_def a ->
            let a = self#structure a in
            self#constr "Ptop_def" [ a ]
        | Ptop_dir a ->
            let a = self#toplevel_directive a in
            self#constr "Ptop_dir" [ a ]

    method toplevel_directive : toplevel_directive -> 'res =
      fun { pdir_name; pdir_arg; pdir_loc } ->
        let pdir_name = self#loc self#string pdir_name in
        let pdir_arg = self#option self#directive_argument pdir_arg in
        let pdir_loc = self#location pdir_loc in
        self#record
          [
            ("pdir_name", pdir_name);
            ("pdir_arg", pdir_arg);
            ("pdir_loc", pdir_loc);
          ]

    method directive_argument : directive_argument -> 'res =
      fun { pdira_desc; pdira_loc } ->
        let pdira_desc = self#directive_argument_desc pdira_desc in
        let pdira_loc = self#location pdira_loc in
        self#record [ ("pdira_desc", pdira_desc); ("pdira_loc", pdira_loc) ]

    method directive_argument_desc : directive_argument_desc -> 'res =
      fun x ->
        match x with
        | Pdir_string a ->
            let a = self#string a in
            self#constr "Pdir_string" [ a ]
        | Pdir_int (a, b) ->
            let a = self#string a in
            let b = self#option self#char b in
            self#constr "Pdir_int" [ a; b ]
        | Pdir_ident a ->
            let a = self#longident a in
            self#constr "Pdir_ident" [ a ]
        | Pdir_bool a ->
            let a = self#bool a in
            self#constr "Pdir_bool" [ a ]

    method cases : cases -> 'res = self#list self#case
  end

[@@@end]
