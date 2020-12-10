(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Paths

(** {3 Modules} *)

module rec Module : sig

  type expansion =
    | AlreadyASig
    | Signature of Signature.t
    | Functor of FunctorParameter.t list * Signature.t

  type decl =
    | Alias of Path.Module.t
    | ModuleType of ModuleType.expr

  type t =
    { id: Identifier.Module.t;
      doc: Comment.docs;
      type_: decl;
      canonical : (Path.Module.t * Reference.Module.t) option;
      hidden : bool;
      display_type : decl option;
      expansion: expansion option;
    }

  module Equation : sig

    type t = decl

  end

end = Module

and FunctorParameter : sig
  type parameter = {
    id : Identifier.Module.t;
    expr : ModuleType.expr;
    expansion: Module.expansion option;
  }

  type t = 
    | Unit
    | Named of parameter
end = FunctorParameter

(** {3 Modules Types} *)

and ModuleType : sig

  type substitution =
    | ModuleEq of Fragment.Module.t * Module.Equation.t
    | TypeEq of Fragment.Type.t * TypeDecl.Equation.t
    | ModuleSubst of Fragment.Module.t * Path.Module.t
    | TypeSubst of Fragment.Type.t * TypeDecl.Equation.t

  type expr =
    | Path of Path.ModuleType.t
    | Signature of Signature.t
    | Functor of FunctorParameter.t * expr
    | With of expr * substitution list
    | TypeOf of Module.decl

  type t =
    { id: Identifier.ModuleType.t;
      doc: Comment.docs;
      expr: expr option;
      expansion: Module.expansion option;
    }

end = ModuleType

and ModuleSubstitution : sig
  type t =
    { id: Identifier.Module.t
    ; doc: Comment.docs
    ; manifest: Path.Module.t
    ; }
end = ModuleSubstitution

(** {3 Signatures} *)

and Signature : sig

  type recursive =
    | Ordinary
    | And
    | Nonrec
    | Rec

  type item =
    | Module of recursive * Module.t
    | ModuleType of ModuleType.t
    | ModuleSubstitution of ModuleSubstitution.t
    | Type of recursive * TypeDecl.t
    | TypeSubstitution of TypeDecl.t
    | TypExt of Extension.t
    | Exception of Exception.t
    | Value of Value.t
    | External of External.t
    | Class of recursive * Class.t
    | ClassType of recursive * ClassType.t
    | Include of Include.t
    | Comment of Comment.docs_or_stop

  type t = item list

end = Signature

(** {3 Includes} *)

and Include : sig
  type expansion = {
    resolved: bool;
    content: Signature.t;
  }

  type t =
    { parent: Identifier.Signature.t;
      doc: Comment.docs;
      decl: Module.decl;
      expansion: expansion; }

end = Include

(** {3 Type Declarations} *)

and TypeDecl : sig

  module Field : sig

    type t =
      { id: Identifier.Field.t;
        doc: Comment.docs;
        mutable_ : bool;
        type_: TypeExpr.t; }

  end

  module Constructor : sig
    type argument =
      | Tuple of TypeExpr.t list
      | Record of Field.t list

    type t =
      { id: Identifier.Constructor.t;
        doc: Comment.docs;
        args: argument;
        res: TypeExpr.t option; }

  end


  module Representation : sig

    type t =
      | Variant of Constructor.t list
      | Record of Field.t list
      | Extensible

  end

  type variance =
    | Pos
    | Neg

  type param_desc =
    | Any
    | Var of string

  type param = param_desc * variance option

  module Equation : sig

    type t =
      { params: param list;
        private_: bool;
        manifest: TypeExpr.t option;
        constraints: (TypeExpr.t * TypeExpr.t) list; }

  end

  type t =
    { id: Identifier.Type.t;
      doc: Comment.docs;
      equation: Equation.t;
      representation: Representation.t option; }

end = TypeDecl

(** {3 Type extensions} *)

and Extension : sig

  module Constructor : sig

    type t =
      { id: Identifier.Extension.t;
        doc: Comment.docs;
        args: TypeDecl.Constructor.argument;
        res: TypeExpr.t option; }

  end

  type t =
    { type_path: Path.Type.t;
      doc: Comment.docs;
      type_params: TypeDecl.param list;
      private_: bool;
      constructors: Constructor.t list; }

end = Extension

(** {3 Exception} *)
and Exception : sig

  type t =
    { id: Identifier.Exception.t;
      doc: Comment.docs;
      args: TypeDecl.Constructor.argument;
      res: TypeExpr.t option; }

end = Exception


(** {3 Values} *)

and Value : sig

  type t =
    { id: Identifier.Value.t;
      doc: Comment.docs;
      type_: TypeExpr.t; }

end = Value

(** {3 External values} *)

and External : sig

  type t =
    { id: Identifier.Value.t;
      doc: Comment.docs;
      type_: TypeExpr.t;
      primitives: string list; }

end = External

(** {3 Classes} *)

and Class : sig

  type decl =
    | ClassType of ClassType.expr
    | Arrow of TypeExpr.label option * TypeExpr.t * decl

  type t =
    { id: Identifier.Class.t;
      doc: Comment.docs;
      virtual_: bool;
      params: TypeDecl.param list;
      type_: decl;
      expansion: ClassSignature.t option; }

end = Class

(** {3 Class Types} *)

and ClassType : sig

  type expr =
    | Constr of Path.ClassType.t * TypeExpr.t list
    | Signature of ClassSignature.t

  type t =
    { id: Identifier.ClassType.t;
      doc: Comment.docs;
      virtual_: bool;
      params: TypeDecl.param list;
      expr: expr;
      expansion: ClassSignature.t option; }

end = ClassType

(** {3 Class Signatures} *)

and ClassSignature : sig

  type item =
    | Method of Method.t
    | InstanceVariable of InstanceVariable.t
    | Constraint of TypeExpr.t * TypeExpr.t
    | Inherit of ClassType.expr
    | Comment of Comment.docs_or_stop

  type t =
    { self: TypeExpr.t option;
      items: item list; }

end = ClassSignature

(** {3 Methods} *)

and Method : sig

  type t =
    { id: Identifier.Method.t;
      doc: Comment.docs;
      private_: bool;
      virtual_: bool;
      type_: TypeExpr.t; }

end = Method

(** {3 Instance variables} *)

and InstanceVariable : sig

  type t =
    { id: Identifier.InstanceVariable.t;
      doc: Comment.docs;
      mutable_: bool;
      virtual_: bool;
      type_: TypeExpr.t; }

end = InstanceVariable

(** {3 Type expressions} *)

and TypeExpr : sig

  module Polymorphic_variant : sig

    type kind =
      | Fixed
      | Closed of string list
      | Open

    module Constructor :
    sig
      type t = {
        name : string;
        constant : bool;
        arguments : TypeExpr.t list;
        doc : Comment.docs;
      }
    end

    type element =
      | Type of TypeExpr.t
      | Constructor of Constructor.t

    type t =
      { kind: kind;
        elements: element list;}

  end

  module Object : sig

    type method_ =
      { name: string;
        type_: TypeExpr.t; }

    type field =
      | Method of method_
      | Inherit of TypeExpr.t

    type t =
      { fields: field list;
        open_ : bool; }

  end

  module Package : sig

    type substitution = Fragment.Type.t * TypeExpr.t

    type t =
      { path: Path.ModuleType.t;
        substitutions: substitution list; }

  end

  type label =
    | Label of string
    | Optional of string

  type t =
    | Var of string
    | Any
    | Alias of t * string
    | Arrow of label option * t * t
    | Tuple of t list
    | Constr of Path.Type.t * t list
    | Polymorphic_variant of TypeExpr.Polymorphic_variant.t
    | Object of TypeExpr.Object.t
    | Class of Path.ClassType.t * t list
    | Poly of string list * t
    | Package of TypeExpr.Package.t

end = TypeExpr

(** {3 Compilation units} *)

module rec Compilation_unit : sig

  module Import : sig

    type t =
      | Unresolved of string * Digest.t option
      | Resolved of Root.t

  end

  module Source : sig

    type t =
      { file: string;
        build_dir: string;
        digest: Digest.t; }

  end

  module Packed : sig

    type item =
      { id: Identifier.Module.t;
        path: Path.Module.t; }

    type t = item list

  end

  type content =
    | Module of Signature.t
    | Pack of Packed.t

  type t =
    { id: Identifier.Module.t;
      doc: Comment.docs;
      digest: Digest.t;
      imports: Import.t list;
      source: Source.t option;
      interface: bool;
      hidden: bool;
      content: content;
      expansion: Signature.t option; }

end = Compilation_unit

module rec Page : sig
  type t =
    { name: Identifier.Page.t;
      content: Comment.docs;
      digest: Digest.t; }
end = Page
