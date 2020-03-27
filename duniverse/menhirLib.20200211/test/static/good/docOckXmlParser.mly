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

%parameter< Root : sig type t end >

%{

open DocOckPaths
open DocOckTypes

let relax_class_path cl =
  match (cl : ('a, Kind.identifier_class) Path.Resolved.t) with
  | Path.Resolved.Identifier (Identifier.Class _)
  | Path.Resolved.Class _ as cl -> cl

let relax_class_type_path cltyp =
  match (cltyp : 'a Path.Resolved.class_type) with
  | Path.Resolved.Identifier (Identifier.Class _ | Identifier.ClassType _)
  | Path.Resolved.Class _
  | Path.Resolved.ClassType _ as cltyp -> cltyp

let relax_datatype_reference typ =
  match (typ : 'a Reference.Resolved.datatype) with
  | Reference.Resolved.Identifier (Identifier.Type _ | Identifier.CoreType _)
  | Reference.Resolved.Type _ as typ -> typ

let relax_extension_reference ext =
  match (ext : 'a Reference.Resolved.extension) with
  | Reference.Resolved.Identifier
      (Identifier.Exception _ | Identifier.CoreException _
      | Identifier.Extension _)
  | Reference.Resolved.Exception _
  | Reference.Resolved.Extension _ as ext -> ext

let relax_exception_reference exn =
  match (exn : 'a Reference.Resolved.exception_) with
  | Reference.Resolved.Identifier
      (Identifier.Exception _ | Identifier.CoreException _)
  | Reference.Resolved.Exception _ as exn -> exn

let relax_class_reference cl =
  match (cl : 'a Reference.Resolved.class_) with
  | Reference.Resolved.Identifier (Identifier.Class _)
  | Reference.Resolved.Class _ as cl -> cl

let relax_class_type_reference cltyp =
  match (cltyp : 'a Reference.Resolved.class_type) with
  | Reference.Resolved.Identifier
      (Identifier.Class _ | Identifier.ClassType _)
  | Reference.Resolved.Class _
  | Reference.Resolved.ClassType _ as cltyp -> cltyp

%}

%token ALIAS
%token ANY
%token APPLY
%token ARGUMENTS
%token ARROW
%token AUTHOR
%token BEFORE
%token BOLD
%token CENTER
%token CLASS
%token CLASS_TYPE
%token CLOSED
%token CODE
%token COLUMN
%token COMMENT
%token CONSTANT
%token CONSTRAINT
%token CONSTRUCTOR
%token DEPRECATED
%token DIGEST
%token DIR
%token DOC
%token DOT
%token ELEMENT
%token EMPHASIZE
%token ENUM
%token ERROR
%token EXCEPTION
%token EXTENSIBLE
%token EXTENSION
%token EXTERNAL
%token FIELD
%token FILE
%token FILENAME
%token FIXED
%token FUNCTOR
%token IDENTIFIER
%token IMPORT
%token INCLUDE
%token INDEX
%token INHERIT
%token INSTANCE_VARIABLE
%token INTERFACE
%token ITALIC
%token ITEM
%token LABEL
%token LEFT
%token LINE
%token LINK
%token LIST
%token LOCATION
%token METHOD
%token MODULE
%token MODULES
%token MODULE_SUBST
%token MODULE_TYPE
%token MUTABLE
%token NAME
%token NEG
%token NEWLINE
%token OBJECT
%token OFFSET
%token OPEN
%token OPTIONAL
%token PACK
%token PACKAGE
%token PARAM
%token PATH
%token POLY
%token POLY_VARIANT
%token POS
%token POSITION
%token PRECODE
%token PRIMITIVE
%token PRIVATE
%token RAISE
%token RECORD
%token REFERENCE
%token RESOLVED
%token RESULT
%token RETURN
%token RIGHT
%token ROOT
%token SECTION
%token SEE
%token SIGNATURE
%token SINCE
%token SOURCE
%token SPECIAL
%token STOP
%token SUBSCRIPT
%token SUBST
%token SUBST_ALIAS
%token SUPERSCRIPT
%token TAG
%token TUPLE
%token TYPE
%token TYPEOF
%token TYPE_SUBST
%token UNIT
%token URL
%token VALUE
%token VAR
%token VARIANT
%token VERBATIM
%token VERSION
%token VIRTUAL
%token WITH


%token <int option> Argument
%token <string> Custom
%token <string option> Target
%token <int> Title


%token DTD
%token CLOSE
%token <string> Data
%token <Root.t> Base
%token EOF

%start <Root.t DocOckTypes.Unit.t> unit
%start <Root.t DocOckTypes.Unit.t> file

%%

name:
  | NAME data = Data CLOSE
      { data }

flag(X):
  | (* empty *)
      { false }
  | X CLOSE
      { true }

module_identifier:
  | ROOT base = Base data = Data CLOSE
      { Identifier.Root(base, data) }
  | MODULE sg = signature_identifier data = Data CLOSE
      { Identifier.Module(sg, data) }
  | pos = Argument sg = signature_identifier data = Data CLOSE
      { match pos with
        | None -> $syntaxerror
        | Some pos -> Identifier.Argument(sg, pos, data) }

module_type_identifier:
  | MODULE_TYPE sg = signature_identifier data = Data CLOSE
      { Identifier.ModuleType(sg, data) }

signature_identifier:
  | md = module_identifier
      { Identifier.signature_of_module md }
  | mty = module_type_identifier
      { Identifier.signature_of_module_type mty }

type_identifier:
  | TYPE sg = signature_identifier data = Data CLOSE
      { Identifier.Type(sg, data) }
  | TYPE data = Data CLOSE
      { Identifier.CoreType data }

constructor_identifier:
  | CONSTRUCTOR sg = type_identifier data = Data CLOSE
      { Identifier.Constructor(sg, data) }

field_identifier:
  | FIELD sg = type_identifier data = Data CLOSE
      { Identifier.Field(sg, data) }

extension_identifier:
  | EXTENSION sg = signature_identifier data = Data CLOSE
      { Identifier.Extension(sg, data) }

exception_identifier:
  | EXCEPTION sg = signature_identifier data = Data CLOSE
      { Identifier.Exception(sg, data) }
  | EXCEPTION data = Data CLOSE
      { Identifier.CoreException data }

value_identifier:
  | VALUE sg = signature_identifier data = Data CLOSE
      { Identifier.Value(sg, data) }

class_identifier:
  | CLASS sg = signature_identifier data = Data CLOSE
      { Identifier.Class(sg, data) }

class_type_identifier:
  | CLASS_TYPE sg = signature_identifier data = Data CLOSE
      { Identifier.ClassType(sg, data) }

class_signature_identifier:
  | cl = class_identifier
      { Identifier.class_signature_of_class cl }
  | clty = class_type_identifier
      { Identifier.class_signature_of_class_type clty }

method_identifier:
  | METHOD sg = class_signature_identifier data = Data CLOSE
      { Identifier.Method(sg, data) }

instance_variable_identifier:
  | INSTANCE_VARIABLE sg = class_signature_identifier data = Data CLOSE
      { Identifier.InstanceVariable(sg, data) }

label_identifier:
  | LABEL sg = parent_identifier data = Data CLOSE
      { Identifier.Label(sg, data) }

parent_identifier:
  | sg = signature_identifier
    { Identifier.parent_of_signature sg }
  | csig = class_signature_identifier
    { Identifier.parent_of_class_signature csig }
  | typ = type_identifier
    { Identifier.parent_of_datatype typ }

element_identifier:
  | id = module_identifier
    { Identifier.any id }
  | id = module_type_identifier
    { Identifier.any id }
  | id = type_identifier
    { Identifier.any id }
  | id = constructor_identifier
    { Identifier.any id }
  | id = field_identifier
    { Identifier.any id }
  | id = extension_identifier
    { Identifier.any id }
  | id = exception_identifier
    { Identifier.any id }
  | id = value_identifier
    { Identifier.any id }
  | id = class_identifier
    { Identifier.any id }
  | id = class_type_identifier
    { Identifier.any id }
  | id = method_identifier
    { Identifier.any id }
  | id = instance_variable_identifier
    { Identifier.any id }
  | id = label_identifier
    { Identifier.any id }

module_resolved_path:
  | IDENTIFIER id = module_identifier CLOSE
      { Path.Resolved.ident_module id }
  | SUBST sub = module_type_resolved_path p = module_resolved_path CLOSE
      { Path.Resolved.Subst(sub, p) }
  | SUBST_ALIAS sub = module_resolved_path p = module_resolved_path CLOSE
      { Path.Resolved.SubstAlias(sub, p) }
  | MODULE md = module_resolved_path data = Data CLOSE
      { Path.Resolved.Module(md, data) }
  | APPLY md = module_resolved_path arg = module_path CLOSE
      { Path.Resolved.Apply(md, arg) }

module_type_resolved_path:
  | IDENTIFIER id = module_type_identifier CLOSE
      { Path.Resolved.ident_module_type id }
  | MODULE_TYPE md = module_resolved_path data = Data CLOSE
      { Path.Resolved.ModuleType(md, data) }

type_resolved_path:
  | IDENTIFIER id = type_identifier CLOSE
      { Path.Resolved.ident_type id }
  | TYPE md = module_resolved_path data = Data CLOSE
      { Path.Resolved.Type(md, data) }
  | cltyp = class_type_resolved_path
      { relax_class_type_path cltyp }

class_resolved_path:
  | IDENTIFIER id = class_identifier CLOSE
      { Path.Resolved.ident_class id }
  | CLASS md = module_resolved_path data = Data CLOSE
      { Path.Resolved.Class(md, data) }

class_type_resolved_path:
  | IDENTIFIER id = class_type_identifier CLOSE
      { Path.Resolved.ident_class_type id }
  | CLASS_TYPE md = module_resolved_path data = Data CLOSE
      { Path.Resolved.ClassType(md, data) }
  | cl = class_resolved_path
      { relax_class_path cl }

module_path:
  | RESOLVED path = module_resolved_path CLOSE
      { Path.Resolved path }
  | ROOT data = Data CLOSE
      { Path.Root data }
  | DOT md = module_path data = Data CLOSE
      { Path.Dot(md, data) }
  | APPLY md = module_path arg = module_path CLOSE
      { Path.Apply(md, arg) }

module_type_path:
  | RESOLVED path = module_type_resolved_path CLOSE
      { Path.Resolved path }
  | DOT md = module_path data = Data CLOSE
      { Path.Dot(md, data) }

type_path:
  | RESOLVED path = type_resolved_path CLOSE
      { Path.Resolved path }
  | DOT md = module_path data = Data CLOSE
      { Path.Dot(md, data) }

class_type_path:
  | RESOLVED path = class_type_resolved_path CLOSE
      { Path.Resolved path }
  | DOT md = module_path data = Data CLOSE
      { Path.Dot(md, data) }

module_resolved_fragment:
  | SUBST sub = module_type_resolved_path p = module_resolved_fragment CLOSE
      { Fragment.Resolved.Subst(sub, p) }
  | SUBST_ALIAS sub = module_resolved_path p = module_resolved_fragment CLOSE
      { Fragment.Resolved.SubstAlias(sub, p) }
  | MODULE md = signature_resolved_fragment data = Data CLOSE
      { Fragment.Resolved.Module(md, data) }

type_resolved_fragment:
  | TYPE md = signature_resolved_fragment data = Data CLOSE
      { Fragment.Resolved.Type(md, data) }
  | CLASS md = signature_resolved_fragment data = Data CLOSE
      { Fragment.Resolved.Class(md, data) }
  | CLASS_TYPE md = signature_resolved_fragment data = Data CLOSE
      { Fragment.Resolved.ClassType(md, data) }

signature_resolved_fragment:
  | ROOT CLOSE
      { Fragment.Resolved.Root }
  | SUBST sub = module_type_resolved_path p = signature_resolved_fragment CLOSE
      { Fragment.Resolved.Subst(sub, p) }
  | SUBST_ALIAS sub = module_resolved_path p = signature_resolved_fragment CLOSE
      { Fragment.Resolved.SubstAlias(sub, p) }
  | MODULE md = signature_resolved_fragment data = Data CLOSE
      { Fragment.Resolved.Module(md, data) }

signature_fragment:
  | RESOLVED frag = signature_resolved_fragment CLOSE
      { Fragment.Resolved frag }
  | DOT md = signature_fragment data = Data CLOSE
      { Fragment.Dot(md, data) }

module_fragment:
  | RESOLVED frag = module_resolved_fragment CLOSE
      { Fragment.Resolved frag }
  | DOT md = signature_fragment data = Data CLOSE
      { Fragment.Dot(md, data) }

type_fragment:
  | RESOLVED frag = type_resolved_fragment CLOSE
      { Fragment.Resolved frag }
  | DOT md = signature_fragment data = Data CLOSE
      { Fragment.Dot(md, data) }

module_resolved_reference:
  | IDENTIFIER id = module_identifier CLOSE
      { Reference.Resolved.ident_module id }
  | MODULE sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.Module(sg, data) }

module_type_resolved_reference:
  | IDENTIFIER id = module_type_identifier CLOSE
      { Reference.Resolved.ident_module_type id }
  | MODULE_TYPE sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.ModuleType(sg, data) }

signature_resolved_reference:
  | md = module_resolved_reference
      { Reference.Resolved.signature_of_module md }
  | mty = module_type_resolved_reference
      { Reference.Resolved.signature_of_module_type mty }

datatype_resolved_reference:
  | IDENTIFIER id = type_identifier CLOSE
      { Reference.Resolved.ident_type id }
  | TYPE sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.Type(sg, data) }

type_resolved_reference:
  | typ = datatype_resolved_reference
      { relax_datatype_reference typ }
  | cltyp = class_type_resolved_reference
      { relax_class_type_reference cltyp }

constructor_resolved_reference:
  | IDENTIFIER id = constructor_identifier CLOSE
      { Reference.Resolved.ident_constructor id }
  | CONSTRUCTOR sg = datatype_resolved_reference data = Data CLOSE
      { Reference.Resolved.Constructor(sg, data) }
  | ext = extension_resolved_reference
      { relax_extension_reference ext }

field_resolved_reference:
  | IDENTIFIER id = field_identifier CLOSE
      { Reference.Resolved.ident_field id }
  | FIELD sg = datatype_resolved_reference data = Data CLOSE
      { Reference.Resolved.Field(sg, data) }

exception_resolved_reference:
  | IDENTIFIER id = exception_identifier CLOSE
      { Reference.Resolved.ident_exception id }
  | EXCEPTION sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.Exception(sg, data) }

extension_resolved_reference:
  | IDENTIFIER id = extension_identifier CLOSE
      { Reference.Resolved.ident_extension id }
  | EXTENSION sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.Extension(sg, data) }
  | exn = exception_resolved_reference
      { relax_exception_reference exn }

value_resolved_reference:
  | IDENTIFIER id = value_identifier CLOSE
      { Reference.Resolved.ident_value id }
  | VALUE sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.Value(sg, data) }

class_resolved_reference:
  | IDENTIFIER id = class_identifier CLOSE
      { Reference.Resolved.ident_class id }
  | CLASS sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.Class(sg, data) }

class_type_resolved_reference:
  | IDENTIFIER id = class_type_identifier CLOSE
      { Reference.Resolved.ident_class_type id }
  | CLASS_TYPE sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.ClassType(sg, data) }
  | cl = class_resolved_reference
      { relax_class_reference cl }

method_resolved_reference:
  | IDENTIFIER id = method_identifier CLOSE
      { Reference.Resolved.ident_method id }
  | METHOD sg = class_type_resolved_reference data = Data CLOSE
      { Reference.Resolved.Method(sg, data) }

instance_variable_resolved_reference:
  | IDENTIFIER id = instance_variable_identifier CLOSE
      { Reference.Resolved.ident_instance_variable id }
  | INSTANCE_VARIABLE sg = class_type_resolved_reference data = Data CLOSE
      { Reference.Resolved.InstanceVariable(sg, data) }

label_resolved_reference:
  | IDENTIFIER id = label_identifier CLOSE
      { Reference.Resolved.ident_label id }
  | LABEL sg = parent_resolved_reference data = Data CLOSE
      { Reference.Resolved.Label(sg, data) }

parent_resolved_reference:
  | sg = signature_resolved_reference
      { Reference.Resolved.parent_of_signature sg }
  | csig = class_type_resolved_reference
      { Reference.Resolved.parent_of_class_signature csig }
  | t = datatype_resolved_reference
      { Reference.Resolved.parent_of_datatype t }

element_resolved_reference:
  | IDENTIFIER id = element_identifier CLOSE
      { Reference.Resolved.Identifier id }
  | MODULE sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.Module(sg, data) }
  | MODULE_TYPE sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.ModuleType(sg, data) }
  | TYPE sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.Type(sg, data) }
  | CONSTRUCTOR sg = datatype_resolved_reference data = Data CLOSE
      { Reference.Resolved.Constructor(sg, data) }
  | FIELD sg = datatype_resolved_reference data = Data CLOSE
      { Reference.Resolved.Field(sg, data) }
  | EXCEPTION sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.Exception(sg, data) }
  | EXTENSION sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.Extension(sg, data) }
  | VALUE sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.Value(sg, data) }
  | CLASS sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.Class(sg, data) }
  | CLASS_TYPE sg = signature_resolved_reference data = Data CLOSE
      { Reference.Resolved.ClassType(sg, data) }
  | METHOD sg = class_type_resolved_reference data = Data CLOSE
      { Reference.Resolved.Method(sg, data) }
  | INSTANCE_VARIABLE sg = class_type_resolved_reference data = Data CLOSE
      { Reference.Resolved.InstanceVariable(sg, data) }
  | LABEL sg = parent_resolved_reference data = Data CLOSE
      { Reference.Resolved.Label(sg, data) }

module_reference:
  | RESOLVED rf = module_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

module_type_reference:
  | RESOLVED rf = module_type_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

type_reference:
  | RESOLVED rf = type_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

constructor_reference:
  | RESOLVED rf = constructor_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

field_reference:
  | RESOLVED rf = field_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

extension_reference:
  | RESOLVED rf = extension_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

exception_reference:
  | RESOLVED rf = exception_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

value_reference:
  | RESOLVED rf = value_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

class_reference:
  | RESOLVED rf = class_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

class_type_reference:
  | RESOLVED rf = class_type_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

method_reference:
  | RESOLVED rf = method_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

instance_variable_reference:
  | RESOLVED rf = instance_variable_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

label_reference:
  | RESOLVED rf = label_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

parent_reference:
  | RESOLVED rf = parent_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

element_reference:
  | RESOLVED rf = element_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = Data CLOSE
      { Reference.Root data }
  | DOT p = parent_reference data = Data CLOSE
      { Reference.Dot(p, data) }

reference:
  | MODULE rf = module_reference CLOSE
      { Documentation.Module rf }
  | MODULE_TYPE rf = module_type_reference CLOSE
      { Documentation.ModuleType rf }
  | TYPE rf = type_reference CLOSE
      { Documentation.Type rf }
  | CONSTRUCTOR rf = constructor_reference CLOSE
      { Documentation.Constructor rf }
  | FIELD rf = field_reference CLOSE
      { Documentation.Field rf }
  | EXTENSION rf = extension_reference CLOSE
      { Documentation.Extension rf }
  | EXCEPTION rf = exception_reference CLOSE
      { Documentation.Exception rf }
  | VALUE rf = value_reference CLOSE
      { Documentation.Value rf }
  | CLASS rf = class_reference CLOSE
      { Documentation.Class rf }
  | CLASS_TYPE rf = class_type_reference CLOSE
      { Documentation.ClassType rf }
  | METHOD rf = method_reference CLOSE
      { Documentation.Method rf }
  | INSTANCE_VARIABLE rf = instance_variable_reference CLOSE
      { Documentation.InstanceVariable rf }
  | ELEMENT rf = element_reference CLOSE
      { Documentation.Element rf }
  | SECTION rf = label_reference CLOSE
      { Documentation.Section rf }
  | LINK data = Data CLOSE
      { Documentation.Link data }
  | tag = Custom data = Data CLOSE
      { Documentation.Custom(tag, data) }

special:
  | MODULES modules = module_reference* CLOSE
      { Documentation.Modules modules }
  | INDEX CLOSE
      { Documentation.Index }

item:
  | ITEM text = text CLOSE
      { text }

%inline string:
  | (* empty *)
      { "" }
  | data = Data
      { data}

text_element:
  | data = Data
      { Documentation.Raw data }
  | CODE str = string CLOSE
      { Documentation.Code str }
  | PRECODE str = string CLOSE
      { Documentation.PreCode str }
  | VERBATIM str = string CLOSE
      { Documentation.Verbatim str }
  | BOLD text = text CLOSE
      { Documentation.(Style(Bold, text)) }
  | ITALIC text = text CLOSE
      { Documentation.(Style(Italic, text)) }
  | EMPHASIZE text = text CLOSE
      { Documentation.(Style(Emphasize, text)) }
  | CENTER text = text CLOSE
      { Documentation.(Style(Center, text)) }
  | LEFT text = text CLOSE
      { Documentation.(Style(Left, text)) }
  | RIGHT text = text CLOSE
      { Documentation.(Style(Right, text)) }
  | SUPERSCRIPT text = text CLOSE
      { Documentation.(Style(Superscript, text)) }
  | SUBSCRIPT text = text CLOSE
      { Documentation.(Style(Subscript, text)) }
  | tag = Custom text = text CLOSE
      { Documentation.(Style(Custom tag, text)) }
  | LIST i = item+ CLOSE
      { Documentation.List i }
  | ENUM i = item+ CLOSE
      { Documentation.Enum i }
  | NEWLINE CLOSE
      { Documentation.Newline }
  | level = Title label = label_identifier? text = text CLOSE
      { Documentation.Title(level, label, text) }
  | REFERENCE rf = reference text = opttext CLOSE
      { Documentation.Reference(rf, text) }
  | target = Target str = string CLOSE
      { Documentation.Target(target, str) }
  | SPECIAL special = special CLOSE
      { Documentation.Special special }

%inline opttext:
  | (* empty *)
      { None }
  | elems = text_element+
      { Some elems }

%inline text:
  | elems = text_element*
      { elems }

see:
  | URL data = Data CLOSE
      { Documentation.Url data }
  | FILE data = Data CLOSE
      { Documentation.File data }
  | DOC data = Data CLOSE
      { Documentation.Doc data }

tag:
  | AUTHOR data = Data CLOSE
      { Documentation.Author data }
  | VERSION data = Data CLOSE
      { Documentation.Version data }
  | SEE see = see text = text CLOSE
      { Documentation.See(see, text) }
  | SINCE data = Data CLOSE
      { Documentation.Since data }
  | BEFORE name = name text = text CLOSE
      { Documentation.Before(name, text) }
  | DEPRECATED text = text CLOSE
      { Documentation.Deprecated text }
  | PARAM name = name text = text CLOSE
      { Documentation.Param(name, text) }
  | RAISE name = name text = text CLOSE
      { Documentation.Raise(name, text) }
  | RETURN text = text CLOSE
      { Documentation.Return text }
  | TAG name = name text = text CLOSE
      { Documentation.Tag(name, text) }

%inline tags:
  | tags = tag*
      { tags }

int:
| data = Data
    { try
        int_of_string data
      with Failure _ -> $syntaxerror }

line:
| LINE line = int CLOSE
    { line }

column:
| COLUMN column = int CLOSE
    { column }

position:
| POSITION line = line column = column CLOSE
    { Documentation.Error.Position.{line; column} }

offset:
| OFFSET start = position finish = position CLOSE
    { Documentation.Error.Offset.{start; finish} }

filename:
| FILENAME data = Data CLOSE
    { data }

location:
| LOCATION filename = filename
    start = position finish = position CLOSE
      { Documentation.Error.Location.{filename; start; finish} }

doc_error:
| ERROR origin = element_identifier offset = offset
    location = location? message = Data CLOSE
      { Documentation.Error.{origin; offset; location; message} }

doc:
| (* empty *)
    { DocOckAttrs.empty }
| DOC text = text tags = tags CLOSE
    { Documentation.(Ok {text; tags}) }
| DOC err = doc_error CLOSE
    { Documentation.Error err }

comment:
  | COMMENT text = text tags = tags CLOSE
      { Documentation.(Documentation (Ok {text; tags})) }
  | COMMENT err = doc_error CLOSE
      { Documentation.(Documentation (Error err)) }
  | STOP CLOSE
      { Documentation.Stop }


poly_variant_kind:
  | FIXED CLOSE
      { TypeExpr.Variant.Fixed }
  | CLOSED names = name* CLOSE
      { TypeExpr.Variant.Closed names }
  | OPEN CLOSE
      { TypeExpr.Variant.Open }

poly_variant_element:
  | TYPE expr = type_expr CLOSE
      { TypeExpr.Variant.Type expr }
  | CONSTRUCTOR data = Data constant = flag(CONSTANT) types = type_expr* CLOSE
      { TypeExpr.Variant.Constructor(data, constant, types) }

poly_variant:
  | kind = poly_variant_kind elements = poly_variant_element*
      { TypeExpr.Variant.{kind; elements} }

object_method:
  | name = name type_ = type_expr
      { TypeExpr.Object.{name; type_} }

object_:
  | methods = object_method* open_ = flag(OPEN)
      { TypeExpr.Object.{methods; open_} }

package_substitution:
  | frag = type_fragment expr = type_expr
      { (frag, expr) }

package:
  | path = module_type_path substitutions = package_substitution*
      { TypeExpr.Package.{path; substitutions} }

argument_label:
  | LABEL data = Data CLOSE
      { TypeExpr.Label data }
  | OPTIONAL data = Data CLOSE
      { TypeExpr.Optional data }

type_expr:
  | VAR data = Data CLOSE
      { TypeExpr.Var data }
  | ANY CLOSE
      { TypeExpr.Any }
  | ALIAS expr = type_expr data = Data CLOSE
      { TypeExpr.Alias(expr, data) }
  | ARROW lbl = argument_label? arg = type_expr res = type_expr CLOSE
      { TypeExpr.Arrow(lbl, arg, res) }
  | TUPLE types = type_expr+ CLOSE
      { TypeExpr.Tuple types }
  | PATH p = type_path params = type_expr* CLOSE
      { TypeExpr.Constr(p, params) }
  | POLY_VARIANT v = poly_variant CLOSE
      { TypeExpr.Variant v }
  | OBJECT o = object_ CLOSE
      { TypeExpr.Object o }
  | CLASS p = class_type_path params = type_expr* CLOSE
      { TypeExpr.Class(p, params) }
  | POLY names = name+ expr = type_expr CLOSE
      { TypeExpr.Poly(names, expr) }
  | PACKAGE pkg = package CLOSE
      { TypeExpr.Package pkg }

external_primitive:
  | PRIMITIVE data = Data CLOSE
      { data }

constructor_arguments:
  | (* empty *)
      { [] }
  | ARGUMENTS types = type_expr* CLOSE
      { types }

constructor_result:
  | (* empty *)
      { None }
  | RESULT type_ = type_expr CLOSE
      { Some type_ }

constructor:
  | CONSTRUCTOR id = constructor_identifier doc = doc
      args = constructor_arguments res = constructor_result CLOSE
        { TypeDecl.Constructor.{id; doc; args; res} }

field:
  | FIELD id = field_identifier doc = doc
      mutable_ = flag(MUTABLE) type_ = type_expr CLOSE
      { TypeDecl.Field.{id; doc; mutable_; type_} }

type_representation:
  | VARIANT constructors = constructor+ CLOSE
      { TypeDecl.Representation.Variant constructors }
  | RECORD fields = field+ CLOSE
      { TypeDecl.Representation.Record fields }
  | EXTENSIBLE CLOSE
      { TypeDecl.Representation.Extensible }

variance:
  | POS CLOSE
      { TypeDecl.Pos }
  | NEG CLOSE
      { TypeDecl.Neg }

type_parameter:
  | PARAM v = variance? CLOSE
      { (TypeDecl.Any, v) }
  | PARAM name = Data v = variance? CLOSE
      { (TypeDecl.Var name, v) }

type_subst_parameter:
  | PARAM name = Data CLOSE
      { name }

type_constraint:
  | CONSTRAINT expr1 = type_expr expr2 = type_expr CLOSE
      { (expr1, expr2) }

type_equation:
  | params = type_parameter* private_ = flag(PRIVATE) manifest = type_expr?
      constraints = type_constraint*
        { let open TypeDecl.Equation in
            {params; private_; manifest; constraints} }

extension_constructor:
  | CONSTRUCTOR id = extension_identifier doc = doc
      args = constructor_arguments res = constructor_result CLOSE
        { Extension.Constructor.{id; doc; args; res} }

class_decl:
  | clty = class_type_expr
      { Class.ClassType clty }
  | ARROW lbl = argument_label? arg = type_expr res = class_decl CLOSE
      { Class.Arrow(lbl, arg, res) }

class_type_expr:
  | PATH p = class_type_path params = type_expr* CLOSE
      { ClassType.Constr(p, params) }
  | SIGNATURE self = type_expr? items = class_signature_item* CLOSE
      { let sg = ClassSignature.{self; items} in
          ClassType.Signature sg }

class_signature_item:
  | INSTANCE_VARIABLE id = instance_variable_identifier doc = doc
      mutable_ = flag(MUTABLE) virtual_ = flag(VIRTUAL) type_ = type_expr CLOSE
        { let open ClassSignature in
          let open InstanceVariable in
            InstanceVariable {id;doc;mutable_;virtual_;type_} }
  | METHOD id = method_identifier doc = doc private_ = flag(PRIVATE)
      virtual_ = flag(VIRTUAL) type_ = type_expr CLOSE
        { let open ClassSignature in
          let open Method in
            Method {id;doc;private_;virtual_;type_} }
  | CONSTRAINT expr1 = type_expr expr2 = type_expr CLOSE
      { ClassSignature.Constraint(expr1, expr2) }
  | INHERIT csig = class_type_expr CLOSE
      { ClassSignature.Inherit csig }
  | comment = comment
      { ClassSignature.Comment comment }

module_decl:
  | ALIAS p = module_path CLOSE
      { Module.Alias p }
  | TYPE expr = module_type_expr CLOSE
      { Module.ModuleType expr }

substitution:
  | MODULE frag = module_fragment eq = module_decl CLOSE
      { ModuleType.ModuleEq(frag, eq) }
  | MODULE_SUBST frag = module_fragment p = module_path CLOSE
      { ModuleType.ModuleSubst(frag, p) }
  | TYPE frag = type_fragment eq = type_equation CLOSE
      { ModuleType.TypeEq(frag, eq) }
  | TYPE_SUBST frag = type_fragment
      params = type_subst_parameter* p = type_path CLOSE
        { ModuleType.TypeSubst(frag, params, p) }

module_argument:
  | Argument id = module_identifier expr = module_type_expr CLOSE
      { Some(id, expr) }
  | Argument CLOSE
      { None }

module_type_expr:
  | p = module_type_path
      { ModuleType.Path p }
  | SIGNATURE sg = signature_item* CLOSE
      { ModuleType.Signature sg }
  | FUNCTOR args = module_argument+ expr = module_type_expr CLOSE
      { List.fold_right
          (fun s e -> ModuleType.Functor(s, e))
          args expr }
  | WITH expr = module_type_expr substs = substitution+ CLOSE
      { ModuleType.With(expr, substs) }
  | TYPEOF md = module_decl CLOSE
      { ModuleType.TypeOf md }

signature_item:
  | VALUE id = value_identifier doc = doc type_ = type_expr CLOSE
      { let open Signature in
        let open Value in
          Value {id;doc;type_} }
  | EXTERNAL id = value_identifier doc = doc type_ = type_expr
      primitives = external_primitive+ CLOSE
        { let open Signature in
          let open External in
            External {id; doc; type_; primitives} }
  | TYPE id = type_identifier doc = doc equation = type_equation
      representation = type_representation? CLOSE
        { let open Signature in
          let open TypeDecl in
            Type {id; doc; equation; representation} }
  | EXTENSION type_path = type_path doc = doc type_params = type_parameter*
      private_ = flag(PRIVATE) constructors = extension_constructor+ CLOSE
        { let open Signature in
          let open Extension in
            TypExt {type_path; doc; type_params; private_; constructors} }
  | EXCEPTION id = exception_identifier doc = doc
      args = constructor_arguments res = constructor_result CLOSE
        { let open Signature in
          let open Exception in
            Exception {id; doc; args; res} }
  | CLASS id = class_identifier doc = doc params = type_parameter*
      virtual_ = flag(VIRTUAL) type_ = class_decl CLOSE
        { let open Signature in
          let open Class in
            Class {id; doc; virtual_; params; type_} }
  | CLASS_TYPE id = class_type_identifier doc = doc params = type_parameter*
      virtual_ = flag(VIRTUAL) expr = class_type_expr CLOSE
        { let open Signature in
          let open ClassType in
            ClassType {id; doc; virtual_; params; expr} }
  | MODULE id = module_identifier doc = doc type_ = module_decl CLOSE
      { let open Signature in
        let open Module in
           Module {id; doc; type_} }
  | MODULE_TYPE id = module_type_identifier doc = doc
      expr = module_type_expr? CLOSE
      { let open Signature in
        let open ModuleType in
          ModuleType {id; doc; expr} }
  | INCLUDE expr = module_type_expr CLOSE
      { Signature.Include expr }
  | comment = comment
      { Signature.Comment comment }

digest:
  | DIGEST data = Data CLOSE
      { try
          Digest.from_hex data
        with Invalid_argument _ -> $syntaxerror }

unit_import:
  | IMPORT data = Data digest = digest? CLOSE
      { Unit.Import.Unresolved(data, digest) }
  | IMPORT base = Base CLOSE
      { Unit.Import.Resolved base }

source_file:
  | FILE data = Data CLOSE
      { data }

source_build_dir:
  | DIR data = Data CLOSE
      { data }

source:
  | SOURCE file = source_file build_dir = source_build_dir digest = digest CLOSE
      { let open Unit.Source in
          {file; build_dir; digest} }

packed_item:
  | ITEM id = module_identifier path = module_path CLOSE
      { let open Unit.Packed in
          {id; path} }

unit_content:
  | MODULE items = signature_item* CLOSE
      { Unit.Module items }
  | PACK items = packed_item* CLOSE
      { Unit.Pack items }

unit:
  | UNIT id = module_identifier doc = doc digest = digest imports = unit_import*
      source = source? interface = flag(INTERFACE) content = unit_content CLOSE
          { let open Unit in
              {id; doc; digest; imports; source; interface; content} }

file:
  | DTD unit = unit EOF
      { unit }
