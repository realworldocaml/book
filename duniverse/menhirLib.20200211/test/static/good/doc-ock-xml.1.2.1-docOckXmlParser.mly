(* Original file: doc-ock-xml.1.2.1/doc-ock-xml-1.2.1/src/docOckXmlParser.mly *)
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

open DocOck.Paths
open DocOck.Types

let relax_class_path cl =
  match (cl : ('a, Kind.identifier_class) Path.Resolved.t) with
  | Path.Resolved.Identifier (Identifier.Class _)
  | Path.Resolved.Class _ as cl -> cl

let relax_class_type_path cltyp =
  match (cltyp : 'a Path.Resolved.class_type) with
  | Path.Resolved.Identifier (Identifier.Class _ | Identifier.ClassType _)
  | Path.Resolved.Class _
  | Path.Resolved.ClassType _ as cltyp -> cltyp

let relax_class_reference cl =
  match (cl : 'a Reference.Resolved.class_) with
  | Reference.Resolved.Identifier (Identifier.Class _)
  | Reference.Resolved.Class _ as cl -> cl

%}

%token ALIAS
%token ALREADY_A_SIG
%token ANY
%token APPLY
%token ARGUMENTS
%token ARROW
%token AUTHOR
%token BEFORE
%token BOLD
%token CANONICAL
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
%token EXPANSION
%token EXTENSIBLE
%token EXTENSION
%token EXTERNAL
%token FIELD
%token FILE
%token FILENAME
%token FIXED
%token FORWARD
%token FUNCTOR
%token HIDDEN
%token IDENTIFIER
%token IMPORT
%token INCLUDE
%token INDEX
%token INHERIT
%token INLINE
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
%token PAGE
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
%token TEXT
%token TUPLE
%token TYPE
%token TYPEOF
%token TYPE_SUBST
%token UNIT
%token UNKNOWN
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

%start <Root.t DocOck.Types.Documentation.text> text_entry
%start <Root.t DocOck.Types.Unit.t> unit
%start <Root.t DocOck.Types.Unit.t> unit_file
%start <Root.t DocOck.Types.Page.t> page
%start <Root.t DocOck.Types.Page.t> page_file

%%

name:
  | NAME data = Data CLOSE
      { data }

flag(X):
  | (* empty *)
      { false }
  | X CLOSE
      { true }

%inline string:
  | (* empty *)
      { "" }
  | data = Data
      { data}

page_identifier:
  | PAGE base = Base data = string CLOSE
      { Identifier.Page(base, data) }

module_identifier:
  | ROOT base = Base data = string CLOSE
      { Identifier.Root(base, data) }
  | MODULE sg = signature_identifier data = string CLOSE
      { Identifier.Module(sg, data) }
  | pos = Argument sg = signature_identifier data = string CLOSE
      { match pos with
        | None -> $syntaxerror
        | Some pos -> Identifier.Argument(sg, pos, data) }

module_type_identifier:
  | MODULE_TYPE sg = signature_identifier data = string CLOSE
      { Identifier.ModuleType(sg, data) }

signature_identifier:
  | md = module_identifier
      { Identifier.signature_of_module md }
  | mty = module_type_identifier
      { Identifier.signature_of_module_type mty }

type_identifier:
  | TYPE sg = signature_identifier data = string CLOSE
      { Identifier.Type(sg, data) }
  | TYPE data = string CLOSE
      { Identifier.CoreType data }

constructor_identifier:
  | CONSTRUCTOR sg = type_identifier data = string CLOSE
      { Identifier.Constructor(sg, data) }

field_identifier:
  | FIELD sg = parent_identifier data = string CLOSE
      { Identifier.Field(sg, data) }

extension_identifier:
  | EXTENSION sg = signature_identifier data = string CLOSE
      { Identifier.Extension(sg, data) }

exception_identifier:
  | EXCEPTION sg = signature_identifier data = string CLOSE
      { Identifier.Exception(sg, data) }
  | EXCEPTION data = string CLOSE
      { Identifier.CoreException data }

value_identifier:
  | VALUE sg = signature_identifier data = string CLOSE
      { Identifier.Value(sg, data) }

class_identifier:
  | CLASS sg = signature_identifier data = string CLOSE
      { Identifier.Class(sg, data) }

class_type_identifier:
  | CLASS_TYPE sg = signature_identifier data = string CLOSE
      { Identifier.ClassType(sg, data) }

class_signature_identifier:
  | cl = class_identifier
      { Identifier.class_signature_of_class cl }
  | clty = class_type_identifier
      { Identifier.class_signature_of_class_type clty }

method_identifier:
  | METHOD sg = class_signature_identifier data = string CLOSE
      { Identifier.Method(sg, data) }

instance_variable_identifier:
  | INSTANCE_VARIABLE sg = class_signature_identifier data = string CLOSE
      { Identifier.InstanceVariable(sg, data) }

label_identifier:
  | LABEL sg = label_parent_identifier data = string CLOSE
      { Identifier.Label(sg, data) }

parent_identifier:
  | sg = signature_identifier
    { Identifier.parent_of_signature sg }
  | csig = class_signature_identifier
    { Identifier.parent_of_class_signature csig }
  | typ = type_identifier
    { Identifier.parent_of_datatype typ }

label_parent_identifier:
  | p = parent_identifier
    { Identifier.label_parent_of_parent p }
  | p = page_identifier
    { Identifier.label_parent_of_page p }

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
  | MODULE md = module_resolved_path data = string CLOSE
      { Path.Resolved.Module(md, data) }
  | CANONICAL rp = module_resolved_path p = module_path CLOSE
      { Path.Resolved.Canonical (rp, p) }
  | APPLY md = module_resolved_path arg = module_path CLOSE
      { Path.Resolved.Apply(md, arg) }

module_type_resolved_path:
  | IDENTIFIER id = module_type_identifier CLOSE
      { Path.Resolved.ident_module_type id }
  | MODULE_TYPE md = module_resolved_path data = string CLOSE
      { Path.Resolved.ModuleType(md, data) }

type_resolved_path:
  | IDENTIFIER id = type_identifier CLOSE
      { Path.Resolved.ident_type id }
  | TYPE md = module_resolved_path data = string CLOSE
      { Path.Resolved.Type(md, data) }
  | cltyp = class_type_resolved_path
      { relax_class_type_path cltyp }

class_resolved_path:
  | IDENTIFIER id = class_identifier CLOSE
      { Path.Resolved.ident_class id }
  | CLASS md = module_resolved_path data = string CLOSE
      { Path.Resolved.Class(md, data) }

class_type_resolved_path:
  | IDENTIFIER id = class_type_identifier CLOSE
      { Path.Resolved.ident_class_type id }
  | CLASS_TYPE md = module_resolved_path data = string CLOSE
      { Path.Resolved.ClassType(md, data) }
  | cl = class_resolved_path
      { relax_class_path cl }

module_path:
  | RESOLVED path = module_resolved_path CLOSE
      { Path.Resolved path }
  | ROOT data = string CLOSE
      { Path.Root data }
  | FORWARD data = string CLOSE
      { Path.Forward data }
  | DOT md = module_path data = string CLOSE
      { Path.Dot(md, data) }
  | APPLY md = module_path arg = module_path CLOSE
      { Path.Apply(md, arg) }

canonical:
  | (* empty *)
      { None }
  | CANONICAL p = module_path r = module_reference CLOSE
      { Some (p, r) }

module_type_path:
  | RESOLVED path = module_type_resolved_path CLOSE
      { Path.Resolved path }
  | DOT md = module_path data = string CLOSE
      { Path.Dot(md, data) }

type_path:
  | RESOLVED path = type_resolved_path CLOSE
      { Path.Resolved path }
  | DOT md = module_path data = string CLOSE
      { Path.Dot(md, data) }

class_type_path:
  | RESOLVED path = class_type_resolved_path CLOSE
      { Path.Resolved path }
  | DOT md = module_path data = string CLOSE
      { Path.Dot(md, data) }

module_resolved_fragment:
  | SUBST sub = module_type_resolved_path p = module_resolved_fragment CLOSE
      { Fragment.Resolved.Subst(sub, p) }
  | SUBST_ALIAS sub = module_resolved_path p = module_resolved_fragment CLOSE
      { Fragment.Resolved.SubstAlias(sub, p) }
  | MODULE md = signature_resolved_fragment data = string CLOSE
      { Fragment.Resolved.Module(md, data) }

type_resolved_fragment:
  | TYPE md = signature_resolved_fragment data = string CLOSE
      { Fragment.Resolved.Type(md, data) }
  | CLASS md = signature_resolved_fragment data = string CLOSE
      { Fragment.Resolved.Class(md, data) }
  | CLASS_TYPE md = signature_resolved_fragment data = string CLOSE
      { Fragment.Resolved.ClassType(md, data) }

signature_resolved_fragment:
  | ROOT CLOSE
      { Fragment.Resolved.Root }
  | SUBST sub = module_type_resolved_path p = module_resolved_fragment CLOSE
      { Fragment.Resolved.Subst(sub, p) }
  | SUBST_ALIAS sub = module_resolved_path p = module_resolved_fragment CLOSE
      { Fragment.Resolved.SubstAlias(sub, p) }
  | MODULE md = signature_resolved_fragment data = string CLOSE
      { Fragment.Resolved.Module(md, data) }

signature_fragment:
  | RESOLVED frag = signature_resolved_fragment CLOSE
      { Fragment.Resolved frag }
  | DOT md = signature_fragment data = string CLOSE
      { Fragment.Dot(md, data) }

module_fragment:
  | RESOLVED frag = module_resolved_fragment CLOSE
      { Fragment.Resolved frag }
  | DOT md = signature_fragment data = string CLOSE
      { Fragment.Dot(md, data) }

type_fragment:
  | RESOLVED frag = type_resolved_fragment CLOSE
      { Fragment.Resolved frag }
  | DOT md = signature_fragment data = string CLOSE
      { Fragment.Dot(md, data) }

module_resolved_reference:
  | IDENTIFIER id = module_identifier CLOSE
      { Reference.Resolved.ident_module id }
  | MODULE sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.Module(sg, data) }
  | SUBST_ALIAS sub = module_resolved_path md = module_resolved_reference CLOSE
      { Reference.Resolved.SubstAlias(sub, md) }
  | CANONICAL rp = module_resolved_reference p = module_reference CLOSE
      { Reference.Resolved.Canonical (rp, p) }

module_type_resolved_reference:
  | IDENTIFIER id = module_type_identifier CLOSE
      { Reference.Resolved.ident_module_type id }
  | MODULE_TYPE sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.ModuleType(sg, data) }

signature_resolved_reference:
  | md = module_resolved_reference
      { Reference.Resolved.signature_of_module md }
  | mty = module_type_resolved_reference
      { Reference.Resolved.signature_of_module_type mty }

datatype_resolved_reference:
  | IDENTIFIER id = type_identifier CLOSE
      { Reference.Resolved.ident_type id }
  | TYPE sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.Type(sg, data) }

class_resolved_reference:
  | IDENTIFIER id = class_identifier CLOSE
      { Reference.Resolved.ident_class id }
  | CLASS sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.Class(sg, data) }

class_type_resolved_reference:
  | IDENTIFIER id = class_type_identifier CLOSE
      { Reference.Resolved.ident_class_type id }
  | CLASS_TYPE sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.ClassType(sg, data) }
  | cl = class_resolved_reference
      { relax_class_reference cl }

parent_resolved_reference:
  | sg = signature_resolved_reference
      { Reference.Resolved.parent_of_signature sg }
  | csig = class_type_resolved_reference
      { Reference.Resolved.parent_of_class_signature csig }
  | t = datatype_resolved_reference
      { Reference.Resolved.parent_of_datatype t }

label_parent_resolved_reference:
  | pr = parent_resolved_reference
      { Reference.Resolved.label_parent_of_parent pr }
  | IDENTIFIER id = page_identifier CLOSE
      { Reference.Resolved.ident_page id }

element_resolved_reference:
  | IDENTIFIER id = element_identifier CLOSE
      { Reference.Resolved.Identifier id }
  | MODULE sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.Module(sg, data) }
  | MODULE_TYPE sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.ModuleType(sg, data) }
  | TYPE sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.Type(sg, data) }
  | CONSTRUCTOR sg = datatype_resolved_reference data = string CLOSE
      { Reference.Resolved.Constructor(sg, data) }
  | FIELD sg = parent_resolved_reference data = string CLOSE
      { Reference.Resolved.Field(sg, data) }
  | EXCEPTION sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.Exception(sg, data) }
  | EXTENSION sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.Extension(sg, data) }
  | VALUE sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.Value(sg, data) }
  | CLASS sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.Class(sg, data) }
  | CLASS_TYPE sg = signature_resolved_reference data = string CLOSE
      { Reference.Resolved.ClassType(sg, data) }
  | METHOD sg = class_type_resolved_reference data = string CLOSE
      { Reference.Resolved.Method(sg, data) }
  | INSTANCE_VARIABLE sg = class_type_resolved_reference data = string CLOSE
      { Reference.Resolved.InstanceVariable(sg, data) }
  | LABEL sg = label_parent_resolved_reference data = string CLOSE
      { Reference.Resolved.Label(sg, data) }

reference_tag:
  | TAG UNKNOWN CLOSE { Reference.TUnknown }
  | TAG MODULE CLOSE { Reference.TModule }
  | TAG MODULE_TYPE CLOSE { Reference.TModuleType }
  | TAG TYPE CLOSE { Reference.TType }
  | TAG CONSTRUCTOR CLOSE { Reference.TConstructor }
  | TAG EXTENSION CLOSE { Reference.TExtension }
  | TAG EXCEPTION CLOSE { Reference.TException }
  | TAG FIELD CLOSE { Reference.TField }
  | TAG VALUE CLOSE { Reference.TValue }
  | TAG CLASS CLOSE { Reference.TClass }
  | TAG CLASS_TYPE CLOSE { Reference.TClassType }
  | TAG METHOD CLOSE { Reference.TMethod }
  | TAG INSTANCE_VARIABLE CLOSE { Reference.TInstanceVariable }
  | TAG LABEL CLOSE { Reference.TLabel }

parent_reference:
  | RESOLVED rf = parent_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = string tag = reference_tag CLOSE
      { match (Obj.magic tag : Reference.kind Reference.tag) with
        | Reference.TUnknown as tag -> Reference.Root (data, tag)
        | Reference.TModule as tag -> Reference.Root (data, tag)
        | Reference.TModuleType as tag -> Reference.Root (data, tag)
        | Reference.TType as tag -> Reference.Root (data, tag)
        | Reference.TClass as tag -> Reference.Root (data, tag)
        | Reference.TClassType as tag -> Reference.Root (data, tag)
 	| _ -> assert false }
  | DOT p = parent_reference data = string CLOSE
      { Reference.Dot(Reference.label_parent_of_parent p, data) }
  | MODULE p = signature_reference data = string CLOSE
      { Reference.Module(p, data) }
  | MODULE_TYPE p = signature_reference data = string CLOSE
      { Reference.ModuleType(p, data) }
  | TYPE p = signature_reference data = string CLOSE
      { Reference.Type(p, data) }
  | CLASS p = signature_reference data = string CLOSE
      { Reference.Class(p, data) }
  | CLASS_TYPE p = signature_reference data = string CLOSE
      { Reference.ClassType(p, data) }

label_parent_reference:
  | RESOLVED rf = label_parent_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = string tag = reference_tag CLOSE
      { match (Obj.magic tag : Reference.kind Reference.tag) with
        | Reference.TUnknown as tag -> Reference.Root (data, tag)
        | Reference.TModule as tag -> Reference.Root (data, tag)
        | Reference.TModuleType as tag -> Reference.Root (data, tag)
        | Reference.TType as tag -> Reference.Root (data, tag)
        | Reference.TClass as tag -> Reference.Root (data, tag)
        | Reference.TClassType as tag -> Reference.Root (data, tag)
 	| _ -> assert false }
  | DOT p = label_parent_reference data = string CLOSE
      { Reference.Dot(p, data) }
  | MODULE p = signature_reference data = string CLOSE
      { Reference.Module(p, data) }
  | MODULE_TYPE p = signature_reference data = string CLOSE
      { Reference.ModuleType(p, data) }
  | TYPE p = signature_reference data = string CLOSE
      { Reference.Type(p, data) }
  | CLASS p = signature_reference data = string CLOSE
      { Reference.Class(p, data) }
  | CLASS_TYPE p = signature_reference data = string CLOSE
      { Reference.ClassType(p, data) }

signature_reference:
  | RESOLVED rf = signature_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = string tag = reference_tag CLOSE
      { match (Obj.magic tag : Reference.kind Reference.tag) with
        | Reference.TUnknown as tag -> Reference.Root (data, tag)
        | Reference.TModule as tag -> Reference.Root (data, tag)
        | Reference.TModuleType as tag -> Reference.Root (data, tag)
	| _ -> assert false }
  | DOT p = parent_reference data = string CLOSE
      { Reference.Dot(Reference.label_parent_of_parent p, data) }
  | MODULE p = signature_reference data = string CLOSE
      { Reference.Module(p, data) }
  | MODULE_TYPE p = signature_reference data = string CLOSE
      { Reference.ModuleType(p, data) }

module_reference:
  | RESOLVED rf = module_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = string tag = reference_tag CLOSE
      { match (Obj.magic tag : Reference.kind Reference.tag) with
        | Reference.TUnknown as tag -> Reference.Root (data, tag)
        | Reference.TModule as tag -> Reference.Root (data, tag)
	| _ -> assert false }
  | DOT p = parent_reference data = string CLOSE
      { Reference.Dot(Reference.label_parent_of_parent p, data) }

datatype_reference:
  | RESOLVED rf = datatype_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = string tag = reference_tag CLOSE
      { match (Obj.magic tag : Reference.kind Reference.tag) with
        | Reference.TUnknown as tag -> Reference.Root (data, tag)
        | Reference.TType as tag -> Reference.Root (data, tag)
	| _ -> assert false }
  | DOT p = parent_reference data = string CLOSE
      { Reference.Dot(Reference.label_parent_of_parent p, data) }
  | TYPE p = signature_reference data = string CLOSE
      { Reference.Type(p, data) }

class_signature_reference:
  | RESOLVED rf = class_type_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = string tag = reference_tag CLOSE
      { match (Obj.magic tag : Reference.kind Reference.tag) with
        | Reference.TUnknown as tag -> Reference.Root (data, tag)
        | Reference.TClass as tag -> Reference.Root (data, tag)
        | Reference.TClassType as tag -> Reference.Root (data, tag)
	| _ -> assert false }
  | DOT p = parent_reference data = string CLOSE
      { Reference.Dot(Reference.label_parent_of_parent p, data) }
  | CLASS p = signature_reference data = string CLOSE
      { Reference.Class(p, data) }
  | CLASS_TYPE p = signature_reference data = string CLOSE
      { Reference.ClassType(p, data) }

element_reference:
  | RESOLVED rf = element_resolved_reference CLOSE
      { Reference.Resolved rf }
  | ROOT data = string tag = reference_tag CLOSE
      { Reference.Root (data, (Obj.magic tag : Reference.kind Reference.tag)) }
  | DOT p = label_parent_reference data = string CLOSE
      { Reference.Dot(p, data) }
  | MODULE p = signature_reference data = string CLOSE
      { Reference.Module(p, data) }
  | MODULE_TYPE p = signature_reference data = string CLOSE
      { Reference.ModuleType(p, data) }
  | TYPE p = signature_reference data = string CLOSE
      { Reference.Type(p, data) }
  | CONSTRUCTOR p = datatype_reference data = string CLOSE
      { Reference.Constructor(p, data) }
  | FIELD p = parent_reference data = string CLOSE
      { Reference.Field(p, data) }
  | EXTENSION p = signature_reference data = string CLOSE
      { Reference.Extension(p, data) }
  | EXCEPTION p = signature_reference data = string CLOSE
      { Reference.Exception(p, data) }
  | VALUE p = signature_reference data = string CLOSE
      { Reference.Value(p, data) }
  | CLASS p = signature_reference data = string CLOSE
      { Reference.Class(p, data) }
  | CLASS_TYPE p = signature_reference data = string CLOSE
      { Reference.ClassType(p, data) }
  | METHOD p = class_signature_reference data = string CLOSE
      { Reference.Method(p, data) }
  | INSTANCE_VARIABLE p = class_signature_reference data = string CLOSE
      { Reference.InstanceVariable(p, data) }
  | LABEL p = label_parent_reference data = string CLOSE
      { Reference.Label(p, data) }

reference:
  | ELEMENT rf = element_reference CLOSE
      { Documentation.Element rf }
  | LINK data = string CLOSE
      { Documentation.Link data }
  | tag = Custom data = string CLOSE
      { Documentation.Custom(tag, data) }

documented_module:
  | rf = module_reference doc = text
      { (rf, doc) }

special:
  | MODULES modules = documented_module* CLOSE
      { Documentation.Modules modules }
  | INDEX CLOSE
      { Documentation.Index }

item:
  | ITEM text = text CLOSE
      { text }

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
  | URL data = string CLOSE
      { Documentation.Url data }
  | FILE data = string CLOSE
      { Documentation.File data }
  | DOC data = string CLOSE
      { Documentation.Doc data }

tag:
  | AUTHOR data = string CLOSE
      { Documentation.Author data }
  | VERSION data = string CLOSE
      { Documentation.Version data }
  | SEE see = see text = text CLOSE
      { Documentation.See(see, text) }
  | SINCE data = string CLOSE
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
  | INLINE CLOSE
      { Documentation.Inline }
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
| FILENAME data = string CLOSE
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
    { DocOck.Attrs.empty }
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

object_field:
  | METHOD m = object_method CLOSE
      { TypeExpr.Object.Method m }
  | INHERIT t = type_expr CLOSE
      { TypeExpr.Object.Inherit t }

object_:
  | fields = object_field* open_ = flag(OPEN)
      { TypeExpr.Object.{fields; open_} }

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
      { TypeDecl.Constructor.Tuple [] }
  | ARGUMENTS types = type_expr* CLOSE
      { TypeDecl.Constructor.Tuple types }
  | RECORD fields = field+ CLOSE
      { TypeDecl.Constructor.Record fields }

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

class_type_expansion_opt:
  | EXPANSION CLOSE { None }
  | EXPANSION cts = class_type_signature CLOSE { Some cts }

class_type_signature:
  | SIGNATURE self = type_expr? items = class_signature_item* CLOSE
      { ClassSignature.{self; items} }

class_type_expr:
  | PATH p = class_type_path params = type_expr* CLOSE
      { ClassType.Constr(p, params) }
  | sg = class_type_signature
      { ClassType.Signature sg }

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
  | TYPE_SUBST frag = type_fragment eq = type_equation CLOSE
        { ModuleType.TypeSubst(frag, eq) }

module_argument:
  | Argument id = module_identifier expr = module_type_expr
      expansion = module_expansion_opt CLOSE
      { Some{FunctorArgument. id; expr; expansion} }
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

expansion_opt:
  | EXPANSION CLOSE { None }
  | EXPANSION SIGNATURE sg = signature_item* CLOSE CLOSE { Some sg }

module_expansion_opt:
  | opt = expansion_opt
      { match opt with None -> None | Some sg -> Some (Module.Signature sg) }
  | EXPANSION ALREADY_A_SIG CLOSE { Some Module.AlreadyASig }
  | EXPANSION FUNCTOR args = module_argument* SIGNATURE sg = signature_item* CLOSE CLOSE CLOSE
      { Some (Module.Functor (args, sg)) }

include_expansion:
  | EXPANSION resolved = flag(RESOLVED) SIGNATURE content = signature_item* CLOSE CLOSE
      { { Include.resolved; content; } }

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
      virtual_ = flag(VIRTUAL) type_ = class_decl
      expansion = class_type_expansion_opt CLOSE
        { let open Signature in
          let open Class in
            Class {id; doc; virtual_; params; type_; expansion} }
  | CLASS_TYPE id = class_type_identifier doc = doc params = type_parameter*
      virtual_ = flag(VIRTUAL) expr = class_type_expr
      expansion = class_type_expansion_opt CLOSE
        { let open Signature in
          let open ClassType in
            ClassType {id; doc; virtual_; params; expr; expansion} }
  | MODULE id = module_identifier doc = doc type_ = module_decl
      expansion = module_expansion_opt canonical = canonical
      hidden = flag(HIDDEN) display_type = option(module_decl) CLOSE
      { let open Signature in
        let open Module in
          Module {id; doc; type_; expansion; canonical; hidden; display_type} }
  | MODULE_TYPE id = module_type_identifier doc = doc
      expr = module_type_expr?  expansion = module_expansion_opt CLOSE
      { let open Signature in
        let open ModuleType in
          ModuleType {id; doc; expr; expansion} }
  | INCLUDE parent = signature_identifier doc = doc decl = module_decl
      expansion = include_expansion CLOSE
      { let open Signature in
        let open Include in
          Include {parent; doc; decl; expansion} }
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
      source = source? interface = flag(INTERFACE) hidden = flag(HIDDEN)
      content = unit_content expansion = expansion_opt CLOSE
          { let open Unit in
              {id; doc; digest; imports; source; expansion ;
               interface; hidden; content} }

unit_file:
  | DTD unit = unit EOF
      { unit }

page:
  | PAGE name = page_identifier content = doc digest = digest CLOSE
      { let open Page in
          {name; content; digest} }

page_file:
  | DTD page = page EOF
      { page }

text_entry:
  | TEXT elems = text_element* CLOSE
      { elems }
