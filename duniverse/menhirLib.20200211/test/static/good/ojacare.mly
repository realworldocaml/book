%{
(*	$Id: parser.mly,v 1.3 2004/03/11 19:51:45 henry Exp $	*)

open Idl
open Syntax_error

(* fonctions de constructions : permets d'associer la position dans le fichier source *)

let make_ident s = {
  id_location = Loc.get ();
  id_desc = s
}

let make_qident p n = {
  qid_location = Loc.get ();
  qid_package = p;
  qid_name = n;
}

let make_type  t = {
  t_location = Loc.get ();
  t_desc = t
}

let make_modifiers desc = {
  mo_location = Loc.get ();
  mo_desc = desc
}

let make_annotation desc = {
  an_location = Loc.get ();
  an_desc = desc
}

let make_arg ?(annotations = []) ?ident t = {
  arg_location = Loc.get ();
  arg_type = t ;
  arg_annot = annotations
}

type def_sort =
    Content of content | Init of init

let make_init annotation args =
  Init
    {
     i_location = Loc.get ();
     i_annot = annotation;
     i_args = args
   }

let make_field ?(modifiers = []) ?(annotations = []) t name =
  List.iter (fun m -> match m.mo_desc with
    Istatic | Ifinal -> ()
  | d -> raise (Syntax_error (Efield_modifiers m))) modifiers;
  Content (Field
    {
     f_location = Loc.get ();
     f_modifiers = modifiers;
     f_annot = annotations;
     f_name = name;
     f_type = t
   })

let make_method ?(modifiers = []) ?(annotations = []) rtype name args =
  List.iter (fun m -> match m.mo_desc with
    Istatic | Iabstract -> ()
  | d -> raise (Syntax_error (Emethod_modifiers m))) modifiers;
  Content (Method
    {
     m_location = Loc.get ();
     m_annot = annotations;
     m_modifiers = modifiers;
     m_name = name;
     m_return_type = rtype;
     m_args = args
   })

let filter_inits l = List.fold_left (fun l -> (function (Init d) -> d::l | _ -> l)) [] l
let filter_contents l = List.fold_left (fun l -> (function (Content d) -> d::l | _ -> l)) [] l

let make_def ?(modifiers = []) ?(annotations = []) ?(interface = false) name  ?super ?(implements = []) decls =
  let contents = filter_contents decls
  and inits = filter_inits decls in

(*  let inits = (* ajout init par défaut ... non car conflit de nom *)
   if inits = [] && not interface then
   [{ i_location = Loc.get ();
   i_annot = [];
   i_args = [] }]
   else inits in *)

  if interface then begin
    List.iter (fun m -> match m.mo_desc with
    | d -> raise (Syntax_error (Einterface_modifiers m))) modifiers;
    if inits != [] then  raise (Syntax_error (Enoinit (List.hd inits)));
    List.iter (fun c ->
      match c with
      | Method m ->
	  if m.m_modifiers != [] then
	    let m = List.hd m.m_modifiers in
	    raise (Syntax_error (Einterfacemethod_modifiers m))
      | Field f ->
	  if f.f_modifiers != [] then
	    let f = List.hd f.f_modifiers in
	    raise (Syntax_error (Einterfacefield_modifiers f))
      ) contents;
  end
  else begin
    List.iter (fun m -> match m.mo_desc with
      Iabstract -> ()
    | d -> raise (Syntax_error (Eclass_modifiers m))) modifiers
  end;
  {
   d_location = Loc.get ();
   d_super = super;
   d_implements = implements;
   d_interface = interface;
   d_modifiers = modifiers;
   d_annot = annotations;
   d_name = name;
   d_inits = inits;
   d_contents = contents;
 }

let make_package name defs = {
   p_name = name;
   p_defs = defs;
 }

%}

%token PACKAGE

%token CLASS INTERFACE
%token EXTENDS IMPLEMENTS
%token STATIC ABSTRACT FINAL
%token NAME CALLBACK

%token VOID
%token BOOLEAN
%token BYTE SHORT CAMLINT INT LONG
%token FLOAT DOUBLE
%token CHAR STRING
%token TOP
%token ARRAY

%token LBRACKET RBRACKET
%token LBRACE RBRACE
%token LPAREN RPAREN
%token COMMA SEMI COLON DOT

%token INIT

%token <string> IDENT

%token EOF

%type <Idl.file> idl_file
%start idl_file

%%
idl_file :
| idl_file_list EOF { $1 }
| defs EOF { [make_package [] $1] }
  ;

idl_file_list :
| PACKAGE ident_list SEMI defs { [make_package $2 $4] }
| PACKAGE ident_list SEMI defs idl_file_list { (make_package $2 $4) :: $5 }
    ;
defs:
| def { [$1] }
| def defs { $1 :: $2 }
    ;

def:
| CLASS ident def_body { make_def $2 $3 }
| annotations CLASS ident def_body { make_def ~annotations:$1 $3 $4 }
| modifiers CLASS ident def_body { make_def  ~modifiers:$1 $3 $4 }
| annotations modifiers CLASS ident def_body { make_def ~modifiers:$2 ~annotations:$1 $4 $5 }
| CLASS ident super def_body { make_def $2 ~super:$3 $4 }
| annotations CLASS ident super def_body { make_def ~annotations:$1 $3 ~super:$4 $5 }
| modifiers CLASS ident super def_body { make_def  ~modifiers:$1 $3 ~super:$4 $5 }
| annotations modifiers CLASS ident super def_body { make_def ~modifiers:$2 ~annotations:$1 $4 ~super:$5 $6 }

| CLASS ident IMPLEMENTS interface_list def_body { make_def $2 ~implements:$4 $5 }
| annotations CLASS ident IMPLEMENTS interface_list def_body { make_def ~annotations:$1 $3 ~implements:$5 $6 }
| modifiers CLASS ident IMPLEMENTS interface_list def_body { make_def  ~modifiers:$1 $3 ~implements:$5 $6 }
| annotations modifiers CLASS ident IMPLEMENTS interface_list def_body { make_def ~modifiers:$2 ~annotations:$1 $4 ~implements:$6 $7 }
| CLASS ident super IMPLEMENTS interface_list def_body { make_def $2 ~super:$3 ~implements:$5 $6 }
| annotations CLASS ident super IMPLEMENTS interface_list def_body { make_def ~annotations:$1 $3 ~super:$4 ~implements:$6 $7 }
| modifiers CLASS ident super IMPLEMENTS interface_list def_body { make_def  ~modifiers:$1 $3 ~super:$4 ~implements:$6 $7 }
| annotations modifiers CLASS ident super IMPLEMENTS interface_list def_body { make_def ~modifiers:$2 ~annotations:$1 $4 ~super:$5 ~implements:$7 $8 }

| INTERFACE ident def_body { make_def ~interface:true $2 $3 }
| annotations INTERFACE ident def_body { make_def ~annotations:$1 ~interface:true $3 $4 }

| INTERFACE ident IMPLEMENTS interface_list def_body { make_def ~interface:true $2 ~implements:$4 $5 }
| annotations INTERFACE ident IMPLEMENTS interface_list def_body { make_def ~annotations:$1 ~interface:true $3 ~implements:$5 $6 }
    ;

super:
| EXTENDS qident { $2 }

def_body:
| LBRACE decls RBRACE { $2 }
| LBRACE RBRACE { [] }
    ;

decls:
| decl SEMI { [$1] }
| decl SEMI decls { $1 :: $3 }
    ;

decl:
| fields { $1 }
| methods { $1 }
| inits { $1 }
;

fields:
| typ ident { make_field $1 $2 }
| annotations typ ident { make_field ~annotations:$1 $2 $3 }
| modifiers typ ident { make_field ~modifiers:$1 $2 $3 }
| annotations modifiers typ ident { make_field ~modifiers:$2 ~annotations:$1 $3 $4 }

methods:
| typ ident args
      { make_method $1 $2 $3 }
| annotations typ ident args
      { make_method ~annotations:$1 $2 $3 $4 }
| modifiers typ ident args
      { make_method ~modifiers:$1 $2 $3 $4 }
| annotations modifiers typ ident args
      { make_method ~modifiers:$2 ~annotations:$1 $3 $4 $5 }

inits:
| annotations INIT args { make_init $1 $3 }
| INIT args { raise (Syntax_error (Einit_no_alias (Loc.get ()))) }
;

args:
| LPAREN RPAREN { [] }
| LPAREN arg_list RPAREN { $2 }
;

arg_list:
| arg { [$1] }
| arg COMMA arg_list { $1 :: $3 }
    ;

arg:
| typ { make_arg $1 }
| annotations typ { make_arg ~annotations:$1 $2}
| typ ident { make_arg ~ident:$2 $1 }
| annotations typ ident { make_arg ~annotations:$1 ~ident:$3 $2 }
;

typ:
| VOID { make_type Ivoid }
| BOOLEAN  { make_type Iboolean }
| BYTE  { make_type Ibyte }
| SHORT  { make_type Ishort }
| INT  { make_type Icamlint }
| LONG  { make_type Ilong }
| FLOAT  { make_type Ifloat }
| DOUBLE  { make_type Idouble }
| CHAR { make_type Ichar }
| STRING { make_type Istring }
| TOP { make_type Itop }
| typ LBRACKET RBRACKET { make_type (Iarray $1) }
| qident { make_type (Iobject $1) }
    ;

modifiers:
| modifier { [$1] }
| modifier modifiers { $1::$2 }

modifier:
| STATIC { make_modifiers Istatic }
| ABSTRACT { make_modifiers Iabstract }
| FINAL  { make_modifiers Ifinal }
  ;

annotations:
| LBRACKET ann_list RBRACKET { $2 }
;

ann_list:
| ann { [$1] }
| ann COMMA ann_list { $1 :: $3 }
;

ann:
| NAME ident { make_annotation (Iname $2) }
| CALLBACK { make_annotation Icallback }
| ARRAY { make_annotation (Icamlarray) }
    ;

ident:
| IDENT { make_ident $1 }
    ;

ident_list:
| IDENT { [$1] }
| ident_list DOT IDENT { $3 :: $1 } // la liste est construite à l'envers
    ;

qident:
| ident { make_qident [] $1 }
| ident_list DOT ident { make_qident $1 $3 }
;

interface_list:
| qident { [$1] }
| qident COMMA interface_list { $1 :: $3 }
;
