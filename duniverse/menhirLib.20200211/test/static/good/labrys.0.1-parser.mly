(* Original file: labrys.0.1/labrys-0.1/src/parsing/parser.mly *)
(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

%parameter < Filename : sig val get : string end >

%{
  let loc startpos endpos =
    let get_pos pos =
      { Location.pos_lnum = pos.Lexing.pos_lnum
      ; pos_cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
      }
    in
    { Location.loc_start = get_pos startpos
    ; loc_end = get_pos endpos
    ; filename = Filename.get
    }
%}

%token Import Library
%token Open
%token Let Equal In
%token Rec
%token Lambda
%token Dot
%token Comma
%token Hash
%token Arrow DoubleArrow
%token LArrowEff RArrowEff
%token LDoubleArrowEff RDoubleArrowEff
%token Forall
%token Match With End
%token Type
%token Alias
%token Pipe As
%token Colon
%token Star Eff Caret
%token Fail
%token Try
%token Exception
%token Class Instance
%token Foreign
%token Underscore
%token Semicolon
%token <string> LowerName
%token <string> UpperName
%token <string> Int
%token <string> Float
%token <char list> Char
%token <char list> String
%token LQMarkParen LParen RParen
%token LQMarkBracket LBracket RBracket
%token LBracketUp RBracketUp
%token LBrace RBrace
%token EOF

%start main
%type <(ParseTree.imports * ParseTree.top list)> main

%start mainInterface
%type <(ParseTree.imports * ParseTree.interface list)> mainInterface

%%

(********* Main functions *********)

main: entry(body) { $1 }

mainInterface: entry(bodyInterface) { $1 }


(********* Implementation *********)

body:
  | x = let_case
      { ParseTree.Value x }
  | typeAlias = typeAlias
      { ParseTree.Type typeAlias }
  | Foreign cname = String name = newLowerName Colon ty = typeExpr
      { ParseTree.Foreign (cname, name, ty) }
  | Type name = newUpperName k = kindopt
      { ParseTree.AbstractType (name, k) }
  | datatype = datatype
      { ParseTree.Datatype datatype }
  | Exception name = newUpperName args = exceptionArgs
      { ParseTree.Exception (name, args) }
  | Open modul = import_module
      { ParseTree.Open modul }
  | Class name = newUpperName params = kind_and_name+ Equal sigs = letSig+ End
      { ParseTree.Class (name, params, sigs) }
  | Instance name = instanceName x = tyclassInstance Equal values = let_case+ End
      { ParseTree.Instance (x, name, values) }

instanceName:
  | { None }
  | LBracket name = newLowerName RBracket
      { Some name }

exceptionArgs:
  | x = typeExprClosed xs = exceptionArgs
      { x :: xs }
  | { [] }

let_case:
  | Let r = is_rec name = newLowerName x = args(let_aux)
      { (name, r, x) }

let_aux:
  | ty = ty_opt(typeExpr) Equal t = term
      { (ty, t) }

%inline is_rec:
  | { ParseTree.NonRec }
  | Rec { ParseTree.Rec }

%inline ty_annot(ty):
  | Colon eff = ty Hash ty = ty
      { (ty, Some eff) }
  | Colon ty = ty
      { (ty, None) }

%inline ty_opt(ty):
  | { None }
  | ty = ty_annot(ty)
      { Some ty }

datatype:
  | Type name = newUpperName args = kind_and_name* Equal Pipe? variants = separated_nonempty_list(Pipe, variant)
      { (name, args, variants) }

typeAlias:
  | Type Alias name = newUpperName Equal ty = typeExpr
      { (name, ty) }

lambda_aux:
  | ty = ty_opt(typeExprProtectedPermissive) Arrow t = term
      { (ty, t) }

termStrictlyUnclosed:
  | Lambda args = nonempty_args(lambda_aux)
      { (loc $startpos $endpos, ParseTree.Abs args) }
  | x = let_case In xs = term
      { (loc $startpos $endpos, ParseTree.Let (x, xs)) }
  | x = termProtectedPermissive Semicolon y = term
      { (loc $startpos $endpos, ParseTree.Seq (x, y)) }
  | t = termProtectedPermissive ty = ty_annot(typeExpr)
      { (loc $startpos $endpos, ParseTree.Annot (t, ty)) }

termNonStrictlyUnclosed:
  | x = app
      { x }
  | Fail LBracket ty = typeExpr RBracket exn = termClosed
      { (loc $startpos $endpos, ParseTree.Fail (ty, exn)) }

termUnclosed:
  | x = termStrictlyUnclosed { x }
  | x = termNonStrictlyUnclosed { x }

termClosed:
  | name = lowerName
      { (loc $startpos $endpos, ParseTree.LowerVal name) }
  | name = upperName
      { (loc $startpos $endpos, ParseTree.UpperVal name) }
  | Match t = term With Pipe? p = separated_nonempty_list(Pipe, pattern) End
      { (loc $startpos $endpos, ParseTree.PatternMatching (t, p)) }
  | Try t = term With Pipe? p = separated_nonempty_list(Pipe, pattern) End
      { (loc $startpos $endpos, ParseTree.Try (t, p)) }
  | n = Int
      { (loc $startpos $endpos, ParseTree.Const (ParseTree.Int n)) }
  | n = Float
      { (loc $startpos $endpos, ParseTree.Const (ParseTree.Float n)) }
  | c = Char
      { (loc $startpos $endpos, ParseTree.Const (ParseTree.Char c)) }
  | s = String
      { (loc $startpos $endpos, ParseTree.Const (ParseTree.String s)) }
  | LParen x = term RParen
      { x }

term:
  | x = termUnclosed { x }
  | x = termClosed { x }

termProtectedPermissive:
  | x = termNonStrictlyUnclosed { x }
  | x = termClosed { x }

appAux:
  | f = termClosed { f }
  | f = app { f }

app:
  | f = appAux LBracket ty = typeExpr RBracket
      { (loc $startpos $endpos, ParseTree.TApp (f, ty)) }
  | f = appAux x = termClosed
      { (loc $startpos $endpos, ParseTree.App (f, x)) }
  | f = appAux LQMarkBracket tyclass = tyclassAppArg RBracket
      { (loc $startpos $endpos, ParseTree.TyClassApp (f, tyclass)) }

tyclassInstance:
  | name = upperName tys = typeExprClosed+
      { (name, tys) }

tyclassAppArg:
  | x = tyclassInstance { ParseTree.TyClassInstance x }
  | x = lowerName { ParseTree.TyClassVariable x }

arg:
  | LParen name = newLowerName Colon ty = typeExpr RParen
      { ParseTree.VArg (name, ty) }
  | ty = kind_and_name_in_value
      { ParseTree.TArg ty }
  | LParen RParen
      { ParseTree.Unit }
  | LQMarkParen name = newLowerName Colon tyclass = tyclass RParen
      { ParseTree.TyClassArg (name, tyclass) }

args(rest):
  | rest = rest
      { ([], rest) }
  | x = arg xs = args(rest)
      { let (xs, rest) = xs in
        ((loc $startpos $endpos, x) :: xs, rest)
      }

nonempty_args(rest):
  | x = arg xs = args(rest)
      { let (xs, rest) = xs in
        ((loc $startpos $endpos, x) :: xs, rest)
      }

tyclass:
  | name = upperName tyvars = kind_and_name_in_value* args = typeExprClosed+
      { (name, tyvars, args) }

typeExprStrictlyUnclosed:
  | param = typeExprProtectedPermissive Arrow ret = typeExpr
      { (loc $startpos $endpos, ParseTree.Fun (param, None, ret)) }
  | param = typeExprProtectedPermissive LArrowEff eff = eff RArrowEff ret = typeExpr
      { (loc $startpos $endpos, ParseTree.Fun (param, Some eff, ret)) }
  | Forall x = kind_and_name+ Comma ret = typeExpr
      { (loc $startpos $endpos, ParseTree.Forall (x, ret)) }
  | LBrace tyclass = tyclass RBrace DoubleArrow ty = typeExpr
      { (loc $startpos $endpos, ParseTree.TyClass (tyclass, None, ty)) }
  | LBrace tyclass = tyclass RBrace LDoubleArrowEff eff = eff RDoubleArrowEff ty = typeExpr
      { (loc $startpos $endpos, ParseTree.TyClass (tyclass, Some eff, ty)) }
  | Lambda x = kind_and_name+ Comma ret = typeExpr
      { (loc $startpos $endpos, ParseTree.AbsOnTy (x, ret)) }

typeExprNonStrictlyUnclosed:
  | x = tyApp
      { x }

typeExprUnclosed:
  | x = typeExprStrictlyUnclosed { x }
  | x = typeExprNonStrictlyUnclosed { x }

typeExprClosed:
  | name = upperName
      { (loc $startpos $endpos, ParseTree.Ty name) }
  | name = newLowerName
      { (loc $startpos $endpos, ParseTree.TyVar name) }
  | LBracket eff = eff RBracket
      { (loc $startpos(eff) $endpos(eff), ParseTree.Eff eff) }
  | LBracketUp sum = sum RBracketUp
      { (loc $startpos(sum) $endpos(sum), ParseTree.Sum sum) }
  | LParen x = typeExpr RParen
      { x }

typeExpr:
  | x = typeExprUnclosed { x }
  | x = typeExprClosed { x }

typeExprProtectedPermissive:
  | x = typeExprNonStrictlyUnclosed { x }
  | x = typeExprClosed { x }

tyAppAux:
  | f = typeExprClosed { f }
  | f = tyApp { f }

tyApp:
  | f = tyAppAux x = typeExprClosed
      { (loc $startpos $endpos, ParseTree.AppOnTy (f, x)) }

kindUnclosed:
  | k1 = kindClosed Arrow k2 = kind
      { ParseTree.KFun (k1, k2) }

kindClosed:
  | Star { ParseTree.KStar }
  | Eff { ParseTree.KEff }
  | Caret { ParseTree.KExn }
  | LParen x = kind RParen { x }

kind:
  | x = kindUnclosed { x }
  | x = kindClosed { x }

eff: eff = separated_list(Comma, typeExpr) { (loc $startpos $endpos, eff) }
sum: sum = separated_list(Pipe, typeExpr) { sum }

variant:
  | name = newUpperName tys = typeExprClosed*
      { (name, tys) }

kindopt:
  | { None }
  | Colon k = kind { Some k }

kind_and_name:
  | name = newLowerName
      { (name, None) }
  | LParen name = newLowerName Colon k = kind RParen
      { (name, Some k) }

kind_and_name_in_value:
  | LBrace name = newLowerName RBrace
      { (name, None) }
  | LBrace name = newLowerName Colon k = kind RBrace
      { (name, Some k) }

pattern:
  | p = pat Arrow t = term
      { (p, t) }

patClosed:
  | name = newLowerName
      { ParseTree.Any name }
  | name = upperName
      { ParseTree.TyConstr (loc $startpos $endpos, name, []) }
  | LParen p = pat RParen
      { p }

patNonStrictlyUnclosed:
  | name = upperName args = patClosed+
      { ParseTree.TyConstr (loc $startpos $endpos, name, args) }
  | p = patProtectedPermissive As name = newLowerName
      { ParseTree.As (p, name) }

patStrictlyUnclosed:
  | p1 = patProtectedPermissive Pipe p2 = pat
      { ParseTree.Or (p1, p2) }

patUnclosed:
  | p = patNonStrictlyUnclosed { p }
  | p = patStrictlyUnclosed { p }

patProtectedPermissive:
  | p = patClosed { p }
  | p = patNonStrictlyUnclosed { p }

pat:
  | p = patClosed { p }
  | p = patUnclosed { p }

letSig:
  | Let name = newLowerName Colon ty = typeExpr
      { (name, ty) }

(********* Names *********)

lowerName_aux:
  | name = LowerName
      { [name] }
  | m = UpperName Dot xs = lowerName_aux
      { m :: xs }

lowerName: x = lowerName_aux { (loc $startpos $endpos, `LowerName x) }

upperName_aux:
  | name = UpperName
      { [name] }
  | m = UpperName Dot xs = upperName_aux
      { m :: xs }

upperName: x = upperName_aux { (loc $startpos $endpos, `UpperName x) }

%inline newLowerName:
  | name = LowerName
      { (loc $startpos $endpos, `NewLowerName name) }
  | Underscore
      { (loc $startpos $endpos, `Underscore) }

newUpperName:
  | name = UpperName
      { (loc $startpos $endpos, `NewUpperName name) }


(********* Interface *********)

bodyInterface:
  | x = letSig
      { ParseTree.IVal x }
  | Type name = newUpperName k = kindopt
      { ParseTree.IAbstractType (name, k) }
  | datatype = datatype
      { ParseTree.IDatatype datatype }
  | typeAlias = typeAlias
      { ParseTree.ITypeAlias typeAlias }
  | Exception name = newUpperName args = exceptionArgs
      { ParseTree.IException (name, args) }
  | Open modul = import_module
      { ParseTree.IOpen modul }
  | Class name = newUpperName params = kind_and_name+ Equal sigs = letSig+ End
      { ParseTree.IClass (name, params, sigs) }
  | Instance name = instanceName x = tyclassInstance
      { ParseTree.IInstance (x, name) }


(********* Module utils *********)

import_module:
  | modul = upperName
      { ParseTree.Source modul }
  | Library modul = upperName
      { ParseTree.Library modul }

import: Import m = import_module { m }


(********* Functions ***********)

entry(body):
  | imports = import* body = body_list(body)
      { (imports, body) }

body_list(body):
  | EOF
      { [] }
  | x = body xs = body_list(body)
      { x :: xs }
