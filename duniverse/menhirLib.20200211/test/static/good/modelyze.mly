/*
Modelyze toolchain
Copyright (C) 2010-2012 David Broman

Modelyze toolchain is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Modelyze toolchain is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Modelyze toolchain.  If not, see <http://www.gnu.org/licenses/>.
*/

%{

  open Ustring.Op
  open Utils
  open Ast
  open Info


  let mktminfo t1 t2 = mkinfo (tm_info t1) (tm_info t2)

  let mkpatinfo t1 t2 = mkinfo (pat_info t1) (pat_info t2)

  let rec metastr n = if n = 0 then us"" else us"#" ^. metastr (n-1)

  let mk_binop fi l op t1 t2 =
    TmApp(fi,l,TmApp(fi,l,(TmVar(fi,Symtbl.add (us op),0)),t1,false),t2,false)
  let mk_unop fi l op t1 =
    TmApp(fi,l,(TmVar(fi,Symtbl.add (us op),0)),t1,false)

  let mk_binpat_op fi l op t1 t2 =
    PatSymApp(fi,PatSymApp(fi,PatExpr(fi,TmVar(fi,Symtbl.add (us op),0)),t1),t2)

  let mk_unpat_op fi l op t1 =
    PatSymApp(fi,PatExpr(fi,TmVar(fi,Symtbl.add (us op),0)),t1)

  let wildcard = Symtbl.add (us"@@wildcard")

  let floatval v = TmConst(NoInfo,0,ConstReal(v))


%}

/* Misc tokens */
%token EOF
%token <int Ast.tokendata> UINT
%token <float Ast.tokendata> UFLOAT
%token <int Ast.tokendata> IDENT
%token <int Ast.tokendata> IDENTPAREN
%token <Ast.primitive Ast.tokendata> PRIMITIVE
%token <Ustring.ustring Ast.tokendata> STRING
%token <unit Ast.tokendata> METAAPP

/* Keywords */
%token <unit Ast.tokendata> FUN
%token <unit Ast.tokendata> DEF
%token <unit Ast.tokendata> IN
%token <unit Ast.tokendata> IF
%token <unit Ast.tokendata> THEN
%token <unit Ast.tokendata> ELSE
%token <unit Ast.tokendata> TRUE
%token <unit Ast.tokendata> FALSE
%token <unit Ast.tokendata> INT
%token <unit Ast.tokendata> REAL
%token <unit Ast.tokendata> BOOL
%token <unit Ast.tokendata> TYSTRING
%token <unit Ast.tokendata> DPRINT
%token <unit Ast.tokendata> DPRINTTYPE
%token <unit Ast.tokendata> SYMSTR
%token <unit Ast.tokendata> LCASE
%token <unit Ast.tokendata> LIFT
%token <unit Ast.tokendata> OF
%token <unit Ast.tokendata> DECON
%token <unit Ast.tokendata> WITH
%token <unit Ast.tokendata> SYM
%token <unit Ast.tokendata> PROJ
%token <unit Ast.tokendata> FST
%token <unit Ast.tokendata> SND
%token <unit Ast.tokendata> IFGUARD
%token <unit Ast.tokendata> IFTHEN
%token <unit Ast.tokendata> IFELSE
%token <unit Ast.tokendata> ERROR
%token <unit Ast.tokendata> MATCH
%token <unit Ast.tokendata> FROM
%token <unit Ast.tokendata> TYPE
%token <unit Ast.tokendata> ARRAY
%token <unit Ast.tokendata> MAP
%token <unit Ast.tokendata> LIST
%token <unit Ast.tokendata> SET
%token <unit Ast.tokendata> DAESOLVER
%token <unit Ast.tokendata> INCLUDE
%token <unit Ast.tokendata> BEGIN
%token <unit Ast.tokendata> END
%token <unit Ast.tokendata> SPECIALIZE

/* Operators */
%token <unit Ast.tokendata> EQ            /* "="  */
%token <unit Ast.tokendata> APXEQ         /* "~=" */
%token <unit Ast.tokendata> LEFTARROW     /* "<-"  */
%token <unit Ast.tokendata> MOD           /* "mod"*/
%token <unit Ast.tokendata> ADD           /* "+"  */
%token <unit Ast.tokendata> SUB           /* "-"  */
%token <unit Ast.tokendata> MUL           /* "*"  */
%token <unit Ast.tokendata> DIV           /* "/"  */
%token <unit Ast.tokendata> LESS          /* "<"  */
%token <unit Ast.tokendata> LESSEQUAL     /* "<=" */
%token <unit Ast.tokendata> GREAT         /* ">"  */
%token <unit Ast.tokendata> GREATEQUAL    /* ">=" */
%token <unit Ast.tokendata> EQUAL         /* "==" */
%token <unit Ast.tokendata> NOTEQUAL      /* "!=" */
%token <unit Ast.tokendata> DOTADD        /* "+." */
%token <unit Ast.tokendata> DOTSUB        /* "-." */
%token <unit Ast.tokendata> DOTMUL        /* "*." */
%token <unit Ast.tokendata> DOTDIV        /* "/." */
%token <unit Ast.tokendata> DOTLESS       /* "<." */
%token <unit Ast.tokendata> DOTLESSEQUAL  /* "<=."*/
%token <unit Ast.tokendata> DOTGREAT      /* ">." */
%token <unit Ast.tokendata> DOTGREATEQUAL /* ">=."*/
%token <unit Ast.tokendata> DOTEQUAL      /* "==."*/
%token <unit Ast.tokendata> DOTNOTEQUAL   /* "!=."*/
%token <unit Ast.tokendata> NOT           /* "!"  */
%token <unit Ast.tokendata> AND           /* "&&" */
%token <unit Ast.tokendata> OR            /* "||" */
%token <unit Ast.tokendata> SEMI          /* ";"  */
%token <unit Ast.tokendata> PLUSPLUS      /* "++"  */
%token <unit Ast.tokendata> EXP           /* "^"  */
%token <unit Ast.tokendata> DOTEXP        /* "^."  */

/* Symbolic Tokens */
%token <unit Ast.tokendata> LPAREN        /* "("  */
%token <unit Ast.tokendata> RPAREN        /* ")"  */
%token <unit Ast.tokendata> LSQUARE       /* "["  */
%token <unit Ast.tokendata> RSQUARE       /* "]"  */
%token <unit Ast.tokendata> LCURLY        /* "{"  */
%token <unit Ast.tokendata> RCURLY        /* "}"  */
%token <unit Ast.tokendata> CONS          /* "::" */
%token <unit Ast.tokendata> COLON         /* ":"  */
%token <unit Ast.tokendata> COMMA         /* ","  */
%token <unit Ast.tokendata> DOT           /* "."  */
%token <unit Ast.tokendata> BAR           /* "|"  */
%token <unit Ast.tokendata> LONGARROW     /* "-->" */
%token <unit Ast.tokendata> ARROW         /* "->" */
%token <unit Ast.tokendata> DARROW        /* "=>" */
%token <unit Ast.tokendata> POLYEQUAL     /* "<==>" */
%token <unit Ast.tokendata> USCORE        /* "_"  */
%token <unit Ast.tokendata> ESCAPE        /* "~"  */
%token <unit Ast.tokendata> SQUOTE        /* "'"  */
%token <unit Ast.tokendata> PARENAPP      /* ")("  */
%token <unit Ast.tokendata> EQSEMI        /* ";;"  */
%token <unit Ast.tokendata> QUESTIONMARK  /* "?"  */


%start main
%type <Ast.top list> main

%nonassoc WITH
%nonassoc BAR
%nonassoc DEFUK
%left SEMI EQSEMI /*prec 1*/
%nonassoc MYAPP
%left OR  /*prec 2*/
%left AND  /*prec 3*/
%left EQ APXEQ LEFTARROW PLUSPLUS /*prec 5*/
%left LESS LESSEQUAL GREAT GREATEQUAL POLYEQUAL EQUAL NOTEQUAL LONGARROW /*prec 6*/
%left DOTLESS DOTLESSEQUAL DOTGREAT DOTGREATEQUAL DOTEQUAL DOTNOTEQUAL /*prec 7*/
%nonassoc NOT /*prec8 */
%left ADD SUB DOTADD DOTSUB /*prec 8*/
%left MUL DIV DOTMUL DOTDIV /*prec 9*/
%left MOD /*prec 10*/
%left EXP DOTEXP /*prec 11*/
%nonassoc UNARYMINUS /*prec 12*/
%left SQUOTE /*prec 13*/

%%




main:
  | top
      { $1 }

top:
  | EOF
      { [] }
  | DEF identparen parenparamlist EQ term top
      { let fi = mkinfo $1.i (tm_info $5) in
        let (plst,endty) = $3 in
        TopLet(fi,$2,endty,List.rev plst,$5,freein_tm $2 $5)::$6 }
  | DEF letpat EQ term top
      { let fi = mkinfo $1.i (tm_info $4) in
        TopLet(fi,$2,None,[],$4,freein_tm $2 $4)::$5 }
  | DEF letpat COLON ty EQ term top
      { let fi = mkinfo $1.i (tm_info $6) in
        TopLet(fi,$2,Some $4,[],$6,freein_tm $2 $6)::$7 }
  | DEF letpat COLON ty top %prec DEFUK
      { let fi = mkinfo $1.i (ty_info $4) in
        TopNu(fi,$2,$4)::$5 }
  | DEF IDENT COMMA revidentseq COLON ty top %prec DEFUK
      { let fi = mkinfo $1.i (ty_info $6) in
        let nulst = List.map (fun x -> TopNu(fi,x,$6)) (List.rev $4) in
        TopNu(fi,$2.v,$6)::(List.append nulst $7) }
  | TYPE IDENT top
      { let fi = mkinfo $1.i $2.i in
        TopNewType(fi,$2.v)::$3 }
  | TYPE IDENT EQ ty top
      { let fi = mkinfo $1.i (ty_info $4) in
        TopNameType(fi,$2.v,$4)::$5 }
  | INCLUDE IDENT top
      { let fi = mkinfo $1.i $2.i in
        let modname = $2.v |> Symtbl.get |> Ustring.to_latin1
                      |> String.lowercase |> us in
        TopInclude(fi,Symtbl.add (modname ^. us".moz"))::$3 }



identparen:
  | IDENT LPAREN
    { $1.v }
  | IDENTPAREN
    { $1.v }

ty:
  | tyarrow
      { $1 }
  | tyarrow DARROW ty
      { let fi = mkinfo (ty_info $1) (ty_info $3) in
	TyMap(fi,$2.l,$1,$3) }

tyarrow:
  | tyatom
      { $1 }
  | tyatom ARROW tyarrow
      { let fi = mkinfo (ty_info $1) (ty_info $3) in
	TyArrow(fi,$2.l,$1,$3) }

tyatom:
  | IDENT
      { TyIdent($1.i,$1.l,$1.v) }
  | INT
      { TyInt($1.i,$1.l) }
  | REAL
      { TyReal($1.i,$1.l) }
  | BOOL
      { TyBool($1.i,$1.l) }
  | TYSTRING
      { TyString($1.i,$1.l) }
  | LPAREN RPAREN
      { TyUnit(mkinfo $1.i $2.i,$1.l) }
  | LSQUARE ty RSQUARE
      { TyList(mkinfo $1.i $3.i,$1.l,$2) }
  | LIST tyatom
      { TyList(mkinfo $1.i (ty_info $2),$1.l,$2) }
  | LCURLY ty RCURLY
      { TyArray(mkinfo $1.i $3.i,$1.l,$2) }
  | ARRAY tyatom
      { TyArray(mkinfo $1.i (ty_info $2),$1.l,$2) }
  | LPAREN revtypetupleseq RPAREN
      { let fi = mkinfo $1.i $3.i in
        match $2 with
	  | [ty] -> ty
	  | tys ->  TyTuple(fi,$1.l,List.rev tys) }
  | LESS GREAT
      { TySym(mkinfo $1.i $2.i,$1.l,TyDyn(mkinfo $1.i $2.i,$1.l))}
  | LESS ty GREAT
      { TySym(mkinfo $1.i $3.i,$1.l,$2) }
  | QUESTIONMARK
      { TyDyn($1.i, $1.l) }
  | MAP tyatom tyatom
      { TyMap(mkinfo $1.i (ty_info $3),$1.l,$2,$3) }
  | SET tyatom
      { TySet(mkinfo $1.i (ty_info $2),$1.l,$2) }
  | DAESOLVER
      { TyDAESolver($1.i,$1.l) }

revtypetupleseq:
    |   ty
        {[$1]}
    |   revtypetupleseq COMMA ty
        {$3::$1}

deconpat:
  | SYM COLON ty
      { let fi = mkinfo $1.i (ty_info $3) in
        MPatSym(fi,TySym($1.i, 0, $3)) }
  | IDENT IDENT
      { MPatSymApp(mkinfo $1.i $2.i,$1.v,$2.v) }
  | LIFT IDENT COLON ty
      { let fi = mkinfo $1.i (ty_info $4) in
        MPatLift(fi,$2.v,$4) }

letpat:
  | IDENT
      { $1.v }
  | USCORE
      { wildcard }




term:
  | semi_op
      { $1 }
  | FUN IDENT COLON tyatom ARROW term
      { let fi = mkinfo $1.i (tm_info $6) in
        TmLam(fi,$1.l,$2.v,$4,$6) }
  | DEF identparen parenparamlist EQ cons SEMI term
      { let fi = mkinfo $1.i (tm_info $7) in
        let (plst,endty) = $3 in
        TmLet(fi,$1.l,$2,endty,List.rev plst,$5,$7,freein_tm $2 $5) }
  | DEF pat_atom EQ cons SEMI term
      { let fi = mkinfo $1.i (tm_info $6) in
        TmMatch(fi,$1.l,$4,[PCase(fi,[Ast.no_auto_esc $2],None,[],$6)]) }
  | DEF letpat COLON ty EQ cons SEMI term
      { let fi = mkinfo $1.i (tm_info $8) in
        TmLet(fi,$1.l,$2,Some $4,[],$6,$8,freein_tm $2 $6) }
  | DEF letpat COLON ty SEMI term
      { let fi = mkinfo $1.i (ty_info $4) in
        TmNu(fi,$1.l,$2,$4,$6) }
  | DEF IDENT COMMA revidentseq COLON ty SEMI term   /* A bug here */
      { let fi = mkinfo $1.i (ty_info $6) in
        List.fold_left (fun a x -> TmNu(fi,$1.l,x,$6,a)) $8
                       ($2.v::(List.rev $4)) }
  | IF term THEN term ELSE term
      { let fi = mkinfo $1.i (tm_info $6) in
        TmIf(fi,$1.l,$2,$4,$6) }
  | LCASE term OF BAR IDENT CONS IDENT ARROW term BAR LSQUARE RSQUARE ARROW term
      { let fi = mkinfo $1.i (tm_info $14) in
        TmLcase(fi,$1.l,$2,$5.v,$7.v,$9,$14) }
  | DECON term WITH deconpat THEN term ELSE term
      { let fi = mkinfo $1.i (tm_info $8) in
	TmCase(fi,$1.l,$2,$4,$6,$8) }
  | PROJ UINT FROM term
      { let fi = mkinfo $1.i (tm_info $4) in
        TmProj(fi,$1.l,$2.v,$4) }
  | MATCH term WITH matchcases
     { let fi = mkinfo $1.i $3.i in
       TmMatch(fi,$1.l,$2,List.rev $4) }
  | ARRAY DOT IDENT atom_list_rev
     { let fi = mkinfo $1.i (tm_info (List.hd $4)) in
       let op = mk_arrayop $3.i $3.v in
       TmArrayOp(fi,$1.l,op,List.rev $4) }
  | MAP DOT IDENT op_atom_list_rev
     { let fi = if $4 = [] then mkinfo $1.i $3.i
                else mkinfo $1.i (tm_info (List.hd $4)) in
       let op = mk_mapop $3.i $3.v in
       TmMapOp(fi,$1.l,op,List.rev $4) }
  | SET DOT IDENT op_atom_list_rev
     { let fi = if $4 = [] then mkinfo $1.i $3.i
                else mkinfo $1.i (tm_info (List.hd $4)) in
       let op = mk_setop $3.i $3.v in
       TmSetOp(fi,$1.l,op,List.rev $4) }
  | DAESOLVER DOT IDENT op_atom_list_rev
     { let fi = if $4 = [] then mkinfo $1.i $3.i
                else mkinfo $1.i (tm_info (List.hd $4)) in
       let op = mk_daesolverop $3.i $3.v in
       TmDAESolverOp(fi,$1.l,op,List.rev $4) }


op_atom_list_rev:
     { [] }
  | atom_list_rev
     { $1 }

atom_list_rev:
  | atom
      { [$1] }
  | atom_list_rev atom
      { $2::$1 }



matchcases:
  | BAR pattern op_guard ARROW term
      { let fi = mkinfo $1.i (tm_info $5) in
	[PCase(fi,[$2],$3,[],$5)] }
  | matchcases BAR pattern op_guard ARROW term
      { let fi = mkinfo $2.i (tm_info $6) in
	(PCase(fi,[$3],$4,[],$6))::$1 }

op_guard:
      { None }
  | IF term
      { Some $2 }

pattern:
  | pat_cons
      { $1 }

pat_cons:
  | pat_op
      { $1 }
  | pat_op CONS pat_cons
      { let fi = mkinfo (pat_info $1) (pat_info $3) in
	PatCons(fi,$1,$3) }


pat_op:
  | pat_left
      { $1 }
  | pat_op EQ pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(=)" $1 $3 }
  | pat_op APXEQ pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(~=)" $1 $3 }
  | pat_op MOD pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(mod)" $1 $3 }
  | pat_op ADD pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(+)" $1 $3 }
  | pat_op SUB pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(-)" $1 $3 }
  | pat_op MUL pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(*)" $1 $3 }
  | pat_op DIV pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(/)" $1 $3 }
  | pat_op LESS pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(<)" $1 $3 }
  | pat_op LESSEQUAL pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(<=)" $1 $3 }
  | pat_op GREAT pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(>)" $1 $3 }
  | pat_op GREATEQUAL pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(>=)" $1 $3 }
  | pat_op EQUAL pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(==)" $1 $3 }
  | pat_op NOTEQUAL pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(!=)" $1 $3 }
  | pat_op DOTADD pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(+.)" $1 $3 }
  | pat_op DOTSUB pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(-.)" $1 $3 }
  | pat_op DOTMUL pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(*.)" $1 $3 }
  | pat_op DOTDIV pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(/.)" $1 $3 }
  | pat_op DOTLESS pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(<.)" $1 $3 }
  | pat_op DOTLESSEQUAL pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(<=.)" $1 $3 }
  | pat_op DOTGREAT pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(>.)" $1 $3 }
  | pat_op DOTGREATEQUAL pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(>=.)" $1 $3 }
  | pat_op DOTEQUAL pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(==.)" $1 $3 }
  | pat_op DOTNOTEQUAL pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(!=.)" $1 $3 }
  | NOT pat_op
      { mk_unpat_op (mkinfo $1.i (pat_info $2)) $1.l "(!)" $2 }
  | pat_op AND pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(&&)" $1 $3 }
  | pat_op OR pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(||)" $1 $3 }
  | pat_op SEMI pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(;)" $1 $3 }
  | pat_op EQSEMI pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(;;)" $1 $3 }
  | pat_op PLUSPLUS pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(++)" $1 $3 }
  | pat_op EXP pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(^)" $1 $3 }
  | pat_op DOTEXP pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(^.)" $1 $3 }
  | pat_op LONGARROW pat_op
      { mk_binpat_op (mkpatinfo $1 $3) $2.l "(-->)" $1 $3 }

  | pat_op SQUOTE
      { mk_unpat_op (mkinfo (pat_info $1) $2.i) $2.l "(')" $1 }
  | SUB pat_op %prec UNARYMINUS
      { mk_unpat_op (mkinfo $1.i (pat_info $2)) $1.l "(--)" $2 }
  | DOTSUB pat_op %prec UNARYMINUS
      { mk_unpat_op (mkinfo $1.i (pat_info $2)) $1.l "(--.)" $2 }


pat_left:
  | pat_atom
      { $1 }
  | pat_left pat_atom
      { let fi = mkinfo (pat_info $1) (pat_info $2) in
	PatSymApp(fi,$1,$2) }
  | LIFT IDENT COLON tyatom
      { let fi = mkinfo $1.i (ty_info $4) in
        PatLift(fi,$2.v,$4) }

pat_atom:
  | IDENT
      { PatVar($1.i,$1.v,true) }
  | TRUE
      { PatExpr($1.i,TmConst($1.i,$1.l,ConstBool(true))) }
  | FALSE
      { PatExpr($1.i,TmConst($1.i,$1.l,ConstBool(false))) }
  | UINT
      { PatExpr($1.i,TmConst($1.i,$1.l,ConstInt($1.v))) }
  | UFLOAT
      { PatExpr($1.i,TmConst($1.i,$1.l,ConstReal($1.v))) }
  | STRING
      { PatExpr($1.i,TmConst($1.i,$1.l,ConstString($1.v))) }
  | LPAREN RPAREN
      { PatExpr(mkinfo $1.i $2.i,TmConst($1.i,$1.l,ConstUnit)) }
  | ESCAPE atom
      { PatExpr(mkinfo $1.i (tm_info $2),$2) }
  | LSQUARE RSQUARE
      { let fi = mkinfo $1.i $2.i in
	PatNil(fi) }
  | LSQUARE revpatseq RSQUARE
      { let fi = mkinfo $1.i $3.i in
        List.fold_right (fun p a -> PatCons(fi,p,a))
          (List.rev $2) (PatNil(fi)) }
  | SYM COLON tyatom
      { PatSym(mkinfo $1.i (ty_info $3),TySym(ty_info $3, 0, $3)) }
  | LPAREN revpatseq RPAREN
      { let fi = mkinfo $1.i $3.i in
        match $2 with
	  | [] -> assert false
	  | [p] -> p
	  | ps -> PatTuple(fi,List.rev ps) }
  | USCORE
      { PatWildcard($1.i) }


revpatseq:
    |   pattern
        {[$1]}
    |   revpatseq COMMA pattern
        {$3::$1}

parenparamlist:
  | revtmtyseq RPAREN rettype
      { ($1,$3) }
  | RPAREN
      { ([wildcard,TyUnit($1.i,$1.l)],None) }

rettype:
    { None }
  | COLON tyatom
    { Some($2) }
  | ARROW tyatom
    { Some($2) }

revtmtyseq:
    |   param
        {[$1]}
    |   revtmtyseq COMMA param
        {$3::$1}

param:
  |  IDENT COLON tyatom
      { ($1.v,$3) }

semi_op:
  | cons
      { $1 }
  | cons SEMI semi_op
      { mk_binop (mktminfo $1 $3) $2.l "(;)" $1 $3 }

cons:
  | op
      { $1 }
  | op CONS cons
      { TmCons(mktminfo $1 $3,$2.l,$1,$3) }


op:
  | app_left
      { $1 }
  | op EQ op
      { mk_binop (mktminfo $1 $3) $2.l "(=)" $1 $3 }
  | op APXEQ op
      { mk_binop (mktminfo $1 $3) $2.l "(~=)" $1 $3 }
  | op LEFTARROW op
      { mk_binop (mktminfo $1 $3) $2.l "(<-)" $1 $3 }
  | op MOD op
      { mk_binop (mktminfo $1 $3) $2.l "(mod)" $1 $3 }
  | op ADD op
      { mk_binop (mktminfo $1 $3) $2.l "(+)" $1 $3 }
  | op SUB op
      { mk_binop (mktminfo $1 $3) $2.l "(-)" $1 $3 }
  | op MUL op
      { mk_binop (mktminfo $1 $3) $2.l "(*)" $1 $3 }
  | op DIV op
      { mk_binop (mktminfo $1 $3) $2.l "(/)" $1 $3 }
  | op LESS op
      { mk_binop (mktminfo $1 $3) $2.l "(<)" $1 $3 }
  | op LESSEQUAL op
      { mk_binop (mktminfo $1 $3) $2.l "(<=)" $1 $3 }
  | op GREAT op
      { mk_binop (mktminfo $1 $3) $2.l "(>)" $1 $3 }
  | op GREATEQUAL op
      { mk_binop (mktminfo $1 $3) $2.l "(>=)" $1 $3 }
  | op EQUAL op
      { mk_binop (mktminfo $1 $3) $2.l "(==)" $1 $3 }
  | op NOTEQUAL op
      { mk_binop (mktminfo $1 $3) $2.l "(!=)" $1 $3 }
  | op DOTADD op
      { mk_binop (mktminfo $1 $3) $2.l "(+.)" $1 $3 }
  | op DOTSUB op
      { mk_binop (mktminfo $1 $3) $2.l "(-.)" $1 $3 }
  | op DOTMUL op
      { mk_binop (mktminfo $1 $3) $2.l "(*.)" $1 $3 }
  | op DOTDIV op
      { mk_binop (mktminfo $1 $3) $2.l "(/.)" $1 $3 }
  | op DOTLESS op
      { mk_binop (mktminfo $1 $3) $2.l "(<.)" $1 $3 }
  | op DOTLESSEQUAL op
      { mk_binop (mktminfo $1 $3) $2.l "(<=.)" $1 $3 }
  | op DOTGREAT op
      { mk_binop (mktminfo $1 $3) $2.l "(>.)" $1 $3 }
  | op DOTGREATEQUAL op
      { mk_binop (mktminfo $1 $3) $2.l "(>=.)" $1 $3 }
  | op DOTEQUAL op
      { mk_binop (mktminfo $1 $3) $2.l "(==.)" $1 $3 }
  | op DOTNOTEQUAL op
      { mk_binop (mktminfo $1 $3) $2.l "(!=.)" $1 $3 }
  | NOT op
      { mk_unop (mkinfo $1.i (tm_info $2)) $1.l "(!)" $2 }
  | op AND op
      { mk_binop (mktminfo $1 $3) $2.l "(&&)" $1 $3 }
  | op OR op
      { mk_binop (mktminfo $1 $3) $2.l "(||)" $1 $3 }
  | op EQSEMI op
      { mk_binop (mktminfo $1 $3) $2.l "(;;)" $1 $3 }
  | op PLUSPLUS op
      { mk_binop (mktminfo $1 $3) $2.l "(++)" $1 $3 }
  | op EXP op
      { mk_binop (mktminfo $1 $3) $2.l "(^)" $1 $3 }
  | op DOTEXP op
      { mk_binop (mktminfo $1 $3) $2.l "(^.)" $1 $3 }
  | op LONGARROW op
      { mk_binop (mktminfo $1 $3) $2.l "(-->)" $1 $3 }

  | op SQUOTE
      { mk_unop (mkinfo (tm_info $1) $2.i) $2.l "(')" $1 }
  | SUB op %prec UNARYMINUS
      { mk_unop (mkinfo $1.i (tm_info $2)) $1.l "(--)" $2 }
  | DOTSUB op %prec UNARYMINUS
      { mk_unop (mkinfo $1.i (tm_info $2)) $1.l "(--.)" $2 }

  | op POLYEQUAL op
      { let fi = mktminfo $1 $3 in
        TmEqual(fi,$2.l,$1,$3) }





app_left:
  | atom
      { $1 }
  | IDENTPAREN RPAREN
      { let t1 = TmVar($1.i,$1.v,0) in
        let t2 = TmConst($2.i,0,ConstUnit) in
        TmApp(mkinfo $1.i $2.i,0,t1,t2,false) }
  | IDENTPAREN revtmseq RPAREN
      { let tm_ident = TmVar($1.i,$1.v,0) in
        let fi = mkinfo $1.i $3.i in
        let rec mkapps lst =
          match lst with
          | t::ts -> TmApp(fi,0,mkapps ts,t,false)
          | [] -> tm_ident
        in mkapps $2 }
  | LPAREN app_left PARENAPP RPAREN
      { let t2 = TmConst($4.i,0,ConstUnit) in
        TmApp(mkinfo $1.i $4.i,0,$2,t2,false) }
  | LPAREN app_left PARENAPP revtmseq RPAREN
      { let fi = mkinfo $1.i $5.i in
        let rec mkapps lst =
          match lst with
          | t::ts -> TmApp(fi,0,mkapps ts,t,false)
          | [] -> $2
        in mkapps $4 }
  | app_left app_right
      { let (l,t) = $2 in
        TmApp(mktminfo $1 t,l,$1,t,false) }
  | FST atom
      { let fi = mkinfo $1.i (tm_info $2) in
        TmProj(fi,$1.l,0,$2) }
  | SND atom
      { let fi = mkinfo $1.i (tm_info $2) in
        TmProj(fi,$1.l,1,$2) }
  | ERROR atom
      { let fi = mkinfo $1.i (tm_info $2) in
        TmError(fi,$1.l,$2) }
  | SPECIALIZE atom atom
      { TmApp(mktminfo $2 $3,0,$2,$3,true) }
  | SPECIALIZE IDENTPAREN atom RPAREN
      { let tm_ident = TmVar($2.i,$2.v,0) in
        TmApp(mkinfo $1.i $4.i,0,tm_ident,$3,true) }
  | SPECIALIZE LPAREN atom PARENAPP atom RPAREN
      { TmApp(mktminfo $3 $5,0,$3,$5,true) }

app_right:
  | atom
      { (0,$1) }
  | METAAPP atom
      { ($1.l,$2) }

atom:
  | IDENT
      { if $1.l = 0 then TmVar($1.i,$1.v,0)
        else  (TmVar($1.i,$1.v,0)) }
  | TRUE
      { TmConst($1.i,$1.l,ConstBool(true)) }
  | FALSE
      { TmConst($1.i,$1.l,ConstBool(false)) }
  | UINT
      { TmConst($1.i,$1.l,ConstInt($1.v)) }
  | UFLOAT
      { TmConst($1.i,$1.l,ConstReal($1.v)) }
  | STRING
      { TmConst($1.i,$1.l,ConstString($1.v)) }
  | PRIMITIVE
      { TmConst($1.i,$1.l,ConstPrim($1.v,[])) }
  | LSQUARE RSQUARE
      { let fi = mkinfo $1.i $2.i in
	TmNil(fi,$1.l,TyDyn(fi,$1.l)) }
  | LSQUARE revtmseq RSQUARE
      { let fi = mkinfo $1.i $3.i in
        TmList(fi,$1.l,$2) }
  | LCURLY term RCURLY
      { $2 }
  | LPAREN RPAREN
      { TmConst(mkinfo $1.i $2.i,$1.l,ConstUnit) }
  | LPAREN revtmseq RPAREN
      { let fi = mkinfo $1.i $3.i in
        match $2 with
	  | [] -> TmConst(fi,$1.l,ConstUnit)
	  | [t] -> if $1.l = 0 then t else  t
	  | ts ->  TmTuple(fi,$1.l,List.rev ts) }
  | BEGIN term END
      { $2 }
  | DPRINT LPAREN term RPAREN
      { TmDPrint($3) }
  | DPRINTTYPE LPAREN term RPAREN
      { TmDPrintType($3) }
  | SYMSTR LPAREN term RPAREN
      { TmSymStr(tm_info $3,$3) }




revidentseq:
    |   IDENT
        {[$1.v]}
    |   revidentseq COMMA IDENT
        {($3.v)::$1}


revtmseq:
    |   term
        {[$1]}
    |   revtmseq COMMA term
        {$3::$1}












