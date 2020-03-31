(* Original file: electrumAnalyzer.0.3.5-2/electrumAnalyzer-0.3.5-2/src/parsing/parser_expr.mly *)
/*******************************************************************************
 * Time-stamp: <2015-10-16 CEST 10:30:34 David Chemouil>>
 * 
 * Electrum Analyzer 
 * Copyright (C) 2014-2015 Onera
 * Authors: 
 *   David Chemouil 
 * 
 * This file is part of the Electrum Analyzer.
 * 
 * The Electrum Analyzer is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * The Electrum Analyzer is distributed in the hope that it will be
 * useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the Electrum Analyzer.  If not, see
 * <http://www.gnu.org/licenses/>.
 ******************************************************************************/

%{
open Ast.Expr 

let split l =
  let rec split firsts = function
    | [] -> assert false
    | [x] -> (List.rev firsts, x)
    | hd::tl -> split (hd :: firsts) tl
  in split [] l
%}

%token ALL SET SOME ONE LONE NO ARROW AT SLASH 

%token <Ast.Ident.t> IDENT

(* %token <Ast.Qname.t> qname *)

%token <int> NUMBER

%token EOF LBRACE RBRACE LBRACKET RBRACKET BAR LPAREN RPAREN
%token LET QALL QSOME QNO QLONE QONE
%token MSOME MSET MLONE MONE
%token EQ DISJ COLON COMMA THIS
%token OR IFF IMPLIES ELSE AND NOT LT LTE GT GTE NEQ IN
%token MINUS PLUS CARD OVERRIDE INTER
%token LPROJ RPROJ DOT TILDE CARET STAR
%token UNIV IDEN NONE INT PRIME VAR
%token NEXT ALWAYS EVENTUALLY UNTIL
%token PREVIOUS HISTORICALLY ONCE SINCE

%token ONE_ARROW_ONE ONE_ARROW_LONE ONE_ARROW_SET ONE_ARROW_SOME
%token LONE_ARROW_ONE LONE_ARROW_LONE LONE_ARROW_SET LONE_ARROW_SOME  
%token SOME_ARROW_ONE SOME_ARROW_LONE SOME_ARROW_SET SOME_ARROW_SOME  
%token SET_ARROW_ONE SET_ARROW_LONE SET_ARROW_SET SET_ARROW_SOME

(* ascending precedence *)
%nonassoc BAR 
%left OR
%left IFF
%right implies_as_op
%nonassoc IMPLIES
%nonassoc ELSE
%left AND
%left UNTIL SINCE
%nonassoc NOT NEXT ALWAYS EVENTUALLY PREVIOUS HISTORICALLY ONCE
%nonassoc LT LTE GT GTE EQ NEQ IN
%nonassoc MSET MONE MLONE MSOME (* for multiplicities in e.g. '[:/in] set E' *)
%nonassoc NO SOME LONE ONE      (* for formulas as 'some E' (= E != none) *)
%left MINUS PLUS
%nonassoc CARD
%left OVERRIDE
%left INTER
%right ONE_ARROW_ONE ONE_ARROW_LONE ONE_ARROW_SET ONE_ARROW_SOME
  LONE_ARROW_ONE LONE_ARROW_LONE LONE_ARROW_SET LONE_ARROW_SOME  
  SOME_ARROW_ONE SOME_ARROW_LONE SOME_ARROW_SET SOME_ARROW_SOME  
  SET_ARROW_ONE SET_ARROW_LONE SET_ARROW_SET SET_ARROW_SOME
%left LPROJ
%left RPROJ
%left LBRACKET                  (* for function or predicate application *)
%left DOT
%nonassoc TILDE CARET STAR
%nonassoc PRIME

%%

(* ******************** *)
(* GRAMMAR              *)
(* ******************** *)

    
%public expr :
 e = prim_expr
 { make_expr (Location.make $startpos $endpos) e }
 | e = parens(expr)
 { e }

prim_expr:
 THIS
 { EThis }
 | AT n = IDENT
 { EAtName n }
 | q = qname
 { EQualName q }
 | b = block
 { EBlock b }
 | n = NUMBER
 { EConst (CNum n) }
 | MINUS
 n = NUMBER 
 { EConst (CNum (- n))  } 
 | UNIV
 { EConst CUniv }
 | IDEN
 { EConst CIden }
 | NONE
 { EConst CNone }
 | INT
 { EConst CInt }
 | LET
 lds = comma_sep1(let_decl)
 block = block_or_bar
 { ELet (lds, block) }
 | quant = quantifier
 ds = comma_sep1(decl)
 block = block_or_bar 
 { EQuant (quant, ds, block) }
 | cdt = expr
 IMPLIES
 then_ = expr %prec implies_as_op
 { EBinary (cdt, BImp, then_) }
 | cdt = expr
 IMPLIES
 then_ = expr
 ELSE
 else_ = expr
 { EIte (cdt, then_, else_) }
 | e1 = expr
 op = bin_op
 e2 = expr
 { EBinary (e1, op, e2) }
 | e1 = expr
 neg = iboption(NOT)
 op = comparator
 e2 = expr
 { EComp (e1, (if neg then negate op else op), e2) }
 | op = un_op
 e = expr
 { EUnary (op, e) }
 | e = expr
 PRIME
 { EUnary (UPrime, e) }        
 | e1 = expr
 cards = arrow
 e2 = expr
 { let (left, right) = cards in
   ECart (e1, left, right, e2) }
 | fct = expr
 args = brackets(comma_sep1(expr))
 { EApp (fct, args) }
 | body = braces(pair(comma_sep1(decl), block_or_bar))
 { ECompr (fst body, snd body) }

 
%inline quantifier :
 QALL { `All }
 | QSOME { `Some }
 | QNO { `No }
 | QLONE { `Lone }
 | QONE { `One }

block_or_bar:
 b = block
 { b }
 | BAR
 e = expr
 { [e] }

%public block:
 exprs = braces(expr*)
 { exprs }

let_decl:
 name = IDENT
 EQ
 expr = expr
 { Ast.Expr.{ name; expr } }
  
%public decl:
 is_variable = iboption(VAR)
 is_disjoint_variables = iboption(DISJ)
 names = comma_sep1(IDENT)
 COLON
 is_disjoint_ranges = iboption(DISJ)
 range = expr
 { Ast.Expr.make_decl ~is_variable ~is_disjoint_variables
     ~names ~is_disjoint_ranges ~range }


%inline bin_op:
 OR
 { BOr }
 | IFF
 { BIff}
 | AND
 { BAnd }
 | MINUS
 { BMinus }
 | PLUS
 { BUnion }
 | OVERRIDE
 { BOver }
 | INTER
 { BInter }
 | LPROJ
 { BLProj }
 | RPROJ
 { BRProj }
 | DOT
 { BDot }
 | UNTIL
 { BUntil }
 | SINCE
 { BSince }
 
%inline un_op:
 NOT
 { UNot }
 | SOME
 { UQual `Some }
 | NO
 { UQual `No }
 | LONE
 { UQual `Lone }
 | ONE
 { UQual `One }
 | MSET
 { UMult `Set }
 | MSOME
 { UMult `Some }
 | MLONE
 { UMult `Lone }
 | MONE
 { UMult `One }
 | CARD
 { UCard }
 | TILDE
 { UTilde }
 | CARET
 { UHat }
 | STAR
 { UStar }
 | NEXT
 { UNext }
 | ALWAYS
 { UAlways }
 | EVENTUALLY
 { UEventually }
 | ONCE
 { UOnce }
 | HISTORICALLY
 { UHist }
 | PREVIOUS
 { UPrevious }

%inline comparator:
 IN
 { CIn }
 | GT
 { CGt }
 | GTE
 { CGte }
 | LT
 { CLt }
 | LTE
 { CLte }
 | NEQ
 { CNeq }
 | EQ
 { CEq }


%inline arrow:
 ONE_ARROW_ONE { (`One, `One) }
 | ONE_ARROW_LONE  { (`One, `Lone) }
 | ONE_ARROW_SET  { (`One, `Set) }
 | ONE_ARROW_SOME  { (`One, `Some) }
 | LONE_ARROW_ONE  { (`Lone, `One) }
 | LONE_ARROW_LONE  { (`Lone, `Lone) }
 | LONE_ARROW_SET  { (`Lone, `Set) }
 | LONE_ARROW_SOME  { (`Lone, `Some) }
 | SOME_ARROW_ONE  { (`Some, `One) }
 | SOME_ARROW_LONE  { (`Some, `Lone) }
 | SOME_ARROW_SET  { (`Some, `Set) }
 | SOME_ARROW_SOME  { (`Some, `Some) }
 | SET_ARROW_ONE  { (`Set, `One) }
 | SET_ARROW_LONE  { (`Set, `Lone) }
 | SET_ARROW_SET  { (`Set, `Set) }
 | SET_ARROW_SOME  { (`Set, `Some) }

%public qname:
 this = iboption(pair(THIS, SLASH))
 ids = separated_nonempty_list(SLASH, IDENT)
 { let path, name = split ids in
   Ast.Qname.make ~this ~path ~name }
