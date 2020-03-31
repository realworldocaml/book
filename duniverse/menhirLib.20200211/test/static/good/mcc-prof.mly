/*
 * Parser for Prof.
 * ----------------------------------------------------------------
 *
 * Copyright (C) Adam Granicz, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 */

%{
open Prof_parse_state
open Prof_type

let pos_of_single = function
     VarExpr (_, pos)
   | NumExpr (_, pos)
   | PlusExpr (_, _, pos)
   | MinusExpr (_, _, pos)
   | DivExpr (_, _, pos)
   | MultExpr (_, _, pos)
   | Average (_, pos)
   | PrintExpr (_, pos) ->
      pos

%}

%token TokEof

%token <Prof_type.pos> TokComma
%token <Prof_type.pos> TokSemi
%token <Prof_type.pos> TokLeftParen
%token <Prof_type.pos> TokRightParen
%token <Prof_type.pos> TokLeftBrack
%token <Prof_type.pos> TokRightBrack
%token <Prof_type.pos> TokOutput
%token <Prof_type.pos> TokPercentages
%token <Prof_type.pos> TokAverage
%token <Prof_type.pos> TokPrint

%token <Prof_type.pos> TokPlus
%token <Prof_type.pos> TokMinus
%token <Prof_type.pos> TokDiv
%token <Prof_type.pos> TokMult

%token <string * Prof_type.pos> TokString
%token <int * Prof_type.pos> TokInt
%token <float * Prof_type.pos> TokFloat

%token <Symbol.symbol * Prof_type.pos> TokId

%left TokMinus TokPlus
%left TokDiv TokMult

%start main
%type <Prof_type.expr list> main
%%

main:
     expr_list TokEof         { $1 }

expr_list:
     expr                     { [$1] }
   | expr_list TokSemi expr   { $1 @ [$3] }

expr:
     item                     { Single $1 }
   | multi                    { Multi $1 }

item:
     TokId                    { VarExpr (fst $1, snd $1) }
   | item TokPlus item        { PlusExpr ($1, $3, union_pos (pos_of_single $1) (pos_of_single $3)) }
   | item TokMinus item       { MinusExpr ($1, $3, union_pos (pos_of_single $1) (pos_of_single $3)) }
   | item TokDiv item         { DivExpr ($1, $3, union_pos (pos_of_single $1) (pos_of_single $3)) }
   | item TokMult item        { MultExpr ($1, $3, union_pos (pos_of_single $1) (pos_of_single $3)) }
   | TokLeftParen item TokRightParen
                              { $2 }
   | item_result              { $1 }
   | TokInt                   { NumExpr (float_of_int (fst $1), snd $1) }
   | TokFloat                 { NumExpr (fst $1, snd $1) }

item_list:
     item                     { [$1] }
   | item_list TokComma item  { $1 @ [$3] }

item_result:
     TokAverage TokLeftParen multi TokRightParen
                              { Average ($3, union_pos $1 $4) }
   | TokPrint TokLeftParen TokString TokRightParen
                              { PrintExpr (fst $3, snd $3) }

multi:
     TokPercentages TokLeftParen multi TokRightParen
                              { Percentages ($3, union_pos $1 $4) }
   | TokLeftBrack item_list TokRightBrack
                              { ListExpr ($2, union_pos $1 $3) }
