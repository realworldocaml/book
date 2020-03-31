/*******************************************************************************
 * Time-stamp: <2015-01-26 CET 14:36:55 David Chemouil>
 * 
 * Electrum Analyzer 
 * Copyright (C) 2014-2015 Onera
 * Authors: 
 *   XXXX
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


%%

%public %inline comma_sep(X) :
  xs = separated_list(COMMA, X)
    { xs }

%public %inline comma_sep1(X) :
  xs = separated_nonempty_list(COMMA, X)
    { xs }

%public %inline braces(X):
  x = delimited(LBRACE, X, RBRACE)
    { x }

%public %inline brackets(X):
  x = delimited(LBRACKET, X, RBRACKET)
    { x }

%public %inline parens(X):
  x = delimited(LPAREN, X, RPAREN)
    { x }

%public %inline iboption(X):
 (* empty *)
 { false }
 | X
 { true }

%public %inline first(X, Y):
   x = X Y { x }


%public %inline second(X, Y):
   X y =Y { y }

(* Given by François Pottier on 2015-01-21
   at http://gallium.inria.fr/blog/lr-lists/ *)
%public right_flexible_list(delim, X):
| (* nothing *)
    { [] }
| x = X
    { [x] }
| x = X delim xs = right_flexible_list(delim, X)
    { x :: xs }
