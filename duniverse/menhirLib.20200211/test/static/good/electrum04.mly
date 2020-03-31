/*******************************************************************************
 * Time-stamp: <2015-03-05 CET 14:01:11 David Chemouil>
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
open Ast.Par
%}

%token FACT ASSERT PRED PRIVATE FUN ENUM
%token SIG EXTENDS ABSTRACT 

%%

%public paragraph :
 f = factDecl 
 { Fact f }
 |
 a = assertDecl
 { Assert a }
 |
 p = predDecl
 { Pred p }
 |
 f = funDecl
 { Fun f }
 |
 e = enumDecl
 { Enum e }
 |
 s = sigDecl
 { Sig s }

factDecl :
 FACT
 name = IDENT?
 body = block
 { make_fact ~name ~body }

assertDecl :
 ASSERT
 name = IDENT?
 body = block
 { make_assertion ~name ~body }

predDecl :
 PRIVATE?
 PRED
 name = IDENT
 params = loption(brackets(comma_sep(decl)))
 body = block
 { make_pred ~name ~params ~body }

funDecl :
 PRIVATE?
 FUN
 name = IDENT
 params = loption(brackets(comma_sep(decl)))
 COLON
 returns = expr  
 body = expr
 { make_func ~name ~params ~body ~returns }

enumDecl :
 ENUM
 name = IDENT
 cases = braces(comma_sep1(IDENT))
 { make_enum ~name ~cases }

sigDecl :
 is_variable = iboption(VAR)
 qual = sigQual
 SIG
 names = comma_sep1(IDENT)
 extends = sigExt?
 fields = braces(right_flexible_list(COMMA, decl))
 fact = block?
 { make_signature ~is_variable ~is_abstract:(fst qual) ~mult:(snd qual)
     ~names ~extends ~fields ~fact }  

sigQual :
 | is_abstract = iboption(ABSTRACT) mult = ioption(some_lone_one) 
     { (is_abstract, mult)}
 | mult = some_lone_one ABSTRACT
     { (true, Some mult)}

%inline some_lone_one:
 LONE
 { `Lone }
 | ONE
 { `One }
 | SOME
 { `Some }

sigExt :
 EXTENDS
 name = qname
 { Extends name }  
 | IN
 names = separated_nonempty_list(PLUS, qname)
   { In names }


