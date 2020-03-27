/*******************************************************************************
 * Time-stamp: <2015-02-24 CET 17:52:53 David Chemouil>
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
open Ast.Ctrl
%}

%token RUN CHECK FOR EXPECT EXACTLY BUT
%token <Ast.Ident.t> NICKNAME

%%


%public cmd:
 c = named_cmd
 ioption(pair(EXPECT, NUMBER))  (* ignored *)
 { c }
 | b = block_cmd
 ioption(pair(EXPECT, NUMBER))  (* ignored *)
 { b }

named_cmd:
 nick = first(NICKNAME, COLON)? (* nickname of the command *)
 cmd = run_or_check
 qname = qname                  (* name of the called pred or assert *)
 scope = scope?
 { make_named_cmd ~qname ~cmd ~scope ~nick }

block_cmd:
 nick = first(NICKNAME, COLON)? (* nickname of the command *)
 cmd = run_or_check
 block = block
 scope = scope?
 { make_block_cmd ~block ~cmd ~scope ~nick }
   

%inline run_or_check:
 RUN
 { Run }
 | CHECK
 { Check }

scope:
 | FOR
   num = NUMBER
   typescopes = loption(second(BUT, comma_sep1(typescope)))
 { make_scope_for_but ~num ~typescopes } 
 | FOR
 typescopes = loption(comma_sep1(typescope))
 { make_scope_for_types ~typescopes }

typescope:
 exactly = iboption(EXACTLY)
 num = NUMBER
 sig_name = qname   
 { make_typescope ~exactly ~num ~sig_name }
