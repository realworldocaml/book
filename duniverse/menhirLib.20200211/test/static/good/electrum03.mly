/*******************************************************************************
 * Time-stamp: <2015-04-17 CEST 11:02:54 David Chemouil>
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
open Ast.File
%}

%start <Ast.File.file> file

%token MODULE OPEN AS
  
%%


%public file:
 s = specification
 EOF
 { s }

specification:
 m = module_decl?
 op = import*
 ps = paragraph_or_cmd*
 { (m, op, ps) }

paragraph_or_cmd:
 p = paragraph
 { Par p }
 | c = cmd
 { Cmd c }

module_decl:
 MODULE
 module_name = qname
 params = loption(brackets(comma_sep1(param)))
 { make_module_decl ~module_name ~params }

param:
 ioption(EXACTLY)
 qn = qname
 { qn }

import:
 OPEN
 module_name = qname
 params = loption(brackets(comma_sep1(qname)))
 pun = punning?
 { make_import ~module_name ~params ~pun }

punning:
 AS
 name = IDENT
 { name }

