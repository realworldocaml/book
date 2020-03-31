/* a parser for rfc2254 ldap filters

   Copyright (C) 2004 Eric Stokes, and The California State University
   at Northridge

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/


%{
  open Ldap_types

  let star_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\2a")
  let lparen_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\28")
  let rparen_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\29")
  let backslash_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\5c")
  let null_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\00")
  let unescape s =
    (Pcre.qreplace ~rex:star_escape_rex ~templ:"*"
       (Pcre.qreplace ~rex:lparen_escape_rex ~templ:"("
          (Pcre.qreplace ~rex:rparen_escape_rex ~templ:")"
             (Pcre.qreplace ~rex:null_escape_rex ~templ:"\000"
                (Pcre.qreplace ~rex:backslash_escape_rex ~templ:"\\" s)))))
%}

%token WHSP LPAREN RPAREN AND OR NOT EOF
%token <string * string> ATTREQUAL
%token <string * Ldap_types.substring_component> ATTREQUALSUB
%token <string * string> ATTRGTE
%token <string * string> ATTRLTE
%token <string * string> ATTRAPPROX
%token <string> ATTRPRESENT
%token <string * string * string> ATTREXTENDEDMATCH
%token <string * string option * string> ATTREXTENDEDDN
%start filter_and_eof
%type <Ldap_types.filter> filter_and_eof
%%

filterlist:
  filterlist filter {$2 :: $1}
| filter {[$1]}
;

filter:
  LPAREN AND filterlist RPAREN {`And $3}
| LPAREN OR filterlist RPAREN {`Or $3}
| LPAREN NOT filter RPAREN {`Not $3}
| LPAREN filter RPAREN {$2}
| ATTREQUALSUB {`Substrings {attrtype=(fst $1);substrings=(snd $1)}}
| ATTREQUAL {`EqualityMatch {attributeDesc=(fst $1);assertionValue=(unescape (snd $1))}}
| ATTRGTE {`GreaterOrEqual {attributeDesc=(fst $1);assertionValue=(unescape (snd $1))}}
| ATTRLTE {`LessOrEqual {attributeDesc=(fst $1);assertionValue=(unescape (snd $1))}}
| ATTRPRESENT {`Present $1}
| ATTRAPPROX {`ApproxMatch {attributeDesc=(fst $1);assertionValue=(unescape (snd $1))}}
| ATTREXTENDEDMATCH {let (a, oid, v) = $1 in
                       `ExtensibleMatch
                         {matchingRule=(Some (unescape oid));
                          ruletype=(Some (unescape a));
                          matchValue=(unescape v);
                          dnAttributes=false}}
| ATTREXTENDEDDN {let (a, oid, v) = $1 in
                    `ExtensibleMatch
                      {matchingRule=(match oid with
                                         Some s -> Some (unescape s)
                                       | None -> None);
                       ruletype=(Some (unescape a));
                       matchValue=(unescape v);
                       dnAttributes=true}}
;

/* used to enforce EOF at the end of the filter */
filter_and_eof:
  filter EOF {$1}
;
