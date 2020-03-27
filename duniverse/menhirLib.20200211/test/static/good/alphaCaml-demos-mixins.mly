%{

open Strings
open Mm.Raw

%}

%token <string> LIDENT
%token LCURLY RCURLY DOT MIXIN END LPAREN RPAREN CLOSE DELETE AND
       IN FAKE DEPENDS ON PLUS LET REC EQUAL VAL AS WILDCARD EOF
       LSQUARE RSQUARE

%type <Mm.Raw.expression> toplevel
%start toplevel

%%

atomic_expression:
| LIDENT
    { EVar $1 }
| LCURLY record_fields RCURLY
    { ERecord $2 }
| atomic_expression DOT LIDENT
    { ERecordSelection ($1, $3) }
| MIXIN components END
    { let _, input, output = $2 in
      EStructure (input, output) }
| LPAREN expression RPAREN
    { $2 }

unary_expression:
| atomic_expression
    { $1 }
| CLOSE unary_expression
    { EClose $2 }
| DELETE fields IN unary_expression
    { EDeletion ($4, $2) }
| FAKE LIDENT DEPENDS ON LIDENT IN unary_expression
    { EFakeDependency ($7, $2, $5) }

summand_expression:
| unary_expression
    { $1 }
| summand_expression PLUS unary_expression
    { EComposition ($1, $3) }

expression:
| summand_expression
    { $1 }
| LET REC bindings IN expression
    { ELetRec (List.rev $3, $5) }

/* One should check against backward dependencies in let rec
   definitions, except when the target has predictable shape.
   This is not currently done. */

bindings:
| binding
    { [ $1 ] }
| bindings AND binding
    { $3 :: $1 }

binding:
| LIDENT EQUAL expression
    { ($1, $3) }

toplevel:
| expression EOF
    { $1 }

record_fields:
| /* epsilon */
    { StringMap.empty }
| record_fields VAL LIDENT EQUAL expression
    { StringMap.add $3 $5 $1 }

/* One should check against duplicate field labels in structures.
   This is not currently done. */

components:
| /* epsilon */
    { 0, StringMap.empty, (StringMap.empty, []) }
| components VAL lident_pun dependencies EQUAL expression
    { let xname, iname = $3 in
      let i, input, (fields, anonymous) = $1 in
      let fields = StringMap.add xname ($4, iname, $6, Mm.KStructMember i) fields in
      i+1, input, (fields, anonymous) }
| components VAL WILDCARD AS LIDENT dependencies EQUAL expression
    { let i, input, (fields, anonymous) = $1 in
      let anonymous = ($6, $5, $8, Mm.KStructMember i) :: anonymous in
      i+1, input, (fields, anonymous) }
| components VAL lident_pun
    { let xname, iname = $3 in
      let i, input, output = $1 in
      let input = StringMap.add xname iname input in
      i+1, input, output }

lident_pun:
| LIDENT AS LIDENT /* external name, internal name */
    { $1, $3 }
| LIDENT           /* pun: both names identical */
    { $1, $1 }

fields:
| /* epsilon */
    { StringSet.empty }
| fields LIDENT
    { StringSet.add $2 $1 }

dependencies:
| /* epsilon */
    { [] }
| LSQUARE variables RSQUARE
    { $2 }

variables:
| /* epsilon */
    { [] }
| variables LIDENT
    { $2 :: $1 }
