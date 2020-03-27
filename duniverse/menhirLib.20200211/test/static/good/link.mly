%{
  open Structures
%}

%token <string> VAR
%token OP
%token CP
%token PAR
%token TIMES
%token SEQ
%token NEXT
%token IMP
%token RIMP
%token LIMP
%token NEG
%token END
%token DIRECTIVE
%token VDASH

%left PAR SEQ
%left TIMES NEXT
%nonassoc NEG

%start main
%type <[`Directive of string | `Sequent of Structures.sequent]> main

%%

main :
  sequent END {`Sequent $1}
| DIRECTIVE VAR {`Directive $2}

formimp:
  VAR  {`Var $1}
| formimp PAR formimp {`Par($1,$3)}
| formimp TIMES formimp {`Times($1,$3)}
| formimp NEXT formimp {`Next($1,$3)}
| formimp SEQ formimp {`Seq($1,$3)}
| formimp IMP formimp {`Imp($1,$3)}
| formimp RIMP formimp {`RImp($1,$3)}
| formimp LIMP formimp {`LImp($1,$3)}
| OP formimp CP {$2}
| formimp NEG {`Not($1)}

sequent:
formimp VDASH formimp {Sequent($1, $3)}
| VDASH formimp {Form($2)}
| formimp {Form($1)}
;
