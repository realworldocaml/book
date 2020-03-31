

/* File parser.mly */
       %{
       open Prooflang


      %}
	%token <Prooflang.step> TRIVIAL
	%token <Prooflang.step> UNFINISHED
        %token FACT FROM SUBGOALS IN TOOBTAIN BY ASSUME PROVE ASSIGN OR CASES DOPPELPUNKT EMPTY KOMMA CASE END EOF APOST QUOT
	%token STRING DOPPELSTRICH PUNKT LET EQUAL LPARAN RPARAN SEMIKOLON DOPPPEQ NAME LAM NOTYPE PFEIL VAR CONST
	%token <string> IDENT

	%token <int list> INTLIST

        %token LPAREN RPAREN
        %token EOL

        %start main             /* the entry point */
        %type <Prooflang.step> main
        %%
        main:
            step EOF                                                                   { $1 }
        ;
        step:
            TRIVIAL                                                                    { $1 }
	  | UNFINISHED				                                       { $1 }
          | FACT nafo FROM reflist SEMIKOLON step                                      { Fact_from ($2, $4, $6) }
          | SUBGOALS nafos IN steplist TOOBTAIN nafo BY reflist SEMIKOLON step         { Subgoals ($2, $4, $6, $8, $10) }
          | ASSUME hyps PROVE nafo IN step TOOBTAIN nafo BY reflist SEMIKOLON step     { Assume ($2,$4,$6,$8,$10,$12) }
          | ASSIGN subst SEMIKOLON step                                                { AssignSubst ($2,$4) }
          | ASSIGN abbrv SEMIKOLON step                                                { AssignAbbrv ($2,$4) }
          | OR LPARAN orsteps RPARAN SEMIKOLON step                                    { Or ($3,$6) }
          | CASES formulalist DOPPELPUNKT caseends TOOBTAIN nafo SEMIKOLON step        { Cases ($2, $4, $6, $8) }





	nafos:
	       nafo                                                           { [$1] }
	     | nafo KOMMA nafo                                                { [$1;$3] }

	nafo:
	      name DOPPELPUNKT formula                                        { ($1, $3) }
	;

	name:
	      PUNKT                                                           { LeerN }
	    | IDENT                                                           { Name $1 }


	;

	 formula:
                   PUNKT                                                      { LeerF }
		 | term                                                       { For $1 }
      ;

         formulalist:
                       formula                                                { [$1] }
		     | formula KOMMA formulalist                              { [$1] @ $3 }

        ;

	reflist:
	      ref                                                             {  [$1]   }
	    | ref KOMMA reflist                                               { [$1] @ $3 }
        ;

	ref:
	     LPARAN name KOMMA formula KOMMA occurence RPARAN                { ($2,$4,$6) }
       ;


        occurence:
                  PUNKT                                                       { LeerO }
               |  INTLIST                                                     { Occ $1 }
     ;


         caseends:
                CASE name DOPPELPUNKT formula DOPPELPUNKT step END            { [($2, $4, $6)] }
               | caseends KOMMA caseends                                      { $1 @ $3 }


       steplist:
              step                                                            { [$1] }
	    | step KOMMA steplist                                             { [$1] @ $3 }

        ;


       hyps:
              hyp                                                           { [] }
            | hyp KOMMA hyps                                                  { [$1] @ $3 }

        ;

       hyp:
             nafo                                                             { Fr $1 }
	   | const DOPPELPUNKT hyptyp                                         { Cn ($1, $3) }
	   | var   DOPPELPUNKT hyptyp                                         { Vr ($1, $3) }
        ;

       subst:
            LPARAN LET VAR var DOPPPEQ term RPARAN                                  { ($4,$6) }
        ;

       abbrv:
            LPARAN LET CONST const DOPPPEQ term RPARAN                              { ($4,$6) }

       ;

       orsteps:     step                                                      { [$1] }
                  | step DOPPELSTRICH orsteps                                   { [$1] @ $3 }

      ;







       const:
                name                                                          { Const $1 }
    ;

       var:
               name                                                          { Var $1 }

    ;

       hyptyp:
                NOTYPE                                                        { None }
              | complextype                                                   { Some $1 }
    ;

       term:
                LPARAN APOST var APOST RPARAN                                 { V $3 }
	      |	LPARAN QUOT const QUOT RPARAN                                 { C $3 }
	      |	LPARAN term term RPARAN                                       { A ($2, $3) }
	      | LPARAN LAM var DOPPELPUNKT complextype PUNKT term RPARAN      { L ( $3, $5, $7) }
    ;

       complextype:
                    IDENT                                                  { TC $1 }
                  | LPARAN complextype PFEIL complextype RPARAN            { TA ($2,$4) }

