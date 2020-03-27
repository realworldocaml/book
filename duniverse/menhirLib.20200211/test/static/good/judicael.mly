/* Cf. bug report no 4. */

%token <int> INT
%token <string> IDENT
%token PLUS
%token PARENG PAREND
%token FIN

%left  PLUS

%start main
%type <unit>main

%%

main:

        instruction {print_string "\ninstruction\n"; flush stdout}

	;

expr:
	/*	Constantes de types simples.	*/
	INT
	{
		print_string "\nINT:"; print_int $1;flush stdout
	}
|expr PLUS expr
	{
	  print_string "\nPlus"
	}
;

instruction:
expr { print_string "\ninstruction\n" ; flush stdout }

|IDENT PARENG PAREND { print_string "\nCall" ; flush stdout}

;

