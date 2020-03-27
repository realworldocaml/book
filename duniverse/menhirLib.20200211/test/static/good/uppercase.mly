/* A grammar where a nonterminal symbol begins with an uppercase
   letter. Non-standard, but allowed, for compatibility with
   ocamlyacc. In versions of Menhir up to 20150911, this caused
   a problem when --infer was used. */

%token A B EOF
%start<unit> entry

%%

entry:
  AB+ EOF {}

AB:
  A B {}

