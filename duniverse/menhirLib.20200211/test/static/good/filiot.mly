%{
(****************************************************************
Binary Tree Automata Library (for OCaml)
Copyright (C) 2005 Emmanuel Filiot

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

version 1.3
contact : filiot@lifl.fr
*****************************************************************)

  open Parsed_formula
  open Error_handler
  let  _ = "meuh"

  type quant = Ex1 | Ex2 | All1 | All2

  let var_list_to_formula ty formula pos l =

    let rec aux = function

      | [] -> formula
      | x::xs ->
	  match ty with
	      Ex1 -> Exists1 (x, aux xs, pos)
	    | All1 -> Forall1 (x, aux xs, pos)
	    | Ex2 -> Exists2 (x, aux xs, pos)
	    | All2 -> Forall2 (x, aux xs, pos)

    in
      aux l


  let c = ref 0
  let get_fresh_var () = incr c ; "zzz" ^ string_of_int !c

(*   let childn n ctxt var = *)

(*     let rec aux = function *)

(* 	1 -> *)

(** Grammar :

    Body ::=   Variables_declaration ';' Formula ';'
             | Formula ';'

 *)

%}


/* special */
%token EOF
%token COMMA
%token POINTVIRGULE
%token SEMICOLON TILDA
%token DOT

/* boolean connectors */
%token AND OR IMPLIES EQUIV TRUE FALSE

/* quantifiers */
%token EX1 EX2 ALL1 ALL2

/* equalities */
%token EQ1 EQ2 DIFF1 DIFF2 SUB

/* orders */
%token PREF SIB SIBSTRICT PREFSTRICT IPREF ISIB ISIBSTRICT IPREFSTRICT

/* extra predicates */
%token ROOT LEAF CHILD FC NS LAB IN NOTIN SING


/* other */
%token <string> STR
%token <int> INT
%token LPAR RPAR
%token VAR

%nonassoc LPAR RPAR
%left TILDA
%left AND
%left OR
%left IMPLIES EQUIV
%left EX1 EX2 ALL1 ALL2

%start main
%type <(Parsed_formula.var list) * Parsed_formula.formula> main
%%

main:
| Body EOF { $1 }
| { raise( Fatal_error "empty input file") }

Body:

| Formula POINTVIRGULE { ([], $1) }
| VAR VariablesDec  Formula POINTVIRGULE { ($2, $3) }
| VAR VariablesDec
      { raise (Fatal_error (error_msg_pos (pos_item 2) ^
			      "no given formula")) }

VariablesDec:

| STR POINTVIRGULE { [$1, pos_item 1] }
| STR COMMA VariablesDec { ($1, pos_item 1) :: $3 }


Formula:

| LPAR Formula RPAR { $2 }
| TRUE { True }
| FALSE { False }
| SING LPAR STR RPAR { Singleton ($3,pos_item 3) }
/* | CHILD LPAR STR DOT INT COMMA STR RPAR { failwith ($3 ^ "." ^ (string_of_int $5)) } */
| BinPredPref LPAR STR COMMA STR RPAR
      { $1 ($3, pos_item 3) ($5, pos_item 5) }
| ROOT LPAR STR RPAR
	  { Root ($3, pos_item 3) }
| LEAF LPAR STR RPAR
	  { Leaf ($3, pos_item 3) }


| EX1 QuantVariablesDec Formula {
    var_list_to_formula Ex1 $3 (pos_rule ()) $2
  }
| EX2 QuantVariablesDec Formula {
    var_list_to_formula Ex2 $3 (pos_rule ()) $2
  }
| ALL1 QuantVariablesDec Formula {
    var_list_to_formula All1 $3 (pos_rule ()) $2
  }
| ALL2 QuantVariablesDec Formula {
    var_list_to_formula All2 $3 (pos_rule ()) $2
  }
| EX1 QuantVariablesDec error {
    raise
      (Fatal_error
	 (error_msg_pos (pos_rule ()) ^
	    "syntax error : maybe you forgot a comma ?")) }
| EX2 QuantVariablesDec error {
    raise
      (Fatal_error
	 (error_msg_pos (pos_rule ()) ^
	    "syntax error : maybe you forgot a comma ?")) }

| ALL1 QuantVariablesDec error {
    raise
      (Fatal_error
	 (error_msg_pos (pos_rule ()) ^
	    "syntax error : maybe you forgot a comma ?")) }

| ALL2 QuantVariablesDec error {
    raise
      (Fatal_error
	 (error_msg_pos (pos_rule ()) ^
	    "syntax error : maybe you forgot a comma ?")) }




/* infix binary predicates */

| STR BinPredInf STR { $2 ($1, pos_item 1) ($3, pos_item 3) }

/* boolean connectors */

| Formula OR Formula { Or ($1, $3, pos_rule ()) }
| Formula AND Formula { And ($1, $3, pos_rule ()) }
| Formula EQUIV Formula { Equiv ($1, $3, pos_rule ()) }
| Formula IMPLIES Formula { Implies ($1, $3, pos_rule ()) }


QuantVariablesDec:

| STR SEMICOLON { [$1, pos_item 1] }
| STR COMMA QuantVariablesDec { ($1, pos_item 1) :: $3 }

BinPredPref:

| CHILD { fun x y -> Child (x,y) }
| FC {fun x y -> First_child (x,y) }
| NS { fun x y -> Next_sibling (x,y) }
| STR { failwith ("unknown identifier : " ^ $1) }
| LAB { fun x y -> Lab (x,y) }

BinPredInf:

|  IN  { fun x y -> Is_in (x,y) }
|  NOTIN  { fun x y -> Neg (Is_in (x,y), pos_rule ()) }
|  EQ1  { fun x y -> Equal1 (x,y) }
|  EQ2  { fun x y -> Equal2 (x,y) }
|  DIFF1  { fun x y -> Diff1 (x,y) }
|  DIFF2  { fun x y -> Diff2 (x,y) }
|  SUB  { fun x y -> Include (x,y) }
|  PREF  { fun x y -> Pref (x,y) }
|  IPREF  { fun x y -> Pref (y,x) }
|  PREFSTRICT  {
     fun x y ->
       And (Pref (x,
		  y),
	    Diff1 (x,y),
	    pos_rule ())
   }
|  IPREFSTRICT  {
    fun x y -> And (Pref (y,x
			 ),
		    Diff1 (x,y),
		    pos_rule ())
   }
|  SIBSTRICT  {
     fun x y -> And (Next_sibling_star
	    (x,y)
	    ,
	  Diff1 (x,y)
	    , pos_rule ()) }
|  SIB  {
     fun x y -> Next_sibling_star (x,y)
   }
|  ISIB  { fun x y ->
	     Next_sibling_star (y,x)
	 }
|  ISIBSTRICT  {
     fun x y -> And (Next_sibling_star
		       ( y,x)
		       ,
		     Diff1 (x,y)
		       , pos_rule ()) }

| STR { failwith ("unknown identifier : " ^ $1) }

