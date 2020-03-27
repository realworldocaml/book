/*
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
  open Support
  open Support.Error
  open Support.Pervasive
  open Syntax
  open Primitives
  open Format
  open Print

  let make_rec fi vars prims name ~args ~ret ~body =
    let arglist = args vars prims in
    let argnames, types = List.split arglist in
    let argcount = List.length argnames in

    let ret_ty_vars = (List.rev argnames) @ vars in
    let body_vars = (List.rev argnames) @ (name::vars) in
    let body = body body_vars prims in

    let maybe_ret = ret ret_ty_vars prims in
    let ret_ty =
      match maybe_ret with
      | Some ty -> ty
      | None -> tm_prim ~fi "Dynamic"
    in


    let fixtype =
      make_arrow_sequence fi vars (List.combine argnames types) ret_ty in
    let shifted_types = mapi (fun index ty -> term_shift_above 1 index ty) types in

    make_prim_app_sequence fi vars
      "fix" []
      [fixtype;
       make_lambda_sequence fi vars
         ((name, fixtype) :: (List.combine argnames shifted_types))
         ~ret:(term_shift_above 1 argcount ret_ty)
         body]

(*



	datatype D (x_i:S_i) =
		C_i
		L_i of T_i

	(* occurences of D in T_i become (DT z) *)
	let rec DT (z:Unit) (x_i:S_i) : * = Z:* -> (Unit -> Z) -> ... -> (Init -> Z) -> (T_1 -> Z) ... (T_n -> Z) -> Z
	let D (x_i:S_i) = (DT unit)

	let C_i (x_i:S_i) : D (x_i) =
           fn  Z:* -> (f1:Unit -> Z) -> ... -> (..:Init -> Z) -> (..:T_1 -> Z) ... (..:T_n -> Z) -> Z => f1 unit

	let L_j (x_i:S_i) (y:T_j) : D (x_i) =
           fn  Z:* -> (..:Unit -> Z) -> ... -> (..:Init -> Z) -> (f1:T_1 -> Z) ... (fn:T_n -> Z) -> Z => fj y

	let caseD (x_i:S_i) =
	   Z:* -> (v: D (x_i)) -> (r1:Unit -> Z) -> ... -> (Init -> Z) -> (T_1 -> Z) ... (rk:T_n -> Z) -> Z => v X r1 .. rk

*)

(*



	datatype D (x_i:S_i) =
		C_i
		L_i of y_i_1:T_i_1...y_i_n:T_i_n

	let rec D (x_i:S_i) : * = Z:* -> (Unit -> Z) -> ... -> (Unit -> Z) -> (y_i_1:T_i_1...y_i_n:T_i_n -> Z) ... (y_i_1:T_i_1...y_i_n:T_i_n -> Z) -> Z

	let C_1 (x_i:S_i) : D (x_i) =
           fn  Z:* -> (f1:Unit -> Z) -> ... -> (..:Unit -> Z) -> (... -> Z) ... (... -> Z) -> Z => f1 unit

	let L_1 (x_i:S_i) (y_k:T_k) : D (x_i) =
           fn  Z:* -> (..:Unit -> Z) -> ... -> (..:Unit -> Z) ->
	             (y_i_1:T_i_1...y_i_n:T_i_n -> Z) ... (y_i_1:T_i_1...y_i_n:T_i_n -> Z) -> Z => fj y_1_1..y_1_n

	let caseD (x_i:S_i) =
	   Z:* -> (v: D (x_i)) -> (r1:Unit -> Z) -> ... -> (Init -> Z) -> (r1: (y_i_1:T_i_1...y_i_n:T_i_n -> Z)) ... => v Z r1 .. rk

*)


let make_datatype fi vars prims (name:string) (args:(string * ty) list) (constrs:(string*(string * ty) list) list) =

  let buf = Buffer.create 20 in
  let spr = Buffer.add_string in
  let vars' = name::((List.map (fun (x,ty)->x) args)@vars) in

  let rec pr_name_type_list pre (a:(string * ty) list) post vars =
    match a with
    | [] -> ()
    | (x,s)::r ->
        spr buf (pre ^ x ^ ":"); string_of_tm buf vars s; spr buf post;
        pr_name_type_list pre r post (x::vars) in

  let pr_name_list a =
    List.iter (fun (x,s) -> spr buf (" " ^ x)) a in

  let pr_fn name args result_ty vars =
    spr buf ("" ^ name ^ ":("); pr_name_type_list "" args "->" vars;
    spr buf ("" ^ result_ty ^ ")") in

  let rec pr_fns pre fns post vars =
    match fns with
    | [] -> ()
    | (name, params)::r ->
        spr buf pre;
        (if params = [] then
          pr_fn (name^"fn") params "Unit->Z" vars
        else
          pr_fn (name^"fn") params "Z" vars);
        spr buf post;
        pr_fns pre r post vars in

  spr buf ("let rec " ^ name); pr_name_type_list "(" args ")" vars;
  spr buf " : * = Z:*";
  pr_fns "\n->" constrs "" vars'; spr buf "\n->Z;;\n\n";

  spr buf ("let case" ^ name); pr_name_type_list "(" args ")" vars;
  spr buf ("(v:" ^ name); pr_name_list args; spr buf ")= v;;\n\n";

  List.iter (fun (label, largs) ->
    spr buf ("let " ^ label); pr_name_type_list "(" args ")" vars;
    pr_name_type_list "(" largs ")" vars'; spr buf "(Z:*)";
    pr_fns "\n(" constrs ")" vars';
    spr buf ("\n= " ^ label ^ "fn ");
    (if largs = [] then spr buf "unit");
    pr_name_list largs; spr buf ";;\n" ) constrs;
  Buffer.contents buf

%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> TYPE
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> BOOL
%token <Support.Error.info> CASE
%token <Support.Error.info> OF
%token <Support.Error.info> AS
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> ASSUME
%token <Support.Error.info> ASSUMENOT
%token <Support.Error.info> ASSERT
%token <Support.Error.info> LET
%token <Support.Error.info> IN
%token <Support.Error.info> REC
%token <Support.Error.info> FIX
%token <Support.Error.info> LETREC
%token <Support.Error.info> USTRING
%token <Support.Error.info> UNIT
%token <Support.Error.info> UUNIT
%token <Support.Error.info> UFLOAT
%token <Support.Error.info> INT
%token <Support.Error.info> TOP
%token <Support.Error.info> FN

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> ID  /* uppercase-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> AND
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONDASH
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DATATYPE
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> GEQ
%token <Support.Error.info> HASH
%token <Support.Error.info> IFF
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> LEQ
%token <Support.Error.info> MINUS
%token <Support.Error.info> OR
%token <Support.Error.info> PLUS
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SEMISEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> SUBTYPE
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR
%token <Support.Error.info> QUESTION

/* NOTE: COLON must be higher precedence than EQ because of
 return type declarations*/
%nonassoc ALWAYS_SHIFT
%right ABSTRACTION DOT
%left SEMI
%right ARROW DARROW
%nonassoc ASSUME LET IN
%right ELSE
%right AS VBAR
%nonassoc ID STAR LAMBDA STRINGV INTV EQ FN REC IF THEN CASE OF QUESTION IFF
%nonassoc GEQ LEQ GT LT
%nonassoc LCURLY LPAREN LSQUARE COLON
%left PLUS MINUS AND
%left APPLICATION
%left APOSTROPHE

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context)
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).

*/

%start toplevel
%type < Syntax.var_list -> string list -> (Syntax.command list * Syntax.var_list)> toplevel

%start term
%type < Syntax.var_list -> string list -> Syntax.term > term

%start command
%type <Syntax.var_list -> string list -> Syntax.command * string list> command

%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { fun vars prims -> ([], vars) }
  | command SEMISEMI toplevel
      { fun vars prims ->
          let (cmd, vars') = $1 vars prims in
          let (cmds, vars'') = $3 vars' prims in
          (cmd::cmds, vars'') }
  | Datatype SEMISEMI toplevel
      {
	    fun vars prims ->
	      let (fi,name,params,constrs) = $1 vars prims in
          let dt_str =
            make_datatype fi vars prims name params constrs
          in
          let (dt_cmds, vars') = (!toplevel_parse_thunk) dt_str vars prims in
          let (cmds, vars'') = $3 vars' prims in
          (dt_cmds @ cmds, vars'')
      }

/* A top-level command */
command :
  | term
      { fun vars prims ->
          let t = $1 vars prims in (Eval(tm_info t, t), vars) }
  | Let
      { fun vars prims ->
          (* The parser generator has a bug/feature so I MUST deconstruct and
             reconstruct this value. *)
          let (fi,x,tm) = $1 vars prims in
          Define (fi,x,tm), x::vars }

  | AssumeWord Environment COLONDASH Type SUBTYPE Type
      { fun vars prims ->
          let ctx = $2 vars prims in
          let vars' = (var_list_from_ctx ctx) @ vars in
          Assume (ctx, $4 vars' prims, $6 vars' prims, $1), vars
      }

/* I do this instead of a second word i.e. ASSUME NOT because NOT
  needs to remain an ID so it can be a primitive as well. */
AssumeWord :
  | ASSUMENOT { false }
  | ASSUME    { true }

Environment :
  |                     { fun vars prims -> empty_ctx }
  | NonEmptyEnvironment { $1 }

NonEmptyEnvironment :
  | Assumption { fun vars prims ->
                   let var, bind = $1 vars prims in
                   add_binding empty_ctx var bind }

  | NonEmptyEnvironment COMMA Assumption
      { fun vars prims ->
          let ctx = $1 vars prims in
          let vars' = var_list_from_ctx ctx in
          let var, bind = $3 (vars' @ vars) prims in
          add_binding ctx var bind
      }

Assumption :
  | ID COLON ident_free_term MaybeEqTerm
      { fun vars prims ->
          $1.v, VarBind($3 vars prims, $4 vars prims) }

  | ID COLON ident_free_term DOT term MaybeEqTerm
      { fun vars prims ->
          let x = $1.v in
          let ty = $3 vars prims in
          x,
          VarBind(
            make_prim_app_sequence $1.i vars "Refine:" []
              [ty;
               TmFun($1.i, x, ty, $5 (x::vars) prims)],
            $6 vars prims) }

MaybeEqTerm :
  |                { fun vars prims -> None }
  | EQEQ term      { fun vars prims -> Some ($2 vars prims) }


Let :
  | LET ID ParamSeq MaybeTy EQ term
      { fun vars prims ->
          let params = $3 vars prims in
          let vars' =  (List.rev_map fst params) @ vars in
          let return = $4 vars' prims in
          let body = $6 vars' prims in
          $1, $2.v, make_lambda_sequence $1 vars params ?ret:return body
      }

  | LET REC ID ParamSeq MaybeTy EQ term
      { fun vars prims ->
          $1, $3.v, (make_rec $1 vars prims $3.v
                       ~args:$4 ~ret:$5 ~body:$7)
      }



Datatype :
  DATATYPE ID ParamSeq EQ ConstructorSeq {
    fun vars prims ->
      let params = $3 vars prims in
      let vars' = $2.v::((List.map (fun (x,ty) -> x) params) @ vars) in
      ($1, $2.v, params, $5 vars' prims)
 }

ConstructorSeq :
  | Constructor %prec VBAR     { fun vars prims -> ($1 vars prims)::[] }
  | Constructor VBAR ConstructorSeq  {
	fun vars prims -> ($1 vars prims)::($3 (vars) prims)
	}

Constructor :
  | ID OF ParamSeq
      { fun vars prims -> ($1.v, $3 vars prims) }
  | ID
      { fun vars prims ->
          (*
          let (_, x) = pick_fresh_var vars "u" in
          let ty = TmPrimitive($1.i, "Unit", []) in
          *)
          ($1.v, []) }




/* A term is a lambda-calculus expression with terms in the type position of
function declarations. */
Type : term %prec ALWAYS_SHIFT { $1 }
term :
  | ID COLON ident_free_term DOT term
      { fun vars prims ->
          make_refinement $1.i  vars $1.v
            ($3 vars prims)
            ($5 ($1.v::vars) prims) }
  | LCURLY ID COLON ident_free_term VBAR term RCURLY
      { fun vars prims ->
          make_refinement $2.i  vars $2.v
            ($4 vars prims)
            ($6 ($2.v::vars) prims) }

  | ID COLON ident_free_term ARROW term %prec ABSTRACTION
      { fun vars prims ->
          let x = $1.v in
          TmArrow($1.i, x, $3 vars prims, $5 (x::vars) prims) }

  | ident_free_term  %prec ARROW  { $1 }

ident_free_term:
  | LPAREN term RPAREN { $2 }

    /* Core syntax */
  | ID { fun vars prims ->
           let fi = $1.i in
           if List.mem $1.v vars then
             var_to_term fi vars $1.v
           else if List.mem $1.v prims then
             tm_prim ~fi $1.v
           else
             error fi ("Identifier " ^ $1.v ^
                 " not bound to variable or primitive") }


  | LAMBDA ID COLON ident_free_term DOT term %prec ABSTRACTION
      { fun vars prims ->
          let x = $2.v in
          TmFun($1, x, $4 vars prims, $6 (x::vars) prims) }

  | Let IN term
      { fun vars prims ->
          let (fi,x,arg) = $1 vars prims in
          TmLet(fi, x, arg, $3 (x::vars) prims) }

  /* Minor Syntactic Sugar */
  | term ARROW term %prec ABSTRACTION
      { fun vars prims ->
          let x = "_" in
          TmArrow($2, x, $1 vars prims, $3 (x::vars) prims) }

  | term term %prec APPLICATION
      { fun vars prims ->
          let t1 = $1 vars prims in
          let t2 = $2 vars prims in
          TmApp(tm_info t1, t1, t2) }

  /* Main syntactic sugar */
  | term AS term
      { fun vars prims ->
          let tm = $1 vars prims in
          let ty = $3 vars prims in
          let info = tm_info tm in
          make_as info vars tm ty
      }

  | FN ParamSeq MaybeTy DARROW term %prec ABSTRACTION
      { fun vars prims ->
          let params = $2 vars prims in
          let vars' =  (List.rev_map fst params) @ vars in
          let return = $3 vars' prims in
          let body = $5 vars' prims in
              make_lambda_sequence $1 vars params ?ret:return body
      }


  | REC ID ParamSeq MaybeTy DARROW term %prec ABSTRACTION
      { fun vars prims ->
          make_rec $1 vars prims $2.v
            ~args:$3 ~ret:$4 ~body:$6
      }

  | term SEMI term
      { fun vars prims ->
          let x = "_" in
          let t1 = $1 vars prims in
          let t2 = $3 (x::vars) prims in
          TmLet(tm_info t1, x, t1, t2) }

/* Placeholders are off limits to the parser, and therefore
not part of the input language
  | APOSTROPHE ID
      { fun vars prims ->
          tm_placeholder vars ~fi:$2.i ~p:(PlaceHolder $2.v) () }
*/

  | QUESTION
      { fun vars prims -> tm_prim ~fi:$1 "Dynamic" }

  | STAR
      { fun vars prims -> tm_prim ~fi:$1 "*" }

  | INTV
      { fun vars prims -> tm_prim ~fi:$1.i (string_of_int $1.v) }

  | STRINGV
      { fun vars prims -> tm_prim ~fi:$1.i ("\"" ^ $1.v ^ "\"") }

  | term EQ term
      { fun vars prims ->
          make_prim_app_sequence $2 vars "inteq" []
            [$1 vars prims;
             $3 vars prims] }

  | term IFF term
      { fun vars prims ->
          make_prim_app_sequence $2 vars "iff" []
            [$1 vars prims;
             $3 vars prims] }

  | term LEQ term
      { fun vars prims ->
          make_prim_app_sequence $2 vars "leq" []
            [($1 vars prims);
             ($3 vars prims)] }

  | term GEQ term
      { fun vars prims ->
          make_prim_app_sequence $2 vars "geq" []
            [($1 vars prims);
             ($3 vars prims)] }

  | term LT term
      { fun vars prims ->
          make_prim_app_sequence $2 vars "lt" []
            [($1 vars prims);
             ($3 vars prims)] }

  | term GT term
      { fun vars prims ->
          make_prim_app_sequence $2 vars "gt" []
            [($1 vars prims);
             ($3 vars prims)] }

  | term PLUS term
      { fun vars prims ->
          make_prim_app_sequence $2 vars "add" []
            [($1 vars prims); ($3 vars prims)] }

  | term MINUS term
      { fun vars prims ->
          make_prim_app_sequence $2 vars "sub" []
            [($1 vars prims); ($3 vars prims)] }

      /*
  | term STAR term
      { fun vars prims ->
          make_prim_app_sequence $2 vars "mul" []
            [($1 vars prims); ($3 vars prims)] }
      */

  | term AND term
      { fun vars prims ->
          make_prim_app_sequence $2 vars "and" []
            [($1 vars prims); ($3 vars prims)] }

  | term OR term
      { fun vars prims ->
          make_prim_app_sequence $2 vars "or" []
            [($1 vars prims); ($3 vars prims)] }

  | LCURLY LabelColonTyList RCURLY
      { fun vars prims ->
          let labels, types = List.split ($2 vars prims) in
          make_prim_app_sequence $1 vars "Record" labels types
      }

  | LT LabelColonTyList GT
      { fun vars prims ->
          let labels, types = List.split ($2 vars prims) in
          make_prim_app_sequence $1 vars "Variant" labels types
      }

/*  | LCURLY LabelEqualTmList RCURLY */

  | LT ID EQ term GT
      { fun vars prims ->
          let fi = $1 in
          let tm, ty = $4 vars prims, tm_prim ~fi "Dynamic" in
          make_prim_app_sequence fi vars "mkVariant" [$2.v] [ty; tm]
      }

/*  | CASE term OF CaseList
      { fun vars prims ->
          let fi = $1 in
          let labels, placeholders, funs = split3 ($4 vars prims) in
          make_prim_app_sequence fi vars "getVariant" labels
            (placeholders @ [tm_prim ~fi "Dynamic"]
             @ funs @ [$2 vars prims]) } */


  | IF LSQUARE Type RSQUARE term THEN term ELSE term
      { fun vars prims ->
          let fi = $1 in
	      make_prim_app_sequence fi vars "IF" []
            [$3 vars prims;
	         $5 vars prims;
	         TmFun(fi, "_", tm_prim ~fi "Dynamic", $7 ("_"::vars) prims);
             TmFun(fi, "_", tm_prim ~fi "Dynamic", $9 ("_"::vars) prims) ]
      }

  | IF term THEN term ELSE term
      { fun vars prims ->
          let fi = $1 in
	  let ph1 = tm_prim ~fi "Dynamic" in
	  let ph2 = tm_prim ~fi "Dynamic" in
	  let ph3 = tm_prim ~fi "Dynamic" in
	  make_prim_app_sequence fi vars "IF" []
            [ph1;
	     $2 vars prims;
	     TmFun(fi, "_", ph2, $4 ("_"::vars) prims);
             TmFun(fi, "_", ph3, $6 ("_"::vars) prims) ] }


ParamSeq :
  |   { fun vars prims -> [] }
  | Param ParamSeq
      { fun vars prims ->
          let x,ty = $1 vars prims in
          (x,ty) :: ($2 (x::vars) prims) }

Param :
  | ID
      { fun vars prims -> $1.v, tm_prim ~fi:$1.i "Dynamic" }
  | LPAREN ID COLON term RPAREN
      { fun vars prims -> $2.v, $4 vars prims }
  | LPAREN ID COLON term DOT term RPAREN
      { fun vars prims ->
          let ty = $4 vars prims in
          let refine = $6 ($2.v :: vars) prims in
          $2.v, make_refinement $2.i vars $2.v ty refine }

MaybeTy :
  | COLON term  { fun vars prims -> Some ($2 vars prims) }
  |             { fun vars prims -> None }

LabelColonTyList :
  | NonEmptyLabelColonTyList { $1 }
  |                          { fun vars prims -> [] }

NonEmptyLabelColonTyList :
  | ID COLON Type
      { fun vars prims ->
          [$1.v, $3 vars prims] }

  | ID COLON Type COMMA NonEmptyLabelColonTyList
      { fun vars prims ->
          let l = $1.v in
          (l, $3 vars prims) :: ($5 (l::vars) prims) }
/*
CaseList :
  | CaseEntry %prec VBAR     { fun vars prims -> [$1 vars prims] }
  | CaseEntry VBAR CaseList  { fun vars prims ->
                                 ($1 vars prims) :: ($3 vars prims) }

CaseEntry :
  | VBAR ID COLON ID DARROW term
      { fun vars prims ->
          let x, p = $4.v, fresh_placeholder $1 vars in
          $2.v, p, TmFun ($1, x, p, $6 (x::vars) prims) }
*/
%%

