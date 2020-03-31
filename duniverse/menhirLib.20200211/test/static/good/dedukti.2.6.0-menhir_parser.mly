(* Original file: dedukti.2.6.0/Dedukti-2.6.0/parser/menhir_parser.mly *)
%{
open Basic
open Preterm
open Internals
open Scoping
open Reduction
open Signature
open Entry

let rec mk_lam : preterm -> (loc*ident*preterm) list -> preterm = fun te ps ->
  match ps with
  | []           -> te
  | (l,x,ty)::ps -> PreLam(l, x, Some ty, mk_lam te ps)

let rec mk_pi  : preterm -> (loc*ident*preterm) list -> preterm = fun ty ps ->
  match ps with
  | []           -> ty
  | (l,x,aa)::ps -> PrePi(l, Some x, aa, mk_pi ty ps)

let mk_config loc id1 id2_opt =
  try
    let open Env in
    let some i = Some(int_of_string i) in
    let config nb_steps strategy = {default_cfg with nb_steps; strategy} in
    match (id1, id2_opt) with
    | ("SNF" , None       ) -> config None     Snf
    | ("HNF" , None       ) -> config None     Hnf
    | ("WHNF", None       ) -> config None     Whnf
    | ("SNF" , Some i     ) -> config (some i) Snf
    | ("HNF" , Some i     ) -> config (some i) Hnf
    | ("WHNF", Some i     ) -> config (some i) Whnf
    | (i     , Some "SNF" ) -> config (some i) Snf
    | (i     , Some "HNF" ) -> config (some i) Hnf
    | (i     , Some "WHNF") -> config (some i) Whnf
    | (i     , None       ) -> {default_cfg with nb_steps = some i}
    | (_     , _          ) -> raise Exit (* captured bellow *)
  with _ -> raise (Parse_error(loc, "invalid command configuration"))
%}

%token EOF
%token DOT
%token COMMA
%token COLON
%token EQUAL
%token ARROW
%token FATARROW
%token LONGARROW
%token DEF
%token LEFTPAR
%token RIGHTPAR
%token LEFTBRA
%token RIGHTBRA
%token LEFTSQU
%token RIGHTSQU
%token <Basic.loc> EVAL
%token <Basic.loc> INFER
%token <Basic.loc> CHECK
%token <Basic.loc> CHECKNOT
%token <Basic.loc> ASSERT
%token <Basic.loc> ASSERTNOT
%token <Basic.loc> PRINT
%token <Basic.loc> GDT
%token <Basic.loc> UNDERSCORE
%token <Basic.loc*Basic.mident> NAME
%token <Basic.loc*Basic.mident> REQUIRE
%token <Basic.loc> TYPE
%token <Basic.loc> KW_DEF
%token <Basic.loc> KW_THM
%token <Basic.loc*Basic.ident> ID
%token <Basic.loc*Basic.mident*Basic.ident> QID
%token <string> STRING

%start line
%type <Basic.mident -> Entry.entry> line
%type <Preterm.prule> rule
%type <Preterm.pdecl> decl
%type <Basic.loc*Basic.ident*Preterm.preterm> param
%type <Preterm.pdecl list> context
%type <Basic.loc*Basic.mident option*Basic.ident*Preterm.prepattern list> top_pattern
%type <Preterm.prepattern> pattern
%type <Preterm.prepattern> pattern_wp
%type <Preterm.preterm> sterm
%type <Preterm.preterm> term

%right ARROW FATARROW

%%

line:
  | id=ID ps=param* COLON ty=term DOT
      {fun md -> Decl(fst id, snd id, Static, scope_term md [] (mk_pi ty ps))}
  | KW_DEF id=ID COLON ty=term DOT
      {fun md -> Decl(fst id, snd id, Definable, scope_term md [] ty)}
  | KW_DEF id=ID COLON ty=term DEF te=term DOT
      {fun md -> Def(fst id, snd id, false, Some(scope_term md [] ty), scope_term md [] te)}
  | KW_DEF id=ID DEF te=term DOT
      {fun md -> Def(fst id, snd id, false, None, scope_term md [] te)}
  | KW_DEF id=ID ps=param+ COLON ty=term DEF te=term DOT
      {fun md -> Def(fst id, snd id, false, Some(scope_term md [] (mk_pi ty ps)),
                     scope_term md [] (mk_lam te ps))}
  | KW_DEF id=ID ps=param+ DEF te=term DOT
      {fun md -> Def(fst id, snd id, false, None, scope_term md [] (mk_lam te ps))}
  | KW_THM id=ID COLON ty=term DEF te=term DOT
      {fun md -> Def(fst id, snd id, true, Some(scope_term md [] ty), scope_term md [] te)}
  | KW_THM id=ID ps=param+ COLON ty=term DEF te=term DOT
      {fun md -> Def(fst id, snd id, true, Some(scope_term md [] (mk_pi ty ps)),
                     scope_term md [] (mk_lam te ps))}
  | rs=rule+ DOT
      {fun md -> Rules(List.map (scope_rule md) rs)}

  | EVAL te=term DOT
      {fun md -> Eval($1, default_cfg, scope_term md [] te)}
  | EVAL cfg=eval_config te=term DOT
      {fun md -> Eval($1, cfg, scope_term md [] te)}
  | INFER te=term DOT
      {fun md -> Infer($1, default_cfg, scope_term md [] te)}
  | INFER cfg=eval_config te=term DOT
      {fun md -> Infer($1, cfg, scope_term md [] te)}

  | CHECK te=aterm COLON ty=term DOT
      {fun md -> Check($1, false, false, HasType(scope_term md [] te, scope_term md [] ty))}
  | CHECKNOT te=aterm COLON ty=term DOT
      {fun md -> Check($1, false, true , HasType(scope_term md [] te, scope_term md [] ty))}
  | ASSERT te=aterm COLON ty=term DOT
      {fun md -> Check($1, true , false, HasType(scope_term md [] te, scope_term md [] ty))}
  | ASSERTNOT te=aterm COLON ty=term DOT
      {fun md -> Check($1, true , true , HasType(scope_term md [] te, scope_term md [] ty))}

  | CHECK t1=aterm EQUAL t2=term DOT
      {fun md -> Check($1, false, false, Convert(scope_term md [] t1, scope_term md [] t2))}
  | CHECKNOT t1=aterm EQUAL t2=term DOT
      {fun md -> Check($1, false, true , Convert(scope_term md [] t1, scope_term md [] t2))}
  | ASSERT t1=aterm EQUAL t2=term DOT
      {fun md -> Check($1, true , false, Convert(scope_term md [] t1, scope_term md [] t2))}
  | ASSERTNOT t1=aterm EQUAL t2=term DOT
      {fun md -> Check($1, true , true , Convert(scope_term md [] t1, scope_term md [] t2))}

  | PRINT STRING DOT {fun _ -> Print($1, $2)}
  | GDT   ID     DOT {fun _ -> DTree($1, None, snd $2)}
  | GDT   QID    DOT {fun _ -> let (_,m,v) = $2 in DTree($1, Some m, v)}
  | n=NAME       DOT {fun _ -> Name(fst n, snd n)}
  | r=REQUIRE    DOT {fun _ -> Require(fst r,snd r)}
  | EOF              {raise End_of_file}

eval_config:
  | LEFTSQU id=ID RIGHTSQU
      {mk_config (Lexer.loc_of_pos $startpos) (string_of_ident (snd id)) None}
  | LEFTSQU id1=ID COMMA id2=ID RIGHTSQU
      {mk_config (Lexer.loc_of_pos $startpos) (string_of_ident (snd id1))
        (Some(string_of_ident (snd id2)))}

param:
  | LEFTPAR id=ID COLON te=term RIGHTPAR
      {(fst id, snd id, te)}

rule:
  | LEFTSQU context RIGHTSQU top_pattern LONGARROW term
      { let (l,md_opt,id,args) = $4 in
        ( l , None, $2 , md_opt, id , args , $6) }
  | LEFTBRA ID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW term
      { let (l,md_opt,id,args) = $7 in
        ( l , Some (None,snd $2), $5 , md_opt, id , args , $9)}
  | LEFTBRA QID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW term
      { let (l,md_opt,id,args) = $7 in
        let (_,m,v) = $2 in
        ( l , Some (Some m,v), $5 , md_opt, id , args , $9)}

decl:
  | ID COLON term { debug 1 "Ignoring type declaration in rule context."; $1 }
  | ID            { $1 }

context:
  | /* empty */                          { [] }
  | separated_nonempty_list(COMMA, decl) { $1 }

top_pattern:
  | ID  pattern_wp* { (fst $1,None,snd $1,$2) }
  | QID pattern_wp* { let (l,md,id)=$1 in (l,Some md,id,$2) }

%inline pid:
  | UNDERSCORE { ($1, mk_ident "_") }
  | ID { $1 }

pattern_wp:
  | ID                       { PPattern (fst $1,None,snd $1,[]) }
  | QID                      { let (l,md,id)=$1 in PPattern (l,Some md,id,[]) }
  | UNDERSCORE               { PJoker $1 }
  | LEFTBRA term RIGHTBRA    { PCondition $2 }
  | LEFTPAR pattern RIGHTPAR { $2 }

pattern:
  | ID  pattern_wp+          { PPattern (fst $1,None,snd $1,$2) }
  | QID pattern_wp+          { let (l,md,id)=$1 in PPattern (l,Some md,id,$2) }
  | ID  FATARROW pattern     { PLambda (fst $1,snd $1,$3) }
  | pattern_wp               { $1 }

sterm:
  | QID                      { let (l,md,id)=$1 in PreQId(l,mk_name md id) }
  | pid                      { PreId (fst $1,snd $1) }
  | LEFTPAR term RIGHTPAR    { $2 }
  | TYPE                     { PreType $1 }

aterm:
  | te=sterm ts=sterm*
      {match ts with [] -> te | a::args -> PreApp(te,a,args)}

term:
  | t=aterm
      { t }
  | pid COLON aterm ARROW term
      { PrePi (fst $1,Some (snd $1), $3, $5) }
  | LEFTPAR ID COLON aterm RIGHTPAR ARROW term
      { PrePi (fst $2,Some (snd $2), $4 ,$7) }
  | term ARROW term
      { PrePi (Lexer.loc_of_pos $startpos,None,$1,$3) }
  | pid FATARROW term
      {PreLam (fst $1, snd $1, None, $3)}
  | pid COLON aterm FATARROW term
      {PreLam (fst $1, snd $1, Some $3, $5)}
%%
