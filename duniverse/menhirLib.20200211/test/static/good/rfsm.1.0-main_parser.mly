(* Original file: rfsm.1.0/rfsm/src/compiler/main_parser.mly *)
%token TYPE
%token FSM
%token MODEL
%token IN
%token OUT
%token INOUT
%token PERIODIC
%token SPORADIC
%token VALUE_CHANGES
%token SHARED
%token STATES
%token INPUT
%token OUTPUT
%token VARS
%token TRANS
%token ITRANS
%token <int> INT
%token TYBOOL
%token TYINT
%token TYEVENT
%token TRUE
%token FALSE
%token <string> LID
%token <string> UID
(* %token <string> STRING *)
%token SEMICOLON
%token COMMA
%token DOT
%token COLON
%token QMARK
%token COLEQ
%token EQUAL
%token NOTEQUAL
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LT
%token GT
%token LTE
%token GTE
%token PLUS MINUS TIMES DIV MOD
%token DOTDOT
%token ARROW_START
%token ARROW_END
%token BAR
(* %token BARBAR *)
%token EOF

(* Precedences and associativities for expressions *)

%nonassoc QMARK COLON     (* Lowest precedence *)
%left EQUAL NOTEQUAL GT LT GTE LTE
%left PLUS MINUS       
%left TIMES DIV MOD    (* Highest precedence *)

%type <Syntax.program> program

%start program

%{
open Location

let mk_location (p1,p2) = Loc (!input_name, p1, p2)

let mk_type_decl p desc = { Syntax.td_desc = desc; Syntax.td_loc = mk_location p }
let mk_global_decl p desc = { Syntax.g_desc = desc; Syntax.g_loc = mk_location p }
let mk_fsm_decl p desc = { Syntax.fsm_desc = desc; Syntax.fsm_loc = mk_location p }
let mk_stim_decl p desc = { Syntax.stim_desc = desc; Syntax.stim_loc = mk_location p }
let mk_fsm_inst p desc = { Syntax.fi_desc = desc; Syntax.fi_loc = mk_location p }
let mk_type_expression p desc = { Syntax.te_desc = desc; Syntax.te_loc = mk_location p }
let mk_type_index_expression p desc = { Syntax.ti_desc = desc; Syntax.ti_loc = mk_location p }
let mk_expression p desc = { Syntax.e_desc = desc; Syntax.e_loc = mk_location p }
let mk_condition p desc = { Syntax.cond_desc = desc; Syntax.cond_loc = mk_location p }
let mk_action p desc = { Syntax.act_desc = desc; Syntax.act_loc = mk_location p }
%}

%%

%public braced(X):
  | LBRACE x=X RBRACE { x }

%public paren(X):
  | LPAREN x=X RPAREN { x }

%public optional(X):
    /* Nothing */ { [] }
  | x=X { x }

%public my_list(X):
  /* nothing */
    { [] }
| x = X; xs = my_list(X)
    { x :: xs }

%public my_nonempty_list(X):
  x = X
    { [ x ] }
| x = X; xs = my_nonempty_list(X)
    { x :: xs }

%public my_separated_nonempty_list(separator, X):
  x = X
    { [ x ] }
| x = X; separator; xs = my_separated_nonempty_list(separator, X)
    { x :: xs }

%public my_separated_list(separator, X):
  /* nothing */
    { [] }
| x = my_separated_nonempty_list(separator, X)
    { x }

program:
  | tydecls=my_list(type_decl)
    models=my_nonempty_list(fsm_model)
    globals=my_nonempty_list(global)
    fsms=my_nonempty_list(fsm_inst)
    EOF
    { { Syntax.p_type_decls = tydecls;
        Syntax.p_fsm_models = models;
        Syntax.p_globals = globals;
        Syntax.p_fsm_insts = fsms; }
      }
  
(* TYPE DECLARATION *)

type_decl:
  | TYPE id=LID EQUAL t=typ
      { mk_type_decl ($symbolstartofs,$endofs) (Syntax.TD_Alias (id,t)) }
  | TYPE id=LID EQUAL cs=braced(my_separated_list(COMMA,UID))
      { mk_type_decl ($symbolstartofs,$endofs) (Syntax.TD_Enum (id,cs)) }

(* FSM MODEL *)

fsm_model:
  | FSM MODEL
      name=id 
      params=optional(params)
      LPAREN ios=my_separated_list(COMMA, io) RPAREN
      LBRACE
      STATES COLON states=terminated(my_separated_list(COMMA, UID),SEMICOLON)
      vars = optional(vars)
      TRANS COLON trans=terminated(my_separated_list(COMMA,transition),SEMICOLON)
      ITRANS COLON itrans=terminated(itransition,SEMICOLON)
      RBRACE {
        mk_fsm_decl
          ($symbolstartofs,$endofs)
          { Syntax.fd_name=name;
            Syntax.fd_params=params;
            Syntax.fd_states=states;
            Syntax.fd_ios=ios;
            Syntax.fd_vars=vars;
            Syntax.fd_trans=trans;
            Syntax.fd_itrans=itrans } }

params:
  | LT params=my_separated_list(COMMA, param) GT { params }

param:
  | id=LID COLON ty=typ { (id, mk_type_expression ($symbolstartofs,$endofs) ty) }

io:
  | IN d=io_desc { (Types.IO_In, d) }
  | OUT d=io_desc { (Types.IO_Out, d) }
  | INOUT d=io_desc { (Types.IO_Inout, d) }

io_desc:
  | id=LID COLON ty=typ { (id, mk_type_expression ($symbolstartofs,$endofs) ty) }

vars:
  | VARS COLON vars=terminated(separated_list(COMMA, var),SEMICOLON) { vars }

var:
  | id=LID COLON ty=typ { (id, mk_type_expression ($symbolstartofs,$endofs) ty) }

transition:
  | prio=prio
    src=UID
    ARROW_START
    cond=condition
    actions=optional(actions)
    ARROW_END
    dst=UID
      { src, mk_condition ($symbolstartofs,$endofs) cond, actions, dst, prio }

prio:
    | (* Nothing *) { false }
    | TIMES { true } 
      
itransition:
  | actions=optional(actions) ARROW_END dst=UID { dst, actions }

condition:
  | ev=LID { ([ev],[]) }
  | ev=LID DOT guards=separated_nonempty_list(DOT, guard) { ([ev], guards) }

guard:
  | e=guard_expr { e }

actions:
  | BAR actions=separated_nonempty_list(SEMICOLON, action) { actions }

action:
  | i=LID               { mk_action ($symbolstartofs,$endofs) (Action.Emit i) }
  | i=LID COLEQ e=expr  { mk_action ($symbolstartofs,$endofs) (Action.Assign (i,e)) }

(* GLOBALS *)

global:
  | INPUT id=id COLON ty=typ EQUAL st=stimuli
      { mk_global_decl
         ($symbolstartofs,$endofs)
         { Syntax.gd_name = id;
           Syntax.gd_type = mk_type_expression ($symbolstartofs,$endofs) ty;
           Syntax.gd_desc = Syntax.GInp st } }
  | OUTPUT id=id COLON ty=typ
      { mk_global_decl
         ($symbolstartofs,$endofs)
         { Syntax.gd_name = id;
           Syntax.gd_type = mk_type_expression ($symbolstartofs,$endofs) ty;
           Syntax.gd_desc = Syntax.GOutp } }
  | SHARED id=id COLON ty=typ
      { mk_global_decl
         ($symbolstartofs,$endofs)
         { Syntax.gd_name = id;
           Syntax.gd_type = mk_type_expression ($symbolstartofs,$endofs) ty;
           Syntax.gd_desc = Syntax.GShared } }

stimuli:
  | PERIODIC LPAREN p=INT COMMA s=INT COMMA d=INT RPAREN
      { mk_stim_decl ($symbolstartofs,$endofs) (Syntax.Periodic(p,s,d)) }
  | SPORADIC ts=paren(separated_list(COMMA,INT))
      { mk_stim_decl ($symbolstartofs,$endofs) (Syntax.Sporadic(ts)) }
  | VALUE_CHANGES vcs=paren(separated_list(COMMA,value_change))
      { mk_stim_decl ($symbolstartofs,$endofs) (Syntax.ValueChange(vcs)) }
  
value_change:
  | t=INT COLON v=const { (t,v) }
  
(* INSTANCEs *)

fsm_inst:
  | FSM name=id EQUAL model=id pvs=opt_inst_params args=paren(separated_list(COMMA,id))
      { mk_fsm_inst
          ($symbolstartofs,$endofs)
          { Syntax.fi_name = name;
            Syntax.fi_model = model;
            Syntax.fi_params = pvs;
            Syntax.fi_args = args }  }

opt_inst_params:
    /* Nothing */ { [] }
  |  LT params=separated_nonempty_list(COMMA, INT) GT { List.map (function v -> Expr.Val_int v) params }
  
(* CORE TYPE EXPRESSIONs *)

typ:
  | TYEVENT { Syntax.TEEvent }
  | TYINT r=option(int_range) { Syntax.TEInt r }
  | TYBOOL { Syntax.TEBool }
  | i=LID { Syntax.TEName i }

int_range:
    | LT lo=type_index_expr DOTDOT hi=type_index_expr GT
        { (mk_type_index_expression ($symbolstartofs,$endofs) lo,
           mk_type_index_expression ($symbolstartofs,$endofs) hi) }

type_index_expr:
  | c = INT
      { Syntax.TEConst c }
  | i = LID
      { Syntax.TEVar i }
  | LPAREN e = type_index_expr RPAREN
      { e }
  | e1 = type_index_expr PLUS e2 = type_index_expr
      { Syntax.TEBinop ("+", e1, e2) }
  | e1 = type_index_expr MINUS e2 = type_index_expr
      { Syntax.TEBinop ("-", e1, e2) }
  | e1 = type_index_expr TIMES e2 = type_index_expr
      { Syntax.TEBinop ("*", e1, e2) }
  | e1 = type_index_expr DIV e2 = type_index_expr
      { Syntax.TEBinop ("/", e1, e2) }
  | e1 = type_index_expr MOD e2 = type_index_expr
      { Syntax.TEBinop ("mod", e1, e2) }

(* GUARD EXPRESSIONS *)

guard_expr:
  | e1 = expr EQUAL e2 = expr
      { (e1, "=", e2) }
  | e1 = expr NOTEQUAL e2 = expr
      { (e1, "!=", e2) }
  | e1 = expr GT e2 = expr
      { (e1, ">", e2) }
  | e1 = expr LT e2 = expr
      { (e1, "<", e2) }
  | e1 = expr GTE e2 = expr
      { (e1, ">=", e2) }
  | e1 = expr LTE e2 = expr
      { (e1, "<=", e2) }

expr:
  | c = INT
      { Expr.EInt c }
  | c = bool
      { Expr.EBool c }
  | v = LID
      { Expr.EVar v }
  | c = UID
      { Expr.EEnum c }
  | LPAREN e = expr RPAREN
      { e }
  | e1 = expr PLUS e2 = expr
      { Expr.EBinop ("+", e1, e2) }
  | e1 = expr MINUS e2 = expr
      { Expr.EBinop ("-", e1, e2) }
  | e1 = expr TIMES e2 = expr
      { Expr.EBinop ("*", e1, e2) }
  | e1 = expr DIV e2 = expr
      { Expr.EBinop ("/", e1, e2) }
  | e1 = expr MOD e2 = expr
      { Expr.EBinop ("mod", e1, e2) }
  | e1 = expr EQUAL e2 = expr
      { Expr.EBinop ("=", e1, e2) }
  | e1 = expr NOTEQUAL e2 = expr
      { Expr.EBinop ("!=", e1, e2) }
  | e1 = expr GT e2 = expr
      { Expr.EBinop (">", e1, e2) }
  | e1 = expr LT e2 = expr
      { Expr.EBinop ("<", e1, e2) }
  | e1 = expr GTE e2 = expr
      { Expr.EBinop (">=", e1, e2) }
  | e1 = expr LTE e2 = expr
      { Expr.EBinop ("<=", e1, e2) }
  | e1 = expr QMARK e2 = expr COLON e3 = expr
      { Expr.ECond (e1, e2, e3) }

const:
  | v = INT { Expr.Val_int v }
  | v = bool { Expr.Val_bool v }
  | c = UID { Expr.Val_enum c }

bool:
  | TRUE { true }
  | FALSE { false }


id:
  | i = LID { i }
  | i = UID { i }
