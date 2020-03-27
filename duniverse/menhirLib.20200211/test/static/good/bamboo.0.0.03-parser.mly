(* Original file: bamboo.0.0.03/bamboo-0.0.03/src/parse/parser.mly *)
%token CONTRACT
%token <string> IDENT
%token <WrapBn.t> DECLIT256
%token <WrapBn.t> DECLIT8
%token ADDRESS
%token UINT256
%token UINT8
%token BYTES32
%token BOOL
%token LPAR
%token RPAR
%token PLUS
%token MINUS
%token MULT
%token RARROW
%token COMMA
%token LSQBR
%token RSQBR
%token LBRACE
%token RBRACE
%token DOT
%token CASE
%token DEFAULT
%token IF
%token ELSE
%token RETURN
%token FALSE
%token TRUE
%token THEN
%token BECOME
%token SEMICOLON
%token EQUALITY
%token NEQ
%token LT
%token GT
%token SINGLE_EQ
%token EVENT
%token LOG
%token DEPLOY
%token ALONG
%token REENTRANCE
%token ABORT
%token SELFDESTRUCT
%token NOT
%token VALUE
%token SENDER
%token MSG
%token THIS
%token LAND
%token NOW
%token VOID
%token BLOCK
%token INDEXED
%token BALANCE
%token EOF

%right RARROW

%left LAND
%left NEQ EQUALITY LT GT
%left PLUS MINUS
%left MULT

%start <unit Syntax.toplevel list> file
%%


%inline plist(X):
   xs = delimited(LPAR, separated_list(COMMA, X), RPAR) {xs}

file:
  | cs = list(contract); EOF; { cs }
  ;

contract:
  | CONTRACT;
    name = IDENT;
    args = plist(arg);
    LBRACE;
    css = list(case);
    RBRACE;
    { Syntax.Contract
      ({ Syntax.contract_cases = css
       ; contract_name = name
       ; contract_arguments = args}) }
  | EVENT;
    name = IDENT;
    args = plist(event_arg);
    SEMICOLON;
    { Syntax.Event { Syntax.event_arguments = args
      ; event_name = name
      }}
  ;

case:
  | ch  = case_header;
    cb  = block;
    {
      { Syntax.case_header = ch
      ; Syntax.case_body = cb
      }
     }
  ;

block:
  | LBRACE;
    scs = list(sentence);
    RBRACE
    { scs }
  ;

case_header:
  | DEFAULT { Syntax.DefaultCaseHeader }
  | CASE; LPAR;
    return_typ = typ;
    name = IDENT;
    args = plist(arg);
    RPAR { Syntax.UsualCaseHeader
      { case_return_typ = [return_typ] (* multi returns not supported *)
      ; Syntax.case_name = name
      ; case_arguments = args
      }
    }
  | CASE; LPAR;
    VOID;
    name = IDENT;
    args = plist(arg);
    RPAR { Syntax.UsualCaseHeader
      { case_return_typ = []
      ; Syntax.case_name = name
      ; case_arguments = args
      }
    }
  ;

arg:
  | t = typ;
    i = IDENT
    { { Syntax.arg_typ = t
      ; Syntax.arg_ident = i
      ; Syntax.arg_location = None
      }
    }
  ;

event_arg:
  | a = arg { Syntax.event_arg_of_arg a false }
  | t = typ;
    INDEXED;
    i = IDENT
    { { Syntax.event_arg_body =
        { Syntax.arg_typ = t
        ; Syntax.arg_ident = i
        ; Syntax.arg_location = None
        }
      ; Syntax.event_arg_indexed = true
      }
    }
    ;

typ:
  | UINT256 { Syntax.Uint256Type }
  | UINT8 { Syntax.Uint8Type }
  | BYTES32 { Syntax.Bytes32Type }
  | ADDRESS { Syntax.AddressType }
  | BOOL { Syntax.BoolType }
  | key = typ;
    RARROW;
    value = typ;
    { Syntax.MappingType (key, value) }
  | s = IDENT { Syntax.ContractInstanceType s }
  ;

%inline body:
  | s = sentence {[s]}
  | b = block {b}
  ;

sentence:
  | ABORT; SEMICOLON { Syntax.AbortSentence }
  | RETURN; value = option(exp); THEN; BECOME; cont = exp; SEMICOLON
    { Syntax.ReturnSentence { Syntax. return_exp = value; return_cont = cont} }
  | lhs = lexp; SINGLE_EQ; rhs = exp; SEMICOLON
    { Syntax.AssignmentSentence (lhs, rhs) }
  | t = typ;
    name = IDENT;
    SINGLE_EQ;
    value = exp;
    SEMICOLON { Syntax.VariableInitSentence
                { Syntax.variable_init_type = t
                ; variable_init_name = name
                ; variable_init_value = value
                }
              }
  | VOID; SINGLE_EQ; value = exp; SEMICOLON
    { Syntax.ExpSentence value }
  | IF; LPAR; cond = exp; RPAR; bodyT = body; ELSE; bodyF = body { Syntax.IfThenElse (cond, bodyT, bodyF) }
  | IF; LPAR; cond = exp; RPAR; body = body { Syntax.IfThenOnly (cond, body) }
  | LOG; name = IDENT; lst = exp_list; SEMICOLON { Syntax.LogSentence (name, lst, None)}
  | SELFDESTRUCT; e = exp; SEMICOLON { Syntax.SelfdestructSentence e }
  ;

%inline op:
  | PLUS {fun (l, r) -> Syntax.PlusExp(l, r)}
  | MINUS {fun (l, r)  -> Syntax.MinusExp(l, r)}
  | MULT {fun (l, r) -> Syntax.MultExp(l, r)}
  | LT {fun (l, r) -> Syntax.LtExp(l, r)}
  | GT {fun (l, r) -> Syntax.GtExp(l, r)}
  | NEQ {fun (l, r) -> Syntax.NeqExp(l, r)}
  | EQUALITY {fun (l, r) -> Syntax.EqualityExp(l, r)}
  ;

exp:
  | lhs = exp; LAND; rhs = exp { Syntax.LandExp (lhs, rhs), () }
  | TRUE { Syntax.TrueExp, () }
  | FALSE { Syntax.FalseExp, () }
  | d = DECLIT256 { Syntax.DecLit256Exp d, ()}
  | d = DECLIT8 { Syntax.DecLit8Exp d, ()}
  | VALUE LPAR MSG RPAR { Syntax.ValueExp, () }
  | SENDER LPAR MSG RPAR { Syntax.SenderExp, () }
  | BALANCE; LPAR; e = exp; RPAR { Syntax.BalanceExp e, () }
  | NOW LPAR BLOCK RPAR { Syntax.NowExp, () }
  | lhs = exp; o = op; rhs = exp { (o (lhs, rhs)), () }
  | s = IDENT
    { Syntax.IdentifierExp s, () }
  | LPAR;
    e = exp;
    RPAR
    { Syntax.ParenthExp e, () }
  | s = IDENT; lst = exp_list { Syntax.FunctionCallExp {Syntax.call_head = s; call_args = lst }, () }
  | DEPLOY; s = IDENT; lst = exp_list; m = msg_info { Syntax.NewExp { Syntax.new_head = s; new_args = lst; new_msg_info = m }, () }
  | contr = exp; DOT; DEFAULT;
    LPAR; RPAR; m = msg_info
    { Syntax.SendExp { Syntax.send_head_contract = contr; send_head_method = None
                       ; send_args = []; send_msg_info = m }, () }
  | contr = exp; DOT; mtd = IDENT; lst = exp_list; m = msg_info
    { Syntax.SendExp { Syntax.send_head_contract = contr; send_head_method = Some mtd
                       ; send_args = (lst); send_msg_info = m }, () }
  | ADDRESS; LPAR; e = exp; RPAR { Syntax.AddressExp e, () }
  | NOT; e = exp { Syntax.NotExp e, () }
  | THIS { Syntax.ThisExp, () }
  | l = lexp;
    { Syntax.ArrayAccessExp l, () }
  ;


%inline exp_list:
   lst = plist(exp) {lst}

msg_info:
  | v = value_info; r = reentrance_info { { Syntax.message_value_info = v;
                                            message_reentrance_info = r } }
  ;

value_info:
  | (* empty *) { None }
  | ALONG; v = exp; { Some v }
  ;

reentrance_info:
  | REENTRANCE; b = block { b }
  ;

lexp:
  | s = exp;
    LSQBR;
    idx = exp;
    RSQBR
    { Syntax.ArrayAccessLExp {
       Syntax.array_access_array = s; array_access_index = idx} }
  ;
