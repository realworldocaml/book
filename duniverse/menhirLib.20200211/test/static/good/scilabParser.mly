%{
  open ScilabAst
  open Lexing

  let extract_str_from_strExp exp = match exp.exp_desc with
    | ConstExp (StringExp strexp) -> strexp.stringExp_value
    | _ -> failwith "shouldn't happen"

  let create_loc start_pos end_pos =
    { first_line = start_pos.pos_lnum;
      first_column = (start_pos.pos_cnum - start_pos.pos_bol);
      last_line = end_pos.pos_lnum;
      last_column = (end_pos.pos_cnum - end_pos.pos_bol) }

  let create_exp loc desc =
    let infos = { is_verbose = false } in
    {exp_location = loc; exp_desc = desc; exp_info = infos}

  let create_dummy_exp () =
    create_exp dummy_loc (ConstExp (CommentExp { commentExp_comment = "dummy exp" }))

  let new_symbol s = ScilabSymbol.new_symbol s
  let simpleVar s = SimpleVar (new_symbol s)

%}
%token SOF
%token LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE DOLLAR SPACES
%token COMMA EOL SEMI IF THEN ELSE ELSEIF END WHILE DO
%token COLON ASSIGN FOR FUNCTION ENDFUNCTION HIDDEN HIDDENFUNCTION
%token PLUS MINUS RDIVIDE LDIVIDE TIMES POWER EQ NE LT GT LE GE
%token SELECT SWITCH OTHERWISE CASE TRY CATCH RETURN BREAK CONTINUE
%token BOOLTRUE BOOLFALSE QUOTE AND ANDAND NOT DOT DOTQUOTE DOTTIMES
%token DOTLDIVIDE DOTRDIVIDE DOTPOWER OR OROR KRONTIMES CONTROLTIMES
%token CONTROLLDIVIDE CONTROLRDIVIDE LINEBREAK KRONLDIVIDE KRONRDIVIDE

%token WIERDOP
%token<float> VARINT
%token<float> VARFLOAT
%token<float> NUM
%token<string> ID
%token<string> COMMENT
%token<string> STR
%token EOF

%nonassoc TOPLEVEL
%nonassoc HIGHLEVEL
%nonassoc UPLEVEL
%nonassoc LISTABLE

%nonassoc CONTROLBREAK

%left OR OROR
%left AND ANDAND

%left COLON COMMA
%left EQ NE LT LE GT GE
%left MINUS PLUS
%left TIMES DOTTIMES KRONTIMES CONTROLTIMES RDIVIDE DOTRDIVIDE KRONRDIVIDE CONTROLRDIVIDE LDIVIDE DOTLDIVIDE KRONLDIVIDE CONTROLLDIVIDE
%left POWER DOTPOWER

%left QUOTE DOTQUOTE

%left DOT

%left NOT

%nonassoc FUNCTIONCALL
%nonassoc BOOLTRUE BOOLFALSE
%nonassoc LPAREN LBRACE


%start program
%type <ScilabAst.ast>program

%%
program :
| expressions                                   { Exp $1 }
| EOL expressions                               { Exp $2 }
| expressionLineBreak                           { let seqexp = SeqExp [] in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc off_st off_end in
                                                  Exp (create_exp loc seqexp) }
| /* Empty */                                   { let seqexp = SeqExp [] in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc off_st off_end in
                                                  Exp (create_exp loc seqexp) }

expressions :
| recursiveExpression                          { let seqexp = SeqExp (List.rev $1) in
                                                 let off_st = Parsing.rhs_start_pos 1 in
                                                 let off_end = Parsing.rhs_end_pos 1 in
                                                 let loc = create_loc off_st off_end in
                                                 create_exp loc seqexp }
| recursiveExpression expression               { let seqexp = SeqExp (List.rev (List.append (match $2.exp_desc with SeqExp l -> l | _ -> [$2]) $1)) in
                                                 let off_st = Parsing.rhs_start_pos 1 in
                                                 let off_end = Parsing.rhs_end_pos 2 in
                                                 let loc = create_loc off_st off_end in
                                                 create_exp loc seqexp }
| recursiveExpression expression COMMENT       { let commentexp = CommentExp { commentExp_comment = $3 } in
                                                 let cmt_st = Parsing.rhs_start_pos 3 in
                                                 let cmt_end = Parsing.rhs_end_pos 3 in
                                                 let cmt_loc = create_loc cmt_st cmt_end in
                                                 let cmt_exp = create_exp cmt_loc (ConstExp commentexp) in
                                                 let seqexp = SeqExp (List.rev (List.append (List.append (match $2.exp_desc with SeqExp l -> l | _ -> [$2]) $1) [cmt_exp])) in
                                                 let off_st = Parsing.rhs_start_pos 1 in
                                                 let off_end = Parsing.rhs_end_pos 2 in
                                                 let loc = create_loc off_st off_end in
                                                 create_exp loc seqexp }
| expression                                   { let seqexp = SeqExp (match $1.exp_desc with SeqExp l -> l | _ -> [$1]) in
                                                 let off_st = Parsing.rhs_start_pos 1 in
                                                 let off_end = Parsing.rhs_end_pos 1 in
                                                 let loc = create_loc off_st off_end in
                                                 create_exp loc seqexp }
| expression COMMENT                           { let commentexp = CommentExp { commentExp_comment = $2 } in
                                                 let cmt_st = Parsing.rhs_start_pos 2 in
                                                 let cmt_end = Parsing.rhs_end_pos 2 in
                                                 let cmt_loc = create_loc cmt_st cmt_end in
                                                 let cmt_exp = create_exp cmt_loc (ConstExp commentexp) in
                                                 let seqexp = SeqExp (
                                                   List.append (match $1.exp_desc with SeqExp l -> l | _ -> [$1]) [cmt_exp]) in
                                                 let off_st = Parsing.rhs_start_pos 1 in
                                                 let off_end = Parsing.rhs_end_pos 2 in
                                                 let loc = create_loc off_st off_end in
                                                 create_exp loc seqexp }

recursiveExpression :
| recursiveExpression expression expressionLineBreak         { List.append (match $2.exp_desc with SeqExp l -> l | _ -> [$2]) $1 }
| recursiveExpression expression COMMENT expressionLineBreak { let commentexp = CommentExp { commentExp_comment = $3 } in
                                                               let cmt_st = Parsing.rhs_start_pos 3 in
                                                               let cmt_end = Parsing.rhs_end_pos 3 in
                                                               let cmt_loc = create_loc cmt_st cmt_end in
                                                               let cmt_exp = create_exp cmt_loc (ConstExp commentexp) in
                                                               cmt_exp::(List.append (match $2.exp_desc with SeqExp l -> l | _ -> [$2]) $1) }
| expression COMMENT expressionLineBreak                     { let commentexp = CommentExp { commentExp_comment = $2 } in
                                                               let cmt_st = Parsing.rhs_start_pos 2 in
                                                               let cmt_end = Parsing.rhs_end_pos 2 in
                                                               let cmt_loc = create_loc cmt_st cmt_end in
                                                               let cmt_exp = create_exp cmt_loc (ConstExp commentexp) in
                                                               cmt_exp::(match $1.exp_desc with SeqExp l -> l | _ -> [$1]) }
| expression expressionLineBreak                             { match $1.exp_desc with SeqExp l -> l | _ -> [$1] }

expressionLineBreak :
| SEMI                                        { }
| COMMA                                       { }
| EOL                                         { }
| expressionLineBreak SEMI                    { }
| expressionLineBreak COMMA                   { }
| expressionLineBreak EOL                     { }

expression :
| functionDeclaration				{ $1 }
| functionCall %prec TOPLEVEL                   { $1 }
| variableDeclaration                           { $1 }
| ifControl                                     { $1 }
| selectControl                                 { $1 }
| forControl                                    { $1 }
| whileControl                                  { $1 }
| tryControl                                    { $1 }
| variable %prec TOPLEVEL                       { $1 }
| implicitFunctionCall  %prec TOPLEVEL          { let list = List.rev $1 in
                                                  let caller = List.hd list in
                                                  let args = List.tl list in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc off_st off_end in
                                                  let callexp = {callExp_name = caller;
                                                                 callExp_args = Array.of_list (List.rev args) } in
                                                  create_exp loc (CallExp callexp) }
| BREAK						{ let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (ControlExp BreakExp) }
| CONTINUE					{ let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (ControlExp ContinueExp) }
| returnControl					{ $1 }
| COMMENT                                       { let commentexp = CommentExp { commentExp_comment = $1 } in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (ConstExp commentexp) }
/*| error */

/* IMPLICIT FUNCTIONCALL */
/* Bash-like : foo bar titi <=> foo('bar', 'titi') */
implicitFunctionCall :
| implicitFunctionCall implicitCallable         { $2::$1 }
| ID implicitCallable                           { let varloc_st = Parsing.rhs_start_pos 1 in
                                                  let varloc_end = Parsing.rhs_end_pos 1 in
                                                  let varloc = create_loc varloc_st varloc_end in
                                                  let var =
                                                    Var { var_location = varloc;
                                                          var_desc = simpleVar $1 } in
                                                  let varexp = create_exp varloc var in
                                                  $2::[varexp]}

implicitCallable :
| ID                                            { let strexp = StringExp
                                                    { stringExp_value = $1 ;
                                                      stringExp_bigString = () } in
                                                  let str_st = Parsing.rhs_start_pos 1 in
                                                  let str_end = Parsing.rhs_end_pos 1 in
                                                  let str_loc = create_loc str_st str_end in
                                                  create_exp str_loc (ConstExp strexp) }
| VARINT                                        { let strexp = StringExp
                                                    { stringExp_value = string_of_float $1 ;
                                                      stringExp_bigString = () } in
                                                  let str_st = Parsing.rhs_start_pos 1 in
                                                  let str_end = Parsing.rhs_end_pos 1 in
                                                  let str_loc = create_loc str_st str_end in
                                                  create_exp str_loc (ConstExp strexp) }
| NUM                                           { let strexp = StringExp
                                                    { stringExp_value = string_of_float $1 ;
                                                      stringExp_bigString = () } in
                                                  let str_st = Parsing.rhs_start_pos 1 in
                                                  let str_end = Parsing.rhs_end_pos 1 in
                                                  let str_loc = create_loc str_st str_end in
                                                  create_exp str_loc (ConstExp strexp) }
| VARFLOAT                                      { let strexp = StringExp
                                                    { stringExp_value = string_of_float $1 ;
                                                      stringExp_bigString = () } in
                                                  let str_st = Parsing.rhs_start_pos 1 in
                                                  let str_end = Parsing.rhs_end_pos 1 in
                                                  let str_loc = create_loc str_st str_end in
                                                  create_exp str_loc (ConstExp strexp) }
| STR                                           { let strexp = StringExp
                                                    { stringExp_value = $1 ;
                                                      stringExp_bigString = () } in
                                                  let str_st = Parsing.rhs_start_pos 1 in
                                                  let str_end = Parsing.rhs_end_pos 1 in
                                                  let str_loc = create_loc str_st str_end in
                                                  create_exp str_loc (ConstExp strexp) }
| DOLLAR                                        { let strexp = StringExp
                                                    { stringExp_value = "$" ;
                                                      stringExp_bigString = () } in
                                                  let str_st = Parsing.rhs_start_pos 1 in
                                                  let str_end = Parsing.rhs_end_pos 1 in
                                                  let str_loc = create_loc str_st str_end in
                                                  create_exp str_loc (ConstExp strexp) }
| BOOLTRUE                                      { let strexp = StringExp
                                                    { stringExp_value = "%t" ;
                                                      stringExp_bigString = () } in
                                                  let str_st = Parsing.rhs_start_pos 1 in
                                                  let str_end = Parsing.rhs_end_pos 1 in
                                                  let str_loc = create_loc str_st str_end in
                                                  create_exp str_loc (ConstExp strexp) }
| BOOLFALSE                                     { let strexp = StringExp
                                                    { stringExp_value = "%f" ;
                                                      stringExp_bigString = () } in
                                                  let str_st = Parsing.rhs_start_pos 1 in
                                                  let str_end = Parsing.rhs_end_pos 1 in
                                                  let str_loc = create_loc str_st str_end in
                                                  create_exp str_loc (ConstExp strexp) }
| implicitCallable DOT ID                       {  let strexp = StringExp
                                                     { stringExp_value =  (extract_str_from_strExp $1) ^ $3 ;
                                                       stringExp_bigString = () } in
                                                  let str_st = Parsing.rhs_start_pos 1 in
                                                  let str_end = Parsing.rhs_end_pos 1 in
                                                  let str_loc = create_loc str_st str_end in
                                                  create_exp str_loc (ConstExp strexp) }

/* FUNCTIONCALL */
functionCall :
| simpleFunctionCall                            { $1 }
| specificFunctionCall                          { $1 }
/*| LPAREN functionCall RPAREN                    { $2 }*/

specificFunctionCall :
| BOOLTRUE LPAREN functionArgs RPAREN           { let varloc_st = Parsing.rhs_start_pos 1 in
                                                  let varloc_end = Parsing.rhs_end_pos 1 in
                                                  let varloc = create_loc varloc_st varloc_end in
                                                  let varexp =
                                                    Var { var_location = varloc;
                                                          var_desc = simpleVar "%t" } in
                                                  let callexp =
                                                    { callExp_name = create_exp varloc varexp;
                                                      callExp_args = Array.of_list (List.rev $3) } in
                                                  let fcall_st = Parsing.rhs_start_pos 1 in
                                                  let fcall_end = Parsing.rhs_end_pos 4 in
                                                  let loc = create_loc fcall_st fcall_end in
                                                  create_exp loc (CallExp callexp) }
| BOOLFALSE LPAREN functionArgs RPAREN          { let varloc_st = Parsing.rhs_start_pos 1 in
                                                  let varloc_end = Parsing.rhs_end_pos 1 in
                                                  let varloc = create_loc varloc_st varloc_end in
                                                  let varexp =
                                                    Var { var_location = varloc;
                                                          var_desc = simpleVar "%f" } in
                                                  let callexp =
                                                    { callExp_name = create_exp varloc varexp;
                                                      callExp_args = Array.of_list (List.rev $3) } in
                                                  let fcall_st = Parsing.rhs_start_pos 1 in
                                                  let fcall_end = Parsing.rhs_end_pos 4 in
                                                  let loc = create_loc fcall_st fcall_end in
                                                  create_exp loc (CallExp callexp) }

simpleFunctionCall :
| ID LPAREN functionArgs RPAREN                 { let varloc_st = Parsing.rhs_start_pos 1 in
                                                  let varloc_end = Parsing.rhs_end_pos 1 in
                                                  let varloc = create_loc varloc_st varloc_end in
                                                  let varexp =
                                                    Var { var_location = varloc;
                                                          var_desc = simpleVar $1 } in
                                                  let callexp =
                                                    { callExp_name = create_exp varloc varexp;
                                                      callExp_args = Array.of_list (List.rev $3) } in
                                                  let fcall_st = Parsing.rhs_start_pos 1 in
                                                  let fcall_end = Parsing.rhs_end_pos 4 in
                                                  let loc = create_loc fcall_st fcall_end in
                                                  create_exp loc (CallExp callexp) }
/*| ID LBRACE functionArgs RPAREN */

functionArgs :
| variable                                      { [$1] }
| functionCall                                  { [$1] }
/*| COLON                                         { let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  [ create_exp loc cvar_exp ] }*/
| variableDeclaration                           { [$1] }
| /* Empty */                                   { [] }
| functionArgs COMMA variable                   { $3::$1 }
| functionArgs COMMA functionCall               { $3::$1 }
/*| functionArgs COMMA COLON                      { let cvarloc_st = Parsing.rhs_start_pos 3 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 3 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp_desc =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let cvar_exp =
                                                    create_exp loc cvar_exp_desc in
                                                cvar_exp::$1 }*/
| functionArgs COMMA variableDeclaration       { $3::$1 }
| functionArgs COMMA                           { $1 }

/* FUNCTION DECLARATION */
functionDeclaration :
| FUNCTION ID ASSIGN ID functionDeclarationArguments functionDeclarationBreak functionBody functionDelimiter {
  let ret_st =  Parsing.rhs_start_pos 2 in
  let ret_end = Parsing.rhs_end_pos 2 in
  let ret_loc = create_loc ret_st ret_end in
  let var_ret = { var_location = ret_loc ;
                  var_desc = simpleVar $2 } in
  let ret_var_array = { arrayListVar_location = ret_loc;
                        arrayListVar_vars = Array.of_list [var_ret] } in
  let args_st = Parsing.rhs_start_pos 5 in
  let args_end = Parsing.rhs_end_pos 5 in
  let args_loc = create_loc args_st args_end in
  let args_var_array = { arrayListVar_location = args_loc;
                         arrayListVar_vars = Array.of_list $5 } in
  let fundec_st = Parsing.rhs_start_pos 1 in
  let fundec_end = Parsing.rhs_end_pos 8 in
  let fundec_loc = create_loc fundec_st fundec_end in
  let fundec = FunctionDec { functionDec_location = fundec_loc;
                             functionDec_symbol = new_symbol $4;
                             functionDec_args = args_var_array;
                             functionDec_returns = ret_var_array;
                             functionDec_body = $7 } in
  create_exp fundec_loc (Dec fundec) }
| FUNCTION COMMENT ID COMMENT ASSIGN ID functionDeclarationArguments functionDeclarationBreak functionBody functionDelimiter {
  let ret_st =  Parsing.rhs_start_pos 3 in
  let ret_end = Parsing.rhs_end_pos 3 in
  let ret_loc = create_loc ret_st ret_end in
  let var_ret = { var_location = ret_loc ;
                  var_desc = simpleVar $3 } in
  let ret_var_array = { arrayListVar_location = ret_loc;
                        arrayListVar_vars = Array.of_list [var_ret] } in
  let args_st = Parsing.rhs_start_pos 7 in
  let args_end = Parsing.rhs_end_pos 7 in
  let args_loc = create_loc args_st args_end in
  let args_var_array = { arrayListVar_location = args_loc;
                         arrayListVar_vars = Array.of_list $7 } in
  let fundec_st = Parsing.rhs_start_pos 1 in
  let fundec_end = Parsing.rhs_end_pos 10 in
  let fundec_loc = create_loc fundec_st fundec_end in
  let fundec = FunctionDec { functionDec_location = fundec_loc;
                             functionDec_symbol = new_symbol $6;
                             functionDec_args = args_var_array;
                             functionDec_returns = ret_var_array;
                             functionDec_body = $9 } in
  create_exp fundec_loc (Dec fundec) }
| FUNCTION COMMENT LBRACK functionDeclarationReturns RBRACK COMMENT ASSIGN ID functionDeclarationArguments functionDeclarationBreak functionBody functionDelimiter {
  let ret_st =  Parsing.rhs_start_pos 4 in
  let ret_end = Parsing.rhs_end_pos 4 in
  let ret_loc = create_loc ret_st ret_end in
  let ret_var_array = { arrayListVar_location = ret_loc;
                        arrayListVar_vars = Array.of_list $4 } in
  let args_st = Parsing.rhs_start_pos 9 in
  let args_end = Parsing.rhs_end_pos 9 in
  let args_loc = create_loc args_st args_end in
  let args_var_array = { arrayListVar_location = args_loc;
                         arrayListVar_vars = Array.of_list $9 } in
  let fundec_st = Parsing.rhs_start_pos 1 in
  let fundec_end = Parsing.rhs_end_pos 12 in
  let fundec_loc = create_loc fundec_st fundec_end in
  let fundec = FunctionDec { functionDec_location = fundec_loc;
                             functionDec_symbol = new_symbol $8;
                             functionDec_args = args_var_array;
                             functionDec_returns = ret_var_array;
                             functionDec_body = $11 } in
  create_exp fundec_loc (Dec fundec) }
| FUNCTION LBRACK functionDeclarationReturns RBRACK ASSIGN ID functionDeclarationArguments functionDeclarationBreak functionBody functionDelimiter {
  let ret_st =  Parsing.rhs_start_pos 3 in
  let ret_end = Parsing.rhs_end_pos 3 in
  let ret_loc = create_loc ret_st ret_end in
  let ret_var_array = { arrayListVar_location = ret_loc;
                        arrayListVar_vars = Array.of_list $3 } in
  let args_st = Parsing.rhs_start_pos 7 in
  let args_end = Parsing.rhs_end_pos 7 in
  let args_loc = create_loc args_st args_end in
  let args_var_array = { arrayListVar_location = args_loc;
                         arrayListVar_vars = Array.of_list $7 } in
  let fundec_st = Parsing.rhs_start_pos 1 in
  let fundec_end = Parsing.rhs_end_pos 10 in
  let fundec_loc = create_loc fundec_st fundec_end in
  let fundec = FunctionDec { functionDec_location = fundec_loc;
                             functionDec_symbol = new_symbol $6;
                             functionDec_args = args_var_array;
                             functionDec_returns = ret_var_array;
                             functionDec_body = $9 } in
  create_exp fundec_loc (Dec fundec) }
| FUNCTION LBRACK RBRACK ASSIGN ID functionDeclarationArguments functionDeclarationBreak functionBody functionDelimiter {
  let ret_st =  Parsing.rhs_start_pos 2 in
  let ret_end = Parsing.rhs_end_pos 3 in
  let ret_loc = create_loc ret_st ret_end in
  let ret_var_array = { arrayListVar_location = ret_loc;
                        arrayListVar_vars = Array.of_list [] } in
  let args_st = Parsing.rhs_start_pos 6 in
  let args_end = Parsing.rhs_end_pos 6 in
  let args_loc = create_loc args_st args_end in
  let args_var_array = { arrayListVar_location = args_loc;
                         arrayListVar_vars = Array.of_list $6 } in
  let fundec_st = Parsing.rhs_start_pos 1 in
  let fundec_end = Parsing.rhs_end_pos 9 in
  let fundec_loc = create_loc fundec_st fundec_end in
  let fundec = FunctionDec { functionDec_location = fundec_loc;
                             functionDec_symbol = new_symbol $5;
                             functionDec_args = args_var_array;
                             functionDec_returns = ret_var_array;
                             functionDec_body = $8 } in
  create_exp fundec_loc (Dec fundec) }
| FUNCTION ID functionDeclarationArguments functionDeclarationBreak functionBody functionDelimiter {
  let ret_st =  Parsing.rhs_start_pos 1 in
  let ret_end = Parsing.rhs_end_pos 6 in
  let ret_loc = create_loc ret_st ret_end in
  let ret_var_array = { arrayListVar_location = ret_loc;
                        arrayListVar_vars = Array.of_list [] } in
  let args_st = Parsing.rhs_start_pos 3 in
  let args_end = Parsing.rhs_end_pos 3 in
  let args_loc = create_loc args_st args_end in
  let args_var_array = { arrayListVar_location = args_loc;
                         arrayListVar_vars = Array.of_list $3 } in
  let fundec_st = Parsing.rhs_start_pos 1 in
  let fundec_end = Parsing.rhs_end_pos 6 in
  let fundec_loc = create_loc fundec_st fundec_end in
  let fundec = FunctionDec { functionDec_location = fundec_loc;
                             functionDec_symbol = new_symbol $2;
                             functionDec_args = args_var_array;
                             functionDec_returns = ret_var_array;
                             functionDec_body = $5 } in
  create_exp fundec_loc (Dec fundec) }

/* HIDDEN FUNCTIONS */
| hiddenFun ID ASSIGN ID functionDeclarationArguments functionDeclarationBreak functionBody functionDelimiter {
  let ret_st =  Parsing.rhs_start_pos 2 in
  let ret_end = Parsing.rhs_end_pos 2 in
  let ret_loc = create_loc ret_st ret_end in
  let var_ret = { var_location = ret_loc ;
                  var_desc = simpleVar $2 } in
  let ret_var_array = { arrayListVar_location = ret_loc;
                        arrayListVar_vars = Array.of_list [var_ret] } in
  let args_st = Parsing.rhs_start_pos 5 in
  let args_end = Parsing.rhs_end_pos 5 in
  let args_loc = create_loc args_st args_end in
  let args_var_array = { arrayListVar_location = args_loc;
                         arrayListVar_vars = Array.of_list $5 } in
  let fundec_st = Parsing.rhs_start_pos 1 in
  let fundec_end = Parsing.rhs_end_pos 8 in
  let fundec_loc = create_loc fundec_st fundec_end in
  let fundec = FunctionDec { functionDec_location = fundec_loc;
                             functionDec_symbol = new_symbol $4;
                             functionDec_args = args_var_array;
                             functionDec_returns = ret_var_array;
                             functionDec_body = $7 } in
  create_exp fundec_loc (Dec fundec) }
| hiddenFun LBRACK functionDeclarationReturns RBRACK ASSIGN ID functionDeclarationArguments functionDeclarationBreak functionBody functionDelimiter {
  let ret_st =  Parsing.rhs_start_pos 3 in
  let ret_end = Parsing.rhs_end_pos 3 in
  let ret_loc = create_loc ret_st ret_end in
  let ret_var_array = { arrayListVar_location = ret_loc;
                        arrayListVar_vars = Array.of_list $3 } in
  let args_st = Parsing.rhs_start_pos 7 in
  let args_end = Parsing.rhs_end_pos 7 in
  let args_loc = create_loc args_st args_end in
  let args_var_array = { arrayListVar_location = args_loc;
                         arrayListVar_vars = Array.of_list $7 } in
  let fundec_st = Parsing.rhs_start_pos 1 in
  let fundec_end = Parsing.rhs_end_pos 10 in
  let fundec_loc = create_loc fundec_st fundec_end in
  let fundec = FunctionDec { functionDec_location = fundec_loc;
                             functionDec_symbol = new_symbol $6;
                             functionDec_args = args_var_array;
                             functionDec_returns = ret_var_array;
                             functionDec_body = $9 } in
  create_exp fundec_loc (Dec fundec) }
| hiddenFun LBRACK RBRACK ASSIGN ID functionDeclarationArguments functionDeclarationBreak functionBody functionDelimiter {
  let ret_st =  Parsing.rhs_start_pos 2 in
  let ret_end = Parsing.rhs_end_pos 3 in
  let ret_loc = create_loc ret_st ret_end in
  let ret_var_array = { arrayListVar_location = ret_loc;
                        arrayListVar_vars = Array.of_list [] } in
  let args_st = Parsing.rhs_start_pos 6 in
  let args_end = Parsing.rhs_end_pos 6 in
  let args_loc = create_loc args_st args_end in
  let args_var_array = { arrayListVar_location = args_loc;
                         arrayListVar_vars = Array.of_list $6 } in
  let fundec_st = Parsing.rhs_start_pos 1 in
  let fundec_end = Parsing.rhs_end_pos 9 in
  let fundec_loc = create_loc fundec_st fundec_end in
  let fundec = FunctionDec { functionDec_location = fundec_loc;
                             functionDec_symbol = new_symbol $5;
                             functionDec_args = args_var_array;
                             functionDec_returns = ret_var_array;
                             functionDec_body = $8 } in
  create_exp fundec_loc (Dec fundec) }
| hiddenFun ID functionDeclarationArguments functionDeclarationBreak functionBody functionDelimiter {
  let ret_st =  Parsing.rhs_start_pos 1 in
  let ret_end = Parsing.rhs_end_pos 6 in
  let ret_loc = create_loc ret_st ret_end in
  let ret_var_array = { arrayListVar_location = ret_loc;
                        arrayListVar_vars = Array.of_list [] } in
  let args_st = Parsing.rhs_start_pos 3 in
  let args_end = Parsing.rhs_end_pos 3 in
  let args_loc = create_loc args_st args_end in
  let args_var_array = { arrayListVar_location = args_loc;
                         arrayListVar_vars = Array.of_list $3 } in
  let fundec_st = Parsing.rhs_start_pos 1 in
  let fundec_end = Parsing.rhs_end_pos 6 in
  let fundec_loc = create_loc fundec_st fundec_end in
  let fundec = FunctionDec { functionDec_location = fundec_loc;
                             functionDec_symbol = new_symbol $2;
                             functionDec_args = args_var_array;
                             functionDec_returns = ret_var_array;
                             functionDec_body = $5 } in
  create_exp fundec_loc (Dec fundec) }

functionDelimiter :
| END                                               { }
| ENDFUNCTION                                       { }

hiddenFun :
| HIDDENFUNCTION                                    { }
| HIDDEN FUNCTION                                   { }

functionDeclarationReturns :
| idList                                        { List.rev $1 }

functionDeclarationArguments :
| LPAREN idList RPAREN                          { List.rev $2 }
| LPAREN RPAREN                                 { [] }
| /* Empty */                                   { [] }

idList :
| idList COMMA ID                               { let varloc_st = Parsing.rhs_start_pos 3 in
                                                  let varloc_end = Parsing.rhs_end_pos 3 in
                                                  let varloc = create_loc varloc_st varloc_end in
                                                  let varexp = { var_location = varloc;
                                                                 var_desc = simpleVar $3 } in
                                                  varexp::$1 }
| ID                                            { let varloc_st = Parsing.rhs_start_pos 1 in
                                                  let varloc_end = Parsing.rhs_end_pos 1 in
                                                  let varloc = create_loc varloc_st varloc_end in
                                                  let varexp = { var_location = varloc;
                                                                 var_desc = simpleVar $1 } in
                                                  [varexp] }

functionDeclarationBreak :
| lineEnd			{ }
| SEMI				{ }
| SEMI EOL			{ }
| COMMA				{ }
| COMMA EOL			{ }
| COMMA COMMA                   { }

functionBody :
| expressions                    { $1 }
| /* Empty */                    { let off_st = Parsing.rhs_start_pos 1 in
                                   let off_end = Parsing.rhs_end_pos 1 in
                                   let loc =
                                     create_loc off_st off_end in
                                   create_exp loc (SeqExp []) }


condition :
| functionCall 	%prec HIGHLEVEL                 { $1 }
| variable      %prec HIGHLEVEL                 { $1 }

/* comparable OP COLON ? */
comparison :
/* & */
| variable AND variable                         { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpLogicalExp_logicalAnd in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (LogicalOpExp (oper,args))) }
| variable AND functionCall                     { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpLogicalExp_logicalAnd in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (LogicalOpExp (oper,args))) }
| functionCall AND variable                     { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpLogicalExp_logicalAnd in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (LogicalOpExp (oper,args))) }
| functionCall AND functionCall                 { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpLogicalExp_logicalAnd in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (LogicalOpExp (oper,args))) }
/*| variable AND COLON                            { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpLogicalExp_logicalAnd in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (LogicalOpExp (oper,args))) }
| functionCall AND COLON                        { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpLogicalExp_logicalAnd in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right =  create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (LogicalOpExp (oper,args))) }*/
/* | */
| variable OR variable                         { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpLogicalExp_logicalOr in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (LogicalOpExp (oper,args))) }
| variable OR functionCall                     { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpLogicalExp_logicalOr in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (LogicalOpExp (oper,args))) }
| functionCall OR variable                     { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpLogicalExp_logicalOr in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (LogicalOpExp (oper,args))) }
| functionCall OR functionCall                 { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpLogicalExp_logicalOr in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (LogicalOpExp (oper,args))) }
/*| variable OR COLON                             { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpLogicalExp_logicalOr in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (LogicalOpExp (oper,args))) }
| functionCall OR COLON                        { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpLogicalExp_logicalOr in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right =  create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (LogicalOpExp (oper,args))) }*/
/* = */
| variable EQ variable                          { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_eq in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| variable EQ functionCall                      { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_eq in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall EQ variable                      { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_eq in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall EQ functionCall                  { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_eq in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
/*| variable EQ COLON                             { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpExp_eq in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall EQ COLON                         { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpExp_eq in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right =  create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }*/
/* <> */
| variable NE variable                          { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_ne in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| variable NE functionCall                      { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_ne in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall NE variable                      { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_ne in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall NE functionCall                  { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_ne in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
/*| variable NE COLON                             { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpExp_ne in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall NE COLON                         { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpExp_ne in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right =  create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }*/
/* < */
| variable LT variable                          { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_lt in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| variable LT functionCall                      { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_lt in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall LT variable                      { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_lt in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall LT functionCall                  { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_lt in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
/*| variable LT COLON                             { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpExp_lt in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall LT COLON                         { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpExp_lt in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right =  create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }*/
/* > */
| variable GT variable                          { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_gt in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| variable GT functionCall                      { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_gt in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall GT variable                      { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_gt in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall GT functionCall                  { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_gt in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
/*| variable GT COLON                             { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpExp_gt in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall GT COLON                         { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpExp_gt in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right =  create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }*/
/* <= */
| variable LE variable                          { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_le in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| variable LE functionCall                      { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_le in
                                                  let args = { opExp_left  = $1 ;
                                                              opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall LE variable                      { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_le in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall LE functionCall                  { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_le in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
/*| variable LE COLON                             { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpExp_le in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall LE COLON                         { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpExp_le in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right =  create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }*/
/* >= */
| variable GE variable                          { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_ge in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| variable GE functionCall                      { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_ge in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall GE variable                      { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_ge in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall GE functionCall                  { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let oper = OpExp_ge in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
/*| variable GE COLON                             { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpExp_ge in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }
| functionCall GE COLON                         { let cmploc_st = Parsing.rhs_start_pos 1 in
                                                  let cmploc_end = Parsing.rhs_end_pos 3 in
                                                  let cmploc = create_loc cmploc_st cmploc_end in
                                                  let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                  let cvar_exp =
                                                    Var { var_location = loc;
                                                          var_desc = ColonVar } in
                                                  let oper = OpExp_ge in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right =  create_exp loc cvar_exp ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp cmploc (MathExp (OpExp (oper,args))) }*/

operation :
/* '+' */
| variable PLUS variable                        { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_plus in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable PLUS functionCall                    { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_plus in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall PLUS variable                    { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_plus in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall PLUS functionCall                { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_plus in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
/* '-' */
| variable MINUS variable                       { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_minus in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable MINUS functionCall                   { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_minus in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall MINUS variable                   { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_minus in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall MINUS functionCall               { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_minus in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
/* '*' '.*' '.*.' '*.' */
| variable TIMES variable                       { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_times in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable TIMES functionCall                   { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_times in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall TIMES variable                   { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_times in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall TIMES functionCall               { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_times in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable DOTTIMES variable                    { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_dottimes in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable DOTTIMES functionCall                { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_dottimes in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall DOTTIMES variable                { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_dottimes in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall DOTTIMES functionCall            { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_dottimes in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable KRONTIMES variable                   { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_krontimes in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable KRONTIMES functionCall               { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_krontimes in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall KRONTIMES variable               { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_krontimes in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall KRONTIMES functionCall           { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_krontimes in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable CONTROLTIMES variable                { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_controltimes in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable CONTROLTIMES functionCall            { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_controltimes in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall CONTROLTIMES variable            { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_controltimes in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall CONTROLTIMES functionCall        { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_controltimes in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
/* '/'   './'   './.'   '/.' */
| variable RDIVIDE variable                     { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_rdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable RDIVIDE functionCall                 { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_rdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall RDIVIDE variable                 { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_rdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall RDIVIDE functionCall             { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_rdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable DOTRDIVIDE variable                  { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_dotrdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable DOTRDIVIDE functionCall              { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_dotrdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall DOTRDIVIDE variable              { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_dotrdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall DOTRDIVIDE functionCall          { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_dotrdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable KRONRDIVIDE variable                 { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_kronrdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable KRONRDIVIDE functionCall             { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_kronrdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall KRONRDIVIDE variable             { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_kronrdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall KRONRDIVIDE functionCall         { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_kronrdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable CONTROLRDIVIDE variable                  { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_controlrdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable CONTROLRDIVIDE functionCall              { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_controlrdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall CONTROLRDIVIDE variable              { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_controlrdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall CONTROLRDIVIDE functionCall          { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_controlrdivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
/* '\'   '.\'   '.\.'   '\.' */
| variable LDIVIDE variable                     { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_ldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable LDIVIDE functionCall                 { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_ldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall LDIVIDE variable                 { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_ldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall LDIVIDE functionCall             { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_ldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable DOTLDIVIDE variable                  { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_dotldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable DOTLDIVIDE functionCall              { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_dotldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall DOTLDIVIDE variable              { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_dotldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall DOTLDIVIDE functionCall          { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_dotldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable KRONLDIVIDE variable                 { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_kronldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable KRONLDIVIDE functionCall             { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_kronldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall KRONLDIVIDE variable             { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_kronldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall KRONLDIVIDE functionCall         { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_kronldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable CONTROLLDIVIDE variable              { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_controlldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable CONTROLLDIVIDE functionCall          { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_controlldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall CONTROLLDIVIDE variable          { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_controlldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall CONTROLLDIVIDE functionCall      { let oploc_st = Parsing.rhs_start_pos 1 in
                                                  let oploc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc oploc_st oploc_end in
                                                  let oper = OpExp_controlldivide in
                                                  let args = { opExp_left  = $1 ;
                                                               opExp_right = $3 ;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
/* '^' '.^' '.^.' '^.' */
| variable POWER variable                       { let powloc_st = Parsing.rhs_start_pos 1 in
                                                  let powloc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc powloc_st powloc_end in
                                                  let oper = OpExp_power in
                                                  let left = $1 in
                                                  let right = $3 in
                                                  let args = { opExp_left  = left ;
                                                               opExp_right = right;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable POWER functionCall                   { let powloc_st = Parsing.rhs_start_pos 1 in
                                                  let powloc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc powloc_st powloc_end in
                                                  let oper = OpExp_power in
                                                  let left = $1 in
                                                  let right = $3 in
                                                  let args = { opExp_left  = left ;
                                                               opExp_right = right;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall POWER variable		       { let powloc_st = Parsing.rhs_start_pos 1 in
                                                 let powloc_end = Parsing.rhs_end_pos 3 in
                                                 let oploc = create_loc powloc_st powloc_end in
                                                 let oper = OpExp_power in
                                                 let left = $1 in
                                                 let right = $3 in
                                                 let args = { opExp_left  = left ;
                                                              opExp_right = right;
                                                              opExp_kind  = OpExp_invalid_kind } in
                                                 create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall POWER functionCall              { let powloc_st = Parsing.rhs_start_pos 1 in
                                                 let powloc_end = Parsing.rhs_end_pos 3 in
                                                 let oploc = create_loc powloc_st powloc_end in
                                                 let oper = OpExp_power in
                                                 let left = $1 in
                                                 let right = $3 in
                                                 let args = { opExp_left  = left ;
                                                              opExp_right = right;
                                                              opExp_kind  = OpExp_invalid_kind } in
                                                 create_exp oploc (MathExp (OpExp (oper,args))) }
| variable DOTPOWER variable                       { let powloc_st = Parsing.rhs_start_pos 1 in
                                                  let powloc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc powloc_st powloc_end in
                                                  let oper = OpExp_dotpower in
                                                  let left = $1 in
                                                  let right = $3 in
                                                  let args = { opExp_left  = left ;
                                                               opExp_right = right;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| variable DOTPOWER functionCall                   { let powloc_st = Parsing.rhs_start_pos 1 in
                                                  let powloc_end = Parsing.rhs_end_pos 3 in
                                                  let oploc = create_loc powloc_st powloc_end in
                                                  let oper = OpExp_dotpower in
                                                  let left = $1 in
                                                  let right = $3 in
                                                  let args = { opExp_left  = left ;
                                                               opExp_right = right;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall DOTPOWER variable		       { let powloc_st = Parsing.rhs_start_pos 1 in
                                                 let powloc_end = Parsing.rhs_end_pos 3 in
                                                 let oploc = create_loc powloc_st powloc_end in
                                                 let oper = OpExp_dotpower in
                                                 let left = $1 in
                                                 let right = $3 in
                                                 let args = { opExp_left  = left ;
                                                              opExp_right = right;
                                                              opExp_kind  = OpExp_invalid_kind } in
                                                 create_exp oploc (MathExp (OpExp (oper,args))) }
| functionCall DOTPOWER functionCall              { let powloc_st = Parsing.rhs_start_pos 1 in
                                                 let powloc_end = Parsing.rhs_end_pos 3 in
                                                 let oploc = create_loc powloc_st powloc_end in
                                                 let oper = OpExp_dotpower in
                                                 let left = $1 in
                                                 let right = $3 in
                                                 let args = { opExp_left  = left ;
                                                              opExp_right = right;
                                                              opExp_kind  = OpExp_invalid_kind } in
                                                 create_exp oploc (MathExp (OpExp (oper,args))) }
/* others */
| MINUS variable                                { let minloc_st = Parsing.rhs_start_pos 1 in
                                                  let minloc_end = Parsing.rhs_end_pos 2 in
                                                  let oploc = create_loc minloc_st minloc_end in
                                                  let oper = OpExp_unaryMinus in
                                                  let dummy_exp = DoubleExp
                                                    { doubleExp_value = 0.0;
                                                      doubleExp_bigDouble = () } in
                                                  let left = create_exp dummy_loc (ConstExp dummy_exp) in
                                                  let right = $2 in
                                                  let args = { opExp_left  = left ;
                                                               opExp_right = right;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| MINUS functionCall                            { let minloc_st = Parsing.rhs_start_pos 1 in
                                                  let minloc_end = Parsing.rhs_end_pos 2 in
                                                  let oploc = create_loc minloc_st minloc_end in
                                                  let oper = OpExp_unaryMinus in
                                                  let dummy_exp = DoubleExp
                                                    { doubleExp_value = 0.0;
                                                      doubleExp_bigDouble = () } in
                                                  let left = create_exp dummy_loc (ConstExp dummy_exp) in
                                                  let right = $2 in
                                                  let args = { opExp_left  = left ;
                                                               opExp_right = right;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  create_exp oploc (MathExp (OpExp (oper,args))) }
| PLUS variable                                 { $2 }
| PLUS functionCall                             { $2 }
| variable QUOTE			       { let tloc_st = Parsing.rhs_start_pos 1 in
                                                 let tloc_end = Parsing.rhs_end_pos 2 in
                                                 let tloc = create_loc tloc_st tloc_end in
                                                 let texp = { transposeExp_exp = $1;
                                                              transposeExp_conjugate = Conjugate} in
                                                 create_exp tloc (MathExp (TransposeExp texp)) }
| functionCall QUOTE			       { let tloc_st = Parsing.rhs_start_pos 1 in
                                                 let tloc_end = Parsing.rhs_end_pos 2 in
                                                 let tloc = create_loc tloc_st tloc_end in
                                                 let texp = { transposeExp_exp = $1;
                                                              transposeExp_conjugate = Conjugate} in
                                                 create_exp tloc (MathExp (TransposeExp texp)) }
| variable DOTQUOTE			       { let tloc_st = Parsing.rhs_start_pos 1 in
                                                 let tloc_end = Parsing.rhs_end_pos 2 in
                                                 let tloc = create_loc tloc_st tloc_end in
                                                 let texp = { transposeExp_exp = $1;
                                                              transposeExp_conjugate = NonConjugate} in
                                                 create_exp tloc (MathExp (TransposeExp texp)) }
| functionCall DOTQUOTE			       { let tloc_st = Parsing.rhs_start_pos 1 in
                                                 let tloc_end = Parsing.rhs_end_pos 2 in
                                                 let tloc = create_loc tloc_st tloc_end in
                                                 let texp = { transposeExp_exp = $1;
                                                              transposeExp_conjugate = NonConjugate} in
                                                 create_exp tloc (MathExp (TransposeExp texp)) }

/*
rightOperand :
| PLUS variable                                 { let oper = OpExp_plus in
                                                  let left = create_dummy_exp () in
                                                  let right = $2 in
                                                  let args = { opExp_left  = left ;
                                                               opExp_right = right;
                                                               opExp_kind  = OpExp_invalid_kind } in
                                                  OpExp (oper,args) }*/


listableBegin :
| COLON variable                                { $2 }
| COLON functionCall                            { $2 }

listableEnd :
| listableBegin COLON variable                  { { listExp_start = create_dummy_exp ();
                                                    listExp_step  = $1;
                                                    listExp_end   = $3 } }
| listableBegin COLON functionCall              { { listExp_start = create_dummy_exp ();
                                                    listExp_step  = $1;
                                                    listExp_end   = $3 } }
| listableBegin %prec LISTABLE                  { let step_st = Parsing.rhs_start_pos 1  in
                                                  let step_end = Parsing.rhs_start_pos 1 in
                                                  let steploc = create_loc step_st step_end in
                                                  let stepexp =
                                                    DoubleExp { doubleExp_value = 1.0;
                                                                doubleExp_bigDouble = ()} in
                                                  let step_1 = create_exp steploc (ConstExp stepexp) in
                                                  { listExp_start = create_dummy_exp ();
                                                    listExp_step  = step_1;
                                                    listExp_end   = $1 } }

variable :
| NOT variable %prec NOT	                { let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 7 in
                                                  let loc = create_loc off_st off_end in
                                                  let nexp = NotExp { notExp_exp = $2 } in
                                                  create_exp loc (MathExp nexp) }
| NOT functionCall %prec NOT	                { let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 7 in
                                                  let loc = create_loc off_st off_end in
                                                  let nexp = NotExp { notExp_exp = $2 } in
                                                  create_exp loc (MathExp nexp) }
| variable DOT ID %prec UPLEVEL	                { let varloc_st = Parsing.rhs_start_pos 3 in
                                                  let varloc_end = Parsing.rhs_end_pos 3 in
                                                  let varloc = create_loc varloc_st varloc_end in
                                                  let varexp =
                                                    Var { var_location = varloc;
                                                          var_desc = simpleVar $3 } in
                                                  let fieldexp = { fieldExp_head = $1 ;
                                                                  fieldExp_tail = create_exp varloc varexp } in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (FieldExp fieldexp) }
| variable DOT keywords %prec UPLEVEL	        { let fieldexp = { fieldExp_head = $1 ;
                                                                   fieldExp_tail = $3 } in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (FieldExp fieldexp) }
| variable DOT functionCall			{ let fieldexp = { fieldExp_head = $1 ;
                                                                   fieldExp_tail = $3 } in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (FieldExp fieldexp) }
| functionCall DOT variable			{ let fieldexp = { fieldExp_head = $1 ;
                                                                   fieldExp_tail = $3 } in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (FieldExp fieldexp) }
| functionCall DOT keywords                     { let fieldexp = { fieldExp_head = $1 ;
                                                                   fieldExp_tail = $3 } in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (FieldExp fieldexp) }
| functionCall DOT functionCall			{ let fieldexp = { fieldExp_head = $1 ;
                                                                   fieldExp_tail = $3 } in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (FieldExp fieldexp) }
| variable listableEnd	                	{ let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 7 in
                                                  let loc = create_loc off_st off_end in
                                                  let lexp = { $2 with listExp_start = $1 } in
                                                  create_exp loc (ListExp lexp) }
| functionCall listableEnd %prec UPLEVEL        { let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 7 in
                                                  let loc = create_loc off_st off_end in
                                                  let lexp = { $2 with listExp_start = $1 } in
                                                  create_exp loc (ListExp lexp) }
| matrix                                        { $1 }
| cell                                          { $1 }
| operation %prec UPLEVEL		        { $1 }
| ID %prec LISTABLE                             { let varloc_st = Parsing.rhs_start_pos 1 in
                                                  let varloc_end = Parsing.rhs_end_pos 1 in
                                                  let varloc = create_loc varloc_st varloc_end in
                                                  let varexp =
                                                    Var { var_location = varloc;
                                                          var_desc = simpleVar $1 } in
                                                  create_exp varloc varexp }
| VARINT %prec LISTABLE                         { let doubleexp =
                                                    DoubleExp { doubleExp_value = $1;
                                                                doubleExp_bigDouble = ()} in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (ConstExp doubleexp) }
| NUM %prec LISTABLE                            { let doubleexp =
                                                    DoubleExp { doubleExp_value = $1;
                                                                doubleExp_bigDouble = ()} in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (ConstExp doubleexp) }
| STR                                           { let strexp = StringExp
                                                    { stringExp_value = $1 ;
                                                      stringExp_bigString = () } in
                                                  let str_st = Parsing.rhs_start_pos 1 in
                                                  let str_end = Parsing.rhs_end_pos 1 in
                                                  let str_loc = create_loc str_st str_end in
                                                  create_exp str_loc (ConstExp strexp) }
| DOLLAR                                        { let varloc_st = Parsing.rhs_start_pos 1 in
                                                  let varloc_end = Parsing.rhs_end_pos 1 in
                                                  let varloc = create_loc varloc_st varloc_end in
                                                  let varexp =
                                                    Var { var_location = varloc;
                                                          var_desc = DollarVar } in
                                                  create_exp varloc varexp }
| COLON                                         { let varloc_st = Parsing.rhs_start_pos 1 in
                                                  let varloc_end = Parsing.rhs_end_pos 1 in
                                                  let varloc = create_loc varloc_st varloc_end in
                                                  let varexp =
                                                    Var { var_location = varloc;
                                                          var_desc = ColonVar } in
                                                  create_exp varloc varexp }
| BOOLTRUE %prec BOOLTRUE                       { let doubleexp =
                                                    BoolExp { boolExp_value = true;
                                                              boolExp_bigBool = ()} in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (ConstExp doubleexp) }
| BOOLFALSE %prec BOOLFALSE                     { let doubleexp =
                                                    BoolExp { boolExp_value = true;
                                                              boolExp_bigBool = ()} in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (ConstExp doubleexp) }
/*| LPAREN variable RPAREN			{ $2 }*/
| LPAREN variableFields RPAREN			{ $2 }
| comparison                                    { $1 }
| variable LPAREN functionArgs RPAREN           { let callexp =
                                                    { callExp_name = $1;
                                                      callExp_args = Array.of_list (List.rev $3)} in
                                                  let fcall_st = Parsing.rhs_start_pos 1 in
                                                  let fcall_end = Parsing.rhs_end_pos 4 in
                                                  let loc = create_loc fcall_st fcall_end in
                                                  create_exp loc (CallExp callexp) }
| functionCall LPAREN functionArgs RPAREN       { let callexp =
                                                    { callExp_name = $1;
                                                      callExp_args = Array.of_list (List.rev $3)} in
                                                  let fcall_st = Parsing.rhs_start_pos 1 in
                                                  let fcall_end = Parsing.rhs_end_pos 4 in
                                                  let loc = create_loc fcall_st fcall_end in
                                                  create_exp loc (CallExp callexp) }

variableField :
| variable            { $1 }
| functionCall        { $1 }

variableFields :
| variableFields COMMA variableFields           { $1 }
| variableField                                 { $1 }
| /* Empty */                                   { let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc =
                                                    create_loc off_st off_end in
                                                  create_exp loc (SeqExp []) }


/* IF THEN ELSE */
ifControl :
| IF condition thenTok thenBody END                   { let ifexp = IfExp
                                                          { ifExp_test = $2;
                                                            ifExp_then = $4;
                                                            ifExp_else = None;
                                                            ifExp_kind =
                                                              IfExp_expression_kind } in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 5 in
                                                        let loc =
                                                          create_loc off_st off_end in
                                                        create_exp loc (ControlExp ifexp) }
| IF condition thenTok thenBody elseTok elseBody END  { let ifexp = IfExp
                                                          { ifExp_test = $2;
                                                            ifExp_then = $4;
                                                            ifExp_else = $6;
                                                            ifExp_kind =
                                                              IfExp_expression_kind } in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 7 in
                                                        let loc =
                                                          create_loc off_st off_end in
                                                        create_exp loc (ControlExp ifexp) }
| IF condition thenTok thenBody elseIfControl END        { let ifexp = IfExp
                                                             { ifExp_test = $2;
                                                               ifExp_then = $4;
                                                               ifExp_else = Some $5;
                                                               ifExp_kind =
                                                                 IfExp_expression_kind } in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 7 in
                                                        let loc =
                                                          create_loc off_st off_end in
                                                        create_exp loc (ControlExp ifexp) }


thenBody :
| /* Empty */                                  { let off_st = Parsing.rhs_start_pos 1 in
                                                 let off_end = Parsing.rhs_end_pos 1 in
                                                 let loc =
                                                   create_loc off_st off_end in
                                                 create_exp loc (SeqExp []) }
| expressions                                  { $1 }


elseBody :
| /* Empty */                                  { None }
| expressions                                  { Some $1 }


ifConditionBreak :
| SEMI						{ }
| SEMI EOL					{ }
| COMMA						{ }
| COMMA EOL					{ }
| EOL						{ }


thenTok :
| THEN                                          { }
| ifConditionBreak THEN				{ }
| ifConditionBreak THEN EOL			{ }
| THEN ifConditionBreak				{ }
| ifConditionBreak				{ }
| /* Empty */                                   { }


elseTok :
| ELSE						{ }
| ELSE COMMA					{ }
| ELSE SEMI					{ }
| ELSE EOL					{ }
| ELSE COMMA EOL				{ }
| ELSE SEMI EOL					{ }

elseIfControl :
| ELSEIF condition thenTok thenBody                   { let ifexp =
                                                          ControlExp
                                                            (IfExp
                                                               { ifExp_test = $2;
                                                                 ifExp_then = $4;
                                                                 ifExp_else = None;
                                                                 ifExp_kind =
                                                                   IfExp_expression_kind }) in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 4 in
                                                        let loc =
                                                          create_loc off_st off_end in
                                                        create_exp loc
                                                          (SeqExp [create_exp loc ifexp]) }
| ELSEIF condition thenTok thenBody elseTok elseBody  { let ifexp =
                                                          ControlExp
                                                            (IfExp
                                                               { ifExp_test = $2;
                                                                 ifExp_then = $4;
                                                                 ifExp_else = $6;
                                                                 ifExp_kind =
                                                                   IfExp_expression_kind }) in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 6 in
                                                        let loc =
                                                          create_loc off_st off_end in
                                                        create_exp loc
                                                          (SeqExp [create_exp loc ifexp]) }
| ELSEIF condition thenTok thenBody elseIfControl     { let ifexp = ControlExp
                                                          (IfExp
                                                             { ifExp_test = $2;
                                                               ifExp_then = $4;
                                                               ifExp_else = Some $5;
                                                               ifExp_kind =
                                                                 IfExp_expression_kind }) in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 6 in
                                                        let loc =
                                                          create_loc off_st off_end in
                                                        create_exp loc
                                                          (SeqExp [create_exp loc ifexp]) }

/* SELECT */
selectControl :
| select selectable selectConditionBreak casesControl END                              { let select_exp = SelectExp
                                                                                           { selectExp_selectme = $2 ;
                                                                                             selectExp_cases    = Array.of_list (List.rev $4) ;
                                                                                             selectExp_default  = None } in
                                                                                         let select_st = Parsing.rhs_start_pos 1 in
                                                                                         let select_end = Parsing.rhs_end_pos 5 in
                                                                                         let select_loc = create_loc select_st select_end in
                                                                                         create_exp select_loc (ControlExp select_exp) }
| select selectable selectConditionBreak casesControl defaultCase elseBody END         { let default_st =  Parsing.rhs_start_pos 6 in
                                                                                         let default_end = Parsing.rhs_end_pos 6 in
                                                                                         let default_loc = create_loc default_st default_end in
                                                                                         let default =
                                                                                           match $6 with
                                                                                             | None -> None
                                                                                             | Some seqexp -> Some (default_loc,[seqexp]) in
                                                                                         let select_exp = SelectExp
                                                                                           { selectExp_selectme = $2 ;
                                                                                             selectExp_cases    = Array.of_list (List.rev $4) ;
                                                                                             selectExp_default  = default } in
                                                                                         let select_st = Parsing.rhs_start_pos 1 in
                                                                                         let select_end = Parsing.rhs_end_pos 7 in
                                                                                         let select_loc = create_loc select_st select_end in
                                                                                         create_exp select_loc (ControlExp select_exp) }
| select selectable COMMENT selectConditionBreak casesControl END                      { let select_exp = SelectExp
                                                                                           { selectExp_selectme = $2 ;
                                                                                             selectExp_cases    = Array.of_list (List.rev $5) ;
                                                                                             selectExp_default  = None } in
                                                                                         let select_st = Parsing.rhs_start_pos 1 in
                                                                                         let select_end = Parsing.rhs_end_pos 6 in
                                                                                         let select_loc = create_loc select_st select_end in
                                                                                         create_exp select_loc (ControlExp select_exp) }
| select selectable COMMENT selectConditionBreak casesControl defaultCase elseBody END { let default_st =  Parsing.rhs_start_pos 7 in
                                                                                         let default_end = Parsing.rhs_end_pos 7 in
                                                                                         let default_loc = create_loc default_st default_end in
                                                                                         let default =  match $7 with
                                                                                           | None -> None
                                                                                           | Some seqexp -> Some (default_loc,[seqexp]) in
                                                                                         let select_exp = SelectExp
                                                                                           { selectExp_selectme = $2 ;
                                                                                             selectExp_cases    = Array.of_list (List.rev $5) ;
                                                                                             selectExp_default  = default } in
                                                                                         let select_st = Parsing.rhs_start_pos 1 in
                                                                                         let select_end = Parsing.rhs_end_pos 8 in
                                                                                         let select_loc = create_loc select_st select_end in
                                                                                         create_exp select_loc (ControlExp select_exp) }

select :
| SELECT                           { }
| SWITCH                           { }

defaultCase :
| elseTok                          { }
| OTHERWISE                        { }
| OTHERWISE COMMA                  { }
| OTHERWISE SEMI                   { }
| OTHERWISE EOL                    { }
| OTHERWISE COMMA EOL              { }
| OTHERWISE SEMI EOL               { }

selectable :
| variable                         { $1 }
| functionCall                     { $1 }

selectConditionBreak :
| EOL                              { }
| COMMA                            { }
| SEMI                             { }
| COMMA EOL                        { }
| SEMI EOL                         { }

casesControl :
| CASE variable caseControlBreak caseBody                  { let casetest_st = Parsing.rhs_start_pos 2 in
                                                             let casetest_end = Parsing.rhs_end_pos 2 in
                                                             let casetest_loc = create_loc casetest_st casetest_end in
                                                             let casebody_st = Parsing.rhs_start_pos 4 in
                                                             let casebody_end = Parsing.rhs_end_pos 4 in
                                                             let casebody_loc = create_loc casebody_st casebody_end in
                                                             let casexp = { caseExp_location = casetest_loc ;
                                                                            caseExp_test = $2 ;
                                                                            caseExp_body_location = casebody_loc ;
                                                                            caseExp_body = $4 } in
                                                             [casexp] }
| CASE functionCall caseControlBreak caseBody              { let casetest_st = Parsing.rhs_start_pos 2 in
                                                             let casetest_end = Parsing.rhs_end_pos 2 in
                                                             let casetest_loc = create_loc casetest_st casetest_end in
                                                             let casebody_st = Parsing.rhs_start_pos 4 in
                                                             let casebody_end = Parsing.rhs_end_pos 4 in
                                                             let casebody_loc = create_loc casebody_st casebody_end in
                                                             let casexp = { caseExp_location = casetest_loc ;
                                                                            caseExp_test = $2 ;
                                                                            caseExp_body_location = casebody_loc ;
                                                                            caseExp_body = $4 } in
                                                             [casexp] }
| comments CASE variable caseControlBreak caseBody         { let casetest_st = Parsing.rhs_start_pos 3 in
                                                             let casetest_end = Parsing.rhs_end_pos 3 in
                                                             let casetest_loc = create_loc casetest_st casetest_end in
                                                             let casebody_st = Parsing.rhs_start_pos 5 in
                                                             let casebody_end = Parsing.rhs_end_pos 5 in
                                                             let casebody_loc = create_loc casebody_st casebody_end in
                                                             let casexp = { caseExp_location = casetest_loc ;
                                                                            caseExp_test = $3 ;
                                                                            caseExp_body_location = casebody_loc ;
                                                                            caseExp_body = $5 } in
                                                             [casexp] }
| comments CASE functionCall caseControlBreak caseBody     { let casetest_st = Parsing.rhs_start_pos 3 in
                                                             let casetest_end = Parsing.rhs_end_pos 3 in
                                                             let casetest_loc = create_loc casetest_st casetest_end in
                                                             let casebody_st = Parsing.rhs_start_pos 5 in
                                                             let casebody_end = Parsing.rhs_end_pos 5 in
                                                             let casebody_loc = create_loc casebody_st casebody_end in
                                                             let casexp = { caseExp_location = casetest_loc ;
                                                                            caseExp_test = $3 ;
                                                                            caseExp_body_location = casebody_loc ;
                                                                            caseExp_body = $5 } in
                                                             [casexp] }
| casesControl CASE variable caseControlBreak caseBody     { let casetest_st = Parsing.rhs_start_pos 3 in
                                                             let casetest_end = Parsing.rhs_end_pos 3 in
                                                             let casetest_loc = create_loc casetest_st casetest_end in
                                                             let casebody_st = Parsing.rhs_start_pos 5 in
                                                             let casebody_end = Parsing.rhs_end_pos 5 in
                                                             let casebody_loc = create_loc casebody_st casebody_end in
                                                             let casexp = { caseExp_location = casetest_loc ;
                                                                            caseExp_test = $3 ;
                                                                            caseExp_body_location = casebody_loc ;
                                                                            caseExp_body = $5 } in
                                                             casexp::$1 }
| casesControl CASE functionCall caseControlBreak caseBody { let casetest_st = Parsing.rhs_start_pos 3 in
                                                             let casetest_end = Parsing.rhs_end_pos 3 in
                                                             let casetest_loc = create_loc casetest_st casetest_end in
                                                             let casebody_st = Parsing.rhs_start_pos 5 in
                                                             let casebody_end = Parsing.rhs_end_pos 5 in
                                                             let casebody_loc = create_loc casebody_st casebody_end in
                                                             let casexp = { caseExp_location = casetest_loc ;
                                                                            caseExp_test = $3 ;
                                                                            caseExp_body_location = casebody_loc ;
                                                                            caseExp_body = $5 } in
                                                             casexp::$1 }
caseBody :
| expressions                      {
  match $1.exp_desc with
    | SeqExp l -> l
    | _ -> [] }
| /* Empty */                      { [] }

caseControlBreak :
| THEN                             { }
| COMMA                            { }
| SEMI                             { }
| EOL                              { }
| THEN EOL                         { }
| COMMA EOL                        { }
| SEMI EOL                         { }
| THEN COMMA                       { }
| THEN COMMA EOL                   { }
| THEN SEMI                        { }
| THEN SEMI EOL                    { }
| /* Epsilon */ %prec CONTROLBREAK { }


/* FOR */
forControl :
| FOR ID ASSIGN forIterator forConditionBreak forBody END               { let vardec_st = Parsing.rhs_start_pos 2 in
                                                                          let vardec_end = Parsing.rhs_end_pos 2 in
                                                                          let vardec_loc = create_loc vardec_st vardec_end in
                                                                          let vardec_desc =
                                                                            { varDec_name = new_symbol $2;
                                                                              varDec_init = $4;
                                                                              varDec_kind = VarDec_invalid_kind} in
                                                                          let forexp = ForExp
                                                                            { forExp_vardec_location = vardec_loc;
                                                                              forExp_vardec = vardec_desc;
                                                                              forExp_body = $6 } in
                                                                          let off_st = Parsing.rhs_start_pos 1 in
                                                                          let off_end = Parsing.rhs_end_pos 7 in
                                                                          let loc = create_loc off_st off_end in
                                                                          create_exp loc (ControlExp forexp) }
| FOR LPAREN ID ASSIGN forIterator RPAREN forConditionBreak forBody END { let vardec_st = Parsing.rhs_start_pos 3 in
                                                                          let vardec_end = Parsing.rhs_end_pos 3 in
                                                                          let vardec_loc = create_loc vardec_st vardec_end in
                                                                          let vardec_desc =
                                                                            { varDec_name = new_symbol $3;
                                                                              varDec_init = $5;
                                                                              varDec_kind = VarDec_invalid_kind} in
                                                                          let forexp = ForExp
                                                                            { forExp_vardec_location = vardec_loc;
                                                                              forExp_vardec = vardec_desc;
                                                                              forExp_body = $8 } in
                                                                          let off_st = Parsing.rhs_start_pos 1 in
                                                                          let off_end = Parsing.rhs_end_pos 9 in
                                                                          let loc = create_loc off_st off_end in
                                                                          create_exp loc (ControlExp forexp)}

forIterator :
| functionCall  %prec UPLEVEL                   { $1 }
| variable %prec UPLEVEL                        { $1 }

forConditionBreak :
| EOL						{ }
| SEMI						{ }
| SEMI EOL					{ }
| COMMA						{ }
| COMMA EOL					{ }
| DO						{ }
| DO EOL					{ }
| /* Empty */					{ }

forBody :
| expressions                                   { $1 }
| /* Empty */                                   { let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc =
                                                    create_loc off_st off_end in
                                                  create_exp loc (SeqExp []) }

/* WHILE */
whileControl :
| WHILE condition whileConditionBreak whileBody END                       { let wexp =
                                                                              WhileExp
                                                                                { whileExp_test = $2;
                                                                                  whileExp_body = $4 } in
                                                                            let off_st = Parsing.rhs_start_pos 1 in
                                                                            let off_end = Parsing.rhs_end_pos 7 in
                                                                            let loc = create_loc off_st off_end in
                                                                            let controlexp = create_exp loc (ControlExp wexp) in
                                                                            let seqexp = SeqExp (controlexp::[]) in
                                                                            let off_st = Parsing.rhs_start_pos 1 in
                                                                            let off_end = Parsing.rhs_end_pos 5 in
                                                                            let loc = create_loc off_st off_end in
                                                                            create_exp loc seqexp }
| WHILE condition whileConditionBreak whileBody elseTok elseWhileBody END { let wexp =
                                                                              WhileExp
                                                                                { whileExp_test = $2;
                                                                                  whileExp_body = $4 } in
                                                                            let off_st = Parsing.rhs_start_pos 1 in
                                                                            let off_end = Parsing.rhs_end_pos 7 in
                                                                            let loc = create_loc off_st off_end in
                                                                            let controlexp = create_exp loc (ControlExp wexp) in
                                                                            let seqexp = SeqExp (controlexp::$6) in
                                                                            let off_st = Parsing.rhs_start_pos 1 in
                                                                            let off_end = Parsing.rhs_end_pos 7 in
                                                                            let loc = create_loc off_st off_end in
                                                                            create_exp loc seqexp }

whileBody :
| /* Empty */           { let off_st = Parsing.rhs_start_pos 1 in
                          let off_end = Parsing.rhs_end_pos 1 in
                          let loc =
                            create_loc off_st off_end in
                          create_exp loc (SeqExp []) }
| expressions           { $1 }

elseWhileBody :
| /* Empty */           { [] }
| expressions           { match $1.exp_desc with | SeqExp l -> l | _ -> [] }

whileConditionBreak :
| COMMA                 { }
| SEMI                  { }
| DO                    { }
| DO COMMA              { }
| DO SEMI               { }
| THEN                  { }
| THEN COMMA            { }
| THEN SEMI             { }
| COMMENT EOL           { }
| EOL                   { }
| COMMA EOL             { }
| SEMI EOL              { }
| DO EOL                { }
| DO COMMA EOL          { }
| DO SEMI EOL           { }
| THEN EOL              { }
| THEN COMMA EOL        { }
| THEN SEMI EOL         { }

/* TRY */
tryControl :
| tryTok catchBody catchTok catchBody END          { let test_loc_st = Parsing.rhs_start_pos 2 in
                                                     let test_loc_end = Parsing.rhs_end_pos 2 in
                                                     let test_loc = create_loc test_loc_st test_loc_end in
                                                     let body_loc_st = Parsing.rhs_start_pos 4 in
                                                     let body_loc_end = Parsing.rhs_start_pos 4 in
                                                     let body_loc = create_loc body_loc_st body_loc_end in
                                                     let tryexp =
                                                       TryCatchExp
                                                         { tryCatchExp_tryme_location = test_loc;
                                                           tryCatchExp_tryme = $2;
                                                           tryCatchExp_catchme_location = body_loc;
                                                           tryCatchExp_catchme = $4} in
                                                     let off_st = Parsing.rhs_start_pos 1 in
                                                     let off_end = Parsing.rhs_end_pos 5 in
                                                     let loc = create_loc off_st off_end in
                                                     create_exp loc (ControlExp tryexp) }
| tryTok catchBody END                             { let test_loc_st = Parsing.rhs_start_pos 2 in
                                                     let test_loc_end = Parsing.rhs_end_pos 2 in
                                                     let test_loc = create_loc test_loc_st test_loc_end in
                                                     let off_st = Parsing.rhs_start_pos 1 in
                                                     let off_end = Parsing.rhs_end_pos 3 in
                                                     let loc = create_loc off_st off_end in
                                                     let tryexp =
                                                       TryCatchExp
                                                         { tryCatchExp_tryme_location = test_loc;
                                                           tryCatchExp_tryme = $2;
                                                           tryCatchExp_catchme_location = loc;
                                                           tryCatchExp_catchme = []} in
                                                     create_exp loc (ControlExp tryexp) }

tryTok :
| TRY                                              { }
| TRY SEMI                                         { }
| TRY COMMA                                        { }
| TRY EOL                                          { }
| TRY SEMI EOL                                     { }
| TRY COMMA EOL                                    { }

catchTok :
| CATCH                                            { }
| CATCH SEMI                                       { }
| CATCH COMMA                                      { }
| CATCH EOL                                        { }
| CATCH SEMI EOL                                   { }
| CATCH COMMA EOL                                  { }

catchBody :
| expressions                                      { match $1.exp_desc with | SeqExp l -> l | _ -> [] }
/*| EOL expressions                                  { match $2.exp_desc with | SeqExp l -> l | _ -> [] }
| COMMA expressions                                { match $2.exp_desc with | SeqExp l -> l | _ -> [] }
| EOL                                              { [] }*/
| /* Empty */                                      { [] }



/* RETURN */
returnControl :
| RETURN                                           { let off_st = Parsing.rhs_start_pos 1 in
                                                     let off_end = Parsing.rhs_end_pos 1 in
                                                     let loc = create_loc off_st off_end in
                                                     let retexp =
                                                       ReturnExp { returnExp_exp = None} in
                                                     create_exp loc (ControlExp retexp) }
| RETURN variable                                  { let off_st = Parsing.rhs_start_pos 1 in
                                                     let off_end = Parsing.rhs_end_pos 1 in
                                                     let loc = create_loc off_st off_end in
                                                     let retexp =
                                                       ReturnExp { returnExp_exp = Some $2} in
                                                     create_exp loc (ControlExp retexp) }


/* CELL */
cell :
| LBRACE matrixOrCellLines RBRACE                         { let mle = Array.of_list (List.rev $2) in
                                                            let mathexp = CellExp { matrixExp_lines = mle } in
                                                            let off_st = Parsing.rhs_start_pos 1 in
                                                            let off_end = Parsing.rhs_end_pos 3 in
                                                            let loc = create_loc off_st off_end in
                                                            create_exp loc (MathExp mathexp) }
| LBRACE matrixOrCellLines matrixOrCellColumns RBRACE     { let st_line = Parsing.rhs_start_pos 3 in
                                                            let end_line = Parsing.rhs_end_pos 3 in
                                                            let loc_line = create_loc st_line end_line in
                                                            let col =
                                                              { matrixLineExp_location = loc_line;
                                                                matrixLineExp_columns = Array.of_list $3 } in
                                                            let mle = Array.of_list (List.rev (col::$2)) in
                                                            let mathexp = CellExp { matrixExp_lines = mle } in
                                                            let off_st = Parsing.rhs_start_pos 1 in
                                                            let off_end = Parsing.rhs_end_pos 4 in
                                                            let loc = create_loc off_st off_end in
                                                            create_exp loc (MathExp mathexp) }
| LBRACE EOL matrixOrCellLines matrixOrCellColumns RBRACE { let st_line = Parsing.rhs_start_pos 4 in
                                                            let end_line = Parsing.rhs_end_pos 4 in
                                                            let loc_line = create_loc st_line end_line in
                                                            let col =
                                                              { matrixLineExp_location = loc_line;
                                                                matrixLineExp_columns = Array.of_list $4 } in
                                                            let mle = Array.of_list (List.rev (col::$3)) in
                                                            let mathexp = CellExp { matrixExp_lines = mle } in
                                                            let off_st = Parsing.rhs_start_pos 1 in
                                                            let off_end = Parsing.rhs_end_pos 5 in
                                                            let loc = create_loc off_st off_end in
                                                            create_exp loc (MathExp mathexp) }
| LBRACE EOL matrixOrCellLines RBRACE                     { let mle = Array.of_list (List.rev $3) in
                                                            let mathexp = CellExp { matrixExp_lines = mle } in
                                                            let off_st = Parsing.rhs_start_pos 1 in
                                                            let off_end = Parsing.rhs_end_pos 4 in
                                                            let loc = create_loc off_st off_end in
                                                            create_exp loc (MathExp mathexp) }
| LBRACE matrixOrCellColumns RBRACE                       { let st_line = Parsing.rhs_start_pos 2 in
                                                            let end_line = Parsing.rhs_end_pos 2 in
                                                            let loc_line = create_loc st_line end_line in
                                                            let mlec =
                                                              { matrixLineExp_location = loc_line;
                                                                matrixLineExp_columns = Array.of_list (List.rev $2) } in
                                                            let mle = Array.of_list [mlec] in
                                                            let mathexp =
                                                              CellExp { matrixExp_lines = mle } in
                                                            let off_st = Parsing.rhs_start_pos 1 in
                                                            let off_end = Parsing.rhs_end_pos 3 in
                                                            let loc = create_loc off_st off_end in
                                                            create_exp loc (MathExp mathexp) }
| LBRACE EOL matrixOrCellColumns RBRACE			  { let st_line = Parsing.rhs_start_pos 3 in
                                                            let end_line = Parsing.rhs_end_pos 3 in
                                                            let loc_line = create_loc st_line end_line in
                                                            let mlec =
                                                              { matrixLineExp_location = loc_line;
                                                                matrixLineExp_columns = Array.of_list (List.rev $3) } in
                                                            let mle = Array.of_list [mlec] in
                                                            let mathexp =
                                                              CellExp { matrixExp_lines = mle } in
                                                            let off_st = Parsing.rhs_start_pos 1 in
                                                            let off_end = Parsing.rhs_end_pos 4 in
                                                            let loc = create_loc off_st off_end in
                                                            create_exp loc (MathExp mathexp) }
| LBRACE EOL RBRACE                                       { let mle = Array.of_list [] in
                                                            let mathexp = CellExp { matrixExp_lines = mle } in
                                                            let off_st = Parsing.rhs_start_pos 1 in
                                                            let off_end = Parsing.rhs_end_pos 3 in
                                                            let loc = create_loc off_st off_end in
                                                            create_exp loc (MathExp mathexp) }
| LBRACE RBRACE                                           { let mle = Array.of_list [] in
                                                            let mathexp = CellExp { matrixExp_lines = mle } in
                                                            let off_st = Parsing.rhs_start_pos 1 in
                                                            let off_end = Parsing.rhs_end_pos 2 in
                                                            let loc = create_loc off_st off_end in
                                                            create_exp loc (MathExp mathexp) }

/* Matrix */

matrix :
| LBRACK matrixOrCellLines RBRACK             { let mle = Array.of_list (List.rev $2) in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 3 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
| LBRACK EOL matrixOrCellLines RBRACK         { let mle = Array.of_list (List.rev $3) in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 4 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
| LBRACK matrixOrCellColumns RBRACK           { let st_line = Parsing.rhs_start_pos 2 in
                                                let end_line = Parsing.rhs_end_pos 2 in
                                                let loc_line = create_loc st_line end_line in
                                                let mlec =
                                                  { matrixLineExp_location = loc_line;
                                                    matrixLineExp_columns = Array.of_list (List.rev $2) } in
                                                let mle = Array.of_list [mlec] in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 3 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
| LBRACK EOL matrixOrCellColumns RBRACK        { let st_line = Parsing.rhs_start_pos 3 in
                                                 let end_line = Parsing.rhs_end_pos 3 in
                                                 let loc_line = create_loc st_line end_line in
                                                 let mlec =
                                                   { matrixLineExp_location = loc_line;
                                                     matrixLineExp_columns = Array.of_list (List.rev $3) } in
                                                 let mle = Array.of_list [mlec] in
                                                 let mathexp =
                                                   MatrixExp { matrixExp_lines = mle } in
                                                 let off_st = Parsing.rhs_start_pos 1 in
                                                 let off_end = Parsing.rhs_end_pos 4 in
                                                 let loc = create_loc off_st off_end in
                                                 create_exp loc (MathExp mathexp) }
| LBRACK matrixOrCellLines matrixOrCellColumns RBRACK
                                              { let st_line = Parsing.rhs_start_pos 3 in
                                                let end_line = Parsing.rhs_end_pos 3 in
                                                let loc_line = create_loc st_line end_line in
                                                let col =
                                                  { matrixLineExp_location = loc_line;
                                                    matrixLineExp_columns = Array.of_list (List.rev $3) } in
                                                let mle = Array.of_list (List.rev (col::$2)) in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 4 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
| LBRACK EOL matrixOrCellLines matrixOrCellColumns RBRACK
                                              { let st_line = Parsing.rhs_start_pos 3 in
                                                let end_line = Parsing.rhs_end_pos 3 in
                                                let loc_line = create_loc st_line end_line in
                                                let col =
                                                  { matrixLineExp_location = loc_line;
                                                    matrixLineExp_columns = Array.of_list (List.rev $4) } in
                                                let mle = Array.of_list (List.rev (col::$3)) in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 4 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
| LBRACK EOL RBRACK                           { let mle =
                                                  (Array.of_list []:matrixLineExp array) in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 3 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
| LBRACK RBRACK                               { let mle =
                                                  (Array.of_list []:matrixLineExp array) in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 2 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }


matrixOrCellLines: /* Use of list then cast to array */
| matrixOrCellLines matrixOrCellLine	              { $2::$1 }
| matrixOrCellLine                                    { [$1]}


matrixOrCellLineBreak :
| SEMI                                                          {  }
| EOL                                                           {  }
| matrixOrCellLineBreak EOL                                     {  }
| matrixOrCellLineBreak SEMI                                    {  }


matrixOrCellLine :
| matrixOrCellColumns matrixOrCellLineBreak                          { let st_line = Parsing.rhs_start_pos 1 in
                                                                       let end_line = Parsing.rhs_end_pos 1 in
                                                                       let loc_line = create_loc st_line end_line in
                                                                       { matrixLineExp_location = loc_line;
                                                                         matrixLineExp_columns =
                                                                           Array.of_list (List.rev $1) } }
| matrixOrCellColumns matrixOrCellColumnsBreak matrixOrCellLineBreak { let st_line = Parsing.rhs_start_pos 1 in
                                                                       let end_line = Parsing.rhs_end_pos 1 in
                                                                       let loc_line = create_loc st_line end_line in
                                                                       { matrixLineExp_location = loc_line;
                                                                         matrixLineExp_columns =
                                                                           Array.of_list (List.rev $1) } }

matrixOrCellColumns :
| matrixOrCellColumns matrixOrCellColumnsBreak variable %prec HIGHLEVEL       { $3::$1 }
| matrixOrCellColumns matrixOrCellColumnsBreak functionCall %prec HIGHLEVEL   { $3::$1 }
| matrixOrCellColumns variable %prec HIGHLEVEL                                { $2::$1 }
| matrixOrCellColumns functionCall %prec HIGHLEVEL                            { $2::$1 }
| matrixOrCellColumns COMMENT %prec HIGHLEVEL                                 { let commentexp = CommentExp { commentExp_comment = $2 } in
                                                                                let cmt_st = Parsing.rhs_start_pos 2 in
                                                                                let cmt_end = Parsing.rhs_end_pos 2 in
                                                                                let cmt_loc = create_loc cmt_st cmt_end in
                                                                                let cmt_exp = create_exp cmt_loc (ConstExp commentexp) in
                                                                                cmt_exp::$1 }
| variable %prec HIGHLEVEL                                                    { [$1] }
| functionCall %prec HIGHLEVEL                                                { [$1] }
| COMMENT %prec HIGHLEVEL                                                     { let commentexp = CommentExp { commentExp_comment = $1 } in
                                                                                let cmt_st = Parsing.rhs_start_pos 1 in
                                                                                let cmt_end = Parsing.rhs_end_pos 1 in
                                                                                let cmt_loc = create_loc cmt_st cmt_end in
                                                                                let cmt_exp = create_exp cmt_loc (ConstExp commentexp) in
                                                                                [cmt_exp] }

matrixOrCellColumnsBreak :
| matrixOrCellColumnsBreak COMMA				{  }
| COMMA								{  }


/* VARAIABLE DECLARATION */
variableDeclaration :
| assignable ASSIGN variable %prec HIGHLEVEL                    { let assignexp =
                                                                    AssignExp {assignExp_left_exp = $1;
                                                                               assignExp_right_exp = $3 } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc assignexp }
| assignable ASSIGN functionCall %prec HIGHLEVEL                { let assignexp =
                                                                    AssignExp {assignExp_left_exp = $1;
                                                                               assignExp_right_exp = $3 } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc assignexp }
| functionCall ASSIGN variable %prec HIGHLEVEL                  { let assignexp =
                                                                    AssignExp {assignExp_left_exp = $1;
                                                                               assignExp_right_exp = $3 } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc assignexp }
| functionCall ASSIGN functionCall %prec HIGHLEVEL              { let assignexp =
                                                                    AssignExp {assignExp_left_exp = $1;
                                                                               assignExp_right_exp = $3 } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc assignexp }
/*| assignable ASSIGN COLON                                       { let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                                  let cvar_exp =
                                                                    Var { var_location = loc;
                                                                          var_desc = ColonVar } in
                                                                  let assignexp =
                                                                    AssignExp {assignExp_left_exp = $1;
                                                                               assignExp_right_exp = create_exp loc cvar_exp } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc assignexp }
| functionCall ASSIGN COLON                                       { let cvarloc_st = Parsing.rhs_start_pos 1 in
                                                                  let cvarloc_end = Parsing.rhs_end_pos 1 in
                                                                  let loc = create_loc cvarloc_st cvarloc_end in
                                                                  let cvar_exp =
                                                                    Var { var_location = loc;
                                                                          var_desc = ColonVar } in
                                                                  let assignexp =
                                                                    AssignExp {assignExp_left_exp = $1;
                                                                               assignExp_right_exp = create_exp loc cvar_exp } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc assignexp }*/
| assignable ASSIGN returnControl                               { let assignexp =
                                                                    AssignExp {assignExp_left_exp = $1;
                                                                               assignExp_right_exp = $3 } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc assignexp }
| functionCall ASSIGN returnControl                             { let assignexp =
                                                                    AssignExp {assignExp_left_exp = $1;
                                                                               assignExp_right_exp = $3 } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc assignexp }

assignable :
| variable DOT ID %prec UPLEVEL	                                { let varloc_st = Parsing.rhs_start_pos 3 in
                                                                  let varloc_end = Parsing.rhs_end_pos 3 in
                                                                  let varloc = create_loc varloc_st varloc_end in
                                                                  let varexp =
                                                                    Var { var_location = varloc;
                                                                          var_desc = simpleVar $3 } in
                                                                  let fieldexp = { fieldExp_head = $1 ;
                                                                                   fieldExp_tail = create_exp varloc varexp } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc (FieldExp fieldexp) }
| variable DOT keywords %prec UPLEVEL	                        { let fieldexp = { fieldExp_head = $1 ;
                                                                                   fieldExp_tail = $3 } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc (FieldExp fieldexp) }
| variable DOT functionCall			                { let fieldexp = { fieldExp_head = $1 ;
                                                                                   fieldExp_tail = $3 } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc (FieldExp fieldexp) }
| functionCall DOT variable			                { let fieldexp = { fieldExp_head = $1 ;
                                                                                   fieldExp_tail = $3 } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc (FieldExp fieldexp) }
| functionCall DOT keywords                                     { let fieldexp = { fieldExp_head = $1 ;
                                                                                   fieldExp_tail = $3 } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc (FieldExp fieldexp) }
| functionCall DOT functionCall			                { let fieldexp = { fieldExp_head = $1 ;
                                                                                   fieldExp_tail = $3 } in
                                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc (FieldExp fieldexp) }
| ID %prec LISTABLE                                             { let varloc_st = Parsing.rhs_start_pos 1 in
                                                                  let varloc_end = Parsing.rhs_end_pos 1 in
                                                                  let varloc = create_loc varloc_st varloc_end in
                                                                  let varexp =
                                                                    Var { var_location = varloc;
                                                                          var_desc = simpleVar $1 } in
                                                                  create_exp varloc varexp}
| multipleResults					        { $1 }
| variable LPAREN functionArgs RPAREN                           { let callexp =
                                                                    { callExp_name = $1;
                                                                      callExp_args = Array.of_list (List.rev $3) } in
                                                                  let fcall_st = Parsing.rhs_start_pos 1 in
                                                                  let fcall_end = Parsing.rhs_end_pos 4 in
                                                                  let loc = create_loc fcall_st fcall_end in
                                                                  create_exp loc (CallExp callexp) }
| functionCall LPAREN functionArgs RPAREN                       { let callexp =
                                                                    { callExp_name = $1;
                                                                      callExp_args = Array.of_list (List.rev $3) } in
                                                                  let fcall_st = Parsing.rhs_start_pos 1 in
                                                                  let fcall_end = Parsing.rhs_end_pos 4 in
                                                                  let loc = create_loc fcall_st fcall_end in
                                                                  create_exp loc (CallExp callexp) }

multipleResults :
| LBRACK matrixOrCellColumns RBRACK		           	{ let off_st = Parsing.rhs_start_pos 1 in
                                                                  let off_end = Parsing.rhs_end_pos 3 in
                                                                  let loc = create_loc off_st off_end in
                                                                  create_exp loc (AssignListExp (Array.of_list (List.rev $2))) }

/* COMMENTS */
comments :
| COMMENT EOL                                       { }
| comments COMMENT EOL                              { }

/* LINE END */
lineEnd :
| EOL                                               { }
| COMMENT EOL                                       { }


/* To allow var.keywords contruction */
keywords :
| IF                                                { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "if" } in
                                                      create_exp varloc var }
| THEN                                              { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "then" } in
                                                      create_exp varloc var }
| ELSE                                              { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "else" } in
                                                      create_exp varloc var }
| ELSEIF                                            { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "elseif" } in
                                                      create_exp varloc var }
| END                                               { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "end" } in
                                                      create_exp varloc var }
| SELECT                                            { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "select" } in
                                                      create_exp varloc var }
| SWITCH                                            { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "switch" } in
                                                      create_exp varloc var }
| OTHERWISE                                         { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "otherwise" } in
                                                      create_exp varloc var }
| CASE                                              { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "case" } in
                                                      create_exp varloc var }
| FUNCTION                                          { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "function" } in
                                                      create_exp varloc var }
| ENDFUNCTION                                       { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "endfunction" } in
                                                      create_exp varloc var }
| HIDDENFUNCTION                                    { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "hiddenfunction" } in
                                                      create_exp varloc var }
| HIDDEN                                            { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "hidden" } in
                                                      create_exp varloc var }
| FOR                                               { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "for" } in
                                                      create_exp varloc var }
| WHILE                                             { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "while" } in
                                                      create_exp varloc var }
| DO                                                { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "do" } in
                                                      create_exp varloc var }
| BREAK                                             { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "break" } in
                                                      create_exp varloc var }
| TRY                                               { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "try" } in
                                                      create_exp varloc var }
| CATCH                                             { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "catch" } in
                                                      create_exp varloc var }
| RETURN                                            { let varloc_st = Parsing.rhs_start_pos 1 in
                                                      let varloc_end = Parsing.rhs_end_pos 1 in
                                                      let varloc = create_loc varloc_st varloc_end in
                                                      let var =
                                                        Var { var_location = varloc;
                                                              var_desc = simpleVar "return" } in
                                                      create_exp varloc var }
/*

*/















