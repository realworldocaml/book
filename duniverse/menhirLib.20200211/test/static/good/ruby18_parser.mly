%{
  module Make (A : Ast.Annotation) = struct
    module Parser_helper = Parser_helper.Make (A)
    open Logging
    open Token
    open Ast.Generic
    open Lexer_state
    open Parser_helper

    let annot pos = A.of_pos pos
%}

%token <Lexing.position>
       K_CLASS K_MODULE K_DEF K_UNDEF K_BEGIN K_RESCUE K_ENSURE K_END K_IF K_UNLESS
       K_THEN K_ELSIF K_ELSE K_CASE K_WHEN K_WHILE K_UNTIL K_FOR K_BREAK K_NEXT
       K_REDO K_RETRY K_IN K_DO K_DO_COND K_DO_BLOCK K_RETURN K_YIELD K_SUPER
       K_SELF K_NIL K_TRUE K_FALSE K_AND K_OR K_NOT K_IF_MOD K_UNLESS_MOD K_WHILE_MOD
       K_UNTIL_MOD K_RESCUE_MOD K_ALIAS K_DEFINED K_lBEGIN K_lEND K__LINE__
       K__FILE__
%token <string * Lexing.position>
       IDENTIFIER FID GVAR IVAR CONSTANT CVAR
%token <string * Lexing.position> STRING_CONTENT
%token <int * Lexing.position> INTEGER
%token <float * Lexing.position> FLOAT
%token <string * Lexing.position> REGEXP_END
%token <Lexing.position>
       UPLUS UMINUS UMINUS_NUM POW CMP EQL EQ EQQ NEQ GEQ LEQ
       ANDOP OROP MATCH NMATCH DOT DOT2 DOT3 AREF ASET LSHFT RSHFT
       COLON2 COLON3
%token <string * Lexing.position> OP_ASGN
%token <Lexing.position>
       ASSOC LPAREN LPAREN2 RPAREN LPAREN_ARG
       LB LBRACK RBRACK LBRACE LBRACE_ARG STAR STAR2 AMPER AMPER2
       TILDE PERCENT DIVIDE PLUS MINUS LT GT PIPE BANG CARET
       LCURLY RCURLY BACK_REF2 SYMBEG STRING_BEG XSTRING_BEG REGEXP_BEG
       WORDS_BEG QWORDS_BEG STRING_DBEG STRING_DVAR STRING_END
       EH COLON COMMA SEMI
%token <string * Lexing.position> COMMENT
%token NL SPACE EOF

%start program
%type <A.t Ast.Generic.rb_stmt list> program

%nonassoc LOWEST
%nonassoc LBRACE_ARG
%nonassoc K_IF_MOD K_UNLESS_MOD K_WHILE_MOD K_UNTIL_MOD
%left     K_OR K_AND
%right    K_NOT
%nonassoc K_DEFINED
%right    EQL OP_ASGN
%left     K_RESCUE_MOD
%right    EH COLON
%nonassoc DOT2 DOT3
%left     OROP
%left     ANDOP
%nonassoc CMP EQ EQQ NEQ MATCH NMATCH
%left     GT GEQ LT LEQ
%left     PIPE CARET
%left     AMPER2
%left     LSHFT RSHFT
%left     PLUS MINUS
%left     STAR2 DIVIDE PERCENT
%right    UMINUS_NUM UMINUS
%right    POW
%right    BANG TILDE UPLUS

%%

         program: program_e1 compstmt
                    { $2 }
      program_e1: { state.lex_state <- Expr_beg }

        bodystmt: compstmt opt_rescue opt_else opt_ensure
                    { { body = $1;
                        body_rescues = $2;
                        body_else = $3;
                        body_ensure = $4 } }

        compstmt: stmts opt_terms
                    { $1 @ $2 }

           stmts: none
                    { [] }
                | stmt
                    { [$1] }
                | stmts terms stmt
                    { $1 @ $2 @ [$3] }
                | error stmt
                    { [$2] }

            stmt: K_ALIAS fitem
                    stmt_e1
                    fitem
                    { Alias ($2, $4, annot $1) }
                | K_ALIAS GVAR GVAR
                    { Alias (fst $2, fst $3, annot $1) }
                | K_UNDEF undef_list
                    { Undef ($2, annot $1) }
                | stmt K_IF_MOD expr_value
                    { If_mod ($1, $3, annot $2) }
                | stmt K_UNLESS_MOD expr_value
                    { Unless_mod ($1, $3, annot $2) }
                | stmt K_WHILE_MOD expr_value
                    { While_mod ($1, $3, annot $2) }
                | stmt K_UNTIL_MOD expr_value
                    { Until_mod ($1, $3, annot $2) }
                | stmt K_RESCUE_MOD stmt
                    { Rescue_mod ($1, $3, annot $2) }
                | K_lBEGIN
                    stmt_e2
                    LCURLY compstmt RCURLY
                    { Pre_exec ($4, annot $1) }
                | K_lEND LCURLY compstmt RCURLY
                    { if state.in_def > 0 || state.in_single > 0 then
                        yyerror "END in method; use at_exit";
                      Post_exec ($3, annot $1) }
                | lhs EQL command_call
                    { expr_stmt (Assign ($1, $3, Asgn_single, dummy_annot)) }
                | mlhs EQL command_call
                    { expr_stmt (Assign (Lhs_dstr $1, $3, Asgn_multi, annot $2)) }
                | var_lhs OP_ASGN command_call
                    { expr_stmt (new_op_asgn $1 (fst $2) $3 ~annot:(annot (snd $2))) }
                | primary_value LB aref_args RBRACK OP_ASGN command_call
                    { expr_stmt (new_op_asgn (Lhs_aref ($1, $3)) (fst $5) $6 ~annot:(annot (snd $5))) }
                | primary_value DOT IDENTIFIER OP_ASGN command_call
                    { expr_stmt (new_op_asgn (Lhs_attr ($1, fst $3)) (fst $4) $5 ~annot:(annot (snd $4))) }
                | primary_value DOT CONSTANT OP_ASGN command_call
                    { expr_stmt (new_op_asgn (Lhs_attr ($1, fst $3)) (fst $4) $5 ~annot:(annot (snd $4))) }
                | primary_value COLON2 IDENTIFIER OP_ASGN command_call
                    { expr_stmt (new_op_asgn (Lhs_attr ($1, fst $3)) (fst $4) $5 ~annot:(annot (snd $4))) }
                | lhs EQL mrhs
                    { expr_stmt (Assign ($1, Array ($3, dummy_annot), Asgn_svalue, dummy_annot)) }
                | mlhs EQL arg_value
                    { expr_stmt (Assign (Lhs_dstr $1, $3, Asgn_multi, annot $2)) }
                | mlhs EQL mrhs
                    { expr_stmt (Assign (Lhs_dstr $1, Array ($3, dummy_annot), Asgn_multi, annot $2)) }
                | expr
                    { expr_stmt $1 }
         stmt_e1: { state.lex_state <- Expr_fname;
                    (*result = self.lexer.lineno*)
                    0 }
         stmt_e2: { if state.in_def > 0 || state.in_single > 0 then
                      yyerror "BEGIN in method";
                    Env.extend state.env }

            expr: command_call
                    { $1 }
                | expr K_AND expr
                    { logop `And $1 $3 }
                | expr K_OR expr
                    { logop `Or $1 $3 }
                | K_NOT expr
                    { Not ($2, annot $1) }
                | BANG command_call
                    { Not ($2, annot $1) }
                | arg
                    { $1 }

      expr_value: expr
                    { $1 }

    command_call: command
                    { $1 }
                | block_command
                    { $1 }
                | K_RETURN call_args
                    { Return (ret_args $2, annot $1) }
                | K_BREAK call_args
                    { Break (ret_args $2, annot $1) }
                | K_NEXT call_args
                    { Next (ret_args $2, annot $1) }

   block_command: block_call
                    { $1 }
                | block_call DOT operation2 command_args
                    { new_call $1 (fst $3) $4 ~annot:(annot (snd $3)) }
                | block_call COLON2 operation2 command_args
                    { new_call $1 (fst $3) $4 ~annot:(annot (snd $3)) }

 cmd_brace_block: LBRACE_ARG
                    cmd_brace_block_e1
                    opt_block_var
                    compstmt RCURLY_error
                    { let blk = { blk_vars = $3; blk_body = $4 } in
                        Env.unextend state.env ;
                        blk }
cmd_brace_block_e1: { Env.extend ~dyn:true state.env;
                      (* TODO result = self.lexer.lineno*)
                      ()}

         command: operation command_args %prec LOWEST
                    { new_fcall (fst $1) $2 ~annot:(annot (snd $1)) }
                | operation command_args cmd_brace_block
                    { new_fcall (fst $1) $2 ~block:(Some $3) ~annot:(annot (snd $1)) }
                | primary_value DOT operation2 command_args %prec LOWEST
                    { new_call $1 (fst $3) $4 ~annot:(annot (snd $3)) }
                | primary_value DOT operation2 command_args cmd_brace_block
                    { new_call $1 (fst $3) $4 ~block:(Some $5) ~annot:(annot (snd $3)) }
                | primary_value COLON2 operation2 command_args %prec LOWEST
                    { new_call $1 (fst $3) $4 ~annot:(annot (snd $3)) }
                | primary_value COLON2 operation2 command_args cmd_brace_block
                    { new_call $1 (fst $3) $4 ~block:(Some $5) ~annot:(annot (snd $3)) }
                | K_SUPER command_args
                    { Super (Some $2, None, annot $1) }
                | K_YIELD command_args
                    { new_yield $2 ~annot:(annot $1) }

            mlhs: mlhs_basic
                    { $1 }
                | LPAREN mlhs_entry RPAREN
                    { $2 }

      mlhs_entry: mlhs_basic
                    { $1 }
                | LPAREN mlhs_entry RPAREN
                    { [Lhs_dstr $2] }

      mlhs_basic: mlhs_head
                    { $1 }
                | mlhs_head mlhs_item
                    { $1 @ [$2] }
                | mlhs_head STAR mlhs_node
                    { $1 @ [Lhs_rest $3] }
                | mlhs_head STAR
                    { $1 @ [Lhs_star] }
                | STAR mlhs_node
                    { [Lhs_rest $2] }
                | STAR
                    { [Lhs_star] }

       mlhs_item: mlhs_node
                    { $1 }
                | LPAREN mlhs_entry RPAREN
                    { Lhs_dstr $2 }

       mlhs_head: mlhs_item COMMA
                    { [$1] }
                | mlhs_head mlhs_item COMMA
                    { $1 @ [$2] }

       mlhs_node: variable
                    { assignable (fst $1) ~annot:(annot (snd $1)) }
                | primary_value LB aref_args RBRACK
                    { Lhs_aref ($1, $3) }
                | primary_value DOT IDENTIFIER
                    { Lhs_attr ($1, fst $3) }
                | primary_value COLON2 IDENTIFIER
                    { Lhs_attr ($1, fst $3) }
                | primary_value DOT CONSTANT
                    { Lhs_attr ($1, fst $3) }
                | primary_value COLON2 CONSTANT
                    { if state.in_def > 0 || state.in_single > 0 then
                        yyerror "dynamic constant assignment";
                      Lhs_var (Var_const (Cpath_relative ($1, fst $3))) }
                | COLON3 CONSTANT
                    { if state.in_def > 0 || state.in_single > 0 then
                        yyerror "dynamic constant assignment";
                      Lhs_var (Var_const (Cpath_absolute (Cpath_name (fst $2)))) }

             lhs: variable
                    { assignable (fst $1) ~annot:(annot (snd $1)) }
                | primary_value LB aref_args RBRACK
                    { Lhs_aref ($1, $3) }
                | primary_value DOT IDENTIFIER
                    { Lhs_attr ($1, fst $3) }
                | primary_value COLON2 IDENTIFIER
                    { Lhs_attr ($1, fst $3) }
                | primary_value DOT CONSTANT
                    { Lhs_attr ($1, fst $3) }
                | primary_value COLON2 CONSTANT
                    { if state.in_def > 0 || state.in_single > 0 then
                        yyerror "dynamic constant assignment";
                      Lhs_var (Var_const (Cpath_relative ($1, fst $3))) }
                | COLON3 CONSTANT
                    { if state.in_def > 0 || state.in_single > 0 then
                        yyerror "dynamic constant assignment";
                      Lhs_var (Var_const (Cpath_absolute (Cpath_name (fst $2)))) }

           cname: IDENTIFIER
                    { yyerror "class/module name must be CONSTANT" }
                | CONSTANT
                    { $1 }

           cpath: COLON3 cname
                    { Cpath_absolute (Cpath_name (fst $2)) }
                | cname
                    { Cpath_name (fst $1) }
                | primary_value COLON2 cname
                    { Cpath_relative ($1, fst $3) }

           fname: IDENTIFIER
                    { $1 }
                | CONSTANT
                    { $1 }
                | FID
                    { $1 }
                | op
                    { state.lex_state <- Expr_end;
                      $1 }
                | reswords
                    { state.lex_state <- Expr_end;
                      $1 }

           fitem: fname  { fst $1 }
                | symbol { fst $1 }

      undef_list: fitem
                    { [$1] }
                |
                    undef_list COMMA
                    undef_list_e1
                    fitem
                    { $1 @ [$4] }
   undef_list_e1: { state.lex_state <- Expr_fname }

              op: PIPE      { "|", $1 }
                | CARET     { "^", $1 }
                | AMPER2    { "&", $1 }
                | CMP       { "<=>", $1 }
                | EQ        { "==", $1 }
                | EQQ       { "===", $1 }
                | MATCH     { "=~", $1 }
                | GT        { ">", $1 }
                | GEQ       { ">=", $1 }
                | LT        { "<", $1 }
                | LEQ       { "<=", $1 }
                | LSHFT     { "<<", $1 }
                | RSHFT     { ">>", $1 }
                | PLUS      { "+", $1 }
                | MINUS     { "-", $1 }
                | STAR2     { "*", $1 }
                | STAR      { "*", $1 }
                | DIVIDE    { "/", $1 }
                | PERCENT   { "%", $1 }
                | POW       { "**", $1 }
                | TILDE     { "~", $1 }
                | UPLUS     { "+@", $1 }
                | UMINUS    { "-@", $1 }
                | AREF      { "[]", $1 }
                | ASET      { "[]=", $1 }
                | BACK_REF2 { "`", $1 }

        reswords: K__LINE__         { "__LINE__", $1 }
                | K__FILE__         { "__FILE__", $1 }
                | K_lBEGIN          { "BEGIN", $1 }
                | K_lEND            { "END", $1 }
                | K_ALIAS           { "alias", $1 }
                | K_AND             { "and", $1 }
                | K_BEGIN           { "begin", $1 }
                | K_BREAK           { "break", $1 }
                | K_CASE            { "case", $1 }
                | K_CLASS           { "class", $1 }
                | K_DEF             { "def", $1 }
                | K_DEFINED         { "defined", $1 }
                | K_DO              { "do", $1 }
                | K_ELSE            { "else", $1 }
                | K_ELSIF           { "elsif", $1 }
                | K_END             { "end", $1 }
                | K_ENSURE          { "ensure", $1 }
                | K_FALSE           { "false", $1 }
                | K_FOR             { "for", $1 }
                | K_IN              { "in", $1 }
                | K_MODULE          { "module", $1 }
                | K_NEXT            { "next", $1 }
                | K_NIL             { "nil", $1 }
                | K_NOT             { "not", $1 }
                | K_OR              { "or", $1 }
                | K_REDO            { "redo", $1 }
                | K_RESCUE          { "rescue", $1 }
                | K_RETRY           { "retry", $1 }
                | K_RETURN          { "return", $1 }
                | K_SELF            { "self", $1 }
                | K_SUPER           { "super", $1 }
                | K_THEN            { "then", $1 }
                | K_TRUE            { "true", $1 }
                | K_UNDEF           { "undef", $1 }
                | K_WHEN            { "when", $1 }
                | K_YIELD           { "yield", $1 }
                | K_IF_MOD          { "if", $1 }
                | K_UNLESS_MOD      { "unless", $1 }
                | K_WHILE_MOD       { "while", $1 }
                | K_UNTIL_MOD       { "until", $1 }
                | K_RESCUE_MOD      { "rescue", $1 }

             arg: lhs EQL arg
                    { Assign ($1, $3, Asgn_single, dummy_annot) }
                | lhs EQL arg K_RESCUE_MOD arg
                    { Assign ($1,
                              Begin ({ body = [expr_stmt $3];
                                       body_rescues = [[], [expr_stmt $5]];
                                       body_else = [];
                                       body_ensure = [] },
                                     annot $4),
                              Asgn_single,
                              dummy_annot) }
                | var_lhs OP_ASGN arg
                    { new_op_asgn $1 (fst $2) $3 ~annot:(annot (snd $2)) }
                | primary_value LB aref_args RBRACK OP_ASGN arg
                    { new_op_asgn (Lhs_aref ($1, $3)) (fst $5) $6 ~annot:(annot (snd $5)) }
                | primary_value DOT IDENTIFIER OP_ASGN arg
                    { new_op_asgn (Lhs_attr ($1, fst $3)) (fst $4) $5 ~annot:(annot (snd $4)) }
                | primary_value DOT CONSTANT OP_ASGN arg
                    { new_op_asgn (Lhs_attr ($1, fst $3)) (fst $4) $5 ~annot:(annot (snd $4)) }
                | primary_value COLON2 IDENTIFIER OP_ASGN arg
                    { new_op_asgn (Lhs_attr ($1, fst $3)) (fst $4) $5 ~annot:(annot (snd $4)) }
                | primary_value COLON2 CONSTANT OP_ASGN arg
                    { yyerror "constant re-assignment" }
                | COLON3 CONSTANT OP_ASGN arg
                    { yyerror "constant re-assignment" }
                | arg DOT2 arg
                    {
(* TODO
                      v1, v2 = val[0], val[2]
                      if v1.node_type == :lit and v2.node_type == :lit and Fixnum === v1.last and Fixnum === v2.last then
                        result = s(:lit, (v1.last)..(v2.last))
                      else
                        result = s(:dot2, v1, v2)
                      end
*)
                      Dot2 ($1, $3, annot $2) }
                | arg DOT3 arg
                    {
(* TODO
                      v1, v2 = val[0], val[2]
                      if v1.node_type == :lit and v2.node_type == :lit and Fixnum === v1.last and Fixnum === v2.last then
                        result = s(:lit, (v1.last)...(v2.last))
                      else
                        result = s(:dot3, v1, v2)
                      end
*)
                      Dot3 ($1, $3, annot $2) }
                | arg PLUS arg
                    { new_call $1 "+" [Arg_value $3] ~annot:(annot $2) }
                | arg MINUS arg
                    { new_call $1 "-" [Arg_value $3] ~annot:(annot $2) }
                | arg STAR2 arg
                    { new_call $1 "*" [Arg_value $3] ~annot:(annot $2) }
                | arg DIVIDE arg
                    { new_call $1 "/" [Arg_value $3] ~annot:(annot $2) }
                | arg PERCENT arg
                    { new_call $1 "%" [Arg_value $3] ~annot:(annot $2) }
                | arg POW arg
                    { new_call $1 "**" [Arg_value $3] ~annot:(annot $2) }
                | UMINUS_NUM INTEGER POW arg
                    { new_call (new_call (Literal (Lit_integer (fst $2), annot (snd $2))) "**" [Arg_value $4] ~annot:(annot $3)) "-@" [] ~annot:(annot $1) }
                | UMINUS_NUM FLOAT POW arg
                    { new_call (new_call (Literal (Lit_float (fst $2), annot (snd $2))) "**" [Arg_value $4] ~annot:(annot $3)) "-@" [] ~annot:(annot $1) }
                | UPLUS arg
                    { match $2 with
                      | Literal _ -> $2
                      | _ -> new_call $2 "+@" [] ~annot:(annot $1) }
                | UMINUS arg
                    { new_call $2 "-@" [] ~annot:(annot $1) }
                | arg PIPE arg
                    { new_call $1 "|" [Arg_value $3] ~annot:(annot $2) }
                | arg CARET arg
                    { new_call $1 "^" [Arg_value $3] ~annot:(annot $2) }
                | arg AMPER2 arg
                    { new_call $1 "&" [Arg_value $3] ~annot:(annot $2) }
                | arg CMP arg
                    { new_call $1 "<=>" [Arg_value $3] ~annot:(annot $2) }
                | arg GT arg
                    { new_call $1 ">" [Arg_value $3] ~annot:(annot $2) }
                | arg GEQ arg
                    { new_call $1 ">=" [Arg_value $3] ~annot:(annot $2) }
                | arg LT arg
                    { new_call $1 "<" [Arg_value $3] ~annot:(annot $2) }
                | arg LEQ arg
                    { new_call $1 "<=" [Arg_value $3] ~annot:(annot $2) }
                | arg EQ arg
                    { new_call $1 "==" [Arg_value $3] ~annot:(annot $2) }
                | arg EQQ arg
                    { new_call $1 "===" [Arg_value $3] ~annot:(annot $2) }
                | arg NEQ arg
                    { Not (new_call $1 "==" [Arg_value $3], dummy_annot) }
                | arg MATCH arg
                    { get_match_node $1 $3 ~annot:(annot $2) }
                | arg NMATCH arg
                    { Not (get_match_node $1 $3 ~annot:(annot $2), dummy_annot) }
                | BANG arg
                    { Not ($2, annot $1) }
                | TILDE arg
                    { new_call $2 "~" [] ~annot:(annot $1) }
                | arg LSHFT arg
                    { new_call $1 "<<" [Arg_value $3] ~annot:(annot $2) }
                | arg RSHFT arg
                    { new_call $1 ">>" [Arg_value $3] ~annot:(annot $2) }
                | arg ANDOP arg
                    { logop `And $1 $3 }
                | arg OROP arg
                    { logop `Or $1 $3 }
                | K_DEFINED opt_nl arg
                    { Defined ($3, annot $1) }
                | arg EH arg COLON arg
                    { Ternary ($1, $3, $5, Ast.annot_of_expr $1) }
                | primary
                    { $1 }

       arg_value: arg
                    { $1 }

       aref_args: none
                    { [] }
                | command opt_nl
                    { warning "parenthesize argument(s) for future version";
                      [Arg_value $1] }
                | args trailer
                    { $1 }
                | args COMMA STAR arg opt_nl
                    { $1 @ [Arg_value $4] }
                | assocs trailer
                    { [Arg_hash $1] }
                | STAR arg opt_nl
                    { [Arg_splat $2] }

      paren_args: LPAREN2 none RPAREN
                    { [] }
                | LPAREN2 call_args opt_nl RPAREN
                    { $2 }
                | LPAREN2 block_call opt_nl RPAREN
                    { warning "parenthesize argument(s) for future version";
                      [Arg_value $2] }
                | LPAREN2 args COMMA block_call opt_nl RPAREN
                    { warning "parenthesize argument(s) for future version";
                      $2 @ [Arg_value $4] }

  opt_paren_args: none
                    { [] }
                | paren_args
                    { $1 }

       call_args: command
                    { warning "parenthesize argument(s) for future version";
                      [] }
                | args opt_block_arg
                    { $1 @ $2 }
                | args COMMA STAR arg_value opt_block_arg
                    { $1 @ (Arg_splat $4 :: $5) }
                | assocs opt_block_arg
                    { Arg_hash $1 :: $2 }
                | assocs COMMA STAR arg_value opt_block_arg
                    { Arg_hash $1 :: Arg_splat $4 :: $5 }
                | args COMMA assocs opt_block_arg
                    { $1 @ (Arg_hash $3 :: $4) }
                | args COMMA assocs COMMA STAR arg opt_block_arg
                    { $1 @ (Arg_hash $3 :: Arg_splat $6 :: $7) }
                | STAR arg_value opt_block_arg
                    { Arg_splat $2 :: $3 }
                | block_arg
                    { [$1] }

      call_args2: arg_value COMMA args opt_block_arg
                    { (Arg_value $1 :: $3) @ $4 }
                | arg_value COMMA block_arg
                    { Arg_value $1 :: [$3] }
                | arg_value COMMA STAR arg_value opt_block_arg
                    { Arg_value $1 :: Arg_splat $4 :: $5 }
                | arg_value COMMA args COMMA STAR arg_value opt_block_arg
                    { (Arg_value $1 :: $3) @ (Arg_splat $6 :: $7) }
                | assocs opt_block_arg
                    { Arg_hash $1 :: $2 }
                | assocs COMMA STAR arg_value opt_block_arg
                    { Arg_hash $1 :: Arg_splat $4 :: $5 }
                | arg_value COMMA assocs opt_block_arg
                    { Arg_value $1 :: Arg_hash $3 :: $4 }
                | arg_value COMMA args COMMA assocs opt_block_arg
                    { (Arg_value $1 :: $3) @ (Arg_hash $5 :: $6) }
                | arg_value COMMA assocs COMMA STAR arg_value opt_block_arg
                    { Arg_value $1 :: Arg_hash $3 :: Arg_splat $6 :: $7 }
                | arg_value COMMA args COMMA assocs COMMA STAR arg_value opt_block_arg
                    { (Arg_value $1 :: $3) @ (Arg_hash $5 :: Arg_splat $8 :: $9) }
                | STAR arg_value opt_block_arg
                    { Arg_splat $2 :: $3 }
                | block_arg
                    { [$1] }

    command_args: command_args_e1
                    open_args
                    { state.cmdarg_stack <- Stack_state.of_list $1;
                      $2 }
 command_args_e1:   { let list = Stack_state.to_list state.cmdarg_stack in
                        Stack_state.push state.cmdarg_stack true;
                        list }

       open_args: call_args
                    { $1 }
                | LPAREN_ARG
                    open_args_e1
                    RPAREN
                    { warning "don't put space before argument parentheses";
                      [] }
                | LPAREN_ARG call_args2
                    open_args_e1
                    RPAREN
                    { warning "don't put space before argument parentheses";
                      $2 }
    open_args_e1: { state.lex_state <- Expr_endarg }

       block_arg: AMPER arg_value
                    { Arg_block $2 }

   opt_block_arg: COMMA block_arg
                    { [$2] }
                | none_block_pass
                    { [] }

            args: arg_value
                    { [Arg_value $1] }
                | args COMMA arg_value
                    { $1 @ [Arg_value $3] }

            mrhs: args COMMA arg_value
                    { $1 @ [Arg_value $3] }
                | args COMMA STAR arg_value
                    { $1 @ [Arg_splat $4] }
                | STAR arg_value
                    { [Arg_splat $2] }

         primary: literal
                    { $1 }
                | strings
                    { $1 }
                | xstring
                    { $1 }
                | regexp
                    { $1 }
                | words
                    { $1 }
                | awords
                    { $1 }
                | var_ref
                    { $1 }
                | FID
                    { new_fcall (fst $1) [] ~annot:(annot (snd $1)) }
                | K_BEGIN bodystmt K_END
                    { Begin ($2, annot $1) }
                | LPAREN_ARG expr
                    primary_e1
                    opt_nl RPAREN
                    { warning "(...) interpreted as grouped expression";
                      $2 }
                | LPAREN compstmt RPAREN
                    { match $2 with
                      | [] -> Variable (Var_pseudo Pvar_nil, annot $1)
                      | _  -> Seq ($2, annot $1) }
                | primary_value COLON2 CONSTANT
                    { Variable (Var_const (Cpath_relative ($1, fst $3)), annot (snd $3)) }
                | COLON3 CONSTANT
                    { Variable (Var_const (Cpath_absolute (Cpath_name (fst $2))), annot (snd $2)) }
                | primary_value LB aref_args RBRACK
                    { new_aref $1 $3 ~annot:(annot $2) }
                | LBRACK aref_args RBRACK
                    { Array ($2, annot $1) }
                | LBRACE assoc_list RCURLY
                    { Hash ($2, annot $1) }
                | K_RETURN
                    { Return ([], annot $1) }
                | K_YIELD LPAREN2 call_args RPAREN
                    { new_yield $3 ~annot:(annot $1) }
                | K_YIELD LPAREN2 RPAREN
                    { new_yield [] ~annot:(annot $1) }
                | K_YIELD
                    { new_yield [] ~annot:(annot $1) }
                | K_DEFINED opt_nl LPAREN2 expr RPAREN
                    { Defined ($4, annot $1) }
                | operation brace_block
                    { new_fcall (fst $1) [] ~block:(Some $2) ~annot:(annot (snd $1)) }
                | method_call
                    { $1 }
                | method_call brace_block
                    { match $1 with
                      | Call (recv, id, args, _, annot) ->
                          Call (recv, id, args, Some $2, annot)
                      | Super (args, _, annot) ->
                          Super (args, Some $2, annot)
                      | _ -> failwith "invalid method_call" }
                | K_IF expr_value then_ compstmt if_tail K_END
                    { If ($2, $4, $5, annot $1) }
                | K_UNLESS expr_value then_ compstmt opt_else K_END
                    { Unless ($2, $4, $5, annot $1) }
                | K_WHILE
                    primary_e2
                    expr_value do_
                    primary_e3
                    compstmt K_END
                    { While ($3, $6, annot $1) }
                | K_UNTIL
                    primary_e2
                    expr_value do_
                    primary_e3
                    compstmt K_END
                    { Until ($3, $6, annot $1) }
                | K_CASE expr_value opt_terms case_body opt_else K_END
                    { new_case (Some $2) $4 $5 ~annot:(annot $1) }
                | K_CASE opt_terms case_body opt_else K_END
                    { new_case None $3 $4 ~annot:(annot $1) }
                | K_CASE opt_terms K_ELSE compstmt K_END
                    { new_case None [] $4 ~annot:(annot $1) }
                | K_FOR block_var K_IN
                    primary_e2
                    expr_value do_
                    primary_e3
                    compstmt K_END
                    { For ($2, $5, $8, annot $1) }
                | K_CLASS
                    cpath superclass
                    primary_e4
                    bodystmt K_END
                    { let ret = Class ($2, $3, $5, annot $1) in
                        Env.unextend state.env;
                        ret }
                | K_CLASS LSHFT
                    expr
                    primary_e5
                    term
                    primary_e6
                    bodystmt K_END
                    { let ret = Sclass ($3, $7, annot $1) in
                        state.in_def <- $4;
                        state.in_single <- $6;
                        Env.unextend state.env;
                        ret }
                | K_MODULE
                    cpath
                    primary_e7
                    bodystmt K_END
                    { let ret = Module ($2, $4, annot $1) in
                        Env.unextend state.env;
                        ret }
                | K_DEF fname
                    primary_e8
                    f_arglist bodystmt K_END
                    { let ret = Defn (fst $2, $4, $5, annot $1) in
                        Env.unextend state.env;
                        state.in_def <- pred state.in_def;
                        ret }
                | K_DEF singleton dot_or_colon
                    primary_e9
                    fname
                    primary_e10
                    f_arglist bodystmt K_END
                    { let ret = Defs ($2, fst $5, $7, $8, annot $1) in
                        Env.unextend state.env;
                        state.in_single <- pred state.in_single;
                        ret }
                | K_BREAK
                    { Break ([], annot $1) }
                | K_NEXT
                    { Next ([], annot $1) }
                | K_REDO
                    { Redo (annot $1) }
                | K_RETRY
                    { Retry (annot $1) }
      primary_e1: { state.lex_state <- Expr_endarg }
      primary_e2: { Stack_state.push state.cond_stack true }
      primary_e3: { Stack_state.pop state.cond_stack }
      primary_e4: { if state.in_def > 0 || state.in_single > 0 then
                      yyerror "class definition in method body";
                    Env.extend state.env }
      primary_e5: { let in_def = state.in_def in
                      state.in_def <- 0;
                      in_def }
      primary_e6: { let in_single = state.in_single in
                      state.in_single <- 0;
                      Env.extend state.env;
                      in_single }
      primary_e7: { if state.in_def > 0 || state.in_single > 0 then
                      yyerror "module definition in method body";
                    Env.extend state.env }
      primary_e8: { state.in_def <- succ state.in_def;
                    Env.extend state.env }
      primary_e9: { state.lex_state <- Expr_fname }
     primary_e10: { state.in_single <- succ state.in_single;
                    Env.extend state.env;
                    state.lex_state <- Expr_end }

   primary_value: primary
                    { $1 }

           then_: term        { () }
                | COLON       { () }
                | K_THEN      { () }
                | term K_THEN { () }

             do_: term      { () }
                | COLON     { () }
                | K_DO_COND { () }

         if_tail: opt_else
                    { $1 }
                | K_ELSIF expr_value then_ compstmt if_tail
                    { [expr_stmt (If ($2, $4, $5, annot $1))] }

        opt_else: none
                    { [] }
                | K_ELSE compstmt
                    { $2 }

       block_var: lhs
                    { $1 }
                | mlhs
                    { Lhs_dstr $1 }

   opt_block_var: none
                    { [] }
                | PIPE PIPE
                    { [] }
                | OROP
                    { [] }
                | PIPE lhs PIPE
                    { [$2] }
                | PIPE mlhs PIPE
                    { $2 }

        do_block: K_DO_BLOCK
                    do_block_e1
                    opt_block_var
                    compstmt K_END_error
                    { let blk = { blk_vars = $3; blk_body = $4 } in
                        Env.unextend state.env;
                        blk }
     do_block_e1: { Env.extend ~dyn:true state.env }

      block_call: command do_block
                    { (* TODO check for block argument *)
                      match $1 with
                      | Call (recv, id, args, _, annot) ->
                          Call (recv, id, args, Some $2, annot)
                      | Super (args, _, annot) ->
                          Super (args, Some $2, annot)
                      | Yield _ ->
                          (* TODO warning *)
                          $1
                      | _ ->
                          failwith "invalid block_call" }
                | block_call DOT operation2 opt_paren_args
                    { new_call $1 (fst $3) $4 ~annot:(annot (snd $3)) }
                | block_call COLON2 operation2 opt_paren_args
                    { new_call $1 (fst $3) $4 ~annot:(annot (snd $3)) }

     method_call: operation
                    paren_args
                    { new_fcall (fst $1) $2 ~annot:(annot (snd $1)) }
                | primary_value DOT operation2 opt_paren_args
                    { new_call $1 (fst $3) $4 ~annot:(annot (snd $3)) }
                | primary_value COLON2 operation2 paren_args
                    { new_call $1 (fst $3) $4 ~annot:(annot (snd $3)) }
                | primary_value COLON2 operation3
                    { new_call $1 (fst $3) [] ~annot:(annot (snd $3)) }
                | K_SUPER paren_args
                    { Super (Some $2, None, annot $1) }
                | K_SUPER
                    { Super (None, None, annot $1) }

     brace_block: LCURLY
                    brace_block_e1
                    opt_block_var
                    compstmt RCURLY_error
                    { let blk = { blk_vars = $3; blk_body = $4 } in
                        Env.unextend state.env;
                        blk }
                | K_DO
                    brace_block_e1
                    opt_block_var
                    compstmt K_END_error
                    { let blk = { blk_vars = $3; blk_body = $4 } in
                        Env.unextend state.env;
                        blk }
  brace_block_e1: { Env.extend ~dyn:true state.env }

       case_body: K_WHEN
                    when_args then_ compstmt cases
                    { ($2, $4) :: $5 }

       when_args: args
                    { $1 }
                | args COMMA STAR arg_value
                    { $1 @ [Arg_splat $4] }
                | STAR arg_value
                    { [Arg_splat $2] }

           cases: case_body
                    { $1 }
                | none
                    { [] }

      opt_rescue: K_RESCUE exc_list exc_var then_ compstmt opt_rescue
                    { let body =
                        match $3 with
                        | Some lhs ->
                            let asgn = Assign (lhs, (Variable (Var_global "!", dummy_annot)), Asgn_single, dummy_annot) in
                              (expr_stmt asgn) :: $5
                        | None ->
                            $5
                      in ($2, body) :: $6 }
                | none
                    { [] }

        exc_list: arg_value
                    { [Arg_value $1] }
                | mrhs
                    { $1 }
                | none
                    { [] }

         exc_var: ASSOC lhs
                    { Some $2 }
                | none
                    { None }

      opt_ensure: K_ENSURE compstmt
                    { $2 }
                | none
                    { [] }

         literal: numeric
                    { let lit_val =
                        match fst $1 with
                        | `Int num ->
                            Lit_integer num
                        | `Float num ->
                            Lit_float num
                        | _ ->
                            failwith "never reach here"
                      in Literal (lit_val, annot (snd $1)) }
                | symbol  { Literal (Lit_symbol [Str_contents (fst $1)], annot (snd $1)) }
                | dsym    { $1 }

         strings: string
                    { $1 }

          string: string1
                    { $1 }
                | string string1
                    { literal_concat $1 $2 }

         string1: STRING_BEG string_contents STRING_END
                    { Literal (Lit_string $2, annot $1) }

         xstring: XSTRING_BEG xstring_contents STRING_END
                    { Literal (Lit_xstring $2, annot $1) }

          regexp: REGEXP_BEG xstring_contents REGEXP_END
                    { new_regexp $2 "TODO" ~annot:(annot $1) }

           words: WORDS_BEG SPACE STRING_END
                    { Array ([], annot $1) }
                | WORDS_BEG word_list STRING_END
                    { Array ($2, annot $1) }

       word_list: none
                    { [] }
                | word_list word SPACE
                    { $1 @ [Arg_value $2] }

            word: string_content
                    { Literal (Lit_string [fst $1], annot (snd $1)) }
                | word string_content
                    { literal_concat $1 (Literal (Lit_string [fst $2], annot (snd $2))) }

          awords: QWORDS_BEG SPACE STRING_END
                    { Array ([], annot $1) }
                | QWORDS_BEG qword_list STRING_END
                    { Array ($2, annot $1) }

      qword_list: none
                    { [] }
                | qword_list STRING_CONTENT SPACE
                    { $1 @ [Arg_value (Literal (Lit_string [Str_contents (fst $2)], annot (snd $2)))] }

 string_contents: none
                    { [] }
                | string_contents string_content
                    { $1 @ [(fst $2)] }

xstring_contents: none
                    { [] }
                | xstring_contents string_content
                    { $1 @ [(fst $2)] }

  string_content: STRING_CONTENT
                    { Str_contents (fst $1), snd $1 }
                | STRING_DVAR
                    string_content_e1
                    string_dvar
                    { state.lex_strterm <- $2;
                      Str_interpol $3, $1 }
                | STRING_DBEG
                    string_content_e2
                    compstmt RCURLY
                    { state.lex_strterm <- $2;
                      Stack_state.lexpop state.cond_stack;
                      Stack_state.lexpop state.cmdarg_stack;
                      Str_interpol (Seq ($3, dummy_annot)), $1 }

string_content_e1: { let ret = state.lex_strterm in
                       state.lex_strterm <- None;
                       state.lex_state <- Expr_beg;
                       ret }
string_content_e2: { let ret = state.lex_strterm in
                       state.lex_strterm <- None;
                       state.lex_state <- Expr_beg;
                       Stack_state.push state.cond_stack false;
                       Stack_state.push state.cmdarg_stack false;
                       ret }

     string_dvar: GVAR { Variable (Var_global (fst $1), annot (snd $1)) }
                | IVAR { Variable (Var_instance (fst $1), annot (snd $1)) }
                | CVAR { Variable (Var_class (fst $1), annot (snd $1)) }

          symbol: SYMBEG sym
                    { state.lex_state <- Expr_end;
                      $2 }

             sym: fname { $1 }
                | IVAR  { $1 }
                | GVAR  { $1 }
                | CVAR  { $1 }

            dsym: SYMBEG xstring_contents STRING_END
                    { Literal (Lit_symbol $2, annot $1) }

         numeric: INTEGER
                    { `Int (fst $1), snd $1 }
                | FLOAT
                    { `Float (fst $1), snd $1 }
                | UMINUS_NUM INTEGER %prec LOWEST
                    { `Int ~-(fst $2), $1 }
                | UMINUS_NUM FLOAT   %prec LOWEST
                    { `Float ~-.(fst $2), $1 }

        variable: IDENTIFIER { $1 }
                | IVAR       { $1 }
                | GVAR       { $1 }
                | CONSTANT   { $1 }
                | CVAR       { $1 }
                | K_NIL      { "nil", $1 }
                | K_SELF     { "self", $1 }
                | K_TRUE     { "true", $1 }
                | K_FALSE    { "false", $1 }
                | K__FILE__  { "__FILE__", $1 }
                | K__LINE__  { "__LINE__", $1 }

         var_ref: variable
                    { gettable (fst $1) ~annot:(annot (snd $1)) }

         var_lhs: variable
                    { assignable (fst $1) ~annot:(annot (snd $1)) }

      superclass: term
                    { None }
                | LT
                    superclass_e1
                    expr_value term
                    { Some $3 }
                | error term
                    { None }
   superclass_e1:   { state.lex_state <- Expr_beg }

       f_arglist: LPAREN2 f_args opt_nl RPAREN
                    { state.lex_state <- Expr_beg;
                      $2 }
                | f_args term
                    { $1 }

          f_args: f_arg COMMA f_optarg COMMA f_rest_arg opt_f_block_arg
                    { formal_params $1 $3 $5 $6 }
                | f_arg COMMA f_optarg opt_f_block_arg
                    { formal_params $1 $3 [] $4 }
                | f_arg COMMA f_rest_arg opt_f_block_arg
                    { formal_params $1 [] $3 $4 }
                | f_arg opt_f_block_arg
                    { formal_params $1 [] [] $2 }
                | f_optarg COMMA f_rest_arg opt_f_block_arg
                    { formal_params [] $1 $3 $4 }
                | f_optarg opt_f_block_arg
                    { formal_params [] $1 [] $2 }
                | f_rest_arg opt_f_block_arg
                    { formal_params [] [] $1 $2 }
                | f_block_arg
                    { formal_params [] [] [] $1 }
                |
                    { formal_params [] [] [] [] }

      f_norm_arg: CONSTANT
                    { yyerror ("formal argument cannot be a constant: " ^ fst $1) }
                | IVAR
                    { yyerror "formal argument cannot be an instance variable" }
                | CVAR
                    { yyerror "formal argument cannot be a class variable" }
                | IDENTIFIER
                    { Env.add state.env (fst $1) `Lvar;
                      Param_req (fst $1) }

           f_arg: f_norm_arg
                    { [$1] }
                | f_arg COMMA f_norm_arg
                    { $1 @ [$3] }

           f_opt: IDENTIFIER EQL arg_value
                    { ignore (assignable (fst $1));
                      Param_opt (fst $1, $3) }

        f_optarg: f_opt
                    { [$1] }
                | f_optarg COMMA f_opt
                    { $1 @ [$3] }

    restarg_mark: STAR2 { $1 }
                | STAR  { $1 }

      f_rest_arg: restarg_mark IDENTIFIER
                    { ignore (assignable (fst $2));
                      [Param_rest (fst $2)] }
                | restarg_mark
                    { Env.add state.env "*" `Lvar;
                      [Param_star] }

     blkarg_mark: AMPER2 { $1 }
                | AMPER  { $1 }

     f_block_arg: blkarg_mark IDENTIFIER
                    { let id = fst $2 in
                        Env.add state.env id `Lvar;
                        [Param_block id] }

 opt_f_block_arg: COMMA f_block_arg
                    { $2 }
                |
                    { [] }

       singleton: var_ref
                    { $1 }
                | LPAREN2
                    singleton_e1
                    expr opt_nl RPAREN
                    {
(* TODO
                      yyerror "Can't define single method for literals." if
                        result[0] == :lit
*)
                        $3 }
    singleton_e1: { state.lex_state <- Expr_beg }

      assoc_list: none
                    { [] }
                | assocs trailer
                    { $1 }
                | args trailer
                    { let rec work = function
                        | [] -> []
                        | Arg_value k :: Arg_value v :: xs ->
                          (k, v) :: work xs
                        | _ -> failwith "invalid assoc"
                      in work $1 }

          assocs: assoc
                    { $1 }
                | assocs COMMA assoc
                    { $1 @ $3 }

           assoc: arg_value ASSOC arg_value
                    { [$1, $3] }

       operation: IDENTIFIER { $1 }
                | CONSTANT   { $1 }
                | FID        { $1 }
      operation2: IDENTIFIER { $1 }
                | CONSTANT   { $1 }
                | FID        { $1 }
                | op         { $1 }
      operation3: IDENTIFIER { $1 }
                | FID        { $1 }
                | op         { $1 }
    dot_or_colon: DOT    { $1 }
                | COLON2 { $1 }
       opt_terms: { [] }
                | terms   { $1 }
          opt_nl: { () }
                | COMMENT { () }
                | NL      { () }
         trailer: { () }
                | COMMENT { () }
                | NL      { () }
                | COMMA   { () }

            term: SEMI    { [] }
                | COMMENT { [Comment (fst $1, annot (snd $1))] }
                | NL      { [] }

           terms: term       { $1 }
                | term terms { $1 @ $2 }

            none: { () }

 none_block_pass: { () }

    RCURLY_error: RCURLY { () } | error { () }
     K_END_error: K_END { () } | error { () }
%%end
