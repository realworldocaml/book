%token P_ATAT // @@
%token MACROMODULE // macromodule
%token FILE_PATH // file_path
%token D_CLOG2 // $clog2
%token D_READMEMB // $readmemb
%token INSTANCE // instance
%token CONSTRAINT // constraint
%token<string> BINARY_BASE_3 // binary_base_3
%token TOKEN_1364_2001 // 1364-2001
%token PACKED // packed
%token D_READMEMH // $readmemh
%token IFF // iff
%token TOKEN_NEWLINE //

%token PRIMITIVE // primitive
%token U_WIRE // uwire
%token D_FFLUSH // $fflush
%token P_ANDEQ // &=
%token EOF // eof
%token BEGIN // begin
%token ENDCLOCKING // endclocking
%token NEW // new
%token D_FOPEN // $fopen
%token TOKEN_B0 // 'B0
%token TOKEN_QUOTE_FALSE // '0
%token D_FWRITE // $fwrite
%token TOKEN_B1 // 'B1
%token TOKEN_QUOTE_TRUE // '1
%token ENDSPECIFY // endspecify
%token TOKEN_X0 // 1'BX
%token PRIORITY // priority
%token SOLVE // solve
%token REALTIME // realtime
%token PROPERTY // property
%token P_OROR // ((
%token BUFIF0 // bufif0
%token PLING // !
%token BUFIF1 // bufif1
%token D_PERIOD // $period
%token TOKEN_LPAREN_STAR // (*
%token DOUBLE_QUOTES // "
%token HASH // #
%token PULLDOWN // pulldown
%token ALWAYS // always
%token DOLLAR // $
%token P_ORMINUSGT // (,
%token SHOWCANCELLED // showcancelled
%token ENDSEQUENCE // endsequence
%token PULLUP // pullup
%token MODULO // %
%token EDGE // edge
%token AMPERSAND // &
%token SQUOTE // '
%token LIBRARY // library
%token LPAREN // (
%token TRANIF0 // tranif0
%token RPAREN // )
%token TRANIF1 // tranif1
%token PARAMETER // parameter
%token TAB // tab
%token EMPTY // empty
%token TIMES // *
%token PLUS // +
%token COMMA // ,
%token MINUS // -
%token P_OREQGT // (=>
%token DOT // .
%token DIVIDE // /
%token TOKEN_ZERO // 0
%token TOKEN_ONE // 1
%token RNMOS // rnmos
%token SUPPLY0 // supply0
%token IF // if
%token TOKEN_TWO // 2
%token SUPPLY1 // supply1
%token D_MONITOR // $monitor
%token TOKEN_THREE // 3
%token D_FSCANF // $fscanf
%token TOKEN_FOUR // 4
%token PROGRAM // program
%token TOKEN_FIVE // 5
%token P_OREQ // (=
%token PMOS // pmos
%token TOKEN_SIX // 6
%token PROTECTED // protected
%token DESIGN // design
%token VECTORED // vectored
%token TOKEN_SEVEN // 7
%token P_XOREQ // ^=
%token TOKEN_X1 // 1'Bx
%token TOKEN_EIGHT // 8
%token CASE // case
%token TOKEN_NINE // 9
%token COLON // :
%token TABLE // table
%token D_BITS // $bits
%token SEMICOLON // ;
%token<string> NAME_OF_UDP_INSTANCE // name_of_udp_instance
%token LESS // <
%token EQUALS // =
%token TOKEN_QUOTE_X1 // 'X
%token TIMEUNIT // timeunit
%token GREATER // >
%token QUERY // ?
%token TOKEN_QUOTE_Z1 // 'Z
%token ENDFUNCTION // endfunction
%token<string> UNSIGNED_NUMBER // unsigned_number
%token AT // @
%token TOKEN_ESCAPED_NEWLINE // \n
%token<string> SIMPLE_IDENTIFIER // simple_identifier
%token FOREVER // forever
%token P_SRIGHTEQ // >>=
%token D_TEST_PLUSARGS // $test$plusargs
%token<string> C_IDENTIFIER // c_identifier
%token P_SSRIGHT3 // >>>
%token INPUT // input
%token TOKEN_STAR_RPAREN // *)
%token BUF // buf
%token P_POW // **
%token VOID // void
%token<string> SYSTEM_FUNCTION_IDENTIFIER // system_function_identifier
%token WIRE // wire
%token FORK // fork
%token SUPER // super
%token SMALL // small
%token RELEASE // release
%token FORCE // force
%token D_FGETC // $fgetc
%token P_SLEFT3 // <<<
%token END_COMMENT // */
%token DASH_GT_GT // ->>
%token P_SLEFTEQ // <<=
%token P_CASENOTEQUAL // !==
%token<string> TIME_UNIT // time_unit
%token P_WILDNOTEQUAL // !=?
%token INCDIR // -incdir
%token TOKEN_FALSE2 // 1'b0
%token D_FATAL // $fatal
%token TOKEN_TRUE2 // 1'b1
%token ENUM // enum
%token ELSE // else
%token TYPE // type
%token EVENT // event
%token ALWAYS_LATCH // always_latch
%token IS_DEFINED_AS // ::=
%token BIND // bind
%token D_FEOF // $feof
%token AND // and
%token BYTE // byte
%token NOCHANGE // $nochange
%token<string> HEX_BASE // hex_base
%token DIST // dist
%token LBRACK // [
%token P_TIMESEQ // *=
%token D_FGETS // $fgets
%token ASSIGN // assign
%token TOKEN_PLUS_PLUS // ++
%token BACKSLASH // \
%token P_ASTGT // *>
%token RBRACK // ]
%token TOKEN_QUOTE_X2 // 'x
%token CARET // ^
%token CELL // cell
%token TOKEN_QUOTE_Z2 // 'z
%token UNDERSCORE // _
%token TRAN // tran
%token D_COUNTDRIVERS // $countdrivers
%token TOKEN_RBRACK_LCURLY // ]{
%token ENDINTERFACE // endinterface
%token D_FCLOSE // $fclose
%token ENDTASK // endtask
%token WAIT // wait
%token RETURN // return
%token NEGEDGE // negedge
%token GENERATE // generate
%token RANDC // randc
%token P_PLUSCOLON // +:
%token VIRTUAL // virtual
%token LOCAL // local
%token CONTEXT // context
%token EXTENDS // extends
%token P_PLUSEQ // +=
%token D_SKEW // $skew
%token P_SSRIGHT3EQ // >>>=
%token JOIN_ANY // join_any
%token REPEAT // repeat
%token TOKEN_VBAR_EQUALS // |=
%token VBAR_DASH_GT // |->
%token BIT // bit
%token WITH // with
%token REAL // real
%token TOKEN_PLING_QUERY_EQUALS // !?=
%token TRI // tri
%token PULSESTYLE_ONEVENT // pulsestyle_onevent
%token TOKEN_FALSE // 'b0
%token TOKEN_TRUE // 'b1
%token D_DISPLAY // $display
%token TOKEN_X2 // 1'bX
%token P_XNOR // ^~
%token RCMOS // rcmos
%token ENDPROPERTY // endproperty
%token COUNTONES // countones
%token STATIC // static
%token LCURLY // {
%token CONTINUE // continue
%token ANY_ASCII_CHARS // Any_ASCII_Characters
%token VBAR // |
%token RCURLY // }
%token P_COLONDIV // :/
%token SPECIFY // specify
%token<string> OCTAL_BASE_3 // octal_base_3
%token TILDE // ~
%token WOR // wor
%token P_NAND // ~&
%token ISUNKNOWN // isunknown
%token P_NOR // ~(
%token AUTOMATIC // automatic
%token TOKEN_DASH_DASH // --
%token FOR // for
%token SPECPARAM // specparam
%token P_COLONCOLON // ::
%token TRIAND // triand
%token TIME // time
%token JOIN_NONE // join_none
%token<string> Z_OR_X // z_or_x
%token CONFIG // config
%token TOKEN_DPI // "DPI"
%token P_COLONEQ // :=
%token<string> DECIMAL_BASE_3 // decimal_base_3
%token D_ONEHOT0 // $onehot0
%token P_ANDANDAND // &&&
%token ASSERT // assert
%token NOSHOWCANCELLED // noshowcancelled
%token<string> SYSTEM_TASK_IDENTIFIER // system_task_identifier
%token INITIAL // initial
%token P_MINUSCOLON // -:
%token NAND // nand
%token IF_NONE // ifnone
%token TOKEN_9_DOLLAR // 9_$
%token<string> DECIMAL_BASE // decimal_base
%token P_MINUSEQ // -=
%token P_DOTSTAR // .*
%token P_MINUSGT // ->
%token TOKEN_X3 // 1'bx
%token MODULE // module
%token NEWLINE // newline
%token D_UNSIGNED // $unsigned
%token TOKEN_9_ // 9_
%token D_SSCANF // $sscanf
%token<string> EDGE_SYMBOL // edge_symbol
%token CHANDLE // chandle
%token TOKEN_ANY_ASCII // {any_ASCII_character_except_white_space}
%token UNIQUE // unique
%token PULSESTYLE_ONDETECT // pulsestyle_ondetect
%token LOGIC // logic
%token D_WIDTH // $width
%token BEFORE // before
%token NMOS // nmos
%token CASEX // casex
%token TOKEN_PLING_EQUALS_GT // |=>
%token INTERFACE // interface
%token CASEZ // casez
%token INTERSECT // intersect
%token DEASSIGN // deassign
%token D_COUNTONES // $countones
%token BOLD // bold
%token BEGIN_COMMENT // /*
%token REF // ref
%token NULL // null
%token PURE // pure
%token EXPORT // export
%token REG // reg
%token SCALARED // scalared
%token TOKEN_SLASH_SLASH // //
%token D_ISUNKNOWN // $isunknown
%token ENDCONFIG // endconfig
%token ENDCLASS // endclass
%token TOKEN_LBRACK_STAR_EQUALS // [*=
%token TOKEN_LPAREN_STAR_RPAREN // (*)
%token SVINT // int
%token OR // or
%token<string> BINARY_BASE // binary_base
%token TOKEN_VBAR_VBAR // ||
%token P_SLEFT // <<
%token P_NOTEQUAL // !=
%token ALWAYS_FF // always_ff
%token P_LTE // <=
%token SHORTINT // shortint
%token CONST // const
%token WAND // wand
%token STEP // step
%token EXTERN // extern
%token MEDIUM // medium
%token<string> OCTAL_BASE // octal_base
%token PULL0 // pull0
%token P_NXOR // ~^
%token PULL1 // pull1
%token FORKJOIN // forkjoin
%token DEFPARAM // defparam
%token CLOCKING // clocking
%token TRI0 // tri0
%token TASK // task
%token P_DIVEQ // /=
%token TRI1 // tri1
%token UNION // union
%token LIBLIST // liblist
%token ENDMODULE // endmodule
%token P_POUNDPOUND // ##
%token RAND // rand
%token SHORTREAL // shortreal
%token D_RECREM // $recrem
%token LOCALPARAM // localparam
%token D_REALTIME // $realtime
%token SIGNED // signed
%token D_ATTRIBUTE // $attribute
%token TOKEN_EDGE01 // 01
%token STRONG0 // strong0
%token<string> HEX_BASE_3 // hex_base_3
%token STRONG1 // strong1
%token LONGINT // longint
%token P_EQUAL // ==
%token HIGHZ0 // highz0
%token D_SIGNED // $signed
%token ENDGENERATE // endgenerate
%token P_EQGT // =>
%token HIGHZ1 // highz1
%token POSEDGE // posedge
%token<string> EXP // exp
%token D_INFO // $info
%token FINAL // final
%token RPMOS // rpmos
%token D_TIMESKEW // $timeskew
%token P_CASEEQUAL // ===
%token SEQUENCE // sequence
%token RANDOMIZE // randomize
%token P_WILDEQUAL // ==?
%token D_WRITE // $write
%token D_HOLD // $hold
%token ALIAS // alias
%token ENDPRIMITIVE // endprimitive
%token TIMEPRECISION // timeprecision
%token ENDPROGRAM // endprogram
%token NOR // nor
%token JOIN // join
%token D_ONEHOT // $onehot
%token NOT // not
%token D_SETUPHOLD // $setuphold
%token D_FDISPLAY // $fdisplay
%token TOKEN_EDGE_10 // 10
%token TOKEN_EDGE_11 // 11
%token WAIT_ORDER // wait_order
%token STRING // string
%token TILDE_VBAR // ~|
%token D_STOP // $stop
%token P_GTE // >=
%token P_SRIGHT // >>
%token TRIREG // trireg
%token THROUGHOUT // throughout
%token WHILE // while
%token D_SETUP // $setup
%token DEFAULT // default
%token TRIOR // trior
%token DO // do
%token PATHPULSE // PATHPULSE$
%token INSIDE // inside
%token D_WARNING // $warning
%token D_RECOVERY // $recovery
%token INOUT // inout
%token P_SLEFT3EQ // <<<=
%token OUTPUT // output
%token FIRST_MATCH // first_match
%token THIS // this
%token LARGE // large
%token ENDCASE // endcase
%token D_REMOVAL // $removal
%token FUNCTION // function
%token CMOS // cmos
%token INTEGER // integer
%token XOR // xor
%token WEAK0 // weak0
%token DISABLE // disable
%token WEAK1 // weak1
%token<string> HEX_DIGIT // hex_digit
%token AT_STAR // @*
%token LBRACK_STAR // [*
%token USE // use
%token D_FINISH // $finish
%token ENDTABLE // endtable
%token<string> Z_DIGIT // z_digit
%token D_TIME // $time
%token FULLSKEW // $fullskew
%token RTRANIF0 // rtranif0
%token<string> OUTPUT_SYMBOL // output_symbol
%token EQUALS_QUERY_EQUALS // =?=
%token TYPEDEF // typedef
%token RTRANIF1 // rtranif1
%token GENVAR // genvar
%token COVER // cover
%token<string> LEVEL_SYMBOL // level_symbol
%token TOKEN_FALSE1 // 1'B0
%token INCLUDE // include
%token TOKEN_TRUE1 // 1'B1
%token RTRAN // rtran
%token BREAK // break
%token XNOR // xnor
%token UNSIGNED // unsigned
%token MODPORT // modport
%token D_FWRITEH // $fwriteh
%token LBRACK_STAR_DASH_GT // [*->
%token P_ANDAND // &&
%token NOTIF0 // notif0
%token D_ERROR // $error
%token ANY_ASCII_CHAR // Any_ASCII_character
%token IMPORT // import
%token NOTIF1 // notif1
%token ALWAYS_COMB // always_comb
%token SPACE // space
%token<string> X_DIGIT // x_digit
%token END // end
%token WITHIN // within
%token P_MODEQ // %=
%token CLASS // class
%token STRUCT // struct
%token <token> MS
%token <token> US
%token <token> NS
%token <token> PS
%token <token> FS
%token <token> AS
%token <string> ASCNUM
%token <char> CHAR
%token <string> ID
%token <token * token > RANGE
%token <int> INT
%token <token * token> DOUBLE
%token <token * token * token> TRIPLE
%token <token * token * token * token> QUADRUPLE
%token <token * token * token * token * token> QUINTUPLE
%token <token * token * token * token * token * token> SEXTUPLE
%token <token * token * token * token * token * token * token> SEPTUPLE
%token <token * token * token * token * token * token * token * token> OCTUPLE
%token <token * token * token * token * token * token * token * token * token> NONUPLE
%token <token * token * token * token * token * token * token * token * token * token> DECUPLE
%token <token * token * token * token * token * token * token * token * token * token * token> UNDECUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token> DUODECUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token> TREDECUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token * token> QUATTUORDECUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token * token * token> QUINDECUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token> SEXDECUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token> SEPTENDECUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token> OCTODECUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token> NOVEMDECUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token> VIGENUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token> UNVIGENUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token> DUOVIGENUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token> TREVIGENUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token> QUATTUORVIGENUPLE
%token <token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token * token> QUINVIGENUPLE
%token  ALTERNATIVE
%token  ASSIGNMENT
%token  BIDIR
%token <string> BINNUM
%token  BITSEL
%token  CASECOND
%token  CELLPIN
%token  CONCAT
%token  D_C
%token <string> DECNUM
%token  DLYASSIGNMENT
%token <token list> DOTTED
%token  D_RANDOM
%token  DRIVER
%token  D_STIME
%token  ENDLABEL
%token  ENDOFFILE
%token <float> FLOATNUM
%token  FULLSKEW_TIMING_CHECK
%token  FUNCREF
%token  FUNCUSED
%token  GENCASE
%token  GENCASECOND
%token <string> HEXNUM
%token  HOLD_TIMING_CHECK
%token  IMPLICIT
%token <string> INTNUM
%token  IOPORT
%token  LEFT_BRACKET
%token  LEFT_CURLY
%token  MEMORY
%token  MINTYPMAX
%token  MODINST
%token  NAMED
%token  NOCHANGE_TIMING_CHECK
%token <string> OCTNUM
%token  PARAMUSED
%token  PARTSEL
%token  P_CELLDEFINE
%token  P_DEFINE
%token  P_DELAY_MODE_PATH
%token  P_DISABLE_PORTFAULTS
%token  P_ELSE
%token  P_ENABLE_PORTFAULTS
%token  P_ENDCELLDEFINE
%token  P_ENDIF
%token  P_ENDPROTECT
%token  PERIOD_TIMING_CHECK
%token  P_IFDEF
%token <string> P_INCLUDE
%token  P_NOSUPPRESS_FAULTS
%token  P_PROTECT
%token <string> PRAGMATIC
%token <string> PREPROC
%token  P_RESETALL
%token  PRIMINST
%token  P_SUPPRESS_FAULTS
%token <string> P_TIMESCALE
%token  RECEIVER
%token  RECOVERY_TIMING_CHECK
%token  RECREM_TIMING_CHECK
%token  REMOVAL_TIMING_CHECK
%token  RIGHT_BRACKET
%token  RIGHT_CURLY
%token  SCALAR
%token  SENSUSED
%token  SETUPHOLD_TIMING_CHECK
%token  SETUP_TIMING_CHECK
%token  SKEW_TIMING_CHECK
%token  SPECIAL
%token  SUBCCT
%token  SUBMODULE
%token  SUBTRACTION
%token  TASKREF
%token  TASKUSED
%token  TIMESKEW_TIMING_CHECK
%token  TIMINGSPEC
%token <token list> TLIST
%token  UNKNOWN
%token  WIDTH_TIMING_CHECK

%start start
%type <Grammar_sysver.token> start

%{ open Grammar_sysver
%}


%%

start: arg1 = source_text { arg1 };

binary_base_3: BINARY_BASE_3 { BINARY_BASE_3 $1 };
name_of_udp_instance: NAME_OF_UDP_INSTANCE { NAME_OF_UDP_INSTANCE $1 };
unsigned_number: UNSIGNED_NUMBER { UNSIGNED_NUMBER $1 };
simple_identifier: SIMPLE_IDENTIFIER { SIMPLE_IDENTIFIER $1 };
c_identifier: C_IDENTIFIER { C_IDENTIFIER $1 };
system_function_identifier: SYSTEM_FUNCTION_IDENTIFIER { SYSTEM_FUNCTION_IDENTIFIER $1 };
time_unit: TIME_UNIT { TIME_UNIT $1 };
hex_base: HEX_BASE { HEX_BASE $1 };
octal_base_3: OCTAL_BASE_3 { OCTAL_BASE_3 $1 };
z_or_x: Z_OR_X { Z_OR_X $1 };
decimal_base_3: DECIMAL_BASE_3 { DECIMAL_BASE_3 $1 };
system_task_identifier: SYSTEM_TASK_IDENTIFIER { SYSTEM_TASK_IDENTIFIER $1 };
decimal_base: DECIMAL_BASE { DECIMAL_BASE $1 };
edge_symbol: EDGE_SYMBOL { EDGE_SYMBOL $1 };
binary_base: BINARY_BASE { BINARY_BASE $1 };
octal_base: OCTAL_BASE { OCTAL_BASE $1 };
hex_base_3: HEX_BASE_3 { HEX_BASE_3 $1 };
exp: EXP { EXP $1 };
hex_digit: HEX_DIGIT { HEX_DIGIT $1 };
z_digit: Z_DIGIT { Z_DIGIT $1 };
output_symbol: OUTPUT_SYMBOL { OUTPUT_SYMBOL $1 };
level_symbol: LEVEL_SYMBOL { LEVEL_SYMBOL $1 };
x_digit: X_DIGIT { X_DIGIT $1 };
/* */
/* */
/* */
/*




 Annex AFormal Syntax






Annex A


Formal Syntax




(Normative)



The formal syntax of SystemVerilog is described using Backus-Naur Form (BNF). The conventions used are:




Keywords and punctuation are in
bold
 text.



Syntactic categories are named in non-bold text.



A vertical bar (
 |
) separates alternatives.



Square brackets (
 [ ]
) enclose optional items.



Braces (
 { }
) enclose items which can be repeated zero or more times.




The full syntax and semantics of Verilog and SystemVerilog are not described solely using BNF. The normative text description contained within the chapters of the IEEE 1364-2001 Verilog standard and this SystemVerilog document provide additional details on the syntax and semantics described in this BNF.




A.1 Source text





A.1.1 Library source text
 */



     /* library_text ::= { library_descriptions   } */






	 /* library_descriptions ::= library_declaration | include_statement | config_declaration */















 /* library_declaration ::= 'library' library_identifier file_path_spec { ','   file_path_spec   } [ '-incdir'   file_path_spec   { ','   file_path_spec   }   ] ';' */



   /* file_path_spec ::= 'file_path' */







 /* include_statement ::= 'include' '<' file_path_spec '>' ';' */
/*





A.1.2 Configuration source text
 */













 /* config_declaration ::= 'config' config_identifier ';' design_statement { config_rule_statement   } 'endconfig' */









 /* design_statement ::= 'design' { [ library_identifier   '.'   ]   cell_identifier   } ';' */
















 /* config_rule_statement ::= default_clause liblist_clause | inst_clause liblist_clause | inst_clause use_clause | cell_clause liblist_clause | cell_clause use_clause */





 /* default_clause ::= 'default' */





  /* inst_clause ::= 'instance' inst_name */





  /* inst_name ::= topmodule_identifier { '.'   instance_identifier   } */







  /* cell_clause ::= 'cell' [ library_identifier   '.'   ] cell_identifier */





  /* liblist_clause ::= 'liblist' { library_identifier   } */









 /* use_clause ::= 'use' [ library_identifier   '.'   ] cell_identifier [ ':'   'config'   ] */
/*





A.1.3

Module and primitive source text
 */



        /* source_text ::= [ timeunits_declaration   ] { description   } */







	 /* description ::= module_declaration | udp_declaration | module_root_item | statement_or_null */







 /* module_nonansi_header ::= { attribute_instance   } module_keyword [ lifetime   ] module_identifier [ parameter_port_list   ] list_of_ports ';' */







 /* module_ansi_header ::= { attribute_instance   } module_keyword [ lifetime   ] module_identifier [ parameter_port_list   ] [ list_of_port_declarations   ] ';' */





























  /* module_declaration ::= module_nonansi_header [ timeunits_declaration   ] { module_item   } 'endmodule' [ ':'   module_identifier   ] | module_ansi_header [ timeunits_declaration   ] { non_port_module_item   } 'endmodule' [ ':'   module_identifier   ] | { attribute_instance   } module_keyword [ lifetime   ] module_identifier '(' '.*' ')' ';' [ timeunits_declaration   ] { module_item   } 'endmodule' [ ':'   module_identifier   ] | 'extern' module_nonansi_header | 'extern' module_ansi_header */







 /* module_keyword ::= 'module' | 'macromodule' */









 /* interface_nonansi_header ::= { attribute_instance   } 'interface' [ lifetime   ] interface_identifier [ parameter_port_list   ] list_of_ports ';' */









 /* interface_ansi_header ::= { attribute_instance   } 'interface' [ lifetime   ] interface_identifier [ parameter_port_list   ] [ list_of_port_declarations   ] ';' */
































  /* interface_declaration ::= interface_nonansi_header [ timeunits_declaration   ] { interface_item   } 'endinterface' [ ':'   interface_identifier   ] | interface_ansi_header [ timeunits_declaration   ] { non_port_interface_item   } 'endinterface' [ ':'   interface_identifier   ] | { attribute_instance   } 'interface' interface_identifier '(' '.*' ')' ';' [ timeunits_declaration   ] { interface_item   } 'endinterface' [ ':'   interface_identifier   ] | 'extern' interface_nonansi_header | 'extern' interface_ansi_header */









 /* program_nonansi_header ::= { attribute_instance   } 'program' [ lifetime   ] program_identifier [ parameter_port_list   ] list_of_ports ';' */









 /* program_ansi_header ::= { attribute_instance   } 'program' [ lifetime   ] program_identifier [ parameter_port_list   ] [ list_of_port_declarations   ] ';' */
































  /* program_declaration ::= program_nonansi_header [ timeunits_declaration   ] { program_item   } 'endprogram' [ ':'   program_identifier   ] | program_ansi_header [ timeunits_declaration   ] { non_port_program_item   } 'endprogram' [ ':'   program_identifier   ] | { attribute_instance   } 'program' program_identifier '(' '.*' ')' ';' [ timeunits_declaration   ] { program_item   } 'endprogram' [ ':'   program_identifier   ] | 'extern' program_nonansi_header | 'extern' program_ansi_header */


















  /* class_declaration ::= { attribute_instance   } [ 'virtual'   ] 'class' [ lifetime   ] class_identifier [ parameter_port_list   ] [ 'extends'   class_identifier   ] ';' [ timeunits_declaration   ] { class_item   } 'endclass' [ ':'   class_identifier   ] */





















   /* timeunits_declaration ::= 'timeunit' time_literal ';' | 'timeprecision' time_literal ';' | 'timeunit' time_literal ';' 'timeprecision' time_literal ';' | 'timeprecision' time_literal ';' 'timeunit' time_literal ';' */
/*





A.1.4

Module parameters and ports
 */









 /* parameter_port_list ::= '#' '(' parameter_declaration { ','   parameter_declaration   } ')' */









 /* list_of_ports ::= '(' port { ','   port   } ')' */













 /* list_of_port_declarations ::= '(' port_declaration { ','   port_declaration   } ')' | '(' ')' */








	    /* non_generic_port_declaration ::= { attribute_instance   } inout_declaration | { attribute_instance   } input_declaration | { attribute_instance   } output_declaration | { attribute_instance   } ref_declaration | { attribute_instance   } interface_port_declaration */











 /* port ::= [ port_expression   ] | '.' port_identifier '(' [ port_expression   ] ')' */











 /* port_expression ::= port_reference | { port_reference { ','   port_reference   } } */








  /* port_reference ::= port_identifier [ [   constant_range_expression   ]   ] */





	    /* port_declaration ::= non_generic_port_declaration | { attribute_instance   } generic_interface_port_declaration */
/*





A.1.5

Module items
 */








	    /* module_common_item ::= { attribute_instance   } module_or_generate_item_declaration | { attribute_instance   } interface_instantiation | { attribute_instance   } program_instantiation | { attribute_instance   } concurrent_assertion_item | { attribute_instance   } bind_directive */







	 /* module_item ::= non_generic_port_declaration ';' | non_port_module_item */



















 /* module_or_generate_item ::= { attribute_instance   } parameter_override | { attribute_instance   } continuous_assign | { attribute_instance   } gate_instantiation | { attribute_instance   } udp_instantiation | { attribute_instance   } module_instantiation | { attribute_instance   } initial_construct | { attribute_instance   } always_construct | { attribute_instance   } combinational_construct | { attribute_instance   } latch_construct | { attribute_instance   } ff_construct | { attribute_instance   } net_alias | { attribute_instance   } final_construct | module_common_item | { attribute_instance   } ';' */









	 /* module_root_item ::= { attribute_instance   } module_instantiation | { attribute_instance   } local_parameter_declaration | interface_declaration | program_declaration | class_declaration | module_common_item */



















 /* module_or_generate_item_declaration ::= net_declaration | data_declaration | genvar_declaration | task_declaration | function_declaration | dpi_import_export | extern_constraint_declaration | extern_method_declaration | clocking_decl | 'default' 'clocking' clocking_identifier ';' */













	 /* non_port_module_item ::= { attribute_instance   } generated_module_instantiation | { attribute_instance   } local_parameter_declaration | module_or_generate_item | { attribute_instance   } parameter_declaration ';' | { attribute_instance   } specify_block | { attribute_instance   } specparam_declaration | class_declaration | module_declaration */







 /* parameter_override ::= 'defparam' list_of_defparam_assignments ';' */













 /* bind_directive ::= 'bind' module_identifier bind_instantiation ';' | 'bind' name_of_instance bind_instantiation ';' */






	 /* bind_instantiation ::= program_instantiation | module_instantiation | interface_instantiation */
/*





A.1.6

Interface items
 */




















 /* interface_or_generate_item ::= { attribute_instance   } continuous_assign | { attribute_instance   } initial_construct | { attribute_instance   } always_construct | { attribute_instance   } combinational_construct | { attribute_instance   } latch_construct | { attribute_instance   } ff_construct | { attribute_instance   } local_parameter_declaration | { attribute_instance   } parameter_declaration ';' | module_common_item | { attribute_instance   } modport_declaration | { attribute_instance   } extern_tf_declaration | { attribute_instance   } final_construct | { attribute_instance   } ';' */















 /* extern_tf_declaration ::= 'extern' method_prototype | 'extern' 'forkjoin' 'task' named_task_proto ';' */







	 /* interface_item ::= non_generic_port_declaration ';' | non_port_interface_item */









	 /* non_port_interface_item ::= { attribute_instance   } generated_interface_instantiation | { attribute_instance   } specparam_declaration | interface_or_generate_item | program_declaration | class_declaration | interface_declaration */
/*





A.1.7

Program items
 */







	 /* program_item ::= port_declaration ';' | non_port_program_item */













	 /* non_port_program_item ::= { attribute_instance   } continuous_assign | { attribute_instance   } module_or_generate_item_declaration | { attribute_instance   } specparam_declaration | { attribute_instance   } local_parameter_declaration | { attribute_instance   } parameter_declaration ';' | { attribute_instance   } initial_construct | { attribute_instance   } concurrent_assertion_item | class_declaration */
/*





A.1.8

Class items
 */



/* */






	    /* class_item ::= { attribute_instance   } class_property | { attribute_instance   } class_method | { attribute_instance   } class_constraint */











 /* class_property ::= { property_qualifier   } data_declaration | 'const' { class_item_qualifier   } data_type const_identifier [ '='   constant_expression   ] ';' */








     /* class_method ::= { method_qualifier   } task_declaration | { method_qualifier   } function_declaration | 'extern' { method_qualifier   } method_prototype */





	 /* class_constraint ::= constraint_prototype | constraint_declaration */













 /* class_item_qualifier ::= 'static' | 'protected' | 'local' */











	 /* property_qualifier ::= 'rand' | 'randc' | class_item_qualifier */








	 /* method_qualifier ::= 'virtual' | class_item_qualifier */













 /* method_prototype ::= 'task' named_task_proto ';' | 'function' named_function_proto ';' */













  /* extern_method_declaration ::= 'function' [ lifetime   ] class_identifier '::' function_body_declaration | 'task' [ lifetime   ] class_identifier '::' task_body_declaration */
/*





A.1.9

Constraints
 */











 /* constraint_declaration ::= [ 'static'   ] 'constraint' constraint_identifier { { constraint_block   } } */




















	 /* constraint_block ::= 'solve' identifier_list 'before' identifier_list ';' | expression 'dist' { dist_list } ';' | constraint_expression */


















   /* constraint_expression ::= expression ';' | expression '=>' constraint_set | 'if' '(' expression ')' constraint_set [ 'else'   constraint_set   ] */









 /* constraint_set ::= constraint_expression | { { constraint_expression   } } */





   /* dist_list ::= dist_item { ','   dist_item   } */









  /* dist_item ::= value_range ':=' expression | value_range ':/' expression */







  /* constraint_prototype ::= [ 'static'   ] 'constraint' constraint_identifier */














 /* extern_constraint_declaration ::= [ 'static'   ] 'constraint' class_identifier '::' constraint_identifier { { constraint_block   } } */





   /* identifier_list ::= identifier { ','   identifier   } */
/*





A.2 Declarations





A.2.1

Declaration types




A.2.1.1

Module parameter declarations
 */















 /* local_parameter_declaration ::= 'localparam' [ signing   ] { packed_dimension   } [ range   ] list_of_param_assignments ';' | 'localparam' data_type list_of_param_assignments ';' */


















 /* parameter_declaration ::= 'parameter' [ signing   ] { packed_dimension   } [ range   ] list_of_param_assignments | 'parameter' data_type list_of_param_assignments | 'parameter' 'type' list_of_type_assignments */








 /* specparam_declaration ::= 'specparam' [ range   ] list_of_specparam_assignments ';' */
/*





A.2.1.2

Port declarations
 */









   /* inout_declaration ::= 'inout' [ port_type   ] list_of_port_identifiers | 'inout' data_type list_of_variable_identifiers */









   /* input_declaration ::= 'input' [ port_type   ] list_of_port_identifiers | 'input' data_type list_of_variable_identifiers */











 /* output_declaration ::= 'output' [ port_type   ] list_of_port_identifiers | 'output' data_type list_of_variable_port_identifiers */









 /* interface_port_declaration ::= interface_identifier list_of_interface_identifiers | interface_identifier '.' modport_identifier list_of_interface_identifiers */





   /* ref_declaration ::= 'ref' data_type list_of_port_identifiers */











   /* generic_interface_port_declaration ::= 'interface' list_of_interface_identifiers | 'interface' '.' modport_identifier list_of_interface_identifiers */
/*





A.2.1.3

Type declarations
 */






	 /* block_data_declaration ::= block_variable_declaration | constant_declaration | type_declaration */







/* constant_declaration ::= 'const' data_type const_assignment ';' */






	 /* data_declaration ::= variable_declaration | constant_declaration | type_declaration */







 /* genvar_declaration ::= 'genvar' list_of_genvar_identifiers ';' */



























































 /* net_declaration ::= net_type [ signing   ] [ delay3   ] list_of_net_identifiers ';' | net_type [ drive_strength   ] [ signing   ] [ delay3   ] list_of_net_decl_assignments ';' | net_type [ 'vectored'   |   'scalared'   ] [ signing   ] { packed_dimension   } range [ delay3   ] list_of_net_identifiers ';' | net_type [ drive_strength   ] [ 'vectored'   |   'scalared'   ] [ signing   ] { packed_dimension   } range [ delay3   ] list_of_net_decl_assignments ';' | 'trireg' [ charge_strength   ] [ signing   ] [ delay3   ] list_of_net_identifiers ';' | 'trireg' [ drive_strength   ] [ signing   ] [ delay3   ] list_of_net_decl_assignments ';' | 'trireg' [ charge_strength   ] [ 'vectored'   |   'scalared'   ] [ signing   ] { packed_dimension   } range [ delay3   ] list_of_net_identifiers ';' | 'trireg' [ drive_strength   ] [ 'vectored'   |   'scalared'   ] [ signing   ] { packed_dimension   } range [ delay3   ] list_of_net_decl_assignments ';' */



























 /* type_declaration ::= 'typedef' [ data_type   ] type_declaration_identifier ';' | 'typedef' hierarchical_identifier '.' type_identifier type_declaration_identifier ';' | 'typedef' [ 'class'   ] class_identifier ';' | 'typedef' class_identifier [ parameter_value_assignment   ] type_declaration_identifier ';' */













 /* block_variable_declaration ::= [ lifetime   ] data_type list_of_variable_identifiers ';' | lifetime data_type list_of_variable_decl_assignments ';' */








 /* variable_declaration ::= [ lifetime   ] data_type list_of_variable_identifiers_or_assignments ';' */







 /* lifetime ::= 'static' | 'automatic' */
/*






A.2.2

Declaration data types




A.2.2.1

Net and variable types
 */



       /* casting_type ::= simple_type | number | signing */































































	 /* data_type ::= integer_vector_type [ signing   ] { packed_dimension   } [ range   ] | integer_atom_type [ signing   ] | type_declaration_identifier { packed_dimension   } | non_integer_type | 'struct' 'packed' [ signing   ] { { struct_union_member   } } { packed_dimension   } | 'union' 'packed' [ signing   ] { { struct_union_member   } } { packed_dimension   } | 'struct' [ signing   ] { { struct_union_member   } } | 'union' [ signing   ] { { struct_union_member   } } | 'enum' [ integer_type   [ signing   ]   { packed_dimension   }   ] { enum_identifier [ '='   constant_expression   ] { ','   enum_identifier   [ '='   constant_expression   ]   } } | 'string' | 'event' | 'chandle' | class_scope_type_identifier */













   /* class_scope_type_identifier ::= class_identifier '::' { class_identifier   '::'   } type_declaration_identifier | class_identifier '::' { class_identifier   '::'   } class_identifier */



     /* integer_type ::= integer_vector_type | integer_atom_type */













 /* integer_atom_type ::= 'byte' | 'shortint' | 'int' | 'longint' | 'integer' */









 /* integer_vector_type ::= 'bit' | 'logic' | 'reg' */











 /* non_integer_type ::= 'time' | 'shortreal' | 'real' | 'realtime' */























 /* net_type ::= 'supply0' | 'supply1' | 'tri' | 'triand' | 'trior' | 'tri0' | 'tri1' | 'wire' | 'wand' | 'wor' */









	       /* port_type ::= data_type | net_type [ signing   ] { packed_dimension   } | 'trireg' [ signing   ] { packed_dimension   } | [ signing   ] { packed_dimension   } range */







 /* signing ::= 'signed' | 'unsigned' */



       /* simple_type ::= integer_type | non_integer_type | type_identifier */







 /* struct_union_member ::= { attribute_instance   } data_type list_of_variable_identifiers_or_assignments ';' */
/*





A.2.2.2 Strengths
 */

















    /* drive_strength ::= '(' strength0 ',' strength1 ')' | '(' strength1 ',' strength0 ')' | '(' strength0 ',' 'highz1' ')' | '(' strength1 ',' 'highz0' ')' | '(' 'highz0' ',' strength1 ')' | '(' 'highz1' ',' strength0 ')' */











 /* strength0 ::= 'supply0' | 'strong0' | 'pull0' | 'weak0' */











 /* strength1 ::= 'supply1' | 'strong1' | 'pull1' | 'weak1' */









 /* charge_strength ::= '(' 'small' ')' | '(' 'medium' ')' | '(' 'large' ')' */
/*





A.2.2.3 Delays
 */













 /* delay3 ::= '#' delay_value | '#' '(' mintypmax_expression [ ','   mintypmax_expression   [ ','   mintypmax_expression   ]   ] ')' */











 /* delay2 ::= '#' delay_value | '#' '(' mintypmax_expression [ ','   mintypmax_expression   ] ')' */






	 /* delay_value ::= 'unsigned_number' | real_number | identifier */
/*






A.2.3

Declaration lists
 */





   /* list_of_defparam_assignments ::= defparam_assignment { ','   defparam_assignment   } */





   /* list_of_genvar_identifiers ::= genvar_identifier { ','   genvar_identifier   } */






      /* list_of_interface_identifiers ::= interface_identifier { unpacked_dimension   } { ','   interface_identifier   { unpacked_dimension   }   } */





  /* list_of_modport_port_identifiers ::= port_identifier { ','   port_identifier   } */





   /* list_of_net_decl_assignments ::= net_decl_assignment { ','   net_decl_assignment   } */






      /* list_of_net_identifiers ::= net_identifier { unpacked_dimension   } { ','   net_identifier   { unpacked_dimension   }   } */





   /* list_of_param_assignments ::= param_assignment { ','   param_assignment   } */






      /* list_of_port_identifiers ::= port_identifier { unpacked_dimension   } { ','   port_identifier   { unpacked_dimension   }   } */





   /* list_of_udp_port_identifiers ::= port_identifier { ','   port_identifier   } */





   /* list_of_specparam_assignments ::= specparam_assignment { ','   specparam_assignment   } */










    /* list_of_tf_port_identifiers ::= port_identifier { unpacked_dimension   } [ '='   expression   ] { ','   port_identifier   { unpacked_dimension   }   [ '='   expression   ]   } */










    /* list_of_tf_variable_identifiers ::= port_identifier variable_dimension [ '='   expression   ] { ','   port_identifier   variable_dimension   [ '='   expression   ]   } */





   /* list_of_type_assignments ::= type_assignment { ','   type_assignment   } */





   /* list_of_variable_decl_assignments ::= variable_decl_assignment { ','   variable_decl_assignment   } */






    /* list_of_variable_identifiers ::= variable_identifier variable_dimension { ','   variable_identifier   variable_dimension   } */





	 /* list_of_variable_identifiers_or_assignments ::= list_of_variable_decl_assignments | list_of_variable_identifiers */










    /* list_of_variable_port_identifiers ::= port_identifier variable_dimension [ '='   constant_expression   ] { ','   port_identifier   variable_dimension   [ '='   constant_expression   ]   } */
/*





A.2.4

Declaration assignments
 */





  /* const_assignment ::= const_identifier '=' constant_expression */





  /* defparam_assignment ::= hierarchical_parameter_identifier '=' constant_expression */





  /* net_decl_assignment ::= net_identifier '=' expression */





  /* param_assignment ::= parameter_identifier '=' constant_param_expression */







	 /* specparam_assignment ::= specparam_identifier '=' constant_mintypmax_expression | pulse_control_specparam */





  /* type_assignment ::= type_identifier '=' data_type */
























 /* pulse_control_specparam ::= 'PATHPULSE$' '=' '(' reject_limit_value [ ','   error_limit_value   ] ')' ';' | 'PATHPULSE$' specify_input_terminal_descriptor '$' specify_output_terminal_descriptor '=' '(' reject_limit_value [ ','   error_limit_value   ] ')' ';' */



   /* error_limit_value ::= limit_value */



   /* reject_limit_value ::= limit_value */



   /* limit_value ::= constant_mintypmax_expression */






















  /* variable_decl_assignment ::= variable_identifier [ variable_dimension   ] [ '='   constant_expression   ] | variable_identifier [ ] '=' 'new' [ constant_expression ] [ '('   variable_identifier   ')'   ] | class_identifier [ parameter_value_assignment   ] '=' 'new' [ '('   list_of_arguments   ')'   ] */
/*





A.2.5 Declaration ranges
 */














 /* unpacked_dimension ::= [ dimension_constant_expression ':' dimension_constant_expression ] | [ dimension_constant_expression ] */














 /* packed_dimension ::= [ dimension_constant_expression ':' dimension_constant_expression ] | [ ] */









 /* range ::= [ msb_constant_expression ':' lsb_constant_expression ] */











 /* associative_dimension ::= [ data_type ] | [ '*' ] */








	 /* variable_dimension ::= { unpacked_dimension   } | [ ] | associative_dimension */







  /* dpi_dimension ::= variable_dimension | { [   ]   } */
/*





A.2.6

Function declarations
 */











































 /* function_data_type ::= integer_vector_type { packed_dimension   } [ range   ] | integer_atom_type | type_declaration_identifier { packed_dimension   } | non_integer_type | 'struct' [ 'packed'   ] { { struct_union_member   } } { packed_dimension   } | 'union' [ 'packed'   ] { { struct_union_member   } } { packed_dimension   } | 'enum' [ integer_type   { packed_dimension   }   ] { enum_identifier [ '='   constant_expression   ] { ','   enum_identifier   [ '='   constant_expression   ]   } } | 'string' | 'chandle' | 'void' */

































   /* function_body_declaration ::= [ signing   ] [ range_or_type   ] [ interface_identifier   '.'   ] function_identifier ';' { function_item_declaration   } { function_statement_or_null   } 'endfunction' [ ':'   function_identifier   ] | [ signing   ] [ range_or_type   ] [ interface_identifier   '.'   ] function_identifier '(' function_port_list ')' ';' { block_item_declaration   } { function_statement_or_null   } 'endfunction' [ ':'   function_identifier   ] */






     /* function_declaration ::= 'function' [ lifetime   ] function_body_declaration */
















 /* function_item_declaration ::= block_item_declaration | { attribute_instance   } tf_input_declaration ';' | { attribute_instance   } tf_output_declaration ';' | { attribute_instance   } tf_inout_declaration ';' | { attribute_instance   } tf_ref_declaration ';' */









	     /* function_port_item ::= { attribute_instance   } tf_input_declaration | { attribute_instance   } tf_output_declaration | { attribute_instance   } tf_inout_declaration | { attribute_instance   } tf_ref_declaration | { attribute_instance   } port_type list_of_tf_port_identifiers | { attribute_instance   } tf_data_type list_of_tf_variable_identifiers */





   /* function_port_list ::= function_port_item { ','   function_port_item   } */







 /* named_function_proto ::= [ signing   ] function_data_type function_identifier '(' list_of_function_proto_formals ')' */






       /* list_of_function_proto_formals ::= [ { attribute_instance   }   function_proto_formal   { ','   { attribute_instance   }   function_proto_formal   }   ] */







	 /* function_proto_formal ::= tf_input_declaration | tf_output_declaration | tf_inout_declaration | tf_ref_declaration */





	 /* range_or_type ::= { packed_dimension   } range | function_data_type */



















 /* dpi_import_export ::= 'import' '"DPI"' [ dpi_import_property   ] [ 'c_identifier'   '='   ] dpi_function_proto ';' | 'export' '"DPI"' [ 'c_identifier'   '='   ] 'function' function_identifier ';' */







 /* dpi_import_property ::= 'context' | 'pure' */









 /* dpi_function_proto ::= named_function_proto | [ signing   ] function_data_type function_identifier '(' list_of_dpi_proto_formals ')' */






       /* list_of_dpi_proto_formals ::= [ { attribute_instance   }   dpi_proto_formal   { ','   { attribute_instance   }   dpi_proto_formal   }   ] */






     /* dpi_proto_formal ::= data_type [ port_identifier   dpi_dimension   { ','   port_identifier   dpi_dimension   }   ] */
/*





A.2.7

Task declarations
 */





























   /* task_body_declaration ::= [ interface_identifier   '.'   ] task_identifier ';' { task_item_declaration   } { statement_or_null   } 'endtask' [ ':'   task_identifier   ] | [ interface_identifier   '.'   ] task_identifier '(' task_port_list ')' ';' { block_item_declaration   } { statement_or_null   } 'endtask' [ ':'   task_identifier   ] */





     /* task_declaration ::= 'task' [ lifetime   ] task_body_declaration */
















 /* task_item_declaration ::= block_item_declaration | { attribute_instance   } tf_input_declaration ';' | { attribute_instance   } tf_output_declaration ';' | { attribute_instance   } tf_inout_declaration ';' | { attribute_instance   } tf_ref_declaration ';' */








   /* task_port_list ::= task_port_item { ','   task_port_item   } | list_of_port_identifiers { ','   task_port_item   } */











	     /* task_port_item ::= { attribute_instance   } tf_input_declaration | { attribute_instance   } tf_output_declaration | { attribute_instance   } tf_inout_declaration | { attribute_instance   } tf_ref_declaration ';' | { attribute_instance   } port_type list_of_tf_port_identifiers | { attribute_instance   } tf_data_type list_of_tf_variable_identifiers */









   /* tf_input_declaration ::= 'input' [ signing   ] { packed_dimension   } list_of_tf_port_identifiers | 'input' tf_data_type list_of_tf_variable_identifiers */









   /* tf_output_declaration ::= 'output' [ signing   ] { packed_dimension   } list_of_tf_port_identifiers | 'output' tf_data_type list_of_tf_variable_identifiers */









   /* tf_inout_declaration ::= 'inout' [ signing   ] { packed_dimension   } list_of_tf_port_identifiers | 'inout' tf_data_type list_of_tf_variable_identifiers */








   /* tf_ref_declaration ::= [ 'const'   ] 'ref' tf_data_type list_of_tf_variable_identifiers */







 /* tf_data_type ::= data_type | 'chandle' */









 /* named_task_proto ::= task_identifier '(' task_proto_formal { ','   task_proto_formal   } ')' */







	 /* task_proto_formal ::= tf_input_declaration | tf_output_declaration | tf_inout_declaration | tf_ref_declaration */
/*





A.2.8 Block item declarations
 */








 /* block_item_declaration ::= { attribute_instance   } block_data_declaration | { attribute_instance   } local_parameter_declaration | { attribute_instance   } parameter_declaration ';' */
/*





A.2.9

Interface declarations
 */









 /* modport_declaration ::= 'modport' modport_item { ','   modport_item   } ';' */









 /* modport_item ::= modport_identifier '(' modport_ports_declaration { ','   modport_ports_declaration   } ')' */






	 /* modport_ports_declaration ::= modport_simple_ports_declaration | modport_hierarchical_ports_declaration | modport_tf_ports_declaration */















     /* modport_simple_ports_declaration ::= 'input' list_of_modport_port_identifiers | 'output' list_of_modport_port_identifiers | 'inout' list_of_modport_port_identifiers | 'ref' [ data_type   ] list_of_modport_port_identifiers */










  /* modport_hierarchical_ports_declaration ::= interface_instance_identifier [ [   constant_expression   ]   ] '.' modport_identifier */




 /* modport_tf_ports_declaration ::= import_export modport_tf_port */
















   /* modport_tf_port ::= 'task' named_task_proto { ','   named_task_proto   } | 'function' named_function_proto { ','   named_function_proto   } | task_or_function_identifier { ','   task_or_function_identifier   } */







 /* import_export ::= 'import' | 'export' */
/*





A.2.10

Assertion declarations
 */






	 /* concurrent_assertion_item ::= concurrent_assert_statement | concurrent_cover_statement | concurrent_assertion_item_declaration */






  /* concurrent_assert_statement ::= [ block_identifier   ':'   ] assert_property_statement */






  /* concurrent_cover_statement ::= [ block_identifier   ':'   ] cover_property_statement */





















  /* assert_property_statement ::= 'assert' 'property' '(' property_spec ')' action_block | 'assert' 'property' '(' property_instance ')' action_block */





















  /* cover_property_statement ::= 'cover' 'property' '(' property_spec ')' statement_or_null | 'cover' 'property' '(' property_instance ')' statement_or_null */








  /* property_instance ::= property_identifier [ '('   actual_arg_list   ')'   ] */





	 /* concurrent_assertion_item_declaration ::= property_declaration | sequence_declaration */

















   /* property_declaration ::= 'property' property_identifier [ property_formal_list   ] ';' { assertion_variable_declaration   } property_spec ';' 'endproperty' [ ':'   property_identifier   ] */










 /* property_formal_list ::= '(' formal_list_item { ','   formal_list_item   } ')' */



















   /* property_spec ::= [ clocking_event   ] [ 'disable'   'iff'   ] '(' expression ')' [ 'not'   ] property_expr | [ 'disable'   'iff'   '('   expression   ')'   ] [ 'not'   ] multi_clock_property_expr */



















 /* property_expr ::= sequence_expr | sequence_expr '|->' [ 'not'   ] sequence_expr | sequence_expr '|=>' [ 'not'   ] sequence_expr | '(' property_expr ')' */














 /* multi_clock_property_expr ::= multi_clock_sequence | multi_clock_sequence '|=>' [ 'not'   ] multi_clock_sequence | '(' multi_clock_property_expr ')' */

















   /* sequence_declaration ::= 'sequence' sequence_identifier [ sequence_formal_list   ] ';' { assertion_variable_declaration   } sequence_spec ';' 'endsequence' [ ':'   sequence_identifier   ] */










 /* sequence_formal_list ::= '(' formal_list_item { ','   formal_list_item   } ')' */





	 /* sequence_spec ::= multi_clock_sequence | sequence_expr */






   /* multi_clock_sequence ::= clocked_sequence { '##'   clocked_sequence   } */




	  /* clocked_sequence ::= clocking_event sequence_expr */











































  /* sequence_expr ::= cycle_delay_range sequence_expr { cycle_delay_range   sequence_expr   } | sequence_expr cycle_delay_range sequence_expr { cycle_delay_range   sequence_expr   } | expression { ','   function_blocking_assignment   } [ boolean_abbrev   ] | '(' expression { ','   function_blocking_assignment   } ')' [ boolean_abbrev   ] | sequence_instance [ sequence_abbrev   ] | '(' sequence_expr ')' [ sequence_abbrev   ] | sequence_expr 'and' sequence_expr | sequence_expr 'intersect' sequence_expr | sequence_expr 'or' sequence_expr | 'first_match' '(' sequence_expr ')' | expression 'throughout' sequence_expr | sequence_expr 'within' sequence_expr */













 /* cycle_delay_range ::= '##' constant_expression | '##' [ cycle_delay_const_range_expression ] */








  /* sequence_instance ::= sequence_identifier [ '('   actual_arg_list   ')'   ] */






   /* formal_list_item ::= formal_identifier [ '='   actual_arg_expr   ] */

























 /* actual_arg_list ::= '(' actual_arg_expr { ','   actual_arg_expr   } ')' | '(' '.' formal_identifier '(' actual_arg_expr ')' { ','   '.'   formal_identifier   '('   actual_arg_expr   ')'   } ')' */




	 /* actual_arg_expr ::= event_expression */






	/* boolean_abbrev ::= consecutive_repetition | non_consecutive_repetition | goto_repetition */



   /* sequence_abbrev ::= consecutive_repetition */







 /* consecutive_repetition ::= '[*' const_or_range_expression ] */







 /* non_consecutive_repetition ::= '[*=' const_or_range_expression ] */







 /* goto_repetition ::= '[*->' const_or_range_expression ] */





	 /* const_or_range_expression ::= constant_expression | cycle_delay_const_range_expression */









 /* cycle_delay_const_range_expression ::= constant_expression ':' constant_expression | constant_expression ':' '$' */




	   /* assertion_variable_declaration ::= data_type list_of_variable_identifiers ';' */
/*





A.3 Primitive instances





A.3.1 Primitive instantiation and instances
 */



















































 /* gate_instantiation ::= cmos_switchtype [ delay3   ] cmos_switch_instance { ','   cmos_switch_instance   } ';' | enable_gatetype [ drive_strength   ] [ delay3   ] enable_gate_instance { ','   enable_gate_instance   } ';' | mos_switchtype [ delay3   ] mos_switch_instance { ','   mos_switch_instance   } ';' | n_input_gatetype [ drive_strength   ] [ delay2   ] n_input_gate_instance { ','   n_input_gate_instance   } ';' | n_output_gatetype [ drive_strength   ] [ delay2   ] n_output_gate_instance { ','   n_output_gate_instance   } ';' | pass_en_switchtype [ delay2   ] pass_enable_switch_instance { ','   pass_enable_switch_instance   } ';' | pass_switchtype pass_switch_instance { ','   pass_switch_instance   } ';' | 'pulldown' [ pulldown_strength   ] pull_gate_instance { ','   pull_gate_instance   } ';' | 'pullup' [ pullup_strength   ] pull_gate_instance { ','   pull_gate_instance   } ';' */














 /* cmos_switch_instance ::= [ name_of_gate_instance   ] '(' output_terminal ',' input_terminal ',' ncontrol_terminal ',' pcontrol_terminal ')' */











 /* enable_gate_instance ::= [ name_of_gate_instance   ] '(' output_terminal ',' input_terminal ',' enable_terminal ')' */











 /* mos_switch_instance ::= [ name_of_gate_instance   ] '(' output_terminal ',' input_terminal ',' enable_terminal ')' */











 /* n_input_gate_instance ::= [ name_of_gate_instance   ] '(' output_terminal ',' input_terminal { ','   input_terminal   } ')' */












 /* n_output_gate_instance ::= [ name_of_gate_instance   ] '(' output_terminal { ','   output_terminal   } ',' input_terminal ')' */









 /* pass_switch_instance ::= [ name_of_gate_instance   ] '(' inout_terminal ',' inout_terminal ')' */










 /* pass_enable_switch_instance ::= [ name_of_gate_instance   ] '(' inout_terminal ',' inout_terminal ',' enable_terminal ')' */







 /* pull_gate_instance ::= [ name_of_gate_instance   ] '(' output_terminal ')' */



      /* name_of_gate_instance ::= gate_instance_identifier { range   } */
/*





A.3.2 Primitive strengths
 */






















 /* pulldown_strength ::= '(' strength0 ',' strength1 ')' | '(' strength1 ',' strength0 ')' | '(' strength0 ')' */






















 /* pullup_strength ::= '(' strength0 ',' strength1 ')' | '(' strength1 ',' strength0 ')' | '(' strength1 ')' */
/*





A.3.3 Primitive terminals
 */



   /* enable_terminal ::= expression */



   /* inout_terminal ::= net_lvalue */



   /* input_terminal ::= expression */



   /* ncontrol_terminal ::= expression */



   /* output_terminal ::= net_lvalue */



   /* pcontrol_terminal ::= expression */
/*





A.3.4 Primitive gate and switch types
 */







 /* cmos_switchtype ::= 'cmos' | 'rcmos' */











 /* enable_gatetype ::= 'bufif0' | 'bufif1' | 'notif0' | 'notif1' */











 /* mos_switchtype ::= 'nmos' | 'pmos' | 'rnmos' | 'rpmos' */















 /* n_input_gatetype ::= 'and' | 'nand' | 'or' | 'nor' | 'xor' | 'xnor' */







 /* n_output_gatetype ::= 'buf' | 'not' */











 /* pass_en_switchtype ::= 'tranif0' | 'tranif1' | 'rtranif1' | 'rtranif0' */







 /* pass_switchtype ::= 'tran' | 'rtran' */
/*





A.4 Module, interface and generated instantiation





A.4.1 Instantiation




A.4.1.1

Module instantiation
 */








 /* module_instantiation ::= module_identifier [ parameter_value_assignment   ] module_instance { ','   module_instance   } ';' */







 /* parameter_value_assignment ::= '#' '(' list_of_parameter_assignments ')' */









   /* list_of_parameter_assignments ::= ordered_parameter_assignment { ','   ordered_parameter_assignment   } | named_parameter_assignment { ','   named_parameter_assignment   } */



     /* ordered_parameter_assignment ::= expression | data_type */

















 /* named_parameter_assignment ::= '.' parameter_identifier '(' [ expression   ] ')' | '.' parameter_identifier '(' data_type ')' */







 /* module_instance ::= name_of_instance '(' [ list_of_port_connections   ] ')' */



      /* name_of_instance ::= module_instance_identifier { range   } */














   /* list_of_port_connections ::= ordered_port_connection { ','   ordered_port_connection   } | dot_named_port_connection { ','   dot_named_port_connection   } | { named_port_connection   ','   } dot_star_port_connection { ','   named_port_connection   } */



        /* ordered_port_connection ::= { attribute_instance   } [ expression   ] */









 /* named_port_connection ::= { attribute_instance   } '.' port_identifier '(' [ expression   ] ')' */







	 /* dot_named_port_connection ::= { attribute_instance   } '.' port_identifier | named_port_connection */





 /* dot_star_port_connection ::= { attribute_instance   } '.*' */
/*





A.4.1.2

Interface instantiation
 */








 /* interface_instantiation ::= interface_identifier [ parameter_value_assignment   ] module_instance { ','   module_instance   } ';' */
/*





Program instantiation
 */








 /* program_instantiation ::= program_identifier [ parameter_value_assignment   ] program_instance { ','   program_instance   } ';' */







 /* program_instance ::= program_instance_identifier { range   } '(' [ list_of_port_connections   ] ')' */
/*






A.4.2 Generated instantiation




A.4.2.1 Generated module instantiation
 */







 /* generated_module_instantiation ::= 'generate' { generate_module_item   } 'endgenerate' */








	 /* generate_module_item ::= generate_module_conditional_statement | generate_module_case_statement | generate_module_loop_statement | [ generate_block_identifier   ':'   ] generate_module_block | module_or_generate_item */








   /* generate_module_conditional_statement ::= 'if' '(' constant_expression ')' generate_module_item [ 'else'   generate_module_item   ] */












 /* generate_module_case_statement ::= 'case' '(' constant_expression ')' genvar_module_case_item { genvar_module_case_item   } 'endcase' */













   /* genvar_module_case_item ::= constant_expression { ','   constant_expression   } ':' generate_module_item | 'default' [ ':'   ] generate_module_item */















		 /* generate_module_loop_statement ::= 'for' '(' genvar_decl_assignment ';' constant_expression ';' genvar_assignment ')' generate_module_named_block */






	  /* genvar_assignment ::= genvar_identifier assignment_operator constant_expression | inc_or_dec_operator genvar_identifier | genvar_identifier inc_or_dec_operator */








  /* genvar_decl_assignment ::= [ 'genvar'   ] genvar_identifier '=' constant_expression */











  /* generate_module_named_block ::= 'begin' ':' generate_block_identifier { generate_module_item   } 'end' [ ':'   generate_block_identifier   ] | generate_block_identifier ':' generate_module_block */












   /* generate_module_block ::= 'begin' [ ':'   generate_block_identifier   ] { generate_module_item   } 'end' [ ':'   generate_block_identifier   ] */
/*





A.4.2.2 Generated interface instantiation
 */







 /* generated_interface_instantiation ::= 'generate' { generate_interface_item   } 'endgenerate' */








	 /* generate_interface_item ::= generate_interface_conditional_statement | generate_interface_case_statement | generate_interface_loop_statement | [ generate_block_identifier   ':'   ] generate_interface_block | interface_or_generate_item */








   /* generate_interface_conditional_statement ::= 'if' '(' constant_expression ')' generate_interface_item [ 'else'   generate_interface_item   ] */








 /* generate_interface_case_statement ::= 'case' '(' constant_expression ')' genvar_interface_case_item { genvar_interface_case_item   } 'endcase' */













   /* genvar_interface_case_item ::= constant_expression { ','   constant_expression   } ':' generate_interface_item | 'default' [ ':'   ] generate_interface_item */













		 /* generate_interface_loop_statement ::= 'for' '(' genvar_decl_assignment ';' constant_expression ';' genvar_assignment ')' generate_interface_named_block */











  /* generate_interface_named_block ::= 'begin' ':' generate_block_identifier { generate_interface_item   } 'end' [ ':'   generate_block_identifier   ] | generate_block_identifier ':' generate_interface_block */














   /* generate_interface_block ::= 'begin' [ ':'   generate_block_identifier   ] { generate_interface_item   } 'end' [ ':'   generate_block_identifier   ] */
/*






A.5 UDP declaration and instantiation





A.5.1 UDP declaration
 */










 /* udp_nonansi_declaration ::= { attribute_instance   } 'primitive' udp_identifier '(' udp_port_list ')' ';' */










 /* udp_ansi_declaration ::= { attribute_instance   } 'primitive' udp_identifier '(' udp_declaration_port_list ')' ';' */































   /* udp_declaration ::= udp_nonansi_declaration udp_port_declaration { udp_port_declaration   } udp_body 'endprimitive' [ ':'   udp_identifier   ] | udp_ansi_declaration udp_body 'endprimitive' [ ':'   udp_identifier   ] | 'extern' udp_nonansi_declaration | 'extern' udp_ansi_declaration | { attribute_instance   } 'primitive' udp_identifier '(' '.*' ')' ';' { udp_port_declaration   } udp_body 'endprimitive' [ ':'   udp_identifier   ] */


























 /* udp_declaration2 ::= { attribute_instance   } 'primitive' udp_identifier '(' udp_port_list ')' ';' udp_port_declaration { udp_port_declaration   } udp_body 'endprimitive' | { attribute_instance   } 'primitive' udp_identifier '(' udp_declaration_port_list ')' ';' udp_body 'endprimitive' */
/*





A.5.2 UDP ports
 */







   /* udp_port_list ::= output_port_identifier ',' input_port_identifier { ','   input_port_identifier   } */







   /* udp_declaration_port_list ::= udp_output_declaration ',' udp_input_declaration { ','   udp_input_declaration   } */






	  /* udp_port_declaration ::= udp_output_declaration ';' | udp_input_declaration ';' | udp_reg_declaration ';' */













   /* udp_output_declaration ::= { attribute_instance   } 'output' port_identifier | { attribute_instance   } 'output' 'reg' port_identifier [ '='   constant_expression   ] */





  /* udp_input_declaration ::= { attribute_instance   } 'input' list_of_udp_port_identifiers */





  /* udp_reg_declaration ::= { attribute_instance   } 'reg' variable_identifier */
/*





A.5.3 UDP body
 */



     /* udp_body ::= combinational_body | sequential_body */







 /* combinational_body ::= 'table' combinational_entry { combinational_entry   } 'endtable' */







 /* combinational_entry ::= level_input_list ':' 'output_symbol' ';' */







 /* sequential_body ::= [ udp_initial_statement   ] 'table' sequential_entry { sequential_entry   } 'endtable' */









 /* udp_initial_statement ::= 'initial' output_port_identifier '=' init_val ';' */























 /* init_val ::= '1'b0' | '1'b1' | '1'bx' | '1'bX' | '1'B0' | '1'B1' | '1'Bx' | '1'BX' | '1' | '0' */









 /* sequential_entry ::= seq_input_list ':' current_state ':' next_state ';' */



     /* seq_input_list ::= level_input_list | edge_input_list */



      /* level_input_list ::= 'level_symbol' { 'level_symbol'   } */



         /* edge_input_list ::= { 'level_symbol'   } edge_indicator { 'level_symbol'   } */





    /* edge_indicator ::= '(' 'level_symbol' 'level_symbol' ')' | 'edge_symbol' */



   /* current_state ::= 'level_symbol' */





 /* next_state ::= 'output_symbol' | - */











 /* 'output_symbol' ::= '0' | '1' | X | X */

















 /* 'level_symbol' ::= '0' | '1' | x | X | '?' | B | B */





















 /* 'edge_symbol' ::= R | R | F | F | P | P | N | N | '*' */
/*





A.5.4 UDP instantiation
 */







 /* udp_instantiation ::= udp_identifier [ drive_strength   ] [ delay2   ] udp_instance { ','   udp_instance   } ';' */











 /* udp_instance ::= [ 'name_of_udp_instance'   ] { range   } '(' output_terminal ',' input_terminal { ','   input_terminal   } ')' */



      /* 'name_of_udp_instance' ::= udp_instance_identifier { range   } */
/*





A.6 Behavioral statements





A.6.1 Continuous assignment and net alias statements
 */













 /* continuous_assign ::= 'assign' [ drive_strength   ] [ delay3   ] list_of_net_assignments ';' | 'assign' [ delay_control   ] list_of_variable_assignments ';' */





   /* list_of_net_assignments ::= net_assignment { ','   net_assignment   } */





   /* list_of_variable_assignments ::= variable_assignment { ','   variable_assignment   } */









 /* net_alias ::= 'alias' net_lvalue '=' net_lvalue ';' */





  /* net_assignment ::= net_lvalue '=' expression */
/*





A.6.2

Procedural blocks and assignments
 */





  /* initial_construct ::= 'initial' statement_or_null */





  /* always_construct ::= 'always' statement */





  /* combinational_construct ::= 'always_comb' statement */





  /* latch_construct ::= 'always_latch' statement */





  /* ff_construct ::= 'always_ff' statement */





  /* final_construct ::= 'final' function_statement */


































	 /* blocking_assignment ::= variable_lvalue '=' delay_or_event_control expression | hierarchical_variable_identifier '=' 'new' [ constant_expression ] [ '('   variable_identifier   ')'   ] | class_identifier [ parameter_value_assignment   ] '=' 'new' [ '('   list_of_arguments   ')'   ] | class_identifier '.' 'randomize' [ '('   ')'   ] 'with' constraint_block ';' | operator_assignment */







 /* operator_assignment ::= variable_lvalue assignment_operator expression */






























 /* assignment_operator ::= '=' | '+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' | '<<=' | '>>=' | '<<<=' | '>>>=' */





     /* nonblocking_assignment ::= variable_lvalue '<=' [ delay_or_event_control   ] expression */





















  /* procedural_continuous_assignments ::= 'assign' variable_assignment | 'deassign' variable_lvalue | 'force' variable_assignment | 'force' net_assignment | 'release' variable_lvalue | 'release' net_lvalue */





  /* function_blocking_assignment ::= variable_lvalue '=' expression */







 /* function_statement_or_null ::= function_statement | { attribute_instance   } ';' */





	 /* variable_assignment ::= operator_assignment | inc_or_dec_expression */
/*





A.6.3

Parallel and sequential blocks
 */







  /* action_block ::= statement_or_null | [ statement   ] 'else' statement_or_null */













   /* function_seq_block ::= 'begin' [ ':'   block_identifier   { block_item_declaration   }   ] { function_statement_or_null   } 'end' [ ':'   block_identifier   ] */













   /* seq_block ::= 'begin' [ ':'   block_identifier   ] { block_item_declaration   } { statement_or_null   } 'end' [ ':'   block_identifier   ] */











   /* par_block ::= 'fork' [ ':'   block_identifier   ] { block_item_declaration   } { statement_or_null   } join_keyword [ ':'   block_identifier   ] */









 /* join_keyword ::= 'join' | 'join_any' | 'join_none' */
/*





A.6.4

Statements
 */







 /* statement_or_null ::= statement | { attribute_instance   } ';' */





   /* statement ::= [ block_identifier   ':'   ] statement_item */
































	    /* statement_item ::= { attribute_instance   } blocking_assignment ';' | { attribute_instance   } nonblocking_assignment ';' | { attribute_instance   } procedural_continuous_assignments ';' | { attribute_instance   } case_statement | { attribute_instance   } conditional_statement | { attribute_instance   } inc_or_dec_expression ';' | { attribute_instance   } function_call ';' | { attribute_instance   } disable_statement | { attribute_instance   } event_trigger | { attribute_instance   } loop_statement | { attribute_instance   } jump_statement | { attribute_instance   } par_block | { attribute_instance   } procedural_timing_control_statement | { attribute_instance   } seq_block | { attribute_instance   } system_task_enable | { attribute_instance   } task_enable | { attribute_instance   } wait_statement | { attribute_instance   } procedural_assertion_item | { attribute_instance   } clocking_drive */





   /* function_statement ::= [ block_identifier   ':'   ] function_statement_item */



















	    /* function_statement_item ::= { attribute_instance   } function_blocking_assignment ';' | { attribute_instance   } function_case_statement | { attribute_instance   } function_conditional_statement | { attribute_instance   } inc_or_dec_expression ';' | { attribute_instance   } function_call ';' | { attribute_instance   } function_loop_statement | { attribute_instance   } jump_statement | { attribute_instance   } function_seq_block | { attribute_instance   } disable_statement | { attribute_instance   } system_task_enable */
/*





A.6.5

Timing control statements
 */




	  /* procedural_timing_control_statement ::= procedural_timing_control statement_or_null */












  /* delay_or_event_control ::= delay_control | event_control | 'repeat' '(' expression ')' event_control */











 /* delay_control ::= '#' delay_value | '#' '(' mintypmax_expression ')' */

















 /* event_control ::= '@' event_identifier | '@' '(' event_expression ')' | '@*' | '@' '(*)' */












  /* event_expression ::= [ edge_identifier   ] expression [ 'iff'   expression   ] | event_expression 'or' event_expression | event_expression ',' event_expression */





	/* procedural_timing_control ::= delay_control | event_control */














 /* jump_statement ::= 'return' [ expression   ] ';' | 'break' ';' | 'continue' ';' */




















  /* wait_statement ::= 'wait' '(' expression ')' statement_or_null | 'wait' 'fork' ';' | 'wait_order' '(' hierarchical_identifier [ ','   hierarchical_identifier   ] ')' action_block */













 /* event_trigger ::= '->' hierarchical_event_identifier ';' | '->>' [ delay_or_event_control   ] hierarchical_event_identifier ';' */
















 /* disable_statement ::= 'disable' hierarchical_task_identifier ';' | 'disable' hierarchical_block_identifier ';' | 'disable' 'fork' ';' */
/*





A.6.6

Conditional statements
 */













	 /* conditional_statement ::= [ unique_priority   ] 'if' '(' expression ')' statement_or_null [ 'else'   statement_or_null   ] | if_else_if_statement */






















   /* if_else_if_statement ::= [ unique_priority   ] 'if' '(' expression ')' statement_or_null { 'else'   [ unique_priority   ]   'if'   '('   expression   ')'   statement_or_null   } [ 'else'   statement_or_null   ] */














	 /* function_conditional_statement ::= [ unique_priority   ] 'if' '(' expression ')' function_statement_or_null [ 'else'   function_statement_or_null   ] | function_if_else_if_statement */






















   /* function_if_else_if_statement ::= [ unique_priority   ] 'if' '(' expression ')' function_statement_or_null { 'else'   [ unique_priority   ]   'if'   '('   expression   ')'   function_statement_or_null   } [ 'else'   function_statement_or_null   ] */







 /* unique_priority ::= 'unique' | 'priority' */
/*





A.6.7

Case statements
 */
























 /* case_statement ::= [ unique_priority   ] 'case' '(' expression ')' case_item { case_item   } 'endcase' | [ unique_priority   ] 'casez' '(' expression ')' case_item { case_item   } 'endcase' | [ unique_priority   ] 'casex' '(' expression ')' case_item { case_item   } 'endcase' */













   /* case_item ::= expression { ','   expression   } ':' statement_or_null | 'default' [ ':'   ] statement_or_null */






























 /* function_case_statement ::= [ unique_priority   ] 'case' '(' expression ')' function_case_item { function_case_item   } 'endcase' | [ unique_priority   ] 'casez' '(' expression ')' function_case_item { function_case_item   } 'endcase' | [ unique_priority   ] 'casex' '(' expression ')' function_case_item { function_case_item   } 'endcase' */













   /* function_case_item ::= expression { ','   expression   } ':' function_statement_or_null | 'default' [ ':'   ] function_statement_or_null */
/*





A.6.8

Looping statements
 */















































 /* function_loop_statement ::= 'forever' function_statement_or_null | 'repeat' '(' expression ')' function_statement_or_null | 'while' '(' expression ')' function_statement_or_null | 'for' '(' variable_decl_or_assignment { ','   variable_decl_or_assignment   } ';' expression ';' variable_assignment { ','   variable_assignment   } ')' function_statement_or_null | 'do' function_statement_or_null 'while' '(' expression ')' ';' */


























































 /* loop_statement ::= 'forever' statement_or_null | 'repeat' '(' expression ')' statement_or_null | 'while' '(' expression ')' statement_or_null | 'for' '(' variable_decl_or_assignment ';' expression ';' variable_assignment ')' statement_or_null | 'for' '(' variable_decl_or_assignment { ','   variable_decl_or_assignment   } ';' expression ';' variable_assignment { ','   variable_assignment   } ')' statement_or_null | 'do' statement_or_null 'while' '(' expression ')' ';' */





	 /* variable_decl_or_assignment ::= data_type list_of_variable_identifiers_or_assignments | variable_assignment */
/*





A.6.9 Task enable statements
 */











 /* system_task_enable ::= 'system_task_identifier' [ '('   [ expression   ]   { ','   [ expression   ]   }   ')'   ] ';' */









 /* task_enable ::= hierarchical_task_identifier [ '('   list_of_arguments   ')'   ] ';' */
/*





A.6.10

Assertion statements
 */






	 /* procedural_assertion_item ::= assert_property_statement | cover_property_statement | immediate_assert_statement */










  /* immediate_assert_statement ::= 'assert' '(' expression ')' action_block */
/*





A.6.11

Clocking domain
 */













 /* clocking_decl ::= [ 'default'   ] 'clocking' [ clocking_identifier   ] clocking_event ';' { clocking_item   } 'endclocking' */











 /* clocking_event ::= '@' identifier | '@' '(' event_expression ')' */












	    /* clocking_item ::= 'default' default_skew ';' | clocking_direction list_of_clocking_decl_assign ';' | { attribute_instance   } concurrent_assertion_item_declaration */














  /* default_skew ::= 'input' clocking_skew | 'output' clocking_skew | 'input' clocking_skew 'output' clocking_skew */

















 /* clocking_direction ::= 'input' [ clocking_skew   ] | 'output' [ clocking_skew   ] | 'input' [ clocking_skew   ] 'output' [ clocking_skew   ] | 'inout' */



       /* list_of_clocking_decl_assign ::= clocking_decl_assign { ','   clocking_decl_assign   } */





   /* clocking_decl_assign ::= signal_identifier [ '='   hierarchical_identifier   ] */





	 /* clocking_skew ::= edge_identifier [ delay_control   ] | delay_control */









  /* clocking_drive ::= clockvar_expression '<=' [ cycle_delay   ] expression | cycle_delay clockvar_expression '<=' expression */





  /* cycle_delay ::= '##' expression */





  /* clockvar ::= clocking_identifier '.' identifier */









 /* clockvar_expression ::= clockvar range | clockvar [ range_expression ] */
/*





A.7 Specify section





A.7.1 Specify block declaration
 */







 /* specify_block ::= 'specify' { specify_item   } 'endspecify' */








	 /* specify_item ::= specparam_declaration | pulsestyle_declaration | showcancelled_declaration | path_declaration | system_timing_check */













 /* pulsestyle_declaration ::= 'pulsestyle_onevent' list_of_path_outputs ';' | 'pulsestyle_ondetect' list_of_path_outputs ';' */













 /* showcancelled_declaration ::= 'showcancelled' list_of_path_outputs ';' | 'noshowcancelled' list_of_path_outputs ';' */
/*





A.7.2 Specify path declarations
 */












 /* path_declaration ::= simple_path_declaration ';' | edge_sensitive_path_declaration ';' | state_dependent_path_declaration ';' */









  /* simple_path_declaration ::= parallel_path_description '=' path_delay_value | full_path_description '=' path_delay_value */










 /* parallel_path_description ::= '(' specify_input_terminal_descriptor [ polarity_operator   ] '=>' specify_output_terminal_descriptor ')' */










 /* full_path_description ::= '(' list_of_path_inputs [ polarity_operator   ] '*>' list_of_path_outputs ')' */






   /* list_of_path_inputs ::= specify_input_terminal_descriptor { ','   specify_input_terminal_descriptor   } */






   /* list_of_path_outputs ::= specify_output_terminal_descriptor { ','   specify_output_terminal_descriptor   } */
/*





A.7.3 Specify block terminals
 */








  /* specify_input_terminal_descriptor ::= input_identifier [ [   constant_range_expression   ]   ] */








  /* specify_output_terminal_descriptor ::= output_identifier [ [   constant_range_expression   ]   ] */



     /* input_identifier ::= input_port_identifier | inout_port_identifier */



     /* output_identifier ::= output_port_identifier | inout_port_identifier */
/*





A.7.4

Specify path delays
 */









 /* path_delay_value ::= list_of_path_delay_expressions | '(' list_of_path_delay_expressions ')' */


















































  /* list_of_path_delay_expressions ::= t_path_delay_expression | trise_path_delay_expression ',' tfall_path_delay_expression | trise_path_delay_expression ',' tfall_path_delay_expression ',' tz_path_delay_expression | t01_path_delay_expression ',' t10_path_delay_expression ',' t0z_path_delay_expression ',' tz1_path_delay_expression ',' t1z_path_delay_expression ',' tz0_path_delay_expression | t01_path_delay_expression ',' t10_path_delay_expression ',' t0z_path_delay_expression ',' tz1_path_delay_expression ',' t1z_path_delay_expression ',' tz0_path_delay_expression ',' t0x_path_delay_expression ',' tx1_path_delay_expression ',' t1x_path_delay_expression ',' tx0_path_delay_expression ',' txz_path_delay_expression ',' tzx_path_delay_expression */



   /* t_path_delay_expression ::= path_delay_expression */



   /* trise_path_delay_expression ::= path_delay_expression */



   /* tfall_path_delay_expression ::= path_delay_expression */



   /* tz_path_delay_expression ::= path_delay_expression */



   /* t01_path_delay_expression ::= path_delay_expression */



   /* t10_path_delay_expression ::= path_delay_expression */



   /* t0z_path_delay_expression ::= path_delay_expression */



   /* tz1_path_delay_expression ::= path_delay_expression */



   /* t1z_path_delay_expression ::= path_delay_expression */



   /* tz0_path_delay_expression ::= path_delay_expression */



   /* t0x_path_delay_expression ::= path_delay_expression */



   /* tx1_path_delay_expression ::= path_delay_expression */



   /* t1x_path_delay_expression ::= path_delay_expression */



   /* tx0_path_delay_expression ::= path_delay_expression */



   /* txz_path_delay_expression ::= path_delay_expression */



   /* tzx_path_delay_expression ::= path_delay_expression */



   /* path_delay_expression ::= constant_mintypmax_expression */









  /* edge_sensitive_path_declaration ::= parallel_edge_sensitive_path_description '=' path_delay_value | full_edge_sensitive_path_description '=' path_delay_value */













 /* parallel_edge_sensitive_path_description ::= '(' [ edge_identifier   ] specify_input_terminal_descriptor '=>' specify_output_terminal_descriptor [ polarity_operator   ] ':' data_source_expression ')' */













 /* full_edge_sensitive_path_description ::= '(' [ edge_identifier   ] list_of_path_inputs '*>' list_of_path_outputs [ polarity_operator   ] ':' data_source_expression ')' */



   /* data_source_expression ::= expression */







 /* edge_identifier ::= 'posedge' | 'negedge' */




















  /* state_dependent_path_declaration ::= 'if' '(' module_path_expression ')' simple_path_declaration | 'if' '(' module_path_expression ')' edge_sensitive_path_declaration | 'ifnone' simple_path_declaration */







 /* polarity_operator ::= '+' | - */
/*





A.7.5 System timing checks




A.7.5.1 System timing check commands
 */















	 /* system_timing_check ::= $setup_timing_check | $hold_timing_check | $setuphold_timing_check | $recovery_timing_check | $removal_timing_check | $recrem_timing_check | $skew_timing_check | $timeskew_timing_check | $fullskew_timing_check | $period_timing_check | $width_timing_check | $nochange_timing_check */














 /* $setup_timing_check ::= '$setup' '(' data_event ',' reference_event ',' timing_check_limit [ ','   [ notify_reg   ]   ] ')' ';' */














 /* $hold_timing_check ::= '$hold' '(' reference_event ',' data_event ',' timing_check_limit [ ','   [ notify_reg   ]   ] ')' ';' */


























 /* $setuphold_timing_check ::= '$setuphold' '(' reference_event ',' data_event ',' timing_check_limit ',' timing_check_limit [ ','   [ notify_reg   ]   [ ','   [ stamptime_condition   ]   [ ','   [ checktime_condition   ]   [ ','   [ delayed_reference   ]   [ ','   [ delayed_data   ]   ]   ]   ]   ]   ] ')' ';' */














 /* $recovery_timing_check ::= '$recovery' '(' reference_event ',' data_event ',' timing_check_limit [ ','   [ notify_reg   ]   ] ')' ';' */














 /* $removal_timing_check ::= '$removal' '(' reference_event ',' data_event ',' timing_check_limit [ ','   [ notify_reg   ]   ] ')' ';' */


























 /* $recrem_timing_check ::= '$recrem' '(' reference_event ',' data_event ',' timing_check_limit ',' timing_check_limit [ ','   [ notify_reg   ]   [ ','   [ stamptime_condition   ]   [ ','   [ checktime_condition   ]   [ ','   [ delayed_reference   ]   [ ','   [ delayed_data   ]   ]   ]   ]   ]   ] ')' ';' */














 /* $skew_timing_check ::= '$skew' '(' reference_event ',' data_event ',' timing_check_limit [ ','   [ notify_reg   ]   ] ')' ';' */



















 /* $timeskew_timing_check ::= '$timeskew' '(' reference_event ',' data_event ',' timing_check_limit [ ','   [ notify_reg   ]   [ ','   [ event_based_flag   ]   [ ','   [ remain_active_flag   ]   ]   ]   ] ')' ';' */





















 /* $fullskew_timing_check ::= '$fullskew' '(' reference_event ',' data_event ',' timing_check_limit ',' timing_check_limit [ ','   [ notify_reg   ]   [ ','   [ event_based_flag   ]   [ ','   [ remain_active_flag   ]   ]   ]   ] ')' ';' */












 /* $period_timing_check ::= '$period' '(' controlled_reference_event ',' timing_check_limit [ ','   [ notify_reg   ]   ] ')' ';' */














 /* $width_timing_check ::= '$width' '(' controlled_reference_event ',' timing_check_limit ',' threshold [ ','   [ notify_reg   ]   ] ')' ';' */

















 /* $nochange_timing_check ::= '$nochange' '(' reference_event ',' data_event ',' start_edge_offset ',' end_edge_offset [ ','   [ notify_reg   ]   ] ')' ';' */
/*





A.7.5.2 System timing check command arguments
 */



   /* checktime_condition ::= mintypmax_expression */



   /* controlled_reference_event ::= controlled_timing_check_event */



   /* data_event ::= timing_check_event */









 /* delayed_data ::= terminal_identifier | terminal_identifier [ constant_mintypmax_expression ] */









 /* delayed_reference ::= terminal_identifier | terminal_identifier [ constant_mintypmax_expression ] */



   /* end_edge_offset ::= mintypmax_expression */



   /* event_based_flag ::= constant_expression */



   /* notify_reg ::= variable_identifier */



   /* reference_event ::= timing_check_event */



   /* remain_active_flag ::= constant_mintypmax_expression */



   /* stamptime_condition ::= mintypmax_expression */



   /* start_edge_offset ::= mintypmax_expression */



  /* threshold ::= constant_expression */



   /* timing_check_limit ::= expression */
/*





A.7.5.3 System timing check event definitions
 */






   /* timing_check_event ::= [ timing_check_event_control   ] specify_terminal_descriptor [ '&&&'   timing_check_condition   ] */






   /* controlled_timing_check_event ::= timing_check_event_control specify_terminal_descriptor [ '&&&'   timing_check_condition   ] */










	 /* timing_check_event_control ::= 'posedge' | 'negedge' | edge_control_specifier */





	 /* specify_terminal_descriptor ::= specify_input_terminal_descriptor | specify_output_terminal_descriptor */











 /* edge_control_specifier ::= 'edge' [ edge_descriptor { ','   edge_descriptor   } ] */












 /* edge_descriptor ::= '01' | '10' | 'z_or_x' zero_or_one | zero_or_one 'z_or_x' */







 /* zero_or_one ::= '0' | '1' */











 /* 'z_or_x' ::= x | X | Z | Z */









 /* timing_check_condition ::= scalar_timing_check_condition | '(' scalar_timing_check_condition ')' */



















  /* scalar_timing_check_condition ::= expression | '~' expression | expression '==' scalar_constant | expression '===' scalar_constant | expression '!=' scalar_constant | expression '!==' scalar_constant */























 /* scalar_constant ::= '1'b0' | '1'b1' | '1'B0' | '1'B1' | ''b0' | ''b1' | ''B0' | ''B1' | '1' | '0' */
/*






A.8 Expressions





A.8.1

Concatenations
 */




























 /* concatenation ::= { expression { ','   expression   } } | { struct_member_label ':' expression { ','   struct_member_label   ':'   expression   } } | { array_member_label ':' expression { ','   array_member_label   ':'   expression   } } */




























 /* constant_concatenation ::= { constant_expression { ','   constant_expression   } } | { struct_member_label ':' constant_expression { ','   struct_member_label   ':'   constant_expression   } } | { array_member_label ':' constant_expression { ','   array_member_label   ':'   constant_expression   } } */








	 /* struct_member_label ::= 'default' | type_identifier | variable_identifier */








	/* array_member_label ::= 'default' | type_identifier | constant_expression */







 /* constant_multiple_concatenation ::= { constant_expression constant_concatenation } */









 /* module_path_concatenation ::= { module_path_expression { ','   module_path_expression   } } */







 /* module_path_multiple_concatenation ::= { constant_expression module_path_concatenation } */







 /* multiple_concatenation ::= { constant_expression concatenation } */
/*





A.8.2 Function calls
 */








  /* constant_function_call ::= function_identifier { attribute_instance   } [ '('   list_of_constant_arguments   ')'   ] */







  /* function_call ::= hierarchical_function_identifier { attribute_instance   } [ '('   list_of_arguments   ')'   ] */





















  /* list_of_arguments ::= [ expression   ] { ','   [ expression   ]   } | '.' identifier '(' [ expression   ] ')' { ','   '.'   identifier   '('   [ expression   ]   ')'   } */





















  /* list_of_constant_arguments ::= [ constant_expression   ] { ','   [ constant_expression   ]   } | '.' identifier '(' [ constant_expression   ] ')' { ','   '.'   identifier   '('   [ constant_expression   ]   ')'   } */









  /* system_function_call ::= 'system_function_identifier' [ '('   expression   { ','   expression   }   ')'   ] */
/*





A.8.3

Expressions
 */



   /* base_expression ::= expression */





	     /* inc_or_dec_expression ::= inc_or_dec_operator { attribute_instance   } variable_lvalue | variable_lvalue { attribute_instance   } inc_or_dec_operator */







  /* conditional_expression ::= expression1 '?' { attribute_instance   } expression2 ':' expression3 */



   /* constant_base_expression ::= constant_expression */












	 /* constant_expression ::= constant_primary | unary_operator { attribute_instance   } constant_primary | constant_expression binary_operator { attribute_instance   } constant_expression | constant_expression '?' { attribute_instance   } constant_expression ':' constant_expression | string_literal */









  /* constant_mintypmax_expression ::= constant_expression | constant_expression ':' constant_expression ':' constant_expression */




	 /* constant_param_expression ::= constant_expression */













  /* constant_range_expression ::= constant_expression | msb_constant_expression ':' lsb_constant_expression | constant_base_expression '+:' width_constant_expression | constant_base_expression '-:' width_constant_expression */



   /* dimension_constant_expression ::= constant_expression */



   /* expression1 ::= expression */



   /* expression2 ::= expression */



   /* expression3 ::= expression */















	 /* expression ::= primary | unary_operator { attribute_instance   } primary | inc_or_dec_expression | '(' operator_assignment ')' | expression binary_operator { attribute_instance   } expression | conditional_expression | string_literal | inside_expression */





  /* inside_expression ::= expression 'inside' range_list_or_array */











 /* range_list_or_array ::= variable_identifier | { value_range { ','   value_range   } } */











 /* value_range ::= expression | [ expression ':' expression ] */



   /* lsb_constant_expression ::= constant_expression */









  /* mintypmax_expression ::= expression | expression ':' expression ':' expression */








  /* module_path_conditional_expression ::= module_path_expression '?' { attribute_instance   } module_path_expression ':' module_path_expression */










	 /* module_path_expression ::= module_path_primary | unary_module_path_operator { attribute_instance   } module_path_primary | module_path_expression binary_module_path_operator { attribute_instance   } module_path_expression | module_path_conditional_expression */









  /* module_path_mintypmax_expression ::= module_path_expression | module_path_expression ':' module_path_expression ':' module_path_expression */



   /* msb_constant_expression ::= constant_expression */













  /* range_expression ::= expression | msb_constant_expression ':' lsb_constant_expression | base_expression '+:' width_constant_expression | base_expression '-:' width_constant_expression */



   /* width_constant_expression ::= constant_expression */
/*





A.8.4

Primaries
 */










































 /* constant_primary ::= constant_concatenation | constant_function_call | '(' constant_mintypmax_expression ')' | constant_multiple_concatenation | genvar_identifier | number | parameter_identifier | specparam_identifier | casting_type ''' '(' constant_expression ')' | casting_type ''' constant_concatenation | casting_type ''' constant_multiple_concatenation | time_literal | ''0' | ''1' | ''z' | ''Z' | ''x' | ''X' */















 /* module_path_primary ::= number | identifier | module_path_concatenation | module_path_multiple_concatenation | function_call | system_function_call | constant_function_call | '(' module_path_mintypmax_expression ')' */






































































 /* primary ::= number | implicit_class_handle hierarchical_identifier { [   expression   ]   } [ [   range_expression   ]   ] [ '.'   method_identifier   { attribute_instance   }   [ '('   expression   { ','   expression   }   ')'   ]   ] | concatenation | multiple_concatenation | function_call | system_function_call | constant_function_call | class_identifier '::' { class_identifier   '::'   } identifier | '(' mintypmax_expression ')' | casting_type ''' '(' expression ')' | 'void' ''' '(' function_call ')' | casting_type ''' concatenation | casting_type ''' multiple_concatenation | time_literal | ''0' | ''1' | ''z' | ''Z' | ''x' | ''X' | 'null' */






	 /* time_literal ::= 'unsigned_number' 'time_unit' | fixed_point_number 'time_unit' */

















 /* 'time_unit' ::= S | MS | US | NS | PS | FS | 'step' */








  /* implicit_class_handle ::= [ 'this'   '.'   ] | [ 'super'   '.'   ] */
/*





A.8.5 Expression left-side values
 */



















 /* net_lvalue ::= hierarchical_net_identifier { [   constant_expression   ]   } [ [   constant_range_expression   ]   ] | { net_lvalue { ','   net_lvalue   } } */



















 /* variable_lvalue ::= hierarchical_variable_identifier { [   expression   ]   } [ [   range_expression   ]   ] | { variable_lvalue { ','   variable_lvalue   } } */
/*





A.8.6

Operators
 */


























 /* unary_operator ::= '+' | - | '!' | '~' | '&' | '~&' | | | '~|' | '^' | '~^' | '^~' */



























































 /* binary_operator ::= '+' | - | '*' | '/' | '%' | '==' | '!=' | '===' | '!==' | '=?=' | '!?=' | '&&' | '||' | '**' | '<' | '<=' | '>' | '>=' | '&' | | | '^' | '^~' | '~^' | '>>' | '<<' | '>>>' | '<<<' */







 /* inc_or_dec_operator ::= '++' | '--' */
























 /* unary_module_path_operator ::= '!' | '~' | '&' | '~&' | | | '~|' | '^' | '~^' | '^~' */
























 /* binary_module_path_operator ::= '==' | '!=' | '&&' | '||' | '&' | | | '^' | '^~' | '~^' */
/*





A.8.7

Numbers
 */








	 /* number ::= decimal_number | octal_number | binary_number | hex_number | real_number */

















  /* decimal_number ::= 'unsigned_number' | [ size   ] 'decimal_base' 'unsigned_number' | [ size   ] 'decimal_base' 'x_digit' { '_'   } | [ size   ] 'decimal_base' 'z_digit' { '_'   } */





 /* binary_number ::= [ size   ] 'binary_base' binary_value */





 /* octal_number ::= [ size   ] 'octal_base' octal_value */





 /* hex_number ::= [ size   ] 'hex_base' hex_value */







 /* sign ::= '+' | - */



   /* size ::= non_zero_unsigned_number */






   /* non_zero_unsigned_number ::= non_zero_decimal_digit { '_'   |   decimal_digit   } */








        /* real_number ::= fixed_point_number | 'unsigned_number' [ '.'   'unsigned_number'   ] 'exp' [ sign   ] 'unsigned_number' */






  /* fixed_point_number ::= 'unsigned_number' '.' 'unsigned_number' */







 /* 'exp' ::= E | E */






    /* 'unsigned_number' ::= decimal_digit { '_'   |   decimal_digit   } */






    /* binary_value ::= binary_digit { '_'   |   binary_digit   } */






    /* octal_value ::= octal_digit { '_'   |   octal_digit   } */






    /* hex_value ::= 'hex_digit' { '_'   |   'hex_digit'   } */




















 /* 'decimal_base' ::= ''' [ s   |   S   ] D | ''' [ s   |   S   ] D */




















 /* 'binary_base' ::= ''' [ s   |   S   ] b | ''' [ s   |   S   ] B */




















 /* 'octal_base' ::= ''' [ s   |   S   ] O | ''' [ s   |   S   ] O */




















 /* 'hex_base' ::= ''' [ s   |   S   ] H | ''' [ s   |   S   ] H */





















 /* non_zero_decimal_digit ::= '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' */























 /* decimal_digit ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' */







 /* binary_digit ::= 'x_digit' | 'z_digit' | '0' | '1' */



















 /* octal_digit ::= 'x_digit' | 'z_digit' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' */















































 /* 'hex_digit' ::= 'x_digit' | 'z_digit' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | A | b | C | d | e | f | A | B | C | D | E | F */







 /* 'x_digit' ::= x | X */









 /* 'z_digit' ::= z | Z | '?' */
/*





A.8.8

Strings
 */







 /* string_literal ::= '"' { 'Any_ASCII_Characters'   } '"' */
/*





A.9 General





A.9.1

Attributes
 */









 /* attribute_instance ::= '(*' attr_spec { ','   attr_spec   } '*)' */







	 /* attr_spec ::= attr_name '=' constant_expression | attr_name */



   /* attr_name ::= identifier */
/*





A.9.2 Comments
 */





	 /* comment ::= one_line_comment | block_comment */





   /* one_line_comment ::= '//' comment_text '\n' */







 /* block_comment ::= '/' '*'  comment_text '*' '/'  */



     /* comment_text ::= { 'Any_ASCII_character'   } */
/*





A.9.3 Identifiers
 */



   /* block_identifier ::= identifier */






















   /* 'c_identifier' ::= [ "a" .. "z"   |   "A" .. "Z"   '_'   ] { [ "a" .. "z"   |   "A" .. "Z"   |   "'0'" .. "'9_'"   ]   } */



   /* cell_identifier ::= identifier */



   /* class_identifier ::= identifier */



   /* clocking_identifier ::= identifier */



   /* config_identifier ::= identifier */



   /* constraint_identifier ::= identifier */



   /* const_identifier ::= identifier */



   /* enum_identifier ::= identifier */









  /* escaped_hierarchical_identifier ::= escaped_hierarchical_branch { '.'   simple_hierarchical_branch   |   '.'   escaped_hierarchical_branch   } */





   /* escaped_identifier ::= '\' '{any_ASCII_character_except_white_space}' white_space */



   /* event_identifier ::= identifier */



   /* formal_identifier ::= identifier */



   /* function_identifier ::= identifier */



   /* gate_instance_identifier ::= identifier */



   /* generate_block_identifier ::= identifier */



   /* genvar_identifier ::= identifier */



   /* hierarchical_block_identifier ::= hierarchical_identifier */



   /* hierarchical_event_identifier ::= hierarchical_identifier */



   /* hierarchical_function_identifier ::= hierarchical_identifier */





	 /* hierarchical_identifier ::= simple_hierarchical_identifier | escaped_hierarchical_identifier */



   /* hierarchical_parameter_identifier ::= hierarchical_identifier */



   /* hierarchical_net_identifier ::= hierarchical_identifier */



   /* hierarchical_variable_identifier ::= hierarchical_identifier */



   /* hierarchical_task_identifier ::= hierarchical_identifier */





	 /* identifier ::= 'simple_identifier' | escaped_identifier */



   /* interface_identifier ::= identifier */



   /* interface_instance_identifier ::= identifier */



     /* inout_port_identifier ::= identifier */



   /* input_port_identifier ::= identifier */



   /* instance_identifier ::= identifier */



   /* library_identifier ::= identifier */



   /* method_identifier ::= identifier */



   /* modport_identifier ::= identifier */



   /* module_identifier ::= identifier */



   /* module_instance_identifier ::= identifier */



     /* net_identifier ::= identifier */



   /* output_port_identifier ::= identifier */



   /* parameter_identifier ::= identifier */



   /* port_identifier ::= identifier */



   /* program_identifier ::= identifier */



   /* program_instance_identifier ::= identifier */



   /* property_identifier ::= identifier */



   /* sequence_identifier ::= identifier */



   /* signal_identifier ::= identifier */






   /* simple_hierarchical_identifier ::= simple_hierarchical_branch [ '.'   escaped_identifier   ] */


















   /* 'simple_identifier' ::= [ ""a" .. "ZA"" .. "Z_"   ] { [ """a" .. "zA"" .. "Z0"" .. "'9_$'"   ]   } */



   /* specparam_identifier ::= identifier */






















   /* 'system_function_identifier' ::= '$' [ """a" .. "zA"" .. "Z0"" .. "'9_$'"   ']{'   [ """a" .. "zA"" .. "Z0"" .. "'9_$'"   ]   ] */






















   /* 'system_task_identifier' ::= '$' [ """a" .. "zA"" .. "Z0"" .. "'9_$'"   ']{'   [ """a" .. "zA"" .. "Z0"" .. "'9_$'"   ]   ] */



    /* task_or_function_identifier ::= task_identifier | function_identifier */



   /* task_identifier ::= identifier */



   /* terminal_identifier ::= identifier */



   /* text_macro_identifier ::= 'simple_identifier' */



   /* topmodule_identifier ::= identifier */



      /* type_declaration_identifier ::= type_identifier { unpacked_dimension   } */



   /* type_identifier ::= identifier */



   /* udp_identifier ::= identifier */



   /* udp_instance_identifier ::= identifier */



   /* variable_identifier ::= identifier */
/*





A.9.4 Identifier branches
 */















    /* simple_hierarchical_branch ::= 'simple_identifier' { [   'unsigned_number'   ]   } [ { '.'   'simple_identifier'   { [   'unsigned_number'   ]   }   }   ] */















    /* escaped_hierarchical_branch ::= escaped_identifier { [   'unsigned_number'   ]   } [ { '.'   escaped_identifier   { [   'unsigned_number'   ]   }   }   ] */
/*





A.9.5 White space
 */




 /* white_space ::= 'space' | 'tab' | 'newline' | 'eof' */
/*




NOTES




Embedded spaces are illegal.



A simple_identifier, c_identifier, and arrayed_reference shall start with an alpha or underscore (_) character, shall have at least one character, and shall not have any spaces.



The period (.) in simple_hierarchical_identifier and simple_hierarchical_branch shall not be preceded or followed by white_space.



The period in escaped_hierarchical_identifier and escaped_hierarchical_branch shall be preceded by white_space, but shall not be followed by white_space.



The $ character in a system_function_identifier or system_task_identifier shall not be followed by white_space. A system_function_identifier or system_task_identifier shall not be escaped.



End of file.



The unsigned number or fixed point number in time_literal shall not be followed by a white_space.



void functions, non integer type functions, and functions with a typedef type cannot have a signing declaration.



Open-array (  */




























/* [ ] ')' form shall only be used 'with' dpi_proto_formal implicit_class_handle shall only appear 'within' the scope of a class_declaration 'or' extern_method_declaration '.' In any one declaration, only one of 'protected' 'or' 'local' is allowed, only one of 'rand' 'or' 'randc' is allowed, 'and' 'static' and/or 'virtual' can appear only once. */

%inline library_declaration : LIBRARY arg3 = identifier arg4 = file_path_spec arg5 = library_declaration_5 arg6 = library_declaration_6 SEMICOLON { Globals.grdbg "library_declaration" (SEXTUPLE(LIBRARY, arg3, arg4, arg5, arg6, SEMICOLON);) }
	;

%inline full_path_description : LPAREN arg3 = list_of_path_inputs arg4 = full_path_description_4 P_ASTGT arg6 = list_of_path_outputs RPAREN { Globals.grdbg "full_path_description" (SEXTUPLE(LPAREN, arg3, arg4, P_ASTGT, arg6, RPAREN);) }
	;

%inline modport_declaration : MODPORT arg3 = modport_item arg4 = modport_declaration_4 SEMICOLON { Globals.grdbg "modport_declaration" (QUADRUPLE(MODPORT, arg3, arg4, SEMICOLON);) }
	;

%inline edge_control_specifier : EDGE LBRACK arg4 = edge_descriptor arg5 = edge_control_specifier_5 RBRACK { Globals.grdbg "edge_control_specifier" (QUINTUPLE(EDGE, LBRACK, arg4, arg5, RBRACK);) }
	;

pass_en_switchtype : TRANIF0 { Globals.grdbg "pass_en_switchtype_3" ((TRANIF0);) }
	| TRANIF1 { Globals.grdbg "pass_en_switchtype_5" ((TRANIF1);) }
	| RTRANIF1 { Globals.grdbg "pass_en_switchtype_7" ((RTRANIF1);) }
	| RTRANIF0 { Globals.grdbg "pass_en_switchtype" ((RTRANIF0);) }
	;

%inline dpi_proto_formal : arg2 = data_type arg3 = dpi_proto_formal_3 { Globals.grdbg "dpi_proto_formal" (DOUBLE(arg2, arg3);) }
	;

%inline ncontrol_terminal : arg2 = expression { Globals.grdbg "ncontrol_terminal" ((arg2);) }
	;

%inline simple_hierarchical_identifier : arg2 = simple_hierarchical_branch arg3 = simple_hierarchical_identifier_3 { Globals.grdbg "simple_hierarchical_identifier" (DOUBLE(arg2, arg3);) }
	;

value_range : arg2 = expression { Globals.grdbg "value_range_3" ((arg2);) }
	| LBRACK arg5 = expression COLON arg7 = expression RBRACK { Globals.grdbg "value_range" (QUINTUPLE(LBRACK, arg5, COLON, arg7, RBRACK);) }
	;

%inline const_identifier : arg2 = identifier { Globals.grdbg "const_identifier" ((arg2);) }
	;

pass_switchtype : TRAN { Globals.grdbg "pass_switchtype_3" ((TRAN);) }
	| RTRAN { Globals.grdbg "pass_switchtype" ((RTRAN);) }
	;

%inline octal_value : arg2 = octal_digit arg3 = octal_value_3 { Globals.grdbg "octal_value" (DOUBLE(arg2, arg3);) }
	;

%inline list_of_specparam_assignments : arg2 = specparam_assignment arg3 = list_of_specparam_assignments_3 { Globals.grdbg "list_of_specparam_assignments" (DOUBLE(arg2, arg3);) }
	;

%inline end_edge_offset : arg2 = mintypmax_expression { Globals.grdbg "end_edge_offset" ((arg2);) }
	;

%inline block_identifier : arg2 = identifier { Globals.grdbg "block_identifier" ((arg2);) }
	;

module_root_item : arg3 = module_instantiation { Globals.grdbg "module_root_item_4" ((arg3);) }
	| arg6 = local_parameter_declaration { Globals.grdbg "module_root_item_7" ((arg6);) }
	| arg8 = interface_declaration { Globals.grdbg "module_root_item_9" ((arg8);) }
	| arg10 = program_declaration { Globals.grdbg "module_root_item_11" ((arg10);) }
	| arg12 = class_declaration { Globals.grdbg "module_root_item_13" ((arg12);) }
	| arg14 = module_common_item { Globals.grdbg "module_root_item" ((arg14);) }
	;

%inline tz1_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "tz1_path_delay_expression" ((arg2);) }
	;

%inline width_constant_expression : arg2 = constant_expression { Globals.grdbg "width_constant_expression" ((arg2);) }
	;

%inline list_of_variable_identifiers : arg2 = identifier arg3 = variable_dimension arg4 = list_of_variable_identifiers_4 { Globals.grdbg "list_of_variable_identifiers" (TRIPLE(arg2, arg3, arg4);) }
	;

dpi_import_property : CONTEXT { Globals.grdbg "dpi_import_property_3" ((CONTEXT);) }
	| PURE { Globals.grdbg "dpi_import_property" ((PURE);) }
	;

%inline named_function_proto : arg2 = named_function_proto_2 arg3 = function_data_type arg4 = identifier LPAREN arg6 = list_of_function_proto_formals RPAREN { Globals.grdbg "named_function_proto" (SEXTUPLE(arg2, arg3, arg4, LPAREN, arg6, RPAREN);) }
	;

%inline inst_clause : INSTANCE arg3 = inst_name { Globals.grdbg "inst_clause" (DOUBLE(INSTANCE, arg3);) }
	;

%inline class_declaration : arg3 = class_declaration_3 CLASS arg5 = class_declaration_5 arg6 = identifier arg7 = class_declaration_7 arg8 = class_declaration_8 SEMICOLON arg10 = class_declaration_10 arg11 = class_declaration_11 ENDCLASS arg13 = class_declaration_13 { Globals.grdbg "class_declaration" (UNDECUPLE(arg3, CLASS, arg5, arg6, arg7, arg8, SEMICOLON, arg10, arg11, ENDCLASS, arg13);) }
	;

%inline specparam_identifier : arg2 = identifier { Globals.grdbg "specparam_identifier" ((arg2);) }
	;

%inline generate_block_identifier : arg2 = identifier { Globals.grdbg "generate_block_identifier" ((arg2);) }
	;

list_of_port_declarations : LPAREN arg3 = port_declaration arg4 = list_of_port_declarations_4 RPAREN { Globals.grdbg "list_of_port_declarations_6" (QUADRUPLE(LPAREN, arg3, arg4, RPAREN);) }
	| LPAREN RPAREN { Globals.grdbg "list_of_port_declarations" (DOUBLE(LPAREN, RPAREN);) }
	;

procedural_continuous_assignments : ASSIGN arg3 = variable_assignment { Globals.grdbg "procedural_continuous_assignments_4" (DOUBLE(ASSIGN, arg3);) }
	| DEASSIGN arg6 = variable_lvalue { Globals.grdbg "procedural_continuous_assignments_7" (DOUBLE(DEASSIGN, arg6);) }
	| FORCE arg9 = variable_assignment { Globals.grdbg "procedural_continuous_assignments_10" (DOUBLE(FORCE, arg9);) }
	| FORCE arg12 = net_assignment { Globals.grdbg "procedural_continuous_assignments_13" (DOUBLE(FORCE, arg12);) }
	| RELEASE arg15 = variable_lvalue { Globals.grdbg "procedural_continuous_assignments_16" (DOUBLE(RELEASE, arg15);) }
	| RELEASE arg18 = net_lvalue { Globals.grdbg "procedural_continuous_assignments" (DOUBLE(RELEASE, arg18);) }
	;

clockvar_expression : arg2 = clockvar arg3 = range { Globals.grdbg "clockvar_expression_4" (DOUBLE(arg2, arg3);) }
	| arg5 = clockvar LBRACK arg7 = range_expression RBRACK { Globals.grdbg "clockvar_expression" (QUADRUPLE(arg5, LBRACK, arg7, RBRACK);) }
	;

inc_or_dec_expression : arg2 = inc_or_dec_operator arg4 = variable_lvalue { Globals.grdbg "inc_or_dec_expression_5" (DOUBLE(arg2, arg4);) }
	| arg6 = variable_lvalue arg8 = inc_or_dec_operator { Globals.grdbg "inc_or_dec_expression" (DOUBLE(arg6, arg8);) }
	;

%inline function_blocking_assignment : arg2 = variable_lvalue EQUALS arg4 = expression { Globals.grdbg "function_blocking_assignment" (TRIPLE(arg2, EQUALS, arg4);) }
	;

%inline constant_declaration : CONST arg3 = data_type arg4 = const_assignment SEMICOLON { Globals.grdbg "constant_declaration" (QUADRUPLE(CONST, arg3, arg4, SEMICOLON);) }
	;

%inline actual_arg_expr : arg2 = event_expression { Globals.grdbg "actual_arg_expr" ((arg2);) }
	;

%inline identifier_list : arg2 = identifier arg3 = identifier_list_3 { Globals.grdbg "identifier_list" (DOUBLE(arg2, arg3);) }
	;

%inline config_identifier : arg2 = identifier { Globals.grdbg "config_identifier" ((arg2);) }
	;

%inline module_ansi_header : arg3 = module_keyword arg4 = module_ansi_header_4 arg5 = identifier arg6 = module_ansi_header_6 arg7 = module_ansi_header_7 SEMICOLON { Globals.grdbg "module_ansi_header" (SEXTUPLE(arg3, arg4, arg5, arg6, arg7, SEMICOLON);) }
	;

%inline tx1_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "tx1_path_delay_expression" ((arg2);) }
	;

array_member_label : DEFAULT { Globals.grdbg "array_member_label_3" ((DEFAULT);) }
	| arg4 = identifier { Globals.grdbg "array_member_label_5" ((arg4);) }
	| arg6 = constant_expression { Globals.grdbg "array_member_label" ((arg6);) }
	;

udp_declaration2 : PRIMITIVE arg4 = identifier LPAREN arg6 = udp_port_list RPAREN SEMICOLON arg9 = udp_port_declaration arg10 = udp_declaration2_10 arg11 = udp_body ENDPRIMITIVE { Globals.grdbg "udp_declaration2_13" (DECUPLE(PRIMITIVE, arg4, LPAREN, arg6, RPAREN, SEMICOLON, arg9, arg10, arg11, ENDPRIMITIVE);) }
	| PRIMITIVE arg16 = identifier LPAREN arg18 = udp_declaration_port_list RPAREN SEMICOLON arg21 = udp_body ENDPRIMITIVE { Globals.grdbg "udp_declaration2" (OCTUPLE(PRIMITIVE, arg16, LPAREN, arg18, RPAREN, SEMICOLON, arg21, ENDPRIMITIVE);) }
	;

statement_or_null : arg2 = statement { Globals.grdbg "statement_or_null_3" ((arg2);) }
	| SEMICOLON { Globals.grdbg "statement_or_null" ((SEMICOLON);) }
	;

%inline multi_clock_sequence : arg2 = clocked_sequence arg3 = multi_clock_sequence_3 { Globals.grdbg "multi_clock_sequence" (DOUBLE(arg2, arg3);) }
	;

%inline list_of_genvar_identifiers : arg2 = identifier arg3 = list_of_genvar_identifiers_3 { Globals.grdbg "list_of_genvar_identifiers" (DOUBLE(arg2, arg3);) }
	;

procedural_timing_control : arg2 = delay_control { Globals.grdbg "procedural_timing_control_3" ((arg2);) }
	| arg4 = event_control { Globals.grdbg "procedural_timing_control" ((arg4);) }
	;

%inline non_consecutive_repetition : TOKEN_LBRACK_STAR_EQUALS arg3 = const_or_range_expression RBRACK { Globals.grdbg "non_consecutive_repetition" (TRIPLE(TOKEN_LBRACK_STAR_EQUALS, arg3, RBRACK);) }
	;

concurrent_assertion_item_declaration : arg2 = property_declaration { Globals.grdbg "concurrent_assertion_item_declaration_3" ((arg2);) }
	| arg4 = sequence_declaration { Globals.grdbg "concurrent_assertion_item_declaration" ((arg4);) }
	;

variable_dimension : arg2 = variable_dimension_2 { Globals.grdbg "variable_dimension_3" ((arg2);) }
	| LBRACK RBRACK { Globals.grdbg "variable_dimension_6" (DOUBLE(LBRACK, RBRACK);) }
	| arg7 = associative_dimension { Globals.grdbg "variable_dimension" ((arg7);) }
	;

local_parameter_declaration : LOCALPARAM arg3 = local_parameter_declaration_3 arg4 = local_parameter_declaration_4 arg5 = local_parameter_declaration_5 arg6 = list_of_param_assignments SEMICOLON { Globals.grdbg "local_parameter_declaration_8" (SEXTUPLE(LOCALPARAM, arg3, arg4, arg5, arg6, SEMICOLON);) }
	| LOCALPARAM arg10 = data_type arg11 = list_of_param_assignments SEMICOLON { Globals.grdbg "local_parameter_declaration" (QUADRUPLE(LOCALPARAM, arg10, arg11, SEMICOLON);) }
	;

%inline net_decl_assignment : arg2 = identifier EQUALS arg4 = expression { Globals.grdbg "net_decl_assignment" (TRIPLE(arg2, EQUALS, arg4);) }
	;

%inline generate_interface_loop_statement : FOR LPAREN arg4 = genvar_decl_assignment SEMICOLON arg6 = constant_expression SEMICOLON arg8 = genvar_assignment RPAREN arg10 = generate_interface_named_block { Globals.grdbg "generate_interface_loop_statement" (NONUPLE(FOR, LPAREN, arg4, SEMICOLON, arg6, SEMICOLON, arg8, RPAREN, arg10);) }
	;

dist_item : arg2 = value_range P_COLONEQ arg4 = expression { Globals.grdbg "dist_item_5" (TRIPLE(arg2, P_COLONEQ, arg4);) }
	| arg6 = value_range P_COLONDIV arg8 = expression { Globals.grdbg "dist_item" (TRIPLE(arg6, P_COLONDIV, arg8);) }
	;

%inline concurrent_cover_statement : arg2 = concurrent_cover_statement_2 arg3 = cover_property_statement { Globals.grdbg "concurrent_cover_statement" (DOUBLE(arg2, arg3);) }
	;

%inline list_of_dpi_proto_formals : arg2 = list_of_dpi_proto_formals_2 { Globals.grdbg "list_of_dpi_proto_formals" ((arg2);) }
	;

%inline struct_union_member : arg3 = data_type arg4 = list_of_variable_identifiers_or_assignments SEMICOLON { Globals.grdbg "struct_union_member" (TRIPLE(arg3, arg4, SEMICOLON);) }
	;

method_qualifier : VIRTUAL { Globals.grdbg "method_qualifier_3" ((VIRTUAL);) }
	| arg4 = class_item_qualifier { Globals.grdbg "method_qualifier" ((arg4);) }
	;

%inline t01_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "t01_path_delay_expression" ((arg2);) }
	;

%inline trise_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "trise_path_delay_expression" ((arg2);) }
	;

extern_method_declaration : FUNCTION arg3 = extern_method_declaration_3 arg4 = identifier P_COLONCOLON arg6 = function_body_declaration { Globals.grdbg "extern_method_declaration_7" (QUINTUPLE(FUNCTION, arg3, arg4, P_COLONCOLON, arg6);) }
	| TASK arg9 = extern_method_declaration_9 arg10 = identifier P_COLONCOLON arg12 = task_body_declaration { Globals.grdbg "extern_method_declaration" (QUINTUPLE(TASK, arg9, arg10, P_COLONCOLON, arg12);) }
	;

%inline list_of_path_outputs : arg2 = specify_output_terminal_descriptor arg3 = list_of_path_outputs_3 { Globals.grdbg "list_of_path_outputs" (DOUBLE(arg2, arg3);) }
	;

init_val : TOKEN_FALSE2 { Globals.grdbg "init_val_3" ((TOKEN_FALSE2);) }
	| TOKEN_TRUE2 { Globals.grdbg "init_val_5" ((TOKEN_TRUE2);) }
	| TOKEN_X3 { Globals.grdbg "init_val_7" ((TOKEN_X3);) }
	| TOKEN_X2 { Globals.grdbg "init_val_9" ((TOKEN_X2);) }
	| TOKEN_FALSE1 { Globals.grdbg "init_val_11" ((TOKEN_FALSE1);) }
	| TOKEN_TRUE1 { Globals.grdbg "init_val_13" ((TOKEN_TRUE1);) }
	| TOKEN_X1 { Globals.grdbg "init_val_15" ((TOKEN_X1);) }
	| TOKEN_X0 { Globals.grdbg "init_val_17" ((TOKEN_X0);) }
	| TOKEN_ONE { Globals.grdbg "init_val_19" ((TOKEN_ONE);) }
	| TOKEN_ZERO { Globals.grdbg "init_val" ((TOKEN_ZERO);) }
	;

generate_interface_named_block : BEGIN COLON arg4 = identifier arg5 = generate_interface_named_block_5 END arg7 = generate_interface_named_block_7 { Globals.grdbg "generate_interface_named_block_8" (SEXTUPLE(BEGIN, COLON, arg4, arg5, END, arg7);) }
	| arg9 = identifier COLON arg11 = generate_interface_block { Globals.grdbg "generate_interface_named_block" (TRIPLE(arg9, COLON, arg11);) }
	;

%inline clockvar : arg2 = identifier DOT arg4 = identifier { Globals.grdbg "clockvar" (TRIPLE(arg2, DOT, arg4);) }
	;

%inline task_declaration : TASK arg3 = task_declaration_3 arg4 = task_body_declaration { Globals.grdbg "task_declaration" (TRIPLE(TASK, arg3, arg4);) }
	;

non_generic_port_declaration : arg3 = inout_declaration { Globals.grdbg "non_generic_port_declaration_4" ((arg3);) }
	| arg6 = input_declaration { Globals.grdbg "non_generic_port_declaration_7" ((arg6);) }
	| arg9 = output_declaration { Globals.grdbg "non_generic_port_declaration_10" ((arg9);) }
	| arg12 = ref_declaration { Globals.grdbg "non_generic_port_declaration_13" ((arg12);) }
	| arg15 = interface_port_declaration { Globals.grdbg "non_generic_port_declaration" ((arg15);) }
	;

%inline clocking_decl : arg2 = clocking_decl_2 CLOCKING arg4 = clocking_decl_4 arg5 = clocking_event SEMICOLON arg7 = clocking_decl_7 ENDCLOCKING { Globals.grdbg "clocking_decl" (SEPTUPLE(arg2, CLOCKING, arg4, arg5, SEMICOLON, arg7, ENDCLOCKING);) }
	;

dpi_dimension : arg2 = variable_dimension { Globals.grdbg "dpi_dimension_3" ((arg2);) }
	| arg4 = dpi_dimension_4 { Globals.grdbg "dpi_dimension" ((arg4);) }
	;

%inline parameter_port_list : HASH LPAREN arg4 = parameter_declaration arg5 = parameter_port_list_5 RPAREN { Globals.grdbg "parameter_port_list" (QUINTUPLE(HASH, LPAREN, arg4, arg5, RPAREN);) }
	;

%inline ff_construct : ALWAYS_FF arg3 = statement { Globals.grdbg "ff_construct" (DOUBLE(ALWAYS_FF, arg3);) }
	;

%inline clocking_decl_assign : arg2 = identifier arg3 = clocking_decl_assign_3 { Globals.grdbg "clocking_decl_assign" (DOUBLE(arg2, arg3);) }
	;

%inline source_text : arg2 = source_text_2 arg3 = source_text_3 { Globals.grdbg "source_text" (DOUBLE(arg2, arg3);) }
	;

%inline multiple_concatenation : LCURLY arg3 = constant_expression arg4 = concatenation RCURLY { Globals.grdbg "multiple_concatenation" (QUADRUPLE(LCURLY, arg3, arg4, RCURLY);) }
	;

%inline genvar_decl_assignment : arg2 = genvar_decl_assignment_2 arg3 = identifier EQUALS arg5 = constant_expression { Globals.grdbg "genvar_decl_assignment" (QUADRUPLE(arg2, arg3, EQUALS, arg5);) }
	;

path_declaration : arg2 = simple_path_declaration SEMICOLON { Globals.grdbg "path_declaration_4" (DOUBLE(arg2, SEMICOLON);) }
	| arg5 = edge_sensitive_path_declaration SEMICOLON { Globals.grdbg "path_declaration_7" (DOUBLE(arg5, SEMICOLON);) }
	| arg8 = state_dependent_path_declaration SEMICOLON { Globals.grdbg "path_declaration" (DOUBLE(arg8, SEMICOLON);) }
	;

%inline extern_constraint_declaration : arg2 = extern_constraint_declaration_2 CONSTRAINT arg4 = identifier P_COLONCOLON arg6 = identifier LCURLY arg8 = extern_constraint_declaration_8 RCURLY { Globals.grdbg "extern_constraint_declaration" (OCTUPLE(arg2, CONSTRAINT, arg4, P_COLONCOLON, arg6, LCURLY, arg8, RCURLY);) }
	;

%inline gate_instance_identifier : arg2 = identifier { Globals.grdbg "gate_instance_identifier" ((arg2);) }
	;

class_method : arg2 = class_method_2 arg3 = task_declaration { Globals.grdbg "class_method_4" (DOUBLE(arg2, arg3);) }
	| arg5 = class_method_5 arg6 = function_declaration { Globals.grdbg "class_method_7" (DOUBLE(arg5, arg6);) }
	| EXTERN arg9 = class_method_9 arg10 = method_prototype { Globals.grdbg "class_method" (TRIPLE(EXTERN, arg9, arg10);) }
	;

%inline module_instance_identifier : arg2 = identifier { Globals.grdbg "module_instance_identifier" ((arg2);) }
	;

%inline generate_interface_block : BEGIN arg3 = generate_interface_block_3 arg4 = generate_interface_block_4 END arg6 = generate_interface_block_6 { Globals.grdbg "generate_interface_block" (QUINTUPLE(BEGIN, arg3, arg4, END, arg6);) }
	;

function_case_item : arg2 = expression arg3 = function_case_item_3 COLON arg5 = function_statement_or_null { Globals.grdbg "function_case_item_6" (QUADRUPLE(arg2, arg3, COLON, arg5);) }
	| DEFAULT arg8 = function_case_item_8 arg9 = function_statement_or_null { Globals.grdbg "function_case_item" (TRIPLE(DEFAULT, arg8, arg9);) }
	;

port_declaration : arg2 = non_generic_port_declaration { Globals.grdbg "port_declaration_3" ((arg2);) }
	| arg5 = generic_interface_port_declaration { Globals.grdbg "port_declaration" ((arg5);) }
	;

bind_directive : BIND arg3 = identifier arg4 = bind_instantiation SEMICOLON { Globals.grdbg "bind_directive_6" (QUADRUPLE(BIND, arg3, arg4, SEMICOLON);) }
	| BIND arg8 = name_of_instance arg9 = bind_instantiation SEMICOLON { Globals.grdbg "bind_directive" (QUADRUPLE(BIND, arg8, arg9, SEMICOLON);) }
	;

cover_property_statement : COVER PROPERTY LPAREN arg5 = property_spec RPAREN arg7 = statement_or_null { Globals.grdbg "cover_property_statement_8" (SEXTUPLE(COVER, PROPERTY, LPAREN, arg5, RPAREN, arg7);) }
	| COVER PROPERTY LPAREN arg12 = property_instance RPAREN arg14 = statement_or_null { Globals.grdbg "cover_property_statement" (SEXTUPLE(COVER, PROPERTY, LPAREN, arg12, RPAREN, arg14);) }
	;

%inline list_of_net_identifiers : arg2 = identifier arg3 = list_of_net_identifiers_3 arg4 = list_of_net_identifiers_4 { Globals.grdbg "list_of_net_identifiers" (TRIPLE(arg2, arg3, arg4);) }
	;

constraint_expression : arg2 = expression SEMICOLON { Globals.grdbg "constraint_expression_4" (DOUBLE(arg2, SEMICOLON);) }
	| arg5 = expression P_EQGT arg7 = constraint_set { Globals.grdbg "constraint_expression_8" (TRIPLE(arg5, P_EQGT, arg7);) }
	| IF LPAREN arg11 = expression RPAREN arg13 = constraint_set arg14 = constraint_expression_14 { Globals.grdbg "constraint_expression" (SEXTUPLE(IF, LPAREN, arg11, RPAREN, arg13, arg14);) }
	;

%inline file_path_spec : FILE_PATH { Globals.grdbg "file_path_spec" ((FILE_PATH);) }
	;

edge_identifier : POSEDGE { Globals.grdbg "edge_identifier_3" ((POSEDGE);) }
	| NEGEDGE { Globals.grdbg "edge_identifier" ((NEGEDGE);) }
	;

sign : PLUS { Globals.grdbg "sign_3" ((PLUS);) }
	| MINUS { Globals.grdbg "sign" ((MINUS);) }
	;

%inline defparam_assignment : arg2 = hierarchical_identifier EQUALS arg4 = constant_expression { Globals.grdbg "defparam_assignment" (TRIPLE(arg2, EQUALS, arg4);) }
	;

%inline always_construct : ALWAYS arg3 = statement { Globals.grdbg "always_construct" (DOUBLE(ALWAYS, arg3);) }
	;

input_declaration : INPUT arg3 = input_declaration_3 arg4 = list_of_port_identifiers { Globals.grdbg "input_declaration_5" (TRIPLE(INPUT, arg3, arg4);) }
	| INPUT arg7 = data_type arg8 = list_of_variable_identifiers { Globals.grdbg "input_declaration" (TRIPLE(INPUT, arg7, arg8);) }
	;

description : arg2 = module_declaration { Globals.grdbg "description_3" ((arg2);) }
	| arg4 = udp_declaration { Globals.grdbg "description_5" ((arg4);) }
	| arg6 = module_root_item { Globals.grdbg "description_7" ((arg6);) }
	| arg8 = statement_or_null { Globals.grdbg "description" ((arg8);) }
	;

simple_type : arg2 = integer_type { Globals.grdbg "simple_type_3" ((arg2);) }
	| arg4 = non_integer_type { Globals.grdbg "simple_type_5" ((arg4);) }
	| arg6 = identifier { Globals.grdbg "simple_type" ((arg6);) }
	;

%inline instance_identifier : arg2 = identifier { Globals.grdbg "instance_identifier" ((arg2);) }
	;

concurrent_assertion_item : arg2 = concurrent_assert_statement { Globals.grdbg "concurrent_assertion_item_3" ((arg2);) }
	| arg4 = concurrent_cover_statement { Globals.grdbg "concurrent_assertion_item_5" ((arg4);) }
	| arg6 = concurrent_assertion_item_declaration { Globals.grdbg "concurrent_assertion_item" ((arg6);) }
	;

constant_primary : arg2 = constant_concatenation { Globals.grdbg "constant_primary_3" ((arg2);) }
	| arg4 = constant_function_call { Globals.grdbg "constant_primary_5" ((arg4);) }
	| LPAREN arg7 = constant_mintypmax_expression RPAREN { Globals.grdbg "constant_primary_9" (TRIPLE(LPAREN, arg7, RPAREN);) }
	| arg10 = constant_multiple_concatenation { Globals.grdbg "constant_primary_11" ((arg10);) }
	| arg12 = identifier { Globals.grdbg "constant_primary_13" ((arg12);) }
	| arg14 = number { Globals.grdbg "constant_primary_15" ((arg14);) }
	| arg16 = identifier { Globals.grdbg "constant_primary_17" ((arg16);) }
	| arg18 = identifier { Globals.grdbg "constant_primary_19" ((arg18);) }
	| arg20 = casting_type SQUOTE LPAREN arg23 = constant_expression RPAREN { Globals.grdbg "constant_primary_25" (QUINTUPLE(arg20, SQUOTE, LPAREN, arg23, RPAREN);) }
	| arg26 = casting_type SQUOTE arg28 = constant_concatenation { Globals.grdbg "constant_primary_29" (TRIPLE(arg26, SQUOTE, arg28);) }
	| arg30 = casting_type SQUOTE arg32 = constant_multiple_concatenation { Globals.grdbg "constant_primary_33" (TRIPLE(arg30, SQUOTE, arg32);) }
	| arg34 = time_literal { Globals.grdbg "constant_primary_35" ((arg34);) }
	| TOKEN_QUOTE_FALSE { Globals.grdbg "constant_primary_37" ((TOKEN_QUOTE_FALSE);) }
	| TOKEN_QUOTE_TRUE { Globals.grdbg "constant_primary_39" ((TOKEN_QUOTE_TRUE);) }
	| TOKEN_QUOTE_Z2 { Globals.grdbg "constant_primary_41" ((TOKEN_QUOTE_Z2);) }
	| TOKEN_QUOTE_Z1 { Globals.grdbg "constant_primary_43" ((TOKEN_QUOTE_Z1);) }
	| TOKEN_QUOTE_X2 { Globals.grdbg "constant_primary_45" ((TOKEN_QUOTE_X2);) }
	| TOKEN_QUOTE_X1 { Globals.grdbg "constant_primary" ((TOKEN_QUOTE_X1);) }
	;

%inline comment_text : arg2 = comment_text_2 { Globals.grdbg "comment_text" ((arg2);) }
	;

%inline path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "path_delay_expression" ((arg2);) }
	;

%inline program_nonansi_header : PROGRAM arg4 = program_nonansi_header_4 arg5 = identifier arg6 = program_nonansi_header_6 arg7 = list_of_ports SEMICOLON { Globals.grdbg "program_nonansi_header" (SEXTUPLE(PROGRAM, arg4, arg5, arg6, arg7, SEMICOLON);) }
	;

%inline list_of_variable_assignments : arg2 = variable_assignment arg3 = list_of_variable_assignments_3 { Globals.grdbg "list_of_variable_assignments" (DOUBLE(arg2, arg3);) }
	;

variable_assignment : arg2 = operator_assignment { Globals.grdbg "variable_assignment_3" ((arg2);) }
	| arg4 = inc_or_dec_expression { Globals.grdbg "variable_assignment" ((arg4);) }
	;

%inline edge_input_list : arg2 = edge_input_list_2 arg3 = edge_indicator arg4 = edge_input_list_4 { Globals.grdbg "edge_input_list" (TRIPLE(arg2, arg3, arg4);) }
	;

enable_gatetype : BUFIF0 { Globals.grdbg "enable_gatetype_3" ((BUFIF0);) }
	| BUFIF1 { Globals.grdbg "enable_gatetype_5" ((BUFIF1);) }
	| NOTIF0 { Globals.grdbg "enable_gatetype_7" ((NOTIF0);) }
	| NOTIF1 { Globals.grdbg "enable_gatetype" ((NOTIF1);) }
	;

%inline list_of_variable_decl_assignments : arg2 = variable_decl_assignment arg3 = list_of_variable_decl_assignments_3 { Globals.grdbg "list_of_variable_decl_assignments" (DOUBLE(arg2, arg3);) }
	;

%inline list_of_variable_port_identifiers : arg2 = identifier arg3 = variable_dimension arg4 = list_of_variable_port_identifiers_4 arg5 = list_of_variable_port_identifiers_5 { Globals.grdbg "list_of_variable_port_identifiers" (QUADRUPLE(arg2, arg3, arg4, arg5);) }
	;

%inline pcontrol_terminal : arg2 = expression { Globals.grdbg "pcontrol_terminal" ((arg2);) }
	;

%inline property_instance : arg2 = identifier arg3 = property_instance_3 { Globals.grdbg "property_instance" (DOUBLE(arg2, arg3);) }
	;

%inline constraint_prototype : arg2 = constraint_prototype_2 CONSTRAINT arg4 = identifier { Globals.grdbg "constraint_prototype" (TRIPLE(arg2, CONSTRAINT, arg4);) }
	;

%inline module_path_conditional_expression : arg2 = module_path_expression QUERY arg5 = module_path_expression COLON arg7 = module_path_expression { Globals.grdbg "module_path_conditional_expression" (QUINTUPLE(arg2, QUERY, arg5, COLON, arg7);) }
	;

case_item : arg2 = expression arg3 = case_item_3 COLON arg5 = statement_or_null { Globals.grdbg "case_item_6" (QUADRUPLE(arg2, arg3, COLON, arg5);) }
	| DEFAULT arg8 = case_item_8 arg9 = statement_or_null { Globals.grdbg "case_item" (TRIPLE(DEFAULT, arg8, arg9);) }
	;

output_identifier : arg2 = identifier { Globals.grdbg "output_identifier_3" ((arg2);) }
	| arg4 = identifier { Globals.grdbg "output_identifier" ((arg4);) }
	;

%inline tzx_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "tzx_path_delay_expression" ((arg2);) }
	;

disable_statement : DISABLE arg3 = hierarchical_identifier SEMICOLON { Globals.grdbg "disable_statement_5" (TRIPLE(DISABLE, arg3, SEMICOLON);) }
	| DISABLE arg7 = hierarchical_identifier SEMICOLON { Globals.grdbg "disable_statement_9" (TRIPLE(DISABLE, arg7, SEMICOLON);) }
	| DISABLE FORK SEMICOLON { Globals.grdbg "disable_statement" (TRIPLE(DISABLE, FORK, SEMICOLON);) }
	;

%inline modport_hierarchical_ports_declaration : arg2 = identifier arg3 = modport_hierarchical_ports_declaration_3 DOT arg5 = identifier { Globals.grdbg "modport_hierarchical_ports_declaration" (QUADRUPLE(arg2, arg3, DOT, arg5);) }
	;

mos_switchtype : NMOS { Globals.grdbg "mos_switchtype_3" ((NMOS);) }
	| PMOS { Globals.grdbg "mos_switchtype_5" ((PMOS);) }
	| RNMOS { Globals.grdbg "mos_switchtype_7" ((RNMOS);) }
	| RPMOS { Globals.grdbg "mos_switchtype" ((RPMOS);) }
	;

%inline parallel_path_description : LPAREN arg3 = specify_input_terminal_descriptor arg4 = parallel_path_description_4 P_EQGT arg6 = specify_output_terminal_descriptor RPAREN { Globals.grdbg "parallel_path_description" (SEXTUPLE(LPAREN, arg3, arg4, P_EQGT, arg6, RPAREN);) }
	;

%inline parameter_override : DEFPARAM arg3 = list_of_defparam_assignments SEMICOLON { Globals.grdbg "parameter_override" (TRIPLE(DEFPARAM, arg3, SEMICOLON);) }
	;

function_item_declaration : arg2 = block_item_declaration { Globals.grdbg "function_item_declaration_3" ((arg2);) }
	| arg5 = tf_input_declaration SEMICOLON { Globals.grdbg "function_item_declaration_7" (DOUBLE(arg5, SEMICOLON);) }
	| arg9 = tf_output_declaration SEMICOLON { Globals.grdbg "function_item_declaration_11" (DOUBLE(arg9, SEMICOLON);) }
	| arg13 = tf_inout_declaration SEMICOLON { Globals.grdbg "function_item_declaration_15" (DOUBLE(arg13, SEMICOLON);) }
	| arg17 = tf_ref_declaration SEMICOLON { Globals.grdbg "function_item_declaration" (DOUBLE(arg17, SEMICOLON);) }
	;

%inline d_width_timing_check : D_WIDTH LPAREN arg4 = controlled_timing_check_event COMMA arg6 = expression COMMA arg8 = constant_expression arg9 = d_width_timing_check_9 RPAREN SEMICOLON { Globals.grdbg "d_width_timing_check" (DECUPLE(D_WIDTH, LPAREN, arg4, COMMA, arg6, COMMA, arg8, arg9, RPAREN, SEMICOLON);) }
	;

cycle_delay_const_range_expression : arg2 = constant_expression COLON arg4 = constant_expression { Globals.grdbg "cycle_delay_const_range_expression_5" (TRIPLE(arg2, COLON, arg4);) }
	| arg6 = constant_expression COLON DOLLAR { Globals.grdbg "cycle_delay_const_range_expression" (TRIPLE(arg6, COLON, DOLLAR);) }
	;

%inline function_seq_block : BEGIN arg3 = function_seq_block_3 arg4 = function_seq_block_4 END arg6 = function_seq_block_6 { Globals.grdbg "function_seq_block" (QUINTUPLE(BEGIN, arg3, arg4, END, arg6);) }
	;

block_variable_declaration : arg2 = block_variable_declaration_2 arg3 = data_type arg4 = list_of_variable_identifiers SEMICOLON { Globals.grdbg "block_variable_declaration_6" (QUADRUPLE(arg2, arg3, arg4, SEMICOLON);) }
	| arg7 = lifetime arg8 = data_type arg9 = list_of_variable_decl_assignments SEMICOLON { Globals.grdbg "block_variable_declaration" (QUADRUPLE(arg7, arg8, arg9, SEMICOLON);) }
	;

%inline cycle_delay : P_POUNDPOUND arg3 = expression { Globals.grdbg "cycle_delay" (DOUBLE(P_POUNDPOUND, arg3);) }
	;

%inline text_macro_identifier : arg2 = SIMPLE_IDENTIFIER { Globals.grdbg "text_macro_identifier" ((SIMPLE_IDENTIFIER arg2);) }
	;

tf_inout_declaration : INOUT arg3 = tf_inout_declaration_3 arg4 = tf_inout_declaration_4 arg5 = list_of_tf_port_identifiers { Globals.grdbg "tf_inout_declaration_6" (QUADRUPLE(INOUT, arg3, arg4, arg5);) }
	| INOUT arg8 = tf_data_type arg9 = list_of_tf_variable_identifiers { Globals.grdbg "tf_inout_declaration" (TRIPLE(INOUT, arg8, arg9);) }
	;

%inline generate_interface_case_statement : CASE LPAREN arg4 = constant_expression RPAREN arg6 = genvar_interface_case_item arg7 = generate_interface_case_statement_7 ENDCASE { Globals.grdbg "generate_interface_case_statement" (SEPTUPLE(CASE, LPAREN, arg4, RPAREN, arg6, arg7, ENDCASE);) }
	;

%inline generated_interface_instantiation : GENERATE arg3 = generated_interface_instantiation_3 ENDGENERATE { Globals.grdbg "generated_interface_instantiation" (TRIPLE(GENERATE, arg3, ENDGENERATE);) }
	;

actual_arg_list : LPAREN arg3 = event_expression arg4 = actual_arg_list_4 RPAREN { Globals.grdbg "actual_arg_list_6" (QUADRUPLE(LPAREN, arg3, arg4, RPAREN);) }
	| LPAREN DOT arg9 = identifier LPAREN arg11 = event_expression RPAREN arg13 = actual_arg_list_13 RPAREN { Globals.grdbg "actual_arg_list" (OCTUPLE(LPAREN, DOT, arg9, LPAREN, arg11, RPAREN, arg13, RPAREN);) }
	;

%inline sequence_abbrev : arg2 = consecutive_repetition { Globals.grdbg "sequence_abbrev" ((arg2);) }
	;

%inline list_of_udp_port_identifiers : arg2 = identifier arg3 = list_of_udp_port_identifiers_3 { Globals.grdbg "list_of_udp_port_identifiers" (DOUBLE(arg2, arg3);) }
	;

%inline n_output_gate_instance : arg2 = n_output_gate_instance_2 LPAREN arg4 = net_lvalue arg5 = n_output_gate_instance_5 COMMA arg7 = expression RPAREN { Globals.grdbg "n_output_gate_instance" (SEPTUPLE(arg2, LPAREN, arg4, arg5, COMMA, arg7, RPAREN);) }
	;

%inline interface_instance_identifier : arg2 = identifier { Globals.grdbg "interface_instance_identifier" ((arg2);) }
	;

task_body_declaration : arg2 = task_body_declaration_2 arg3 = identifier SEMICOLON arg5 = task_body_declaration_5 arg6 = task_body_declaration_6 ENDTASK arg8 = task_body_declaration_8 { Globals.grdbg "task_body_declaration_9" (SEPTUPLE(arg2, arg3, SEMICOLON, arg5, arg6, ENDTASK, arg8);) }
	| arg10 = task_body_declaration_10 arg11 = identifier LPAREN arg13 = task_port_list RPAREN SEMICOLON arg16 = task_body_declaration_16 arg17 = task_body_declaration_17 ENDTASK arg19 = task_body_declaration_19 { Globals.grdbg "task_body_declaration" (DECUPLE(arg10, arg11, LPAREN, arg13, RPAREN, SEMICOLON, arg16, arg17, ENDTASK, arg19);) }
	;

%inline reference_event : arg2 = timing_check_event { Globals.grdbg "reference_event" ((arg2);) }
	;

statement_item : arg3 = blocking_assignment SEMICOLON { Globals.grdbg "statement_item_5" (DOUBLE(arg3, SEMICOLON);) }
	| arg7 = nonblocking_assignment SEMICOLON { Globals.grdbg "statement_item_9" (DOUBLE(arg7, SEMICOLON);) }
	| arg11 = procedural_continuous_assignments SEMICOLON { Globals.grdbg "statement_item_13" (DOUBLE(arg11, SEMICOLON);) }
	| arg15 = case_statement { Globals.grdbg "statement_item_16" ((arg15);) }
	| arg18 = conditional_statement { Globals.grdbg "statement_item_19" ((arg18);) }
	| arg21 = inc_or_dec_expression SEMICOLON { Globals.grdbg "statement_item_23" (DOUBLE(arg21, SEMICOLON);) }
	| arg25 = function_call SEMICOLON { Globals.grdbg "statement_item_27" (DOUBLE(arg25, SEMICOLON);) }
	| arg29 = disable_statement { Globals.grdbg "statement_item_30" ((arg29);) }
	| arg32 = event_trigger { Globals.grdbg "statement_item_33" ((arg32);) }
	| arg35 = loop_statement { Globals.grdbg "statement_item_36" ((arg35);) }
	| arg38 = jump_statement { Globals.grdbg "statement_item_39" ((arg38);) }
	| arg41 = par_block { Globals.grdbg "statement_item_42" ((arg41);) }
	| arg44 = procedural_timing_control_statement { Globals.grdbg "statement_item_45" ((arg44);) }
	| arg47 = seq_block { Globals.grdbg "statement_item_48" ((arg47);) }
	| arg50 = system_task_enable { Globals.grdbg "statement_item_51" ((arg50);) }
	| arg53 = task_enable { Globals.grdbg "statement_item_54" ((arg53);) }
	| arg56 = wait_statement { Globals.grdbg "statement_item_57" ((arg56);) }
	| arg59 = procedural_assertion_item { Globals.grdbg "statement_item_60" ((arg59);) }
	| arg62 = clocking_drive { Globals.grdbg "statement_item" ((arg62);) }
	;

%inline t1x_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "t1x_path_delay_expression" ((arg2);) }
	;

ordered_parameter_assignment : arg2 = expression { Globals.grdbg "ordered_parameter_assignment_3" ((arg2);) }
	| arg4 = data_type { Globals.grdbg "ordered_parameter_assignment" ((arg4);) }
	;

zero_or_one : TOKEN_ZERO { Globals.grdbg "zero_or_one_3" ((TOKEN_ZERO);) }
	| TOKEN_ONE { Globals.grdbg "zero_or_one" ((TOKEN_ONE);) }
	;

time_literal : arg2 = UNSIGNED_NUMBER arg3 = TIME_UNIT { Globals.grdbg "time_literal_4" (DOUBLE(UNSIGNED_NUMBER arg2, TIME_UNIT arg3);) }
	| arg5 = fixed_point_number arg6 = TIME_UNIT { Globals.grdbg "time_literal" (DOUBLE(arg5, TIME_UNIT arg6);) }
	;

%inline terminal_identifier : arg2 = identifier { Globals.grdbg "terminal_identifier" ((arg2);) }
	;

%inline sequence_instance : arg2 = identifier arg3 = sequence_instance_3 { Globals.grdbg "sequence_instance" (DOUBLE(arg2, arg3);) }
	;

%inline t0x_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "t0x_path_delay_expression" ((arg2);) }
	;

%inline use_clause : USE arg3 = use_clause_3 arg4 = identifier arg5 = use_clause_5 { Globals.grdbg "use_clause" (QUADRUPLE(USE, arg3, arg4, arg5);) }
	;

%inline property_formal_list : LPAREN arg3 = formal_list_item arg4 = property_formal_list_4 RPAREN { Globals.grdbg "property_formal_list" (QUADRUPLE(LPAREN, arg3, arg4, RPAREN);) }
	;

wait_statement : WAIT LPAREN arg4 = expression RPAREN arg6 = statement_or_null { Globals.grdbg "wait_statement_7" (QUINTUPLE(WAIT, LPAREN, arg4, RPAREN, arg6);) }
	| WAIT FORK SEMICOLON { Globals.grdbg "wait_statement_11" (TRIPLE(WAIT, FORK, SEMICOLON);) }
	| WAIT_ORDER LPAREN arg14 = hierarchical_identifier arg15 = wait_statement_15 RPAREN arg17 = action_block { Globals.grdbg "wait_statement" (SEXTUPLE(WAIT_ORDER, LPAREN, arg14, arg15, RPAREN, arg17);) }
	;

%inline seq_block : BEGIN arg3 = seq_block_3 arg4 = seq_block_4 arg5 = seq_block_5 END arg7 = seq_block_7 { Globals.grdbg "seq_block" (SEXTUPLE(BEGIN, arg3, arg4, arg5, END, arg7);) }
	;

function_statement_item : arg3 = function_blocking_assignment SEMICOLON { Globals.grdbg "function_statement_item_5" (DOUBLE(arg3, SEMICOLON);) }
	| arg7 = function_case_statement { Globals.grdbg "function_statement_item_8" ((arg7);) }
	| arg10 = function_conditional_statement { Globals.grdbg "function_statement_item_11" ((arg10);) }
	| arg13 = inc_or_dec_expression SEMICOLON { Globals.grdbg "function_statement_item_15" (DOUBLE(arg13, SEMICOLON);) }
	| arg17 = function_call SEMICOLON { Globals.grdbg "function_statement_item_19" (DOUBLE(arg17, SEMICOLON);) }
	| arg21 = function_loop_statement { Globals.grdbg "function_statement_item_22" ((arg21);) }
	| arg24 = jump_statement { Globals.grdbg "function_statement_item_25" ((arg24);) }
	| arg27 = function_seq_block { Globals.grdbg "function_statement_item_28" ((arg27);) }
	| arg30 = disable_statement { Globals.grdbg "function_statement_item_31" ((arg30);) }
	| arg33 = system_task_enable { Globals.grdbg "function_statement_item" ((arg33);) }
	;

event_control : AT arg3 = identifier { Globals.grdbg "event_control_4" (DOUBLE(AT, arg3);) }
	| AT LPAREN arg7 = event_expression RPAREN { Globals.grdbg "event_control_9" (QUADRUPLE(AT, LPAREN, arg7, RPAREN);) }
	| AT_STAR { Globals.grdbg "event_control_11" ((AT_STAR);) }
	| AT TOKEN_LPAREN_STAR_RPAREN { Globals.grdbg "event_control" (DOUBLE(AT, TOKEN_LPAREN_STAR_RPAREN);) }
	;

%inline sequential_entry : arg2 = seq_input_list COLON arg4 = current_state COLON arg6 = next_state SEMICOLON { Globals.grdbg "sequential_entry" (SEXTUPLE(arg2, COLON, arg4, COLON, arg6, SEMICOLON);) }
	;

seq_input_list : arg2 = level_input_list { Globals.grdbg "seq_input_list_3" ((arg2);) }
	| arg4 = edge_input_list { Globals.grdbg "seq_input_list" ((arg4);) }
	;

%inline constraint_declaration : arg2 = constraint_declaration_2 CONSTRAINT arg4 = identifier LCURLY arg6 = constraint_declaration_6 RCURLY { Globals.grdbg "constraint_declaration" (SEXTUPLE(arg2, CONSTRAINT, arg4, LCURLY, arg6, RCURLY);) }
	;

%inline full_edge_sensitive_path_description : LPAREN arg3 = full_edge_sensitive_path_description_3 arg4 = list_of_path_inputs P_ASTGT arg6 = list_of_path_outputs arg7 = full_edge_sensitive_path_description_7 COLON arg9 = expression RPAREN { Globals.grdbg "full_edge_sensitive_path_description" (NONUPLE(LPAREN, arg3, arg4, P_ASTGT, arg6, arg7, COLON, arg9, RPAREN);) }
	;

%inline program_ansi_header : PROGRAM arg4 = program_ansi_header_4 arg5 = identifier arg6 = program_ansi_header_6 arg7 = program_ansi_header_7 SEMICOLON { Globals.grdbg "program_ansi_header" (SEXTUPLE(PROGRAM, arg4, arg5, arg6, arg7, SEMICOLON);) }
	;

edge_sensitive_path_declaration : arg2 = parallel_edge_sensitive_path_description EQUALS arg4 = path_delay_value { Globals.grdbg "edge_sensitive_path_declaration_5" (TRIPLE(arg2, EQUALS, arg4);) }
	| arg6 = full_edge_sensitive_path_description EQUALS arg8 = path_delay_value { Globals.grdbg "edge_sensitive_path_declaration" (TRIPLE(arg6, EQUALS, arg8);) }
	;

%inline list_of_net_assignments : arg2 = net_assignment arg3 = list_of_net_assignments_3 { Globals.grdbg "list_of_net_assignments" (DOUBLE(arg2, arg3);) }
	;

constant_range_expression : arg2 = constant_expression { Globals.grdbg "constant_range_expression_3" ((arg2);) }
	| arg4 = constant_expression COLON arg6 = constant_expression { Globals.grdbg "constant_range_expression_7" (TRIPLE(arg4, COLON, arg6);) }
	| arg8 = constant_expression P_PLUSCOLON arg10 = constant_expression { Globals.grdbg "constant_range_expression_11" (TRIPLE(arg8, P_PLUSCOLON, arg10);) }
	| arg12 = constant_expression P_MINUSCOLON arg14 = constant_expression { Globals.grdbg "constant_range_expression" (TRIPLE(arg12, P_MINUSCOLON, arg14);) }
	;

pullup_strength : LPAREN arg3 = strength0 COMMA arg5 = strength1 RPAREN { Globals.grdbg "pullup_strength_7" (QUINTUPLE(LPAREN, arg3, COMMA, arg5, RPAREN);) }
	| LPAREN arg9 = strength1 COMMA arg11 = strength0 RPAREN { Globals.grdbg "pullup_strength_13" (QUINTUPLE(LPAREN, arg9, COMMA, arg11, RPAREN);) }
	| LPAREN arg15 = strength1 RPAREN { Globals.grdbg "pullup_strength" (TRIPLE(LPAREN, arg15, RPAREN);) }
	;

%inline tf_ref_declaration : arg2 = tf_ref_declaration_2 REF arg4 = tf_data_type arg5 = list_of_tf_variable_identifiers { Globals.grdbg "tf_ref_declaration" (QUADRUPLE(arg2, REF, arg4, arg5);) }
	;

%inline escaped_hierarchical_branch : arg2 = escaped_identifier arg3 = escaped_hierarchical_branch_3 arg4 = escaped_hierarchical_branch_4 { Globals.grdbg "escaped_hierarchical_branch" (TRIPLE(arg2, arg3, arg4);) }
	;

%inline system_task_enable : arg2 = SYSTEM_TASK_IDENTIFIER arg3 = system_task_enable_3 SEMICOLON { Globals.grdbg "system_task_enable" (TRIPLE(SYSTEM_TASK_IDENTIFIER arg2, arg3, SEMICOLON);) }
	;

%inline function_port_list : arg2 = function_port_item arg3 = function_port_list_3 { Globals.grdbg "function_port_list" (DOUBLE(arg2, arg3);) }
	;

module_declaration : arg2 = module_nonansi_header arg3 = module_declaration_3 arg4 = module_declaration_4 ENDMODULE arg6 = module_declaration_6 { Globals.grdbg "module_declaration_7" (QUINTUPLE(arg2, arg3, arg4, ENDMODULE, arg6);) }
	| arg8 = module_ansi_header arg9 = module_declaration_9 arg10 = module_declaration_10 ENDMODULE arg12 = module_declaration_12 { Globals.grdbg "module_declaration_13" (QUINTUPLE(arg8, arg9, arg10, ENDMODULE, arg12);) }
	| arg15 = module_keyword arg16 = module_declaration_16 arg17 = identifier LPAREN P_DOTSTAR RPAREN SEMICOLON arg22 = module_declaration_22 arg23 = module_declaration_23 ENDMODULE arg25 = module_declaration_25 { Globals.grdbg "module_declaration_26" (UNDECUPLE(arg15, arg16, arg17, LPAREN, P_DOTSTAR, RPAREN, SEMICOLON, arg22, arg23, ENDMODULE, arg25);) }
	| EXTERN arg28 = module_nonansi_header { Globals.grdbg "module_declaration_29" (DOUBLE(EXTERN, arg28);) }
	| EXTERN arg31 = module_ansi_header { Globals.grdbg "module_declaration" (DOUBLE(EXTERN, arg31);) }
	;

block_item_declaration : arg3 = block_data_declaration { Globals.grdbg "block_item_declaration_4" ((arg3);) }
	| arg6 = local_parameter_declaration { Globals.grdbg "block_item_declaration_7" ((arg6);) }
	| arg9 = parameter_declaration SEMICOLON { Globals.grdbg "block_item_declaration" (DOUBLE(arg9, SEMICOLON);) }
	;

simple_path_declaration : arg2 = parallel_path_description EQUALS arg4 = path_delay_value { Globals.grdbg "simple_path_declaration_5" (TRIPLE(arg2, EQUALS, arg4);) }
	| arg6 = full_path_description EQUALS arg8 = path_delay_value { Globals.grdbg "simple_path_declaration" (TRIPLE(arg6, EQUALS, arg8);) }
	;

%inline task_identifier : arg2 = identifier { Globals.grdbg "task_identifier" ((arg2);) }
	;

non_port_program_item : arg3 = continuous_assign { Globals.grdbg "non_port_program_item_4" ((arg3);) }
	| arg6 = module_or_generate_item_declaration { Globals.grdbg "non_port_program_item_7" ((arg6);) }
	| arg9 = specparam_declaration { Globals.grdbg "non_port_program_item_10" ((arg9);) }
	| arg12 = local_parameter_declaration { Globals.grdbg "non_port_program_item_13" ((arg12);) }
	| arg15 = parameter_declaration SEMICOLON { Globals.grdbg "non_port_program_item_17" (DOUBLE(arg15, SEMICOLON);) }
	| arg19 = initial_construct { Globals.grdbg "non_port_program_item_20" ((arg19);) }
	| arg22 = concurrent_assertion_item { Globals.grdbg "non_port_program_item_23" ((arg22);) }
	| arg24 = class_declaration { Globals.grdbg "non_port_program_item" ((arg24);) }
	;

delay2 : HASH arg3 = delay_value { Globals.grdbg "delay2_4" (DOUBLE(HASH, arg3);) }
	| HASH LPAREN arg7 = mintypmax_expression arg8 = delay2_8 RPAREN { Globals.grdbg "delay2" (QUINTUPLE(HASH, LPAREN, arg7, arg8, RPAREN);) }
	;

delay3 : HASH arg3 = delay_value { Globals.grdbg "delay3_4" (DOUBLE(HASH, arg3);) }
	| HASH LPAREN arg7 = mintypmax_expression arg8 = delay3_8 RPAREN { Globals.grdbg "delay3" (QUINTUPLE(HASH, LPAREN, arg7, arg8, RPAREN);) }
	;

%inline parallel_edge_sensitive_path_description : LPAREN arg3 = parallel_edge_sensitive_path_description_3 arg4 = specify_input_terminal_descriptor P_EQGT arg6 = specify_output_terminal_descriptor arg7 = parallel_edge_sensitive_path_description_7 COLON arg9 = expression RPAREN { Globals.grdbg "parallel_edge_sensitive_path_description" (NONUPLE(LPAREN, arg3, arg4, P_EQGT, arg6, arg7, COLON, arg9, RPAREN);) }
	;

%inline net_alias : ALIAS arg3 = net_lvalue EQUALS arg5 = net_lvalue SEMICOLON { Globals.grdbg "net_alias" (QUINTUPLE(ALIAS, arg3, EQUALS, arg5, SEMICOLON);) }
	;

delay_or_event_control : arg2 = delay_control { Globals.grdbg "delay_or_event_control_3" ((arg2);) }
	| arg4 = event_control { Globals.grdbg "delay_or_event_control_5" ((arg4);) }
	| REPEAT LPAREN arg8 = expression RPAREN arg10 = event_control { Globals.grdbg "delay_or_event_control" (QUINTUPLE(REPEAT, LPAREN, arg8, RPAREN, arg10);) }
	;

%inline non_zero_unsigned_number : arg2 = non_zero_decimal_digit arg3 = non_zero_unsigned_number_3 { Globals.grdbg "non_zero_unsigned_number" (DOUBLE(arg2, arg3);) }
	;

property_expr : arg2 = sequence_expr { Globals.grdbg "property_expr_3" ((arg2);) }
	| arg4 = sequence_expr VBAR_DASH_GT arg6 = property_expr_6 arg7 = sequence_expr { Globals.grdbg "property_expr_8" (QUADRUPLE(arg4, VBAR_DASH_GT, arg6, arg7);) }
	| arg9 = sequence_expr TOKEN_PLING_EQUALS_GT arg11 = property_expr_11 arg12 = sequence_expr { Globals.grdbg "property_expr_13" (QUADRUPLE(arg9, TOKEN_PLING_EQUALS_GT, arg11, arg12);) }
	| LPAREN arg15 = property_expr RPAREN { Globals.grdbg "property_expr" (TRIPLE(LPAREN, arg15, RPAREN);) }
	;

%inline module_nonansi_header : arg3 = module_keyword arg4 = module_nonansi_header_4 arg5 = identifier arg6 = module_nonansi_header_6 arg7 = list_of_ports SEMICOLON { Globals.grdbg "module_nonansi_header" (SEXTUPLE(arg3, arg4, arg5, arg6, arg7, SEMICOLON);) }
	;

%inline d_recovery_timing_check : D_RECOVERY LPAREN arg4 = timing_check_event COMMA arg6 = timing_check_event COMMA arg8 = expression arg9 = d_recovery_timing_check_9 RPAREN SEMICOLON { Globals.grdbg "d_recovery_timing_check" (DECUPLE(D_RECOVERY, LPAREN, arg4, COMMA, arg6, COMMA, arg8, arg9, RPAREN, SEMICOLON);) }
	;

%inline inout_port_identifier : arg2 = identifier { Globals.grdbg "inout_port_identifier" ((arg2);) }
	;

%inline property_declaration : PROPERTY arg3 = identifier arg4 = property_declaration_4 SEMICOLON arg6 = property_declaration_6 arg7 = property_spec SEMICOLON ENDPROPERTY arg10 = property_declaration_10 { Globals.grdbg "property_declaration" (NONUPLE(PROPERTY, arg3, arg4, SEMICOLON, arg6, arg7, SEMICOLON, ENDPROPERTY, arg10);) }
	;

%inline name_of_gate_instance : arg2 = identifier arg3 = name_of_gate_instance_3 { Globals.grdbg "name_of_gate_instance" (DOUBLE(arg2, arg3);) }
	;

specify_terminal_descriptor : arg2 = specify_input_terminal_descriptor { Globals.grdbg "specify_terminal_descriptor_3" ((arg2);) }
	| arg4 = specify_output_terminal_descriptor { Globals.grdbg "specify_terminal_descriptor" ((arg4);) }
	;

default_skew : INPUT arg3 = clocking_skew { Globals.grdbg "default_skew_4" (DOUBLE(INPUT, arg3);) }
	| OUTPUT arg6 = clocking_skew { Globals.grdbg "default_skew_7" (DOUBLE(OUTPUT, arg6);) }
	| INPUT arg9 = clocking_skew OUTPUT arg11 = clocking_skew { Globals.grdbg "default_skew" (QUADRUPLE(INPUT, arg9, OUTPUT, arg11);) }
	;

%inline udp_identifier : arg2 = identifier { Globals.grdbg "udp_identifier" ((arg2);) }
	;

%inline interface_ansi_header : INTERFACE arg4 = interface_ansi_header_4 arg5 = identifier arg6 = interface_ansi_header_6 arg7 = interface_ansi_header_7 SEMICOLON { Globals.grdbg "interface_ansi_header" (SEXTUPLE(INTERFACE, arg4, arg5, arg6, arg7, SEMICOLON);) }
	;

module_path_expression : arg2 = module_path_primary { Globals.grdbg "module_path_expression_3" ((arg2);) }
	| arg4 = unary_module_path_operator arg6 = module_path_primary { Globals.grdbg "module_path_expression_7" (DOUBLE(arg4, arg6);) }
	| arg8 = module_path_expression arg9 = binary_module_path_operator arg11 = module_path_expression { Globals.grdbg "module_path_expression_12" (TRIPLE(arg8, arg9, arg11);) }
	| arg13 = module_path_conditional_expression { Globals.grdbg "module_path_expression" ((arg13);) }
	;

integer_type : arg2 = integer_vector_type { Globals.grdbg "integer_type_3" ((arg2);) }
	| arg4 = integer_atom_type { Globals.grdbg "integer_type" ((arg4);) }
	;

class_item : arg3 = class_property { Globals.grdbg "class_item_4" ((arg3);) }
	| arg6 = class_method { Globals.grdbg "class_item_7" ((arg6);) }
	| arg9 = class_constraint { Globals.grdbg "class_item" ((arg9);) }
	;

%inline binary_value : arg2 = binary_digit arg3 = binary_value_3 { Globals.grdbg "binary_value" (DOUBLE(arg2, arg3);) }
	;

unary_module_path_operator : PLING { Globals.grdbg "unary_module_path_operator_3" ((PLING);) }
	| TILDE { Globals.grdbg "unary_module_path_operator_5" ((TILDE);) }
	| AMPERSAND { Globals.grdbg "unary_module_path_operator_7" ((AMPERSAND);) }
	| P_NAND { Globals.grdbg "unary_module_path_operator_9" ((P_NAND);) }
	| VBAR { Globals.grdbg "unary_module_path_operator_11" ((VBAR);) }
	| TILDE_VBAR { Globals.grdbg "unary_module_path_operator_13" ((TILDE_VBAR);) }
	| CARET { Globals.grdbg "unary_module_path_operator_15" ((CARET);) }
	| P_NXOR { Globals.grdbg "unary_module_path_operator_17" ((P_NXOR);) }
	| P_XNOR { Globals.grdbg "unary_module_path_operator" ((P_XNOR);) }
	;

dot_named_port_connection : DOT arg4 = identifier { Globals.grdbg "dot_named_port_connection_5" (DOUBLE(DOT, arg4);) }
	| arg6 = named_port_connection { Globals.grdbg "dot_named_port_connection" ((arg6);) }
	;

%inline program_instance : arg2 = identifier arg3 = program_instance_3 LPAREN arg5 = program_instance_5 RPAREN { Globals.grdbg "program_instance" (QUINTUPLE(arg2, arg3, LPAREN, arg5, RPAREN);) }
	;

%inline enable_gate_instance : arg2 = enable_gate_instance_2 LPAREN arg4 = net_lvalue COMMA arg6 = expression COMMA arg8 = expression RPAREN { Globals.grdbg "enable_gate_instance" (OCTUPLE(arg2, LPAREN, arg4, COMMA, arg6, COMMA, arg8, RPAREN);) }
	;

%inline type_declaration_identifier : arg2 = identifier arg3 = type_declaration_identifier_3 { Globals.grdbg "type_declaration_identifier" (DOUBLE(arg2, arg3);) }
	;

decimal_number : arg2 = UNSIGNED_NUMBER { Globals.grdbg "decimal_number_3" ((UNSIGNED_NUMBER arg2);) }
	| arg4 = decimal_number_4 arg5 = DECIMAL_BASE arg6 = UNSIGNED_NUMBER { Globals.grdbg "decimal_number_7" (TRIPLE(arg4, DECIMAL_BASE arg5, UNSIGNED_NUMBER arg6);) }
	| arg8 = decimal_number_8 arg9 = DECIMAL_BASE arg10 = X_DIGIT arg11 = decimal_number_11 { Globals.grdbg "decimal_number_12" (QUADRUPLE(arg8, DECIMAL_BASE arg9, X_DIGIT arg10, arg11);) }
	| arg13 = decimal_number_13 arg14 = DECIMAL_BASE arg15 = Z_DIGIT arg16 = decimal_number_16 { Globals.grdbg "decimal_number" (QUADRUPLE(arg13, DECIMAL_BASE arg14, Z_DIGIT arg15, arg16);) }
	;

%inline escaped_identifier : BACKSLASH TOKEN_ANY_ASCII arg4 = white_space { Globals.grdbg "escaped_identifier" (TRIPLE(BACKSLASH, TOKEN_ANY_ASCII, arg4);) }
	;

struct_member_label : DEFAULT { Globals.grdbg "struct_member_label_3" ((DEFAULT);) }
	| arg4 = identifier { Globals.grdbg "struct_member_label_5" ((arg4);) }
	| arg6 = identifier { Globals.grdbg "struct_member_label" ((arg6);) }
	;

%inline constraint_identifier : arg2 = identifier { Globals.grdbg "constraint_identifier" ((arg2);) }
	;

%inline udp_instance_identifier : arg2 = identifier { Globals.grdbg "udp_instance_identifier" ((arg2);) }
	;

%inline method_identifier : arg2 = identifier { Globals.grdbg "method_identifier" ((arg2);) }
	;

%inline constant_base_expression : arg2 = constant_expression { Globals.grdbg "constant_base_expression" ((arg2);) }
	;

%inline generate_module_conditional_statement : IF LPAREN arg4 = constant_expression RPAREN arg6 = generate_module_item arg7 = generate_module_conditional_statement_7 { Globals.grdbg "generate_module_conditional_statement" (SEXTUPLE(IF, LPAREN, arg4, RPAREN, arg6, arg7);) }
	;

event_expression : arg2 = event_expression_2 arg3 = expression arg4 = event_expression_4 { Globals.grdbg "event_expression_5" (TRIPLE(arg2, arg3, arg4);) }
	| arg6 = event_expression OR arg8 = event_expression { Globals.grdbg "event_expression_9" (TRIPLE(arg6, OR, arg8);) }
	| arg10 = event_expression COMMA arg12 = event_expression { Globals.grdbg "event_expression" (TRIPLE(arg10, COMMA, arg12);) }
	;

output_declaration : OUTPUT arg3 = output_declaration_3 arg4 = list_of_port_identifiers { Globals.grdbg "output_declaration_5" (TRIPLE(OUTPUT, arg3, arg4);) }
	| OUTPUT arg7 = data_type arg8 = list_of_variable_port_identifiers { Globals.grdbg "output_declaration" (TRIPLE(OUTPUT, arg7, arg8);) }
	;

port_expression : arg2 = port_reference { Globals.grdbg "port_expression_3" ((arg2);) }
	| LCURLY arg5 = port_reference arg6 = port_expression_6 RCURLY { Globals.grdbg "port_expression" (QUADRUPLE(LCURLY, arg5, arg6, RCURLY);) }
	;

%inline udp_initial_statement : INITIAL arg3 = identifier EQUALS arg5 = init_val SEMICOLON { Globals.grdbg "udp_initial_statement" (QUINTUPLE(INITIAL, arg3, EQUALS, arg5, SEMICOLON);) }
	;

%inline variable_identifier : arg2 = identifier { Globals.grdbg "variable_identifier" ((arg2);) }
	;

%inline udp_instantiation : arg2 = identifier arg3 = udp_instantiation_3 arg4 = udp_instantiation_4 arg5 = udp_instance arg6 = udp_instantiation_6 SEMICOLON { Globals.grdbg "udp_instantiation" (SEXTUPLE(arg2, arg3, arg4, arg5, arg6, SEMICOLON);) }
	;

range_list_or_array : arg2 = identifier { Globals.grdbg "range_list_or_array_3" ((arg2);) }
	| LCURLY arg5 = value_range arg6 = range_list_or_array_6 RCURLY { Globals.grdbg "range_list_or_array" (QUADRUPLE(LCURLY, arg5, arg6, RCURLY);) }
	;

list_of_parameter_assignments : arg2 = ordered_parameter_assignment arg3 = list_of_parameter_assignments_3 { Globals.grdbg "list_of_parameter_assignments_4" (DOUBLE(arg2, arg3);) }
	| arg5 = named_parameter_assignment arg6 = list_of_parameter_assignments_6 { Globals.grdbg "list_of_parameter_assignments" (DOUBLE(arg5, arg6);) }
	;

%inline hierarchical_variable_identifier : arg2 = hierarchical_identifier { Globals.grdbg "hierarchical_variable_identifier" ((arg2);) }
	;

%inline d_fullskew_timing_check : FULLSKEW LPAREN arg4 = timing_check_event COMMA arg6 = timing_check_event COMMA arg8 = expression COMMA arg10 = expression arg11 = d_fullskew_timing_check_11 RPAREN SEMICOLON { Globals.grdbg "d_fullskew_timing_check" (DUODECUPLE(FULLSKEW, LPAREN, arg4, COMMA, arg6, COMMA, arg8, COMMA, arg10, arg11, RPAREN, SEMICOLON);) }
	;

%inline d_skew_timing_check : D_SKEW LPAREN arg4 = timing_check_event COMMA arg6 = timing_check_event COMMA arg8 = expression arg9 = d_skew_timing_check_9 RPAREN SEMICOLON { Globals.grdbg "d_skew_timing_check" (DECUPLE(D_SKEW, LPAREN, arg4, COMMA, arg6, COMMA, arg8, arg9, RPAREN, SEMICOLON);) }
	;

cycle_delay_range : P_POUNDPOUND arg3 = constant_expression { Globals.grdbg "cycle_delay_range_4" (DOUBLE(P_POUNDPOUND, arg3);) }
	| P_POUNDPOUND LBRACK arg7 = cycle_delay_const_range_expression RBRACK { Globals.grdbg "cycle_delay_range" (QUADRUPLE(P_POUNDPOUND, LBRACK, arg7, RBRACK);) }
	;

number : arg2 = decimal_number { Globals.grdbg "number_3" ((arg2);) }
	| arg4 = octal_number { Globals.grdbg "number_5" ((arg4);) }
	| arg6 = binary_number { Globals.grdbg "number_7" ((arg6);) }
	| arg8 = hex_number { Globals.grdbg "number_9" ((arg8);) }
	| arg10 = real_number { Globals.grdbg "number" ((arg10);) }
	;

function_port_item : arg3 = tf_input_declaration { Globals.grdbg "function_port_item_4" ((arg3);) }
	| arg6 = tf_output_declaration { Globals.grdbg "function_port_item_7" ((arg6);) }
	| arg9 = tf_inout_declaration { Globals.grdbg "function_port_item_10" ((arg9);) }
	| arg12 = tf_ref_declaration { Globals.grdbg "function_port_item_13" ((arg12);) }
	| arg15 = port_type arg16 = list_of_tf_port_identifiers { Globals.grdbg "function_port_item_17" (DOUBLE(arg15, arg16);) }
	| arg19 = tf_data_type arg20 = list_of_tf_variable_identifiers { Globals.grdbg "function_port_item" (DOUBLE(arg19, arg20);) }
	;

const_or_range_expression : arg2 = constant_expression { Globals.grdbg "const_or_range_expression_3" ((arg2);) }
	| arg4 = cycle_delay_const_range_expression { Globals.grdbg "const_or_range_expression" ((arg4);) }
	;

%inline udp_reg_declaration : REG arg4 = identifier { Globals.grdbg "udp_reg_declaration" (DOUBLE(REG, arg4);) }
	;

%inline generate_module_block : BEGIN arg3 = generate_module_block_3 arg4 = generate_module_block_4 END arg6 = generate_module_block_6 { Globals.grdbg "generate_module_block" (QUINTUPLE(BEGIN, arg3, arg4, END, arg6);) }
	;

%inline formal_identifier : arg2 = identifier { Globals.grdbg "formal_identifier" ((arg2);) }
	;

integer_atom_type : BYTE { Globals.grdbg "integer_atom_type_3" ((BYTE);) }
	| SHORTINT { Globals.grdbg "integer_atom_type_5" ((SHORTINT);) }
	| SVINT { Globals.grdbg "integer_atom_type_7" ((SVINT);) }
	| LONGINT { Globals.grdbg "integer_atom_type_9" ((LONGINT);) }
	| INTEGER { Globals.grdbg "integer_atom_type" ((INTEGER);) }
	;

octal_digit : arg2 = X_DIGIT { Globals.grdbg "octal_digit_3" ((X_DIGIT arg2);) }
	| arg4 = Z_DIGIT { Globals.grdbg "octal_digit_5" ((Z_DIGIT arg4);) }
	| TOKEN_ZERO { Globals.grdbg "octal_digit_7" ((TOKEN_ZERO);) }
	| TOKEN_ONE { Globals.grdbg "octal_digit_9" ((TOKEN_ONE);) }
	| TOKEN_TWO { Globals.grdbg "octal_digit_11" ((TOKEN_TWO);) }
	| TOKEN_THREE { Globals.grdbg "octal_digit_13" ((TOKEN_THREE);) }
	| TOKEN_FOUR { Globals.grdbg "octal_digit_15" ((TOKEN_FOUR);) }
	| TOKEN_FIVE { Globals.grdbg "octal_digit_17" ((TOKEN_FIVE);) }
	| TOKEN_SIX { Globals.grdbg "octal_digit_19" ((TOKEN_SIX);) }
	| TOKEN_SEVEN { Globals.grdbg "octal_digit" ((TOKEN_SEVEN);) }
	;

delayed_reference : arg2 = identifier { Globals.grdbg "delayed_reference_3" ((arg2);) }
	| arg4 = identifier LBRACK arg6 = constant_mintypmax_expression RBRACK { Globals.grdbg "delayed_reference" (QUADRUPLE(arg4, LBRACK, arg6, RBRACK);) }
	;

class_item_qualifier : STATIC { Globals.grdbg "class_item_qualifier_3" ((STATIC);) }
	| PROTECTED { Globals.grdbg "class_item_qualifier_5" ((PROTECTED);) }
	| LOCAL { Globals.grdbg "class_item_qualifier" ((LOCAL);) }
	;

type_declaration : TYPEDEF arg3 = type_declaration_3 arg4 = type_declaration_identifier SEMICOLON { Globals.grdbg "type_declaration_6" (QUADRUPLE(TYPEDEF, arg3, arg4, SEMICOLON);) }
	| TYPEDEF arg8 = hierarchical_identifier DOT arg10 = identifier arg11 = type_declaration_identifier SEMICOLON { Globals.grdbg "type_declaration_13" (SEXTUPLE(TYPEDEF, arg8, DOT, arg10, arg11, SEMICOLON);) }
	| TYPEDEF arg15 = type_declaration_15 arg16 = identifier SEMICOLON { Globals.grdbg "type_declaration_18" (QUADRUPLE(TYPEDEF, arg15, arg16, SEMICOLON);) }
	| TYPEDEF arg20 = identifier arg21 = type_declaration_21 arg22 = type_declaration_identifier SEMICOLON { Globals.grdbg "type_declaration" (QUINTUPLE(TYPEDEF, arg20, arg21, arg22, SEMICOLON);) }
	;

%inline enum_identifier : arg2 = identifier { Globals.grdbg "enum_identifier" ((arg2);) }
	;

net_type : SUPPLY0 { Globals.grdbg "net_type_3" ((SUPPLY0);) }
	| SUPPLY1 { Globals.grdbg "net_type_5" ((SUPPLY1);) }
	| TRI { Globals.grdbg "net_type_7" ((TRI);) }
	| TRIAND { Globals.grdbg "net_type_9" ((TRIAND);) }
	| TRIOR { Globals.grdbg "net_type_11" ((TRIOR);) }
	| TRI0 { Globals.grdbg "net_type_13" ((TRI0);) }
	| TRI1 { Globals.grdbg "net_type_15" ((TRI1);) }
	| WIRE { Globals.grdbg "net_type_17" ((WIRE);) }
	| WAND { Globals.grdbg "net_type_19" ((WAND);) }
	| WOR { Globals.grdbg "net_type" ((WOR);) }
	;

%inline threshold : arg2 = constant_expression { Globals.grdbg "threshold" ((arg2);) }
	;

port_type : arg2 = data_type { Globals.grdbg "port_type_3" ((arg2);) }
	| arg4 = net_type arg5 = port_type_5 arg6 = port_type_6 { Globals.grdbg "port_type_7" (TRIPLE(arg4, arg5, arg6);) }
	| TRIREG arg9 = port_type_9 arg10 = port_type_10 { Globals.grdbg "port_type_11" (TRIPLE(TRIREG, arg9, arg10);) }
	| arg12 = port_type_12 arg13 = port_type_13 arg14 = range { Globals.grdbg "port_type" (TRIPLE(arg12, arg13, arg14);) }
	;

%inline hierarchical_parameter_identifier : arg2 = hierarchical_identifier { Globals.grdbg "hierarchical_parameter_identifier" ((arg2);) }
	;

%inline d_timeskew_timing_check : D_TIMESKEW LPAREN arg4 = timing_check_event COMMA arg6 = timing_check_event COMMA arg8 = expression arg9 = d_timeskew_timing_check_9 RPAREN SEMICOLON { Globals.grdbg "d_timeskew_timing_check" (DECUPLE(D_TIMESKEW, LPAREN, arg4, COMMA, arg6, COMMA, arg8, arg9, RPAREN, SEMICOLON);) }
	;

%inline checktime_condition : arg2 = mintypmax_expression { Globals.grdbg "checktime_condition" ((arg2);) }
	;

drive_strength : LPAREN arg3 = strength0 COMMA arg5 = strength1 RPAREN { Globals.grdbg "drive_strength_7" (QUINTUPLE(LPAREN, arg3, COMMA, arg5, RPAREN);) }
	| LPAREN arg9 = strength1 COMMA arg11 = strength0 RPAREN { Globals.grdbg "drive_strength_13" (QUINTUPLE(LPAREN, arg9, COMMA, arg11, RPAREN);) }
	| LPAREN arg15 = strength0 COMMA HIGHZ1 RPAREN { Globals.grdbg "drive_strength_19" (QUINTUPLE(LPAREN, arg15, COMMA, HIGHZ1, RPAREN);) }
	| LPAREN arg21 = strength1 COMMA HIGHZ0 RPAREN { Globals.grdbg "drive_strength_25" (QUINTUPLE(LPAREN, arg21, COMMA, HIGHZ0, RPAREN);) }
	| LPAREN HIGHZ0 COMMA arg29 = strength1 RPAREN { Globals.grdbg "drive_strength_31" (QUINTUPLE(LPAREN, HIGHZ0, COMMA, arg29, RPAREN);) }
	| LPAREN HIGHZ1 COMMA arg35 = strength0 RPAREN { Globals.grdbg "drive_strength" (QUINTUPLE(LPAREN, HIGHZ1, COMMA, arg35, RPAREN);) }
	;

%inline interface_identifier : arg2 = identifier { Globals.grdbg "interface_identifier" ((arg2);) }
	;

%inline named_port_connection : DOT arg4 = identifier LPAREN arg6 = named_port_connection_6 RPAREN { Globals.grdbg "named_port_connection" (QUINTUPLE(DOT, arg4, LPAREN, arg6, RPAREN);) }
	;

dpi_import_export : IMPORT TOKEN_DPI arg4 = dpi_import_export_4 arg5 = dpi_import_export_5 arg6 = dpi_function_proto SEMICOLON { Globals.grdbg "dpi_import_export_8" (SEXTUPLE(IMPORT, TOKEN_DPI, arg4, arg5, arg6, SEMICOLON);) }
	| EXPORT TOKEN_DPI arg11 = dpi_import_export_11 FUNCTION arg13 = identifier SEMICOLON { Globals.grdbg "dpi_import_export" (SEXTUPLE(EXPORT, TOKEN_DPI, arg11, FUNCTION, arg13, SEMICOLON);) }
	;

mintypmax_expression : arg2 = expression { Globals.grdbg "mintypmax_expression_3" ((arg2);) }
	| arg4 = expression COLON arg6 = expression COLON arg8 = expression { Globals.grdbg "mintypmax_expression" (QUINTUPLE(arg4, COLON, arg6, COLON, arg8);) }
	;

list_of_path_delay_expressions : arg2 = path_delay_expression { Globals.grdbg "list_of_path_delay_expressions_3" ((arg2);) }
	| arg4 = path_delay_expression COMMA arg6 = path_delay_expression { Globals.grdbg "list_of_path_delay_expressions_7" (TRIPLE(arg4, COMMA, arg6);) }
	| arg8 = path_delay_expression COMMA arg10 = path_delay_expression COMMA arg12 = path_delay_expression { Globals.grdbg "list_of_path_delay_expressions_13" (QUINTUPLE(arg8, COMMA, arg10, COMMA, arg12);) }
	| arg14 = path_delay_expression COMMA arg16 = path_delay_expression COMMA arg18 = path_delay_expression COMMA arg20 = path_delay_expression COMMA arg22 = path_delay_expression COMMA arg24 = path_delay_expression { Globals.grdbg "list_of_path_delay_expressions_25" (UNDECUPLE(arg14, COMMA, arg16, COMMA, arg18, COMMA, arg20, COMMA, arg22, COMMA, arg24);) }
	| arg26 = path_delay_expression COMMA arg28 = path_delay_expression COMMA arg30 = path_delay_expression COMMA arg32 = path_delay_expression COMMA arg34 = path_delay_expression COMMA arg36 = path_delay_expression COMMA arg38 = path_delay_expression COMMA arg40 = path_delay_expression COMMA arg42 = path_delay_expression COMMA arg44 = path_delay_expression COMMA arg46 = path_delay_expression COMMA arg48 = path_delay_expression { Globals.grdbg "list_of_path_delay_expressions" (TREVIGENUPLE(arg26, COMMA, arg28, COMMA, arg30, COMMA, arg32, COMMA, arg34, COMMA, arg36, COMMA, arg38, COMMA, arg40, COMMA, arg42, COMMA, arg44, COMMA, arg46, COMMA, arg48);) }
	;

property_spec : arg2 = property_spec_2 arg3 = property_spec_3 LPAREN arg5 = expression RPAREN arg7 = property_spec_7 arg8 = property_expr { Globals.grdbg "property_spec_9" (SEPTUPLE(arg2, arg3, LPAREN, arg5, RPAREN, arg7, arg8);) }
	| arg10 = property_spec_10 arg11 = property_spec_11 arg12 = multi_clock_property_expr { Globals.grdbg "property_spec" (TRIPLE(arg10, arg11, arg12);) }
	;

class_property : arg2 = class_property_2 arg3 = data_declaration { Globals.grdbg "class_property_4" (DOUBLE(arg2, arg3);) }
	| CONST arg6 = class_property_6 arg7 = data_type arg8 = identifier arg9 = class_property_9 SEMICOLON { Globals.grdbg "class_property" (SEXTUPLE(CONST, arg6, arg7, arg8, arg9, SEMICOLON);) }
	;

real_number : arg2 = fixed_point_number { Globals.grdbg "real_number_3" ((arg2);) }
	| arg4 = UNSIGNED_NUMBER arg5 = real_number_5 arg6 = EXP arg7 = real_number_7 arg8 = UNSIGNED_NUMBER { Globals.grdbg "real_number" (QUINTUPLE(UNSIGNED_NUMBER arg4, arg5, EXP arg6, arg7, UNSIGNED_NUMBER arg8);) }
	;

%inline d_recrem_timing_check : D_RECREM LPAREN arg4 = timing_check_event COMMA arg6 = timing_check_event COMMA arg8 = expression COMMA arg10 = expression arg11 = d_recrem_timing_check_11 RPAREN SEMICOLON { Globals.grdbg "d_recrem_timing_check" (DUODECUPLE(D_RECREM, LPAREN, arg4, COMMA, arg6, COMMA, arg8, COMMA, arg10, arg11, RPAREN, SEMICOLON);) }
	;

edge_indicator : LPAREN arg3 = LEVEL_SYMBOL arg4 = LEVEL_SYMBOL RPAREN { Globals.grdbg "edge_indicator_6" (QUADRUPLE(LPAREN, LEVEL_SYMBOL arg3, LEVEL_SYMBOL arg4, RPAREN);) }
	| arg7 = EDGE_SYMBOL { Globals.grdbg "edge_indicator" ((EDGE_SYMBOL arg7);) }
	;

%inline pass_switch_instance : arg2 = pass_switch_instance_2 LPAREN arg4 = net_lvalue COMMA arg6 = net_lvalue RPAREN { Globals.grdbg "pass_switch_instance" (SEXTUPLE(arg2, LPAREN, arg4, COMMA, arg6, RPAREN);) }
	;

%inline nonblocking_assignment : arg2 = variable_lvalue P_LTE arg4 = nonblocking_assignment_4 arg5 = expression { Globals.grdbg "nonblocking_assignment" (QUADRUPLE(arg2, P_LTE, arg4, arg5);) }
	;

pulldown_strength : LPAREN arg3 = strength0 COMMA arg5 = strength1 RPAREN { Globals.grdbg "pulldown_strength_7" (QUINTUPLE(LPAREN, arg3, COMMA, arg5, RPAREN);) }
	| LPAREN arg9 = strength1 COMMA arg11 = strength0 RPAREN { Globals.grdbg "pulldown_strength_13" (QUINTUPLE(LPAREN, arg9, COMMA, arg11, RPAREN);) }
	| LPAREN arg15 = strength0 RPAREN { Globals.grdbg "pulldown_strength" (TRIPLE(LPAREN, arg15, RPAREN);) }
	;

%inline list_of_port_identifiers : arg2 = identifier arg3 = list_of_port_identifiers_3 arg4 = list_of_port_identifiers_4 { Globals.grdbg "list_of_port_identifiers" (TRIPLE(arg2, arg3, arg4);) }
	;

%inline parameter_identifier : arg2 = identifier { Globals.grdbg "parameter_identifier" ((arg2);) }
	;

%inline start_edge_offset : arg2 = mintypmax_expression { Globals.grdbg "start_edge_offset" ((arg2);) }
	;

state_dependent_path_declaration : IF LPAREN arg4 = module_path_expression RPAREN arg6 = simple_path_declaration { Globals.grdbg "state_dependent_path_declaration_7" (QUINTUPLE(IF, LPAREN, arg4, RPAREN, arg6);) }
	| IF LPAREN arg10 = module_path_expression RPAREN arg12 = edge_sensitive_path_declaration { Globals.grdbg "state_dependent_path_declaration_13" (QUINTUPLE(IF, LPAREN, arg10, RPAREN, arg12);) }
	| IF_NONE arg15 = simple_path_declaration { Globals.grdbg "state_dependent_path_declaration" (DOUBLE(IF_NONE, arg15);) }
	;

%inline include_statement : INCLUDE LESS arg4 = file_path_spec GREATER SEMICOLON { Globals.grdbg "include_statement" (QUINTUPLE(INCLUDE, LESS, arg4, GREATER, SEMICOLON);) }
	;

%inline immediate_assert_statement : ASSERT LPAREN arg4 = expression RPAREN arg6 = action_block { Globals.grdbg "immediate_assert_statement" (QUINTUPLE(ASSERT, LPAREN, arg4, RPAREN, arg6);) }
	;

%inline modport_identifier : arg2 = identifier { Globals.grdbg "modport_identifier" ((arg2);) }
	;

%inline inout_terminal : arg2 = net_lvalue { Globals.grdbg "inout_terminal" ((arg2);) }
	;

extern_tf_declaration : EXTERN arg3 = method_prototype { Globals.grdbg "extern_tf_declaration_4" (DOUBLE(EXTERN, arg3);) }
	| EXTERN FORKJOIN TASK arg8 = named_task_proto SEMICOLON { Globals.grdbg "extern_tf_declaration" (QUINTUPLE(EXTERN, FORKJOIN, TASK, arg8, SEMICOLON);) }
	;

expression : arg2 = primary { Globals.grdbg "expression_3" ((arg2);) }
	| arg4 = unary_operator arg6 = primary { Globals.grdbg "expression_7" (DOUBLE(arg4, arg6);) }
	| arg8 = inc_or_dec_expression { Globals.grdbg "expression_9" ((arg8);) }
	| LPAREN arg11 = operator_assignment RPAREN { Globals.grdbg "expression_13" (TRIPLE(LPAREN, arg11, RPAREN);) }
	| arg14 = expression arg15 = binary_operator arg17 = expression { Globals.grdbg "expression_18" (TRIPLE(arg14, arg15, arg17);) }
	| arg19 = conditional_expression { Globals.grdbg "expression_20" ((arg19);) }
	| arg21 = string_literal { Globals.grdbg "expression_22" ((arg21);) }
	| arg23 = inside_expression { Globals.grdbg "expression" ((arg23);) }
	;

generate_interface_item : arg2 = generate_interface_conditional_statement { Globals.grdbg "generate_interface_item_3" ((arg2);) }
	| arg4 = generate_interface_case_statement { Globals.grdbg "generate_interface_item_5" ((arg4);) }
	| arg6 = generate_interface_loop_statement { Globals.grdbg "generate_interface_item_7" ((arg6);) }
	| arg8 = generate_interface_item_8 arg9 = generate_interface_block { Globals.grdbg "generate_interface_item_10" (DOUBLE(arg8, arg9);) }
	| arg11 = interface_or_generate_item { Globals.grdbg "generate_interface_item" ((arg11);) }
	;

%inline d_setup_timing_check : D_SETUP LPAREN arg4 = timing_check_event COMMA arg6 = timing_check_event COMMA arg8 = expression arg9 = d_setup_timing_check_9 RPAREN SEMICOLON { Globals.grdbg "d_setup_timing_check" (DECUPLE(D_SETUP, LPAREN, arg4, COMMA, arg6, COMMA, arg8, arg9, RPAREN, SEMICOLON);) }
	;

task_item_declaration : arg2 = block_item_declaration { Globals.grdbg "task_item_declaration_3" ((arg2);) }
	| arg5 = tf_input_declaration SEMICOLON { Globals.grdbg "task_item_declaration_7" (DOUBLE(arg5, SEMICOLON);) }
	| arg9 = tf_output_declaration SEMICOLON { Globals.grdbg "task_item_declaration_11" (DOUBLE(arg9, SEMICOLON);) }
	| arg13 = tf_inout_declaration SEMICOLON { Globals.grdbg "task_item_declaration_15" (DOUBLE(arg13, SEMICOLON);) }
	| arg17 = tf_ref_declaration SEMICOLON { Globals.grdbg "task_item_declaration" (DOUBLE(arg17, SEMICOLON);) }
	;

%inline timing_check_limit : arg2 = expression { Globals.grdbg "timing_check_limit" ((arg2);) }
	;

range_expression : arg2 = expression { Globals.grdbg "range_expression_3" ((arg2);) }
	| arg4 = constant_expression COLON arg6 = constant_expression { Globals.grdbg "range_expression_7" (TRIPLE(arg4, COLON, arg6);) }
	| arg8 = expression P_PLUSCOLON arg10 = constant_expression { Globals.grdbg "range_expression_11" (TRIPLE(arg8, P_PLUSCOLON, arg10);) }
	| arg12 = expression P_MINUSCOLON arg14 = constant_expression { Globals.grdbg "range_expression" (TRIPLE(arg12, P_MINUSCOLON, arg14);) }
	;

%inline output_port_identifier : arg2 = identifier { Globals.grdbg "output_port_identifier" ((arg2);) }
	;

%inline generate_interface_conditional_statement : IF LPAREN arg4 = constant_expression RPAREN arg6 = generate_interface_item arg7 = generate_interface_conditional_statement_7 { Globals.grdbg "generate_interface_conditional_statement" (SEXTUPLE(IF, LPAREN, arg4, RPAREN, arg6, arg7);) }
	;

bind_instantiation : arg2 = program_instantiation { Globals.grdbg "bind_instantiation_3" ((arg2);) }
	| arg4 = module_instantiation { Globals.grdbg "bind_instantiation_5" ((arg4);) }
	| arg6 = interface_instantiation { Globals.grdbg "bind_instantiation" ((arg6);) }
	;

%inline list_of_path_inputs : arg2 = specify_input_terminal_descriptor arg3 = list_of_path_inputs_3 { Globals.grdbg "list_of_path_inputs" (DOUBLE(arg2, arg3);) }
	;

interface_item : arg2 = non_generic_port_declaration SEMICOLON { Globals.grdbg "interface_item_4" (DOUBLE(arg2, SEMICOLON);) }
	| arg5 = non_port_interface_item { Globals.grdbg "interface_item" ((arg5);) }
	;

assignment_operator : EQUALS { Globals.grdbg "assignment_operator_3" ((EQUALS);) }
	| P_PLUSEQ { Globals.grdbg "assignment_operator_5" ((P_PLUSEQ);) }
	| P_MINUSEQ { Globals.grdbg "assignment_operator_7" ((P_MINUSEQ);) }
	| P_TIMESEQ { Globals.grdbg "assignment_operator_9" ((P_TIMESEQ);) }
	| P_DIVEQ { Globals.grdbg "assignment_operator_11" ((P_DIVEQ);) }
	| P_MODEQ { Globals.grdbg "assignment_operator_13" ((P_MODEQ);) }
	| P_ANDEQ { Globals.grdbg "assignment_operator_15" ((P_ANDEQ);) }
	| TOKEN_VBAR_EQUALS { Globals.grdbg "assignment_operator_17" ((TOKEN_VBAR_EQUALS);) }
	| P_XOREQ { Globals.grdbg "assignment_operator_19" ((P_XOREQ);) }
	| P_SLEFTEQ { Globals.grdbg "assignment_operator_21" ((P_SLEFTEQ);) }
	| P_SRIGHTEQ { Globals.grdbg "assignment_operator_23" ((P_SRIGHTEQ);) }
	| P_SLEFT3EQ { Globals.grdbg "assignment_operator_25" ((P_SLEFT3EQ);) }
	| P_SSRIGHT3EQ { Globals.grdbg "assignment_operator" ((P_SSRIGHT3EQ);) }
	;

udp_body : arg2 = combinational_body { Globals.grdbg "udp_body_3" ((arg2);) }
	| arg4 = sequential_body { Globals.grdbg "udp_body" ((arg4);) }
	;

delay_value : arg2 = UNSIGNED_NUMBER { Globals.grdbg "delay_value_3" ((UNSIGNED_NUMBER arg2);) }
	| arg4 = real_number { Globals.grdbg "delay_value_5" ((arg4);) }
	| arg6 = identifier { Globals.grdbg "delay_value" ((arg6);) }
	;

%inline type_assignment : arg2 = identifier EQUALS arg4 = data_type { Globals.grdbg "type_assignment" (TRIPLE(arg2, EQUALS, arg4);) }
	;

%inline list_of_param_assignments : arg2 = param_assignment arg3 = list_of_param_assignments_3 { Globals.grdbg "list_of_param_assignments" (DOUBLE(arg2, arg3);) }
	;

program_declaration : arg2 = program_nonansi_header arg3 = program_declaration_3 arg4 = program_declaration_4 ENDPROGRAM arg6 = program_declaration_6 { Globals.grdbg "program_declaration_7" (QUINTUPLE(arg2, arg3, arg4, ENDPROGRAM, arg6);) }
	| arg8 = program_ansi_header arg9 = program_declaration_9 arg10 = program_declaration_10 ENDPROGRAM arg12 = program_declaration_12 { Globals.grdbg "program_declaration_13" (QUINTUPLE(arg8, arg9, arg10, ENDPROGRAM, arg12);) }
	| PROGRAM arg16 = identifier LPAREN P_DOTSTAR RPAREN SEMICOLON arg21 = program_declaration_21 arg22 = program_declaration_22 ENDPROGRAM arg24 = program_declaration_24 { Globals.grdbg "program_declaration_25" (DECUPLE(PROGRAM, arg16, LPAREN, P_DOTSTAR, RPAREN, SEMICOLON, arg21, arg22, ENDPROGRAM, arg24);) }
	| EXTERN arg27 = program_nonansi_header { Globals.grdbg "program_declaration_28" (DOUBLE(EXTERN, arg27);) }
	| EXTERN arg30 = program_ansi_header { Globals.grdbg "program_declaration" (DOUBLE(EXTERN, arg30);) }
	;

loop_statement : FOREVER arg3 = statement_or_null { Globals.grdbg "loop_statement_4" (DOUBLE(FOREVER, arg3);) }
	| REPEAT LPAREN arg7 = expression RPAREN arg9 = statement_or_null { Globals.grdbg "loop_statement_10" (QUINTUPLE(REPEAT, LPAREN, arg7, RPAREN, arg9);) }
	| WHILE LPAREN arg13 = expression RPAREN arg15 = statement_or_null { Globals.grdbg "loop_statement_16" (QUINTUPLE(WHILE, LPAREN, arg13, RPAREN, arg15);) }
	| FOR LPAREN arg19 = variable_decl_or_assignment SEMICOLON arg21 = expression SEMICOLON arg23 = variable_assignment RPAREN arg25 = statement_or_null { Globals.grdbg "loop_statement_26" (NONUPLE(FOR, LPAREN, arg19, SEMICOLON, arg21, SEMICOLON, arg23, RPAREN, arg25);) }
	| FOR LPAREN arg29 = variable_decl_or_assignment arg30 = loop_statement_30 SEMICOLON arg32 = expression SEMICOLON arg34 = variable_assignment arg35 = loop_statement_35 RPAREN arg37 = statement_or_null { Globals.grdbg "loop_statement_38" (UNDECUPLE(FOR, LPAREN, arg29, arg30, SEMICOLON, arg32, SEMICOLON, arg34, arg35, RPAREN, arg37);) }
	| DO arg40 = statement_or_null WHILE LPAREN arg43 = expression RPAREN SEMICOLON { Globals.grdbg "loop_statement" (SEPTUPLE(DO, arg40, WHILE, LPAREN, arg43, RPAREN, SEMICOLON);) }
	;

%inline system_function_call : arg2 = SYSTEM_FUNCTION_IDENTIFIER arg3 = system_function_call_3 { Globals.grdbg "system_function_call" (DOUBLE(SYSTEM_FUNCTION_IDENTIFIER arg2, arg3);) }
	;

modport_tf_port : TASK arg3 = named_task_proto arg4 = modport_tf_port_4 { Globals.grdbg "modport_tf_port_5" (TRIPLE(TASK, arg3, arg4);) }
	| FUNCTION arg7 = named_function_proto arg8 = modport_tf_port_8 { Globals.grdbg "modport_tf_port_9" (TRIPLE(FUNCTION, arg7, arg8);) }
	| arg10 = task_or_function_identifier arg11 = modport_tf_port_11 { Globals.grdbg "modport_tf_port" (DOUBLE(arg10, arg11);) }
	;

%inline hierarchical_event_identifier : arg2 = hierarchical_identifier { Globals.grdbg "hierarchical_event_identifier" ((arg2);) }
	;

%inline udp_declaration_port_list : arg2 = udp_output_declaration COMMA arg4 = udp_input_declaration arg5 = udp_declaration_port_list_5 { Globals.grdbg "udp_declaration_port_list" (QUADRUPLE(arg2, COMMA, arg4, arg5);) }
	;

n_output_gatetype : BUF { Globals.grdbg "n_output_gatetype_3" ((BUF);) }
	| NOT { Globals.grdbg "n_output_gatetype" ((NOT);) }
	;

signing : SIGNED { Globals.grdbg "signing_3" ((SIGNED);) }
	| UNSIGNED { Globals.grdbg "signing" ((UNSIGNED);) }
	;

data_declaration : arg2 = variable_declaration { Globals.grdbg "data_declaration_3" ((arg2);) }
	| arg4 = constant_declaration { Globals.grdbg "data_declaration_5" ((arg4);) }
	| arg6 = type_declaration { Globals.grdbg "data_declaration" ((arg6);) }
	;

%inline latch_construct : ALWAYS_LATCH arg3 = statement { Globals.grdbg "latch_construct" (DOUBLE(ALWAYS_LATCH, arg3);) }
	;

generate_module_named_block : BEGIN COLON arg4 = identifier arg5 = generate_module_named_block_5 END arg7 = generate_module_named_block_7 { Globals.grdbg "generate_module_named_block_8" (SEXTUPLE(BEGIN, COLON, arg4, arg5, END, arg7);) }
	| arg9 = identifier COLON arg11 = generate_module_block { Globals.grdbg "generate_module_named_block" (TRIPLE(arg9, COLON, arg11);) }
	;

%inline hierarchical_task_identifier : arg2 = hierarchical_identifier { Globals.grdbg "hierarchical_task_identifier" ((arg2);) }
	;

%inline generate_module_loop_statement : FOR LPAREN arg4 = genvar_decl_assignment SEMICOLON arg6 = constant_expression SEMICOLON arg8 = genvar_assignment RPAREN arg10 = generate_module_named_block { Globals.grdbg "generate_module_loop_statement" (NONUPLE(FOR, LPAREN, arg4, SEMICOLON, arg6, SEMICOLON, arg8, RPAREN, arg10);) }
	;

non_zero_decimal_digit : TOKEN_ONE { Globals.grdbg "non_zero_decimal_digit_3" ((TOKEN_ONE);) }
	| TOKEN_TWO { Globals.grdbg "non_zero_decimal_digit_5" ((TOKEN_TWO);) }
	| TOKEN_THREE { Globals.grdbg "non_zero_decimal_digit_7" ((TOKEN_THREE);) }
	| TOKEN_FOUR { Globals.grdbg "non_zero_decimal_digit_9" ((TOKEN_FOUR);) }
	| TOKEN_FIVE { Globals.grdbg "non_zero_decimal_digit_11" ((TOKEN_FIVE);) }
	| TOKEN_SIX { Globals.grdbg "non_zero_decimal_digit_13" ((TOKEN_SIX);) }
	| TOKEN_SEVEN { Globals.grdbg "non_zero_decimal_digit_15" ((TOKEN_SEVEN);) }
	| TOKEN_EIGHT { Globals.grdbg "non_zero_decimal_digit_17" ((TOKEN_EIGHT);) }
	| TOKEN_NINE { Globals.grdbg "non_zero_decimal_digit" ((TOKEN_NINE);) }
	;

%inline tfall_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "tfall_path_delay_expression" ((arg2);) }
	;

%inline sequence_formal_list : LPAREN arg3 = formal_list_item arg4 = sequence_formal_list_4 RPAREN { Globals.grdbg "sequence_formal_list" (QUADRUPLE(LPAREN, arg3, arg4, RPAREN);) }
	;

clocking_direction : INPUT arg3 = clocking_direction_3 { Globals.grdbg "clocking_direction_4" (DOUBLE(INPUT, arg3);) }
	| OUTPUT arg6 = clocking_direction_6 { Globals.grdbg "clocking_direction_7" (DOUBLE(OUTPUT, arg6);) }
	| INPUT arg9 = clocking_direction_9 OUTPUT arg11 = clocking_direction_11 { Globals.grdbg "clocking_direction_12" (QUADRUPLE(INPUT, arg9, OUTPUT, arg11);) }
	| INOUT { Globals.grdbg "clocking_direction" ((INOUT);) }
	;

interface_port_declaration : arg2 = identifier arg3 = list_of_interface_identifiers { Globals.grdbg "interface_port_declaration_4" (DOUBLE(arg2, arg3);) }
	| arg5 = identifier DOT arg7 = identifier arg8 = list_of_interface_identifiers { Globals.grdbg "interface_port_declaration" (QUADRUPLE(arg5, DOT, arg7, arg8);) }
	;

%inline d_period_timing_check : D_PERIOD LPAREN arg4 = controlled_timing_check_event COMMA arg6 = expression arg7 = d_period_timing_check_7 RPAREN SEMICOLON { Globals.grdbg "d_period_timing_check" (OCTUPLE(D_PERIOD, LPAREN, arg4, COMMA, arg6, arg7, RPAREN, SEMICOLON);) }
	;

%inline module_identifier : arg2 = identifier { Globals.grdbg "module_identifier" ((arg2);) }
	;

%inline class_identifier : arg2 = identifier { Globals.grdbg "class_identifier" ((arg2);) }
	;

function_loop_statement : FOREVER arg3 = function_statement_or_null { Globals.grdbg "function_loop_statement_4" (DOUBLE(FOREVER, arg3);) }
	| REPEAT LPAREN arg7 = expression RPAREN arg9 = function_statement_or_null { Globals.grdbg "function_loop_statement_10" (QUINTUPLE(REPEAT, LPAREN, arg7, RPAREN, arg9);) }
	| WHILE LPAREN arg13 = expression RPAREN arg15 = function_statement_or_null { Globals.grdbg "function_loop_statement_16" (QUINTUPLE(WHILE, LPAREN, arg13, RPAREN, arg15);) }
	| FOR LPAREN arg19 = variable_decl_or_assignment arg20 = function_loop_statement_20 SEMICOLON arg22 = expression SEMICOLON arg24 = variable_assignment arg25 = function_loop_statement_25 RPAREN arg27 = function_statement_or_null { Globals.grdbg "function_loop_statement_28" (UNDECUPLE(FOR, LPAREN, arg19, arg20, SEMICOLON, arg22, SEMICOLON, arg24, arg25, RPAREN, arg27);) }
	| DO arg30 = function_statement_or_null WHILE LPAREN arg33 = expression RPAREN SEMICOLON { Globals.grdbg "function_loop_statement" (SEPTUPLE(DO, arg30, WHILE, LPAREN, arg33, RPAREN, SEMICOLON);) }
	;

%inline controlled_reference_event : arg2 = controlled_timing_check_event { Globals.grdbg "controlled_reference_event" ((arg2);) }
	;

module_keyword : MODULE { Globals.grdbg "module_keyword_3" ((MODULE);) }
	| MACROMODULE { Globals.grdbg "module_keyword" ((MACROMODULE);) }
	;

jump_statement : RETURN arg3 = jump_statement_3 SEMICOLON { Globals.grdbg "jump_statement_5" (TRIPLE(RETURN, arg3, SEMICOLON);) }
	| BREAK SEMICOLON { Globals.grdbg "jump_statement_8" (DOUBLE(BREAK, SEMICOLON);) }
	| CONTINUE SEMICOLON { Globals.grdbg "jump_statement" (DOUBLE(CONTINUE, SEMICOLON);) }
	;

list_of_variable_identifiers_or_assignments : arg2 = list_of_variable_decl_assignments { Globals.grdbg "list_of_variable_identifiers_or_assignments_3" ((arg2);) }
	| arg4 = list_of_variable_identifiers { Globals.grdbg "list_of_variable_identifiers_or_assignments" ((arg4);) }
	;

%inline module_path_multiple_concatenation : LCURLY arg3 = constant_expression arg4 = module_path_concatenation RCURLY { Globals.grdbg "module_path_multiple_concatenation" (QUADRUPLE(LCURLY, arg3, arg4, RCURLY);) }
	;

%inline program_identifier : arg2 = identifier { Globals.grdbg "program_identifier" ((arg2);) }
	;

%inline attr_name : arg2 = identifier { Globals.grdbg "attr_name" ((arg2);) }
	;

pulse_control_specparam : PATHPULSE EQUALS LPAREN arg5 = limit_value arg6 = pulse_control_specparam_6 RPAREN SEMICOLON { Globals.grdbg "pulse_control_specparam_9" (SEPTUPLE(PATHPULSE, EQUALS, LPAREN, arg5, arg6, RPAREN, SEMICOLON);) }
	| PATHPULSE arg11 = specify_input_terminal_descriptor DOLLAR arg13 = specify_output_terminal_descriptor EQUALS LPAREN arg16 = limit_value arg17 = pulse_control_specparam_17 RPAREN SEMICOLON { Globals.grdbg "pulse_control_specparam" (DECUPLE(PATHPULSE, arg11, DOLLAR, arg13, EQUALS, LPAREN, arg16, arg17, RPAREN, SEMICOLON);) }
	;

module_or_generate_item : arg3 = parameter_override { Globals.grdbg "module_or_generate_item_4" ((arg3);) }
	| arg6 = continuous_assign { Globals.grdbg "module_or_generate_item_7" ((arg6);) }
	| arg9 = gate_instantiation { Globals.grdbg "module_or_generate_item_10" ((arg9);) }
	| arg12 = udp_instantiation { Globals.grdbg "module_or_generate_item_13" ((arg12);) }
	| arg15 = module_instantiation { Globals.grdbg "module_or_generate_item_16" ((arg15);) }
	| arg18 = initial_construct { Globals.grdbg "module_or_generate_item_19" ((arg18);) }
	| arg21 = always_construct { Globals.grdbg "module_or_generate_item_22" ((arg21);) }
	| arg24 = combinational_construct { Globals.grdbg "module_or_generate_item_25" ((arg24);) }
	| arg27 = latch_construct { Globals.grdbg "module_or_generate_item_28" ((arg27);) }
	| arg30 = ff_construct { Globals.grdbg "module_or_generate_item_31" ((arg30);) }
	| arg33 = net_alias { Globals.grdbg "module_or_generate_item_34" ((arg33);) }
	| arg36 = final_construct { Globals.grdbg "module_or_generate_item_37" ((arg36);) }
	| arg38 = module_common_item { Globals.grdbg "module_or_generate_item_39" ((arg38);) }
	| SEMICOLON { Globals.grdbg "module_or_generate_item" ((SEMICOLON);) }
	;

procedural_assertion_item : arg2 = assert_property_statement { Globals.grdbg "procedural_assertion_item_3" ((arg2);) }
	| arg4 = cover_property_statement { Globals.grdbg "procedural_assertion_item_5" ((arg4);) }
	| arg6 = immediate_assert_statement { Globals.grdbg "procedural_assertion_item" ((arg6);) }
	;

%inline controlled_timing_check_event : arg2 = timing_check_event_control arg3 = specify_terminal_descriptor arg4 = controlled_timing_check_event_4 { Globals.grdbg "controlled_timing_check_event" (TRIPLE(arg2, arg3, arg4);) }
	;

%inline named_task_proto : arg2 = identifier LPAREN arg4 = task_proto_formal arg5 = named_task_proto_5 RPAREN { Globals.grdbg "named_task_proto" (QUINTUPLE(arg2, LPAREN, arg4, arg5, RPAREN);) }
	;

%inline expression1 : arg2 = expression { Globals.grdbg "expression1" ((arg2);) }
	;

%inline expression2 : arg2 = expression { Globals.grdbg "expression2" ((arg2);) }
	;

%inline expression3 : arg2 = expression { Globals.grdbg "expression3" ((arg2);) }
	;

interface_declaration : arg2 = interface_nonansi_header arg3 = interface_declaration_3 arg4 = interface_declaration_4 ENDINTERFACE arg6 = interface_declaration_6 { Globals.grdbg "interface_declaration_7" (QUINTUPLE(arg2, arg3, arg4, ENDINTERFACE, arg6);) }
	| arg8 = interface_ansi_header arg9 = interface_declaration_9 arg10 = interface_declaration_10 ENDINTERFACE arg12 = interface_declaration_12 { Globals.grdbg "interface_declaration_13" (QUINTUPLE(arg8, arg9, arg10, ENDINTERFACE, arg12);) }
	| INTERFACE arg16 = identifier LPAREN P_DOTSTAR RPAREN SEMICOLON arg21 = interface_declaration_21 arg22 = interface_declaration_22 ENDINTERFACE arg24 = interface_declaration_24 { Globals.grdbg "interface_declaration_25" (DECUPLE(INTERFACE, arg16, LPAREN, P_DOTSTAR, RPAREN, SEMICOLON, arg21, arg22, ENDINTERFACE, arg24);) }
	| EXTERN arg27 = interface_nonansi_header { Globals.grdbg "interface_declaration_28" (DOUBLE(EXTERN, arg27);) }
	| EXTERN arg30 = interface_ansi_header { Globals.grdbg "interface_declaration" (DOUBLE(EXTERN, arg30);) }
	;

%inline consecutive_repetition : LBRACK_STAR arg3 = const_or_range_expression RBRACK { Globals.grdbg "consecutive_repetition" (TRIPLE(LBRACK_STAR, arg3, RBRACK);) }
	;

list_of_constant_arguments : arg2 = list_of_constant_arguments_2 arg3 = list_of_constant_arguments_3 { Globals.grdbg "list_of_constant_arguments_4" (DOUBLE(arg2, arg3);) }
	| DOT arg6 = identifier LPAREN arg8 = list_of_constant_arguments_8 RPAREN arg10 = list_of_constant_arguments_10 { Globals.grdbg "list_of_constant_arguments" (SEXTUPLE(DOT, arg6, LPAREN, arg8, RPAREN, arg10);) }
	;

interface_or_generate_item : arg3 = continuous_assign { Globals.grdbg "interface_or_generate_item_4" ((arg3);) }
	| arg6 = initial_construct { Globals.grdbg "interface_or_generate_item_7" ((arg6);) }
	| arg9 = always_construct { Globals.grdbg "interface_or_generate_item_10" ((arg9);) }
	| arg12 = combinational_construct { Globals.grdbg "interface_or_generate_item_13" ((arg12);) }
	| arg15 = latch_construct { Globals.grdbg "interface_or_generate_item_16" ((arg15);) }
	| arg18 = ff_construct { Globals.grdbg "interface_or_generate_item_19" ((arg18);) }
	| arg21 = local_parameter_declaration { Globals.grdbg "interface_or_generate_item_22" ((arg21);) }
	| arg24 = parameter_declaration SEMICOLON { Globals.grdbg "interface_or_generate_item_26" (DOUBLE(arg24, SEMICOLON);) }
	| arg27 = module_common_item { Globals.grdbg "interface_or_generate_item_28" ((arg27);) }
	| arg30 = modport_declaration { Globals.grdbg "interface_or_generate_item_31" ((arg30);) }
	| arg33 = extern_tf_declaration { Globals.grdbg "interface_or_generate_item_34" ((arg33);) }
	| arg36 = final_construct { Globals.grdbg "interface_or_generate_item_37" ((arg36);) }
	| SEMICOLON { Globals.grdbg "interface_or_generate_item" ((SEMICOLON);) }
	;

%inline range : LBRACK arg3 = constant_expression COLON arg5 = constant_expression RBRACK { Globals.grdbg "range" (QUINTUPLE(LBRACK, arg3, COLON, arg5, RBRACK);) }
	;

%inline port_identifier : arg2 = identifier { Globals.grdbg "port_identifier" ((arg2);) }
	;

%inline txz_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "txz_path_delay_expression" ((arg2);) }
	;

%inline sequence_declaration : SEQUENCE arg3 = identifier arg4 = sequence_declaration_4 SEMICOLON arg6 = sequence_declaration_6 arg7 = sequence_spec SEMICOLON ENDSEQUENCE arg10 = sequence_declaration_10 { Globals.grdbg "sequence_declaration" (NONUPLE(SEQUENCE, arg3, arg4, SEMICOLON, arg6, arg7, SEMICOLON, ENDSEQUENCE, arg10);) }
	;

%inline string_literal : DOUBLE_QUOTES arg3 = string_literal_3 DOUBLE_QUOTES { Globals.grdbg "string_literal" (TRIPLE(DOUBLE_QUOTES, arg3, DOUBLE_QUOTES);) }
	;

%inline name_of_instance : arg2 = identifier arg3 = name_of_instance_3 { Globals.grdbg "name_of_instance" (DOUBLE(arg2, arg3);) }
	;

%inline remain_active_flag : arg2 = constant_mintypmax_expression { Globals.grdbg "remain_active_flag" ((arg2);) }
	;

assert_property_statement : ASSERT PROPERTY LPAREN arg5 = property_spec RPAREN arg7 = action_block { Globals.grdbg "assert_property_statement_8" (SEXTUPLE(ASSERT, PROPERTY, LPAREN, arg5, RPAREN, arg7);) }
	| ASSERT PROPERTY LPAREN arg12 = property_instance RPAREN arg14 = action_block { Globals.grdbg "assert_property_statement" (SEXTUPLE(ASSERT, PROPERTY, LPAREN, arg12, RPAREN, arg14);) }
	;

pulsestyle_declaration : PULSESTYLE_ONEVENT arg3 = list_of_path_outputs SEMICOLON { Globals.grdbg "pulsestyle_declaration_5" (TRIPLE(PULSESTYLE_ONEVENT, arg3, SEMICOLON);) }
	| PULSESTYLE_ONDETECT arg7 = list_of_path_outputs SEMICOLON { Globals.grdbg "pulsestyle_declaration" (TRIPLE(PULSESTYLE_ONDETECT, arg7, SEMICOLON);) }
	;

%inline list_of_ports : LPAREN arg3 = port arg4 = list_of_ports_4 RPAREN { Globals.grdbg "list_of_ports" (QUADRUPLE(LPAREN, arg3, arg4, RPAREN);) }
	;

%inline event_identifier : arg2 = identifier { Globals.grdbg "event_identifier" ((arg2);) }
	;

hierarchical_identifier : arg2 = simple_hierarchical_identifier { Globals.grdbg "hierarchical_identifier_3" ((arg2);) }
	| arg4 = escaped_hierarchical_identifier { Globals.grdbg "hierarchical_identifier" ((arg4);) }
	;

clocking_drive : arg2 = clockvar_expression P_LTE arg4 = clocking_drive_4 arg5 = expression { Globals.grdbg "clocking_drive_6" (QUADRUPLE(arg2, P_LTE, arg4, arg5);) }
	| arg7 = cycle_delay arg8 = clockvar_expression P_LTE arg10 = expression { Globals.grdbg "clocking_drive" (QUADRUPLE(arg7, arg8, P_LTE, arg10);) }
	;

unary_operator : PLUS { Globals.grdbg "unary_operator_3" ((PLUS);) }
	| MINUS { Globals.grdbg "unary_operator_5" ((MINUS);) }
	| PLING { Globals.grdbg "unary_operator_7" ((PLING);) }
	| TILDE { Globals.grdbg "unary_operator_9" ((TILDE);) }
	| AMPERSAND { Globals.grdbg "unary_operator_11" ((AMPERSAND);) }
	| P_NAND { Globals.grdbg "unary_operator_13" ((P_NAND);) }
	| VBAR { Globals.grdbg "unary_operator_15" ((VBAR);) }
	| TILDE_VBAR { Globals.grdbg "unary_operator_17" ((TILDE_VBAR);) }
	| CARET { Globals.grdbg "unary_operator_19" ((CARET);) }
	| P_NXOR { Globals.grdbg "unary_operator_21" ((P_NXOR);) }
	| P_XNOR { Globals.grdbg "unary_operator" ((P_XNOR);) }
	;

%inline goto_repetition : LBRACK_STAR_DASH_GT arg3 = const_or_range_expression RBRACK { Globals.grdbg "goto_repetition" (TRIPLE(LBRACK_STAR_DASH_GT, arg3, RBRACK);) }
	;

%inline t1z_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "t1z_path_delay_expression" ((arg2);) }
	;

%inline timing_check_event : arg2 = timing_check_event_2 arg3 = specify_terminal_descriptor arg4 = timing_check_event_4 { Globals.grdbg "timing_check_event" (TRIPLE(arg2, arg3, arg4);) }
	;

%inline par_block : FORK arg3 = par_block_3 arg4 = par_block_4 arg5 = par_block_5 arg6 = join_keyword arg7 = par_block_7 { Globals.grdbg "par_block" (SEXTUPLE(FORK, arg3, arg4, arg5, arg6, arg7);) }
	;

%inline t0z_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "t0z_path_delay_expression" ((arg2);) }
	;

%inline design_statement : DESIGN arg3 = design_statement_3 SEMICOLON { Globals.grdbg "design_statement" (TRIPLE(DESIGN, arg3, SEMICOLON);) }
	;

%inline ref_declaration : REF arg3 = data_type arg4 = list_of_port_identifiers { Globals.grdbg "ref_declaration" (TRIPLE(REF, arg3, arg4);) }
	;

%inline list_of_net_decl_assignments : arg2 = net_decl_assignment arg3 = list_of_net_decl_assignments_3 { Globals.grdbg "list_of_net_decl_assignments" (DOUBLE(arg2, arg3);) }
	;

event_trigger : P_MINUSGT arg3 = hierarchical_identifier SEMICOLON { Globals.grdbg "event_trigger_5" (TRIPLE(P_MINUSGT, arg3, SEMICOLON);) }
	| DASH_GT_GT arg7 = event_trigger_7 arg8 = hierarchical_identifier SEMICOLON { Globals.grdbg "event_trigger" (QUADRUPLE(DASH_GT_GT, arg7, arg8, SEMICOLON);) }
	;

unique_priority : UNIQUE { Globals.grdbg "unique_priority_3" ((UNIQUE);) }
	| PRIORITY { Globals.grdbg "unique_priority" ((PRIORITY);) }
	;

%inline formal_list_item : arg2 = identifier arg3 = formal_list_item_3 { Globals.grdbg "formal_list_item" (DOUBLE(arg2, arg3);) }
	;

case_statement : arg2 = case_statement_2 CASE LPAREN arg5 = expression RPAREN arg7 = case_item arg8 = case_statement_8 ENDCASE { Globals.grdbg "case_statement_10" (OCTUPLE(arg2, CASE, LPAREN, arg5, RPAREN, arg7, arg8, ENDCASE);) }
	| arg11 = case_statement_11 CASEZ LPAREN arg14 = expression RPAREN arg16 = case_item arg17 = case_statement_17 ENDCASE { Globals.grdbg "case_statement_19" (OCTUPLE(arg11, CASEZ, LPAREN, arg14, RPAREN, arg16, arg17, ENDCASE);) }
	| arg20 = case_statement_20 CASEX LPAREN arg23 = expression RPAREN arg25 = case_item arg26 = case_statement_26 ENDCASE { Globals.grdbg "case_statement" (OCTUPLE(arg20, CASEX, LPAREN, arg23, RPAREN, arg25, arg26, ENDCASE);) }
	;

%inline clocking_identifier : arg2 = identifier { Globals.grdbg "clocking_identifier" ((arg2);) }
	;

concatenation : LCURLY arg3 = expression arg4 = concatenation_4 RCURLY { Globals.grdbg "concatenation_6" (QUADRUPLE(LCURLY, arg3, arg4, RCURLY);) }
	| LCURLY arg8 = struct_member_label COLON arg10 = expression arg11 = concatenation_11 RCURLY { Globals.grdbg "concatenation_13" (SEXTUPLE(LCURLY, arg8, COLON, arg10, arg11, RCURLY);) }
	| LCURLY arg15 = array_member_label COLON arg17 = expression arg18 = concatenation_18 RCURLY { Globals.grdbg "concatenation" (SEXTUPLE(LCURLY, arg15, COLON, arg17, arg18, RCURLY);) }
	;

class_scope_type_identifier : arg2 = identifier P_COLONCOLON arg4 = class_scope_type_identifier_4 arg5 = type_declaration_identifier { Globals.grdbg "class_scope_type_identifier_6" (QUADRUPLE(arg2, P_COLONCOLON, arg4, arg5);) }
	| arg7 = identifier P_COLONCOLON arg9 = class_scope_type_identifier_9 arg10 = identifier { Globals.grdbg "class_scope_type_identifier" (QUADRUPLE(arg7, P_COLONCOLON, arg9, arg10);) }
	;

%inline hierarchical_net_identifier : arg2 = hierarchical_identifier { Globals.grdbg "hierarchical_net_identifier" ((arg2);) }
	;

clocking_item : DEFAULT arg3 = default_skew SEMICOLON { Globals.grdbg "clocking_item_5" (TRIPLE(DEFAULT, arg3, SEMICOLON);) }
	| arg6 = clocking_direction arg7 = list_of_clocking_decl_assign SEMICOLON { Globals.grdbg "clocking_item_9" (TRIPLE(arg6, arg7, SEMICOLON);) }
	| arg11 = concurrent_assertion_item_declaration { Globals.grdbg "clocking_item" ((arg11);) }
	;

%inline generate_module_case_statement : CASE LPAREN arg4 = constant_expression RPAREN arg6 = genvar_module_case_item arg7 = generate_module_case_statement_7 ENDCASE { Globals.grdbg "generate_module_case_statement" (SEPTUPLE(CASE, LPAREN, arg4, RPAREN, arg6, arg7, ENDCASE);) }
	;

%inline cmos_switch_instance : arg2 = cmos_switch_instance_2 LPAREN arg4 = net_lvalue COMMA arg6 = expression COMMA arg8 = expression COMMA arg10 = expression RPAREN { Globals.grdbg "cmos_switch_instance" (DECUPLE(arg2, LPAREN, arg4, COMMA, arg6, COMMA, arg8, COMMA, arg10, RPAREN);) }
	;

%inline program_instantiation : arg2 = identifier arg3 = program_instantiation_3 arg4 = program_instance arg5 = program_instantiation_5 SEMICOLON { Globals.grdbg "program_instantiation" (QUINTUPLE(arg2, arg3, arg4, arg5, SEMICOLON);) }
	;

%inline task_enable : arg2 = hierarchical_identifier arg3 = task_enable_3 SEMICOLON { Globals.grdbg "task_enable" (TRIPLE(arg2, arg3, SEMICOLON);) }
	;

%inline library_identifier : arg2 = identifier { Globals.grdbg "library_identifier" ((arg2);) }
	;

inc_or_dec_operator : TOKEN_PLUS_PLUS { Globals.grdbg "inc_or_dec_operator_3" ((TOKEN_PLUS_PLUS);) }
	| TOKEN_DASH_DASH { Globals.grdbg "inc_or_dec_operator" ((TOKEN_DASH_DASH);) }
	;

%inline hex_value : arg2 = HEX_DIGIT arg3 = hex_value_3 { Globals.grdbg "hex_value" (DOUBLE(HEX_DIGIT arg2, arg3);) }
	;

boolean_abbrev : arg2 = consecutive_repetition { Globals.grdbg "boolean_abbrev_3" ((arg2);) }
	| arg4 = non_consecutive_repetition { Globals.grdbg "boolean_abbrev_5" ((arg4);) }
	| arg6 = goto_repetition { Globals.grdbg "boolean_abbrev" ((arg6);) }
	;

n_input_gatetype : AND { Globals.grdbg "n_input_gatetype_3" ((AND);) }
	| NAND { Globals.grdbg "n_input_gatetype_5" ((NAND);) }
	| OR { Globals.grdbg "n_input_gatetype_7" ((OR);) }
	| NOR { Globals.grdbg "n_input_gatetype_9" ((NOR);) }
	| XOR { Globals.grdbg "n_input_gatetype_11" ((XOR);) }
	| XNOR { Globals.grdbg "n_input_gatetype" ((XNOR);) }
	;

tf_input_declaration : INPUT arg3 = tf_input_declaration_3 arg4 = tf_input_declaration_4 arg5 = list_of_tf_port_identifiers { Globals.grdbg "tf_input_declaration_6" (QUADRUPLE(INPUT, arg3, arg4, arg5);) }
	| INPUT arg8 = tf_data_type arg9 = list_of_tf_variable_identifiers { Globals.grdbg "tf_input_declaration" (TRIPLE(INPUT, arg8, arg9);) }
	;

constant_mintypmax_expression : arg2 = constant_expression { Globals.grdbg "constant_mintypmax_expression_3" ((arg2);) }
	| arg4 = constant_expression COLON arg6 = constant_expression COLON arg8 = constant_expression { Globals.grdbg "constant_mintypmax_expression" (QUINTUPLE(arg4, COLON, arg6, COLON, arg8);) }
	;

%inline tz_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "tz_path_delay_expression" ((arg2);) }
	;

%inline property_identifier : arg2 = identifier { Globals.grdbg "property_identifier" ((arg2);) }
	;

%inline specify_output_terminal_descriptor : arg2 = output_identifier arg3 = specify_output_terminal_descriptor_3 { Globals.grdbg "specify_output_terminal_descriptor" (DOUBLE(arg2, arg3);) }
	;

%inline library_text : arg2 = library_text_2 { Globals.grdbg "library_text" ((arg2);) }
	;

function_data_type : arg2 = integer_vector_type arg3 = function_data_type_3 arg4 = function_data_type_4 { Globals.grdbg "function_data_type_5" (TRIPLE(arg2, arg3, arg4);) }
	| arg6 = integer_atom_type { Globals.grdbg "function_data_type_7" ((arg6);) }
	| arg8 = type_declaration_identifier arg9 = function_data_type_9 { Globals.grdbg "function_data_type_10" (DOUBLE(arg8, arg9);) }
	| arg11 = non_integer_type { Globals.grdbg "function_data_type_12" ((arg11);) }
	| STRUCT arg14 = function_data_type_14 LCURLY arg16 = function_data_type_16 RCURLY arg18 = function_data_type_18 { Globals.grdbg "function_data_type_19" (SEXTUPLE(STRUCT, arg14, LCURLY, arg16, RCURLY, arg18);) }
	| UNION arg21 = function_data_type_21 LCURLY arg23 = function_data_type_23 RCURLY arg25 = function_data_type_25 { Globals.grdbg "function_data_type_26" (SEXTUPLE(UNION, arg21, LCURLY, arg23, RCURLY, arg25);) }
	| ENUM arg28 = function_data_type_28 LCURLY arg30 = identifier arg31 = function_data_type_31 arg32 = function_data_type_32 RCURLY { Globals.grdbg "function_data_type_34" (SEPTUPLE(ENUM, arg28, LCURLY, arg30, arg31, arg32, RCURLY);) }
	| STRING { Globals.grdbg "function_data_type_36" ((STRING);) }
	| CHANDLE { Globals.grdbg "function_data_type_38" ((CHANDLE);) }
	| VOID { Globals.grdbg "function_data_type" ((VOID);) }
	;

sequence_expr : arg2 = cycle_delay_range arg3 = sequence_expr arg4 = sequence_expr_4 { Globals.grdbg "sequence_expr_5" (TRIPLE(arg2, arg3, arg4);) }
	| arg6 = sequence_expr arg7 = cycle_delay_range arg8 = sequence_expr arg9 = sequence_expr_9 { Globals.grdbg "sequence_expr_10" (QUADRUPLE(arg6, arg7, arg8, arg9);) }
	| arg11 = expression arg12 = sequence_expr_12 arg13 = sequence_expr_13 { Globals.grdbg "sequence_expr_14" (TRIPLE(arg11, arg12, arg13);) }
	| LPAREN arg16 = expression arg17 = sequence_expr_17 RPAREN arg19 = sequence_expr_19 { Globals.grdbg "sequence_expr_20" (QUINTUPLE(LPAREN, arg16, arg17, RPAREN, arg19);) }
	| arg21 = sequence_instance arg22 = sequence_expr_22 { Globals.grdbg "sequence_expr_23" (DOUBLE(arg21, arg22);) }
	| LPAREN arg25 = sequence_expr RPAREN arg27 = sequence_expr_27 { Globals.grdbg "sequence_expr_28" (QUADRUPLE(LPAREN, arg25, RPAREN, arg27);) }
	| arg29 = sequence_expr AND arg31 = sequence_expr { Globals.grdbg "sequence_expr_32" (TRIPLE(arg29, AND, arg31);) }
	| arg33 = sequence_expr INTERSECT arg35 = sequence_expr { Globals.grdbg "sequence_expr_36" (TRIPLE(arg33, INTERSECT, arg35);) }
	| arg37 = sequence_expr OR arg39 = sequence_expr { Globals.grdbg "sequence_expr_40" (TRIPLE(arg37, OR, arg39);) }
	| FIRST_MATCH LPAREN arg43 = sequence_expr RPAREN { Globals.grdbg "sequence_expr_45" (QUADRUPLE(FIRST_MATCH, LPAREN, arg43, RPAREN);) }
	| arg46 = expression THROUGHOUT arg48 = sequence_expr { Globals.grdbg "sequence_expr_49" (TRIPLE(arg46, THROUGHOUT, arg48);) }
	| arg50 = sequence_expr WITHIN arg52 = sequence_expr { Globals.grdbg "sequence_expr" (TRIPLE(arg50, WITHIN, arg52);) }
	;

%inline udp_port_list : arg2 = identifier COMMA arg4 = identifier arg5 = udp_port_list_5 { Globals.grdbg "udp_port_list" (QUADRUPLE(arg2, COMMA, arg4, arg5);) }
	;

constant_expression : arg2 = constant_primary { Globals.grdbg "constant_expression_3" ((arg2);) }
	| arg4 = unary_operator arg6 = constant_primary { Globals.grdbg "constant_expression_7" (DOUBLE(arg4, arg6);) }
	| arg8 = constant_expression arg9 = binary_operator arg11 = constant_expression { Globals.grdbg "constant_expression_12" (TRIPLE(arg8, arg9, arg11);) }
	| arg13 = constant_expression QUERY arg16 = constant_expression COLON arg18 = constant_expression { Globals.grdbg "constant_expression_19" (QUINTUPLE(arg13, QUERY, arg16, COLON, arg18);) }
	| arg20 = string_literal { Globals.grdbg "constant_expression" ((arg20);) }
	;

function_case_statement : arg2 = function_case_statement_2 CASE LPAREN arg5 = expression RPAREN arg7 = function_case_item arg8 = function_case_statement_8 ENDCASE { Globals.grdbg "function_case_statement_10" (OCTUPLE(arg2, CASE, LPAREN, arg5, RPAREN, arg7, arg8, ENDCASE);) }
	| arg11 = function_case_statement_11 CASEZ LPAREN arg14 = expression RPAREN arg16 = function_case_item arg17 = function_case_statement_17 ENDCASE { Globals.grdbg "function_case_statement_19" (OCTUPLE(arg11, CASEZ, LPAREN, arg14, RPAREN, arg16, arg17, ENDCASE);) }
	| arg20 = function_case_statement_20 CASEX LPAREN arg23 = expression RPAREN arg25 = function_case_item arg26 = function_case_statement_26 ENDCASE { Globals.grdbg "function_case_statement" (OCTUPLE(arg20, CASEX, LPAREN, arg23, RPAREN, arg25, arg26, ENDCASE);) }
	;

multi_clock_property_expr : arg2 = multi_clock_sequence { Globals.grdbg "multi_clock_property_expr_3" ((arg2);) }
	| arg4 = multi_clock_sequence TOKEN_PLING_EQUALS_GT arg6 = multi_clock_property_expr_6 arg7 = multi_clock_sequence { Globals.grdbg "multi_clock_property_expr_8" (QUADRUPLE(arg4, TOKEN_PLING_EQUALS_GT, arg6, arg7);) }
	| LPAREN arg10 = multi_clock_property_expr RPAREN { Globals.grdbg "multi_clock_property_expr" (TRIPLE(LPAREN, arg10, RPAREN);) }
	;

%inline mos_switch_instance : arg2 = mos_switch_instance_2 LPAREN arg4 = net_lvalue COMMA arg6 = expression COMMA arg8 = expression RPAREN { Globals.grdbg "mos_switch_instance" (OCTUPLE(arg2, LPAREN, arg4, COMMA, arg6, COMMA, arg8, RPAREN);) }
	;

%inline binary_number : arg2 = binary_number_2 arg3 = BINARY_BASE arg4 = binary_value { Globals.grdbg "binary_number" (TRIPLE(arg2, BINARY_BASE arg3, arg4);) }
	;

attr_spec : arg2 = identifier EQUALS arg4 = constant_expression { Globals.grdbg "attr_spec_5" (TRIPLE(arg2, EQUALS, arg4);) }
	| arg6 = identifier { Globals.grdbg "attr_spec" ((arg6);) }
	;

non_integer_type : TIME { Globals.grdbg "non_integer_type_3" ((TIME);) }
	| SHORTREAL { Globals.grdbg "non_integer_type_5" ((SHORTREAL);) }
	| REAL { Globals.grdbg "non_integer_type_7" ((REAL);) }
	| REALTIME { Globals.grdbg "non_integer_type" ((REALTIME);) }
	;

polarity_operator : PLUS { Globals.grdbg "polarity_operator_3" ((PLUS);) }
	| MINUS { Globals.grdbg "polarity_operator" ((MINUS);) }
	;

%inline data_source_expression : arg2 = expression { Globals.grdbg "data_source_expression" ((arg2);) }
	;

%inline module_path_concatenation : LCURLY arg3 = module_path_expression arg4 = module_path_concatenation_4 RCURLY { Globals.grdbg "module_path_concatenation" (QUADRUPLE(LCURLY, arg3, arg4, RCURLY);) }
	;

%inline hierarchical_function_identifier : arg2 = hierarchical_identifier { Globals.grdbg "hierarchical_function_identifier" ((arg2);) }
	;

%inline udp_nonansi_declaration : PRIMITIVE arg4 = identifier LPAREN arg6 = udp_port_list RPAREN SEMICOLON { Globals.grdbg "udp_nonansi_declaration" (SEXTUPLE(PRIMITIVE, arg4, LPAREN, arg6, RPAREN, SEMICOLON);) }
	;

module_path_primary : arg2 = number { Globals.grdbg "module_path_primary_3" ((arg2);) }
	| arg4 = identifier { Globals.grdbg "module_path_primary_5" ((arg4);) }
	| arg6 = module_path_concatenation { Globals.grdbg "module_path_primary_7" ((arg6);) }
	| arg8 = module_path_multiple_concatenation { Globals.grdbg "module_path_primary_9" ((arg8);) }
	| arg10 = function_call { Globals.grdbg "module_path_primary_11" ((arg10);) }
	| arg12 = system_function_call { Globals.grdbg "module_path_primary_13" ((arg12);) }
	| arg14 = constant_function_call { Globals.grdbg "module_path_primary_15" ((arg14);) }
	| LPAREN arg17 = module_path_mintypmax_expression RPAREN { Globals.grdbg "module_path_primary" (TRIPLE(LPAREN, arg17, RPAREN);) }
	;

binary_operator : PLUS { Globals.grdbg "binary_operator_3" ((PLUS);) }
	| MINUS { Globals.grdbg "binary_operator_5" ((MINUS);) }
	| TIMES { Globals.grdbg "binary_operator_7" ((TIMES);) }
	| DIVIDE { Globals.grdbg "binary_operator_9" ((DIVIDE);) }
	| MODULO { Globals.grdbg "binary_operator_11" ((MODULO);) }
	| P_EQUAL { Globals.grdbg "binary_operator_13" ((P_EQUAL);) }
	| P_NOTEQUAL { Globals.grdbg "binary_operator_15" ((P_NOTEQUAL);) }
	| P_CASEEQUAL { Globals.grdbg "binary_operator_17" ((P_CASEEQUAL);) }
	| P_CASENOTEQUAL { Globals.grdbg "binary_operator_19" ((P_CASENOTEQUAL);) }
	| EQUALS_QUERY_EQUALS { Globals.grdbg "binary_operator_21" ((EQUALS_QUERY_EQUALS);) }
	| TOKEN_PLING_QUERY_EQUALS { Globals.grdbg "binary_operator_23" ((TOKEN_PLING_QUERY_EQUALS);) }
	| P_ANDAND { Globals.grdbg "binary_operator_25" ((P_ANDAND);) }
	| TOKEN_VBAR_VBAR { Globals.grdbg "binary_operator_27" ((TOKEN_VBAR_VBAR);) }
	| P_POW { Globals.grdbg "binary_operator_29" ((P_POW);) }
	| LESS { Globals.grdbg "binary_operator_31" ((LESS);) }
	| P_LTE { Globals.grdbg "binary_operator_33" ((P_LTE);) }
	| GREATER { Globals.grdbg "binary_operator_35" ((GREATER);) }
	| P_GTE { Globals.grdbg "binary_operator_37" ((P_GTE);) }
	| AMPERSAND { Globals.grdbg "binary_operator_39" ((AMPERSAND);) }
	| VBAR { Globals.grdbg "binary_operator_41" ((VBAR);) }
	| CARET { Globals.grdbg "binary_operator_43" ((CARET);) }
	| P_XNOR { Globals.grdbg "binary_operator_45" ((P_XNOR);) }
	| P_NXOR { Globals.grdbg "binary_operator_47" ((P_NXOR);) }
	| P_SRIGHT { Globals.grdbg "binary_operator_49" ((P_SRIGHT);) }
	| P_SLEFT { Globals.grdbg "binary_operator_51" ((P_SLEFT);) }
	| P_SSRIGHT3 { Globals.grdbg "binary_operator_53" ((P_SSRIGHT3);) }
	| P_SLEFT3 { Globals.grdbg "binary_operator" ((P_SLEFT3);) }
	;

identifier : arg2 = SIMPLE_IDENTIFIER { Globals.grdbg "identifier_3" ((SIMPLE_IDENTIFIER arg2);) }
	| arg4 = escaped_identifier { Globals.grdbg "identifier" ((arg4);) }
	;

genvar_assignment : arg2 = identifier arg3 = assignment_operator arg4 = constant_expression { Globals.grdbg "genvar_assignment_5" (TRIPLE(arg2, arg3, arg4);) }
	| arg6 = inc_or_dec_operator arg7 = identifier { Globals.grdbg "genvar_assignment_8" (DOUBLE(arg6, arg7);) }
	| arg9 = identifier arg10 = inc_or_dec_operator { Globals.grdbg "genvar_assignment" (DOUBLE(arg9, arg10);) }
	;

%inline generated_module_instantiation : GENERATE arg3 = generated_module_instantiation_3 ENDGENERATE { Globals.grdbg "generated_module_instantiation" (TRIPLE(GENERATE, arg3, ENDGENERATE);) }
	;

%inline cell_identifier : arg2 = identifier { Globals.grdbg "cell_identifier" ((arg2);) }
	;

%inline function_identifier : arg2 = identifier { Globals.grdbg "function_identifier" ((arg2);) }
	;

continuous_assign : ASSIGN arg3 = continuous_assign_3 arg4 = continuous_assign_4 arg5 = list_of_net_assignments SEMICOLON { Globals.grdbg "continuous_assign_7" (QUINTUPLE(ASSIGN, arg3, arg4, arg5, SEMICOLON);) }
	| ASSIGN arg9 = continuous_assign_9 arg10 = list_of_variable_assignments SEMICOLON { Globals.grdbg "continuous_assign" (QUADRUPLE(ASSIGN, arg9, arg10, SEMICOLON);) }
	;

%inline type_identifier : arg2 = identifier { Globals.grdbg "type_identifier" ((arg2);) }
	;

%inline module_instance : arg2 = name_of_instance LPAREN arg4 = module_instance_4 RPAREN { Globals.grdbg "module_instance" (QUADRUPLE(arg2, LPAREN, arg4, RPAREN);) }
	;

%inline liblist_clause : LIBLIST arg3 = liblist_clause_3 { Globals.grdbg "liblist_clause" (DOUBLE(LIBLIST, arg3);) }
	;

%inline parameter_value_assignment : HASH LPAREN arg4 = list_of_parameter_assignments RPAREN { Globals.grdbg "parameter_value_assignment" (QUADRUPLE(HASH, LPAREN, arg4, RPAREN);) }
	;

%inline function_statement : arg2 = function_statement_2 arg3 = function_statement_item { Globals.grdbg "function_statement" (DOUBLE(arg2, arg3);) }
	;

%inline operator_assignment : arg2 = variable_lvalue arg3 = assignment_operator arg4 = expression { Globals.grdbg "operator_assignment" (TRIPLE(arg2, arg3, arg4);) }
	;

task_proto_formal : arg2 = tf_input_declaration { Globals.grdbg "task_proto_formal_3" ((arg2);) }
	| arg4 = tf_output_declaration { Globals.grdbg "task_proto_formal_5" ((arg4);) }
	| arg6 = tf_inout_declaration { Globals.grdbg "task_proto_formal_7" ((arg6);) }
	| arg8 = tf_ref_declaration { Globals.grdbg "task_proto_formal" ((arg8);) }
	;

data_type : arg2 = integer_vector_type arg3 = data_type_3 arg4 = data_type_4 arg5 = data_type_5 { Globals.grdbg "data_type_6" (QUADRUPLE(arg2, arg3, arg4, arg5);) }
	| arg7 = integer_atom_type arg8 = data_type_8 { Globals.grdbg "data_type_9" (DOUBLE(arg7, arg8);) }
	| arg10 = type_declaration_identifier arg11 = data_type_11 { Globals.grdbg "data_type_12" (DOUBLE(arg10, arg11);) }
	| arg13 = non_integer_type { Globals.grdbg "data_type_14" ((arg13);) }
	| STRUCT PACKED arg17 = data_type_17 LCURLY arg19 = data_type_19 RCURLY arg21 = data_type_21 { Globals.grdbg "data_type_22" (SEPTUPLE(STRUCT, PACKED, arg17, LCURLY, arg19, RCURLY, arg21);) }
	| UNION PACKED arg25 = data_type_25 LCURLY arg27 = data_type_27 RCURLY arg29 = data_type_29 { Globals.grdbg "data_type_30" (SEPTUPLE(UNION, PACKED, arg25, LCURLY, arg27, RCURLY, arg29);) }
	| STRUCT arg32 = data_type_32 LCURLY arg34 = data_type_34 RCURLY { Globals.grdbg "data_type_36" (QUINTUPLE(STRUCT, arg32, LCURLY, arg34, RCURLY);) }
	| UNION arg38 = data_type_38 LCURLY arg40 = data_type_40 RCURLY { Globals.grdbg "data_type_42" (QUINTUPLE(UNION, arg38, LCURLY, arg40, RCURLY);) }
	| ENUM arg44 = data_type_44 LCURLY arg46 = identifier arg47 = data_type_47 arg48 = data_type_48 RCURLY { Globals.grdbg "data_type_50" (SEPTUPLE(ENUM, arg44, LCURLY, arg46, arg47, arg48, RCURLY);) }
	| STRING { Globals.grdbg "data_type_52" ((STRING);) }
	| EVENT { Globals.grdbg "data_type_54" ((EVENT);) }
	| CHANDLE { Globals.grdbg "data_type_56" ((CHANDLE);) }
	| arg57 = class_scope_type_identifier { Globals.grdbg "data_type" ((arg57);) }
	;

function_proto_formal : arg2 = tf_input_declaration { Globals.grdbg "function_proto_formal_3" ((arg2);) }
	| arg4 = tf_output_declaration { Globals.grdbg "function_proto_formal_5" ((arg4);) }
	| arg6 = tf_inout_declaration { Globals.grdbg "function_proto_formal_7" ((arg6);) }
	| arg8 = tf_ref_declaration { Globals.grdbg "function_proto_formal" ((arg8);) }
	;

%inline base_expression : arg2 = expression { Globals.grdbg "base_expression" ((arg2);) }
	;

join_keyword : JOIN { Globals.grdbg "join_keyword_3" ((JOIN);) }
	| JOIN_ANY { Globals.grdbg "join_keyword_5" ((JOIN_ANY);) }
	| JOIN_NONE { Globals.grdbg "join_keyword" ((JOIN_NONE);) }
	;

%inline t_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "t_path_delay_expression" ((arg2);) }
	;

associative_dimension : LBRACK arg3 = data_type RBRACK { Globals.grdbg "associative_dimension_5" (TRIPLE(LBRACK, arg3, RBRACK);) }
	| LBRACK TIMES RBRACK { Globals.grdbg "associative_dimension" (TRIPLE(LBRACK, TIMES, RBRACK);) }
	;

variable_decl_or_assignment : arg2 = data_type arg3 = list_of_variable_identifiers_or_assignments { Globals.grdbg "variable_decl_or_assignment_4" (DOUBLE(arg2, arg3);) }
	| arg5 = variable_assignment { Globals.grdbg "variable_decl_or_assignment" ((arg5);) }
	;

port : arg2 = port_2 { Globals.grdbg "port_3" ((arg2);) }
	| DOT arg5 = identifier LPAREN arg7 = port_7 RPAREN { Globals.grdbg "port" (QUINTUPLE(DOT, arg5, LPAREN, arg7, RPAREN);) }
	;

blocking_assignment : arg2 = variable_lvalue EQUALS arg4 = delay_or_event_control arg5 = expression { Globals.grdbg "blocking_assignment_6" (QUADRUPLE(arg2, EQUALS, arg4, arg5);) }
	| arg7 = hierarchical_identifier EQUALS NEW LBRACK arg11 = constant_expression RBRACK arg13 = blocking_assignment_13 { Globals.grdbg "blocking_assignment_14" (SEPTUPLE(arg7, EQUALS, NEW, LBRACK, arg11, RBRACK, arg13);) }
	| arg15 = identifier arg16 = blocking_assignment_16 EQUALS NEW arg19 = blocking_assignment_19 { Globals.grdbg "blocking_assignment_20" (QUINTUPLE(arg15, arg16, EQUALS, NEW, arg19);) }
	| arg21 = identifier DOT RANDOMIZE arg24 = blocking_assignment_24 WITH arg26 = constraint_block SEMICOLON { Globals.grdbg "blocking_assignment_28" (SEPTUPLE(arg21, DOT, RANDOMIZE, arg24, WITH, arg26, SEMICOLON);) }
	| arg29 = operator_assignment { Globals.grdbg "blocking_assignment" ((arg29);) }
	;

%inline specparam_declaration : SPECPARAM arg3 = specparam_declaration_3 arg4 = list_of_specparam_assignments SEMICOLON { Globals.grdbg "specparam_declaration" (QUADRUPLE(SPECPARAM, arg3, arg4, SEMICOLON);) }
	;

binary_digit : arg2 = X_DIGIT { Globals.grdbg "binary_digit_3" ((X_DIGIT arg2);) }
	| arg4 = Z_DIGIT { Globals.grdbg "binary_digit_5" ((Z_DIGIT arg4);) }
	| TOKEN_ZERO { Globals.grdbg "binary_digit_7" ((TOKEN_ZERO);) }
	| TOKEN_ONE { Globals.grdbg "binary_digit" ((TOKEN_ONE);) }
	;

constant_concatenation : LCURLY arg3 = constant_expression arg4 = constant_concatenation_4 RCURLY { Globals.grdbg "constant_concatenation_6" (QUADRUPLE(LCURLY, arg3, arg4, RCURLY);) }
	| LCURLY arg8 = struct_member_label COLON arg10 = constant_expression arg11 = constant_concatenation_11 RCURLY { Globals.grdbg "constant_concatenation_13" (SEXTUPLE(LCURLY, arg8, COLON, arg10, arg11, RCURLY);) }
	| LCURLY arg15 = array_member_label COLON arg17 = constant_expression arg18 = constant_concatenation_18 RCURLY { Globals.grdbg "constant_concatenation" (SEXTUPLE(LCURLY, arg15, COLON, arg17, arg18, RCURLY);) }
	;

constraint_set : arg2 = constraint_expression { Globals.grdbg "constraint_set_3" ((arg2);) }
	| LCURLY arg5 = constraint_set_5 RCURLY { Globals.grdbg "constraint_set" (TRIPLE(LCURLY, arg5, RCURLY);) }
	;

%inline fixed_point_number : arg2 = UNSIGNED_NUMBER DOT arg4 = UNSIGNED_NUMBER { Globals.grdbg "fixed_point_number" (TRIPLE(UNSIGNED_NUMBER arg2, DOT, UNSIGNED_NUMBER arg4);) }
	;

specify_item : arg2 = specparam_declaration { Globals.grdbg "specify_item_3" ((arg2);) }
	| arg4 = pulsestyle_declaration { Globals.grdbg "specify_item_5" ((arg4);) }
	| arg6 = showcancelled_declaration { Globals.grdbg "specify_item_7" ((arg6);) }
	| arg8 = path_declaration { Globals.grdbg "specify_item_9" ((arg8);) }
	| arg10 = system_timing_check { Globals.grdbg "specify_item" ((arg10);) }
	;

%inline net_assignment : arg2 = net_lvalue EQUALS arg4 = expression { Globals.grdbg "net_assignment" (TRIPLE(arg2, EQUALS, arg4);) }
	;

%inline udp_input_declaration : INPUT arg4 = list_of_udp_port_identifiers { Globals.grdbg "udp_input_declaration" (DOUBLE(INPUT, arg4);) }
	;

%inline function_call : arg2 = hierarchical_identifier arg4 = function_call_4 { Globals.grdbg "function_call" (DOUBLE(arg2, arg4);) }
	;

block_data_declaration : arg2 = block_variable_declaration { Globals.grdbg "block_data_declaration_3" ((arg2);) }
	| arg4 = constant_declaration { Globals.grdbg "block_data_declaration_5" ((arg4);) }
	| arg6 = type_declaration { Globals.grdbg "block_data_declaration" ((arg6);) }
	;

%inline clocked_sequence : arg2 = clocking_event arg3 = sequence_expr { Globals.grdbg "clocked_sequence" (DOUBLE(arg2, arg3);) }
	;

path_delay_value : arg2 = list_of_path_delay_expressions { Globals.grdbg "path_delay_value_3" ((arg2);) }
	| LPAREN arg5 = list_of_path_delay_expressions RPAREN { Globals.grdbg "path_delay_value" (TRIPLE(LPAREN, arg5, RPAREN);) }
	;

%inline statement : arg2 = statement_2 arg3 = statement_item { Globals.grdbg "statement" (DOUBLE(arg2, arg3);) }
	;

%inline stamptime_condition : arg2 = mintypmax_expression { Globals.grdbg "stamptime_condition" ((arg2);) }
	;

program_item : arg2 = port_declaration SEMICOLON { Globals.grdbg "program_item_4" (DOUBLE(arg2, SEMICOLON);) }
	| arg5 = non_port_program_item { Globals.grdbg "program_item" ((arg5);) }
	;

module_item : arg2 = non_generic_port_declaration SEMICOLON { Globals.grdbg "module_item_4" (DOUBLE(arg2, SEMICOLON);) }
	| arg5 = non_port_module_item { Globals.grdbg "module_item" ((arg5);) }
	;

task_or_function_identifier : arg2 = identifier { Globals.grdbg "task_or_function_identifier_3" ((arg2);) }
	| arg4 = identifier { Globals.grdbg "task_or_function_identifier" ((arg4);) }
	;

unpacked_dimension : LBRACK arg3 = constant_expression COLON arg5 = constant_expression RBRACK { Globals.grdbg "unpacked_dimension_7" (QUINTUPLE(LBRACK, arg3, COLON, arg5, RBRACK);) }
	| LBRACK arg9 = constant_expression RBRACK { Globals.grdbg "unpacked_dimension" (TRIPLE(LBRACK, arg9, RBRACK);) }
	;

class_constraint : arg2 = constraint_prototype { Globals.grdbg "class_constraint_3" ((arg2);) }
	| arg4 = constraint_declaration { Globals.grdbg "class_constraint" ((arg4);) }
	;

%inline combinational_body : TABLE arg3 = combinational_entry arg4 = combinational_body_4 ENDTABLE { Globals.grdbg "combinational_body" (QUADRUPLE(TABLE, arg3, arg4, ENDTABLE);) }
	;

next_state : arg2 = OUTPUT_SYMBOL { Globals.grdbg "next_state_3" ((OUTPUT_SYMBOL arg2);) }
	| MINUS { Globals.grdbg "next_state" ((MINUS);) }
	;

%inline list_of_tf_port_identifiers : arg2 = identifier arg3 = list_of_tf_port_identifiers_3 arg4 = list_of_tf_port_identifiers_4 arg5 = list_of_tf_port_identifiers_5 { Globals.grdbg "list_of_tf_port_identifiers" (QUADRUPLE(arg2, arg3, arg4, arg5);) }
	;

%inline d_nochange_timing_check : NOCHANGE LPAREN arg4 = timing_check_event COMMA arg6 = timing_check_event COMMA arg8 = mintypmax_expression COMMA arg10 = mintypmax_expression arg11 = d_nochange_timing_check_11 RPAREN SEMICOLON { Globals.grdbg "d_nochange_timing_check" (DUODECUPLE(NOCHANGE, LPAREN, arg4, COMMA, arg6, COMMA, arg8, COMMA, arg10, arg11, RPAREN, SEMICOLON);) }
	;

%inline initial_construct : INITIAL arg3 = statement_or_null { Globals.grdbg "initial_construct" (DOUBLE(INITIAL, arg3);) }
	;

sequence_spec : arg2 = multi_clock_sequence { Globals.grdbg "sequence_spec_3" ((arg2);) }
	| arg4 = sequence_expr { Globals.grdbg "sequence_spec" ((arg4);) }
	;

net_lvalue : arg2 = hierarchical_identifier arg3 = net_lvalue_3 arg4 = net_lvalue_4 { Globals.grdbg "net_lvalue_5" (TRIPLE(arg2, arg3, arg4);) }
	| LCURLY arg7 = net_lvalue arg8 = net_lvalue_8 RCURLY { Globals.grdbg "net_lvalue" (QUADRUPLE(LCURLY, arg7, arg8, RCURLY);) }
	;

udp_declaration : arg2 = udp_nonansi_declaration arg3 = udp_port_declaration arg4 = udp_declaration_4 arg5 = udp_body ENDPRIMITIVE arg7 = udp_declaration_7 { Globals.grdbg "udp_declaration_8" (SEXTUPLE(arg2, arg3, arg4, arg5, ENDPRIMITIVE, arg7);) }
	| arg9 = udp_ansi_declaration arg10 = udp_body ENDPRIMITIVE arg12 = udp_declaration_12 { Globals.grdbg "udp_declaration_13" (QUADRUPLE(arg9, arg10, ENDPRIMITIVE, arg12);) }
	| EXTERN arg15 = udp_nonansi_declaration { Globals.grdbg "udp_declaration_16" (DOUBLE(EXTERN, arg15);) }
	| EXTERN arg18 = udp_ansi_declaration { Globals.grdbg "udp_declaration_19" (DOUBLE(EXTERN, arg18);) }
	| PRIMITIVE arg22 = identifier LPAREN P_DOTSTAR RPAREN SEMICOLON arg27 = udp_declaration_27 arg28 = udp_body ENDPRIMITIVE arg30 = udp_declaration_30 { Globals.grdbg "udp_declaration" (DECUPLE(PRIMITIVE, arg22, LPAREN, P_DOTSTAR, RPAREN, SEMICOLON, arg27, arg28, ENDPRIMITIVE, arg30);) }
	;

%inline notify_reg : arg2 = identifier { Globals.grdbg "notify_reg" ((arg2);) }
	;

list_of_port_connections : arg2 = ordered_port_connection arg3 = list_of_port_connections_3 { Globals.grdbg "list_of_port_connections_4" (DOUBLE(arg2, arg3);) }
	| arg5 = dot_named_port_connection arg6 = list_of_port_connections_6 { Globals.grdbg "list_of_port_connections_7" (DOUBLE(arg5, arg6);) }
	| arg8 = list_of_port_connections_8 arg9 = dot_star_port_connection arg10 = list_of_port_connections_10 { Globals.grdbg "list_of_port_connections" (TRIPLE(arg8, arg9, arg10);) }
	;

%inline d_hold_timing_check : D_HOLD LPAREN arg4 = timing_check_event COMMA arg6 = timing_check_event COMMA arg8 = expression arg9 = d_hold_timing_check_9 RPAREN SEMICOLON { Globals.grdbg "d_hold_timing_check" (DECUPLE(D_HOLD, LPAREN, arg4, COMMA, arg6, COMMA, arg8, arg9, RPAREN, SEMICOLON);) }
	;

%inline inside_expression : arg2 = expression INSIDE arg4 = range_list_or_array { Globals.grdbg "inside_expression" (TRIPLE(arg2, INSIDE, arg4);) }
	;

%inline modport_tf_ports_declaration : arg2 = import_export arg3 = modport_tf_port { Globals.grdbg "modport_tf_ports_declaration" (DOUBLE(arg2, arg3);) }
	;

non_port_module_item : arg3 = generated_module_instantiation { Globals.grdbg "non_port_module_item_4" ((arg3);) }
	| arg6 = local_parameter_declaration { Globals.grdbg "non_port_module_item_7" ((arg6);) }
	| arg8 = module_or_generate_item { Globals.grdbg "non_port_module_item_9" ((arg8);) }
	| arg11 = parameter_declaration SEMICOLON { Globals.grdbg "non_port_module_item_13" (DOUBLE(arg11, SEMICOLON);) }
	| arg15 = specify_block { Globals.grdbg "non_port_module_item_16" ((arg15);) }
	| arg18 = specparam_declaration { Globals.grdbg "non_port_module_item_19" ((arg18);) }
	| arg20 = class_declaration { Globals.grdbg "non_port_module_item_21" ((arg20);) }
	| arg22 = module_declaration { Globals.grdbg "non_port_module_item" ((arg22);) }
	;

module_path_mintypmax_expression : arg2 = module_path_expression { Globals.grdbg "module_path_mintypmax_expression_3" ((arg2);) }
	| arg4 = module_path_expression COLON arg6 = module_path_expression COLON arg8 = module_path_expression { Globals.grdbg "module_path_mintypmax_expression" (QUINTUPLE(arg4, COLON, arg6, COLON, arg8);) }
	;

inout_declaration : INOUT arg3 = inout_declaration_3 arg4 = list_of_port_identifiers { Globals.grdbg "inout_declaration_5" (TRIPLE(INOUT, arg3, arg4);) }
	| INOUT arg7 = data_type arg8 = list_of_variable_identifiers { Globals.grdbg "inout_declaration" (TRIPLE(INOUT, arg7, arg8);) }
	;

%inline list_of_interface_identifiers : arg2 = identifier arg3 = list_of_interface_identifiers_3 arg4 = list_of_interface_identifiers_4 { Globals.grdbg "list_of_interface_identifiers" (TRIPLE(arg2, arg3, arg4);) }
	;

%inline d_setuphold_timing_check : D_SETUPHOLD LPAREN arg4 = timing_check_event COMMA arg6 = timing_check_event COMMA arg8 = expression COMMA arg10 = expression arg11 = d_setuphold_timing_check_11 RPAREN SEMICOLON { Globals.grdbg "d_setuphold_timing_check" (DUODECUPLE(D_SETUPHOLD, LPAREN, arg4, COMMA, arg6, COMMA, arg8, COMMA, arg10, arg11, RPAREN, SEMICOLON);) }
	;

strength0 : SUPPLY0 { Globals.grdbg "strength0_3" ((SUPPLY0);) }
	| STRONG0 { Globals.grdbg "strength0_5" ((STRONG0);) }
	| PULL0 { Globals.grdbg "strength0_7" ((PULL0);) }
	| WEAK0 { Globals.grdbg "strength0" ((WEAK0);) }
	;

%inline error_limit_value : arg2 = constant_mintypmax_expression { Globals.grdbg "error_limit_value" ((arg2);) }
	;

strength1 : SUPPLY1 { Globals.grdbg "strength1_3" ((SUPPLY1);) }
	| STRONG1 { Globals.grdbg "strength1_5" ((STRONG1);) }
	| PULL1 { Globals.grdbg "strength1_7" ((PULL1);) }
	| WEAK1 { Globals.grdbg "strength1" ((WEAK1);) }
	;

charge_strength : LPAREN SMALL RPAREN { Globals.grdbg "charge_strength_5" (TRIPLE(LPAREN, SMALL, RPAREN);) }
	| LPAREN MEDIUM RPAREN { Globals.grdbg "charge_strength_9" (TRIPLE(LPAREN, MEDIUM, RPAREN);) }
	| LPAREN LARGE RPAREN { Globals.grdbg "charge_strength" (TRIPLE(LPAREN, LARGE, RPAREN);) }
	;

%inline attribute_instance : TOKEN_LPAREN_STAR arg3 = attr_spec arg4 = attribute_instance_4 TOKEN_STAR_RPAREN { Globals.grdbg "attribute_instance" (QUADRUPLE(TOKEN_LPAREN_STAR, arg3, arg4, TOKEN_STAR_RPAREN);) }
	;

conditional_statement : arg2 = conditional_statement_2 IF LPAREN arg5 = expression RPAREN arg7 = statement_or_null arg8 = conditional_statement_8 { Globals.grdbg "conditional_statement_9" (SEPTUPLE(arg2, IF, LPAREN, arg5, RPAREN, arg7, arg8);) }
	| arg10 = if_else_if_statement { Globals.grdbg "conditional_statement" ((arg10);) }
	;

%inline variable_declaration : arg2 = variable_declaration_2 arg3 = data_type arg4 = list_of_variable_identifiers_or_assignments SEMICOLON { Globals.grdbg "variable_declaration" (QUADRUPLE(arg2, arg3, arg4, SEMICOLON);) }
	;

action_block : arg2 = statement_or_null { Globals.grdbg "action_block_3" ((arg2);) }
	| arg4 = action_block_4 ELSE arg6 = statement_or_null { Globals.grdbg "action_block" (TRIPLE(arg4, ELSE, arg6);) }
	;

lifetime : STATIC { Globals.grdbg "lifetime_3" ((STATIC);) }
	| AUTOMATIC { Globals.grdbg "lifetime" ((AUTOMATIC);) }
	;

%inline pull_gate_instance : arg2 = pull_gate_instance_2 LPAREN arg4 = net_lvalue RPAREN { Globals.grdbg "pull_gate_instance" (QUADRUPLE(arg2, LPAREN, arg4, RPAREN);) }
	;

%inline one_line_comment : TOKEN_SLASH_SLASH arg3 = comment_text TOKEN_ESCAPED_NEWLINE { Globals.grdbg "one_line_comment" (TRIPLE(TOKEN_SLASH_SLASH, arg3, TOKEN_ESCAPED_NEWLINE);) }
	;

%inline d_removal_timing_check : D_REMOVAL LPAREN arg4 = timing_check_event COMMA arg6 = timing_check_event COMMA arg8 = expression arg9 = d_removal_timing_check_9 RPAREN SEMICOLON { Globals.grdbg "d_removal_timing_check" (DECUPLE(D_REMOVAL, LPAREN, arg4, COMMA, arg6, COMMA, arg8, arg9, RPAREN, SEMICOLON);) }
	;

%inline topmodule_identifier : arg2 = identifier { Globals.grdbg "topmodule_identifier" ((arg2);) }
	;

%inline constant_function_call : arg2 = identifier arg4 = constant_function_call_4 { Globals.grdbg "constant_function_call" (DOUBLE(arg2, arg4);) }
	;

%inline reject_limit_value : arg2 = constant_mintypmax_expression { Globals.grdbg "reject_limit_value" ((arg2);) }
	;

showcancelled_declaration : SHOWCANCELLED arg3 = list_of_path_outputs SEMICOLON { Globals.grdbg "showcancelled_declaration_5" (TRIPLE(SHOWCANCELLED, arg3, SEMICOLON);) }
	| NOSHOWCANCELLED arg7 = list_of_path_outputs SEMICOLON { Globals.grdbg "showcancelled_declaration" (TRIPLE(NOSHOWCANCELLED, arg7, SEMICOLON);) }
	;

%inline constant_param_expression : arg2 = constant_expression { Globals.grdbg "constant_param_expression" ((arg2);) }
	;

%inline final_construct : FINAL arg3 = function_statement { Globals.grdbg "final_construct" (DOUBLE(FINAL, arg3);) }
	;

%inline module_instantiation : arg2 = identifier arg3 = module_instantiation_3 arg4 = module_instance arg5 = module_instantiation_5 SEMICOLON { Globals.grdbg "module_instantiation" (QUINTUPLE(arg2, arg3, arg4, arg5, SEMICOLON);) }
	;

timing_check_event_control : POSEDGE { Globals.grdbg "timing_check_event_control_3" ((POSEDGE);) }
	| NEGEDGE { Globals.grdbg "timing_check_event_control_5" ((NEGEDGE);) }
	| arg6 = edge_control_specifier { Globals.grdbg "timing_check_event_control" ((arg6);) }
	;

%inline conditional_expression : arg2 = expression QUERY arg5 = expression COLON arg7 = expression { Globals.grdbg "conditional_expression" (QUINTUPLE(arg2, QUERY, arg5, COLON, arg7);) }
	;

%inline hex_number : arg2 = hex_number_2 arg3 = HEX_BASE arg4 = hex_value { Globals.grdbg "hex_number" (TRIPLE(arg2, HEX_BASE arg3, arg4);) }
	;

packed_dimension : LBRACK arg3 = constant_expression COLON arg5 = constant_expression RBRACK { Globals.grdbg "packed_dimension_7" (QUINTUPLE(LBRACK, arg3, COLON, arg5, RBRACK);) }
	| LBRACK RBRACK { Globals.grdbg "packed_dimension" (DOUBLE(LBRACK, RBRACK);) }
	;

%inline ordered_port_connection : arg3 = ordered_port_connection_3 { Globals.grdbg "ordered_port_connection" ((arg3);) }
	;

net_declaration : arg2 = net_type arg3 = net_declaration_3 arg4 = net_declaration_4 arg5 = list_of_net_identifiers SEMICOLON { Globals.grdbg "net_declaration_7" (QUINTUPLE(arg2, arg3, arg4, arg5, SEMICOLON);) }
	| arg8 = net_type arg9 = net_declaration_9 arg10 = net_declaration_10 arg11 = net_declaration_11 arg12 = list_of_net_decl_assignments SEMICOLON { Globals.grdbg "net_declaration_14" (SEXTUPLE(arg8, arg9, arg10, arg11, arg12, SEMICOLON);) }
	| arg15 = net_type arg16 = net_declaration_16 arg17 = net_declaration_17 arg18 = net_declaration_18 arg19 = range arg20 = net_declaration_20 arg21 = list_of_net_identifiers SEMICOLON { Globals.grdbg "net_declaration_23" (OCTUPLE(arg15, arg16, arg17, arg18, arg19, arg20, arg21, SEMICOLON);) }
	| arg24 = net_type arg25 = net_declaration_25 arg26 = net_declaration_26 arg27 = net_declaration_27 arg28 = net_declaration_28 arg29 = range arg30 = net_declaration_30 arg31 = list_of_net_decl_assignments SEMICOLON { Globals.grdbg "net_declaration_33" (NONUPLE(arg24, arg25, arg26, arg27, arg28, arg29, arg30, arg31, SEMICOLON);) }
	| TRIREG arg35 = net_declaration_35 arg36 = net_declaration_36 arg37 = net_declaration_37 arg38 = list_of_net_identifiers SEMICOLON { Globals.grdbg "net_declaration_40" (SEXTUPLE(TRIREG, arg35, arg36, arg37, arg38, SEMICOLON);) }
	| TRIREG arg42 = net_declaration_42 arg43 = net_declaration_43 arg44 = net_declaration_44 arg45 = list_of_net_decl_assignments SEMICOLON { Globals.grdbg "net_declaration_47" (SEXTUPLE(TRIREG, arg42, arg43, arg44, arg45, SEMICOLON);) }
	| TRIREG arg49 = net_declaration_49 arg50 = net_declaration_50 arg51 = net_declaration_51 arg52 = net_declaration_52 arg53 = range arg54 = net_declaration_54 arg55 = list_of_net_identifiers SEMICOLON { Globals.grdbg "net_declaration_57" (NONUPLE(TRIREG, arg49, arg50, arg51, arg52, arg53, arg54, arg55, SEMICOLON);) }
	| TRIREG arg59 = net_declaration_59 arg60 = net_declaration_60 arg61 = net_declaration_61 arg62 = net_declaration_62 arg63 = range arg64 = net_declaration_64 arg65 = list_of_net_decl_assignments SEMICOLON { Globals.grdbg "net_declaration" (NONUPLE(TRIREG, arg59, arg60, arg61, arg62, arg63, arg64, arg65, SEMICOLON);) }
	;

%inline if_else_if_statement : arg2 = if_else_if_statement_2 IF LPAREN arg5 = expression RPAREN arg7 = statement_or_null arg8 = if_else_if_statement_8 arg9 = if_else_if_statement_9 { Globals.grdbg "if_else_if_statement" (OCTUPLE(arg2, IF, LPAREN, arg5, RPAREN, arg7, arg8, arg9);) }
	;

%inline sequential_body : arg2 = sequential_body_2 TABLE arg4 = sequential_entry arg5 = sequential_body_5 ENDTABLE { Globals.grdbg "sequential_body" (QUINTUPLE(arg2, TABLE, arg4, arg5, ENDTABLE);) }
	;

%inline combinational_construct : ALWAYS_COMB arg3 = statement { Globals.grdbg "combinational_construct" (DOUBLE(ALWAYS_COMB, arg3);) }
	;

%inline combinational_entry : arg2 = level_input_list COLON arg4 = OUTPUT_SYMBOL SEMICOLON { Globals.grdbg "combinational_entry" (QUADRUPLE(arg2, COLON, OUTPUT_SYMBOL arg4, SEMICOLON);) }
	;

input_identifier : arg2 = identifier { Globals.grdbg "input_identifier_3" ((arg2);) }
	| arg4 = identifier { Globals.grdbg "input_identifier" ((arg4);) }
	;

binary_module_path_operator : P_EQUAL { Globals.grdbg "binary_module_path_operator_3" ((P_EQUAL);) }
	| P_NOTEQUAL { Globals.grdbg "binary_module_path_operator_5" ((P_NOTEQUAL);) }
	| P_ANDAND { Globals.grdbg "binary_module_path_operator_7" ((P_ANDAND);) }
	| TOKEN_VBAR_VBAR { Globals.grdbg "binary_module_path_operator_9" ((TOKEN_VBAR_VBAR);) }
	| AMPERSAND { Globals.grdbg "binary_module_path_operator_11" ((AMPERSAND);) }
	| VBAR { Globals.grdbg "binary_module_path_operator_13" ((VBAR);) }
	| CARET { Globals.grdbg "binary_module_path_operator_15" ((CARET);) }
	| P_XNOR { Globals.grdbg "binary_module_path_operator_17" ((P_XNOR);) }
	| P_NXOR { Globals.grdbg "binary_module_path_operator" ((P_NXOR);) }
	;

%inline current_state : arg2 = LEVEL_SYMBOL { Globals.grdbg "current_state" ((LEVEL_SYMBOL arg2);) }
	;

%inline concurrent_assert_statement : arg2 = concurrent_assert_statement_2 arg3 = assert_property_statement { Globals.grdbg "concurrent_assert_statement" (DOUBLE(arg2, arg3);) }
	;

%inline interface_nonansi_header : INTERFACE arg4 = interface_nonansi_header_4 arg5 = identifier arg6 = interface_nonansi_header_6 arg7 = list_of_ports SEMICOLON { Globals.grdbg "interface_nonansi_header" (SEXTUPLE(INTERFACE, arg4, arg5, arg6, arg7, SEMICOLON);) }
	;

%inline const_assignment : arg2 = identifier EQUALS arg4 = constant_expression { Globals.grdbg "const_assignment" (TRIPLE(arg2, EQUALS, arg4);) }
	;

%inline modport_item : arg2 = identifier LPAREN arg4 = modport_ports_declaration arg5 = modport_item_5 RPAREN { Globals.grdbg "modport_item" (QUINTUPLE(arg2, LPAREN, arg4, arg5, RPAREN);) }
	;

%inline octal_number : arg2 = octal_number_2 arg3 = OCTAL_BASE arg4 = octal_value { Globals.grdbg "octal_number" (TRIPLE(arg2, OCTAL_BASE arg3, arg4);) }
	;

casting_type : arg2 = simple_type { Globals.grdbg "casting_type_3" ((arg2);) }
	| arg4 = number { Globals.grdbg "casting_type_5" ((arg4);) }
	| arg6 = signing { Globals.grdbg "casting_type" ((arg6);) }
	;

udp_output_declaration : OUTPUT arg4 = identifier { Globals.grdbg "udp_output_declaration_5" (DOUBLE(OUTPUT, arg4);) }
	| OUTPUT REG arg9 = identifier arg10 = udp_output_declaration_10 { Globals.grdbg "udp_output_declaration" (QUADRUPLE(OUTPUT, REG, arg9, arg10);) }
	;

delay_control : HASH arg3 = delay_value { Globals.grdbg "delay_control_4" (DOUBLE(HASH, arg3);) }
	| HASH LPAREN arg7 = mintypmax_expression RPAREN { Globals.grdbg "delay_control" (QUADRUPLE(HASH, LPAREN, arg7, RPAREN);) }
	;

%inline size : arg2 = non_zero_unsigned_number { Globals.grdbg "size" ((arg2);) }
	;

%inline list_of_type_assignments : arg2 = type_assignment arg3 = list_of_type_assignments_3 { Globals.grdbg "list_of_type_assignments" (DOUBLE(arg2, arg3);) }
	;

comment : arg2 = one_line_comment { Globals.grdbg "comment_3" ((arg2);) }
	| arg4 = block_comment { Globals.grdbg "comment" ((arg4);) }
	;

scalar_constant : TOKEN_FALSE2 { Globals.grdbg "scalar_constant_3" ((TOKEN_FALSE2);) }
	| TOKEN_TRUE2 { Globals.grdbg "scalar_constant_5" ((TOKEN_TRUE2);) }
	| TOKEN_FALSE1 { Globals.grdbg "scalar_constant_7" ((TOKEN_FALSE1);) }
	| TOKEN_TRUE1 { Globals.grdbg "scalar_constant_9" ((TOKEN_TRUE1);) }
	| TOKEN_FALSE { Globals.grdbg "scalar_constant_11" ((TOKEN_FALSE);) }
	| TOKEN_TRUE { Globals.grdbg "scalar_constant_13" ((TOKEN_TRUE);) }
	| TOKEN_B0 { Globals.grdbg "scalar_constant_15" ((TOKEN_B0);) }
	| TOKEN_B1 { Globals.grdbg "scalar_constant_17" ((TOKEN_B1);) }
	| TOKEN_ONE { Globals.grdbg "scalar_constant_19" ((TOKEN_ONE);) }
	| TOKEN_ZERO { Globals.grdbg "scalar_constant" ((TOKEN_ZERO);) }
	;

%inline lsb_constant_expression : arg2 = constant_expression { Globals.grdbg "lsb_constant_expression" ((arg2);) }
	;

%inline limit_value : arg2 = constant_mintypmax_expression { Globals.grdbg "limit_value" ((arg2);) }
	;

%inline enable_terminal : arg2 = expression { Globals.grdbg "enable_terminal" ((arg2);) }
	;

edge_descriptor : TOKEN_EDGE01 { Globals.grdbg "edge_descriptor_3" ((TOKEN_EDGE01);) }
	| TOKEN_EDGE_10 { Globals.grdbg "edge_descriptor_5" ((TOKEN_EDGE_10);) }
	| arg6 = Z_OR_X arg7 = zero_or_one { Globals.grdbg "edge_descriptor_8" (DOUBLE(Z_OR_X arg6, arg7);) }
	| arg9 = zero_or_one arg10 = Z_OR_X { Globals.grdbg "edge_descriptor" (DOUBLE(arg9, Z_OR_X arg10);) }
	;

%inline inst_name : arg2 = identifier arg3 = inst_name_3 { Globals.grdbg "inst_name" (DOUBLE(arg2, arg3);) }
	;

%inline udp_ansi_declaration : PRIMITIVE arg4 = identifier LPAREN arg6 = udp_declaration_port_list RPAREN SEMICOLON { Globals.grdbg "udp_ansi_declaration" (SEXTUPLE(PRIMITIVE, arg4, LPAREN, arg6, RPAREN, SEMICOLON);) }
	;

specparam_assignment : arg2 = identifier EQUALS arg4 = constant_mintypmax_expression { Globals.grdbg "specparam_assignment_5" (TRIPLE(arg2, EQUALS, arg4);) }
	| arg6 = pulse_control_specparam { Globals.grdbg "specparam_assignment" ((arg6);) }
	;

modport_ports_declaration : arg2 = modport_simple_ports_declaration { Globals.grdbg "modport_ports_declaration_3" ((arg2);) }
	| arg4 = modport_hierarchical_ports_declaration { Globals.grdbg "modport_ports_declaration_5" ((arg4);) }
	| arg6 = modport_tf_ports_declaration { Globals.grdbg "modport_ports_declaration" ((arg6);) }
	;

%inline list_of_modport_port_identifiers : arg2 = identifier arg3 = list_of_modport_port_identifiers_3 { Globals.grdbg "list_of_modport_port_identifiers" (DOUBLE(arg2, arg3);) }
	;

%inline config_declaration : CONFIG arg3 = identifier SEMICOLON arg5 = design_statement arg6 = config_declaration_6 ENDCONFIG { Globals.grdbg "config_declaration" (SEXTUPLE(CONFIG, arg3, SEMICOLON, arg5, arg6, ENDCONFIG);) }
	;

%inline genvar_identifier : arg2 = identifier { Globals.grdbg "genvar_identifier" ((arg2);) }
	;

implicit_class_handle : arg2 = implicit_class_handle_2 { Globals.grdbg "implicit_class_handle_3" ((arg2);) }
	| arg4 = implicit_class_handle_4 { Globals.grdbg "implicit_class_handle" ((arg4);) }
	;

%inline signal_identifier : arg2 = identifier { Globals.grdbg "signal_identifier" ((arg2);) }
	;

method_prototype : TASK arg3 = named_task_proto SEMICOLON { Globals.grdbg "method_prototype_5" (TRIPLE(TASK, arg3, SEMICOLON);) }
	| FUNCTION arg7 = named_function_proto SEMICOLON { Globals.grdbg "method_prototype" (TRIPLE(FUNCTION, arg7, SEMICOLON);) }
	;

constraint_block : SOLVE arg3 = identifier_list BEFORE arg5 = identifier_list SEMICOLON { Globals.grdbg "constraint_block_7" (QUINTUPLE(SOLVE, arg3, BEFORE, arg5, SEMICOLON);) }
	| arg8 = expression DIST LCURLY arg11 = dist_list RCURLY SEMICOLON { Globals.grdbg "constraint_block_14" (SEXTUPLE(arg8, DIST, LCURLY, arg11, RCURLY, SEMICOLON);) }
	| arg15 = constraint_expression { Globals.grdbg "constraint_block" ((arg15);) }
	;

%inline constant_multiple_concatenation : LCURLY arg3 = constant_expression arg4 = constant_concatenation RCURLY { Globals.grdbg "constant_multiple_concatenation" (QUADRUPLE(LCURLY, arg3, arg4, RCURLY);) }
	;

%inline specify_input_terminal_descriptor : arg2 = input_identifier arg3 = specify_input_terminal_descriptor_3 { Globals.grdbg "specify_input_terminal_descriptor" (DOUBLE(arg2, arg3);) }
	;

genvar_module_case_item : arg2 = constant_expression arg3 = genvar_module_case_item_3 COLON arg5 = generate_module_item { Globals.grdbg "genvar_module_case_item_6" (QUADRUPLE(arg2, arg3, COLON, arg5);) }
	| DEFAULT arg8 = genvar_module_case_item_8 arg9 = generate_module_item { Globals.grdbg "genvar_module_case_item" (TRIPLE(DEFAULT, arg8, arg9);) }
	;

%inline tz0_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "tz0_path_delay_expression" ((arg2);) }
	;

%inline block_comment : BEGIN_COMMENT arg3 = comment_text END_COMMENT { Globals.grdbg "block_comment" (TRIPLE(BEGIN_COMMENT, arg3, END_COMMENT);) }
	;

variable_lvalue : arg2 = hierarchical_identifier arg3 = variable_lvalue_3 arg4 = variable_lvalue_4 { Globals.grdbg "variable_lvalue_5" (TRIPLE(arg2, arg3, arg4);) }
	| LCURLY arg7 = variable_lvalue arg8 = variable_lvalue_8 RCURLY { Globals.grdbg "variable_lvalue" (QUADRUPLE(LCURLY, arg7, arg8, RCURLY);) }
	;

%inline list_of_defparam_assignments : arg2 = defparam_assignment arg3 = list_of_defparam_assignments_3 { Globals.grdbg "list_of_defparam_assignments" (DOUBLE(arg2, arg3);) }
	;

%inline port_reference : arg2 = identifier arg3 = port_reference_3 { Globals.grdbg "port_reference" (DOUBLE(arg2, arg3);) }
	;

%inline procedural_timing_control_statement : arg2 = procedural_timing_control arg3 = statement_or_null { Globals.grdbg "procedural_timing_control_statement" (DOUBLE(arg2, arg3);) }
	;

cmos_switchtype : CMOS { Globals.grdbg "cmos_switchtype_3" ((CMOS);) }
	| RCMOS { Globals.grdbg "cmos_switchtype" ((RCMOS);) }
	;

%inline list_of_clocking_decl_assign : arg2 = clocking_decl_assign arg3 = list_of_clocking_decl_assign_3 { Globals.grdbg "list_of_clocking_decl_assign" (DOUBLE(arg2, arg3);) }
	;

%inline program_instance_identifier : arg2 = identifier { Globals.grdbg "program_instance_identifier" ((arg2);) }
	;

system_timing_check : arg2 = d_setup_timing_check { Globals.grdbg "system_timing_check_3" ((arg2);) }
	| arg4 = d_hold_timing_check { Globals.grdbg "system_timing_check_5" ((arg4);) }
	| arg6 = d_setuphold_timing_check { Globals.grdbg "system_timing_check_7" ((arg6);) }
	| arg8 = d_recovery_timing_check { Globals.grdbg "system_timing_check_9" ((arg8);) }
	| arg10 = d_removal_timing_check { Globals.grdbg "system_timing_check_11" ((arg10);) }
	| arg12 = d_recrem_timing_check { Globals.grdbg "system_timing_check_13" ((arg12);) }
	| arg14 = d_skew_timing_check { Globals.grdbg "system_timing_check_15" ((arg14);) }
	| arg16 = d_timeskew_timing_check { Globals.grdbg "system_timing_check_17" ((arg16);) }
	| arg18 = d_fullskew_timing_check { Globals.grdbg "system_timing_check_19" ((arg18);) }
	| arg20 = d_period_timing_check { Globals.grdbg "system_timing_check_21" ((arg20);) }
	| arg22 = d_width_timing_check { Globals.grdbg "system_timing_check_23" ((arg22);) }
	| arg24 = d_nochange_timing_check { Globals.grdbg "system_timing_check" ((arg24);) }
	;

%inline list_of_tf_variable_identifiers : arg2 = identifier arg3 = variable_dimension arg4 = list_of_tf_variable_identifiers_4 arg5 = list_of_tf_variable_identifiers_5 { Globals.grdbg "list_of_tf_variable_identifiers" (QUADRUPLE(arg2, arg3, arg4, arg5);) }
	;

gate_instantiation : arg2 = cmos_switchtype arg3 = gate_instantiation_3 arg4 = cmos_switch_instance arg5 = gate_instantiation_5 SEMICOLON { Globals.grdbg "gate_instantiation_7" (QUINTUPLE(arg2, arg3, arg4, arg5, SEMICOLON);) }
	| arg8 = enable_gatetype arg9 = gate_instantiation_9 arg10 = gate_instantiation_10 arg11 = enable_gate_instance arg12 = gate_instantiation_12 SEMICOLON { Globals.grdbg "gate_instantiation_14" (SEXTUPLE(arg8, arg9, arg10, arg11, arg12, SEMICOLON);) }
	| arg15 = mos_switchtype arg16 = gate_instantiation_16 arg17 = mos_switch_instance arg18 = gate_instantiation_18 SEMICOLON { Globals.grdbg "gate_instantiation_20" (QUINTUPLE(arg15, arg16, arg17, arg18, SEMICOLON);) }
	| arg21 = n_input_gatetype arg22 = gate_instantiation_22 arg23 = gate_instantiation_23 arg24 = n_input_gate_instance arg25 = gate_instantiation_25 SEMICOLON { Globals.grdbg "gate_instantiation_27" (SEXTUPLE(arg21, arg22, arg23, arg24, arg25, SEMICOLON);) }
	| arg28 = n_output_gatetype arg29 = gate_instantiation_29 arg30 = gate_instantiation_30 arg31 = n_output_gate_instance arg32 = gate_instantiation_32 SEMICOLON { Globals.grdbg "gate_instantiation_34" (SEXTUPLE(arg28, arg29, arg30, arg31, arg32, SEMICOLON);) }
	| arg35 = pass_en_switchtype arg36 = gate_instantiation_36 arg37 = pass_enable_switch_instance arg38 = gate_instantiation_38 SEMICOLON { Globals.grdbg "gate_instantiation_40" (QUINTUPLE(arg35, arg36, arg37, arg38, SEMICOLON);) }
	| arg41 = pass_switchtype arg42 = pass_switch_instance arg43 = gate_instantiation_43 SEMICOLON { Globals.grdbg "gate_instantiation_45" (QUADRUPLE(arg41, arg42, arg43, SEMICOLON);) }
	| PULLDOWN arg47 = gate_instantiation_47 arg48 = pull_gate_instance arg49 = gate_instantiation_49 SEMICOLON { Globals.grdbg "gate_instantiation_51" (QUINTUPLE(PULLDOWN, arg47, arg48, arg49, SEMICOLON);) }
	| PULLUP arg53 = gate_instantiation_53 arg54 = pull_gate_instance arg55 = gate_instantiation_55 SEMICOLON { Globals.grdbg "gate_instantiation" (QUINTUPLE(PULLUP, arg53, arg54, arg55, SEMICOLON);) }
	;

range_or_type : arg2 = range_or_type_2 arg3 = range { Globals.grdbg "range_or_type_4" (DOUBLE(arg2, arg3);) }
	| arg5 = function_data_type { Globals.grdbg "range_or_type" ((arg5);) }
	;

task_port_list : arg2 = task_port_item arg3 = task_port_list_3 { Globals.grdbg "task_port_list_4" (DOUBLE(arg2, arg3);) }
	| arg5 = list_of_port_identifiers arg6 = task_port_list_6 { Globals.grdbg "task_port_list" (DOUBLE(arg5, arg6);) }
	;

tf_output_declaration : OUTPUT arg3 = tf_output_declaration_3 arg4 = tf_output_declaration_4 arg5 = list_of_tf_port_identifiers { Globals.grdbg "tf_output_declaration_6" (QUADRUPLE(OUTPUT, arg3, arg4, arg5);) }
	| OUTPUT arg8 = tf_data_type arg9 = list_of_tf_variable_identifiers { Globals.grdbg "tf_output_declaration" (TRIPLE(OUTPUT, arg8, arg9);) }
	;

%inline net_identifier : arg2 = identifier { Globals.grdbg "net_identifier" ((arg2);) }
	;

property_qualifier : RAND { Globals.grdbg "property_qualifier_3" ((RAND);) }
	| RANDC { Globals.grdbg "property_qualifier_5" ((RANDC);) }
	| arg6 = class_item_qualifier { Globals.grdbg "property_qualifier" ((arg6);) }
	;

%inline udp_instance : arg2 = udp_instance_2 arg3 = udp_instance_3 LPAREN arg5 = net_lvalue COMMA arg7 = expression arg8 = udp_instance_8 RPAREN { Globals.grdbg "udp_instance" (OCTUPLE(arg2, arg3, LPAREN, arg5, COMMA, arg7, arg8, RPAREN);) }
	;

white_space : SPACE { Globals.grdbg "white_space_3" ((SPACE);) }
	| TAB { Globals.grdbg "white_space_5" ((TAB);) }
	| NEWLINE { Globals.grdbg "white_space_7" ((NEWLINE);) }
	| EOF { Globals.grdbg "white_space" ((EOF);) }
	;

library_descriptions : arg2 = library_declaration { Globals.grdbg "library_descriptions_3" ((arg2);) }
	| arg4 = include_statement { Globals.grdbg "library_descriptions_5" ((arg4);) }
	| arg6 = config_declaration { Globals.grdbg "library_descriptions" ((arg6);) }
	;

%inline dot_star_port_connection : P_DOTSTAR { Globals.grdbg "dot_star_port_connection" ((P_DOTSTAR);) }
	;

%inline assertion_variable_declaration : arg2 = data_type arg3 = list_of_variable_identifiers SEMICOLON { Globals.grdbg "assertion_variable_declaration" (TRIPLE(arg2, arg3, SEMICOLON);) }
	;

%inline tx0_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "tx0_path_delay_expression" ((arg2);) }
	;

%inline list_of_function_proto_formals : arg2 = list_of_function_proto_formals_2 { Globals.grdbg "list_of_function_proto_formals" ((arg2);) }
	;

%inline genvar_declaration : GENVAR arg3 = list_of_genvar_identifiers SEMICOLON { Globals.grdbg "genvar_declaration" (TRIPLE(GENVAR, arg3, SEMICOLON);) }
	;

timeunits_declaration : TIMEUNIT arg3 = time_literal SEMICOLON { Globals.grdbg "timeunits_declaration_5" (TRIPLE(TIMEUNIT, arg3, SEMICOLON);) }
	| TIMEPRECISION arg7 = time_literal SEMICOLON { Globals.grdbg "timeunits_declaration_9" (TRIPLE(TIMEPRECISION, arg7, SEMICOLON);) }
	| TIMEUNIT arg11 = time_literal SEMICOLON TIMEPRECISION arg14 = time_literal SEMICOLON { Globals.grdbg "timeunits_declaration_16" (SEXTUPLE(TIMEUNIT, arg11, SEMICOLON, TIMEPRECISION, arg14, SEMICOLON);) }
	| TIMEPRECISION arg18 = time_literal SEMICOLON TIMEUNIT arg21 = time_literal SEMICOLON { Globals.grdbg "timeunits_declaration" (SEXTUPLE(TIMEPRECISION, arg18, SEMICOLON, TIMEUNIT, arg21, SEMICOLON);) }
	;

%inline simple_hierarchical_branch : arg2 = SIMPLE_IDENTIFIER arg3 = simple_hierarchical_branch_3 arg4 = simple_hierarchical_branch_4 { Globals.grdbg "simple_hierarchical_branch" (TRIPLE(SIMPLE_IDENTIFIER arg2, arg3, arg4);) }
	;

%inline n_input_gate_instance : arg2 = n_input_gate_instance_2 LPAREN arg4 = net_lvalue COMMA arg6 = expression arg7 = n_input_gate_instance_7 RPAREN { Globals.grdbg "n_input_gate_instance" (SEPTUPLE(arg2, LPAREN, arg4, COMMA, arg6, arg7, RPAREN);) }
	;

delayed_data : arg2 = identifier { Globals.grdbg "delayed_data_3" ((arg2);) }
	| arg4 = identifier LBRACK arg6 = constant_mintypmax_expression RBRACK { Globals.grdbg "delayed_data" (QUADRUPLE(arg4, LBRACK, arg6, RBRACK);) }
	;

variable_decl_assignment : arg2 = identifier arg3 = variable_decl_assignment_3 arg4 = variable_decl_assignment_4 { Globals.grdbg "variable_decl_assignment_5" (TRIPLE(arg2, arg3, arg4);) }
	| arg6 = identifier LBRACK RBRACK EQUALS NEW LBRACK arg12 = constant_expression RBRACK arg14 = variable_decl_assignment_14 { Globals.grdbg "variable_decl_assignment_15" (NONUPLE(arg6, LBRACK, RBRACK, EQUALS, NEW, LBRACK, arg12, RBRACK, arg14);) }
	| arg16 = identifier arg17 = variable_decl_assignment_17 EQUALS NEW arg20 = variable_decl_assignment_20 { Globals.grdbg "variable_decl_assignment" (QUINTUPLE(arg16, arg17, EQUALS, NEW, arg20);) }
	;

module_common_item : arg3 = module_or_generate_item_declaration { Globals.grdbg "module_common_item_4" ((arg3);) }
	| arg6 = interface_instantiation { Globals.grdbg "module_common_item_7" ((arg6);) }
	| arg9 = program_instantiation { Globals.grdbg "module_common_item_10" ((arg9);) }
	| arg12 = concurrent_assertion_item { Globals.grdbg "module_common_item_13" ((arg12);) }
	| arg15 = bind_directive { Globals.grdbg "module_common_item" ((arg15);) }
	;

list_of_arguments : arg2 = list_of_arguments_2 arg3 = list_of_arguments_3 { Globals.grdbg "list_of_arguments_4" (DOUBLE(arg2, arg3);) }
	| DOT arg6 = identifier LPAREN arg8 = list_of_arguments_8 RPAREN arg10 = list_of_arguments_10 { Globals.grdbg "list_of_arguments" (SEXTUPLE(DOT, arg6, LPAREN, arg8, RPAREN, arg10);) }
	;

%inline pass_enable_switch_instance : arg2 = pass_enable_switch_instance_2 LPAREN arg4 = net_lvalue COMMA arg6 = net_lvalue COMMA arg8 = expression RPAREN { Globals.grdbg "pass_enable_switch_instance" (OCTUPLE(arg2, LPAREN, arg4, COMMA, arg6, COMMA, arg8, RPAREN);) }
	;

%inline t10_path_delay_expression : arg2 = constant_mintypmax_expression { Globals.grdbg "t10_path_delay_expression" ((arg2);) }
	;

%inline sequence_identifier : arg2 = identifier { Globals.grdbg "sequence_identifier" ((arg2);) }
	;

generic_interface_port_declaration : INTERFACE arg3 = list_of_interface_identifiers { Globals.grdbg "generic_interface_port_declaration_4" (DOUBLE(INTERFACE, arg3);) }
	| INTERFACE DOT arg7 = identifier arg8 = list_of_interface_identifiers { Globals.grdbg "generic_interface_port_declaration" (QUADRUPLE(INTERFACE, DOT, arg7, arg8);) }
	;

udp_port_declaration : arg2 = udp_output_declaration SEMICOLON { Globals.grdbg "udp_port_declaration_4" (DOUBLE(arg2, SEMICOLON);) }
	| arg5 = udp_input_declaration SEMICOLON { Globals.grdbg "udp_port_declaration_7" (DOUBLE(arg5, SEMICOLON);) }
	| arg8 = udp_reg_declaration SEMICOLON { Globals.grdbg "udp_port_declaration" (DOUBLE(arg8, SEMICOLON);) }
	;

%inline specify_block : SPECIFY arg3 = specify_block_3 ENDSPECIFY { Globals.grdbg "specify_block" (TRIPLE(SPECIFY, arg3, ENDSPECIFY);) }
	;

tf_data_type : arg2 = data_type { Globals.grdbg "tf_data_type_3" ((arg2);) }
	| CHANDLE { Globals.grdbg "tf_data_type" ((CHANDLE);) }
	;

%inline interface_instantiation : arg2 = identifier arg3 = interface_instantiation_3 arg4 = module_instance arg5 = interface_instantiation_5 SEMICOLON { Globals.grdbg "interface_instantiation" (QUINTUPLE(arg2, arg3, arg4, arg5, SEMICOLON);) }
	;

%inline data_event : arg2 = timing_check_event { Globals.grdbg "data_event" ((arg2);) }
	;

integer_vector_type : BIT { Globals.grdbg "integer_vector_type_3" ((BIT);) }
	| LOGIC { Globals.grdbg "integer_vector_type_5" ((LOGIC);) }
	| REG { Globals.grdbg "integer_vector_type" ((REG);) }
	;

decimal_digit : TOKEN_ZERO { Globals.grdbg "decimal_digit_3" ((TOKEN_ZERO);) }
	| TOKEN_ONE { Globals.grdbg "decimal_digit_5" ((TOKEN_ONE);) }
	| TOKEN_TWO { Globals.grdbg "decimal_digit_7" ((TOKEN_TWO);) }
	| TOKEN_THREE { Globals.grdbg "decimal_digit_9" ((TOKEN_THREE);) }
	| TOKEN_FOUR { Globals.grdbg "decimal_digit_11" ((TOKEN_FOUR);) }
	| TOKEN_FIVE { Globals.grdbg "decimal_digit_13" ((TOKEN_FIVE);) }
	| TOKEN_SIX { Globals.grdbg "decimal_digit_15" ((TOKEN_SIX);) }
	| TOKEN_SEVEN { Globals.grdbg "decimal_digit_17" ((TOKEN_SEVEN);) }
	| TOKEN_EIGHT { Globals.grdbg "decimal_digit_19" ((TOKEN_EIGHT);) }
	| TOKEN_NINE { Globals.grdbg "decimal_digit" ((TOKEN_NINE);) }
	;

%inline output_terminal : arg2 = net_lvalue { Globals.grdbg "output_terminal" ((arg2);) }
	;

function_conditional_statement : arg2 = function_conditional_statement_2 IF LPAREN arg5 = expression RPAREN arg7 = function_statement_or_null arg8 = function_conditional_statement_8 { Globals.grdbg "function_conditional_statement_9" (SEPTUPLE(arg2, IF, LPAREN, arg5, RPAREN, arg7, arg8);) }
	| arg10 = function_if_else_if_statement { Globals.grdbg "function_conditional_statement" ((arg10);) }
	;

module_or_generate_item_declaration : arg2 = net_declaration { Globals.grdbg "module_or_generate_item_declaration_3" ((arg2);) }
	| arg4 = data_declaration { Globals.grdbg "module_or_generate_item_declaration_5" ((arg4);) }
	| arg6 = genvar_declaration { Globals.grdbg "module_or_generate_item_declaration_7" ((arg6);) }
	| arg8 = task_declaration { Globals.grdbg "module_or_generate_item_declaration_9" ((arg8);) }
	| arg10 = function_declaration { Globals.grdbg "module_or_generate_item_declaration_11" ((arg10);) }
	| arg12 = dpi_import_export { Globals.grdbg "module_or_generate_item_declaration_13" ((arg12);) }
	| arg14 = extern_constraint_declaration { Globals.grdbg "module_or_generate_item_declaration_15" ((arg14);) }
	| arg16 = extern_method_declaration { Globals.grdbg "module_or_generate_item_declaration_17" ((arg16);) }
	| arg18 = clocking_decl { Globals.grdbg "module_or_generate_item_declaration_19" ((arg18);) }
	| DEFAULT CLOCKING arg22 = identifier SEMICOLON { Globals.grdbg "module_or_generate_item_declaration" (QUADRUPLE(DEFAULT, CLOCKING, arg22, SEMICOLON);) }
	;

scalar_timing_check_condition : arg2 = expression { Globals.grdbg "scalar_timing_check_condition_3" ((arg2);) }
	| TILDE arg5 = expression { Globals.grdbg "scalar_timing_check_condition_6" (DOUBLE(TILDE, arg5);) }
	| arg7 = expression P_EQUAL arg9 = scalar_constant { Globals.grdbg "scalar_timing_check_condition_10" (TRIPLE(arg7, P_EQUAL, arg9);) }
	| arg11 = expression P_CASEEQUAL arg13 = scalar_constant { Globals.grdbg "scalar_timing_check_condition_14" (TRIPLE(arg11, P_CASEEQUAL, arg13);) }
	| arg15 = expression P_NOTEQUAL arg17 = scalar_constant { Globals.grdbg "scalar_timing_check_condition_18" (TRIPLE(arg15, P_NOTEQUAL, arg17);) }
	| arg19 = expression P_CASENOTEQUAL arg21 = scalar_constant { Globals.grdbg "scalar_timing_check_condition" (TRIPLE(arg19, P_CASENOTEQUAL, arg21);) }
	;

primary : arg2 = number { Globals.grdbg "primary_3" ((arg2);) }
	| arg4 = implicit_class_handle arg5 = hierarchical_identifier arg6 = primary_6 arg7 = primary_7 arg8 = primary_8 { Globals.grdbg "primary_9" (QUINTUPLE(arg4, arg5, arg6, arg7, arg8);) }
	| arg10 = concatenation { Globals.grdbg "primary_11" ((arg10);) }
	| arg12 = multiple_concatenation { Globals.grdbg "primary_13" ((arg12);) }
	| arg14 = function_call { Globals.grdbg "primary_15" ((arg14);) }
	| arg16 = system_function_call { Globals.grdbg "primary_17" ((arg16);) }
	| arg18 = constant_function_call { Globals.grdbg "primary_19" ((arg18);) }
	| arg20 = identifier P_COLONCOLON arg22 = primary_22 arg23 = identifier { Globals.grdbg "primary_24" (QUADRUPLE(arg20, P_COLONCOLON, arg22, arg23);) }
	| LPAREN arg26 = mintypmax_expression RPAREN { Globals.grdbg "primary_28" (TRIPLE(LPAREN, arg26, RPAREN);) }
	| arg29 = casting_type SQUOTE LPAREN arg32 = expression RPAREN { Globals.grdbg "primary_34" (QUINTUPLE(arg29, SQUOTE, LPAREN, arg32, RPAREN);) }
	| VOID SQUOTE LPAREN arg38 = function_call RPAREN { Globals.grdbg "primary_40" (QUINTUPLE(VOID, SQUOTE, LPAREN, arg38, RPAREN);) }
	| arg41 = casting_type SQUOTE arg43 = concatenation { Globals.grdbg "primary_44" (TRIPLE(arg41, SQUOTE, arg43);) }
	| arg45 = casting_type SQUOTE arg47 = multiple_concatenation { Globals.grdbg "primary_48" (TRIPLE(arg45, SQUOTE, arg47);) }
	| arg49 = time_literal { Globals.grdbg "primary_50" ((arg49);) }
	| TOKEN_QUOTE_FALSE { Globals.grdbg "primary_52" ((TOKEN_QUOTE_FALSE);) }
	| TOKEN_QUOTE_TRUE { Globals.grdbg "primary_54" ((TOKEN_QUOTE_TRUE);) }
	| TOKEN_QUOTE_Z2 { Globals.grdbg "primary_56" ((TOKEN_QUOTE_Z2);) }
	| TOKEN_QUOTE_Z1 { Globals.grdbg "primary_58" ((TOKEN_QUOTE_Z1);) }
	| TOKEN_QUOTE_X2 { Globals.grdbg "primary_60" ((TOKEN_QUOTE_X2);) }
	| TOKEN_QUOTE_X1 { Globals.grdbg "primary_62" ((TOKEN_QUOTE_X1);) }
	| NULL { Globals.grdbg "primary" ((NULL);) }
	;

%inline level_input_list : arg2 = LEVEL_SYMBOL arg3 = level_input_list_3 { Globals.grdbg "level_input_list" (DOUBLE(LEVEL_SYMBOL arg2, arg3);) }
	;

%inline hierarchical_block_identifier : arg2 = hierarchical_identifier { Globals.grdbg "hierarchical_block_identifier" ((arg2);) }
	;

timing_check_condition : arg2 = scalar_timing_check_condition { Globals.grdbg "timing_check_condition_3" ((arg2);) }
	| LPAREN arg5 = scalar_timing_check_condition RPAREN { Globals.grdbg "timing_check_condition" (TRIPLE(LPAREN, arg5, RPAREN);) }
	;

modport_simple_ports_declaration : INPUT arg3 = list_of_modport_port_identifiers { Globals.grdbg "modport_simple_ports_declaration_4" (DOUBLE(INPUT, arg3);) }
	| OUTPUT arg6 = list_of_modport_port_identifiers { Globals.grdbg "modport_simple_ports_declaration_7" (DOUBLE(OUTPUT, arg6);) }
	| INOUT arg9 = list_of_modport_port_identifiers { Globals.grdbg "modport_simple_ports_declaration_10" (DOUBLE(INOUT, arg9);) }
	| REF arg12 = modport_simple_ports_declaration_12 arg13 = list_of_modport_port_identifiers { Globals.grdbg "modport_simple_ports_declaration" (TRIPLE(REF, arg12, arg13);) }
	;

%inline dimension_constant_expression : arg2 = constant_expression { Globals.grdbg "dimension_constant_expression" ((arg2);) }
	;

%inline event_based_flag : arg2 = constant_expression { Globals.grdbg "event_based_flag" ((arg2);) }
	;

non_port_interface_item : arg3 = generated_interface_instantiation { Globals.grdbg "non_port_interface_item_4" ((arg3);) }
	| arg6 = specparam_declaration { Globals.grdbg "non_port_interface_item_7" ((arg6);) }
	| arg8 = interface_or_generate_item { Globals.grdbg "non_port_interface_item_9" ((arg8);) }
	| arg10 = program_declaration { Globals.grdbg "non_port_interface_item_11" ((arg10);) }
	| arg12 = class_declaration { Globals.grdbg "non_port_interface_item_13" ((arg12);) }
	| arg14 = interface_declaration { Globals.grdbg "non_port_interface_item" ((arg14);) }
	;

named_parameter_assignment : DOT arg3 = identifier LPAREN arg5 = named_parameter_assignment_5 RPAREN { Globals.grdbg "named_parameter_assignment_7" (QUINTUPLE(DOT, arg3, LPAREN, arg5, RPAREN);) }
	| DOT arg9 = identifier LPAREN arg11 = data_type RPAREN { Globals.grdbg "named_parameter_assignment" (QUINTUPLE(DOT, arg9, LPAREN, arg11, RPAREN);) }
	;

import_export : IMPORT { Globals.grdbg "import_export_3" ((IMPORT);) }
	| EXPORT { Globals.grdbg "import_export" ((EXPORT);) }
	;

function_body_declaration : arg2 = function_body_declaration_2 arg3 = function_body_declaration_3 arg4 = function_body_declaration_4 arg5 = identifier SEMICOLON arg7 = function_body_declaration_7 arg8 = function_body_declaration_8 ENDFUNCTION arg10 = function_body_declaration_10 { Globals.grdbg "function_body_declaration_11" (NONUPLE(arg2, arg3, arg4, arg5, SEMICOLON, arg7, arg8, ENDFUNCTION, arg10);) }
	| arg12 = function_body_declaration_12 arg13 = function_body_declaration_13 arg14 = function_body_declaration_14 arg15 = identifier LPAREN arg17 = function_port_list RPAREN SEMICOLON arg20 = function_body_declaration_20 arg21 = function_body_declaration_21 ENDFUNCTION arg23 = function_body_declaration_23 { Globals.grdbg "function_body_declaration" (DUODECUPLE(arg12, arg13, arg14, arg15, LPAREN, arg17, RPAREN, SEMICOLON, arg20, arg21, ENDFUNCTION, arg23);) }
	;

generate_module_item : arg2 = generate_module_conditional_statement { Globals.grdbg "generate_module_item_3" ((arg2);) }
	| arg4 = generate_module_case_statement { Globals.grdbg "generate_module_item_5" ((arg4);) }
	| arg6 = generate_module_loop_statement { Globals.grdbg "generate_module_item_7" ((arg6);) }
	| arg8 = generate_module_item_8 arg9 = generate_module_block { Globals.grdbg "generate_module_item_10" (DOUBLE(arg8, arg9);) }
	| arg11 = module_or_generate_item { Globals.grdbg "generate_module_item" ((arg11);) }
	;

clocking_skew : arg2 = edge_identifier arg3 = clocking_skew_3 { Globals.grdbg "clocking_skew_4" (DOUBLE(arg2, arg3);) }
	| arg5 = delay_control { Globals.grdbg "clocking_skew" ((arg5);) }
	;

%inline function_if_else_if_statement : arg2 = function_if_else_if_statement_2 IF LPAREN arg5 = expression RPAREN arg7 = function_statement_or_null arg8 = function_if_else_if_statement_8 arg9 = function_if_else_if_statement_9 { Globals.grdbg "function_if_else_if_statement" (OCTUPLE(arg2, IF, LPAREN, arg5, RPAREN, arg7, arg8, arg9);) }
	;

genvar_interface_case_item : arg2 = constant_expression arg3 = genvar_interface_case_item_3 COLON arg5 = generate_interface_item { Globals.grdbg "genvar_interface_case_item_6" (QUADRUPLE(arg2, arg3, COLON, arg5);) }
	| DEFAULT arg8 = genvar_interface_case_item_8 arg9 = generate_interface_item { Globals.grdbg "genvar_interface_case_item" (TRIPLE(DEFAULT, arg8, arg9);) }
	;

task_port_item : arg3 = tf_input_declaration { Globals.grdbg "task_port_item_4" ((arg3);) }
	| arg6 = tf_output_declaration { Globals.grdbg "task_port_item_7" ((arg6);) }
	| arg9 = tf_inout_declaration { Globals.grdbg "task_port_item_10" ((arg9);) }
	| arg12 = tf_ref_declaration SEMICOLON { Globals.grdbg "task_port_item_14" (DOUBLE(arg12, SEMICOLON);) }
	| arg16 = port_type arg17 = list_of_tf_port_identifiers { Globals.grdbg "task_port_item_18" (DOUBLE(arg16, arg17);) }
	| arg20 = tf_data_type arg21 = list_of_tf_variable_identifiers { Globals.grdbg "task_port_item" (DOUBLE(arg20, arg21);) }
	;

%inline cell_clause : CELL arg3 = cell_clause_3 arg4 = identifier { Globals.grdbg "cell_clause" (TRIPLE(CELL, arg3, arg4);) }
	;

%inline msb_constant_expression : arg2 = constant_expression { Globals.grdbg "msb_constant_expression" ((arg2);) }
	;

clocking_event : AT arg3 = identifier { Globals.grdbg "clocking_event_4" (DOUBLE(AT, arg3);) }
	| AT LPAREN arg7 = event_expression RPAREN { Globals.grdbg "clocking_event" (QUADRUPLE(AT, LPAREN, arg7, RPAREN);) }
	;

%inline dist_list : arg2 = dist_item arg3 = dist_list_3 { Globals.grdbg "dist_list" (DOUBLE(arg2, arg3);) }
	;

dpi_function_proto : arg2 = named_function_proto { Globals.grdbg "dpi_function_proto_3" ((arg2);) }
	| arg4 = dpi_function_proto_4 arg5 = function_data_type arg6 = identifier LPAREN arg8 = list_of_dpi_proto_formals RPAREN { Globals.grdbg "dpi_function_proto" (SEXTUPLE(arg4, arg5, arg6, LPAREN, arg8, RPAREN);) }
	;

%inline input_terminal : arg2 = expression { Globals.grdbg "input_terminal" ((arg2);) }
	;

%inline param_assignment : arg2 = identifier EQUALS arg4 = constant_expression { Globals.grdbg "param_assignment" (TRIPLE(arg2, EQUALS, arg4);) }
	;

%inline default_clause : DEFAULT { Globals.grdbg "default_clause" ((DEFAULT);) }
	;

parameter_declaration : PARAMETER arg3 = parameter_declaration_3 arg4 = parameter_declaration_4 arg5 = parameter_declaration_5 arg6 = list_of_param_assignments { Globals.grdbg "parameter_declaration_7" (QUINTUPLE(PARAMETER, arg3, arg4, arg5, arg6);) }
	| PARAMETER arg9 = data_type arg10 = list_of_param_assignments { Globals.grdbg "parameter_declaration_11" (TRIPLE(PARAMETER, arg9, arg10);) }
	| PARAMETER TYPE arg14 = list_of_type_assignments { Globals.grdbg "parameter_declaration" (TRIPLE(PARAMETER, TYPE, arg14);) }
	;

%inline escaped_hierarchical_identifier : arg2 = escaped_hierarchical_branch arg3 = escaped_hierarchical_identifier_3 { Globals.grdbg "escaped_hierarchical_identifier" (DOUBLE(arg2, arg3);) }
	;

%inline function_declaration : FUNCTION arg3 = function_declaration_3 arg4 = function_body_declaration { Globals.grdbg "function_declaration" (TRIPLE(FUNCTION, arg3, arg4);) }
	;

config_rule_statement : arg2 = default_clause arg3 = liblist_clause { Globals.grdbg "config_rule_statement_4" (DOUBLE(arg2, arg3);) }
	| arg5 = inst_clause arg6 = liblist_clause { Globals.grdbg "config_rule_statement_7" (DOUBLE(arg5, arg6);) }
	| arg8 = inst_clause arg9 = use_clause { Globals.grdbg "config_rule_statement_10" (DOUBLE(arg8, arg9);) }
	| arg11 = cell_clause arg12 = liblist_clause { Globals.grdbg "config_rule_statement_13" (DOUBLE(arg11, arg12);) }
	| arg14 = cell_clause arg15 = use_clause { Globals.grdbg "config_rule_statement" (DOUBLE(arg14, arg15);) }
	;

%inline input_port_identifier : arg2 = identifier { Globals.grdbg "input_port_identifier" ((arg2);) }
	;

function_statement_or_null : arg2 = function_statement { Globals.grdbg "function_statement_or_null_3" ((arg2);) }
	| SEMICOLON { Globals.grdbg "function_statement_or_null" ((SEMICOLON);) }
	;

library_declaration_5 : { Globals.grdbg "library_declaration_5_2" ( EMPTY) }
	| arg3 = library_declaration_5 COMMA arg5 = file_path_spec { Globals.grdbg "library_declaration_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

library_declaration_6_5 : { Globals.grdbg "library_declaration_6_5_2" ( EMPTY) }
	| arg3 = library_declaration_6_5 COMMA arg5 = file_path_spec { Globals.grdbg "library_declaration_6_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline library_declaration_6 : { Globals.grdbg "library_declaration_6_2" ( EMPTY) }
	| INCDIR arg4 = file_path_spec arg5 = library_declaration_6_5 { Globals.grdbg "library_declaration_6" (TRIPLE(INCDIR, arg4, arg5);) }
	;

%inline full_path_description_4 : { Globals.grdbg "full_path_description_4_2" ( EMPTY) }
	| arg3 = polarity_operator { Globals.grdbg "full_path_description_4" ((arg3);) }
	;

modport_declaration_4 : { Globals.grdbg "modport_declaration_4_2" ( EMPTY) }
	| arg3 = modport_declaration_4 COMMA arg5 = modport_item { Globals.grdbg "modport_declaration_4" (TRIPLE(arg3, COMMA, arg5);) }
	;

edge_control_specifier_5 : { Globals.grdbg "edge_control_specifier_5_2" ( EMPTY) }
	| arg3 = edge_control_specifier_5 COMMA arg5 = edge_descriptor { Globals.grdbg "edge_control_specifier_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

dpi_proto_formal_3_5 : { Globals.grdbg "dpi_proto_formal_3_5_2" ( EMPTY) }
	| arg3 = dpi_proto_formal_3_5 COMMA arg5 = identifier arg6 = dpi_dimension { Globals.grdbg "dpi_proto_formal_3_5" (QUADRUPLE(arg3, COMMA, arg5, arg6);) }
	;

%inline dpi_proto_formal_3 : { Globals.grdbg "dpi_proto_formal_3_2" ( EMPTY) }
	| arg3 = identifier arg4 = dpi_dimension arg5 = dpi_proto_formal_3_5 { Globals.grdbg "dpi_proto_formal_3" (TRIPLE(arg3, arg4, arg5);) }
	;

%inline simple_hierarchical_identifier_3 : { Globals.grdbg "simple_hierarchical_identifier_3_2" ( EMPTY) }
	| DOT arg4 = escaped_identifier { Globals.grdbg "simple_hierarchical_identifier_3" (DOUBLE(DOT, arg4);) }
	;

octal_value_3 : { Globals.grdbg "octal_value_3_2" ( EMPTY) }
	| arg3 = octal_value_3 UNDERSCORE { Globals.grdbg "octal_value_3_5" (DOUBLE(arg3, UNDERSCORE);) }
	| arg6 = octal_digit { Globals.grdbg "octal_value_3" ((arg6);) }
	;

list_of_specparam_assignments_3 : { Globals.grdbg "list_of_specparam_assignments_3_2" ( EMPTY) }
	| arg3 = list_of_specparam_assignments_3 COMMA arg5 = specparam_assignment { Globals.grdbg "list_of_specparam_assignments_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

list_of_variable_identifiers_4 : { Globals.grdbg "list_of_variable_identifiers_4_2" ( EMPTY) }
	| arg3 = list_of_variable_identifiers_4 COMMA arg5 = identifier arg6 = variable_dimension { Globals.grdbg "list_of_variable_identifiers_4" (QUADRUPLE(arg3, COMMA, arg5, arg6);) }
	;

%inline named_function_proto_2 : { Globals.grdbg "named_function_proto_2_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "named_function_proto_2" ((arg3);) }
	;

%inline class_declaration_3 : { Globals.grdbg "class_declaration_3_2" ( EMPTY) }
	| VIRTUAL { Globals.grdbg "class_declaration_3" ((VIRTUAL);) }
	;

%inline class_declaration_5 : { Globals.grdbg "class_declaration_5_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "class_declaration_5" ((arg3);) }
	;

%inline class_declaration_7 : { Globals.grdbg "class_declaration_7_2" ( EMPTY) }
	| arg3 = parameter_port_list { Globals.grdbg "class_declaration_7" ((arg3);) }
	;

%inline class_declaration_8 : { Globals.grdbg "class_declaration_8_2" ( EMPTY) }
	| EXTENDS arg4 = identifier { Globals.grdbg "class_declaration_8" (DOUBLE(EXTENDS, arg4);) }
	;

%inline class_declaration_10 : { Globals.grdbg "class_declaration_10_2" ( EMPTY) }
	| arg3 = timeunits_declaration { Globals.grdbg "class_declaration_10" ((arg3);) }
	;

class_declaration_11 : { Globals.grdbg "class_declaration_11_2" ( EMPTY) }
	| arg3 = class_declaration_11 arg4 = class_item { Globals.grdbg "class_declaration_11" (DOUBLE(arg3, arg4);) }
	;

%inline class_declaration_13 : { Globals.grdbg "class_declaration_13_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "class_declaration_13" (DOUBLE(COLON, arg4);) }
	;

list_of_port_declarations_4 : { Globals.grdbg "list_of_port_declarations_4_2" ( EMPTY) }
	| arg3 = list_of_port_declarations_4 COMMA arg5 = port_declaration { Globals.grdbg "list_of_port_declarations_4" (TRIPLE(arg3, COMMA, arg5);) }
	;

identifier_list_3 : { Globals.grdbg "identifier_list_3_2" ( EMPTY) }
	| arg3 = identifier_list_3 COMMA arg5 = identifier { Globals.grdbg "identifier_list_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline module_ansi_header_4 : { Globals.grdbg "module_ansi_header_4_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "module_ansi_header_4" ((arg3);) }
	;

%inline module_ansi_header_6 : { Globals.grdbg "module_ansi_header_6_2" ( EMPTY) }
	| arg3 = parameter_port_list { Globals.grdbg "module_ansi_header_6" ((arg3);) }
	;

%inline module_ansi_header_7 : { Globals.grdbg "module_ansi_header_7_2" ( EMPTY) }
	| arg3 = list_of_port_declarations { Globals.grdbg "module_ansi_header_7" ((arg3);) }
	;

udp_declaration2_10 : { Globals.grdbg "udp_declaration2_10_2" ( EMPTY) }
	| arg3 = udp_declaration2_10 arg4 = udp_port_declaration { Globals.grdbg "udp_declaration2_10" (DOUBLE(arg3, arg4);) }
	;

multi_clock_sequence_3 : { Globals.grdbg "multi_clock_sequence_3_2" ( EMPTY) }
	| arg3 = multi_clock_sequence_3 P_POUNDPOUND arg5 = clocked_sequence { Globals.grdbg "multi_clock_sequence_3" (TRIPLE(arg3, P_POUNDPOUND, arg5);) }
	;

list_of_genvar_identifiers_3 : { Globals.grdbg "list_of_genvar_identifiers_3_2" ( EMPTY) }
	| arg3 = list_of_genvar_identifiers_3 COMMA arg5 = identifier { Globals.grdbg "list_of_genvar_identifiers_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

variable_dimension_2 : { Globals.grdbg "variable_dimension_2_2" ( EMPTY) }
	| arg3 = variable_dimension_2 arg4 = unpacked_dimension { Globals.grdbg "variable_dimension_2" (DOUBLE(arg3, arg4);) }
	;

%inline local_parameter_declaration_3 : { Globals.grdbg "local_parameter_declaration_3_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "local_parameter_declaration_3" ((arg3);) }
	;

local_parameter_declaration_4 : { Globals.grdbg "local_parameter_declaration_4_2" ( EMPTY) }
	| arg3 = local_parameter_declaration_4 arg4 = packed_dimension { Globals.grdbg "local_parameter_declaration_4" (DOUBLE(arg3, arg4);) }
	;

%inline local_parameter_declaration_5 : { Globals.grdbg "local_parameter_declaration_5_2" ( EMPTY) }
	| arg3 = range { Globals.grdbg "local_parameter_declaration_5" ((arg3);) }
	;

%inline concurrent_cover_statement_2 : { Globals.grdbg "concurrent_cover_statement_2_2" ( EMPTY) }
	| arg3 = identifier COLON { Globals.grdbg "concurrent_cover_statement_2" (DOUBLE(arg3, COLON);) }
	;

list_of_dpi_proto_formals_2_5 : { Globals.grdbg "list_of_dpi_proto_formals_2_5_2" ( EMPTY) }
	| arg3 = list_of_dpi_proto_formals_2_5 COMMA arg6 = dpi_proto_formal { Globals.grdbg "list_of_dpi_proto_formals_2_5" (TRIPLE(arg3, COMMA, arg6);) }
	;

%inline list_of_dpi_proto_formals_2 : { Globals.grdbg "list_of_dpi_proto_formals_2_2" ( EMPTY) }
	| arg4 = dpi_proto_formal arg5 = list_of_dpi_proto_formals_2_5 { Globals.grdbg "list_of_dpi_proto_formals_2" (DOUBLE(arg4, arg5);) }
	;

%inline extern_method_declaration_3 : { Globals.grdbg "extern_method_declaration_3_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "extern_method_declaration_3" ((arg3);) }
	;

%inline extern_method_declaration_9 : { Globals.grdbg "extern_method_declaration_9_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "extern_method_declaration_9" ((arg3);) }
	;

list_of_path_outputs_3 : { Globals.grdbg "list_of_path_outputs_3_2" ( EMPTY) }
	| arg3 = list_of_path_outputs_3 COMMA arg5 = specify_output_terminal_descriptor { Globals.grdbg "list_of_path_outputs_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

generate_interface_named_block_5 : { Globals.grdbg "generate_interface_named_block_5_2" ( EMPTY) }
	| arg3 = generate_interface_named_block_5 arg4 = generate_interface_item { Globals.grdbg "generate_interface_named_block_5" (DOUBLE(arg3, arg4);) }
	;

%inline generate_interface_named_block_7 : { Globals.grdbg "generate_interface_named_block_7_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "generate_interface_named_block_7" (DOUBLE(COLON, arg4);) }
	;

%inline task_declaration_3 : { Globals.grdbg "task_declaration_3_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "task_declaration_3" ((arg3);) }
	;

%inline clocking_decl_2 : { Globals.grdbg "clocking_decl_2_2" ( EMPTY) }
	| DEFAULT { Globals.grdbg "clocking_decl_2" ((DEFAULT);) }
	;

%inline clocking_decl_4 : { Globals.grdbg "clocking_decl_4_2" ( EMPTY) }
	| arg3 = identifier { Globals.grdbg "clocking_decl_4" ((arg3);) }
	;

clocking_decl_7 : { Globals.grdbg "clocking_decl_7_2" ( EMPTY) }
	| arg3 = clocking_decl_7 arg4 = clocking_item { Globals.grdbg "clocking_decl_7" (DOUBLE(arg3, arg4);) }
	;

dpi_dimension_4 : { Globals.grdbg "dpi_dimension_4_2" ( EMPTY) }
	| arg3 = dpi_dimension_4 LBRACK RBRACK { Globals.grdbg "dpi_dimension_4" (TRIPLE(arg3, LBRACK, RBRACK);) }
	;

parameter_port_list_5 : { Globals.grdbg "parameter_port_list_5_2" ( EMPTY) }
	| arg3 = parameter_port_list_5 COMMA arg5 = parameter_declaration { Globals.grdbg "parameter_port_list_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline clocking_decl_assign_3 : { Globals.grdbg "clocking_decl_assign_3_2" ( EMPTY) }
	| EQUALS arg4 = hierarchical_identifier { Globals.grdbg "clocking_decl_assign_3" (DOUBLE(EQUALS, arg4);) }
	;

%inline source_text_2 : { Globals.grdbg "source_text_2_2" ( EMPTY) }
	| arg3 = timeunits_declaration { Globals.grdbg "source_text_2" ((arg3);) }
	;

source_text_3 : { Globals.grdbg "source_text_3_2" ( EMPTY) }
	| arg3 = source_text_3 arg4 = description { Globals.grdbg "source_text_3" (DOUBLE(arg3, arg4);) }
	;

%inline genvar_decl_assignment_2 : { Globals.grdbg "genvar_decl_assignment_2_2" ( EMPTY) }
	| GENVAR { Globals.grdbg "genvar_decl_assignment_2" ((GENVAR);) }
	;

%inline extern_constraint_declaration_2 : { Globals.grdbg "extern_constraint_declaration_2_2" ( EMPTY) }
	| STATIC { Globals.grdbg "extern_constraint_declaration_2" ((STATIC);) }
	;

extern_constraint_declaration_8 : { Globals.grdbg "extern_constraint_declaration_8_2" ( EMPTY) }
	| arg3 = extern_constraint_declaration_8 arg4 = constraint_block { Globals.grdbg "extern_constraint_declaration_8" (DOUBLE(arg3, arg4);) }
	;

class_method_2 : { Globals.grdbg "class_method_2_2" ( EMPTY) }
	| arg3 = class_method_2 arg4 = method_qualifier { Globals.grdbg "class_method_2" (DOUBLE(arg3, arg4);) }
	;

class_method_5 : { Globals.grdbg "class_method_5_2" ( EMPTY) }
	| arg3 = class_method_5 arg4 = method_qualifier { Globals.grdbg "class_method_5" (DOUBLE(arg3, arg4);) }
	;

class_method_9 : { Globals.grdbg "class_method_9_2" ( EMPTY) }
	| arg3 = class_method_9 arg4 = method_qualifier { Globals.grdbg "class_method_9" (DOUBLE(arg3, arg4);) }
	;

%inline generate_interface_block_3 : { Globals.grdbg "generate_interface_block_3_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "generate_interface_block_3" (DOUBLE(COLON, arg4);) }
	;

generate_interface_block_4 : { Globals.grdbg "generate_interface_block_4_2" ( EMPTY) }
	| arg3 = generate_interface_block_4 arg4 = generate_interface_item { Globals.grdbg "generate_interface_block_4" (DOUBLE(arg3, arg4);) }
	;

%inline generate_interface_block_6 : { Globals.grdbg "generate_interface_block_6_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "generate_interface_block_6" (DOUBLE(COLON, arg4);) }
	;

function_case_item_3 : { Globals.grdbg "function_case_item_3_2" ( EMPTY) }
	| arg3 = function_case_item_3 COMMA arg5 = expression { Globals.grdbg "function_case_item_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline function_case_item_8 : { Globals.grdbg "function_case_item_8_2" ( EMPTY) }
	| COLON { Globals.grdbg "function_case_item_8" ((COLON);) }
	;

list_of_net_identifiers_3 : { Globals.grdbg "list_of_net_identifiers_3_2" ( EMPTY) }
	| arg3 = list_of_net_identifiers_3 arg4 = unpacked_dimension { Globals.grdbg "list_of_net_identifiers_3" (DOUBLE(arg3, arg4);) }
	;

list_of_net_identifiers_4_6 : { Globals.grdbg "list_of_net_identifiers_4_6_2" ( EMPTY) }
	| arg3 = list_of_net_identifiers_4_6 arg4 = unpacked_dimension { Globals.grdbg "list_of_net_identifiers_4_6" (DOUBLE(arg3, arg4);) }
	;

list_of_net_identifiers_4 : { Globals.grdbg "list_of_net_identifiers_4_2" ( EMPTY) }
	| arg3 = list_of_net_identifiers_4 COMMA arg5 = identifier arg6 = list_of_net_identifiers_4_6 { Globals.grdbg "list_of_net_identifiers_4" (QUADRUPLE(arg3, COMMA, arg5, arg6);) }
	;

%inline constraint_expression_14 : { Globals.grdbg "constraint_expression_14_2" ( EMPTY) }
	| ELSE arg4 = constraint_set { Globals.grdbg "constraint_expression_14" (DOUBLE(ELSE, arg4);) }
	;

%inline input_declaration_3 : { Globals.grdbg "input_declaration_3_2" ( EMPTY) }
	| arg3 = port_type { Globals.grdbg "input_declaration_3" ((arg3);) }
	;

comment_text_2 : { Globals.grdbg "comment_text_2_2" ( EMPTY) }
	| arg3 = comment_text_2 ANY_ASCII_CHAR { Globals.grdbg "comment_text_2" (DOUBLE(arg3, ANY_ASCII_CHAR);) }
	;

%inline program_nonansi_header_4 : { Globals.grdbg "program_nonansi_header_4_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "program_nonansi_header_4" ((arg3);) }
	;

%inline program_nonansi_header_6 : { Globals.grdbg "program_nonansi_header_6_2" ( EMPTY) }
	| arg3 = parameter_port_list { Globals.grdbg "program_nonansi_header_6" ((arg3);) }
	;

list_of_variable_assignments_3 : { Globals.grdbg "list_of_variable_assignments_3_2" ( EMPTY) }
	| arg3 = list_of_variable_assignments_3 COMMA arg5 = variable_assignment { Globals.grdbg "list_of_variable_assignments_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

edge_input_list_2 : { Globals.grdbg "edge_input_list_2_2" ( EMPTY) }
	| arg3 = edge_input_list_2 arg4 = LEVEL_SYMBOL { Globals.grdbg "edge_input_list_2" (DOUBLE(arg3, LEVEL_SYMBOL arg4);) }
	;

edge_input_list_4 : { Globals.grdbg "edge_input_list_4_2" ( EMPTY) }
	| arg3 = edge_input_list_4 arg4 = LEVEL_SYMBOL { Globals.grdbg "edge_input_list_4" (DOUBLE(arg3, LEVEL_SYMBOL arg4);) }
	;

list_of_variable_decl_assignments_3 : { Globals.grdbg "list_of_variable_decl_assignments_3_2" ( EMPTY) }
	| arg3 = list_of_variable_decl_assignments_3 COMMA arg5 = variable_decl_assignment { Globals.grdbg "list_of_variable_decl_assignments_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline list_of_variable_port_identifiers_4 : { Globals.grdbg "list_of_variable_port_identifiers_4_2" ( EMPTY) }
	| EQUALS arg4 = constant_expression { Globals.grdbg "list_of_variable_port_identifiers_4" (DOUBLE(EQUALS, arg4);) }
	;

%inline list_of_variable_port_identifiers_5_7 : { Globals.grdbg "list_of_variable_port_identifiers_5_7_2" ( EMPTY) }
	| EQUALS arg4 = constant_expression { Globals.grdbg "list_of_variable_port_identifiers_5_7" (DOUBLE(EQUALS, arg4);) }
	;

list_of_variable_port_identifiers_5 : { Globals.grdbg "list_of_variable_port_identifiers_5_2" ( EMPTY) }
	| arg3 = list_of_variable_port_identifiers_5 COMMA arg5 = identifier arg6 = variable_dimension arg7 = list_of_variable_port_identifiers_5_7 { Globals.grdbg "list_of_variable_port_identifiers_5" (QUINTUPLE(arg3, COMMA, arg5, arg6, arg7);) }
	;

%inline property_instance_3 : { Globals.grdbg "property_instance_3_2" ( EMPTY) }
	| LPAREN arg4 = actual_arg_list RPAREN { Globals.grdbg "property_instance_3" (TRIPLE(LPAREN, arg4, RPAREN);) }
	;

%inline constraint_prototype_2 : { Globals.grdbg "constraint_prototype_2_2" ( EMPTY) }
	| STATIC { Globals.grdbg "constraint_prototype_2" ((STATIC);) }
	;

case_item_3 : { Globals.grdbg "case_item_3_2" ( EMPTY) }
	| arg3 = case_item_3 COMMA arg5 = expression { Globals.grdbg "case_item_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline case_item_8 : { Globals.grdbg "case_item_8_2" ( EMPTY) }
	| COLON { Globals.grdbg "case_item_8" ((COLON);) }
	;

%inline modport_hierarchical_ports_declaration_3 : { Globals.grdbg "modport_hierarchical_ports_declaration_3_2" ( EMPTY) }
	| LBRACK arg4 = constant_expression RBRACK { Globals.grdbg "modport_hierarchical_ports_declaration_3" (TRIPLE(LBRACK, arg4, RBRACK);) }
	;

%inline parallel_path_description_4 : { Globals.grdbg "parallel_path_description_4_2" ( EMPTY) }
	| arg3 = polarity_operator { Globals.grdbg "parallel_path_description_4" ((arg3);) }
	;

%inline d_width_timing_check_9_4 : { Globals.grdbg "d_width_timing_check_9_4_2" ( EMPTY) }
	| arg3 = variable_identifier { Globals.grdbg "d_width_timing_check_9_4" ((arg3);) }
	;

%inline d_width_timing_check_9 : { Globals.grdbg "d_width_timing_check_9_2" ( EMPTY) }
	| COMMA arg4 = d_width_timing_check_9_4 { Globals.grdbg "d_width_timing_check_9" (DOUBLE(COMMA, arg4);) }
	;

function_seq_block_3_5 : { Globals.grdbg "function_seq_block_3_5_2" ( EMPTY) }
	| arg3 = function_seq_block_3_5 arg4 = block_item_declaration { Globals.grdbg "function_seq_block_3_5" (DOUBLE(arg3, arg4);) }
	;

%inline function_seq_block_3 : { Globals.grdbg "function_seq_block_3_2" ( EMPTY) }
	| COLON arg4 = identifier arg5 = function_seq_block_3_5 { Globals.grdbg "function_seq_block_3" (TRIPLE(COLON, arg4, arg5);) }
	;

function_seq_block_4 : { Globals.grdbg "function_seq_block_4_2" ( EMPTY) }
	| arg3 = function_seq_block_4 arg4 = function_statement_or_null { Globals.grdbg "function_seq_block_4" (DOUBLE(arg3, arg4);) }
	;

%inline function_seq_block_6 : { Globals.grdbg "function_seq_block_6_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "function_seq_block_6" (DOUBLE(COLON, arg4);) }
	;

%inline block_variable_declaration_2 : { Globals.grdbg "block_variable_declaration_2_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "block_variable_declaration_2" ((arg3);) }
	;

%inline tf_inout_declaration_3 : { Globals.grdbg "tf_inout_declaration_3_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "tf_inout_declaration_3" ((arg3);) }
	;

tf_inout_declaration_4 : { Globals.grdbg "tf_inout_declaration_4_2" ( EMPTY) }
	| arg3 = tf_inout_declaration_4 arg4 = packed_dimension { Globals.grdbg "tf_inout_declaration_4" (DOUBLE(arg3, arg4);) }
	;

generate_interface_case_statement_7 : { Globals.grdbg "generate_interface_case_statement_7_2" ( EMPTY) }
	| arg3 = generate_interface_case_statement_7 arg4 = genvar_interface_case_item { Globals.grdbg "generate_interface_case_statement_7" (DOUBLE(arg3, arg4);) }
	;

generated_interface_instantiation_3 : { Globals.grdbg "generated_interface_instantiation_3_2" ( EMPTY) }
	| arg3 = generated_interface_instantiation_3 arg4 = generate_interface_item { Globals.grdbg "generated_interface_instantiation_3" (DOUBLE(arg3, arg4);) }
	;

actual_arg_list_4 : { Globals.grdbg "actual_arg_list_4_2" ( EMPTY) }
	| arg3 = actual_arg_list_4 COMMA arg5 = event_expression { Globals.grdbg "actual_arg_list_4" (TRIPLE(arg3, COMMA, arg5);) }
	;

actual_arg_list_13 : { Globals.grdbg "actual_arg_list_13_2" ( EMPTY) }
	| arg3 = actual_arg_list_13 COMMA DOT arg6 = identifier LPAREN arg8 = event_expression RPAREN { Globals.grdbg "actual_arg_list_13" (SEPTUPLE(arg3, COMMA, DOT, arg6, LPAREN, arg8, RPAREN);) }
	;

list_of_udp_port_identifiers_3 : { Globals.grdbg "list_of_udp_port_identifiers_3_2" ( EMPTY) }
	| arg3 = list_of_udp_port_identifiers_3 COMMA arg5 = identifier { Globals.grdbg "list_of_udp_port_identifiers_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline n_output_gate_instance_2 : { Globals.grdbg "n_output_gate_instance_2_2" ( EMPTY) }
	| arg3 = name_of_gate_instance { Globals.grdbg "n_output_gate_instance_2" ((arg3);) }
	;

n_output_gate_instance_5 : { Globals.grdbg "n_output_gate_instance_5_2" ( EMPTY) }
	| arg3 = n_output_gate_instance_5 COMMA arg5 = net_lvalue { Globals.grdbg "n_output_gate_instance_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline task_body_declaration_2 : { Globals.grdbg "task_body_declaration_2_2" ( EMPTY) }
	| arg3 = identifier DOT { Globals.grdbg "task_body_declaration_2" (DOUBLE(arg3, DOT);) }
	;

task_body_declaration_5 : { Globals.grdbg "task_body_declaration_5_2" ( EMPTY) }
	| arg3 = task_body_declaration_5 arg4 = task_item_declaration { Globals.grdbg "task_body_declaration_5" (DOUBLE(arg3, arg4);) }
	;

task_body_declaration_6 : { Globals.grdbg "task_body_declaration_6_2" ( EMPTY) }
	| arg3 = task_body_declaration_6 arg4 = statement_or_null { Globals.grdbg "task_body_declaration_6" (DOUBLE(arg3, arg4);) }
	;

%inline task_body_declaration_8 : { Globals.grdbg "task_body_declaration_8_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "task_body_declaration_8" (DOUBLE(COLON, arg4);) }
	;

%inline task_body_declaration_10 : { Globals.grdbg "task_body_declaration_10_2" ( EMPTY) }
	| arg3 = identifier DOT { Globals.grdbg "task_body_declaration_10" (DOUBLE(arg3, DOT);) }
	;

task_body_declaration_16 : { Globals.grdbg "task_body_declaration_16_2" ( EMPTY) }
	| arg3 = task_body_declaration_16 arg4 = block_item_declaration { Globals.grdbg "task_body_declaration_16" (DOUBLE(arg3, arg4);) }
	;

task_body_declaration_17 : { Globals.grdbg "task_body_declaration_17_2" ( EMPTY) }
	| arg3 = task_body_declaration_17 arg4 = statement_or_null { Globals.grdbg "task_body_declaration_17" (DOUBLE(arg3, arg4);) }
	;

%inline task_body_declaration_19 : { Globals.grdbg "task_body_declaration_19_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "task_body_declaration_19" (DOUBLE(COLON, arg4);) }
	;

%inline sequence_instance_3 : { Globals.grdbg "sequence_instance_3_2" ( EMPTY) }
	| LPAREN arg4 = actual_arg_list RPAREN { Globals.grdbg "sequence_instance_3" (TRIPLE(LPAREN, arg4, RPAREN);) }
	;

%inline use_clause_3 : { Globals.grdbg "use_clause_3_2" ( EMPTY) }
	| arg3 = identifier DOT { Globals.grdbg "use_clause_3" (DOUBLE(arg3, DOT);) }
	;

%inline use_clause_5 : { Globals.grdbg "use_clause_5_2" ( EMPTY) }
	| COLON CONFIG { Globals.grdbg "use_clause_5" (DOUBLE(COLON, CONFIG);) }
	;

property_formal_list_4 : { Globals.grdbg "property_formal_list_4_2" ( EMPTY) }
	| arg3 = property_formal_list_4 COMMA arg5 = formal_list_item { Globals.grdbg "property_formal_list_4" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline wait_statement_15 : { Globals.grdbg "wait_statement_15_2" ( EMPTY) }
	| COMMA arg4 = hierarchical_identifier { Globals.grdbg "wait_statement_15" (DOUBLE(COMMA, arg4);) }
	;

%inline seq_block_3 : { Globals.grdbg "seq_block_3_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "seq_block_3" (DOUBLE(COLON, arg4);) }
	;

seq_block_4 : { Globals.grdbg "seq_block_4_2" ( EMPTY) }
	| arg3 = seq_block_4 arg4 = block_item_declaration { Globals.grdbg "seq_block_4" (DOUBLE(arg3, arg4);) }
	;

seq_block_5 : { Globals.grdbg "seq_block_5_2" ( EMPTY) }
	| arg3 = seq_block_5 arg4 = statement_or_null { Globals.grdbg "seq_block_5" (DOUBLE(arg3, arg4);) }
	;

%inline seq_block_7 : { Globals.grdbg "seq_block_7_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "seq_block_7" (DOUBLE(COLON, arg4);) }
	;

%inline constraint_declaration_2 : { Globals.grdbg "constraint_declaration_2_2" ( EMPTY) }
	| STATIC { Globals.grdbg "constraint_declaration_2" ((STATIC);) }
	;

constraint_declaration_6 : { Globals.grdbg "constraint_declaration_6_2" ( EMPTY) }
	| arg3 = constraint_declaration_6 arg4 = constraint_block { Globals.grdbg "constraint_declaration_6" (DOUBLE(arg3, arg4);) }
	;

%inline full_edge_sensitive_path_description_3 : { Globals.grdbg "full_edge_sensitive_path_description_3_2" ( EMPTY) }
	| arg3 = edge_identifier { Globals.grdbg "full_edge_sensitive_path_description_3" ((arg3);) }
	;

%inline full_edge_sensitive_path_description_7 : { Globals.grdbg "full_edge_sensitive_path_description_7_2" ( EMPTY) }
	| arg3 = polarity_operator { Globals.grdbg "full_edge_sensitive_path_description_7" ((arg3);) }
	;

%inline program_ansi_header_4 : { Globals.grdbg "program_ansi_header_4_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "program_ansi_header_4" ((arg3);) }
	;

%inline program_ansi_header_6 : { Globals.grdbg "program_ansi_header_6_2" ( EMPTY) }
	| arg3 = parameter_port_list { Globals.grdbg "program_ansi_header_6" ((arg3);) }
	;

%inline program_ansi_header_7 : { Globals.grdbg "program_ansi_header_7_2" ( EMPTY) }
	| arg3 = list_of_port_declarations { Globals.grdbg "program_ansi_header_7" ((arg3);) }
	;

list_of_net_assignments_3 : { Globals.grdbg "list_of_net_assignments_3_2" ( EMPTY) }
	| arg3 = list_of_net_assignments_3 COMMA arg5 = net_assignment { Globals.grdbg "list_of_net_assignments_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline tf_ref_declaration_2 : { Globals.grdbg "tf_ref_declaration_2_2" ( EMPTY) }
	| CONST { Globals.grdbg "tf_ref_declaration_2" ((CONST);) }
	;

escaped_hierarchical_branch_3 : { Globals.grdbg "escaped_hierarchical_branch_3_2" ( EMPTY) }
	| arg3 = escaped_hierarchical_branch_3 LBRACK arg5 = UNSIGNED_NUMBER RBRACK { Globals.grdbg "escaped_hierarchical_branch_3" (QUADRUPLE(arg3, LBRACK, UNSIGNED_NUMBER arg5, RBRACK);) }
	;

escaped_hierarchical_branch_4_3_6 : { Globals.grdbg "escaped_hierarchical_branch_4_3_6_2" ( EMPTY) }
	| arg3 = escaped_hierarchical_branch_4_3_6 LBRACK arg5 = UNSIGNED_NUMBER RBRACK { Globals.grdbg "escaped_hierarchical_branch_4_3_6" (QUADRUPLE(arg3, LBRACK, UNSIGNED_NUMBER arg5, RBRACK);) }
	;

escaped_hierarchical_branch_4_3 : { Globals.grdbg "escaped_hierarchical_branch_4_3_2" ( EMPTY) }
	| arg3 = escaped_hierarchical_branch_4_3 DOT arg5 = escaped_identifier arg6 = escaped_hierarchical_branch_4_3_6 { Globals.grdbg "escaped_hierarchical_branch_4_3" (QUADRUPLE(arg3, DOT, arg5, arg6);) }
	;

%inline escaped_hierarchical_branch_4 : { Globals.grdbg "escaped_hierarchical_branch_4_2" ( EMPTY) }
	| arg3 = escaped_hierarchical_branch_4_3 { Globals.grdbg "escaped_hierarchical_branch_4" ((arg3);) }
	;

%inline system_task_enable_3_4 : { Globals.grdbg "system_task_enable_3_4_2" ( EMPTY) }
	| arg3 = expression { Globals.grdbg "system_task_enable_3_4" ((arg3);) }
	;

%inline system_task_enable_3_5_5 : { Globals.grdbg "system_task_enable_3_5_5_2" ( EMPTY) }
	| arg3 = expression { Globals.grdbg "system_task_enable_3_5_5" ((arg3);) }
	;

system_task_enable_3_5 : { Globals.grdbg "system_task_enable_3_5_2" ( EMPTY) }
	| arg3 = system_task_enable_3_5 COMMA arg5 = system_task_enable_3_5_5 { Globals.grdbg "system_task_enable_3_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline system_task_enable_3 : { Globals.grdbg "system_task_enable_3_2" ( EMPTY) }
	| LPAREN arg4 = system_task_enable_3_4 arg5 = system_task_enable_3_5 RPAREN { Globals.grdbg "system_task_enable_3" (QUADRUPLE(LPAREN, arg4, arg5, RPAREN);) }
	;

function_port_list_3 : { Globals.grdbg "function_port_list_3_2" ( EMPTY) }
	| arg3 = function_port_list_3 COMMA arg5 = function_port_item { Globals.grdbg "function_port_list_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline module_declaration_3 : { Globals.grdbg "module_declaration_3_2" ( EMPTY) }
	| arg3 = timeunits_declaration { Globals.grdbg "module_declaration_3" ((arg3);) }
	;

module_declaration_4 : { Globals.grdbg "module_declaration_4_2" ( EMPTY) }
	| arg3 = module_declaration_4 arg4 = module_item { Globals.grdbg "module_declaration_4" (DOUBLE(arg3, arg4);) }
	;

%inline module_declaration_6 : { Globals.grdbg "module_declaration_6_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "module_declaration_6" (DOUBLE(COLON, arg4);) }
	;

%inline module_declaration_9 : { Globals.grdbg "module_declaration_9_2" ( EMPTY) }
	| arg3 = timeunits_declaration { Globals.grdbg "module_declaration_9" ((arg3);) }
	;

module_declaration_10 : { Globals.grdbg "module_declaration_10_2" ( EMPTY) }
	| arg3 = module_declaration_10 arg4 = non_port_module_item { Globals.grdbg "module_declaration_10" (DOUBLE(arg3, arg4);) }
	;

%inline module_declaration_12 : { Globals.grdbg "module_declaration_12_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "module_declaration_12" (DOUBLE(COLON, arg4);) }
	;

%inline module_declaration_16 : { Globals.grdbg "module_declaration_16_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "module_declaration_16" ((arg3);) }
	;

%inline module_declaration_22 : { Globals.grdbg "module_declaration_22_2" ( EMPTY) }
	| arg3 = timeunits_declaration { Globals.grdbg "module_declaration_22" ((arg3);) }
	;

module_declaration_23 : { Globals.grdbg "module_declaration_23_2" ( EMPTY) }
	| arg3 = module_declaration_23 arg4 = module_item { Globals.grdbg "module_declaration_23" (DOUBLE(arg3, arg4);) }
	;

%inline module_declaration_25 : { Globals.grdbg "module_declaration_25_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "module_declaration_25" (DOUBLE(COLON, arg4);) }
	;

%inline delay2_8 : { Globals.grdbg "delay2_8_2" ( EMPTY) }
	| COMMA arg4 = mintypmax_expression { Globals.grdbg "delay2_8" (DOUBLE(COMMA, arg4);) }
	;

%inline delay3_8_5 : { Globals.grdbg "delay3_8_5_2" ( EMPTY) }
	| COMMA arg4 = mintypmax_expression { Globals.grdbg "delay3_8_5" (DOUBLE(COMMA, arg4);) }
	;

%inline delay3_8 : { Globals.grdbg "delay3_8_2" ( EMPTY) }
	| COMMA arg4 = mintypmax_expression arg5 = delay3_8_5 { Globals.grdbg "delay3_8" (TRIPLE(COMMA, arg4, arg5);) }
	;

%inline parallel_edge_sensitive_path_description_3 : { Globals.grdbg "parallel_edge_sensitive_path_description_3_2" ( EMPTY) }
	| arg3 = edge_identifier { Globals.grdbg "parallel_edge_sensitive_path_description_3" ((arg3);) }
	;

%inline parallel_edge_sensitive_path_description_7 : { Globals.grdbg "parallel_edge_sensitive_path_description_7_2" ( EMPTY) }
	| arg3 = polarity_operator { Globals.grdbg "parallel_edge_sensitive_path_description_7" ((arg3);) }
	;

non_zero_unsigned_number_3 : { Globals.grdbg "non_zero_unsigned_number_3_2" ( EMPTY) }
	| arg3 = non_zero_unsigned_number_3 UNDERSCORE { Globals.grdbg "non_zero_unsigned_number_3_5" (DOUBLE(arg3, UNDERSCORE);) }
	| arg6 = decimal_digit { Globals.grdbg "non_zero_unsigned_number_3" ((arg6);) }
	;

%inline property_expr_6 : { Globals.grdbg "property_expr_6_2" ( EMPTY) }
	| NOT { Globals.grdbg "property_expr_6" ((NOT);) }
	;

%inline property_expr_11 : { Globals.grdbg "property_expr_11_2" ( EMPTY) }
	| NOT { Globals.grdbg "property_expr_11" ((NOT);) }
	;

%inline module_nonansi_header_4 : { Globals.grdbg "module_nonansi_header_4_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "module_nonansi_header_4" ((arg3);) }
	;

%inline module_nonansi_header_6 : { Globals.grdbg "module_nonansi_header_6_2" ( EMPTY) }
	| arg3 = parameter_port_list { Globals.grdbg "module_nonansi_header_6" ((arg3);) }
	;

%inline d_recovery_timing_check_9_4 : { Globals.grdbg "d_recovery_timing_check_9_4_2" ( EMPTY) }
	| arg3 = variable_identifier { Globals.grdbg "d_recovery_timing_check_9_4" ((arg3);) }
	;

%inline d_recovery_timing_check_9 : { Globals.grdbg "d_recovery_timing_check_9_2" ( EMPTY) }
	| COMMA arg4 = d_recovery_timing_check_9_4 { Globals.grdbg "d_recovery_timing_check_9" (DOUBLE(COMMA, arg4);) }
	;

%inline property_declaration_4 : { Globals.grdbg "property_declaration_4_2" ( EMPTY) }
	| arg3 = property_formal_list { Globals.grdbg "property_declaration_4" ((arg3);) }
	;

property_declaration_6 : { Globals.grdbg "property_declaration_6_2" ( EMPTY) }
	| arg3 = property_declaration_6 arg4 = assertion_variable_declaration { Globals.grdbg "property_declaration_6" (DOUBLE(arg3, arg4);) }
	;

%inline property_declaration_10 : { Globals.grdbg "property_declaration_10_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "property_declaration_10" (DOUBLE(COLON, arg4);) }
	;

name_of_gate_instance_3 : { Globals.grdbg "name_of_gate_instance_3_2" ( EMPTY) }
	| arg3 = name_of_gate_instance_3 arg4 = range { Globals.grdbg "name_of_gate_instance_3" (DOUBLE(arg3, arg4);) }
	;

%inline interface_ansi_header_4 : { Globals.grdbg "interface_ansi_header_4_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "interface_ansi_header_4" ((arg3);) }
	;

%inline interface_ansi_header_6 : { Globals.grdbg "interface_ansi_header_6_2" ( EMPTY) }
	| arg3 = parameter_port_list { Globals.grdbg "interface_ansi_header_6" ((arg3);) }
	;

%inline interface_ansi_header_7 : { Globals.grdbg "interface_ansi_header_7_2" ( EMPTY) }
	| arg3 = list_of_port_declarations { Globals.grdbg "interface_ansi_header_7" ((arg3);) }
	;

binary_value_3 : { Globals.grdbg "binary_value_3_2" ( EMPTY) }
	| arg3 = binary_value_3 UNDERSCORE { Globals.grdbg "binary_value_3_5" (DOUBLE(arg3, UNDERSCORE);) }
	| arg6 = binary_digit { Globals.grdbg "binary_value_3" ((arg6);) }
	;

program_instance_3 : { Globals.grdbg "program_instance_3_2" ( EMPTY) }
	| arg3 = program_instance_3 arg4 = range { Globals.grdbg "program_instance_3" (DOUBLE(arg3, arg4);) }
	;

%inline program_instance_5 : { Globals.grdbg "program_instance_5_2" ( EMPTY) }
	| arg3 = list_of_port_connections { Globals.grdbg "program_instance_5" ((arg3);) }
	;

%inline enable_gate_instance_2 : { Globals.grdbg "enable_gate_instance_2_2" ( EMPTY) }
	| arg3 = name_of_gate_instance { Globals.grdbg "enable_gate_instance_2" ((arg3);) }
	;

type_declaration_identifier_3 : { Globals.grdbg "type_declaration_identifier_3_2" ( EMPTY) }
	| arg3 = type_declaration_identifier_3 arg4 = unpacked_dimension { Globals.grdbg "type_declaration_identifier_3" (DOUBLE(arg3, arg4);) }
	;

%inline decimal_number_4 : { Globals.grdbg "decimal_number_4_2" ( EMPTY) }
	| arg3 = non_zero_unsigned_number { Globals.grdbg "decimal_number_4" ((arg3);) }
	;

%inline decimal_number_8 : { Globals.grdbg "decimal_number_8_2" ( EMPTY) }
	| arg3 = non_zero_unsigned_number { Globals.grdbg "decimal_number_8" ((arg3);) }
	;

decimal_number_11 : { Globals.grdbg "decimal_number_11_2" ( EMPTY) }
	| arg3 = decimal_number_11 UNDERSCORE { Globals.grdbg "decimal_number_11" (DOUBLE(arg3, UNDERSCORE);) }
	;

%inline decimal_number_13 : { Globals.grdbg "decimal_number_13_2" ( EMPTY) }
	| arg3 = non_zero_unsigned_number { Globals.grdbg "decimal_number_13" ((arg3);) }
	;

decimal_number_16 : { Globals.grdbg "decimal_number_16_2" ( EMPTY) }
	| arg3 = decimal_number_16 UNDERSCORE { Globals.grdbg "decimal_number_16" (DOUBLE(arg3, UNDERSCORE);) }
	;

%inline generate_module_conditional_statement_7 : { Globals.grdbg "generate_module_conditional_statement_7_2" ( EMPTY) }
	| ELSE arg4 = generate_module_item { Globals.grdbg "generate_module_conditional_statement_7" (DOUBLE(ELSE, arg4);) }
	;

%inline event_expression_2 : { Globals.grdbg "event_expression_2_2" ( EMPTY) }
	| arg3 = edge_identifier { Globals.grdbg "event_expression_2" ((arg3);) }
	;

%inline event_expression_4 : { Globals.grdbg "event_expression_4_2" ( EMPTY) }
	| IFF arg4 = expression { Globals.grdbg "event_expression_4" (DOUBLE(IFF, arg4);) }
	;

%inline output_declaration_3 : { Globals.grdbg "output_declaration_3_2" ( EMPTY) }
	| arg3 = port_type { Globals.grdbg "output_declaration_3" ((arg3);) }
	;

port_expression_6 : { Globals.grdbg "port_expression_6_2" ( EMPTY) }
	| arg3 = port_expression_6 COMMA arg5 = port_reference { Globals.grdbg "port_expression_6" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline udp_instantiation_3 : { Globals.grdbg "udp_instantiation_3_2" ( EMPTY) }
	| arg3 = drive_strength { Globals.grdbg "udp_instantiation_3" ((arg3);) }
	;

%inline udp_instantiation_4 : { Globals.grdbg "udp_instantiation_4_2" ( EMPTY) }
	| arg3 = delay2 { Globals.grdbg "udp_instantiation_4" ((arg3);) }
	;

udp_instantiation_6 : { Globals.grdbg "udp_instantiation_6_2" ( EMPTY) }
	| arg3 = udp_instantiation_6 COMMA arg5 = udp_instance { Globals.grdbg "udp_instantiation_6" (TRIPLE(arg3, COMMA, arg5);) }
	;

range_list_or_array_6 : { Globals.grdbg "range_list_or_array_6_2" ( EMPTY) }
	| arg3 = range_list_or_array_6 COMMA arg5 = value_range { Globals.grdbg "range_list_or_array_6" (TRIPLE(arg3, COMMA, arg5);) }
	;

list_of_parameter_assignments_3 : { Globals.grdbg "list_of_parameter_assignments_3_2" ( EMPTY) }
	| arg3 = list_of_parameter_assignments_3 COMMA arg5 = ordered_parameter_assignment { Globals.grdbg "list_of_parameter_assignments_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

list_of_parameter_assignments_6 : { Globals.grdbg "list_of_parameter_assignments_6_2" ( EMPTY) }
	| arg3 = list_of_parameter_assignments_6 COMMA arg5 = named_parameter_assignment { Globals.grdbg "list_of_parameter_assignments_6" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline d_fullskew_timing_check_11_4 : { Globals.grdbg "d_fullskew_timing_check_11_4_2" ( EMPTY) }
	| arg3 = variable_identifier { Globals.grdbg "d_fullskew_timing_check_11_4" ((arg3);) }
	;

%inline d_fullskew_timing_check_11_5_4 : { Globals.grdbg "d_fullskew_timing_check_11_5_4_2" ( EMPTY) }
	| arg3 = constant_expression { Globals.grdbg "d_fullskew_timing_check_11_5_4" ((arg3);) }
	;

%inline d_fullskew_timing_check_11_5_5_4 : { Globals.grdbg "d_fullskew_timing_check_11_5_5_4_2" ( EMPTY) }
	| arg3 = constant_mintypmax_expression { Globals.grdbg "d_fullskew_timing_check_11_5_5_4" ((arg3);) }
	;

%inline d_fullskew_timing_check_11_5_5 : { Globals.grdbg "d_fullskew_timing_check_11_5_5_2" ( EMPTY) }
	| COMMA arg4 = d_fullskew_timing_check_11_5_5_4 { Globals.grdbg "d_fullskew_timing_check_11_5_5" (DOUBLE(COMMA, arg4);) }
	;

%inline d_fullskew_timing_check_11_5 : { Globals.grdbg "d_fullskew_timing_check_11_5_2" ( EMPTY) }
	| COMMA arg4 = d_fullskew_timing_check_11_5_4 arg5 = d_fullskew_timing_check_11_5_5 { Globals.grdbg "d_fullskew_timing_check_11_5" (TRIPLE(COMMA, arg4, arg5);) }
	;

%inline d_fullskew_timing_check_11 : { Globals.grdbg "d_fullskew_timing_check_11_2" ( EMPTY) }
	| COMMA arg4 = d_fullskew_timing_check_11_4 arg5 = d_fullskew_timing_check_11_5 { Globals.grdbg "d_fullskew_timing_check_11" (TRIPLE(COMMA, arg4, arg5);) }
	;

%inline d_skew_timing_check_9_4 : { Globals.grdbg "d_skew_timing_check_9_4_2" ( EMPTY) }
	| arg3 = variable_identifier { Globals.grdbg "d_skew_timing_check_9_4" ((arg3);) }
	;

%inline d_skew_timing_check_9 : { Globals.grdbg "d_skew_timing_check_9_2" ( EMPTY) }
	| COMMA arg4 = d_skew_timing_check_9_4 { Globals.grdbg "d_skew_timing_check_9" (DOUBLE(COMMA, arg4);) }
	;

%inline generate_module_block_3 : { Globals.grdbg "generate_module_block_3_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "generate_module_block_3" (DOUBLE(COLON, arg4);) }
	;

generate_module_block_4 : { Globals.grdbg "generate_module_block_4_2" ( EMPTY) }
	| arg3 = generate_module_block_4 arg4 = generate_module_item { Globals.grdbg "generate_module_block_4" (DOUBLE(arg3, arg4);) }
	;

%inline generate_module_block_6 : { Globals.grdbg "generate_module_block_6_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "generate_module_block_6" (DOUBLE(COLON, arg4);) }
	;

%inline type_declaration_3 : { Globals.grdbg "type_declaration_3_2" ( EMPTY) }
	| arg3 = data_type { Globals.grdbg "type_declaration_3" ((arg3);) }
	;

%inline type_declaration_15 : { Globals.grdbg "type_declaration_15_2" ( EMPTY) }
	| CLASS { Globals.grdbg "type_declaration_15" ((CLASS);) }
	;

%inline type_declaration_21 : { Globals.grdbg "type_declaration_21_2" ( EMPTY) }
	| arg3 = parameter_value_assignment { Globals.grdbg "type_declaration_21" ((arg3);) }
	;

%inline port_type_5 : { Globals.grdbg "port_type_5_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "port_type_5" ((arg3);) }
	;

port_type_6 : { Globals.grdbg "port_type_6_2" ( EMPTY) }
	| arg3 = port_type_6 arg4 = packed_dimension { Globals.grdbg "port_type_6" (DOUBLE(arg3, arg4);) }
	;

%inline port_type_9 : { Globals.grdbg "port_type_9_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "port_type_9" ((arg3);) }
	;

port_type_10 : { Globals.grdbg "port_type_10_2" ( EMPTY) }
	| arg3 = port_type_10 arg4 = packed_dimension { Globals.grdbg "port_type_10" (DOUBLE(arg3, arg4);) }
	;

%inline port_type_12 : { Globals.grdbg "port_type_12_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "port_type_12" ((arg3);) }
	;

port_type_13 : { Globals.grdbg "port_type_13_2" ( EMPTY) }
	| arg3 = port_type_13 arg4 = packed_dimension { Globals.grdbg "port_type_13" (DOUBLE(arg3, arg4);) }
	;

%inline d_timeskew_timing_check_9_4 : { Globals.grdbg "d_timeskew_timing_check_9_4_2" ( EMPTY) }
	| arg3 = variable_identifier { Globals.grdbg "d_timeskew_timing_check_9_4" ((arg3);) }
	;

%inline d_timeskew_timing_check_9_5_4 : { Globals.grdbg "d_timeskew_timing_check_9_5_4_2" ( EMPTY) }
	| arg3 = constant_expression { Globals.grdbg "d_timeskew_timing_check_9_5_4" ((arg3);) }
	;

%inline d_timeskew_timing_check_9_5_5_4 : { Globals.grdbg "d_timeskew_timing_check_9_5_5_4_2" ( EMPTY) }
	| arg3 = constant_mintypmax_expression { Globals.grdbg "d_timeskew_timing_check_9_5_5_4" ((arg3);) }
	;

%inline d_timeskew_timing_check_9_5_5 : { Globals.grdbg "d_timeskew_timing_check_9_5_5_2" ( EMPTY) }
	| COMMA arg4 = d_timeskew_timing_check_9_5_5_4 { Globals.grdbg "d_timeskew_timing_check_9_5_5" (DOUBLE(COMMA, arg4);) }
	;

%inline d_timeskew_timing_check_9_5 : { Globals.grdbg "d_timeskew_timing_check_9_5_2" ( EMPTY) }
	| COMMA arg4 = d_timeskew_timing_check_9_5_4 arg5 = d_timeskew_timing_check_9_5_5 { Globals.grdbg "d_timeskew_timing_check_9_5" (TRIPLE(COMMA, arg4, arg5);) }
	;

%inline d_timeskew_timing_check_9 : { Globals.grdbg "d_timeskew_timing_check_9_2" ( EMPTY) }
	| COMMA arg4 = d_timeskew_timing_check_9_4 arg5 = d_timeskew_timing_check_9_5 { Globals.grdbg "d_timeskew_timing_check_9" (TRIPLE(COMMA, arg4, arg5);) }
	;

%inline named_port_connection_6 : { Globals.grdbg "named_port_connection_6_2" ( EMPTY) }
	| arg3 = expression { Globals.grdbg "named_port_connection_6" ((arg3);) }
	;

%inline dpi_import_export_4 : { Globals.grdbg "dpi_import_export_4_2" ( EMPTY) }
	| arg3 = dpi_import_property { Globals.grdbg "dpi_import_export_4" ((arg3);) }
	;

%inline dpi_import_export_5 : { Globals.grdbg "dpi_import_export_5_2" ( EMPTY) }
	| arg3 = C_IDENTIFIER EQUALS { Globals.grdbg "dpi_import_export_5" (DOUBLE(C_IDENTIFIER arg3, EQUALS);) }
	;

%inline dpi_import_export_11 : { Globals.grdbg "dpi_import_export_11_2" ( EMPTY) }
	| arg3 = C_IDENTIFIER EQUALS { Globals.grdbg "dpi_import_export_11" (DOUBLE(C_IDENTIFIER arg3, EQUALS);) }
	;

%inline property_spec_2 : { Globals.grdbg "property_spec_2_2" ( EMPTY) }
	| arg3 = clocking_event { Globals.grdbg "property_spec_2" ((arg3);) }
	;

%inline property_spec_3 : { Globals.grdbg "property_spec_3_2" ( EMPTY) }
	| DISABLE IFF { Globals.grdbg "property_spec_3" (DOUBLE(DISABLE, IFF);) }
	;

%inline property_spec_7 : { Globals.grdbg "property_spec_7_2" ( EMPTY) }
	| NOT { Globals.grdbg "property_spec_7" ((NOT);) }
	;

%inline property_spec_10 : { Globals.grdbg "property_spec_10_2" ( EMPTY) }
	| DISABLE IFF LPAREN arg6 = expression RPAREN { Globals.grdbg "property_spec_10" (QUINTUPLE(DISABLE, IFF, LPAREN, arg6, RPAREN);) }
	;

%inline property_spec_11 : { Globals.grdbg "property_spec_11_2" ( EMPTY) }
	| NOT { Globals.grdbg "property_spec_11" ((NOT);) }
	;

class_property_2 : { Globals.grdbg "class_property_2_2" ( EMPTY) }
	| arg3 = class_property_2 arg4 = property_qualifier { Globals.grdbg "class_property_2" (DOUBLE(arg3, arg4);) }
	;

class_property_6 : { Globals.grdbg "class_property_6_2" ( EMPTY) }
	| arg3 = class_property_6 arg4 = class_item_qualifier { Globals.grdbg "class_property_6" (DOUBLE(arg3, arg4);) }
	;

%inline class_property_9 : { Globals.grdbg "class_property_9_2" ( EMPTY) }
	| EQUALS arg4 = constant_expression { Globals.grdbg "class_property_9" (DOUBLE(EQUALS, arg4);) }
	;

%inline real_number_5 : { Globals.grdbg "real_number_5_2" ( EMPTY) }
	| DOT arg4 = UNSIGNED_NUMBER { Globals.grdbg "real_number_5" (DOUBLE(DOT, UNSIGNED_NUMBER arg4);) }
	;

%inline real_number_7 : { Globals.grdbg "real_number_7_2" ( EMPTY) }
	| arg3 = sign { Globals.grdbg "real_number_7" ((arg3);) }
	;

%inline d_recrem_timing_check_11_4 : { Globals.grdbg "d_recrem_timing_check_11_4_2" ( EMPTY) }
	| arg3 = variable_identifier { Globals.grdbg "d_recrem_timing_check_11_4" ((arg3);) }
	;

%inline d_recrem_timing_check_11_5_4 : { Globals.grdbg "d_recrem_timing_check_11_5_4_2" ( EMPTY) }
	| arg3 = mintypmax_expression { Globals.grdbg "d_recrem_timing_check_11_5_4" ((arg3);) }
	;

%inline d_recrem_timing_check_11_5_5_4 : { Globals.grdbg "d_recrem_timing_check_11_5_5_4_2" ( EMPTY) }
	| arg3 = mintypmax_expression { Globals.grdbg "d_recrem_timing_check_11_5_5_4" ((arg3);) }
	;

%inline d_recrem_timing_check_11_5_5_5_4 : { Globals.grdbg "d_recrem_timing_check_11_5_5_5_4_2" ( EMPTY) }
	| arg3 = delayed_reference { Globals.grdbg "d_recrem_timing_check_11_5_5_5_4" ((arg3);) }
	;

%inline d_recrem_timing_check_11_5_5_5_5_4 : { Globals.grdbg "d_recrem_timing_check_11_5_5_5_5_4_2" ( EMPTY) }
	| arg3 = delayed_data { Globals.grdbg "d_recrem_timing_check_11_5_5_5_5_4" ((arg3);) }
	;

%inline d_recrem_timing_check_11_5_5_5_5 : { Globals.grdbg "d_recrem_timing_check_11_5_5_5_5_2" ( EMPTY) }
	| COMMA arg4 = d_recrem_timing_check_11_5_5_5_5_4 { Globals.grdbg "d_recrem_timing_check_11_5_5_5_5" (DOUBLE(COMMA, arg4);) }
	;

%inline d_recrem_timing_check_11_5_5_5 : { Globals.grdbg "d_recrem_timing_check_11_5_5_5_2" ( EMPTY) }
	| COMMA arg4 = d_recrem_timing_check_11_5_5_5_4 arg5 = d_recrem_timing_check_11_5_5_5_5 { Globals.grdbg "d_recrem_timing_check_11_5_5_5" (TRIPLE(COMMA, arg4, arg5);) }
	;

%inline d_recrem_timing_check_11_5_5 : { Globals.grdbg "d_recrem_timing_check_11_5_5_2" ( EMPTY) }
	| COMMA arg4 = d_recrem_timing_check_11_5_5_4 arg5 = d_recrem_timing_check_11_5_5_5 { Globals.grdbg "d_recrem_timing_check_11_5_5" (TRIPLE(COMMA, arg4, arg5);) }
	;

%inline d_recrem_timing_check_11_5 : { Globals.grdbg "d_recrem_timing_check_11_5_2" ( EMPTY) }
	| COMMA arg4 = d_recrem_timing_check_11_5_4 arg5 = d_recrem_timing_check_11_5_5 { Globals.grdbg "d_recrem_timing_check_11_5" (TRIPLE(COMMA, arg4, arg5);) }
	;

%inline d_recrem_timing_check_11 : { Globals.grdbg "d_recrem_timing_check_11_2" ( EMPTY) }
	| COMMA arg4 = d_recrem_timing_check_11_4 arg5 = d_recrem_timing_check_11_5 { Globals.grdbg "d_recrem_timing_check_11" (TRIPLE(COMMA, arg4, arg5);) }
	;

%inline pass_switch_instance_2 : { Globals.grdbg "pass_switch_instance_2_2" ( EMPTY) }
	| arg3 = name_of_gate_instance { Globals.grdbg "pass_switch_instance_2" ((arg3);) }
	;

%inline nonblocking_assignment_4 : { Globals.grdbg "nonblocking_assignment_4_2" ( EMPTY) }
	| arg3 = delay_or_event_control { Globals.grdbg "nonblocking_assignment_4" ((arg3);) }
	;

list_of_port_identifiers_3 : { Globals.grdbg "list_of_port_identifiers_3_2" ( EMPTY) }
	| arg3 = list_of_port_identifiers_3 arg4 = unpacked_dimension { Globals.grdbg "list_of_port_identifiers_3" (DOUBLE(arg3, arg4);) }
	;

list_of_port_identifiers_4_6 : { Globals.grdbg "list_of_port_identifiers_4_6_2" ( EMPTY) }
	| arg3 = list_of_port_identifiers_4_6 arg4 = unpacked_dimension { Globals.grdbg "list_of_port_identifiers_4_6" (DOUBLE(arg3, arg4);) }
	;

list_of_port_identifiers_4 : { Globals.grdbg "list_of_port_identifiers_4_2" ( EMPTY) }
	| arg3 = list_of_port_identifiers_4 COMMA arg5 = identifier arg6 = list_of_port_identifiers_4_6 { Globals.grdbg "list_of_port_identifiers_4" (QUADRUPLE(arg3, COMMA, arg5, arg6);) }
	;

%inline generate_interface_item_8 : { Globals.grdbg "generate_interface_item_8_2" ( EMPTY) }
	| arg3 = identifier COLON { Globals.grdbg "generate_interface_item_8" (DOUBLE(arg3, COLON);) }
	;

%inline d_setup_timing_check_9_4 : { Globals.grdbg "d_setup_timing_check_9_4_2" ( EMPTY) }
	| arg3 = variable_identifier { Globals.grdbg "d_setup_timing_check_9_4" ((arg3);) }
	;

%inline d_setup_timing_check_9 : { Globals.grdbg "d_setup_timing_check_9_2" ( EMPTY) }
	| COMMA arg4 = d_setup_timing_check_9_4 { Globals.grdbg "d_setup_timing_check_9" (DOUBLE(COMMA, arg4);) }
	;

%inline generate_interface_conditional_statement_7 : { Globals.grdbg "generate_interface_conditional_statement_7_2" ( EMPTY) }
	| ELSE arg4 = generate_interface_item { Globals.grdbg "generate_interface_conditional_statement_7" (DOUBLE(ELSE, arg4);) }
	;

list_of_path_inputs_3 : { Globals.grdbg "list_of_path_inputs_3_2" ( EMPTY) }
	| arg3 = list_of_path_inputs_3 COMMA arg5 = specify_input_terminal_descriptor { Globals.grdbg "list_of_path_inputs_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

list_of_param_assignments_3 : { Globals.grdbg "list_of_param_assignments_3_2" ( EMPTY) }
	| arg3 = list_of_param_assignments_3 COMMA arg5 = param_assignment { Globals.grdbg "list_of_param_assignments_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline program_declaration_3 : { Globals.grdbg "program_declaration_3_2" ( EMPTY) }
	| arg3 = timeunits_declaration { Globals.grdbg "program_declaration_3" ((arg3);) }
	;

program_declaration_4 : { Globals.grdbg "program_declaration_4_2" ( EMPTY) }
	| arg3 = program_declaration_4 arg4 = program_item { Globals.grdbg "program_declaration_4" (DOUBLE(arg3, arg4);) }
	;

%inline program_declaration_6 : { Globals.grdbg "program_declaration_6_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "program_declaration_6" (DOUBLE(COLON, arg4);) }
	;

%inline program_declaration_9 : { Globals.grdbg "program_declaration_9_2" ( EMPTY) }
	| arg3 = timeunits_declaration { Globals.grdbg "program_declaration_9" ((arg3);) }
	;

program_declaration_10 : { Globals.grdbg "program_declaration_10_2" ( EMPTY) }
	| arg3 = program_declaration_10 arg4 = non_port_program_item { Globals.grdbg "program_declaration_10" (DOUBLE(arg3, arg4);) }
	;

%inline program_declaration_12 : { Globals.grdbg "program_declaration_12_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "program_declaration_12" (DOUBLE(COLON, arg4);) }
	;

%inline program_declaration_21 : { Globals.grdbg "program_declaration_21_2" ( EMPTY) }
	| arg3 = timeunits_declaration { Globals.grdbg "program_declaration_21" ((arg3);) }
	;

program_declaration_22 : { Globals.grdbg "program_declaration_22_2" ( EMPTY) }
	| arg3 = program_declaration_22 arg4 = program_item { Globals.grdbg "program_declaration_22" (DOUBLE(arg3, arg4);) }
	;

%inline program_declaration_24 : { Globals.grdbg "program_declaration_24_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "program_declaration_24" (DOUBLE(COLON, arg4);) }
	;

loop_statement_30 : { Globals.grdbg "loop_statement_30_2" ( EMPTY) }
	| arg3 = loop_statement_30 COMMA arg5 = variable_decl_or_assignment { Globals.grdbg "loop_statement_30" (TRIPLE(arg3, COMMA, arg5);) }
	;

loop_statement_35 : { Globals.grdbg "loop_statement_35_2" ( EMPTY) }
	| arg3 = loop_statement_35 COMMA arg5 = variable_assignment { Globals.grdbg "loop_statement_35" (TRIPLE(arg3, COMMA, arg5);) }
	;

system_function_call_3_5 : { Globals.grdbg "system_function_call_3_5_2" ( EMPTY) }
	| arg3 = system_function_call_3_5 COMMA arg5 = expression { Globals.grdbg "system_function_call_3_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline system_function_call_3 : { Globals.grdbg "system_function_call_3_2" ( EMPTY) }
	| LPAREN arg4 = expression arg5 = system_function_call_3_5 RPAREN { Globals.grdbg "system_function_call_3" (QUADRUPLE(LPAREN, arg4, arg5, RPAREN);) }
	;

modport_tf_port_4 : { Globals.grdbg "modport_tf_port_4_2" ( EMPTY) }
	| arg3 = modport_tf_port_4 COMMA arg5 = named_task_proto { Globals.grdbg "modport_tf_port_4" (TRIPLE(arg3, COMMA, arg5);) }
	;

modport_tf_port_8 : { Globals.grdbg "modport_tf_port_8_2" ( EMPTY) }
	| arg3 = modport_tf_port_8 COMMA arg5 = named_function_proto { Globals.grdbg "modport_tf_port_8" (TRIPLE(arg3, COMMA, arg5);) }
	;

modport_tf_port_11 : { Globals.grdbg "modport_tf_port_11_2" ( EMPTY) }
	| arg3 = modport_tf_port_11 COMMA arg5 = task_or_function_identifier { Globals.grdbg "modport_tf_port_11" (TRIPLE(arg3, COMMA, arg5);) }
	;

udp_declaration_port_list_5 : { Globals.grdbg "udp_declaration_port_list_5_2" ( EMPTY) }
	| arg3 = udp_declaration_port_list_5 COMMA arg5 = udp_input_declaration { Globals.grdbg "udp_declaration_port_list_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

generate_module_named_block_5 : { Globals.grdbg "generate_module_named_block_5_2" ( EMPTY) }
	| arg3 = generate_module_named_block_5 arg4 = generate_module_item { Globals.grdbg "generate_module_named_block_5" (DOUBLE(arg3, arg4);) }
	;

%inline generate_module_named_block_7 : { Globals.grdbg "generate_module_named_block_7_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "generate_module_named_block_7" (DOUBLE(COLON, arg4);) }
	;

sequence_formal_list_4 : { Globals.grdbg "sequence_formal_list_4_2" ( EMPTY) }
	| arg3 = sequence_formal_list_4 COMMA arg5 = formal_list_item { Globals.grdbg "sequence_formal_list_4" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline clocking_direction_3 : { Globals.grdbg "clocking_direction_3_2" ( EMPTY) }
	| arg3 = clocking_skew { Globals.grdbg "clocking_direction_3" ((arg3);) }
	;

%inline clocking_direction_6 : { Globals.grdbg "clocking_direction_6_2" ( EMPTY) }
	| arg3 = clocking_skew { Globals.grdbg "clocking_direction_6" ((arg3);) }
	;

%inline clocking_direction_9 : { Globals.grdbg "clocking_direction_9_2" ( EMPTY) }
	| arg3 = clocking_skew { Globals.grdbg "clocking_direction_9" ((arg3);) }
	;

%inline clocking_direction_11 : { Globals.grdbg "clocking_direction_11_2" ( EMPTY) }
	| arg3 = clocking_skew { Globals.grdbg "clocking_direction_11" ((arg3);) }
	;

%inline d_period_timing_check_7_4 : { Globals.grdbg "d_period_timing_check_7_4_2" ( EMPTY) }
	| arg3 = variable_identifier { Globals.grdbg "d_period_timing_check_7_4" ((arg3);) }
	;

%inline d_period_timing_check_7 : { Globals.grdbg "d_period_timing_check_7_2" ( EMPTY) }
	| COMMA arg4 = d_period_timing_check_7_4 { Globals.grdbg "d_period_timing_check_7" (DOUBLE(COMMA, arg4);) }
	;

function_loop_statement_20 : { Globals.grdbg "function_loop_statement_20_2" ( EMPTY) }
	| arg3 = function_loop_statement_20 COMMA arg5 = variable_decl_or_assignment { Globals.grdbg "function_loop_statement_20" (TRIPLE(arg3, COMMA, arg5);) }
	;

function_loop_statement_25 : { Globals.grdbg "function_loop_statement_25_2" ( EMPTY) }
	| arg3 = function_loop_statement_25 COMMA arg5 = variable_assignment { Globals.grdbg "function_loop_statement_25" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline jump_statement_3 : { Globals.grdbg "jump_statement_3_2" ( EMPTY) }
	| arg3 = expression { Globals.grdbg "jump_statement_3" ((arg3);) }
	;

%inline pulse_control_specparam_6 : { Globals.grdbg "pulse_control_specparam_6_2" ( EMPTY) }
	| COMMA arg4 = limit_value { Globals.grdbg "pulse_control_specparam_6" (DOUBLE(COMMA, arg4);) }
	;

%inline pulse_control_specparam_17 : { Globals.grdbg "pulse_control_specparam_17_2" ( EMPTY) }
	| COMMA arg4 = limit_value { Globals.grdbg "pulse_control_specparam_17" (DOUBLE(COMMA, arg4);) }
	;

%inline controlled_timing_check_event_4 : { Globals.grdbg "controlled_timing_check_event_4_2" ( EMPTY) }
	| P_ANDANDAND arg4 = timing_check_condition { Globals.grdbg "controlled_timing_check_event_4" (DOUBLE(P_ANDANDAND, arg4);) }
	;

named_task_proto_5 : { Globals.grdbg "named_task_proto_5_2" ( EMPTY) }
	| arg3 = named_task_proto_5 COMMA arg5 = task_proto_formal { Globals.grdbg "named_task_proto_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline interface_declaration_3 : { Globals.grdbg "interface_declaration_3_2" ( EMPTY) }
	| arg3 = timeunits_declaration { Globals.grdbg "interface_declaration_3" ((arg3);) }
	;

interface_declaration_4 : { Globals.grdbg "interface_declaration_4_2" ( EMPTY) }
	| arg3 = interface_declaration_4 arg4 = interface_item { Globals.grdbg "interface_declaration_4" (DOUBLE(arg3, arg4);) }
	;

%inline interface_declaration_6 : { Globals.grdbg "interface_declaration_6_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "interface_declaration_6" (DOUBLE(COLON, arg4);) }
	;

%inline interface_declaration_9 : { Globals.grdbg "interface_declaration_9_2" ( EMPTY) }
	| arg3 = timeunits_declaration { Globals.grdbg "interface_declaration_9" ((arg3);) }
	;

interface_declaration_10 : { Globals.grdbg "interface_declaration_10_2" ( EMPTY) }
	| arg3 = interface_declaration_10 arg4 = non_port_interface_item { Globals.grdbg "interface_declaration_10" (DOUBLE(arg3, arg4);) }
	;

%inline interface_declaration_12 : { Globals.grdbg "interface_declaration_12_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "interface_declaration_12" (DOUBLE(COLON, arg4);) }
	;

%inline interface_declaration_21 : { Globals.grdbg "interface_declaration_21_2" ( EMPTY) }
	| arg3 = timeunits_declaration { Globals.grdbg "interface_declaration_21" ((arg3);) }
	;

interface_declaration_22 : { Globals.grdbg "interface_declaration_22_2" ( EMPTY) }
	| arg3 = interface_declaration_22 arg4 = interface_item { Globals.grdbg "interface_declaration_22" (DOUBLE(arg3, arg4);) }
	;

%inline interface_declaration_24 : { Globals.grdbg "interface_declaration_24_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "interface_declaration_24" (DOUBLE(COLON, arg4);) }
	;

%inline list_of_constant_arguments_2 : { Globals.grdbg "list_of_constant_arguments_2_2" ( EMPTY) }
	| arg3 = constant_expression { Globals.grdbg "list_of_constant_arguments_2" ((arg3);) }
	;

%inline list_of_constant_arguments_3_5 : { Globals.grdbg "list_of_constant_arguments_3_5_2" ( EMPTY) }
	| arg3 = constant_expression { Globals.grdbg "list_of_constant_arguments_3_5" ((arg3);) }
	;

list_of_constant_arguments_3 : { Globals.grdbg "list_of_constant_arguments_3_2" ( EMPTY) }
	| arg3 = list_of_constant_arguments_3 COMMA arg5 = list_of_constant_arguments_3_5 { Globals.grdbg "list_of_constant_arguments_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline list_of_constant_arguments_8 : { Globals.grdbg "list_of_constant_arguments_8_2" ( EMPTY) }
	| arg3 = constant_expression { Globals.grdbg "list_of_constant_arguments_8" ((arg3);) }
	;

%inline list_of_constant_arguments_10_8 : { Globals.grdbg "list_of_constant_arguments_10_8_2" ( EMPTY) }
	| arg3 = constant_expression { Globals.grdbg "list_of_constant_arguments_10_8" ((arg3);) }
	;

list_of_constant_arguments_10 : { Globals.grdbg "list_of_constant_arguments_10_2" ( EMPTY) }
	| arg3 = list_of_constant_arguments_10 COMMA DOT arg6 = identifier LPAREN arg8 = list_of_constant_arguments_10_8 RPAREN { Globals.grdbg "list_of_constant_arguments_10" (SEPTUPLE(arg3, COMMA, DOT, arg6, LPAREN, arg8, RPAREN);) }
	;

%inline sequence_declaration_4 : { Globals.grdbg "sequence_declaration_4_2" ( EMPTY) }
	| arg3 = sequence_formal_list { Globals.grdbg "sequence_declaration_4" ((arg3);) }
	;

sequence_declaration_6 : { Globals.grdbg "sequence_declaration_6_2" ( EMPTY) }
	| arg3 = sequence_declaration_6 arg4 = assertion_variable_declaration { Globals.grdbg "sequence_declaration_6" (DOUBLE(arg3, arg4);) }
	;

%inline sequence_declaration_10 : { Globals.grdbg "sequence_declaration_10_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "sequence_declaration_10" (DOUBLE(COLON, arg4);) }
	;

string_literal_3 : { Globals.grdbg "string_literal_3_2" ( EMPTY) }
	| arg3 = string_literal_3 ANY_ASCII_CHARS { Globals.grdbg "string_literal_3" (DOUBLE(arg3, ANY_ASCII_CHARS);) }
	;

name_of_instance_3 : { Globals.grdbg "name_of_instance_3_2" ( EMPTY) }
	| arg3 = name_of_instance_3 arg4 = range { Globals.grdbg "name_of_instance_3" (DOUBLE(arg3, arg4);) }
	;

list_of_ports_4 : { Globals.grdbg "list_of_ports_4_2" ( EMPTY) }
	| arg3 = list_of_ports_4 COMMA arg5 = port { Globals.grdbg "list_of_ports_4" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline clocking_drive_4 : { Globals.grdbg "clocking_drive_4_2" ( EMPTY) }
	| arg3 = cycle_delay { Globals.grdbg "clocking_drive_4" ((arg3);) }
	;

%inline timing_check_event_2 : { Globals.grdbg "timing_check_event_2_2" ( EMPTY) }
	| arg3 = timing_check_event_control { Globals.grdbg "timing_check_event_2" ((arg3);) }
	;

%inline timing_check_event_4 : { Globals.grdbg "timing_check_event_4_2" ( EMPTY) }
	| P_ANDANDAND arg4 = timing_check_condition { Globals.grdbg "timing_check_event_4" (DOUBLE(P_ANDANDAND, arg4);) }
	;

%inline par_block_3 : { Globals.grdbg "par_block_3_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "par_block_3" (DOUBLE(COLON, arg4);) }
	;

par_block_4 : { Globals.grdbg "par_block_4_2" ( EMPTY) }
	| arg3 = par_block_4 arg4 = block_item_declaration { Globals.grdbg "par_block_4" (DOUBLE(arg3, arg4);) }
	;

par_block_5 : { Globals.grdbg "par_block_5_2" ( EMPTY) }
	| arg3 = par_block_5 arg4 = statement_or_null { Globals.grdbg "par_block_5" (DOUBLE(arg3, arg4);) }
	;

%inline par_block_7 : { Globals.grdbg "par_block_7_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "par_block_7" (DOUBLE(COLON, arg4);) }
	;

%inline design_statement_3_4 : { Globals.grdbg "design_statement_3_4_2" ( EMPTY) }
	| arg3 = identifier DOT { Globals.grdbg "design_statement_3_4" (DOUBLE(arg3, DOT);) }
	;

design_statement_3 : { Globals.grdbg "design_statement_3_2" ( EMPTY) }
	| arg3 = design_statement_3 arg4 = design_statement_3_4 arg5 = identifier { Globals.grdbg "design_statement_3" (TRIPLE(arg3, arg4, arg5);) }
	;

list_of_net_decl_assignments_3 : { Globals.grdbg "list_of_net_decl_assignments_3_2" ( EMPTY) }
	| arg3 = list_of_net_decl_assignments_3 COMMA arg5 = net_decl_assignment { Globals.grdbg "list_of_net_decl_assignments_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline event_trigger_7 : { Globals.grdbg "event_trigger_7_2" ( EMPTY) }
	| arg3 = delay_or_event_control { Globals.grdbg "event_trigger_7" ((arg3);) }
	;

%inline formal_list_item_3 : { Globals.grdbg "formal_list_item_3_2" ( EMPTY) }
	| EQUALS arg4 = event_expression { Globals.grdbg "formal_list_item_3" (DOUBLE(EQUALS, arg4);) }
	;

%inline case_statement_2 : { Globals.grdbg "case_statement_2_2" ( EMPTY) }
	| arg3 = unique_priority { Globals.grdbg "case_statement_2" ((arg3);) }
	;

case_statement_8 : { Globals.grdbg "case_statement_8_2" ( EMPTY) }
	| arg3 = case_statement_8 arg4 = case_item { Globals.grdbg "case_statement_8" (DOUBLE(arg3, arg4);) }
	;

%inline case_statement_11 : { Globals.grdbg "case_statement_11_2" ( EMPTY) }
	| arg3 = unique_priority { Globals.grdbg "case_statement_11" ((arg3);) }
	;

case_statement_17 : { Globals.grdbg "case_statement_17_2" ( EMPTY) }
	| arg3 = case_statement_17 arg4 = case_item { Globals.grdbg "case_statement_17" (DOUBLE(arg3, arg4);) }
	;

%inline case_statement_20 : { Globals.grdbg "case_statement_20_2" ( EMPTY) }
	| arg3 = unique_priority { Globals.grdbg "case_statement_20" ((arg3);) }
	;

case_statement_26 : { Globals.grdbg "case_statement_26_2" ( EMPTY) }
	| arg3 = case_statement_26 arg4 = case_item { Globals.grdbg "case_statement_26" (DOUBLE(arg3, arg4);) }
	;

concatenation_4 : { Globals.grdbg "concatenation_4_2" ( EMPTY) }
	| arg3 = concatenation_4 COMMA arg5 = expression { Globals.grdbg "concatenation_4" (TRIPLE(arg3, COMMA, arg5);) }
	;

concatenation_11 : { Globals.grdbg "concatenation_11_2" ( EMPTY) }
	| arg3 = concatenation_11 COMMA arg5 = struct_member_label COLON arg7 = expression { Globals.grdbg "concatenation_11" (QUINTUPLE(arg3, COMMA, arg5, COLON, arg7);) }
	;

concatenation_18 : { Globals.grdbg "concatenation_18_2" ( EMPTY) }
	| arg3 = concatenation_18 COMMA arg5 = array_member_label COLON arg7 = expression { Globals.grdbg "concatenation_18" (QUINTUPLE(arg3, COMMA, arg5, COLON, arg7);) }
	;

class_scope_type_identifier_4 : { Globals.grdbg "class_scope_type_identifier_4_2" ( EMPTY) }
	| arg3 = class_scope_type_identifier_4 arg4 = identifier P_COLONCOLON { Globals.grdbg "class_scope_type_identifier_4" (TRIPLE(arg3, arg4, P_COLONCOLON);) }
	;

class_scope_type_identifier_9 : { Globals.grdbg "class_scope_type_identifier_9_2" ( EMPTY) }
	| arg3 = class_scope_type_identifier_9 arg4 = identifier P_COLONCOLON { Globals.grdbg "class_scope_type_identifier_9" (TRIPLE(arg3, arg4, P_COLONCOLON);) }
	;

generate_module_case_statement_7 : { Globals.grdbg "generate_module_case_statement_7_2" ( EMPTY) }
	| arg3 = generate_module_case_statement_7 arg4 = genvar_module_case_item { Globals.grdbg "generate_module_case_statement_7" (DOUBLE(arg3, arg4);) }
	;

%inline cmos_switch_instance_2 : { Globals.grdbg "cmos_switch_instance_2_2" ( EMPTY) }
	| arg3 = name_of_gate_instance { Globals.grdbg "cmos_switch_instance_2" ((arg3);) }
	;

%inline program_instantiation_3 : { Globals.grdbg "program_instantiation_3_2" ( EMPTY) }
	| arg3 = parameter_value_assignment { Globals.grdbg "program_instantiation_3" ((arg3);) }
	;

program_instantiation_5 : { Globals.grdbg "program_instantiation_5_2" ( EMPTY) }
	| arg3 = program_instantiation_5 COMMA arg5 = program_instance { Globals.grdbg "program_instantiation_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline task_enable_3 : { Globals.grdbg "task_enable_3_2" ( EMPTY) }
	| LPAREN arg4 = list_of_arguments RPAREN { Globals.grdbg "task_enable_3" (TRIPLE(LPAREN, arg4, RPAREN);) }
	;

hex_value_3 : { Globals.grdbg "hex_value_3_2" ( EMPTY) }
	| arg3 = hex_value_3 UNDERSCORE { Globals.grdbg "hex_value_3_5" (DOUBLE(arg3, UNDERSCORE);) }
	| arg6 = HEX_DIGIT { Globals.grdbg "hex_value_3" ((HEX_DIGIT arg6);) }
	;

%inline tf_input_declaration_3 : { Globals.grdbg "tf_input_declaration_3_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "tf_input_declaration_3" ((arg3);) }
	;

tf_input_declaration_4 : { Globals.grdbg "tf_input_declaration_4_2" ( EMPTY) }
	| arg3 = tf_input_declaration_4 arg4 = packed_dimension { Globals.grdbg "tf_input_declaration_4" (DOUBLE(arg3, arg4);) }
	;

%inline specify_output_terminal_descriptor_3 : { Globals.grdbg "specify_output_terminal_descriptor_3_2" ( EMPTY) }
	| LBRACK arg4 = constant_range_expression RBRACK { Globals.grdbg "specify_output_terminal_descriptor_3" (TRIPLE(LBRACK, arg4, RBRACK);) }
	;

library_text_2 : { Globals.grdbg "library_text_2_2" ( EMPTY) }
	| arg3 = library_text_2 arg4 = library_descriptions { Globals.grdbg "library_text_2" (DOUBLE(arg3, arg4);) }
	;

function_data_type_3 : { Globals.grdbg "function_data_type_3_2" ( EMPTY) }
	| arg3 = function_data_type_3 arg4 = packed_dimension { Globals.grdbg "function_data_type_3" (DOUBLE(arg3, arg4);) }
	;

%inline function_data_type_4 : { Globals.grdbg "function_data_type_4_2" ( EMPTY) }
	| arg3 = range { Globals.grdbg "function_data_type_4" ((arg3);) }
	;

function_data_type_9 : { Globals.grdbg "function_data_type_9_2" ( EMPTY) }
	| arg3 = function_data_type_9 arg4 = packed_dimension { Globals.grdbg "function_data_type_9" (DOUBLE(arg3, arg4);) }
	;

%inline function_data_type_14 : { Globals.grdbg "function_data_type_14_2" ( EMPTY) }
	| PACKED { Globals.grdbg "function_data_type_14" ((PACKED);) }
	;

function_data_type_16 : { Globals.grdbg "function_data_type_16_2" ( EMPTY) }
	| arg3 = function_data_type_16 arg4 = struct_union_member { Globals.grdbg "function_data_type_16" (DOUBLE(arg3, arg4);) }
	;

function_data_type_18 : { Globals.grdbg "function_data_type_18_2" ( EMPTY) }
	| arg3 = function_data_type_18 arg4 = packed_dimension { Globals.grdbg "function_data_type_18" (DOUBLE(arg3, arg4);) }
	;

%inline function_data_type_21 : { Globals.grdbg "function_data_type_21_2" ( EMPTY) }
	| PACKED { Globals.grdbg "function_data_type_21" ((PACKED);) }
	;

function_data_type_23 : { Globals.grdbg "function_data_type_23_2" ( EMPTY) }
	| arg3 = function_data_type_23 arg4 = struct_union_member { Globals.grdbg "function_data_type_23" (DOUBLE(arg3, arg4);) }
	;

function_data_type_25 : { Globals.grdbg "function_data_type_25_2" ( EMPTY) }
	| arg3 = function_data_type_25 arg4 = packed_dimension { Globals.grdbg "function_data_type_25" (DOUBLE(arg3, arg4);) }
	;

function_data_type_28_4 : { Globals.grdbg "function_data_type_28_4_2" ( EMPTY) }
	| arg3 = function_data_type_28_4 arg4 = packed_dimension { Globals.grdbg "function_data_type_28_4" (DOUBLE(arg3, arg4);) }
	;

%inline function_data_type_28 : { Globals.grdbg "function_data_type_28_2" ( EMPTY) }
	| arg3 = integer_type arg4 = function_data_type_28_4 { Globals.grdbg "function_data_type_28" (DOUBLE(arg3, arg4);) }
	;

%inline function_data_type_31 : { Globals.grdbg "function_data_type_31_2" ( EMPTY) }
	| EQUALS arg4 = constant_expression { Globals.grdbg "function_data_type_31" (DOUBLE(EQUALS, arg4);) }
	;

%inline function_data_type_32_6 : { Globals.grdbg "function_data_type_32_6_2" ( EMPTY) }
	| EQUALS arg4 = constant_expression { Globals.grdbg "function_data_type_32_6" (DOUBLE(EQUALS, arg4);) }
	;

function_data_type_32 : { Globals.grdbg "function_data_type_32_2" ( EMPTY) }
	| arg3 = function_data_type_32 COMMA arg5 = identifier arg6 = function_data_type_32_6 { Globals.grdbg "function_data_type_32" (QUADRUPLE(arg3, COMMA, arg5, arg6);) }
	;

sequence_expr_4 : { Globals.grdbg "sequence_expr_4_2" ( EMPTY) }
	| arg3 = sequence_expr_4 arg4 = cycle_delay_range arg5 = sequence_expr { Globals.grdbg "sequence_expr_4" (TRIPLE(arg3, arg4, arg5);) }
	;

sequence_expr_9 : { Globals.grdbg "sequence_expr_9_2" ( EMPTY) }
	| arg3 = sequence_expr_9 arg4 = cycle_delay_range arg5 = sequence_expr { Globals.grdbg "sequence_expr_9" (TRIPLE(arg3, arg4, arg5);) }
	;

sequence_expr_12 : { Globals.grdbg "sequence_expr_12_2" ( EMPTY) }
	| arg3 = sequence_expr_12 COMMA arg5 = function_blocking_assignment { Globals.grdbg "sequence_expr_12" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline sequence_expr_13 : { Globals.grdbg "sequence_expr_13_2" ( EMPTY) }
	| arg3 = boolean_abbrev { Globals.grdbg "sequence_expr_13" ((arg3);) }
	;

sequence_expr_17 : { Globals.grdbg "sequence_expr_17_2" ( EMPTY) }
	| arg3 = sequence_expr_17 COMMA arg5 = function_blocking_assignment { Globals.grdbg "sequence_expr_17" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline sequence_expr_19 : { Globals.grdbg "sequence_expr_19_2" ( EMPTY) }
	| arg3 = boolean_abbrev { Globals.grdbg "sequence_expr_19" ((arg3);) }
	;

%inline sequence_expr_22 : { Globals.grdbg "sequence_expr_22_2" ( EMPTY) }
	| arg3 = consecutive_repetition { Globals.grdbg "sequence_expr_22" ((arg3);) }
	;

%inline sequence_expr_27 : { Globals.grdbg "sequence_expr_27_2" ( EMPTY) }
	| arg3 = consecutive_repetition { Globals.grdbg "sequence_expr_27" ((arg3);) }
	;

udp_port_list_5 : { Globals.grdbg "udp_port_list_5_2" ( EMPTY) }
	| arg3 = udp_port_list_5 COMMA arg5 = identifier { Globals.grdbg "udp_port_list_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline function_case_statement_2 : { Globals.grdbg "function_case_statement_2_2" ( EMPTY) }
	| arg3 = unique_priority { Globals.grdbg "function_case_statement_2" ((arg3);) }
	;

function_case_statement_8 : { Globals.grdbg "function_case_statement_8_2" ( EMPTY) }
	| arg3 = function_case_statement_8 arg4 = function_case_item { Globals.grdbg "function_case_statement_8" (DOUBLE(arg3, arg4);) }
	;

%inline function_case_statement_11 : { Globals.grdbg "function_case_statement_11_2" ( EMPTY) }
	| arg3 = unique_priority { Globals.grdbg "function_case_statement_11" ((arg3);) }
	;

function_case_statement_17 : { Globals.grdbg "function_case_statement_17_2" ( EMPTY) }
	| arg3 = function_case_statement_17 arg4 = function_case_item { Globals.grdbg "function_case_statement_17" (DOUBLE(arg3, arg4);) }
	;

%inline function_case_statement_20 : { Globals.grdbg "function_case_statement_20_2" ( EMPTY) }
	| arg3 = unique_priority { Globals.grdbg "function_case_statement_20" ((arg3);) }
	;

function_case_statement_26 : { Globals.grdbg "function_case_statement_26_2" ( EMPTY) }
	| arg3 = function_case_statement_26 arg4 = function_case_item { Globals.grdbg "function_case_statement_26" (DOUBLE(arg3, arg4);) }
	;

%inline multi_clock_property_expr_6 : { Globals.grdbg "multi_clock_property_expr_6_2" ( EMPTY) }
	| NOT { Globals.grdbg "multi_clock_property_expr_6" ((NOT);) }
	;

%inline mos_switch_instance_2 : { Globals.grdbg "mos_switch_instance_2_2" ( EMPTY) }
	| arg3 = name_of_gate_instance { Globals.grdbg "mos_switch_instance_2" ((arg3);) }
	;

%inline binary_number_2 : { Globals.grdbg "binary_number_2_2" ( EMPTY) }
	| arg3 = non_zero_unsigned_number { Globals.grdbg "binary_number_2" ((arg3);) }
	;

module_path_concatenation_4 : { Globals.grdbg "module_path_concatenation_4_2" ( EMPTY) }
	| arg3 = module_path_concatenation_4 COMMA arg5 = module_path_expression { Globals.grdbg "module_path_concatenation_4" (TRIPLE(arg3, COMMA, arg5);) }
	;

generated_module_instantiation_3 : { Globals.grdbg "generated_module_instantiation_3_2" ( EMPTY) }
	| arg3 = generated_module_instantiation_3 arg4 = generate_module_item { Globals.grdbg "generated_module_instantiation_3" (DOUBLE(arg3, arg4);) }
	;

%inline continuous_assign_3 : { Globals.grdbg "continuous_assign_3_2" ( EMPTY) }
	| arg3 = drive_strength { Globals.grdbg "continuous_assign_3" ((arg3);) }
	;

%inline continuous_assign_4 : { Globals.grdbg "continuous_assign_4_2" ( EMPTY) }
	| arg3 = delay3 { Globals.grdbg "continuous_assign_4" ((arg3);) }
	;

%inline continuous_assign_9 : { Globals.grdbg "continuous_assign_9_2" ( EMPTY) }
	| arg3 = delay_control { Globals.grdbg "continuous_assign_9" ((arg3);) }
	;

%inline module_instance_4 : { Globals.grdbg "module_instance_4_2" ( EMPTY) }
	| arg3 = list_of_port_connections { Globals.grdbg "module_instance_4" ((arg3);) }
	;

liblist_clause_3 : { Globals.grdbg "liblist_clause_3_2" ( EMPTY) }
	| arg3 = liblist_clause_3 arg4 = identifier { Globals.grdbg "liblist_clause_3" (DOUBLE(arg3, arg4);) }
	;

%inline function_statement_2 : { Globals.grdbg "function_statement_2_2" ( EMPTY) }
	| arg3 = identifier COLON { Globals.grdbg "function_statement_2" (DOUBLE(arg3, COLON);) }
	;

%inline data_type_3 : { Globals.grdbg "data_type_3_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "data_type_3" ((arg3);) }
	;

data_type_4 : { Globals.grdbg "data_type_4_2" ( EMPTY) }
	| arg3 = data_type_4 arg4 = packed_dimension { Globals.grdbg "data_type_4" (DOUBLE(arg3, arg4);) }
	;

%inline data_type_5 : { Globals.grdbg "data_type_5_2" ( EMPTY) }
	| arg3 = range { Globals.grdbg "data_type_5" ((arg3);) }
	;

%inline data_type_8 : { Globals.grdbg "data_type_8_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "data_type_8" ((arg3);) }
	;

data_type_11 : { Globals.grdbg "data_type_11_2" ( EMPTY) }
	| arg3 = data_type_11 arg4 = packed_dimension { Globals.grdbg "data_type_11" (DOUBLE(arg3, arg4);) }
	;

%inline data_type_17 : { Globals.grdbg "data_type_17_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "data_type_17" ((arg3);) }
	;

data_type_19 : { Globals.grdbg "data_type_19_2" ( EMPTY) }
	| arg3 = data_type_19 arg4 = struct_union_member { Globals.grdbg "data_type_19" (DOUBLE(arg3, arg4);) }
	;

data_type_21 : { Globals.grdbg "data_type_21_2" ( EMPTY) }
	| arg3 = data_type_21 arg4 = packed_dimension { Globals.grdbg "data_type_21" (DOUBLE(arg3, arg4);) }
	;

%inline data_type_25 : { Globals.grdbg "data_type_25_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "data_type_25" ((arg3);) }
	;

data_type_27 : { Globals.grdbg "data_type_27_2" ( EMPTY) }
	| arg3 = data_type_27 arg4 = struct_union_member { Globals.grdbg "data_type_27" (DOUBLE(arg3, arg4);) }
	;

data_type_29 : { Globals.grdbg "data_type_29_2" ( EMPTY) }
	| arg3 = data_type_29 arg4 = packed_dimension { Globals.grdbg "data_type_29" (DOUBLE(arg3, arg4);) }
	;

%inline data_type_32 : { Globals.grdbg "data_type_32_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "data_type_32" ((arg3);) }
	;

data_type_34 : { Globals.grdbg "data_type_34_2" ( EMPTY) }
	| arg3 = data_type_34 arg4 = struct_union_member { Globals.grdbg "data_type_34" (DOUBLE(arg3, arg4);) }
	;

%inline data_type_38 : { Globals.grdbg "data_type_38_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "data_type_38" ((arg3);) }
	;

data_type_40 : { Globals.grdbg "data_type_40_2" ( EMPTY) }
	| arg3 = data_type_40 arg4 = struct_union_member { Globals.grdbg "data_type_40" (DOUBLE(arg3, arg4);) }
	;

%inline data_type_44_4 : { Globals.grdbg "data_type_44_4_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "data_type_44_4" ((arg3);) }
	;

data_type_44_5 : { Globals.grdbg "data_type_44_5_2" ( EMPTY) }
	| arg3 = data_type_44_5 arg4 = packed_dimension { Globals.grdbg "data_type_44_5" (DOUBLE(arg3, arg4);) }
	;

%inline data_type_44 : { Globals.grdbg "data_type_44_2" ( EMPTY) }
	| arg3 = integer_type arg4 = data_type_44_4 arg5 = data_type_44_5 { Globals.grdbg "data_type_44" (TRIPLE(arg3, arg4, arg5);) }
	;

%inline data_type_47 : { Globals.grdbg "data_type_47_2" ( EMPTY) }
	| EQUALS arg4 = constant_expression { Globals.grdbg "data_type_47" (DOUBLE(EQUALS, arg4);) }
	;

%inline data_type_48_6 : { Globals.grdbg "data_type_48_6_2" ( EMPTY) }
	| EQUALS arg4 = constant_expression { Globals.grdbg "data_type_48_6" (DOUBLE(EQUALS, arg4);) }
	;

data_type_48 : { Globals.grdbg "data_type_48_2" ( EMPTY) }
	| arg3 = data_type_48 COMMA arg5 = identifier arg6 = data_type_48_6 { Globals.grdbg "data_type_48" (QUADRUPLE(arg3, COMMA, arg5, arg6);) }
	;

%inline port_2 : { Globals.grdbg "port_2_2" ( EMPTY) }
	| arg3 = port_expression { Globals.grdbg "port_2" ((arg3);) }
	;

%inline port_7 : { Globals.grdbg "port_7_2" ( EMPTY) }
	| arg3 = port_expression { Globals.grdbg "port_7" ((arg3);) }
	;

%inline blocking_assignment_13 : { Globals.grdbg "blocking_assignment_13_2" ( EMPTY) }
	| LPAREN arg4 = identifier RPAREN { Globals.grdbg "blocking_assignment_13" (TRIPLE(LPAREN, arg4, RPAREN);) }
	;

%inline blocking_assignment_16 : { Globals.grdbg "blocking_assignment_16_2" ( EMPTY) }
	| arg3 = parameter_value_assignment { Globals.grdbg "blocking_assignment_16" ((arg3);) }
	;

%inline blocking_assignment_19 : { Globals.grdbg "blocking_assignment_19_2" ( EMPTY) }
	| LPAREN arg4 = list_of_arguments RPAREN { Globals.grdbg "blocking_assignment_19" (TRIPLE(LPAREN, arg4, RPAREN);) }
	;

%inline blocking_assignment_24 : { Globals.grdbg "blocking_assignment_24_2" ( EMPTY) }
	| LPAREN RPAREN { Globals.grdbg "blocking_assignment_24" (DOUBLE(LPAREN, RPAREN);) }
	;

%inline specparam_declaration_3 : { Globals.grdbg "specparam_declaration_3_2" ( EMPTY) }
	| arg3 = range { Globals.grdbg "specparam_declaration_3" ((arg3);) }
	;

constant_concatenation_4 : { Globals.grdbg "constant_concatenation_4_2" ( EMPTY) }
	| arg3 = constant_concatenation_4 COMMA arg5 = constant_expression { Globals.grdbg "constant_concatenation_4" (TRIPLE(arg3, COMMA, arg5);) }
	;

constant_concatenation_11 : { Globals.grdbg "constant_concatenation_11_2" ( EMPTY) }
	| arg3 = constant_concatenation_11 COMMA arg5 = struct_member_label COLON arg7 = constant_expression { Globals.grdbg "constant_concatenation_11" (QUINTUPLE(arg3, COMMA, arg5, COLON, arg7);) }
	;

constant_concatenation_18 : { Globals.grdbg "constant_concatenation_18_2" ( EMPTY) }
	| arg3 = constant_concatenation_18 COMMA arg5 = array_member_label COLON arg7 = constant_expression { Globals.grdbg "constant_concatenation_18" (QUINTUPLE(arg3, COMMA, arg5, COLON, arg7);) }
	;

constraint_set_5 : { Globals.grdbg "constraint_set_5_2" ( EMPTY) }
	| arg3 = constraint_set_5 arg4 = constraint_expression { Globals.grdbg "constraint_set_5" (DOUBLE(arg3, arg4);) }
	;

%inline function_call_4 : { Globals.grdbg "function_call_4_2" ( EMPTY) }
	| LPAREN arg4 = list_of_arguments RPAREN { Globals.grdbg "function_call_4" (TRIPLE(LPAREN, arg4, RPAREN);) }
	;

%inline statement_2 : { Globals.grdbg "statement_2_2" ( EMPTY) }
	| arg3 = identifier COLON { Globals.grdbg "statement_2" (DOUBLE(arg3, COLON);) }
	;

combinational_body_4 : { Globals.grdbg "combinational_body_4_2" ( EMPTY) }
	| arg3 = combinational_body_4 arg4 = combinational_entry { Globals.grdbg "combinational_body_4" (DOUBLE(arg3, arg4);) }
	;

list_of_tf_port_identifiers_3 : { Globals.grdbg "list_of_tf_port_identifiers_3_2" ( EMPTY) }
	| arg3 = list_of_tf_port_identifiers_3 arg4 = unpacked_dimension { Globals.grdbg "list_of_tf_port_identifiers_3" (DOUBLE(arg3, arg4);) }
	;

%inline list_of_tf_port_identifiers_4 : { Globals.grdbg "list_of_tf_port_identifiers_4_2" ( EMPTY) }
	| EQUALS arg4 = expression { Globals.grdbg "list_of_tf_port_identifiers_4" (DOUBLE(EQUALS, arg4);) }
	;

list_of_tf_port_identifiers_5_6 : { Globals.grdbg "list_of_tf_port_identifiers_5_6_2" ( EMPTY) }
	| arg3 = list_of_tf_port_identifiers_5_6 arg4 = unpacked_dimension { Globals.grdbg "list_of_tf_port_identifiers_5_6" (DOUBLE(arg3, arg4);) }
	;

%inline list_of_tf_port_identifiers_5_7 : { Globals.grdbg "list_of_tf_port_identifiers_5_7_2" ( EMPTY) }
	| EQUALS arg4 = expression { Globals.grdbg "list_of_tf_port_identifiers_5_7" (DOUBLE(EQUALS, arg4);) }
	;

list_of_tf_port_identifiers_5 : { Globals.grdbg "list_of_tf_port_identifiers_5_2" ( EMPTY) }
	| arg3 = list_of_tf_port_identifiers_5 COMMA arg5 = identifier arg6 = list_of_tf_port_identifiers_5_6 arg7 = list_of_tf_port_identifiers_5_7 { Globals.grdbg "list_of_tf_port_identifiers_5" (QUINTUPLE(arg3, COMMA, arg5, arg6, arg7);) }
	;

%inline d_nochange_timing_check_11_4 : { Globals.grdbg "d_nochange_timing_check_11_4_2" ( EMPTY) }
	| arg3 = variable_identifier { Globals.grdbg "d_nochange_timing_check_11_4" ((arg3);) }
	;

%inline d_nochange_timing_check_11 : { Globals.grdbg "d_nochange_timing_check_11_2" ( EMPTY) }
	| COMMA arg4 = d_nochange_timing_check_11_4 { Globals.grdbg "d_nochange_timing_check_11" (DOUBLE(COMMA, arg4);) }
	;

net_lvalue_3 : { Globals.grdbg "net_lvalue_3_2" ( EMPTY) }
	| arg3 = net_lvalue_3 LBRACK arg5 = constant_expression RBRACK { Globals.grdbg "net_lvalue_3" (QUADRUPLE(arg3, LBRACK, arg5, RBRACK);) }
	;

%inline net_lvalue_4 : { Globals.grdbg "net_lvalue_4_2" ( EMPTY) }
	| LBRACK arg4 = constant_range_expression RBRACK { Globals.grdbg "net_lvalue_4" (TRIPLE(LBRACK, arg4, RBRACK);) }
	;

net_lvalue_8 : { Globals.grdbg "net_lvalue_8_2" ( EMPTY) }
	| arg3 = net_lvalue_8 COMMA arg5 = net_lvalue { Globals.grdbg "net_lvalue_8" (TRIPLE(arg3, COMMA, arg5);) }
	;

udp_declaration_4 : { Globals.grdbg "udp_declaration_4_2" ( EMPTY) }
	| arg3 = udp_declaration_4 arg4 = udp_port_declaration { Globals.grdbg "udp_declaration_4" (DOUBLE(arg3, arg4);) }
	;

%inline udp_declaration_7 : { Globals.grdbg "udp_declaration_7_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "udp_declaration_7" (DOUBLE(COLON, arg4);) }
	;

%inline udp_declaration_12 : { Globals.grdbg "udp_declaration_12_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "udp_declaration_12" (DOUBLE(COLON, arg4);) }
	;

udp_declaration_27 : { Globals.grdbg "udp_declaration_27_2" ( EMPTY) }
	| arg3 = udp_declaration_27 arg4 = udp_port_declaration { Globals.grdbg "udp_declaration_27" (DOUBLE(arg3, arg4);) }
	;

%inline udp_declaration_30 : { Globals.grdbg "udp_declaration_30_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "udp_declaration_30" (DOUBLE(COLON, arg4);) }
	;

list_of_port_connections_3 : { Globals.grdbg "list_of_port_connections_3_2" ( EMPTY) }
	| arg3 = list_of_port_connections_3 COMMA arg5 = ordered_port_connection { Globals.grdbg "list_of_port_connections_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

list_of_port_connections_6 : { Globals.grdbg "list_of_port_connections_6_2" ( EMPTY) }
	| arg3 = list_of_port_connections_6 COMMA arg5 = dot_named_port_connection { Globals.grdbg "list_of_port_connections_6" (TRIPLE(arg3, COMMA, arg5);) }
	;

list_of_port_connections_8 : { Globals.grdbg "list_of_port_connections_8_2" ( EMPTY) }
	| arg3 = list_of_port_connections_8 arg4 = named_port_connection COMMA { Globals.grdbg "list_of_port_connections_8" (TRIPLE(arg3, arg4, COMMA);) }
	;

list_of_port_connections_10 : { Globals.grdbg "list_of_port_connections_10_2" ( EMPTY) }
	| arg3 = list_of_port_connections_10 COMMA arg5 = named_port_connection { Globals.grdbg "list_of_port_connections_10" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline d_hold_timing_check_9_4 : { Globals.grdbg "d_hold_timing_check_9_4_2" ( EMPTY) }
	| arg3 = variable_identifier { Globals.grdbg "d_hold_timing_check_9_4" ((arg3);) }
	;

%inline d_hold_timing_check_9 : { Globals.grdbg "d_hold_timing_check_9_2" ( EMPTY) }
	| COMMA arg4 = d_hold_timing_check_9_4 { Globals.grdbg "d_hold_timing_check_9" (DOUBLE(COMMA, arg4);) }
	;

%inline inout_declaration_3 : { Globals.grdbg "inout_declaration_3_2" ( EMPTY) }
	| arg3 = port_type { Globals.grdbg "inout_declaration_3" ((arg3);) }
	;

list_of_interface_identifiers_3 : { Globals.grdbg "list_of_interface_identifiers_3_2" ( EMPTY) }
	| arg3 = list_of_interface_identifiers_3 arg4 = unpacked_dimension { Globals.grdbg "list_of_interface_identifiers_3" (DOUBLE(arg3, arg4);) }
	;

list_of_interface_identifiers_4_6 : { Globals.grdbg "list_of_interface_identifiers_4_6_2" ( EMPTY) }
	| arg3 = list_of_interface_identifiers_4_6 arg4 = unpacked_dimension { Globals.grdbg "list_of_interface_identifiers_4_6" (DOUBLE(arg3, arg4);) }
	;

list_of_interface_identifiers_4 : { Globals.grdbg "list_of_interface_identifiers_4_2" ( EMPTY) }
	| arg3 = list_of_interface_identifiers_4 COMMA arg5 = identifier arg6 = list_of_interface_identifiers_4_6 { Globals.grdbg "list_of_interface_identifiers_4" (QUADRUPLE(arg3, COMMA, arg5, arg6);) }
	;

%inline d_setuphold_timing_check_11_4 : { Globals.grdbg "d_setuphold_timing_check_11_4_2" ( EMPTY) }
	| arg3 = variable_identifier { Globals.grdbg "d_setuphold_timing_check_11_4" ((arg3);) }
	;

%inline d_setuphold_timing_check_11_5_4 : { Globals.grdbg "d_setuphold_timing_check_11_5_4_2" ( EMPTY) }
	| arg3 = mintypmax_expression { Globals.grdbg "d_setuphold_timing_check_11_5_4" ((arg3);) }
	;

%inline d_setuphold_timing_check_11_5_5_4 : { Globals.grdbg "d_setuphold_timing_check_11_5_5_4_2" ( EMPTY) }
	| arg3 = mintypmax_expression { Globals.grdbg "d_setuphold_timing_check_11_5_5_4" ((arg3);) }
	;

%inline d_setuphold_timing_check_11_5_5_5_4 : { Globals.grdbg "d_setuphold_timing_check_11_5_5_5_4_2" ( EMPTY) }
	| arg3 = delayed_reference { Globals.grdbg "d_setuphold_timing_check_11_5_5_5_4" ((arg3);) }
	;

%inline d_setuphold_timing_check_11_5_5_5_5_4 : { Globals.grdbg "d_setuphold_timing_check_11_5_5_5_5_4_2" ( EMPTY) }
	| arg3 = delayed_data { Globals.grdbg "d_setuphold_timing_check_11_5_5_5_5_4" ((arg3);) }
	;

%inline d_setuphold_timing_check_11_5_5_5_5 : { Globals.grdbg "d_setuphold_timing_check_11_5_5_5_5_2" ( EMPTY) }
	| COMMA arg4 = d_setuphold_timing_check_11_5_5_5_5_4 { Globals.grdbg "d_setuphold_timing_check_11_5_5_5_5" (DOUBLE(COMMA, arg4);) }
	;

%inline d_setuphold_timing_check_11_5_5_5 : { Globals.grdbg "d_setuphold_timing_check_11_5_5_5_2" ( EMPTY) }
	| COMMA arg4 = d_setuphold_timing_check_11_5_5_5_4 arg5 = d_setuphold_timing_check_11_5_5_5_5 { Globals.grdbg "d_setuphold_timing_check_11_5_5_5" (TRIPLE(COMMA, arg4, arg5);) }
	;

%inline d_setuphold_timing_check_11_5_5 : { Globals.grdbg "d_setuphold_timing_check_11_5_5_2" ( EMPTY) }
	| COMMA arg4 = d_setuphold_timing_check_11_5_5_4 arg5 = d_setuphold_timing_check_11_5_5_5 { Globals.grdbg "d_setuphold_timing_check_11_5_5" (TRIPLE(COMMA, arg4, arg5);) }
	;

%inline d_setuphold_timing_check_11_5 : { Globals.grdbg "d_setuphold_timing_check_11_5_2" ( EMPTY) }
	| COMMA arg4 = d_setuphold_timing_check_11_5_4 arg5 = d_setuphold_timing_check_11_5_5 { Globals.grdbg "d_setuphold_timing_check_11_5" (TRIPLE(COMMA, arg4, arg5);) }
	;

%inline d_setuphold_timing_check_11 : { Globals.grdbg "d_setuphold_timing_check_11_2" ( EMPTY) }
	| COMMA arg4 = d_setuphold_timing_check_11_4 arg5 = d_setuphold_timing_check_11_5 { Globals.grdbg "d_setuphold_timing_check_11" (TRIPLE(COMMA, arg4, arg5);) }
	;

attribute_instance_4 : { Globals.grdbg "attribute_instance_4_2" ( EMPTY) }
	| arg3 = attribute_instance_4 COMMA arg5 = attr_spec { Globals.grdbg "attribute_instance_4" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline conditional_statement_2 : { Globals.grdbg "conditional_statement_2_2" ( EMPTY) }
	| arg3 = unique_priority { Globals.grdbg "conditional_statement_2" ((arg3);) }
	;

%inline conditional_statement_8 : { Globals.grdbg "conditional_statement_8_2" ( EMPTY) }
	| ELSE arg4 = statement_or_null { Globals.grdbg "conditional_statement_8" (DOUBLE(ELSE, arg4);) }
	;

%inline variable_declaration_2 : { Globals.grdbg "variable_declaration_2_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "variable_declaration_2" ((arg3);) }
	;

%inline action_block_4 : { Globals.grdbg "action_block_4_2" ( EMPTY) }
	| arg3 = statement { Globals.grdbg "action_block_4" ((arg3);) }
	;

%inline pull_gate_instance_2 : { Globals.grdbg "pull_gate_instance_2_2" ( EMPTY) }
	| arg3 = name_of_gate_instance { Globals.grdbg "pull_gate_instance_2" ((arg3);) }
	;

%inline d_removal_timing_check_9_4 : { Globals.grdbg "d_removal_timing_check_9_4_2" ( EMPTY) }
	| arg3 = variable_identifier { Globals.grdbg "d_removal_timing_check_9_4" ((arg3);) }
	;

%inline d_removal_timing_check_9 : { Globals.grdbg "d_removal_timing_check_9_2" ( EMPTY) }
	| COMMA arg4 = d_removal_timing_check_9_4 { Globals.grdbg "d_removal_timing_check_9" (DOUBLE(COMMA, arg4);) }
	;

%inline constant_function_call_4 : { Globals.grdbg "constant_function_call_4_2" ( EMPTY) }
	| LPAREN arg4 = list_of_constant_arguments RPAREN { Globals.grdbg "constant_function_call_4" (TRIPLE(LPAREN, arg4, RPAREN);) }
	;

%inline module_instantiation_3 : { Globals.grdbg "module_instantiation_3_2" ( EMPTY) }
	| arg3 = parameter_value_assignment { Globals.grdbg "module_instantiation_3" ((arg3);) }
	;

module_instantiation_5 : { Globals.grdbg "module_instantiation_5_2" ( EMPTY) }
	| arg3 = module_instantiation_5 COMMA arg5 = module_instance { Globals.grdbg "module_instantiation_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline hex_number_2 : { Globals.grdbg "hex_number_2_2" ( EMPTY) }
	| arg3 = non_zero_unsigned_number { Globals.grdbg "hex_number_2" ((arg3);) }
	;

%inline ordered_port_connection_3 : { Globals.grdbg "ordered_port_connection_3_2" ( EMPTY) }
	| arg3 = expression { Globals.grdbg "ordered_port_connection_3" ((arg3);) }
	;

%inline net_declaration_3 : { Globals.grdbg "net_declaration_3_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "net_declaration_3" ((arg3);) }
	;

%inline net_declaration_4 : { Globals.grdbg "net_declaration_4_2" ( EMPTY) }
	| arg3 = delay3 { Globals.grdbg "net_declaration_4" ((arg3);) }
	;

%inline net_declaration_9 : { Globals.grdbg "net_declaration_9_2" ( EMPTY) }
	| arg3 = drive_strength { Globals.grdbg "net_declaration_9" ((arg3);) }
	;

%inline net_declaration_10 : { Globals.grdbg "net_declaration_10_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "net_declaration_10" ((arg3);) }
	;

%inline net_declaration_11 : { Globals.grdbg "net_declaration_11_2" ( EMPTY) }
	| arg3 = delay3 { Globals.grdbg "net_declaration_11" ((arg3);) }
	;

%inline net_declaration_16 : { Globals.grdbg "net_declaration_16_2" ( EMPTY) }
	| VECTORED { Globals.grdbg "net_declaration_16_4" ((VECTORED);) }
	| SCALARED { Globals.grdbg "net_declaration_16" ((SCALARED);) }
	;

%inline net_declaration_17 : { Globals.grdbg "net_declaration_17_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "net_declaration_17" ((arg3);) }
	;

net_declaration_18 : { Globals.grdbg "net_declaration_18_2" ( EMPTY) }
	| arg3 = net_declaration_18 arg4 = packed_dimension { Globals.grdbg "net_declaration_18" (DOUBLE(arg3, arg4);) }
	;

%inline net_declaration_20 : { Globals.grdbg "net_declaration_20_2" ( EMPTY) }
	| arg3 = delay3 { Globals.grdbg "net_declaration_20" ((arg3);) }
	;

%inline net_declaration_25 : { Globals.grdbg "net_declaration_25_2" ( EMPTY) }
	| arg3 = drive_strength { Globals.grdbg "net_declaration_25" ((arg3);) }
	;

%inline net_declaration_26 : { Globals.grdbg "net_declaration_26_2" ( EMPTY) }
	| VECTORED { Globals.grdbg "net_declaration_26_4" ((VECTORED);) }
	| SCALARED { Globals.grdbg "net_declaration_26" ((SCALARED);) }
	;

%inline net_declaration_27 : { Globals.grdbg "net_declaration_27_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "net_declaration_27" ((arg3);) }
	;

net_declaration_28 : { Globals.grdbg "net_declaration_28_2" ( EMPTY) }
	| arg3 = net_declaration_28 arg4 = packed_dimension { Globals.grdbg "net_declaration_28" (DOUBLE(arg3, arg4);) }
	;

%inline net_declaration_30 : { Globals.grdbg "net_declaration_30_2" ( EMPTY) }
	| arg3 = delay3 { Globals.grdbg "net_declaration_30" ((arg3);) }
	;

%inline net_declaration_35 : { Globals.grdbg "net_declaration_35_2" ( EMPTY) }
	| arg3 = charge_strength { Globals.grdbg "net_declaration_35" ((arg3);) }
	;

%inline net_declaration_36 : { Globals.grdbg "net_declaration_36_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "net_declaration_36" ((arg3);) }
	;

%inline net_declaration_37 : { Globals.grdbg "net_declaration_37_2" ( EMPTY) }
	| arg3 = delay3 { Globals.grdbg "net_declaration_37" ((arg3);) }
	;

%inline net_declaration_42 : { Globals.grdbg "net_declaration_42_2" ( EMPTY) }
	| arg3 = drive_strength { Globals.grdbg "net_declaration_42" ((arg3);) }
	;

%inline net_declaration_43 : { Globals.grdbg "net_declaration_43_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "net_declaration_43" ((arg3);) }
	;

%inline net_declaration_44 : { Globals.grdbg "net_declaration_44_2" ( EMPTY) }
	| arg3 = delay3 { Globals.grdbg "net_declaration_44" ((arg3);) }
	;

%inline net_declaration_49 : { Globals.grdbg "net_declaration_49_2" ( EMPTY) }
	| arg3 = charge_strength { Globals.grdbg "net_declaration_49" ((arg3);) }
	;

%inline net_declaration_50 : { Globals.grdbg "net_declaration_50_2" ( EMPTY) }
	| VECTORED { Globals.grdbg "net_declaration_50_4" ((VECTORED);) }
	| SCALARED { Globals.grdbg "net_declaration_50" ((SCALARED);) }
	;

%inline net_declaration_51 : { Globals.grdbg "net_declaration_51_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "net_declaration_51" ((arg3);) }
	;

net_declaration_52 : { Globals.grdbg "net_declaration_52_2" ( EMPTY) }
	| arg3 = net_declaration_52 arg4 = packed_dimension { Globals.grdbg "net_declaration_52" (DOUBLE(arg3, arg4);) }
	;

%inline net_declaration_54 : { Globals.grdbg "net_declaration_54_2" ( EMPTY) }
	| arg3 = delay3 { Globals.grdbg "net_declaration_54" ((arg3);) }
	;

%inline net_declaration_59 : { Globals.grdbg "net_declaration_59_2" ( EMPTY) }
	| arg3 = drive_strength { Globals.grdbg "net_declaration_59" ((arg3);) }
	;

%inline net_declaration_60 : { Globals.grdbg "net_declaration_60_2" ( EMPTY) }
	| VECTORED { Globals.grdbg "net_declaration_60_4" ((VECTORED);) }
	| SCALARED { Globals.grdbg "net_declaration_60" ((SCALARED);) }
	;

%inline net_declaration_61 : { Globals.grdbg "net_declaration_61_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "net_declaration_61" ((arg3);) }
	;

net_declaration_62 : { Globals.grdbg "net_declaration_62_2" ( EMPTY) }
	| arg3 = net_declaration_62 arg4 = packed_dimension { Globals.grdbg "net_declaration_62" (DOUBLE(arg3, arg4);) }
	;

%inline net_declaration_64 : { Globals.grdbg "net_declaration_64_2" ( EMPTY) }
	| arg3 = delay3 { Globals.grdbg "net_declaration_64" ((arg3);) }
	;

%inline if_else_if_statement_2 : { Globals.grdbg "if_else_if_statement_2_2" ( EMPTY) }
	| arg3 = unique_priority { Globals.grdbg "if_else_if_statement_2" ((arg3);) }
	;

%inline if_else_if_statement_8_5 : { Globals.grdbg "if_else_if_statement_8_5_2" ( EMPTY) }
	| arg3 = unique_priority { Globals.grdbg "if_else_if_statement_8_5" ((arg3);) }
	;

if_else_if_statement_8 : { Globals.grdbg "if_else_if_statement_8_2" ( EMPTY) }
	| arg3 = if_else_if_statement_8 ELSE arg5 = if_else_if_statement_8_5 IF LPAREN arg8 = expression RPAREN arg10 = statement_or_null { Globals.grdbg "if_else_if_statement_8" (OCTUPLE(arg3, ELSE, arg5, IF, LPAREN, arg8, RPAREN, arg10);) }
	;

%inline if_else_if_statement_9 : { Globals.grdbg "if_else_if_statement_9_2" ( EMPTY) }
	| ELSE arg4 = statement_or_null { Globals.grdbg "if_else_if_statement_9" (DOUBLE(ELSE, arg4);) }
	;

%inline sequential_body_2 : { Globals.grdbg "sequential_body_2_2" ( EMPTY) }
	| arg3 = udp_initial_statement { Globals.grdbg "sequential_body_2" ((arg3);) }
	;

sequential_body_5 : { Globals.grdbg "sequential_body_5_2" ( EMPTY) }
	| arg3 = sequential_body_5 arg4 = sequential_entry { Globals.grdbg "sequential_body_5" (DOUBLE(arg3, arg4);) }
	;

%inline concurrent_assert_statement_2 : { Globals.grdbg "concurrent_assert_statement_2_2" ( EMPTY) }
	| arg3 = identifier COLON { Globals.grdbg "concurrent_assert_statement_2" (DOUBLE(arg3, COLON);) }
	;

%inline interface_nonansi_header_4 : { Globals.grdbg "interface_nonansi_header_4_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "interface_nonansi_header_4" ((arg3);) }
	;

%inline interface_nonansi_header_6 : { Globals.grdbg "interface_nonansi_header_6_2" ( EMPTY) }
	| arg3 = parameter_port_list { Globals.grdbg "interface_nonansi_header_6" ((arg3);) }
	;

modport_item_5 : { Globals.grdbg "modport_item_5_2" ( EMPTY) }
	| arg3 = modport_item_5 COMMA arg5 = modport_ports_declaration { Globals.grdbg "modport_item_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline octal_number_2 : { Globals.grdbg "octal_number_2_2" ( EMPTY) }
	| arg3 = non_zero_unsigned_number { Globals.grdbg "octal_number_2" ((arg3);) }
	;

%inline udp_output_declaration_10 : { Globals.grdbg "udp_output_declaration_10_2" ( EMPTY) }
	| EQUALS arg4 = constant_expression { Globals.grdbg "udp_output_declaration_10" (DOUBLE(EQUALS, arg4);) }
	;

list_of_type_assignments_3 : { Globals.grdbg "list_of_type_assignments_3_2" ( EMPTY) }
	| arg3 = list_of_type_assignments_3 COMMA arg5 = type_assignment { Globals.grdbg "list_of_type_assignments_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

inst_name_3 : { Globals.grdbg "inst_name_3_2" ( EMPTY) }
	| arg3 = inst_name_3 DOT arg5 = identifier { Globals.grdbg "inst_name_3" (TRIPLE(arg3, DOT, arg5);) }
	;

list_of_modport_port_identifiers_3 : { Globals.grdbg "list_of_modport_port_identifiers_3_2" ( EMPTY) }
	| arg3 = list_of_modport_port_identifiers_3 COMMA arg5 = identifier { Globals.grdbg "list_of_modport_port_identifiers_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

config_declaration_6 : { Globals.grdbg "config_declaration_6_2" ( EMPTY) }
	| arg3 = config_declaration_6 arg4 = config_rule_statement { Globals.grdbg "config_declaration_6" (DOUBLE(arg3, arg4);) }
	;

%inline implicit_class_handle_2 : { Globals.grdbg "implicit_class_handle_2_2" ( EMPTY) }
	| THIS DOT { Globals.grdbg "implicit_class_handle_2" (DOUBLE(THIS, DOT);) }
	;

%inline implicit_class_handle_4 : { Globals.grdbg "implicit_class_handle_4_2" ( EMPTY) }
	| SUPER DOT { Globals.grdbg "implicit_class_handle_4" (DOUBLE(SUPER, DOT);) }
	;

%inline specify_input_terminal_descriptor_3 : { Globals.grdbg "specify_input_terminal_descriptor_3_2" ( EMPTY) }
	| LBRACK arg4 = constant_range_expression RBRACK { Globals.grdbg "specify_input_terminal_descriptor_3" (TRIPLE(LBRACK, arg4, RBRACK);) }
	;

genvar_module_case_item_3 : { Globals.grdbg "genvar_module_case_item_3_2" ( EMPTY) }
	| arg3 = genvar_module_case_item_3 COMMA arg5 = constant_expression { Globals.grdbg "genvar_module_case_item_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline genvar_module_case_item_8 : { Globals.grdbg "genvar_module_case_item_8_2" ( EMPTY) }
	| COLON { Globals.grdbg "genvar_module_case_item_8" ((COLON);) }
	;

variable_lvalue_3 : { Globals.grdbg "variable_lvalue_3_2" ( EMPTY) }
	| arg3 = variable_lvalue_3 LBRACK arg5 = expression RBRACK { Globals.grdbg "variable_lvalue_3" (QUADRUPLE(arg3, LBRACK, arg5, RBRACK);) }
	;

%inline variable_lvalue_4 : { Globals.grdbg "variable_lvalue_4_2" ( EMPTY) }
	| LBRACK arg4 = range_expression RBRACK { Globals.grdbg "variable_lvalue_4" (TRIPLE(LBRACK, arg4, RBRACK);) }
	;

variable_lvalue_8 : { Globals.grdbg "variable_lvalue_8_2" ( EMPTY) }
	| arg3 = variable_lvalue_8 COMMA arg5 = variable_lvalue { Globals.grdbg "variable_lvalue_8" (TRIPLE(arg3, COMMA, arg5);) }
	;

list_of_defparam_assignments_3 : { Globals.grdbg "list_of_defparam_assignments_3_2" ( EMPTY) }
	| arg3 = list_of_defparam_assignments_3 COMMA arg5 = defparam_assignment { Globals.grdbg "list_of_defparam_assignments_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline port_reference_3 : { Globals.grdbg "port_reference_3_2" ( EMPTY) }
	| LBRACK arg4 = constant_range_expression RBRACK { Globals.grdbg "port_reference_3" (TRIPLE(LBRACK, arg4, RBRACK);) }
	;

list_of_clocking_decl_assign_3 : { Globals.grdbg "list_of_clocking_decl_assign_3_2" ( EMPTY) }
	| arg3 = list_of_clocking_decl_assign_3 COMMA arg5 = clocking_decl_assign { Globals.grdbg "list_of_clocking_decl_assign_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline list_of_tf_variable_identifiers_4 : { Globals.grdbg "list_of_tf_variable_identifiers_4_2" ( EMPTY) }
	| EQUALS arg4 = expression { Globals.grdbg "list_of_tf_variable_identifiers_4" (DOUBLE(EQUALS, arg4);) }
	;

%inline list_of_tf_variable_identifiers_5_7 : { Globals.grdbg "list_of_tf_variable_identifiers_5_7_2" ( EMPTY) }
	| EQUALS arg4 = expression { Globals.grdbg "list_of_tf_variable_identifiers_5_7" (DOUBLE(EQUALS, arg4);) }
	;

list_of_tf_variable_identifiers_5 : { Globals.grdbg "list_of_tf_variable_identifiers_5_2" ( EMPTY) }
	| arg3 = list_of_tf_variable_identifiers_5 COMMA arg5 = identifier arg6 = variable_dimension arg7 = list_of_tf_variable_identifiers_5_7 { Globals.grdbg "list_of_tf_variable_identifiers_5" (QUINTUPLE(arg3, COMMA, arg5, arg6, arg7);) }
	;

%inline gate_instantiation_3 : { Globals.grdbg "gate_instantiation_3_2" ( EMPTY) }
	| arg3 = delay3 { Globals.grdbg "gate_instantiation_3" ((arg3);) }
	;

gate_instantiation_5 : { Globals.grdbg "gate_instantiation_5_2" ( EMPTY) }
	| arg3 = gate_instantiation_5 COMMA arg5 = cmos_switch_instance { Globals.grdbg "gate_instantiation_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline gate_instantiation_9 : { Globals.grdbg "gate_instantiation_9_2" ( EMPTY) }
	| arg3 = drive_strength { Globals.grdbg "gate_instantiation_9" ((arg3);) }
	;

%inline gate_instantiation_10 : { Globals.grdbg "gate_instantiation_10_2" ( EMPTY) }
	| arg3 = delay3 { Globals.grdbg "gate_instantiation_10" ((arg3);) }
	;

gate_instantiation_12 : { Globals.grdbg "gate_instantiation_12_2" ( EMPTY) }
	| arg3 = gate_instantiation_12 COMMA arg5 = enable_gate_instance { Globals.grdbg "gate_instantiation_12" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline gate_instantiation_16 : { Globals.grdbg "gate_instantiation_16_2" ( EMPTY) }
	| arg3 = delay3 { Globals.grdbg "gate_instantiation_16" ((arg3);) }
	;

gate_instantiation_18 : { Globals.grdbg "gate_instantiation_18_2" ( EMPTY) }
	| arg3 = gate_instantiation_18 COMMA arg5 = mos_switch_instance { Globals.grdbg "gate_instantiation_18" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline gate_instantiation_22 : { Globals.grdbg "gate_instantiation_22_2" ( EMPTY) }
	| arg3 = drive_strength { Globals.grdbg "gate_instantiation_22" ((arg3);) }
	;

%inline gate_instantiation_23 : { Globals.grdbg "gate_instantiation_23_2" ( EMPTY) }
	| arg3 = delay2 { Globals.grdbg "gate_instantiation_23" ((arg3);) }
	;

gate_instantiation_25 : { Globals.grdbg "gate_instantiation_25_2" ( EMPTY) }
	| arg3 = gate_instantiation_25 COMMA arg5 = n_input_gate_instance { Globals.grdbg "gate_instantiation_25" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline gate_instantiation_29 : { Globals.grdbg "gate_instantiation_29_2" ( EMPTY) }
	| arg3 = drive_strength { Globals.grdbg "gate_instantiation_29" ((arg3);) }
	;

%inline gate_instantiation_30 : { Globals.grdbg "gate_instantiation_30_2" ( EMPTY) }
	| arg3 = delay2 { Globals.grdbg "gate_instantiation_30" ((arg3);) }
	;

gate_instantiation_32 : { Globals.grdbg "gate_instantiation_32_2" ( EMPTY) }
	| arg3 = gate_instantiation_32 COMMA arg5 = n_output_gate_instance { Globals.grdbg "gate_instantiation_32" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline gate_instantiation_36 : { Globals.grdbg "gate_instantiation_36_2" ( EMPTY) }
	| arg3 = delay2 { Globals.grdbg "gate_instantiation_36" ((arg3);) }
	;

gate_instantiation_38 : { Globals.grdbg "gate_instantiation_38_2" ( EMPTY) }
	| arg3 = gate_instantiation_38 COMMA arg5 = pass_enable_switch_instance { Globals.grdbg "gate_instantiation_38" (TRIPLE(arg3, COMMA, arg5);) }
	;

gate_instantiation_43 : { Globals.grdbg "gate_instantiation_43_2" ( EMPTY) }
	| arg3 = gate_instantiation_43 COMMA arg5 = pass_switch_instance { Globals.grdbg "gate_instantiation_43" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline gate_instantiation_47 : { Globals.grdbg "gate_instantiation_47_2" ( EMPTY) }
	| arg3 = pulldown_strength { Globals.grdbg "gate_instantiation_47" ((arg3);) }
	;

gate_instantiation_49 : { Globals.grdbg "gate_instantiation_49_2" ( EMPTY) }
	| arg3 = gate_instantiation_49 COMMA arg5 = pull_gate_instance { Globals.grdbg "gate_instantiation_49" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline gate_instantiation_53 : { Globals.grdbg "gate_instantiation_53_2" ( EMPTY) }
	| arg3 = pullup_strength { Globals.grdbg "gate_instantiation_53" ((arg3);) }
	;

gate_instantiation_55 : { Globals.grdbg "gate_instantiation_55_2" ( EMPTY) }
	| arg3 = gate_instantiation_55 COMMA arg5 = pull_gate_instance { Globals.grdbg "gate_instantiation_55" (TRIPLE(arg3, COMMA, arg5);) }
	;

range_or_type_2 : { Globals.grdbg "range_or_type_2_2" ( EMPTY) }
	| arg3 = range_or_type_2 arg4 = packed_dimension { Globals.grdbg "range_or_type_2" (DOUBLE(arg3, arg4);) }
	;

task_port_list_3 : { Globals.grdbg "task_port_list_3_2" ( EMPTY) }
	| arg3 = task_port_list_3 COMMA arg5 = task_port_item { Globals.grdbg "task_port_list_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

task_port_list_6 : { Globals.grdbg "task_port_list_6_2" ( EMPTY) }
	| arg3 = task_port_list_6 COMMA arg5 = task_port_item { Globals.grdbg "task_port_list_6" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline tf_output_declaration_3 : { Globals.grdbg "tf_output_declaration_3_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "tf_output_declaration_3" ((arg3);) }
	;

tf_output_declaration_4 : { Globals.grdbg "tf_output_declaration_4_2" ( EMPTY) }
	| arg3 = tf_output_declaration_4 arg4 = packed_dimension { Globals.grdbg "tf_output_declaration_4" (DOUBLE(arg3, arg4);) }
	;

%inline udp_instance_2 : { Globals.grdbg "udp_instance_2_2" ( EMPTY) }
	| arg3 = NAME_OF_UDP_INSTANCE { Globals.grdbg "udp_instance_2" ((NAME_OF_UDP_INSTANCE arg3);) }
	;

udp_instance_3 : { Globals.grdbg "udp_instance_3_2" ( EMPTY) }
	| arg3 = udp_instance_3 arg4 = range { Globals.grdbg "udp_instance_3" (DOUBLE(arg3, arg4);) }
	;

udp_instance_8 : { Globals.grdbg "udp_instance_8_2" ( EMPTY) }
	| arg3 = udp_instance_8 COMMA arg5 = expression { Globals.grdbg "udp_instance_8" (TRIPLE(arg3, COMMA, arg5);) }
	;

list_of_function_proto_formals_2_5 : { Globals.grdbg "list_of_function_proto_formals_2_5_2" ( EMPTY) }
	| arg3 = list_of_function_proto_formals_2_5 COMMA arg6 = function_proto_formal { Globals.grdbg "list_of_function_proto_formals_2_5" (TRIPLE(arg3, COMMA, arg6);) }
	;

%inline list_of_function_proto_formals_2 : { Globals.grdbg "list_of_function_proto_formals_2_2" ( EMPTY) }
	| arg4 = function_proto_formal arg5 = list_of_function_proto_formals_2_5 { Globals.grdbg "list_of_function_proto_formals_2" (DOUBLE(arg4, arg5);) }
	;

simple_hierarchical_branch_3 : { Globals.grdbg "simple_hierarchical_branch_3_2" ( EMPTY) }
	| arg3 = simple_hierarchical_branch_3 LBRACK arg5 = UNSIGNED_NUMBER RBRACK { Globals.grdbg "simple_hierarchical_branch_3" (QUADRUPLE(arg3, LBRACK, UNSIGNED_NUMBER arg5, RBRACK);) }
	;

simple_hierarchical_branch_4_3_6 : { Globals.grdbg "simple_hierarchical_branch_4_3_6_2" ( EMPTY) }
	| arg3 = simple_hierarchical_branch_4_3_6 LBRACK arg5 = UNSIGNED_NUMBER RBRACK { Globals.grdbg "simple_hierarchical_branch_4_3_6" (QUADRUPLE(arg3, LBRACK, UNSIGNED_NUMBER arg5, RBRACK);) }
	;

simple_hierarchical_branch_4_3 : { Globals.grdbg "simple_hierarchical_branch_4_3_2" ( EMPTY) }
	| arg3 = simple_hierarchical_branch_4_3 DOT arg5 = SIMPLE_IDENTIFIER arg6 = simple_hierarchical_branch_4_3_6 { Globals.grdbg "simple_hierarchical_branch_4_3" (QUADRUPLE(arg3, DOT, SIMPLE_IDENTIFIER arg5, arg6);) }
	;

%inline simple_hierarchical_branch_4 : { Globals.grdbg "simple_hierarchical_branch_4_2" ( EMPTY) }
	| arg3 = simple_hierarchical_branch_4_3 { Globals.grdbg "simple_hierarchical_branch_4" ((arg3);) }
	;

%inline n_input_gate_instance_2 : { Globals.grdbg "n_input_gate_instance_2_2" ( EMPTY) }
	| arg3 = name_of_gate_instance { Globals.grdbg "n_input_gate_instance_2" ((arg3);) }
	;

n_input_gate_instance_7 : { Globals.grdbg "n_input_gate_instance_7_2" ( EMPTY) }
	| arg3 = n_input_gate_instance_7 COMMA arg5 = expression { Globals.grdbg "n_input_gate_instance_7" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline variable_decl_assignment_3 : { Globals.grdbg "variable_decl_assignment_3_2" ( EMPTY) }
	| arg3 = variable_dimension { Globals.grdbg "variable_decl_assignment_3" ((arg3);) }
	;

%inline variable_decl_assignment_4 : { Globals.grdbg "variable_decl_assignment_4_2" ( EMPTY) }
	| EQUALS arg4 = constant_expression { Globals.grdbg "variable_decl_assignment_4" (DOUBLE(EQUALS, arg4);) }
	;

%inline variable_decl_assignment_14 : { Globals.grdbg "variable_decl_assignment_14_2" ( EMPTY) }
	| LPAREN arg4 = identifier RPAREN { Globals.grdbg "variable_decl_assignment_14" (TRIPLE(LPAREN, arg4, RPAREN);) }
	;

%inline variable_decl_assignment_17 : { Globals.grdbg "variable_decl_assignment_17_2" ( EMPTY) }
	| arg3 = parameter_value_assignment { Globals.grdbg "variable_decl_assignment_17" ((arg3);) }
	;

%inline variable_decl_assignment_20 : { Globals.grdbg "variable_decl_assignment_20_2" ( EMPTY) }
	| LPAREN arg4 = list_of_arguments RPAREN { Globals.grdbg "variable_decl_assignment_20" (TRIPLE(LPAREN, arg4, RPAREN);) }
	;

%inline list_of_arguments_2 : { Globals.grdbg "list_of_arguments_2_2" ( EMPTY) }
	| arg3 = expression { Globals.grdbg "list_of_arguments_2" ((arg3);) }
	;

%inline list_of_arguments_3_5 : { Globals.grdbg "list_of_arguments_3_5_2" ( EMPTY) }
	| arg3 = expression { Globals.grdbg "list_of_arguments_3_5" ((arg3);) }
	;

list_of_arguments_3 : { Globals.grdbg "list_of_arguments_3_2" ( EMPTY) }
	| arg3 = list_of_arguments_3 COMMA arg5 = list_of_arguments_3_5 { Globals.grdbg "list_of_arguments_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline list_of_arguments_8 : { Globals.grdbg "list_of_arguments_8_2" ( EMPTY) }
	| arg3 = expression { Globals.grdbg "list_of_arguments_8" ((arg3);) }
	;

%inline list_of_arguments_10_8 : { Globals.grdbg "list_of_arguments_10_8_2" ( EMPTY) }
	| arg3 = expression { Globals.grdbg "list_of_arguments_10_8" ((arg3);) }
	;

list_of_arguments_10 : { Globals.grdbg "list_of_arguments_10_2" ( EMPTY) }
	| arg3 = list_of_arguments_10 COMMA DOT arg6 = identifier LPAREN arg8 = list_of_arguments_10_8 RPAREN { Globals.grdbg "list_of_arguments_10" (SEPTUPLE(arg3, COMMA, DOT, arg6, LPAREN, arg8, RPAREN);) }
	;

%inline pass_enable_switch_instance_2 : { Globals.grdbg "pass_enable_switch_instance_2_2" ( EMPTY) }
	| arg3 = name_of_gate_instance { Globals.grdbg "pass_enable_switch_instance_2" ((arg3);) }
	;

specify_block_3 : { Globals.grdbg "specify_block_3_2" ( EMPTY) }
	| arg3 = specify_block_3 arg4 = specify_item { Globals.grdbg "specify_block_3" (DOUBLE(arg3, arg4);) }
	;

%inline interface_instantiation_3 : { Globals.grdbg "interface_instantiation_3_2" ( EMPTY) }
	| arg3 = parameter_value_assignment { Globals.grdbg "interface_instantiation_3" ((arg3);) }
	;

interface_instantiation_5 : { Globals.grdbg "interface_instantiation_5_2" ( EMPTY) }
	| arg3 = interface_instantiation_5 COMMA arg5 = module_instance { Globals.grdbg "interface_instantiation_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline function_conditional_statement_2 : { Globals.grdbg "function_conditional_statement_2_2" ( EMPTY) }
	| arg3 = unique_priority { Globals.grdbg "function_conditional_statement_2" ((arg3);) }
	;

%inline function_conditional_statement_8 : { Globals.grdbg "function_conditional_statement_8_2" ( EMPTY) }
	| ELSE arg4 = function_statement_or_null { Globals.grdbg "function_conditional_statement_8" (DOUBLE(ELSE, arg4);) }
	;

primary_6 : { Globals.grdbg "primary_6_2" ( EMPTY) }
	| arg3 = primary_6 LBRACK arg5 = expression RBRACK { Globals.grdbg "primary_6" (QUADRUPLE(arg3, LBRACK, arg5, RBRACK);) }
	;

%inline primary_7 : { Globals.grdbg "primary_7_2" ( EMPTY) }
	| LBRACK arg4 = range_expression RBRACK { Globals.grdbg "primary_7" (TRIPLE(LBRACK, arg4, RBRACK);) }
	;

primary_8_6_5 : { Globals.grdbg "primary_8_6_5_2" ( EMPTY) }
	| arg3 = primary_8_6_5 COMMA arg5 = expression { Globals.grdbg "primary_8_6_5" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline primary_8_6 : { Globals.grdbg "primary_8_6_2" ( EMPTY) }
	| LPAREN arg4 = expression arg5 = primary_8_6_5 RPAREN { Globals.grdbg "primary_8_6" (QUADRUPLE(LPAREN, arg4, arg5, RPAREN);) }
	;

%inline primary_8 : { Globals.grdbg "primary_8_2" ( EMPTY) }
	| DOT arg4 = identifier arg6 = primary_8_6 { Globals.grdbg "primary_8" (TRIPLE(DOT, arg4, arg6);) }
	;

primary_22 : { Globals.grdbg "primary_22_2" ( EMPTY) }
	| arg3 = primary_22 arg4 = identifier P_COLONCOLON { Globals.grdbg "primary_22" (TRIPLE(arg3, arg4, P_COLONCOLON);) }
	;

level_input_list_3 : { Globals.grdbg "level_input_list_3_2" ( EMPTY) }
	| arg3 = level_input_list_3 arg4 = LEVEL_SYMBOL { Globals.grdbg "level_input_list_3" (DOUBLE(arg3, LEVEL_SYMBOL arg4);) }
	;

%inline modport_simple_ports_declaration_12 : { Globals.grdbg "modport_simple_ports_declaration_12_2" ( EMPTY) }
	| arg3 = data_type { Globals.grdbg "modport_simple_ports_declaration_12" ((arg3);) }
	;

%inline named_parameter_assignment_5 : { Globals.grdbg "named_parameter_assignment_5_2" ( EMPTY) }
	| arg3 = expression { Globals.grdbg "named_parameter_assignment_5" ((arg3);) }
	;

%inline function_body_declaration_2 : { Globals.grdbg "function_body_declaration_2_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "function_body_declaration_2" ((arg3);) }
	;

%inline function_body_declaration_3 : { Globals.grdbg "function_body_declaration_3_2" ( EMPTY) }
	| arg3 = range_or_type { Globals.grdbg "function_body_declaration_3" ((arg3);) }
	;

%inline function_body_declaration_4 : { Globals.grdbg "function_body_declaration_4_2" ( EMPTY) }
	| arg3 = identifier DOT { Globals.grdbg "function_body_declaration_4" (DOUBLE(arg3, DOT);) }
	;

function_body_declaration_7 : { Globals.grdbg "function_body_declaration_7_2" ( EMPTY) }
	| arg3 = function_body_declaration_7 arg4 = function_item_declaration { Globals.grdbg "function_body_declaration_7" (DOUBLE(arg3, arg4);) }
	;

function_body_declaration_8 : { Globals.grdbg "function_body_declaration_8_2" ( EMPTY) }
	| arg3 = function_body_declaration_8 arg4 = function_statement_or_null { Globals.grdbg "function_body_declaration_8" (DOUBLE(arg3, arg4);) }
	;

%inline function_body_declaration_10 : { Globals.grdbg "function_body_declaration_10_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "function_body_declaration_10" (DOUBLE(COLON, arg4);) }
	;

%inline function_body_declaration_12 : { Globals.grdbg "function_body_declaration_12_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "function_body_declaration_12" ((arg3);) }
	;

%inline function_body_declaration_13 : { Globals.grdbg "function_body_declaration_13_2" ( EMPTY) }
	| arg3 = range_or_type { Globals.grdbg "function_body_declaration_13" ((arg3);) }
	;

%inline function_body_declaration_14 : { Globals.grdbg "function_body_declaration_14_2" ( EMPTY) }
	| arg3 = identifier DOT { Globals.grdbg "function_body_declaration_14" (DOUBLE(arg3, DOT);) }
	;

function_body_declaration_20 : { Globals.grdbg "function_body_declaration_20_2" ( EMPTY) }
	| arg3 = function_body_declaration_20 arg4 = block_item_declaration { Globals.grdbg "function_body_declaration_20" (DOUBLE(arg3, arg4);) }
	;

function_body_declaration_21 : { Globals.grdbg "function_body_declaration_21_2" ( EMPTY) }
	| arg3 = function_body_declaration_21 arg4 = function_statement_or_null { Globals.grdbg "function_body_declaration_21" (DOUBLE(arg3, arg4);) }
	;

%inline function_body_declaration_23 : { Globals.grdbg "function_body_declaration_23_2" ( EMPTY) }
	| COLON arg4 = identifier { Globals.grdbg "function_body_declaration_23" (DOUBLE(COLON, arg4);) }
	;

%inline generate_module_item_8 : { Globals.grdbg "generate_module_item_8_2" ( EMPTY) }
	| arg3 = identifier COLON { Globals.grdbg "generate_module_item_8" (DOUBLE(arg3, COLON);) }
	;

%inline clocking_skew_3 : { Globals.grdbg "clocking_skew_3_2" ( EMPTY) }
	| arg3 = delay_control { Globals.grdbg "clocking_skew_3" ((arg3);) }
	;

%inline function_if_else_if_statement_2 : { Globals.grdbg "function_if_else_if_statement_2_2" ( EMPTY) }
	| arg3 = unique_priority { Globals.grdbg "function_if_else_if_statement_2" ((arg3);) }
	;

%inline function_if_else_if_statement_8_5 : { Globals.grdbg "function_if_else_if_statement_8_5_2" ( EMPTY) }
	| arg3 = unique_priority { Globals.grdbg "function_if_else_if_statement_8_5" ((arg3);) }
	;

function_if_else_if_statement_8 : { Globals.grdbg "function_if_else_if_statement_8_2" ( EMPTY) }
	| arg3 = function_if_else_if_statement_8 ELSE arg5 = function_if_else_if_statement_8_5 IF LPAREN arg8 = expression RPAREN arg10 = function_statement_or_null { Globals.grdbg "function_if_else_if_statement_8" (OCTUPLE(arg3, ELSE, arg5, IF, LPAREN, arg8, RPAREN, arg10);) }
	;

%inline function_if_else_if_statement_9 : { Globals.grdbg "function_if_else_if_statement_9_2" ( EMPTY) }
	| ELSE arg4 = function_statement_or_null { Globals.grdbg "function_if_else_if_statement_9" (DOUBLE(ELSE, arg4);) }
	;

genvar_interface_case_item_3 : { Globals.grdbg "genvar_interface_case_item_3_2" ( EMPTY) }
	| arg3 = genvar_interface_case_item_3 COMMA arg5 = constant_expression { Globals.grdbg "genvar_interface_case_item_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline genvar_interface_case_item_8 : { Globals.grdbg "genvar_interface_case_item_8_2" ( EMPTY) }
	| COLON { Globals.grdbg "genvar_interface_case_item_8" ((COLON);) }
	;

%inline cell_clause_3 : { Globals.grdbg "cell_clause_3_2" ( EMPTY) }
	| arg3 = identifier DOT { Globals.grdbg "cell_clause_3" (DOUBLE(arg3, DOT);) }
	;

dist_list_3 : { Globals.grdbg "dist_list_3_2" ( EMPTY) }
	| arg3 = dist_list_3 COMMA arg5 = dist_item { Globals.grdbg "dist_list_3" (TRIPLE(arg3, COMMA, arg5);) }
	;

%inline dpi_function_proto_4 : { Globals.grdbg "dpi_function_proto_4_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "dpi_function_proto_4" ((arg3);) }
	;

%inline parameter_declaration_3 : { Globals.grdbg "parameter_declaration_3_2" ( EMPTY) }
	| arg3 = signing { Globals.grdbg "parameter_declaration_3" ((arg3);) }
	;

parameter_declaration_4 : { Globals.grdbg "parameter_declaration_4_2" ( EMPTY) }
	| arg3 = parameter_declaration_4 arg4 = packed_dimension { Globals.grdbg "parameter_declaration_4" (DOUBLE(arg3, arg4);) }
	;

%inline parameter_declaration_5 : { Globals.grdbg "parameter_declaration_5_2" ( EMPTY) }
	| arg3 = range { Globals.grdbg "parameter_declaration_5" ((arg3);) }
	;

escaped_hierarchical_identifier_3 : { Globals.grdbg "escaped_hierarchical_identifier_3_2" ( EMPTY) }
	| arg3 = escaped_hierarchical_identifier_3 DOT arg5 = simple_hierarchical_branch { Globals.grdbg "escaped_hierarchical_identifier_3_6" (TRIPLE(arg3, DOT, arg5);) }
	| DOT arg8 = escaped_hierarchical_branch { Globals.grdbg "escaped_hierarchical_identifier_3" (DOUBLE(DOT, arg8);) }
	;

%inline function_declaration_3 : { Globals.grdbg "function_declaration_3_2" ( EMPTY) }
	| arg3 = lifetime { Globals.grdbg "function_declaration_3" ((arg3);) }
	;

// END_OF_FILE
