%{
open Typedef

let pos_fn = ref "";;
let pos_ln = ref 0;;
let pos_cn = ref 0;;

let delunderscore strg = String.concat "" (Str.split (Str.regexp "['_']+") strg)
;;

let string2base_number strg =
	try
	let pos = String.index strg '\'' in
		let b = String.get strg (pos+1)
		and len = try
			int_of_string (delunderscore (String.sub strg 0 pos))
		with int_of_string -> begin
			Printf.printf "fatal error : base number without width %s at " strg;
			Printf.printf "file : %s " !pos_fn;
			Printf.printf "line : %d " ((!pos_ln) + 1); (*because it start from 0*)
			Printf.printf "char : %d\n" !pos_cn;
			exit 1
		end
		and num = delunderscore(String.sub strg (pos+2) ((String.length strg)-pos-2))
		in begin
			match b with
			'b' -> begin
				let lennum = String.length num
				in begin
					if lennum = len then T_number_base(len,'b',num)
					else if lennum < len then T_number_base(len,'b',(String.concat "" [(String.make (len - lennum) '0');num]))
					else begin
						Printf.printf  "warning : lennum > len %s\n" strg;
						T_number_base(len,'b',(String.sub num (lennum-len) len))
					end
				end
			end
			| 'B' -> begin
				let lennum = String.length num
				in begin
					if lennum = len then T_number_base(len,'b',num)
					else if lennum < len then T_number_base(len,'b',(String.concat "" [(String.make (len - lennum) '0');num]))
					else begin
						Printf.printf  "warning : lennum > len %s\n" strg;
						T_number_base(len,'b',(String.sub num (lennum-len) len))
					end
				end
			end
			| 'h' -> begin
				Printf.printf "NOTE : to convert %s 1\n" strg;
				let newnum = Misc2.hexstring2binstring num
				in
				let lennum = String.length newnum
				in begin
					if lennum = len then T_number_base(len,'b',newnum)
					else if lennum < len then T_number_base(len,'b',(String.concat "" [(String.make (len - lennum) '0');newnum]))
					else begin
						Printf.printf  "warning : lennum > len %s\n" strg;
						T_number_base(len,'b',(String.sub newnum (lennum-len) len))
					end
				end
			end
			| 'H' -> begin
				Printf.printf "NOTE : to convert %s 2\n" strg;
				let newnum = Misc2.hexstring2binstring num
				in
				let lennum = String.length newnum
				in begin
					if lennum = len then T_number_base(len,'b',newnum)
					else if lennum < len then T_number_base(len,'b',(String.concat "" [(String.make (len - lennum) '0');newnum]))
					else begin
						Printf.printf  "warning : lennum > len %s\n" strg;
						T_number_base(len,'b',(String.sub newnum (lennum-len) len))
					end
				end
			end
			| 'd' -> begin
				let newnum = Misc2.decstring2binstring num
				in
				let lennum = String.length newnum
				in begin
					if lennum = len then T_number_base(len,'b',newnum)
					else if lennum < len then T_number_base(len,'b',(String.concat "" [(String.make (len - lennum) '0');newnum]))
					else begin
						Printf.printf  "warning : lennum > len %s\n" strg;
						T_number_base(len,'b',(String.sub newnum (lennum-len) len))
					end
				end
			end
			| 'D' -> begin
				let newnum = Misc2.decstring2binstring num
				in
				let lennum = String.length newnum
				in begin
					if lennum = len then T_number_base(len,'b',newnum)
					else if lennum < len then T_number_base(len,'b',(String.concat "" [(String.make (len - lennum) '0');newnum]))
					else begin
						Printf.printf  "warning : lennum > len %s\n" strg;
						T_number_base(len,'b',(String.sub newnum (lennum-len) len))
					end
				end
			end
			| _ -> begin
				Printf.printf  "fatal error : invalid number %s\n" strg;
				exit 1
			end
		end
	with Not_found -> print_endline "not found";print_endline strg; exit 1
;;

let parse_error str = begin
	Printf.printf "fatal error : %s at " str;
	Printf.printf "file : %s " !pos_fn;
	Printf.printf "line : %d " ((!pos_ln) + 1); (*because it start from 0*)
	Printf.printf "char : %d\n" !pos_cn;
	exit 1
end
;;
let get_pos (pos : Lexing.position ) = begin
	pos_fn := pos.Lexing.pos_fname;
	pos_ln := pos.Lexing.pos_lnum;
	pos_cn := pos.Lexing.pos_cnum-pos.Lexing.pos_bol
end
;;
let get_endpos pos2 = begin
	match pos2 with
	(_,pos) -> get_pos pos
end
;;
%}

%token <Lexing.position*Lexing.position> KEY_MODULE
%token <Lexing.position*Lexing.position> KEY_ENDMODULE
%token <Lexing.position*Lexing.position> KEY_INPUT
%token <Lexing.position*Lexing.position> KEY_INOUT
%token <Lexing.position*Lexing.position> KEY_OUTPUT
%token <Lexing.position*Lexing.position> KEY_SMALL
%token <Lexing.position*Lexing.position> KEY_MEDIUM
%token <Lexing.position*Lexing.position> KEY_LARGE
%token <Lexing.position*Lexing.position> KEY_SCALARED
%token <Lexing.position*Lexing.position> KEY_VECTORED
%token <Lexing.position*Lexing.position> KEY_ASSIGN
%token <Lexing.position*Lexing.position> KEY_REG
%token <Lexing.position*Lexing.position> KEY_ALWAYS
%token <Lexing.position*Lexing.position> KEY_IF
%token <Lexing.position*Lexing.position> KEY_ELSE
%token <Lexing.position*Lexing.position> KEY_CASE
%token <Lexing.position*Lexing.position> KEY_ENDCASE
%token <Lexing.position*Lexing.position> KEY_DISABLE
%token <Lexing.position*Lexing.position> KEY_FORCE
%token <Lexing.position*Lexing.position> KEY_DEFAULT
%token <Lexing.position*Lexing.position> KEY_CASEZ
%token <Lexing.position*Lexing.position> KEY_CASEX
%token <Lexing.position*Lexing.position> KEY_FOREVER
%token <Lexing.position*Lexing.position> KEY_REPEAT
%token <Lexing.position*Lexing.position> KEY_WHILE
%token <Lexing.position*Lexing.position> KEY_FOR
%token <Lexing.position*Lexing.position> KEY_WAIT
%token <Lexing.position*Lexing.position> KEY_RELEASE
%token <Lexing.position*Lexing.position> KEY_FORK
%token <Lexing.position*Lexing.position> KEY_JOIN
%token <Lexing.position*Lexing.position> KEY_EVENT
%token <Lexing.position*Lexing.position> KEY_TIME
%token <Lexing.position*Lexing.position> KEY_REAL
%token <Lexing.position*Lexing.position> KEY_INTEGER
%token <Lexing.position*Lexing.position> KEY_PARAMETER
%token <Lexing.position*Lexing.position> KEY_BEGIN
%token <Lexing.position*Lexing.position> KEY_END
%token <Lexing.position*Lexing.position> KEY_EDGE
%token <Lexing.position*Lexing.position> KEY_POSEDGE
%token <Lexing.position*Lexing.position> KEY_NEGEDGE
%token <Lexing.position*Lexing.position> KEY_OR
%token <Lexing.position*Lexing.position> KEY_SPECIFY
%token <Lexing.position*Lexing.position> KEY_ENDSPECIFY
%token <Lexing.position*Lexing.position> KEY_TASK
%token <Lexing.position*Lexing.position> KEY_ENDTASK
%token <Lexing.position*Lexing.position> KEY_ENDFUNCTION
%token <Lexing.position*Lexing.position> KEY_FUNCTION
%token <Lexing.position*Lexing.position> KEY_INITIAL
%token <Lexing.position*Lexing.position> KEY_SPECPARAM
%token <Lexing.position*Lexing.position> KEY_DEFPARAM
%token <Lexing.position*Lexing.position> LEADTO
%token <Lexing.position*Lexing.position> AT

%token <string> IDENTIFIER
/*
%token <string> DECIMAL_NUMBER
*/
%token <string> UNSIGNED_NUMBER
%token <string> FLOAT_NUMBER
%token <string> BASE_NUMBER
%token <string> DOLLOR_SYSTEM_IDENTIFIER

%token DOLLOR_SETUP
%token DOLLOR_HOLD
%token DOLLOR_PERIOD
%token DOLLOR_WIDTH
%token DOLLOR_SKEW
%token DOLLOR_RECOVERY
%token DOLLOR_SETUPHOLD
%token <Lexing.position*Lexing.position> AND3

%token <Lexing.position*Lexing.position> EOL
%token <Lexing.position*Lexing.position> EOF
%token <Lexing.position*Lexing.position> LBRACE
%token <Lexing.position*Lexing.position> RBRACE
%token <Lexing.position*Lexing.position> LBRACKET
%token <Lexing.position*Lexing.position> RBRACKET
%token <Lexing.position*Lexing.position> LPAREN
%token <Lexing.position*Lexing.position> RPAREN
%token <Lexing.position*Lexing.position> COMMA
%token <Lexing.position*Lexing.position> SEMICOLON
%token <Lexing.position*Lexing.position> COLON
%token <Lexing.position*Lexing.position> DOT
%token <Lexing.position*Lexing.position> JING
%token <Lexing.position*Lexing.position> SINGLEASSIGN
%token <Lexing.position*Lexing.position> PATHTO
%token <Lexing.position*Lexing.position> PATHTOSTAR
%token <Lexing.position*Lexing.position> QUESTION_MARK_COLON
/*
      | "+"                                          { ADD                      }
      | "-"                                          { SUB                      }
      | "*"                                          { MUL                      }
      | "/"                                          { DIV                      }
      | "%"                                          { MOD                      }
      | ">"                                          { GT                       }
      | ">="                                         { GE                       }
      | "<"                                          { LT                       }
      | "<="                                         { LE                       }
      | "!"                                          { LOGIC_NEG                }
      | "&&"                                         { LOGIC_AND                }
      | "||"                                         { LOGIC_OR                 }
      | "=="                                         { LOGIC_EQU                }
      | "!="                                         { LOGIC_INE                }
      | "==="                                        { CASE_EQU                 }
      | "!=="                                        { CASE_INE                 }
      | "~"                                          { BIT_NEG                  }
      | "&"                                          { BIT_AND                  }
      | "|"                                          { BIT_OR                   }
      | "^"                                          { BIT_XOR                  }
      | "^~"                                         { BIT_EQU                  }
      | "~^"                                         { BIT_EQU                  }
      | "~&"                                         { RED_NAND                 }
      | "~|"                                         { RED_NOR                  }
      | "<<"                                         { LEFT_SHIFT               }
      | ">>"                                         { RIGHT_SHIFT              }
      | "?"                                          { QUESTION_MARK            }

*/

%token  <Lexing.position*Lexing.position> LOGIC_OR
%token  <Lexing.position*Lexing.position> LOGIC_AND
%token  <Lexing.position*Lexing.position> BIT_OR
%token  <Lexing.position*Lexing.position> BIT_XOR BIT_EQU
%token  <Lexing.position*Lexing.position> BIT_AND
%token  <Lexing.position*Lexing.position> LOGIC_EQU LOGIC_INE CASE_EQU CASE_INE
%token  <Lexing.position*Lexing.position> GT GE LT LE
%token  <Lexing.position*Lexing.position> LEFT_SHIFT RIGHT_SHIFT
%token  <Lexing.position*Lexing.position> ADD SUB
%token  <Lexing.position*Lexing.position> MUL DIV MOD
%token  <Lexing.position*Lexing.position> LOGIC_NEG BIT_NEG RED_NAND RED_NOR
%token  <Lexing.position*Lexing.position> QUESTION_MARK

%right QUESTION_MARK
%left  LOGIC_OR
%left  LOGIC_AND
%left  BIT_OR
%left  BIT_XOR BIT_EQU
%left  BIT_AND
%left  LOGIC_EQU LOGIC_INE CASE_EQU CASE_INE
%left  GT GE LT LE
%left  LEFT_SHIFT RIGHT_SHIFT
%left  ADD SUB
%left  MUL DIV MOD
/*unary only operator*/
%left  LOGIC_NEG BIT_NEG RED_NAND RED_NOR
/*unary operator symbol that maybe used as binary operator*/
%left  UADD USUB UAND UOR UXOR UEQU


%token CAPITAL_E
%token LITTLE_E
%token <Lexing.position*Lexing.position> DOLLOR
%token <string> NETTYPE
%token <string> GATETYPE
%token <string> STRENGTH0
%token <string> STRENGTH1
%token <string> STRING
/*A.1*/
/*Source Text*/
%start source_text
%type <Typedef.module_def list> source_text


%%
source_text	:
		description_list EOF
			{
				get_endpos $2;
				$1
			}
;

description_list:
		description	{ $1::[] }
		| description source_text		{  $1::$2 }
;

description	:
		module_def	{
					$1
				}
/*		| primitive   {0}*/
/* not supported */
;

module_def			:
		KEY_MODULE IDENTIFIER list_of_ports_opt SEMICOLON module_item_optlist KEY_ENDMODULE
			{
				(*print_endline $2;*)
				get_endpos $6;
				T_module_def($2,$3,$5)
			}
;

list_of_ports_opt :
			{[]}
		|	list_of_ports	{$1}
;

list_of_ports	:
		LPAREN  port comma_port_optlist RPAREN
			{
				get_endpos $4;
				$2::$3
			}
;

comma_port_optlist:
			{[]}
		| comma_port comma_port_optlist  {$1::$2}
;

comma_port	:
		COMMA port
			{
				$2
			}
;

port :
	port_expression_opt			{$1}
/*	| DOT IDENTIFIER LPAREN port_expression_opt LPAREN	{0}*/
/*this seems incorrect*/
;

port_expression_opt	:
						{[]}
	| port_expression			{$1}
;

port_expression:
	port_reference				{[$1]}
	| LBRACE port_reference	comma_port_reference_optlist	RBRACE
		{
			get_endpos $4;
			$2::$3
		}
;

comma_port_reference_optlist :
						{[]}
	| comma_port_reference comma_port_reference_optlist {$1::$2}
;

comma_port_reference :
	COMMA port_reference			{$2}
;

port_reference :
	IDENTIFIER				{$1}
;

module_item_optlist	:
					{[]}
		|	module_item module_item_optlist	{$1::$2}
;

module_item	:
		parameter_declaration   {$1}
		| input_declaration   {$1}
		| output_declaration   {$1}
		| inout_declaration   {$1}
		| net_declaration   {$1}
		| reg_declaration  {$1}
		| time_declaration  {T_time_declaration($1)}
		| integer_declaration  {T_integer_declaration($1)}
		| real_declaration  {$1}
		| event_declaration  {$1}
		| gate_declaration  {$1}
/*because UDP and module cannot be distinguished,so we select to make them into same*/
/*		| UDP_instantiation  {$1}*/
		| module_instantiation  {$1}
		| parameter_override  {$1}
		| continuous_assign  {T_continuous_assign($1)}
		| specify_block  {$1}
		| initial_statement  {T_initial_statement($1)}
		| always_statement  {T_always_statement($1)}
		| task  {$1}
		| function_avoid_amb  {$1}
;

function_avoid_amb:
	KEY_FUNCTION range_or_type_opt IDENTIFIER SEMICOLON tf_declaration_list statement KEY_ENDFUNCTION
			{
				get_endpos $7;
				T_function_avoid_amb($2,$3,$5,$6)
			}
;

tf_declaration_list:
	tf_declaration   {[$1]}
	| tf_declaration tf_declaration_list  {$1::$2}
;

range_or_type_opt:
				{T_range_or_type_NOSPEC}
	| range_or_type		{$1}
;

range_or_type:
	range	{T_range_or_type_range($1)}
	| KEY_INTEGER   {T_range_or_type_INTEGER}
	| KEY_REAL   {T_range_or_type_REAL}
;

task:
	KEY_TASK IDENTIFIER SEMICOLON tf_declaration_optlist statement_or_null KEY_ENDTASK
		{
			get_endpos $6;
			T_task($2,$4,$5)
		}
;

tf_declaration_optlist:
			{[]}
	| tf_declaration tf_declaration_optlist {$1::$2}
;
/*it will has model_item type*/
tf_declaration:
	parameter_declaration	{$1}
	| input_declaration	{$1}
	| output_declaration	{$1}
	| inout_declaration	{$1}
	| reg_declaration	{$1}
	| time_declaration	{T_time_declaration($1)}
	| integer_declaration	{T_integer_declaration($1)}
	| real_declaration	{$1}
	| event_declaration	{$1}
;

initial_statement:
	KEY_INITIAL statement    {$2}
;

specify_block:
	KEY_SPECIFY specify_item_optlist KEY_ENDSPECIFY
		{
			get_endpos $3;
			T_specify_block($2)
		}
;

specify_item_optlist:
			{[]}
	| specify_item specify_item_optlist {$1::$2}
;

specify_item:
	specparam_declaration	{0}
	| path_declaration 	{0}
	| level_sensitive_path_declaration	{0}
	| edge_sensitive_path_declaration	{0}
	| system_timing_check	{0}
	| sdpd {0}
;

sdpd:
	KEY_IF LPAREN expression RPAREN path_description SINGLEASSIGN path_delay_value SEMICOLON
		{
			get_endpos $8;
			0
		}
;


system_timing_check:
	DOLLOR_SETUP LPAREN timing_check_event COMMA timing_check_event COMMA timing_check_limit comma_notify_register_opt RPAREN  SEMICOLON
	{
		get_endpos $10;
		0
	}
	| DOLLOR_HOLD LPAREN timing_check_event COMMA timing_check_event COMMA timing_check_limit comma_notify_register_opt RPAREN  SEMICOLON
	{
		get_endpos $10;
		0
	}
	| DOLLOR_PERIOD LPAREN controlled_timing_check_event COMMA timing_check_limit comma_notify_register_opt RPAREN  SEMICOLON
	{
		get_endpos $8;
		0
	}
	| DOLLOR_WIDTH LPAREN controlled_timing_check_event COMMA timing_check_limit comma_constant_expression_comma_notify_register_opt RPAREN  SEMICOLON
	{
		get_endpos $8;
		0
	}
	| DOLLOR_SKEW LPAREN timing_check_event COMMA timing_check_event COMMA timing_check_limit comma_notify_register_opt RPAREN  SEMICOLON
	{
		get_endpos $10;
		0
	}
	| DOLLOR_RECOVERY LPAREN controlled_timing_check_event COMMA timing_check_event COMMA timing_check_limit comma_notify_register_opt RPAREN  SEMICOLON
	{
		get_endpos $10;
		0
	}
	| DOLLOR_SETUPHOLD LPAREN timing_check_event COMMA timing_check_event COMMA timing_check_limit COMMA timing_check_limit comma_notify_register_opt RPAREN  SEMICOLON
	{
		get_endpos $12;
		0
	}
;

comma_constant_expression_comma_notify_register_opt:
			{0}
	| comma_constant_expression_comma_notify_register {0}
;

comma_constant_expression_comma_notify_register:
	COMMA constant_expression COMMA notify_register  {0}
;

notify_register:
	identifier  {0}
;

controlled_timing_check_event:
	timing_check_event_control specify_terminal_descriptor and3_timing_check_condition_opt  {0}
;

and3_timing_check_condition_opt:
			{0}
	| and3_timing_check_condition  {0}
;

and3_timing_check_condition:
	AND3 timing_check_condition  {0}
;

timing_check_condition:
	expression  {0}
;

comma_notify_register_opt:
			{0}
	| comma_notify_register {0}
;

comma_notify_register:
	COMMA notify_register  {0}
;

timing_check_limit:
	expression  {0}
;

timing_check_event:
	timing_check_event_control_opt specify_terminal_descriptor and3_timing_check_condition_opt   {0}
;

timing_check_event_control_opt:
			{0}
	| timing_check_event_control {0}
;

timing_check_event_control:
	KEY_POSEDGE
	{
		get_endpos $1;
		0
	}
	| KEY_NEGEDGE
	{
		get_endpos $1;
		0
	}
	| edge_control_specifier {0}
;

edge_control_specifier:
	KEY_EDGE LBRACKET edge_descriptor comma_edge_descriptor_optlist RBRACKET
	{
		get_endpos $5;
		0
	}
;

edge_descriptor:
		{print_string "no supported edge_descriptor";0}
;

comma_edge_descriptor_optlist:
			{0}
	| comma_edge_descriptor comma_edge_descriptor_optlist {0}
;

comma_edge_descriptor:
	COMMA edge_descriptor  {0}
;



edge_sensitive_path_declaration:
	KEY_IF_LPAREN_expression_RPAREN_opt LPAREN edge_identifier_opt specify_terminal_descriptor PATHTO LPAREN specify_terminal_descriptor polarity_operator QUESTION_MARK_COLON data_source_expression RPAREN RPAREN SINGLEASSIGN path_delay_value SEMICOLON
	{
		get_endpos $15;
		0
	}
	| KEY_IF_LPAREN_expression_RPAREN_opt LPAREN edge_identifier_opt specify_terminal_descriptor PATHTOSTAR LPAREN list_of_path_input_outputs polarity_operator QUESTION_MARK_COLON data_source_expression RPAREN RPAREN  SINGLEASSIGN path_delay_value SEMICOLON
	{
		get_endpos $15;
		0
	}
;

list_of_path_input_outputs:
	specify_terminal_descriptor comma_specify_terminal_descriptor_optlist {0}
;


comma_specify_terminal_descriptor_optlist:
			{0}
	| comma_specify_terminal_descriptor comma_specify_terminal_descriptor_optlist {0}
;

comma_specify_terminal_descriptor:
	COMMA specify_terminal_descriptor {0}
;

data_source_expression:
	expression  {0}
;

edge_identifier_opt:
		{0}
	| edge_identifier  {0}
;

edge_identifier:
	KEY_POSEDGE
	{
		get_endpos $1;
		0
	}
	| KEY_NEGEDGE
	{
		get_endpos $1;
		0
	}
;

polarity_operator:
	ADD
	{
		get_endpos $1;
		0
	}
	| SUB
	{
		get_endpos $1;
		0
	}
;

KEY_IF_LPAREN_expression_RPAREN_opt:
			{0}
	| KEY_IF LPAREN expression RPAREN
	{
		get_endpos $4;
		0
	}
;

level_sensitive_path_declaration:
	KEY_IF LPAREN conditional_port_expression RPAREN LPAREN specify_terminal_descriptor polarity_operator_opt PATHTO specify_terminal_descriptor RPAREN SINGLEASSIGN path_delay_value  SEMICOLON
	{
		get_endpos $13;
		0
	}
	| KEY_IF LPAREN conditional_port_expression RPAREN LPAREN list_of_path_input_outputs polarity_operator_opt PATHTOSTAR list_of_path_input_outputs RPAREN SINGLEASSIGN path_delay_value SEMICOLON
	{
		get_endpos $13;
		0
	}
;


specify_terminal_descriptor:
	IDENTIFIER	{0}
	| IDENTIFIER LBRACKET expression RBRACKET
	{
		get_endpos $4;
		0
	}
	| IDENTIFIER LBRACKET expression COLON expression RBRACKET
	{
		get_endpos $6;
		0
	}
;

polarity_operator_opt:
		{0}
	| polarity_operator {0}
;

conditional_port_expression:
	expression  {0}
;

path_declaration:
	path_description SINGLEASSIGN path_delay_value  SEMICOLON
	{
		get_endpos $4;
		0
	}
;

path_description:
	LPAREN specify_terminal_descriptor PATHTO specify_terminal_descriptor RPAREN
	{
		get_endpos $5;
		0
	}
	| LPAREN list_of_path_input_outputs PATHTOSTAR list_of_path_input_outputs RPAREN
	{
		get_endpos $5;
		0
	}
;

path_delay_value:
	path_delay_expression   {0}
	| LPAREN path_delay_expression COMMA path_delay_expression RPAREN
	{
		get_endpos $5;
		0
	}
	| LPAREN path_delay_expression COMMA path_delay_expression COMMA path_delay_expression RPAREN
	{
		get_endpos $7;
		0
	}
	| LPAREN path_delay_expression COMMA path_delay_expression COMMA path_delay_expression COMMA path_delay_expression COMMA path_delay_expression COMMA path_delay_expression RPAREN
	{
		get_endpos $13;
		0
	}
;

path_delay_expression:
	mintypmax_expression {0}
;

specparam_declaration:
	KEY_SPECPARAM list_of_param_assignments  SEMICOLON
	{
		get_endpos $3;
		0
	}
;


parameter_override:
	KEY_DEFPARAM list_of_param_assignments SEMICOLON
	{
		get_endpos $3;
		T_parameter_override($2)
	}
;
/*
UDP_instantiation:
	IDENTIFIER drive_strength_opt delay_opt UDP_instance comma_UDP_instance_optlist  SEMICOLON   {0}
;

comma_UDP_instance_optlist:
			{0}
	| comma_UDP_instance comma_UDP_instance_optlist   {0}
;

comma_UDP_instance:
	COMMA UDP_instance   {0}
;

UDP_instance:
	name_of_UDP_instance_opt LPAREN terminal comma_terminal_optlist RPAREN {0}

name_of_UDP_instance_opt:
			{0}
	| IDENTIFIER   {0}
;
*/
gate_declaration:
	GATETYPE drive_strength_opt delay_opt gate_instance comma_gate_instance_optlist  SEMICOLON
	{
		get_endpos $6;
		T_gate_declaration($1,$2,$3,($4::$5))
	}
;

comma_gate_instance_optlist:
				{[]}
	| comma_gate_instance comma_gate_instance_optlist  {$1::$2}
;

comma_gate_instance:
	COMMA gate_instance   {$2}
;

gate_instance:
	name_of_gate_instance_opt LPAREN terminal comma_terminal_optlist RPAREN
		{
			get_endpos $5;
			T_gate_instance($1,($3::$4))
		}
;

name_of_gate_instance_opt:
				{""}
	| IDENTIFIER {$1}
;


comma_terminal_optlist:
				{[]}
	| comma_terminal comma_terminal_optlist {$1::$2}
;

comma_terminal:
	COMMA terminal   {$2}
;

terminal:
       expression     {$1}
/*       | IDENTIFIER    {0}*/
;

drive_strength_opt:
			{T_drive_strength_NOSPEC}
	| drive_strength	{$1}
;

integer_declaration:
	KEY_INTEGER list_of_register_variables SEMICOLON
		{
			get_endpos $3;
			$2
		}
;

time_declaration:
	KEY_TIME list_of_register_variables SEMICOLON
		{
			get_endpos $3;
			$2
		}
;

always_statement:
	KEY_ALWAYS statement   {$2}
;

statement:
	blocking_assignment SEMICOLON
		{
			get_endpos $2;
			T_blocking_assignment($1)
		}
	| non_blocking_assignment SEMICOLON
		{
			get_endpos $2;
			T_non_blocking_assignment($1)
		}
	| KEY_IF LPAREN expression RPAREN statement_or_null
		{
			get_endpos $4;
			T_if_statement($3,$5)
		}
	| KEY_IF LPAREN expression RPAREN statement_or_null KEY_ELSE statement_or_null
		{
			get_endpos $6;
			T_if_else_statement($3,$5,$7)
		}
	| KEY_CASE LPAREN expression RPAREN case_item_list KEY_ENDCASE
		{
			get_endpos $6;
			T_case_statement($3,$5)
		}
	| KEY_CASEZ LPAREN expression RPAREN case_item_list KEY_ENDCASE
		{
			get_endpos $6;
			T_casez_statement($3,$5)
		}
	| KEY_CASEX LPAREN expression RPAREN case_item_list KEY_ENDCASE
		{
			get_endpos $6;
			T_casex_statement($3,$5)
		}
	| KEY_FOREVER statement
		{
			get_endpos $1;
			T_forever_statement($2)
		}
	| KEY_REPEAT LPAREN expression RPAREN statement
		{
			get_endpos $4;
			T_repeat_statement($3,$5)
		}
	| KEY_WHILE LPAREN expression RPAREN statement
		{
			get_endpos $4;
			T_while_statement($3,$5)
		}
	| KEY_FOR LPAREN assignment SEMICOLON expression SEMICOLON assignment RPAREN statement
		{
			get_endpos $8;
			T_for_statement($3,$5,$7,$9)
		}
	| delay_control statement_or_null   {T_delay_statement($1,$2)}
	| event_control statement_or_null   {T_event_statement($1,$2)}
	| KEY_WAIT LPAREN expression RPAREN statement_or_null
		{
			get_endpos $4;
			T_wait_statement($3,$5)
		}
	| LEADTO name_of_event SEMICOLON
		{
			get_endpos $3;
			T_leadto_event($2)
		}
	| KEY_BEGIN statement_optlist KEY_END
		{
			get_endpos $3;
			T_seq_block("",[],$2)
		}   /*seq_block*/
        | KEY_BEGIN COLON IDENTIFIER block_declaration_optlist statement_optlist KEY_END
		{
			get_endpos $6;
			T_seq_block($3,$4,$5)
		} /*seq_block*/
	| KEY_FORK statement_optlist KEY_JOIN
		{
			get_endpos $3;
			T_par_block("",[],$2)
		}
	| KEY_FORK COLON IDENTIFIER block_declaration_optlist statement_optlist KEY_JOIN
		{
			get_endpos $6;
			T_par_block($3,$4,$5)
		}
	| IDENTIFIER SEMICOLON
		{
			get_endpos $2;
			T_task_enable($1,[])
		}
	| IDENTIFIER LPAREN expression comma_expression_optlist RPAREN SEMICOLON
		{
			get_endpos $6;
			T_task_enable($1,($3::$4))
		}
	| name_of_system_task SEMICOLON
		{
			get_endpos $2;
			T_system_task_enable($1,[])
		}
	| name_of_system_task LPAREN expression comma_expression_optlist RPAREN SEMICOLON
		{
			get_endpos $6;
			T_system_task_enable($1,($3::$4))
		}
	| KEY_DISABLE IDENTIFIER SEMICOLON
		{
			get_endpos $3;
			T_disable_statement($2)
		}  /*this maybe task or block*/
	| KEY_FORCE assignment SEMICOLON
		{
			get_endpos $3;
			T_force_statement($2)
		}
	| KEY_RELEASE lvalue SEMICOLON
		{
			get_endpos $3;
			T_release_statement($2)
		}
;

statement_optlist:
				{[]}
	| statement statement_optlist {$1::$2}
;

block_declaration_optlist:
				{[]}
	| block_declaration  block_declaration_optlist   {$1::$2}
;
/*it will has model_item type*/
block_declaration:
	parameter_declaration {$1}
	| reg_declaration {$1}
	| integer_declaration {T_integer_declaration($1)}
	| real_declaration {$1}
	| time_declaration {T_time_declaration($1)}
	| event_declaration {$1}
;

event_declaration:
	KEY_EVENT name_of_event comma_name_of_event_optlist SEMICOLON
		{
			get_endpos $4;
			T_event_declaration($2::$3)
		}
;

comma_name_of_event_optlist:
				{[]}
	| comma_name_of_event comma_name_of_event_optlist {$1::$2}
;

comma_name_of_event:
	COMMA name_of_event   {$2}
;

name_of_event:
	IDENTIFIER  {$1}
;


real_declaration:
	KEY_REAL list_of_variables SEMICOLON
		{
			get_endpos $3;
			T_real_declaration($2)
		}
;


parameter_declaration:
	KEY_PARAMETER list_of_param_assignments SEMICOLON
		{
			get_endpos $3;
			T_parameter_declaration($2)
		}
;

list_of_param_assignments:
	param_assignment comma_param_assignment_optlist  {$1::$2}
;

comma_param_assignment_optlist:
			{[]}
	| comma_param_assignment comma_param_assignment_optlist  {$1::$2}
;

comma_param_assignment:
	COMMA param_assignment  {$2}
;

param_assignment:
	identifier SINGLEASSIGN constant_expression
		{
			get_endpos $2;
			T_param_assignment($1,$3)
		}
;

name_of_system_task:
	DOLLOR_SYSTEM_IDENTIFIER {$1}
;

event_control:
	AT identifier
		{
			get_endpos $1;
			T_event_control_id($2)
		}
	| AT LPAREN event_expression_list RPAREN
		{
			get_endpos $4;
			T_event_control_evexp($3)
		}
;

event_expression_list:
	event_expression or_event_expression_optlist   {$1::$2}
	| event_expression comma_event_expression_optlist   {$1::$2}
;

or_event_expression_optlist:
			{[]}
	| KEY_OR event_expression or_event_expression_optlist  {$2::$3}
;

comma_event_expression_optlist:
			{[]}
	| COMMA event_expression comma_event_expression_optlist {$2::$3}
;

event_expression:
	expression   {T_event_expression($1)}
	| KEY_POSEDGE SCALAR_EVENT_EXPRESSION   {T_event_expression_posedge($2)}
	| KEY_NEGEDGE SCALAR_EVENT_EXPRESSION   {T_event_expression_negedge($2)}
;


/*SCALAR_EVENT_EXPRESSION is an expression that resolves to a one bit value*/
SCALAR_EVENT_EXPRESSION:
	expression   {$1}
;

delay_control:
	JING expression   {T_delay_control($2)}
/*        | JING LPAREN mintypmax_expression RPAREN   {0}
this has been included in primary*/
;

case_item_list:
	case_item   {[$1]}
	| case_item case_item_list   {$1::$2}
;

case_item:
	expression comma_expression_optlist COLON statement_or_null   {T_case_item_normal($1::$2,$4)}
	| KEY_DEFAULT COLON statement_or_null   {T_case_item_default($3)}
	| KEY_DEFAULT statement_or_null   {T_case_item_default($2)}
;

blocking_assignment:
	lvalue SINGLEASSIGN expression   {T_blocking_assignment_direct($1,$3)}
	| lvalue SINGLEASSIGN delay_control expression    {T_blocking_assignment_delay($1,$4,$3)}
	| lvalue SINGLEASSIGN event_control expression   {T_blocking_assignment_event($1,$4,$3)}
;

non_blocking_assignment:
	lvalue LE expression   {T_non_blocking_assignment_direct($1,$3)}
	| lvalue LE delay_control expression   {T_non_blocking_assignment_delay($1,$4,$3)}
	| lvalue LE event_control expression    {T_non_blocking_assignment_event($1,$4,$3)}
;
statement_or_null:
	statement	{$1}
	| SEMICOLON
		{
			get_endpos $1;
			T_statement_NOSPEC
		}
;

reg_declaration:
	KEY_REG range_opt list_of_register_variables SEMICOLON
		{
			get_endpos $4;
			T_reg_declaration($2,$3)
		}
;


list_of_register_variables:
	register_variable comma_register_variable_optlist  {$1::$2}
;

comma_register_variable_optlist:
			{[]}
	| comma_register_variable comma_register_variable_optlist  {$1::$2}
;

comma_register_variable:
	COMMA register_variable   {$2}
;

register_variable:
	IDENTIFIER	{T_register_variables_ID($1)}
	| IDENTIFIER LBRACKET constant_expression COLON constant_expression RBRACKET
		{
			get_endpos $6;
			T_register_variables_IDrange($1,$3,$5)
		}
;
/*in UDP inst, # means delay*/
/*but in module inst, # means parameter*/
module_instantiation:
	IDENTIFIER drive_strength_opt parameter_value_assignment_opt module_instance comma_module_instance_optlist SEMICOLON
		{
			get_endpos $6;
			T_module_instantiation($1,$2,$3,$4::$5)
		}
;

comma_module_instance_optlist:
			{[]}
	| comma_module_instance comma_module_instance_optlist   {$1::$2}
;

comma_module_instance:
	COMMA module_instance    {$2}
;

module_instance:
	IDENTIFIER LPAREN list_of_module_connections RPAREN
		{
			get_endpos $4;
			T_module_instance($1,$3)
		}
;


list_of_module_connections:
	module_port_connection comma_module_port_connection_optlist	{T_list_of_module_connections_unnamed($1::$2)}
	| named_port_connection comma_named_port_connection_optlist     {T_list_of_module_connections_named($1::$2)}
;

comma_named_port_connection_optlist:
				{[]}
	| comma_named_port_connection comma_named_port_connection_optlist  {$1::$2}
;

comma_named_port_connection:
	COMMA named_port_connection   {$2}
;

named_port_connection:
	DOT IDENTIFIER LPAREN  RPAREN
		{
			get_endpos $4;
			T_named_port_connection($2,T_expression_NOSPEC(0))
		}
	| DOT IDENTIFIER LPAREN expression RPAREN
		{
			get_endpos $5;
			T_named_port_connection($2,$4)
		}
;

comma_module_port_connection_optlist:
				{[]}
	| comma_module_port_connection  comma_module_port_connection_optlist {$1::$2}
;

comma_module_port_connection:
	COMMA module_port_connection    {$2}
;

module_port_connection:
	                  {T_expression_NOSPEC(0)}
	| expression	{$1}
;

parameter_value_assignment_opt:
	                       {[]}
	| parameter_value_assignment  {$1}
;

parameter_value_assignment:
	JING LPAREN expression comma_expression_optlist RPAREN
		{
			get_endpos $5;
			$3::$4
		}
;


continuous_assign:
	 KEY_ASSIGN delay_opt list_of_assignments SEMICOLON
	 	{
			get_endpos $4;
			T_continuous_assign_assign(T_drive_strength_NOSPEC,$2,$3)
		}
	| KEY_ASSIGN drive_strength delay_opt list_of_assignments SEMICOLON
		{
			get_endpos $5;
			T_continuous_assign_assign($2,$3,$4)
		}
	| NETTYPE expandrange_opt delay_opt list_of_assignments SEMICOLON
		{
			get_endpos $5;
			T_continuous_assign_net($1,T_drive_strength_NOSPEC,$2,$3,$4)
		}
	| NETTYPE drive_strength expandrange_opt delay_opt list_of_assignments SEMICOLON
		{
			get_endpos $6;
			T_continuous_assign_net($1,$2,$3,$4,$5)
		}
;

list_of_assignments:
	assignment comma_assignment_optlist   {$1::$2}
;

comma_assignment_optlist:
			{[]}
	| comma_assignment comma_assignment_optlist  {$1::$2}
;

comma_assignment:
	COMMA assignment  {$2}
;

assignment:
	lvalue SINGLEASSIGN expression  {T_assignment($1,$3)}
;

lvalue :
	identifier			{T_lvalue_id($1)}
	| identifier LBRACKET expression RBRACKET
		{
			get_endpos $4;
			T_lvalue_arrbit($1,$3)
		}
	| identifier LBRACKET constant_expression COLON constant_expression RBRACKET
		{
			get_endpos $6;
			T_lvalue_arrrange($1,$3,$5)
		}
	| concatenation		{T_lvalue_concat($1)}
;

expression:
	primary			{T_primary($1)}
	| ADD primary %prec UADD   {T_add1($2)}			/* +a */
	| SUB primary %prec USUB  {T_sub1($2)}			/* -a */
	| LOGIC_NEG primary   {T_logicneg($2)} 			/* !a */
	| BIT_NEG primary   {T_bitneg($2)}				/* ~a */
	| BIT_AND primary %prec  UAND  {T_reduce_and($2)}		/* &a reduction */
	| RED_NAND primary   {T_reduce_nand($2)}			/* ~&a reducation*/
	| BIT_OR primary %prec UOR   {T_reduce_or($2)}		/* |a */
	| RED_NOR primary   {T_reduce_nor($2)}				/* ~|a */
	| BIT_XOR primary %prec UXOR   {T_reduce_xor($2)}		/* ^a */
	| BIT_EQU primary %prec UEQU  {T_reduce_xnor($2)}		/* ^~a or ~^a */
	| expression ADD expression  {T_add2($1,$3)}
	| expression SUB expression  {T_sub2($1,$3)}
	| expression MUL expression  {T_mul2($1,$3)}
	| expression DIV expression  {T_div($1,$3)}
	| expression MOD expression  {T_mod($1,$3)}
	| expression LOGIC_EQU expression  {T_logic_equ($1,$3)}
	| expression LOGIC_INE expression  {T_logic_ine($1,$3)}
	| expression CASE_EQU expression  {T_case_equ($1,$3)}
	| expression CASE_INE expression  {T_case_ine($1,$3)}
	| expression LOGIC_AND expression  {T_logic_and2($1,$3)}
	| expression LOGIC_OR expression  {T_logic_or2($1,$3)}
	| expression LT expression  {T_lt($1,$3)}
	| expression LE expression  {T_le($1,$3)}
	| expression GT expression  {T_gt($1,$3)}
	| expression GE expression  {T_ge($1,$3)}
	| expression BIT_AND expression  {T_bit_and2($1,$3)}
	| expression BIT_OR expression  {T_bit_or2($1,$3)}
	| expression BIT_XOR expression  {T_bit_xor2($1,$3)}
	| expression BIT_EQU expression  {T_bit_equ($1,$3)}
	| expression LEFT_SHIFT expression  {T_leftshift($1,$3)}
	| expression RIGHT_SHIFT expression  {T_rightshift($1,$3)}
	| expression QUESTION_MARK expression  COLON expression  {T_selection($1,$3,$5)}
	| STRING   {T_string($1)}
;

primary:
	number   {T_primary_num($1)}
	| identifier		{T_primary_id($1)}
	| identifier LBRACKET expression RBRACKET
		{
			get_endpos $4;
			T_primary_arrbit($1,$3)
		}
	| identifier LBRACKET constant_expression COLON constant_expression RBRACKET
		{
			get_endpos $6;
			T_primary_arrrange($1,$3,$5)
		}
	| concatenation          {T_primary_concat($1)}
	| multiple_concatenation  {$1}
	| identifier LPAREN expression comma_expression_optlist RPAREN
		{
			get_endpos $5;
			T_primary_funcall($1,$3::$4)
		}
	| name_of_system_function LPAREN expression comma_expression_optlist RPAREN
		{
			get_endpos $5;
			T_primary_sysfuncall($1,$3::$4)
		}
	| name_of_system_function      {T_primary_sysfuncall($1,[])}
	| LPAREN mintypmax_expression RPAREN
		{
			get_endpos $3;
			T_primary_minmaxexp($2)
		}
;

multiple_concatenation:
	LBRACE expression LBRACE expression comma_expression_optlist RBRACE RBRACE
		{
			get_endpos $7;
			T_primary_multiconcat($2,$4::$5)
		}
;

concatenation:
	LBRACE expression comma_expression_optlist RBRACE
		{
			get_endpos $4;
			$2::$3
		}
;

comma_expression_optlist:
				{[]}
	| comma_expression comma_expression_optlist   {$1::$2}
;

mintypmax_expression:
	expression	{T_mintypmax_expression_1($1)}
	| expression COLON expression COLON expression   {T_mintypmax_expression_3($1,$3,$5)}
;


comma_expression:
	COMMA expression   {$2}
;

number:
	UNSIGNED_NUMBER				{T_number_unsign(int_of_string (delunderscore $1))}
	| BASE_NUMBER  {string2base_number $1}
	| UNSIGNED_NUMBER DOT UNSIGNED_NUMBER     {T_number_float(float_of_string( String.concat "" [delunderscore $1;".";delunderscore $3]))}
	| FLOAT_NUMBER	{T_number_float(float_of_string (delunderscore $1))}
;


identifier:
	IDENTIFIER dot_IDENTIFIER_optlist		{$1::$2}
;

dot_IDENTIFIER_optlist:
				{[]}
	| dot_IDENTIFIER dot_IDENTIFIER_optlist {$1::$2}
;

dot_IDENTIFIER:
	DOT IDENTIFIER			{$2}
;

drive_strength:
	LPAREN STRENGTH0 COMMA STRENGTH1 RPAREN
		{
			get_endpos $5;
			T_drive_strength($2,$4)
		}
	| LPAREN STRENGTH1 COMMA STRENGTH0 RPAREN
		{
			get_endpos $5;
			T_drive_strength($2,$4)
		}
;



input_declaration:
	KEY_INPUT range_opt list_of_variables SEMICOLON
		{
			get_endpos $4;
			T_input_declaration($2,$3)
		}
;
output_declaration:
	KEY_OUTPUT range_opt list_of_variables SEMICOLON
		{
			get_endpos $4;
			T_output_declaration($2,$3)
		}
;
inout_declaration:
	KEY_INOUT range_opt list_of_variables SEMICOLON
		{
			get_endpos $4;
			T_inout_declaration($2,$3)
		}
;
net_declaration:
	NETTYPE expandrange_opt delay_opt list_of_variables SEMICOLON
		{
			get_endpos $5;
			T_net_declaration($1,T_charge_strength_NOSPEC,$2,$3,$4)
		}
	| NETTYPE charge_strength expandrange_opt delay_opt list_of_variables SEMICOLON
		{
			get_endpos $6;
			T_net_declaration($1,$2,$3,$4,$5)
		}
;

delay_opt:
			{T_delay_NOSPEC}
	| delay		{$1}
;

delay:
	 JING number	{T_delay_number($2)}
	 | JING identifier {T_delay_id($2)}
	 | JING LPAREN mintypmax_expression RPAREN
	 	{
			get_endpos $4;
			T_delay_minmax1($3)
		}
	 | JING LPAREN mintypmax_expression COMMA mintypmax_expression COMMA mintypmax_expression RPAREN
	 	{
			get_endpos $8;
			T_delay_minmax3($3,$5,$7)
		}
;

expandrange_opt:
			{T_expandrange_NOSPEC}
	| expandrange    {$1}
;

expandrange:
	range		{T_expandrange_range($1)}
	| KEY_SCALARED  range {T_expandrange_scalared($2)}
	| KEY_VECTORED  range {T_expandrange_vectored($2)}
;

charge_strength:
	LPAREN KEY_SMALL RPAREN
		{
			get_endpos $3;
			T_charge_strength_SMALL
		}
	| LPAREN KEY_MEDIUM RPAREN
		{
			get_endpos $3;
			T_charge_strength_MEDIUM
		}
	| LPAREN KEY_LARGE RPAREN
		{
			get_endpos $3;
			T_charge_strength_LARGE
		}
;

range_opt :
				{T_range_NOSPEC}
	| range {$1}
;

range :
	LBRACKET constant_expression COLON constant_expression RBRACKET
		{
			get_endpos $5;
			T_range($2,$4)
		}
;

constant_expression:
	expression		{$1}
;

list_of_variables:
		IDENTIFIER comma_IDENTIFIER_optlist	{$1::$2}
;

comma_IDENTIFIER_optlist:
				{[]}
	| comma_IDENTIFIER comma_IDENTIFIER_optlist  {$1::$2}
;

comma_IDENTIFIER:
	COMMA IDENTIFIER   {$2}
;

name_of_system_function:
	DOLLOR  IDENTIFIER {(String.concat "" ("$"::[$2]))::[]}
;
/*
dot_UNSIGNED_NUMBER_opt:
				{0}
	| dot_UNSIGNED_NUMBER	{0}
;

dot_UNSIGNED_NUMBER:
	DOT UNSIGNED_NUMBER	{0}
;
*/
