/*
   Parse a peephole optimization file, stored in .kupo files
   Copyright (C) 2001 Justin David Smith, Caltech

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/***  Header  ***/


%{
   open Symbol
   open Moogle_ast_type
   open Moogle_util
   open Moogle_exn
%} /* End header */


/***  Token Definitions  ***/


/* Symbols/Operators */
%token <Moogle_ast_type.pos> TokColon
%token <Moogle_ast_type.pos> TokSemi
%token <Moogle_ast_type.pos> TokComma
%token <Moogle_ast_type.pos> TokDollar
%token <Moogle_ast_type.pos> TokPercent
%token <Moogle_ast_type.pos> TokAt
%token <Moogle_ast_type.pos> TokPlus
%token <Moogle_ast_type.pos> TokMinus
%token <Moogle_ast_type.pos> TokStar
%token <Moogle_ast_type.pos> TokCaret
%token <Moogle_ast_type.pos> TokAmp
%token <Moogle_ast_type.pos> TokPipe
%token <Moogle_ast_type.pos> TokTilde
%token <Moogle_ast_type.pos> TokLParen
%token <Moogle_ast_type.pos> TokRParen
%token <Moogle_ast_type.pos> TokLBrack
%token <Moogle_ast_type.pos> TokRBrack
%token <Moogle_ast_type.pos> TokBang
%token <Moogle_ast_type.pos> TokUnder
%token <Moogle_ast_type.pos> TokArrow
%token <Moogle_ast_type.pos> TokQuery
%token <Moogle_ast_type.pos> TokEqual
%token <Moogle_ast_type.pos> TokNotEqual
%token <Moogle_ast_type.pos> TokLessThan
%token <Moogle_ast_type.pos> TokLessEqual
%token <Moogle_ast_type.pos> TokGreaterThan
%token <Moogle_ast_type.pos> TokGreaterEqual
%token <Moogle_ast_type.pos> TokAmpAmp
%token <Moogle_ast_type.pos> TokPipePipe

/* Blocks */
%token <string        * Moogle_ast_type.pos> TokOCaml
%token <string        * Moogle_ast_type.pos> TokString
%token <Symbol.symbol * Moogle_ast_type.pos> TokIdent
%token <Symbol.symbol * Moogle_ast_type.pos> TokCapIdent
%token <int32         * Moogle_ast_type.pos> TokInt

/* Keywords */
%token <Moogle_ast_type.pos> TokAbs
%token <Moogle_ast_type.pos> TokCopy
%token <Moogle_ast_type.pos> TokDead
%token <Moogle_ast_type.pos> TokEndscope
%token <Moogle_ast_type.pos> TokIgnores
%token <Moogle_ast_type.pos> TokInput
%token <Moogle_ast_type.pos> TokInsts
%token <Moogle_ast_type.pos> TokIrrelevant
%token <Moogle_ast_type.pos> TokIspower2
%token <Moogle_ast_type.pos> TokLog2
%token <Moogle_ast_type.pos> TokMemory
%token <Moogle_ast_type.pos> TokName
%token <Moogle_ast_type.pos> TokNew
%token <Moogle_ast_type.pos> TokOpen
%token <Moogle_ast_type.pos> TokOutput
%token <Moogle_ast_type.pos> TokPow2
%token <Moogle_ast_type.pos> TokPreserves
%token <Moogle_ast_type.pos> TokRegisters
%token <Moogle_ast_type.pos> TokSar
%token <Moogle_ast_type.pos> TokShl
%token <Moogle_ast_type.pos> TokShr
%token <Moogle_ast_type.pos> TokSmall
%token <Moogle_ast_type.pos> TokStack
%token <Moogle_ast_type.pos> TokTrans
%token <Moogle_ast_type.pos> TokTrivial
%token <Moogle_ast_type.pos> TokWhen

/* End-of-file */
%token <Moogle_ast_type.pos> TokEof


/***  Operator Precedences  ***/


%left       TokShl TokShr TokSar shift_prec
%left       TokAmp TokPipe logical_prec
%left       TokPlus TokMinus plusminus_prec
%right      TokPow2 TokLog2 TokTilde TokAbs unary_prec
%left       TokPipePipe TokAmpAmp bool_prec


/***  Start Productions  ***/


%start   prog
%type    <Moogle_ast_type.expr list> prog


%%


/***  Toplevel  Productions  ***/


prog:
   TokEof      { [] }
 | expr prog   { $1 :: $2 }
   ;

expr:
   input_output_statement  { $1 }
 | open_statement          { $1 }
 | name_statement          { $1 }
 | register_statement      { $1 }
 | instruction_statement   { $1 }
 | transform_statement     { $1 }
 | trivial_statement       { $1 }
   ;


/***  Patterns  ***/


/* in_pattern_expr
   out_pattern_expr
   Builds a pattern match expression for instructions.
   An instruction pattern begins with an opcode, then
   a sequence of pattern operands. */
in_pattern_expr:
   opcode_name in_operand_list_opt inst_modifier_list_opt {
      let opcode, pos = $1 in
      let operands = $2 in
      let modifiers = $3 in
      let pos = union_pos_inst_restriction_list pos modifiers in
         InInst (opcode, operands, modifiers, pos)
   }
 | TokUnder inst_modifier_list_opt {
      let modifiers = $2 in
      let pos = union_pos_inst_restriction_list $1 modifiers in
         InInst (underscore, [], modifiers, pos)
   }
 | TokStar inst_modifier_list_opt {
      let modifiers = $2 in
      let pos = union_pos_inst_restriction_list $1 modifiers in
         InStar (modifiers, pos)
   }
   ;

out_pattern_expr:
   opcode_name out_operand_list_opt {
      let opcode, pos = $1 in
      let operands = $2 in
      let pos = union_pos_operand_list pos operands in
         OutInst (opcode, operands, pos)
   }
 | TokCopy out_integer_list {
      let ls, pos = $2 in
      let pos = union_pos $1 pos in
         OutCopy (ls, pos)
   }
 | TokIdent TokInt TokComma arith_expr_list {
      let transform, pos = $1 in
      let i, _ = $2 in
      let operands = $4 in
      let pos = union_pos_arith_list pos operands in
         OutTransform (transform, i, operands, pos);
   }
 | TokIdent TokInt {
      let transform, pos1 = $1 in
      let i, pos2 = $2 in
      let pos = union_pos pos1 pos2 in
         OutTransform (transform, i, [], pos)
   }
 | TokNew TokPercent TokIdent {
      let v, pos = $3 in
      let pos = union_pos $1 pos in
         OutNew (v, TyReg, pos)
   }
   ;

in_pattern_list:
   in_pattern_expr TokSemi in_pattern_list { $1 :: $3 }
 | in_pattern_expr TokSemi { [$1] }
   ;

out_pattern_list:
   out_pattern_expr TokSemi out_pattern_list { $1 :: $3 }
 | out_pattern_expr TokSemi { [$1] }
   ;

out_pattern_list_opt:
   out_pattern_list { $1 }
 | { [] }
   ;

opcode_name:
   TokCapIdent { $1 }
 | TokIdent { $1 }
   ;

out_integer_list:
   TokInt TokComma out_integer_list {
      let i, pos1 = $1 in
      let ls, pos2 = $3 in
      let pos = union_pos pos1 pos2 in
         i :: ls, pos
   }
 | TokInt {
      let i, pos = $1 in
         [i], pos
   }
   ;

/* in_operand
   out_operand
   Builds an operand pattern; which may be wild, a
   match against a number, register, or memory location,
   etc. and may be qualified to exclude certain cases. */
in_operand:
   TokCapIdent {
      let ident, pos = $1 in
         OpConstructor (ident, pos)
   }
 | in_operand_name operand_modifier_list_opt {
      let ident, pos = $1 in
      let modifiers = $2 in
      let pos = union_pos_op_restriction_list pos modifiers in
         Op (ident, modifiers, pos)
   }
 | TokPercent in_operand_name operand_modifier_list_opt {
      let ident, _ = $2 in
      let modifiers = $3 in
      let pos = union_pos_op_restriction_list $1 modifiers in
         OpRegister (ident, modifiers, pos)
   }
 | TokAt in_operand_name operand_modifier_list_opt {
      let ident, pos = $2 in
      let modifiers = $3 in
      let pos = union_pos_op_restriction_list $1 modifiers in
         OpFloatRegister (ident, modifiers, pos)
   }
 | TokAt TokStack in_operand_stack_index {
      let value = $3 in
      let pos = union_pos $1 $2 in
         OpFPStack (value, pos)
   }
 | TokStar TokLParen in_operand_name TokComma in_operand_name TokComma
           value_reg TokComma value_reg TokRParen operand_modifier_list_opt {
      let ident_ptr, _ = $3 in
      let ident_reg, _ = $5 in
      let ident_off, _ = $7 in
      let ident_mul, _ = $9 in
      let modifiers = $11 in
      let pos = union_pos_op_restriction_list $1 modifiers in
         OpMemRegRegOffMul (ident_ptr, ident_reg, ident_off, ident_mul, modifiers, pos)
   }
 | TokStar TokLParen in_operand_name TokComma value_reg TokRParen operand_modifier_list_opt {
      let ident_ptr, _ = $3 in
      let ident_off, _ = $5 in
      let modifiers = $7 in
      let pos = union_pos_op_restriction_list $1 modifiers in
         OpMemRegOff (ident_ptr, ident_off, modifiers, pos)
   }
 | TokStar TokLParen in_operand_name TokRParen operand_modifier_list_opt {
      let ident_ptr, _ = $3 in
      let modifiers = $5 in
      let pos = union_pos_op_restriction_list $1 modifiers in
         OpMemReg (ident_ptr, modifiers, pos)
   }
 | TokStar in_operand_name operand_modifier_list_opt {
      let ident, _ = $2 in
      let modifiers = $3 in
      let modifiers = OpIsMem $1 :: modifiers in
      let pos = union_pos_op_restriction_list $1 modifiers in
         Op (ident, modifiers, pos)
   }
 | TokDollar value_reg {
      let reg, pos = $2 in
      let pos = union_pos $1 pos in
         OpOffset (reg, pos)
   }
   ;

out_operand:
   TokCapIdent {
      let ident, pos = $1 in
         OpConstructor (ident, pos)
   }
 | TokIdent {
      let ident, pos = $1 in
         Op (ident, (), pos)
   }
 | TokPercent TokIdent {
      let ident, pos = $2 in
      let pos = union_pos $1 pos in
         OpRegister (ident, (), pos)
   }
 | TokAt TokIdent {
      let ident, pos = $2 in
      let pos = union_pos $1 pos in
         OpFloatRegister (ident, (), pos)
   }
 | TokAt TokStack out_operand_stack_index {
      let value = $3 in
      let pos = union_pos $1 $2 in
         OpFPStack (value, pos)
   }
 | TokStar TokLParen TokIdent TokComma TokIdent TokComma arith_expr TokComma arith_expr TokRParen {
      let ident_ptr, _ = $3 in
      let ident_reg, _ = $5 in
      let ident_off = $7 in
      let ident_mul = $9 in
      let pos = union_pos $1 $10 in
         OpMemRegRegOffMul (ident_ptr, ident_reg, ident_off, ident_mul, (), pos)
   }
 | TokStar TokLParen TokIdent TokComma arith_expr TokRParen {
      let ident_ptr, _ = $3 in
      let ident_off = $5 in
      let pos = union_pos $1 $6 in
         OpMemRegOff (ident_ptr, ident_off, (), pos)
   }
 | TokStar TokLParen TokIdent TokRParen {
      let ident_ptr, _ = $3 in
      let pos = union_pos $1 $4 in
         OpMemReg (ident_ptr, (), pos)
   }
 | TokDollar arith_expr {
      let reg = $2 in
      let pos = union_pos $1 (pos_of_arith reg) in
         OpInteger (reg, pos)
   }
   ;

in_operand_list:
   in_operand TokComma in_operand_list { $1 :: $3 }
 | in_operand { [$1] }
   ;

out_operand_list:
   out_operand TokComma out_operand_list { $1 :: $3 }
 | out_operand { [$1] }
   ;

in_operand_list_opt:
   in_operand_list { $1 }
 | { [] }
   ;

out_operand_list_opt:
   out_operand_list { $1 }
 | { [] }
   ;

in_operand_name:
   TokUnder { new_symbol_string "wild", $1 }
 | TokIdent { $1 }
   ;

value_reg:
   in_operand_name {
      let ident, pos = $1 in
         ValueReg ident, pos
   }
 | TokInt {
      let value, pos = $1 in
         ValueInt value, pos
   }
   ;

in_operand_stack_index:
   TokLParen value_reg TokRParen {
      let value, _ = $2 in
         value
   }
 | { ValueInt Int32.zero }
   ;

out_operand_stack_index:
   TokLParen arith_expr TokRParen { $2 }
 | { ArithInt (Int32.zero, !Moogle_state.current_pos) }
   ;

/* operand_modifier
   Modifiers which may appear on certain operands to
   restrict the patterns they match (i.e. operand must
   not contain certain registers).  */
operand_modifier:
   TokBang TokIdent {
      let ident, pos = $2 in
      let pos = union_pos $1 pos in
         OpNotContains (ident, pos)
   }
 | TokBang TokStack {
      let pos = union_pos $1 $2 in
         OpNotFPStack pos
   }
 | TokSmall {
      OpSmall $1
   }
   ;

operand_modifier_list:
   operand_modifier operand_modifier_list { $1 :: $2 }
 | operand_modifier { [$1] }
   ;

operand_modifier_list_opt:
   operand_modifier_list { $1 }
 | { [] }
   ;

/* inst_modifier
   Modifiers attached to an entire instructions.  */
inst_modifier:
   TokPreserves preserves_item { [$2] }
 | TokPreserves TokLBrack preserves_list TokRBrack { $3 }
 | TokIgnores ignores_item { [$2] }
 | TokIgnores TokLBrack ignores_list TokRBrack { $3 }
 | TokDead dead_item { [$2] }
 | TokDead TokLBrack dead_list TokRBrack { $3 }
 | TokEndscope TokIdent {
      let ident, pos = $2 in
      let pos = union_pos $1 pos in
         [InstEndscope (ident, pos)]
   }
 | TokIrrelevant { [InstIrrelevant $1] }
 | TokIdent {
      let ident, pos = $1 in
         [InstTag (ident, pos)]
   }
   ;

inst_modifier_list:
   inst_modifier inst_modifier_list { $1 @ $2 }
 | inst_modifier { $1 }
   ;

inst_modifier_list_opt:
   TokColon inst_modifier_list { $2 }
 | { [] }
   ;

preserves_list:
   preserves_item TokSemi preserves_list { $1 :: $3 }
 | preserves_item { [$1] }
   ;

preserves_item:
   TokMemory { InstPreservesMem $1 }
 | TokIdent {
      let ident, pos = $1 in
         InstPreserves (ident, pos)
   }
   ;

ignores_list:
   ignores_item TokSemi ignores_list { $1 :: $3 }
 | ignores_item { [$1] }
   ;

ignores_item:
   TokMemory { InstIgnoresMem $1 }
 | TokIdent {
      let ident, pos = $1 in
         InstIgnores (ident, pos)
   }
   ;

dead_list:
   dead_item TokSemi dead_list { $1 :: $3 }
 | dead_item { [$1] }
   ;

dead_item:
   TokIdent {
      let ident, pos = $1 in
         InstNotLive (ident, pos)
   }
   ;

/***  Input/Output blocks  ***/


/* input_output_statement
   Basic block for peephole optimization patterns. */
input_output_statement:
 | io_input_list io_output_list {
      let inputs, pos1 = $1 in
      let outputs, pos2 = $2 in
      let pos = union_pos pos1 pos2 in
         Rule (inputs, outputs, pos)
   }
   ;

/* io_input
   Defines a single Input expression.  This begins with the
   INPUT keyword, followed by an optional string identifier
   (the name), followed by an input pattern.  The pattern may
   be terminated by a WHEN clause to apply a clause to this
   pattern.  At least one of these expressions must be present
   in any InputOutput.  */
io_input:
   TokInput io_name TokColon in_pattern_list TokWhen clause_expr {
      let name = $2 in
      let insts = $4 in
      let clause = $6 in
      let pos = union_pos $1 (pos_of_clause clause) in
      let input = InPattern (insts, Some clause, pos) in
         (name, input), pos
   }
 | TokInput io_name TokColon in_pattern_list {
      let name = $2 in
      let insts = $4 in
      let pos = union_pos_in_inst_list $1 insts in
      let input = InPattern (insts, None, pos) in
         (name, input), pos
   }
   ;

io_input_list:
   io_input io_input_list {
      let input, pos1 = $1 in
      let inputs, pos2 = $2 in
      let pos = union_pos pos1 pos2 in
         input :: inputs, pos
   }
 | io_input {
      let input, pos = $1 in
         [input], pos
   }
   ;

io_name:
   TokString {
      let name, _ = $1 in
         Some name
   }
 | { None }
   ;

/* io_output
   Analagous to the above, this defines single Onput expression.
   This begins with the OUTPUT keyword, followed by an optional
   WHEN clause, followed by an output pattern.  At least one of
   these expressions must be present in any InputOutput.  Note
   that the pattern is optional for output.  */
io_output:
   TokOutput TokWhen clause_expr TokColon out_pattern_list_opt {
      let clause = $3 in
      let pos = union_pos_out_inst_list $1 $5 in
         OutPattern ($5, Some clause, pos), pos
   }
 | TokOutput TokColon out_pattern_list_opt {
      let pos = union_pos_out_inst_list $1 $3 in
         OutPattern ($3, None, pos), pos
   }
   ;

io_output_list:
   io_output io_output_list {
      let output, pos1 = $1 in
      let outputs, pos2 = $2 in
      let pos = union_pos pos1 pos2 in
         output :: outputs, pos
   }
 | io_output {
      let output, pos = $1 in
         [output], pos
   }
   ;

/* arith_expr
   Defines an integer arithmetic expression. */
arith_expr:
   TokIdent {
      let ident, pos = $1 in
         ArithReg (ident, pos)
   }
 | TokInt {
      let value, pos = $1 in
         ArithInt (value, pos)
   }
 | TokLParen arith_expr TokRParen { $2 }
 | arith_expr plus_minus_op arith_expr %prec plusminus_prec {
      let v1 = $1 in
      let v2 = $3 in
      let pos = union_pos (pos_of_arith v1) (pos_of_arith v2) in
         ArithBinop ($2, v1, v2, pos)
   }
 | arith_expr logical_op arith_expr %prec logical_prec {
      let v1 = $1 in
      let v2 = $3 in
      let pos = union_pos (pos_of_arith v1) (pos_of_arith v2) in
         ArithBinop ($2, v1, v2, pos)
   }
 | arith_expr shift_op arith_expr %prec shift_prec {
      let v1 = $1 in
      let v2 = $3 in
      let pos = union_pos (pos_of_arith v1) (pos_of_arith v2) in
         ArithBinop ($2, v1, v2, pos)
   }
 | unary_op arith_expr %prec unary_prec {
      let op, pos = $1 in
      let v = $2 in
      let pos = union_pos pos (pos_of_arith v) in
         ArithUnop (op, v, pos)
   }
   ;

arith_expr_list:
   arith_expr TokComma arith_expr_list { $1 :: $3 }
 | arith_expr { [$1] }
   ;

plus_minus_op:
   TokPlus { PlusOp }
 | TokMinus { MinusOp }
   ;

logical_op:
   TokAmp { AndOp }
 | TokPipe { OrOp }
   ;

shift_op:
   TokShl { ShlOp }
 | TokSar { SarOp }
 | TokShr { ShrOp }
   ;

unary_op:
   TokMinus { UMinusOp, $1 }
 | TokTilde { UNotOp, $1 }
 | TokAbs  { UAbsOp, $1 }
 | TokPow2 { UPow2Op, $1 }
 | TokLog2 { ULog2Op, $1 }
   ;

/* clause_expr
   Denotes a clause, as occurring after a WHEN keyword.  Clauses
   are simple boolean expressions which may, at their option, also
   invoke snippets of OCaml code.  */
clause_expr:
   TokLParen clause_expr TokRParen { $2 }
 | TokOCaml {
      let ml, pos = $1 in
         ClauseML (ml, pos)
   }
 | arith_expr relop arith_expr {
      let a = $1 in
      let b = $3 in
      let pos = union_pos (pos_of_arith a) (pos_of_arith b) in
         ClauseCompare ($2, a, b, pos)
   }
 | TokIspower2 arith_expr {
      let v = $2 in
      let pos = union_pos $1 (pos_of_arith v) in
         ClauseIspower2 (v, pos)
   }
 | clause_expr bool_op clause_expr %prec bool_prec {
      let c1 = $1 in
      let c2 = $3 in
      let op = $2 in
      let pos = union_pos (pos_of_clause c1) (pos_of_clause c2) in
         ClauseBoolean (op, c1, c2, pos)
   }
   ;

relop:
   TokEqual          { EqOp }
 | TokNotEqual       { NeqOp }
 | TokLessThan       { LtOp }
 | TokLessEqual      { LeOp }
 | TokGreaterThan    { GtOp }
 | TokGreaterEqual   { GeOp }
   ;

bool_op:
   TokAmpAmp         { BAndOp }
 | TokPipePipe       { BOrOp }
   ;


/***  Misc. Statements  ***/


/* open_statement
   Statement used to open ML modules in the program.  */
open_statement:
   TokOpen TokCapIdent {
      let ident, pos = $2 in
      let pos = union_pos $1 pos in
         Open (ident, pos)
   }
   ;

/* name_statement
   Used to apply a name to all anonymous (unnamed) rules
   following the declaration.  Scope is valid until the
   next Name directive.  */
name_statement:
   TokName TokString {
      let name, pos = $2 in
      let pos = union_pos $1 pos in
         Name (name, pos)
   }
   ;

/* register_statement
   Used to declare which symbols are machine registers
   and should be treated specially.  The type is included
   by specifying the type restricting pattern, e.g. give
   '%' to indicate the register is integer register.  The
   anonymous operand form is not supported.  */
register_statement:
   TokRegisters TokColon register_list {
      let regs, pos = $3 in
      let pos = union_pos $1 pos in
         Registers (regs, pos)
   }
   ;

register_list:
   register TokSemi register_list {
      let r, pos1 = $1 in
      let regs, pos2 = $3 in
      let pos = union_pos pos1 pos2 in
         r :: regs, pos
   }
 | register TokSemi {
      let r, pos = $1 in
      let pos = union_pos pos $2 in
         [r], pos
   }
   ;

register:
   TokPercent TokIdent {
      let r, pos = $2 in
      let pos = union_pos $1 pos in
         (r, TyReg), pos
   }
 | TokAt TokIdent {
      let r, pos = $2 in
      let pos = union_pos $1 pos in
         (r, TyFlReg), pos
   }
   ;

/* instruction_statement
   Declares instructions, including their attributes and
   the constructor to use to build the instruction.  */
instruction_statement:
   TokInsts instruction_tags TokColon instruction_list {
      let tags = List.map fst $2 in
      let insts, pos = $4 in
      let pos = union_pos $1 pos in
      let insts = List.map (fun (Instruction (name, op, cons, consop, itags, pos)) ->
         Instruction (name, op, cons, consop, itags @ tags, pos)) insts in
         Instructions (insts, pos)
   }
   ;

instruction_tags:
   TokIdent instruction_tags { $1 :: $2 }
 | { [] }
   ;

instruction_list:
   instruction TokSemi instruction_list {
      let Instruction (_, _, _, _, _, pos1) = $1 in
      let insts, pos2 = $3 in
      let pos = union_pos pos1 pos2 in
         $1 :: insts, pos
   }
 | instruction TokSemi {
      let Instruction (_, _, _, _, _, pos1) = $1 in
      let pos = union_pos pos1 $2 in
         [$1], pos
   }
   ;

instruction:
   instruction_data TokColon instruction_tag_list {
      let name, op, cons, consop, pos1 = $1 in
      let tags, pos2 = $3 in
      let pos = union_pos pos1 pos2 in
         Instruction (name, op, cons, consop, tags, pos)
   }
 | instruction_data {
      let name, op, cons, consop, pos = $1 in
         Instruction (name, op, cons, consop, [], pos)
   }
   ;

instruction_data:
   TokIdent TokArrow TokCapIdent {
      let name, pos1 = $1 in
      let cons, pos2 = $3 in
      let pos = union_pos pos1 pos2 in
         name, [], cons, [], pos
   }
 | TokIdent in_operand_list TokArrow TokCapIdent in_operand {
      let name, pos1 = $1 in
      let operands = $2 in
      let cons, _ = $4 in
      let cons_operand = $5 in
      let pos = union_pos pos1 (pos_of_operand cons_operand) in
         name, operands, cons, [cons_operand], pos
   }
 | TokIdent in_operand_list TokArrow TokCapIdent TokLParen in_operand_list TokRParen {
      let name, pos1 = $1 in
      let operands = $2 in
      let cons, _ = $4 in
      let cons_operands = $6 in
      let pos = union_pos pos1 $7 in
         name, operands, cons, cons_operands, pos
   }
   ;

instruction_tag_list:
   TokIdent instruction_tag_list {
      let tag, pos1 = $1 in
      let tags, pos2 = $2 in
         tag :: tags, union_pos pos1 pos2
   }
 | TokIdent {
      let tag, pos = $1 in
         [tag], pos
   }
   ;

/* transform_statement
   The transform is similar to an instruction block, but a
   transform block defines a mapping of instructions onto
   themselves, and allows the user to rewrite instructions
   as part of an output pattern according to these pre-
   determined rules.  */
transform_statement:
   TokTrans TokIdent symbol_list_opt TokColon transform_list {
      let name, _ = $2 in
      let args = $3 in
      let insts, pos = $5 in
      let pos = union_pos $1 pos in
         Transforms (name, args, insts, pos)
   }
   ;

transform_list:
   transform TokSemi transform_list {
      let Transform (_, _, _, _, pos1) = $1 in
      let insts, pos2 = $3 in
      let pos = union_pos pos1 pos2 in
         $1 :: insts, pos
   }
 | transform TokSemi {
      let Transform (_, _, _, _, pos1) = $1 in
      let pos = union_pos pos1 $2 in
         [$1], pos
   }
   ;

transform:
   TokIdent in_operand_list_opt TokArrow TokIdent out_operand_list_opt {
      let opcode1, pos1 = $1 in
      let operand1 = $2 in
      let opcode2, pos2 = $4 in
      let operand2 = $5 in
      let pos = union_pos_operand_list pos1 operand2 in
         Transform (opcode1, operand1, opcode2, operand2, pos)
   }
   ;

symbol_list:
   TokIdent TokComma symbol_list {
      let ident, _ = $1 in
         ident :: $3
   }
 | TokIdent {
      let ident, _ = $1 in
         [ident]
   }
   ;

symbol_list_opt:
   symbol_list { $1 }
 | { [] }
   ;

/* trivial_statement
   Identifies the tag associated with trivial instructions. */
trivial_statement:
   TokTrivial TokIdent {
      let ident, pos = $2 in
      let pos = union_pos $1 pos in
         Trivial (ident, pos)
   }
   ;

