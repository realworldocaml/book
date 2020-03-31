(* Original file: omake.0.9.8.7/omake-0.9-0.9.8.7/src/shell/omake_shell_parse.mly *)
/*
 * Parser for the command line.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2004-2006 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 * 
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
 *
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 */
%{
open Lm_printf
open Lm_symbol
open Lm_location

open Omake_env
open Omake_pos
open Omake_shell_type
open Omake_command_type
open Omake_value_type

module Pos = MakePos (struct let name = "Omake_shell_parse" end)
open Pos

(*
 * If the command is a node, detect it here.
 *)
let collect_redirect chan =
   match chan with
      [ValNode node] ->
         RedirectNode node
    | _ ->
         RedirectArg chan

(*
 * Build a command from a sequence of words.
 *)
let null_command loc =
   { cmd_loc     = loc;
     cmd_env     = [];
     cmd_exe     = ();
     cmd_argv    = [];
     cmd_stdin   = RedirectNone;
     cmd_stdout  = RedirectNone;
     cmd_stderr  = false;
     cmd_append  = false
   }

let command_of_values argv loc =
   { cmd_loc     = loc;
     cmd_env     = [];
     cmd_exe     = ();
     cmd_argv    = argv;
     cmd_stdin   = RedirectNone;
     cmd_stdout  = RedirectNone;
     cmd_stderr  = false;
     cmd_append  = false
   }

(*
 * Diversions.
 *)
let rec set_stdin_inner pipe file =
   match pipe with
      PipeApply (loc, apply) ->
         let apply = { apply with apply_stdin = file } in
            PipeApply (loc, apply)
    | PipeCommand (loc, command) ->
         let command = { command with cmd_stdin = file } in
            PipeCommand (loc, command)
    | PipeCond (loc, _, _, _)
    | PipeCompose (loc, _, _, _) ->
         raise (Invalid_argument "Omake_shell_parse.set_stdin: internal error")
    | PipeGroup (loc, group) ->
         let group = { group with group_stdin = file } in
            PipeGroup (loc, group)
    | PipeBackground (loc, pipe) ->
         PipeBackground (loc, set_stdin_inner pipe file)

let rec set_stdout_inner pipe file stderr append =
   match pipe with
      PipeApply (loc, apply) ->
         let apply =
            { apply with apply_stdout = file;
                         apply_stderr = stderr;
                         apply_append = append
            }
         in
            PipeApply (loc, apply)
    | PipeCommand (loc, command) ->
         let command =
            { command with cmd_stdout = file;
                           cmd_stderr = stderr;
                           cmd_append = append
            }
         in
            PipeCommand (loc, command)
    | PipeCond (loc, _, _, _)
    | PipeCompose (loc, _, _, _) ->
         raise (Invalid_argument "Omake_shell_parse.set_stdout: internal error")
    | PipeGroup (loc, group) ->
         let group =
            { group with group_stdout = file;
                         group_stderr = stderr;
                         group_append = append
            }
         in
            PipeGroup (loc, group)
    | PipeBackground (loc, pipe) ->
         PipeBackground (loc, set_stdout_inner pipe file stderr append)

let set_stdin pipe file =
   set_stdin_inner pipe (collect_redirect file)

let set_stdout pipe file stderr append =
   set_stdout_inner pipe (collect_redirect file) stderr append
%}

/*
 * Terminators
 */
%token <Lm_location.loc> TokEof

/*
 * Indentation tokens are converted in the lexer
 * to TokBegin/TokEnd.
 */
%token <Omake_value_type.value list * Lm_location.loc> TokValues
%token <string * Lm_location.loc> TokDefine
%token <string * Lm_location.loc> TokLeftParen
%token <string * Lm_location.loc> TokRightParen
%token <string * Lm_location.loc> TokLessThan
%token <string * Lm_location.loc> TokGreaterThan
%token <string * Lm_location.loc> TokGreaterGreaterThan
%token <string * Lm_location.loc> TokAmp
%token <string * Lm_location.loc> TokPipe
%token <string * Lm_location.loc> TokSemiColon
%token <string * Lm_location.loc> TokAnd
%token <string * Lm_location.loc> TokOr

/*
 * Pipes are right-associative:
 *    "foo1 | foo2 |& foo3" should _not_ pass foo1's stderr to foo3.
 *
 * The pipe has higher precedence that ";" :
 *    "foo1; foo2 | foo3" should redirect only foo2's output, not foo1's.
 *
 * The "&&" has higher precedence than "||" (same as in tcsh, different from bash)
 *
 * Separately, each of the "&&", "||", and ";" is right-associative (the semantics
 * is associative, so this does not really matter, but right-associativity is more
 * efficient at run-time).
 */
%nonassoc TokAmp
%right TokSemiColon
%right TokPipe
%right TokOr
%right TokAnd
%nonassoc TokGreaterThan TokGreaterGreaterThan TokLessThan

/*
 * A complete program.
 */
%start prog
%type <Omake_env.value_pipe> prog

%%
/*
 * Remove the location.
 */
prog:     pipe TokEof
          { let pipe, _ = $1 in
               pipe
          }
        | TokEof
          { let loc = $1 in
               PipeCommand (loc, null_command loc)
          }
        ;

/*
 * A pipe is a composition of commands.
 */
pipe:     command
          { let command, loc = $1 in
               PipeCommand (loc, command), loc
          }
        | pipe TokSemiColon pipe
          { let pipe1, loc1 = $1 in
            let pipe2, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
               PipeCond (loc, PipeSequence, pipe1, pipe2), loc
          }
        | pipe TokAnd pipe
          { let pipe1, loc1 = $1 in
            let pipe2, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
               PipeCond (loc, PipeAnd, pipe1, pipe2), loc
          }
        | pipe TokOr pipe
          { let pipe1, loc1 = $1 in
            let pipe2, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
               PipeCond (loc, PipeOr, pipe1, pipe2), loc
          }
        | pipe TokPipe pipe
          { let pipe1, loc1 = $1 in
            let pipe2, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
               PipeCompose (loc, false, pipe1, pipe2), loc
          }
        | pipe TokPipe TokAmp pipe %prec TokPipe
          { let pipe1, loc1 = $1 in
            let pipe2, loc2 = $4 in
            let loc = union_loc loc1 loc2 in
               PipeCompose (loc, true, pipe1, pipe2), loc
          }
        | pipe TokAmp
          { let pipe, loc1 = $1 in
            let _, loc2 = $2 in
            let loc = union_loc loc1 loc2 in
               PipeBackground (loc, pipe), loc
          }
        | TokLeftParen pipe TokRightParen
          { let _, loc1 = $1 in
            let _, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
            let pipe, _ = $2 in
            let group =
               { group_stdin  = RedirectNone;
                 group_stdout = RedirectNone;
                 group_stderr = false;
                 group_append = false;
                 group_pipe = pipe
               }
            in
               PipeGroup (loc, group), loc
          }
        | pipe TokLessThan word
          { let pipe, loc1 = $1 in
            let file, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
            let pipe = set_stdin pipe file in
               pipe, loc
          }
        | pipe TokGreaterThan word
          { let pipe, loc1 = $1 in
            let file, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
            let pipe = set_stdout pipe file false false in
               pipe, loc
          }
        | pipe TokGreaterGreaterThan word
          { let pipe, loc1 = $1 in
            let file, loc2 = $3 in
            let loc = union_loc loc1 loc2 in
            let pipe = set_stdout pipe file false true in
               pipe, loc
          }
        | pipe TokGreaterThan TokAmp word %prec TokGreaterThan
          { let pipe, loc1 = $1 in
            let file, loc2 = $4 in
            let loc = union_loc loc1 loc2 in
            let pipe = set_stdout pipe file true false in
               pipe, loc
          }
        | pipe TokGreaterGreaterThan TokAmp word %prec TokGreaterGreaterThan
          { let pipe, loc1 = $1 in
            let file, loc2 = $4 in
            let loc = union_loc loc1 loc2 in
            let pipe = set_stdout pipe file true true in
               pipe, loc
          }
        ;

/*
 * A command is just a sequence of words.
 */
command: rev_command
          { let rev_argv, loc = $1 in
            let command = command_of_values (List.rev rev_argv) loc in
               command, loc
          }
        ;

rev_command:
          word
          { let values, loc = $1 in
               [values], loc
          }
        | rev_command word
          { let values1, loc1 = $1 in
            let values2, loc2 = $2 in
               values2 :: values1, union_loc loc1 loc2
          }
        ;

/*
 * A word is just a core set of values.
 */
word:	  TokValues
	  { $1 }
	;
