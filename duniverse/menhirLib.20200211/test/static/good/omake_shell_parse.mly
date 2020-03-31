/*
 * Parser for the command line.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2004 Jason Hickey, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * jyh@cs.caltech.edu
 */
%{
open Lm_printf
open Lm_symbol
open Lm_location

open Omake_env
open Omake_shell_type

module Pos = MakePos (struct let name = "Omake_shell_parse" end)
open Pos

(*
 * Concat a list of strings in reverse order.
 *)
let concat_word wordl =
   match wordl with
      [word] ->
           word
    | _ ->
        let buf = Buffer.create 32 in
           List.iter (fun s ->
                 Buffer.add_string buf s) (List.rev wordl);
           Buffer.contents buf

(*
 * Build the environment.
 *)
let env_of_list l =
   List.rev_map (fun (v, x) ->
      let len = String.length v in
      let i2 =
         if len = 0 || v.[len - 1] <> '=' then
            len
         else
            len - 1
      in
      let v = String.sub v 0 (i2 - 0) in
         Lm_symbol.add v, x) l

(*
 * Diversions.
 *)
let rec set_stdin pipe file =
   match pipe with
      PipeApply (loc, apply) ->
         let apply = { apply with apply_stdin = Some file } in
	    PipeApply (loc, apply)
    | PipeCommand (loc, command) ->
         let command = { command with cmd_stdin = Some file } in
            PipeCommand (loc, command)
    | PipeCond (loc, _, _, _)
    | PipeCompose (loc, _, _, _) ->
         let group =
            { group_loc = loc;
              group_stdin = Some file;
              group_stdout = None;
              group_stderr = false;
              group_append = false;
              group_pipe = pipe
            }
         in
            PipeGroup (loc, group)
    | PipeGroup (loc, group) ->
         let group = { group with group_stdin = Some file } in
            PipeGroup (loc, group)
    | PipeBackground (loc, pipe) ->
         PipeBackground (loc, set_stdin pipe file)

let rec set_stdout pipe file stderr append =
   match pipe with
      PipeApply (loc, apply) ->
         let apply =
            { apply with apply_stdout = Some file;
            	         apply_stderr = stderr;
                         apply_append = append
            }
         in
            PipeApply (loc, apply)
    | PipeCommand (loc, command) ->
         let command =
            { command with cmd_stdout = Some file;
                           cmd_stderr = stderr;
                           cmd_append = append
            }
         in
            PipeCommand (loc, command)
    | PipeCond (loc, _, _, _)
    | PipeCompose (loc, _, _, _) ->
         let group =
            { group_loc = loc;
              group_stdin = None;
              group_stdout = Some file;
              group_stderr = stderr;
              group_append = append;
              group_pipe = pipe
            }
         in
            PipeGroup (loc, group)
    | PipeGroup (loc, group) ->
         let group =
            { group with group_stdout = Some file;
                         group_stderr = stderr;
                         group_append = append
            }
         in
            PipeGroup (loc, group)
    | PipeBackground (loc, pipe) ->
         PipeBackground (loc, set_stdin pipe file)
%}

/*
 * Terminators
 */
%token <Lm_location.loc> TokEof

/*
 * Indentation tokens are converted in the lexer
 * to TokBegin/TokEnd.
 */
%token <string * Lm_location.loc> TokWhite
%token <string * Lm_location.loc> TokString
%token <string * Lm_location.loc> TokData
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
 * Pipes are left-associative.
 */
%left TokPipe TokAmp
%left TokSemiColon
%left TokAnd TokOr
%left TokGreaterThan TokGreaterGreaterThan TokLessThan

/*
 * A complete program.
 */
%start prog
%type <Omake_shell_type.pipe> prog

%%
/*
 * Remove the location.
 */
prog:     pipe TokEof
          { let pipe, _ = $1 in
               pipe
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
        | pipe TokPipe TokAmp pipe
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
               { group_loc = loc;
                 group_stdin = None;
                 group_stdout = None;
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
 * A command has a core sequence of words,
 * followed by some possible redirections.
 */
command: rev_command
          { let escaped, env, argv, loc = $1 in
            let env = env_of_list env in
            let argv = List.rev argv in
	    let exe = List.hd argv in
            let command =
               { cmd_loc     = loc;
                 cmd_env     = env;
                 cmd_exe     = exe;
                 cmd_argv    = argv;
                 cmd_stdin   = None;
                 cmd_stdout  = None;
                 cmd_stderr  = false;
                 cmd_append  = false;
                 cmd_escaped = escaped
               }
            in
               command, loc
          }
        ;

rev_command:
          nondef_word
          { let escaped, word, loc = $1 in
               escaped, [], [word], loc
          }
        | rev_cmd_env TokWhite nondef_word
          { let env, loc1 = $1 in
            let escaped, word, loc2 = $3 in
               escaped, env, [word], union_loc loc1 loc2
          }
        | rev_command TokWhite word
          { let escaped, env, words, loc1 = $1 in
            let word, loc2 = $3 in
               escaped, env, word :: words, union_loc loc1 loc2
          }
        ;

rev_cmd_env:
          TokDefine opt_word
          { let name, loc = $1 in
               [name, $2], loc
          }
        | rev_cmd_env TokWhite TokDefine opt_word
          { let env, loc1 = $1 in
            let name, loc2 = $3 in
            let word = $4 in
               ((name, word) :: env), union_loc loc1 loc2
          }
        ;

/*
 * A word is a sequence of strings that does not degin with
 * a definition.
 */
nondef_word:
          rev_nondef_word
          { let escaped, idl, loc = $1 in
               escaped, concat_word idl, loc
          }
        ;

rev_nondef_word:
          TokString
          { let id, loc = $1 in
               if id = "\\" then
                  true, [], loc
               else
                  false, [id], loc
          }
        | TokData
          { let id, loc = $1 in
               true, [id], loc
          }
        | rev_nondef_word any_id
          { let escaped, idl, loc1 = $1 in
            let word, loc2 = $2 in
               escaped, word :: idl, union_loc loc1 loc2
          }
        ;

/*
 * Any word can include definitions.
 */
opt_word:
          /* empty */
          { "" }
        | word
          { let id, _ = $1 in
               id
          }
        ;

word:     rev_word
          { let idl, loc = $1 in
               concat_word idl, loc
          }
        ;

rev_word:
          any_id
          { let id, loc = $1 in
               [id], loc
          }
        | rev_word any_id
          { let idl, loc1 = $1 in
            let word, loc2 = $2 in
               word :: idl, union_loc loc1 loc2
          }
        ;

/*
 * Any id is a string or a define.
 */
any_id:   TokString
          { $1 }
        | TokData
          { $1 }
        | TokDefine
          { $1 }
        ;
