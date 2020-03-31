(* Original file: camomile.1.0.1/camomile-1.0.1/Camomile/toolslib/colParser.mly *)
/* Collation rule parser

Copyright (C) 2003 Yamagata Yoriyuki

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License
as published by the Free Software Foundation; either version 2 of
the License, or (at your option) any later version.

As a special exception to the GNU Library General Public License, you
may link, statically or dynamically, a "work that uses this library"
with a publicly distributed version of this library to produce an
executable file containing portions of this library, and distribute
that executable file under terms of your choice, without any of the
additional requirements listed in clause 6 of the GNU Library General
Public License. By "a publicly distributed version of this library",
we mean either the unmodified Library as distributed by the authors,
or a modified version of this library that is distributed under the
conditions defined in clause 3 of the GNU Library General Public
License. This exception does not however invalidate any other reasons
why the executable file might be covered by the GNU Library General
Public License .

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA

You can contact the authour by sending email to
yoriyuki.y@gmail.com

*/

%{

open CamomileLibrary

let parse_error _ = failwith "Syntax error"
let acset : AbsCe.aceset_info = Toolslib.Unidata.read_data "acset"

    %}

  %token <CamomileLibrary.UChar.t> UCHAR
  %token <string list> OPTION
  %token PRIMARY SECONDARY TERTIARY EQ RESET EXPAND PREFIX EOF
  %nonassoc RESET
  %left PRIMARY SECONDARY TERTIARY EQ
  %left EXPAND
  %right PREFIX

  %start main
  %type <AbsCe.ace_info> main
  %%
main :
  header rules EOF
  {let ceset = acset.lowercase_first_tbl in
  let ace_info = AbsCe.create_ace_info ceset in
  let ace_info = $1 ace_info in
  {ace_info with ceset = $2 ace_info.AbsCe.ceset}}
| rules EOF
  {let ceset = acset.lowercase_first_tbl in
  let ace_info = AbsCe.create_ace_info ceset in
  {ace_info with ceset = $1 ace_info.ceset}}
| header EOF
    {let ceset = acset.lowercase_first_tbl in
    $1 (AbsCe.create_ace_info ceset)}
| EOF
    {AbsCe.create_ace_info acset.lowercase_first_tbl};

  header :
    header_option header
    {fun env ->
      $2 ($1 env)}
| header_option
    {fun env -> $1 env};

  header_option :
    OPTION {fun env ->
      match $1 with
	["alternate"; "non-ignorable"] ->
	  {env with variable_option = `Non_ignorable}
      | ["alternate"; "shifted"] ->
	  {env with variable_option = `Shifted}
      | ["alternate"; "shifted-trimmed"] ->
	  {env with variable_option = `Shift_Trimmed}
      | ["alternate"; "blanked"] ->
	  {env with variable_option = `Blanked}
      | ["backwards"; "2"] ->
	  {env with french = true}
      | ["backwards"; x] ->
	  failwith
	    ("backward comparison for the level " ^ x ^ " is not supported.")
      | ["normalization"; _] ->
	  prerr_endline "Warning : normalization option is not supported";
	  env
      | ["caseLevel"; _] ->
	  prerr_endline "Warning : caseLevel option is not supported";
	  env

      | [("caseFirst" | "casefirst"); ("off" | "lower")] ->
	  let ceset = acset.lowercase_first_tbl in
	  {env with ceset = ceset}
      | [("caseFirst" | "casefirst"); "upper"] ->
	  let ceset = acset.uppercase_first_tbl in
	  {env with ceset = ceset}
      | ["strength"; _] ->
	  prerr_endline "Warning : strength option is not supported";
	  env
      | ["hiraganaQ"; _] ->
	  let ceset = env.ceset in
	  let ce, ceset = AbsCe.add_after `Primary (AbsCe.last_variable ceset) ceset in
	  let ceset = AbsCe.put `HiraganaQ [ce] ceset in
	  {env with ceset = ceset; hiraganaQ = true}
      | x ->
	  let s = String.concat " " x in
	  failwith ("unknown option:" ^ s)}
  | UCHAR {
    fun env ->
      if $1 = UChar.of_char '@' then
	{env with french = true}
      else
	failwith "stray character"};

  rules :
    rule rules
    {fun env ->
      let _, _, env = $1 env in
      $2 env}
| rule {fun env -> let _, _, env = $1 env in env};

  rule :
  RESET init {$2}
| RESET OPTION init {
  fun ceset ->
    match $2 with
      ["before"; depth] ->
	let prec =
	  match int_of_string depth with
	    1 -> `Primary
	  | 2 -> `Secondary
	  | 3 -> `Tertiary
	  | _ -> failwith ("Level " ^ depth ^ " is not supported")
	in
	let pos, exp, ceset = $3 ceset in
	(AbsCe.prev prec pos ceset, exp, ceset)
    | _ -> failwith "Unknown option"}
| rule PRIMARY elem {
  fun ceset ->
    let pos, exp, ceset = $1 ceset in
    let pos', ceset = AbsCe.add_after `Primary pos ceset in
    let ceset' = $3 [] (pos' :: exp) ceset in
    (pos', exp, ceset')}
| rule SECONDARY elem {
  fun ceset ->
    let pos, exp, ceset = $1 ceset in
    let pos', ceset = AbsCe.add_after `Secondary pos ceset in
    let ceset' = $3 [] (pos' :: exp) ceset in
    (pos', exp, ceset')}
| rule TERTIARY elem {
  fun ceset ->
    let pos, exp, ceset = $1 ceset in
    let pos', ceset = AbsCe.add_after `Tertiary pos ceset in
    let ceset' = $3 [] (pos' :: exp) ceset in
    (pos', exp, ceset')}
| rule EQ elem {
  fun ceset ->
    let pos, exp, ceset = $1 ceset in
    let ceset' = $3 [] (pos :: exp) ceset in
    (pos, exp, ceset')};

  ulist : UCHAR {[$1]} | UCHAR ulist {$1 :: $2};

  init :
    ulist {
  fun ceset ->
    let ceset, es = AbsCe.ces_of ceset $1 in
    (List.hd es, List.tl es, ceset)}
| OPTION {
  fun ceset ->
    match $1 with
      [("first" | "last") ; "teritiary"; "ignorable"]
    | [("first" | "last") ; "secondary"; "ignorable"] ->
	(AbsCe.complete_ignorable ceset, [], ceset)
    | ["first"; "primary"; "ignorable"] ->
	let ce = AbsCe.next `Secondary (AbsCe.complete_ignorable ceset) ceset in
	(ce, [], ceset)
    | ["last"; "primary"; "ignorable"] ->
	let ce = AbsCe.next `Primary (AbsCe.complete_ignorable ceset) ceset in
	(ce, [], ceset)
    | ["first"; "variable"] ->
	let ce = AbsCe.next `Primary (AbsCe.complete_ignorable ceset) ceset in
	(ce, [], ceset)
    | ["last"; "variable"] ->
	(AbsCe.last_variable ceset, [], ceset)
    | ["first"; "regular"] ->
	let ce = AbsCe.next `Primary (AbsCe.last_variable ceset) ceset in
	(ce, [], ceset)
    | ["last"; "regular"] | ["top"] ->
	let ce = AbsCe.prev `Tertiary (AbsCe.first_implicit ceset) ceset in
	(ce, [], ceset)
    | ["first"; "implicit"] ->
	(AbsCe.first_implicit ceset, [], ceset)
    | ["last"; "implicit"] ->
	let ce = AbsCe.prev `Tertiary (AbsCe.first_trailing ceset) ceset in
	(ce, [], ceset)
    | ["first"; "trailing"] ->
	(AbsCe.first_trailing ceset, [], ceset)
    | ["last"; "trailing"] ->
	(AbsCe.top ceset, [], ceset)
    | _ -> assert false};

  elem :
    ulist {fun prefix ces ceset ->
      AbsCe.put (`Seq (prefix @ $1)) ces ceset}
| elem EXPAND ulist {fun prefix ces ceset ->
    let ceset', exps = AbsCe.ces_of ceset $3 in
    $1 prefix (ces @ exps) ceset'}
| ulist PREFIX elem {fun prefix ces ceset ->
    let ceset', exps = AbsCe.ces_of ceset $1 in
    $3 (prefix @ $1) (exps @ ces) ceset'}
| OPTION {fun prefix ces ceset ->
    match $1 with ["variable"; "top"] ->
      (match prefix, ces with
	[], [_] ->
	  AbsCe.put `LastVariable ces ceset
      | _, _ ->
	  failwith "Variable top should be neither contraction nor prefixed.")
    | _ -> failwith "Unknown option"};
