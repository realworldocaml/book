/*========================================================================*/
/*                                                                        */
/*             cppmem model exploration tool                              */
/*                                                                        */
/*                    Mark Batty                                          */
/*                    Scott Owens                                         */
/*                    Jean Pichon                                         */
/*                    Susmit Sarkar                                       */
/*                    Peter Sewell                                        */
/*                                                                        */
/*  This file is copyright 2011, 2012 by the above authors.               */
/*                                                                        */
/*  Redistribution and use in source and binary forms, with or without    */
/*  modification, are permitted provided that the following conditions    */
/*  are met:                                                              */
/*  1. Redistributions of source code must retain the above copyright     */
/*  notice, this list of conditions and the following disclaimer.         */
/*  2. Redistributions in binary form must reproduce the above copyright  */
/*  notice, this list of conditions and the following disclaimer in the   */
/*  documentation and/or other materials provided with the distribution.  */
/*  3. The names of the authors may not be used to endorse or promote     */
/*  products derived from this software without specific prior written    */
/*  permission.                                                           */
/*                                                                        */
/*  THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS    */
/*  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE    */
/*  ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY       */
/*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL    */
/*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE     */
/*  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS         */
/*  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHE   */
/*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR       */
/*  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN   */
/*  IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                         */
/*========================================================================*/

/* parser for executions */

%{
open Globals
open Location
open Types
open Auxl
open Value
open Ids

let mkl () =
   [ { Location.loc_start = symbol_start_pos ();
       Location.loc_end = symbol_end_pos () } ]

let mkp () =
   symbol_start_pos ()

(* let mkl x =  *)
(*   let l = {Location.loc_start=symbol_start_pos ();  *)
(* 	   Location.loc_end=symbol_end_pos ()} in *)
(*   (\* print_string (Location.pp_t l) ; flush stdout; *\)  *)
(*   {desc=x; loc=l} *)

let parse_error s =
  raise (My_parse_error ("Parse error: " ^ s ^ " "
	   ^ Location.pp_position2 (Parsing.symbol_end_pos () )))




(* %token EQUAL             *)
%}

%token MODEL
%token LOCATION_KINDS
%token ATOMIC
%token NONATOMIC
%token MUTEX
%token RSKEL
%token WSKEL
%token RMWSKEL
%token FSKEL

%token  L
%token  U
%token  <Types.order_type>R
%token  <Types.order_type>W
%token  <Types.order_type>RMW
%token  <Types.order_type>F
%token <string> IDENT
%token <string> EDGE
%token <string> CHECK
%token  COLON
%token  SEMICOLON
%token  DOT
%token  EQUALS
%token  SLASH
%token  LBRACE
%token  RBRACE
%token  COMMA
%token  SAMELOC
%token  ATOMICLOCS
%token  EDGETAIL
%token  EDGEHEAD
%token  IGNORE CONSIDER
%token  DISPLAY_ACTION DISPLAY_EDGE SUPPRESS_ACTION SUPPRESS_EDGE
%token  ADD REMOVE
%token  <string> FILENAME
%token  GENERATE
%token  DOTFILE ISAFILE EXCFILE INSTRFILE
%token  QUIT NEXT NEXTCANDIDATE NEXTCONSISTENT INTERMEDIATE NONSTOP HELP RELABEL
%token  MODE NEATO FIG PS INIT PAR TEX TXT
%token  FROM
%token  TO
%token  ALL
%token  SHOW
%token  EOF
%token  WHITESPACE
%token SET
%token FONTSIZE
%token NODE_HEIGHT
%token NODE_WIDTH
%token PENWIDTH
%token THREAD_IDS
%token FILLED
%token XSCALE
%token YSCALE
%token RANKSEP NODESEP
%token LEGEND
%token LAYOUT
%token NEATO_PAR
%token NEATO_PAR_INIT
%token NEATO_DOWNWARDS
%token <int> NUM
%token <string> STRING
%token <bool> BOOL
%token STAR
%token <Types.order_type> MO
%token <Cmm.lock_outcome> LOCKOUTCOME

%start execfile             /* the entry point for execfile */
%start skeletonexecfile             /* the entry point for skeletonexecfile */

%start instructions         /* the entry point for instructions */

%type <Cmm.action> action

%type <Types.execfileitem list> execfile

%type <Types.skeletonexecfileitem list> skeletonexecfile

%type <Types.raw_instructions> instructions


%%




/* main:
    action  EOF  { $2 }
*/

execfile:
   items { $1 }

instructions :
    EOF                           { empty_raw_instructions }
  | instruction                   { $1 }
  | instruction DOT               { $1 }
  | instruction DOT instructions  { add_raw_instructions $1 $3 }

instruction :
    command                    { {empty_raw_instructions with
				     raw_commands = [$1]} }
  | SHOW IDENT                      { let i =
                                            try int_of_string $2
                                            with Failure "int_of_string" -> parse_error ("Show expects a number, not " ^ $2) in
                                         {empty_raw_instructions with raw_show = Some i;} }
  | SHOW ALL                         { {empty_raw_instructions with raw_show = None;} }

  | CONSIDER transition_sequence { let rel =
                                           let (start,ts) = $2 in
                                           match ts with
                                           | [] -> parse_error "not considering blank constraint"
                                           | (e,_) :: _ ->
                                               (e,List.map (fun (_,fin) -> (start,fin)) ts)
                                   in
                                   {empty_raw_instructions with
                                    raw_constrain_rels =
                                    [ConsiderRel rel];} }
  | CONSIDER prefixed_transition_sequence { let rel =
                                           let (start,ts) = $2 in
                                           match ts with
                                           | [] -> parse_error "not considering blank constraint"
                                           | (e,_) :: _ ->
                                               (e,List.map (fun (_,fin) -> (start,fin)) ts)
                                   in
                                   {empty_raw_instructions with
                                    raw_constrain_rels =
                                    [ConsiderRel rel];} }
  | IGNORE transition_sequence { let rel =
                                           let (start,ts) = $2 in
                                           match ts with
                                           | [] -> parse_error "not ignoring blank constraint"
                                           | (e,_) :: _ ->
                                               (e,List.map (fun (_,fin) -> (start,fin)) ts)
                                   in
                                   {empty_raw_instructions with
                                    raw_constrain_rels =
                                    [IgnoreRel rel];} }
  | IGNORE prefixed_transition_sequence { let rel =
                                           let (start,ts) = $2 in
                                           match ts with
                                           | [] -> parse_error "not ignoring blank constraint"
                                           | (e,_) :: _ ->
                                               (e,List.map (fun (_,fin) -> (start,fin)) ts)
                                   in
                                   {empty_raw_instructions with
                                    raw_constrain_rels =
                                    [IgnoreRel rel];} }
  | ADD transition_sequence    { {empty_raw_instructions with
                                     raw_add_rels = [$2]; } }
  | REMOVE transition_sequence    { {empty_raw_instructions with
                                        raw_remove_rels = [$2]; } }
  | ADD prefixed_transition_sequence    { {empty_raw_instructions with
                                              raw_add_rels = [$2]; } }
  | REMOVE prefixed_transition_sequence    { {empty_raw_instructions with
                                                 raw_remove_rels = [$2]; } }
  | ADD action_or_name_list    { {empty_raw_instructions with
                                     raw_add_actions = $2; } }
  | REMOVE action_or_name_list    { {empty_raw_instructions with
                                        raw_remove_actions = $2; } }
  | modes                 { {empty_raw_instructions with
                                     raw_mode = $1} }
  | IGNORE checks
      { {empty_raw_instructions with
	 raw_checks = (List.map (fun c -> IgnoreCheck c) $2); } }
  | CONSIDER checks
      { {empty_raw_instructions with
	 raw_checks = (List.map (fun c -> ConsiderCheck c) $2); } }
  | DISPLAY_ACTION action_or_name_list  { {empty_raw_instructions with
                                       raw_node_instructions = [Raw_display_n $2]; } }
  | SUPPRESS_ACTION action_or_name_list  { {empty_raw_instructions with
                                        raw_node_instructions = [Raw_suppress_n $2]; } }
  | DISPLAY_EDGE edges                 { {empty_raw_instructions with
                                        raw_edge_instructions = [Raw_display_e  (List.map (fun x -> (x,None)) $2)];} }
  | SUPPRESS_EDGE edges                 { {empty_raw_instructions with
                                        raw_edge_instructions = [Raw_suppress_e (List.map (fun x -> (x,None)) $2)];} }
  | DISPLAY_EDGE prefixed_transition_sequence { let (start,ts) = $2 in
                                           let e =
                                             match ts with
                                             | (e,_) :: _ -> e
                                             | [] -> parse_error "Cannot happen: prefixed_transition_sequence w/o prefix"
                                           in
                                           {empty_raw_instructions with
                                             raw_edge_instructions = [Raw_display_e [e,Some ($2)]];}}
  | SUPPRESS_EDGE prefixed_transition_sequence { let (start,ts) = $2 in
                                           let e =
                                             match ts with
                                             | (e,_) :: _ -> e
                                             | [] -> parse_error "Cannot happen: prefixed_transition_sequence w/o prefix"
                                           in
                                           {empty_raw_instructions with
                                             raw_edge_instructions = [Raw_suppress_e [e,Some ($2)]];}}


modes :
    mode                   { [$1] }
  | mode modes               { $1 :: $2 }

mode :
    SET FONTSIZE EQUALS IDENT  { Fontsize (try int_of_string $4 with Invalid_argument _ -> parse_error "fontsize must be an int" )}
  | SET NODE_HEIGHT EQUALS FILENAME { Node_height (try float_of_string $4 with Invalid_argument _ -> parse_error "node_height must be a float")}
  | SET NODE_WIDTH EQUALS FILENAME { Node_width (try float_of_string $4 with Invalid_argument _ -> parse_error "node_width must be a float") }
  | SET FILLED EQUALS BOOL { Filled $4 }
  | SET XSCALE EQUALS FILENAME { Xscale (try float_of_string $4 with Invalid_argument _ -> parse_error "xscale must be a float") }
  | SET YSCALE EQUALS FILENAME { Yscale (try float_of_string $4 with Invalid_argument _ -> parse_error "yscale must be a float") }
  | SET RANKSEP EQUALS FILENAME { Ranksep (try float_of_string $4 with Invalid_argument _ -> parse_error "ranksep must be a float") }
  | SET NODESEP EQUALS FILENAME { Nodesep (try float_of_string $4 with Invalid_argument _ -> parse_error "nodesep must be a float") }
  | SET PENWIDTH EQUALS FILENAME { Penwidth (try float_of_string $4 with Invalid_argument _ -> parse_error "penwidth must be a float") }
  | SET LEGEND EQUALS STRING { Legend (if $4="" then None else Some $4) }
  | SET LAYOUT EQUALS DOTFILE { Layout LO_dot }
  | SET LAYOUT EQUALS NEATO_PAR { Layout LO_neato_par }
  | SET LAYOUT EQUALS NEATO_PAR_INIT { Layout LO_neato_par_init }
  | SET LAYOUT EQUALS NEATO_DOWNWARDS { Layout LO_neato_downwards }
  | SET TEX EQUALS BOOL { Texmode $4 }
  | SET THREAD_IDS EQUALS BOOL { Thread_ids $4 }


command :
      QUIT                        { Quit }
  | NEXT                          { Continue }
  | NONSTOP                       { StopAt Never }
  | INTERMEDIATE                  { StopAt Always }
  | NEXTCANDIDATE                 { StopAt OnCandidates }
  | NEXTCONSISTENT                { StopAt OnSolutions }
  | HELP                          { Help }
  | RELABEL                       { Relabel }
  | GENERATE DOTFILE FILENAME     { Generate (Dot,$3) }
  | GENERATE ISAFILE FILENAME     { Generate (Isa,$3) }
  | GENERATE TEX FILENAME     { Generate (Tex,$3) }
  | GENERATE EXCFILE FILENAME     { Generate (Exc,$3) }
  | GENERATE INSTRFILE FILENAME   { Generate (Instructions,$3) }

items:
   item items                     { $1 :: $2 }
 | EOF                            { [] }

item:
   MODEL COLON IDENT { Model $3 }
 | transition_sequence            { Trans $1 }
 | action_or_name                 { Trans ($1,[]) }
 | prefixed_transition_sequence   { Trans $1 }
 | LOCATION_KINDS COLON lk              { Lk $3 }
 | output_edge_inst                    { DisplayEdgeInstr $1 }
 | suppress_edge_inst                  { SuppressEdgeInstr $1 }
 | DISPLAY_ACTION action_or_name_list DOT     { DisplayNodeInstr $2 }
 | SUPPRESS_ACTION action_or_name_list DOT    { SuppressNodeInstr $2 }
 | IGNORE checks                       { Ignore $2 }
 | SHOW IDENT DOT                      { let i =
                                            try int_of_string $2
                                            with Failure "int_of_string" -> parse_error ("Show expects a number, not " ^ $2) in
                                         Show (Some i) }
 | SHOW ALL DOT                        { Show None }

checks:
   IDENT /* singleton */         { [$1] }
 | IDENT checks                  { $1 :: $2 }

output_edge_inst:
   DISPLAY_EDGE edges from_opt to_opt DOT              { List.map (fun e -> (e,RawCrossProduct($3,$4))) $2 }
 | DISPLAY_EDGE edges COLON action_edges DOT           { List.map (fun e -> (e,RawExact $4)) $2 }

suppress_edge_inst:
   SUPPRESS_EDGE edges from_opt to_opt DOT              { List.map (fun e -> (e,RawCrossProduct($3,$4))) $2 }
 | SUPPRESS_EDGE edges COLON action_edges DOT           { List.map (fun e -> (e,RawExact $4)) $2 }

action_edges:
   action_edge                 { [$1] }
 | action_edge action_edges    { $1 :: $2 }

action_edge:
   action_or_name EDGEHEAD action_or_name { ($1,$3) }

edgename:
    MO                        { match $1 with | Atomic Cmm.Seq_cst -> "sc" | _ -> parse_error "non-\"sc\" memory order appearing in relation name position"}
 |  IDENT                      { $1 }

edges:
   edgename /* singleton */         { [$1] }
 | edgename edges                  { $1 :: $2 }


from_opt:
   /* Nothing */                       { All }
 | FROM action_or_name_list            { Actions $2 }
 | FROM ALL                            { All }

to_opt:
   /* Nothing */                       { All }
 | TO action_or_name_list              { Actions $2 }
 | TO ALL                              { All }

action_or_name_list:
   action_or_name                      { [$1] }
 | action_or_name action_or_name_list  { $1 :: $2 }


transition_sequence:
     action_or_name EDGETAIL edgename EDGEHEAD action_or_name { ($1,[($3,$5)]) }
  |  transition_sequence EDGETAIL edgename EDGEHEAD action_or_name   { let (start,trans)= $1 in (start,trans@[($3,$5)]) }

prefixed_transition_sequence:
     edgename  COLON prefixed_transition_sequence_body  { let (start,trans)=$3 in (start, List.map (function x -> ($1,x)) trans) }

prefixed_transition_sequence_body:
    action_or_name { ($1,[]) }
  | prefixed_transition_sequence_body EDGEHEAD  action_or_name  { let (start,trans) = $1 in (start,trans@[$3]) }




action_or_name:
    IDENT  { Action_name $1 }
  | action { Action $1 }

action:
    IDENT SEMICOLON IDENT COLON L  location LOCKOUTCOME { Cmm.Lock($1,named_thread_id $3, $6, $7) }
  | IDENT SEMICOLON IDENT COLON U  location { Cmm.Unlock($1,named_thread_id $3,$6) }
  | IDENT SEMICOLON IDENT COLON R location  EQUALS  valu
      { match $5 with
      | Nonatomic -> Cmm.Load ($1,named_thread_id $3,Cmm.NA,$6,$8)
      | Atomic mo -> Cmm.Load ($1,named_thread_id $3,mo,$6,$8)
      | Mutex -> raise (Failure "parse error: R with mutex type")}
  | IDENT SEMICOLON IDENT COLON W location  EQUALS  valu
      { match $5 with
      | Nonatomic -> Cmm.Store ($1,named_thread_id $3,Cmm.NA,$6,$8)
      | Atomic mo -> Cmm.Store ($1,named_thread_id $3,mo,$6,$8)
      | Mutex -> raise (Failure "parse error: W with mutex type")}

  | IDENT SEMICOLON IDENT COLON RMW location  EQUALS  valu SLASH valu
      { match $5 with
      | Nonatomic -> raise (Failure "parse error: RMW with nonatomic order type")
      | Atomic mo -> Cmm.RMW ($1,named_thread_id $3,mo,$6,$8,$10)
      | Mutex -> raise (Failure "parse error: RMW with mutex type") }
  | IDENT SEMICOLON IDENT COLON RMW location
      { (* TODO: jp: have an actual rule to parse blocked RMWs *)
        match $5 with
        | Nonatomic -> raise (Failure "parse error: BRMW with nonatomic order type")
        | Atomic mo -> Cmm.Blocked_rmw ($1,named_thread_id $3,$6)
        | Mutex -> raise (Failure "parse error: BRMW with mutex type") }
  | IDENT SEMICOLON IDENT COLON F
      { match $5 with
      | Nonatomic -> raise (Failure "parse error: F with nonatomic order type")
      | Atomic mo -> Cmm.Fence ($1,named_thread_id $3,mo)
      | Mutex -> raise (Failure "parse error: F with mutex type") }

location:
   IDENT  { Cmm.Rigid (Cmm.Symbolic $1) }

valu:
   IDENT  { Cmm.Rigid (Cmm.Symbolic $1) }




lk:
    LBRACE lk_body RBRACE { $2 }
  | LBRACE RBRACE { [] }

lk_body:
    location COLON location_kind                  {      [($1,$3)] }
  | lk_body COMMA location COLON location_kind    { $1 @ [($3,$5)] }

location_kind:
    ATOMIC    { Cmm.Atomic }
  | NONATOMIC { Cmm.Non_Atomic }
  | MUTEX     { Cmm.Mutex }








skeletonexecfile:
   skeleton_items { $1 }

skeleton_items:
   skeleton_item skeleton_items                     { $1 :: $2 }
 | EOF                            { [] }

skeleton_item:
   skeleton_transition_sequence            { S_Trans $1 }
 | skeleton_action_or_name                 { S_Trans ($1,[]) }
 | skeleton_prefixed_transition_sequence   { S_Trans $1 }
 | SAMELOC COLON skeleton_action_set_set   { S_Sameloc $3 }
 | ATOMICLOCS COLON skeleton_action_set    { S_Atomiclocs $3 }


skeleton_transition_sequence:
     skeleton_action_or_name EDGETAIL edgename EDGEHEAD skeleton_action_or_name { ($1,[($3,$5)]) }
  |  skeleton_transition_sequence EDGETAIL edgename EDGEHEAD skeleton_action_or_name   { let (start,trans)= $1 in (start,trans@[($3,$5)]) }

skeleton_prefixed_transition_sequence:
     edgename  COLON skeleton_prefixed_transition_sequence_body  { let (start,trans)=$3 in (start, List.map (function x -> ($1,x)) trans) }

skeleton_prefixed_transition_sequence_body:
    skeleton_action_or_name { ($1,[]) }
  | skeleton_prefixed_transition_sequence_body EDGEHEAD  skeleton_action_or_name  { let (start,trans) = $1 in (start,trans@[$3]) }



skeleton_action_set_set:
    LBRACE skeleton_action_set_set_body RBRACE { $2 }
  | LBRACE RBRACE { [] }

skeleton_action_set_set_body:
    skeleton_action_set { [$1] }
  | skeleton_action_set_set_body COMMA  skeleton_action_set  { $1 @ [$3] }

skeleton_action_set:
    LBRACE skeleton_action_set_body RBRACE { $2 }
  | LBRACE RBRACE { [] }

skeleton_action_set_body:
    skeleton_action_or_name { [$1] }
  | skeleton_action_set_body COMMA  skeleton_action_or_name  { $1 @ [$3] }

skeleton_action_or_name:
    IDENT  { S_Action_name $1 }
  | skeleton_action { S_Action $1 }

skeleton_action:
    IDENT SEMICOLON IDENT COLON L  { S_Lock($1,named_thread_id $3) }
  | IDENT SEMICOLON IDENT COLON U  { S_Unlock($1,named_thread_id $3) }
  | IDENT SEMICOLON IDENT COLON RSKEL skeleton_mos { S_Read($1,named_thread_id $3,$6) }
  | IDENT SEMICOLON IDENT COLON WSKEL skeleton_mos { S_Write($1,named_thread_id $3,$6) }
  | IDENT SEMICOLON IDENT COLON RMWSKEL skeleton_mos { S_Atomic_rmw($1,named_thread_id $3,$6) }
  | IDENT SEMICOLON IDENT COLON FSKEL skeleton_mos { S_Fence($1,named_thread_id $3,$6) }

skeleton_mos:
    STAR  { MO_Star }
  | ATOMIC { MO_Atomic }
  | LBRACE skeleton_mo_set_body RBRACE { MO_Set $2 }

skeleton_mo_set_body:
    MO { [$1] }
  | skeleton_mo_set_body COMMA  MO { $1 @ [$3] }

