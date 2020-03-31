
%{ (*code goes here*)

open ParserTypes

exception Heck of string

let star (ident,p1,p2) (t1,t2) = (ident^"*",p1,t2)

let make_node_decl name n =
	{ detached = false
	; abstract = true
	; ismutable = false
	; externalnode = false
	; nodename=name
	; nodefunction = (strip_position name)
	; inputs = (match n.outputs with
			(Some o) -> o
			| _ -> raise (Heck "make_node_decl expects source to have declared outputs"))
	; guardrefs = []
	; outputs = None }


type general_formal =
	Typed of decl_formal
	| GuardRef of
                ( string ParserTypes.positioned
                * string list list
                * ParserTypes.guardmod list
                * string ParserTypes.positioned option
                * string list )

let rec comma_separate ll =
	match ll with
		(h::(_::_ as t)) -> (h @ [ "," ] @ (comma_separate t))
		| _ -> List.concat ll

let rec break_dotted_name nsn =
        try let ind = String.index nsn '.' in
            let len = String.length nsn in
            let hstr = String.sub nsn 0 ind in
            let tstr = String.sub nsn (ind+1) (len-(ind+1))
            in  hstr::"."::(break_dotted_name tstr)
        with Not_found -> [nsn]


%}
%token ENDOFFILE
%token <ParserTypes.position*ParserTypes.position> ATOMIC, PRECEDENCE;
%token <ParserTypes.position*ParserTypes.position> DETACHED, ABSTRACT, MUTABLE;
%token <ParserTypes.position*ParserTypes.position> ARROW, STAR, EXCLAMATION;
%token <ParserTypes.position*ParserTypes.position> LEFT_CR_BRACE, RIGHT_CR_BRACE;
%token <ParserTypes.position*ParserTypes.position> PIPE, COLON, COMMA, EQUALS, SEMI;
%token <ParserTypes.position*ParserTypes.position> LEFT_PAREN, RIGHT_PAREN, LEFT_SQ_BRACE, RIGHT_SQ_BRACE;
%token <ParserTypes.position*ParserTypes.position> TYPEDEF, SOURCE, TERMINATE, INITIAL, DOOR;
%token <ParserTypes.position*ParserTypes.position> PLUS, QUESTION, DOUBLECOLON, DOT;
%token <ParserTypes.position*ParserTypes.position> MINUS, ELLIPSIS;
%token <ParserTypes.position*ParserTypes.position> LESSTHAN, GREATERTHAN, AMPERSAND;
%token <ParserTypes.position*ParserTypes.position> AMPERSANDEQUALS;
%token <int*ParserTypes.position*ParserTypes.position> NUMBER;
%token <string*ParserTypes.position*ParserTypes.position> IDENTIFIER;
%token <ParserTypes.position*ParserTypes.position> BOOL, CONST, GUARD;
%token <ParserTypes.position*ParserTypes.position> CONDITION, NODE, EXCLUSIVE;
%token <ParserTypes.position*ParserTypes.position> SEQUENCE, POOL, READWRITE, FREE;
%token <ParserTypes.position*ParserTypes.position> HANDLE, ERROR, AS, WHERE;
%token <ParserTypes.position*ParserTypes.position> READ, WRITE, SLASH;
%token <ParserTypes.position*ParserTypes.position> UPGRADEABLE, NULLOK;
%token <ParserTypes.position*ParserTypes.position> MODULE, BEGIN, END;
%token <ParserTypes.position*ParserTypes.position> PLUGIN, EXTERNAL, DEPENDS;
%token <ParserTypes.position*ParserTypes.position> INSTANCE, IF, STATIC;
%token <ParserTypes.position*ParserTypes.position> UNORDERED, GC;
%token <ParserTypes.position*ParserTypes.position> BACKARROW, NOTEQUALS, ISEQUALS;
%token <ParserTypes.position*ParserTypes.position> DOUBLEAMPERSAND;
%token <ParserTypes.position*ParserTypes.position> DOUBLEBAR;
%token <string> INCLUDE;
%left PLUS
%left STAR
%right ARROW
%right PIPE
%start top_level_program
%type <ParserTypes.program> top_level_program
%type <ParserTypes.program> program
%%
/*script begin*/
top_level_program:
	program ENDOFFILE
	{ $1 }

program:
	code_list
		{ let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1
		  in { cond_decl_list=List.rev cl
		     ; atom_decl_list=List.rev al
		     ; node_decl_list=List.rev nl
		     ; mainfun_list=List.rev sl
		     ; expr_list=List.rev el
		     ; err_list=List.rev erl
		     ; mod_def_list=List.rev mdl
		     ; mod_inst_list=List.rev mil
                     ; plugin_list=List.rev pl
		     ; plugin_depend_list=List.rev pdl
                     ; terminate_list=List.rev tl
                     ; order_decl_list=List.rev odl
		     } }
;

/*** Code: sources, abstract nodes and error handlers ***/

code_list:
	code_list main_fn
	{ trace_thing "code_list";
	  let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1 in
	  let findsrc s =
		try List.find (fun n -> (strip_position s.sourcename) = (strip_position n.nodename)) nl
		with Not_found -> raise (Heck ("could not find source declaration before definition for "^(strip_position s.sourcename)))
	  in  match $2 with
		(s,None) -> (s::sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl)
	       |(s,Some e) -> (s::sl,e::el,erl,mil,cl,al,(make_node_decl e.exprname (findsrc s))::nl,mdl,pl,pdl,tl,odl) }
	| code_list expr_part
	{ trace_thing "code_list";
	  let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1
	  in  (sl,$2::el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl) }
	| code_list err_def
	{ trace_thing "code_list";
	  let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1
	  in  (sl,el,$2::erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl) }
	| code_list mod_inst
	{ trace_thing "code_list";
	  let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1
	  in  (sl,el,erl,$2::mil,cl,al,nl,mdl,pl,pdl,tl,odl) }
	| code_list node_decl
	{ trace_thing "code_list";
          let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1
          in sl,el,erl,mil,cl,al,$2::nl,mdl,pl,pdl,tl,odl }
	| code_list atom_decl
	{ trace_thing "code_list";
          let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1
	  in sl,el,erl,mil,cl,$2::al,nl,mdl,pl,pdl,tl,odl }
	| code_list cond_decl
	{ trace_thing "code_list";
          let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1
	  in sl,el,erl,mil,$2::cl,al,nl,mdl,pl,pdl,tl,odl }
	| code_list mod_decl
	{ trace_thing "code_list";
          let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1
	  in sl,el,erl,mil,cl,al,nl,$2::mdl,pl,pdl,tl,odl }
	| code_list plugin_decl
	{ trace_thing "code_list";
          let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1
	  in sl,el,erl,mil,cl,al,nl,mdl,$2::pl,pdl,tl,odl }
	| code_list plugin_depends_decl
	{ trace_thing "code_list";
          let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1
	  in sl,el,erl,mil,cl,al,nl,mdl,pl,$2::pdl,tl,odl }
	| code_list term_decl
	{ trace_thing "code_list";
          let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1
	  in  sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,$2::tl,odl }
	| code_list guard_order_decl
	{ trace_thing "code_list";
          let sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,odl = $1
	  in  sl,el,erl,mil,cl,al,nl,mdl,pl,pdl,tl,$2 @ odl }
	| /*epsilon*/
	{ trace_thing "code_list";
          [],[],[],[],[],[],[],[],[],[],[],[] }

/*** modules ***/

mod_inst:
	external_opt INSTANCE namespaced_ident IDENTIFIER where_guard_indentifications_opt SEMI
	{ trace_thing "mod_inst"
        ; { externalinst=$1; modsourcename=$3; modinstname=$4; guardaliases = $5 } }

where_guard_indentifications_opt:
        WHERE guard_indentification_list
        { $2 }
        | /*epsilon*/
        { [] }

guard_indentification_list:
        guard_identification
        { [ $1 ] }
        | guard_identification COMMA guard_indentification_list
        { $1::$3 }

guard_identification:
        IDENTIFIER EQUALS IDENTIFIER
        { $1, $3 }


mod_decl:
	mod_modifier_list MODULE namespaced_ident BEGIN program END
	{ trace_thing "mod_decl";
	  { modulename=$3
	  ; isstaticmodule=List.mem "static" $1
	  ; programdef=$5 } }

mod_modifier_list:
	/* nothing */
	{ [] }
	| mod_modifier mod_modifier_list
	{ $1::$2 }

mod_modifier:
	STATIC
	{ "static" }



/*** plugins ***/

plugin_depends_decl:
	DEPENDS namespaced_ident SEMI
	{ trace_thing "plugin_depends_def"; $2 }

plugin_decl:
        PLUGIN namespaced_ident BEGIN program END
        { trace_thing "plugin_def"; { pluginname=$2; pluginprogramdef=$4 } }


/*** namespaced identifier ***/

namespaced_ident:
	IDENTIFIER
	{ trace_thing "namespaced_ident"; $1 }
	| IDENTIFIER DOUBLECOLON namespaced_ident
	{ let i,ip1,ip2 = $1 in
	  let d,dp1,dp2 = $3
	  in  trace_thing "namespaced_ident"; (i^"::"^d,ip1,dp2) }

term_decl:
        TERMINATE namespaced_ident SEMI
        { $2 }

source_or_initial_or_door:
        SOURCE
        { 0 }
        | INITIAL
        { 1 }
        | DOOR
        { 2 }

main_fn:
	source_or_initial_or_door namespaced_ident SEMI
	{ trace_thing "main_fn";
	  ({ sourcename =$2
          ; sourcefunction=strip_position $2
          ; successor=None
          ; runoncetype=$1
          }, None) }
	|
	source_or_initial_or_door namespaced_ident PIPE ident_list SEMI
	{ trace_thing "main_fn";
          let rhs_name =
		let n,p1,p2 = $2
		in  (((n^"_RHS"),p1,p2) : string positioned)
	  in
	  ( { sourcename=$2
            ; sourcefunction=strip_position $2
            ; successor=(Some rhs_name)
            ; runoncetype=$1
            }
	  , Some
	    { exprname = rhs_name
	    ; condbinding=[]
	    ; successors = $4
	    ; etype = Choice
	    }
	  ) }
;

/*** Declarations ***/


/*** Conditions ***/

cond_decl:
	external_opt CONDITION namespaced_ident simple_arg_list ARROW BOOL SEMI
	{ trace_thing "cond_decl";
	  { externalcond=$1; condname=$3; condfunction=strip_position $3; condinputs=$4 } }

/*** Atomics ***/

atom_type:
	EXCLUSIVE
	{ "exclusive" }
	| READWRITE
	{ "readwrite" }
	| SEQUENCE
	{ "sequence" }
	| POOL
	{ "pool" }
	| FREE
	{ "free" }

atom_mod:
        UNORDERED
        { "unordered" }
        | GC
        { "gc" }

atom_mod_opt_list:
        /*epsilon*/
        { [] }
        | SLASH atom_mod atom_mod_opt_list
        { $2::$3 }

atom_decl:
	external_opt atom_type atom_mod_opt_list namespaced_ident simple_arg_list ARROW data_type SEMI
	{ trace_thing "atom_decl";
	  { externalatom=$1
          ; atomname=$4
          ; atominputs=$5
          ; outputtype=$7
          ; atomtype = $2
          ; atommodifiers = $3
          } }

external_opt:
        EXTERNAL
        { true }
        | /* epsilon */
        { false }


guard_order_decl:
        PRECEDENCE guard_order_list SEMI
        { $2 }

guard_order_list:
        ident LESSTHAN ident
        { [($1,$3)] }
        | ident LESSTHAN guard_order_list
        { let rhs = match $3 with
                        ((l,_)::_) -> l
                        | _ -> raise Not_found
          in  ($1,rhs)::$3 }



/*** Nodes ***/

node_target_arg_list:
	ELLIPSIS
	{ None }
	| simple_arg_list
	{ Some $1 }

node_decl:
	external_opt NODE node_mod_list namespaced_ident arg_list ARROW node_target_arg_list SEMI
	{ trace_thing "node_decl";
          let clean_cpp_uninterpreted args grefs =
                let strargs = List.map (fun x -> strip_position x.name) args in
                let strgargs = List.map
			(fun (x,_,_,xo,_) ->
				strip_position (match xo with
							(Some x') -> x'
							| _ -> x)) grefs in
		let strgargs = "self"::strgargs in
                let rec on_str_list' firstisarg firstisgarg strl =
                        match strl with
                                (h1::h2::tl) ->
                                        let h2isarg = List.mem h2 strargs in
					let h2isgarg = (not h2isarg) &&
						(List.mem h2 strgargs)
                                        in  if firstisarg || firstisgarg || h2isarg || h2isgarg then
                                                (if firstisarg
                                                 then Arg h1
                                                 else if firstisgarg
						 then GArg h1
						 else Context h1)
                                                ::(on_str_list' h2isarg h2isgarg (h2::tl))
                                            else on_str_list' false false ((h1^h2)::tl)
                                | [] -> []
                                | [x] -> [if firstisarg
                                          then Arg x
                                          else if firstisgarg
					  then GArg x
					  else Context x]
                        in
                let on_str_list strl =
                        match on_str_list' false false (""::strl) with
                                ((Context "")::tl) -> tl
                                | res -> res
                in  on_str_list in
	  let filt_type item =
		match item with
			Typed i -> true
			| _ -> false in
	  let ins, grefs = List.partition filt_type $5 in
	  let filttyped x =(match x with Typed i -> i | _ -> raise Not_found) in
	  let filtguardref x =(match x with GuardRef i -> i | _ -> raise Not_found) in
	  let ins = List.map filttyped ins in
	  let grefs = List.map filtguardref grefs in
          let grefs =
		let clean_cpp_un x = clean_cpp_uninterpreted ins grefs x
		in  List.map (fun (guardname,arguments,modifiers,localgname,guardcond) ->
                        { guardname = guardname
                        ; arguments = List.map clean_cpp_un arguments
                        ; modifiers = modifiers
                        ; localgname = localgname
                        ; guardcond = clean_cpp_un guardcond
                        }) grefs in
	  let is_mut = (List.mem "mutable" $3) in
	  let is_abs = (List.mem "abstract" $3) ||
	  	(match $7 with None -> true | _ -> false)
	  in
	  { detached=List.mem "detached" $3
          ; abstract=is_abs
          ; ismutable=is_mut
          ; externalnode=$1
          ; nodename=$4
          ; nodefunction=(strip_position $4)
          ; inputs=ins
          ; guardrefs=grefs
          ; outputs= $7 }
	}
;

node_mod_list:
	{ trace_thing "node_mod_list"; [] }
	| DETACHED node_mod_list
	{ trace_thing "node_mod_list"; "detached"::$2 }
	| ABSTRACT node_mod_list
	{ trace_thing "node_mod_list"; "abstract"::$2 }
	| MUTABLE node_mod_list
	{ trace_thing "node_mod_list"; "mutable"::$2 }

/*** Argument Lists ***/

simple_arg_list:
	LEFT_PAREN typed_item_list RIGHT_PAREN
	{ $2 }
	| LEFT_PAREN RIGHT_PAREN
	{ [] }
;

typed_item_list:
	typed_item_list COMMA typed_item
	{ $1 @ [$3] }
	| typed_item
	{ [ $1 ] }
;

/*
typed_item:
	IDENTIFIER
	{ let _,p1,p2 = $1
          in (("",p1,p1), $1, ("",p2,p2)) }
	| IDENTIFIER STAR
	{ let (_,p1,p2) as middle = star $1 $2
          in (("",p1,p1), middle, ("",p2,p2)) }
	| IDENTIFIER IDENTIFIER
	{ let _,p1,_ = $1
	  in  (("",p1,p1),$1,$2) }
	| IDENTIFIER STAR IDENTIFIER
	{ let (_,p1,_) as middle = star $1 $2
	  in  (("",p1,p1),middle,$3) }
	| IDENTIFIER IDENTIFIER STAR
	{ let (_,_,p2) as middle = star $2 $3
          in ($1,middle,("",p2,p2)) }
	| IDENTIFIER IDENTIFIER IDENTIFIER
	{ ($1,$2,$3) }
	| IDENTIFIER IDENTIFIER STAR IDENTIFIER
	{ ($1,star $2 $3,$4) }
;
*/

data_type:
	type_mod_opt raw_data_type star_list
	{ let stars, pos2 = $3 in
	  let rt,pos1,_ = $2 in
	  if pos2 = noposition then { dctypemod=$1; dctype=$2 }
	  else { dctypemod=$1; dctype=(rt^stars,pos1,pos2) }
	}
;

star_list:
	star_list STAR
	{ let ses,_ = $1 in
	  let _,pend = $2 in
	  ses^"*", pend
	}
	| star_list AMPERSAND
	{ let ses,_ = $1 in
	  let _,pend = $2 in
	  ses^"&", pend
	}
	| /*epsilon*/
	{ ("",noposition) }

type_mod_opt:
	CONST
	{ let p1,p2 = $1 in ("const",p1,p2) }
	| /*epsilon*/
	{ ("",noposition,noposition) }
;

template_item_list:
	template_item_list COMMA template_item
	{ let (a,pa1,pa2) = $1 in
	  let (b,pb1,pb2) = $3
	  in  (a^","^b,pa1,pb2) }
	| template_item
	{ $1 }
;

template_item:
	NUMBER
	{ let n,p1,p2 = $1 in (string_of_int n),p1,p2 }
	| data_type
	{ let (a,pa1,pa2),(b,pb1,pb2) = $1.dctypemod, $1.dctype
	  in  (a^b,pa1,pb2) }
;

raw_data_type:
	BOOL
	{ let p1,p2 = $1 in ("bool",p1,p2) }
	| IDENTIFIER
	{ $1 }
	| LEFT_PAREN data_type RIGHT_PAREN
	{ let (a,p1,_),(b,_,p2) = $2.dctypemod, $2.dctype
	  in (a^b,p1,p2) }
	| IDENTIFIER DOUBLECOLON raw_data_type
	{ let i,ip1,ip2 = $1 in
	  let d,dp1,dp2 = $3
	  in  (i^"::"^d,ip1,dp2) }
	| IDENTIFIER LESSTHAN template_item_list GREATERTHAN
	{ let i,ip1,_ = $1 in
	  let t,_,_ = $3 in
	  let _, pend = $4
	  in  (i^"<"^t^">",ip1,pend) }
;

typed_item:
	data_type
	{ let _,_,pend = $1.dctype in
	  { ctypemod=$1.dctypemod; ctype=$1.dctype; name=("",pend,pend) }
	}
	| data_type IDENTIFIER
	{ { ctypemod=$1.dctypemod; ctype=$1.dctype; name=$2 } }
;

arg_list:
	LEFT_PAREN RIGHT_PAREN
	{ trace_thing "arg_list"; [] }
	| LEFT_PAREN typed_or_guardref_item_list RIGHT_PAREN
	{ trace_thing "arg_list"; $2 }
;

typed_or_guardref_item_list:
	typed_or_guardref_item_list COMMA typed_or_guardref_item
	{ trace_thing "typed_or_guardref_item_list"; $1 @ [ $3 ] }
	| typed_or_guardref_item
	{ trace_thing "typed_or_guardref_item_list"; [ $1 ] }
;

typed_or_guardref_item:
	typed_item
	{ trace_thing "typed_or_guardref_item"; Typed $1 }
	| guardref_item
	{ trace_thing "typed_or_guardref_item"; GuardRef $1 }
;

guardref_item:
	GUARD IDENTIFIER guardref_modifier_list LEFT_PAREN uninterpreted_cpp_code_comma_list RIGHT_PAREN as_named_opt if_condition_opt
	{ trace_thing "guardref_item";
	  ( (*guardname= *) $2
          , (*arguments= *) $5
          , (*modifiers= *) $3
          , (*localgname= *) $7
          , (*guardcond= *) $8 ) }
;

guardref_modifier_list:
	SLASH READ guardref_modifier_list
	{ trace_thing "guardref_modifier_list"; (Read::$3) }
	| SLASH WRITE guardref_modifier_list
	{ trace_thing "guardref_modifier_list"; (Write::$3) }
	| SLASH UPGRADEABLE guardref_modifier_list
	{ trace_thing "guardref_modifier_list"; (Upgradeable::$3) }
	| SLASH NULLOK guardref_modifier_list
	{ trace_thing "guardref_modifier_list"; (NullOk::$3) }
	| /*epsilon*/
	{ trace_thing "guardref_modifier_list"; [] }

as_named_opt:
	AS IDENTIFIER
	{ Some $2 }
	| /*epsilon*/
	{ None }

if_condition_opt:
        /*epsilon*/
        { [] }
        | IF uninterpreted_cpp_code
        { $2 }

/*** C++ stuff ***/

uninterpreted_cpp_code:
        /*epsilon*/
        { [] }
        | uninterpreted_cpp_code_fragment uninterpreted_cpp_code
        { $1 @ $2 }

uninterpreted_cpp_code_fragment:
        NUMBER
        { let n,_,_ = $1 in [string_of_int n] }
        | ident
        { let s,_,_ = $1 in break_dotted_name s }
        | DOUBLECOLON uninterpreted_cpp_code_fragment
        { "::"::$2 }
        | LESSTHAN uninterpreted_cpp_code_fragment
        { (" < "::$2) }
        | GREATERTHAN uninterpreted_cpp_code_fragment
        { (" > "::$2) }
        | ARROW uninterpreted_cpp_code_fragment
        { (" => "::$2) }
        | BACKARROW uninterpreted_cpp_code_fragment
        { (" <= "::$2) }
        | ISEQUALS uninterpreted_cpp_code_fragment
        { (" == "::$2) }
        | NOTEQUALS uninterpreted_cpp_code_fragment
        { (" != "::$2) }
        | DOUBLEAMPERSAND uninterpreted_cpp_code_fragment
        { (" && "::$2) }
        | DOUBLEBAR uninterpreted_cpp_code_fragment
        { (" || "::$2) }
        | LEFT_PAREN uninterpreted_cpp_code_comma_list RIGHT_PAREN
        { ("("::(comma_separate $2)) @ [")"] }
        | LEFT_SQ_BRACE uninterpreted_cpp_code RIGHT_SQ_BRACE
	{ ("["::$2) @ [ "]" ] }
        | EXCLAMATION uninterpreted_cpp_code_fragment
	{ "!"::$2 }
        | PIPE uninterpreted_cpp_code_fragment
        { "->"::$2 }
        | STAR uninterpreted_cpp_code_fragment
        { "*"::$2 }
        | PLUS uninterpreted_cpp_code_fragment
        { "+"::$2 }
        | MINUS uninterpreted_cpp_code_fragment
        { "-"::$2 }
        | QUESTION uninterpreted_cpp_code_fragment
        { ("?"::$2) }
        | COLON uninterpreted_cpp_code_fragment
        { (":"::$2) }
        | DOT uninterpreted_cpp_code_fragment
        { ("."::$2) }

uninterpreted_cpp_code_comma_list:
        uninterpreted_cpp_code
        { match $1 with
                [] -> []
                | _ -> [$1] }
        | uninterpreted_cpp_code COMMA uninterpreted_cpp_code_comma_list
        { $1::$3 }

/*** Expressions ***/

expr_part:
	assn SEMI
	{ $1 }
;

assn:
	IDENTIFIER EQUALS ident_list
	{ { exprname=$1; condbinding=[]; successors=$3; etype=Choice } }
	| IDENTIFIER COLON LEFT_SQ_BRACE comma_list RIGHT_SQ_BRACE EQUALS ident_list
	{ { exprname=$1; condbinding=$4; successors=$7; etype=Choice } }
        | IDENTIFIER EQUALS namespaced_ident AMPERSAND par_ident_list
        { { exprname=$1; condbinding=[]; successors=$3::$5; etype=Concurrent } }
        | IDENTIFIER AMPERSANDEQUALS namespaced_ident
        { let id,p1,p2 = $1
          in { exprname=("&"^id,p1,p2); condbinding=[]; successors=[$3]; etype=Concurrent } }
;

par_ident_list:
	par_ident_list AMPERSAND namespaced_ident
	{ $1 @ [ $3 ] }
	| namespaced_ident
	{ [ $1 ] }
;

ident_list:
	ident_list PIPE namespaced_ident
	{ $1 @ [ $3 ] }
	| namespaced_ident
	{ [ $1 ] }
	| /*epsilon*/
	{ [] }
;


ident:
	IDENTIFIER
	{ $1 }
;

comma_list:
	comma_list COMMA IDENTIFIER
	{ $1 @ [ Ident $3 ] }
	| comma_list COMMA STAR
	{ $1 @ [ Star ] }
	| STAR
	{ [ Star ] }
	| IDENTIFIER
	{ [ Ident $1 ] }
;

/*** Error Handlers ***/

err_def:
	HANDLE ERROR ident_list PIPE namespaced_ident SEMI
	{ { onnodes=$3; handler=$5 } }
;

/*
err_list:
	err_list err_def
	{ trace_thing "err_list"; $1 @ [ $2 ] }
	|
	{ trace_thing "err_list"; [] }
;
*/




