(* Original file: boomerang/lib/bparser.mly df589c7 *)

%{
(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2008 J. Nathan Foster and Benjamin C. Pierce                 *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or              *)
(* modify it under the terms of the GNU Lesser General Public                 *)
(* License as published by the Free Software Foundation; either               *)
(* version 2.1 of the License, or (at your option) any later version.         *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *)
(* Lesser General Public License for more details.                            *)
(******************************************************************************)
(* /src/parser.mly                                                            *)
(* Boomerang parser                                                           *)
(* $Id: bparser.srcy 4998 2011-03-16 21:53:34Z mgree $ *)
(******************************************************************************)

(* imports and abbreviations *)
open Bsyntax
open Bident
module L = Blenses
let sprintf = Printf.sprintf
let (@) = Safelist.append

(* helpers for merging parsing info *)
let m = Info.merge_inc
let me1 e1 i2 = m (info_of_exp e1) i2
let me2 i1 e2 = m i1 (info_of_exp e2)
let me e1 e2 = m (info_of_exp e1) (info_of_exp e2)
let mp2 i1 p2 = m i1 (info_of_pat p2)
let mp p1 p2 = m (info_of_pat p1) (info_of_pat p2)

(* error *)
let syntax_error i msg =
  raise
    (Error.Harmony_error
        (fun () -> Util.format "@[%s: Syntax error: %s @\n@]"
          (Info.string_of_t i)
          msg))

(* default chunk tag *)
let mk_default_tag i =
  let p a b = EPair (i, a, b) in
  let species = mk_core_var i "Greedy" in
  let predicate = mk_app i (mk_core_var i "Threshold") (EInteger (i, 0)) in
  let weight = mk_core_var i "NoKey" in
  let name = EString (i, "") in
  let tag = mk_app i (mk_core_var i "Tag") (p (p (p species predicate) weight) name) in
  tag

(* helper for parsing csets *)
let parse_cset s =
  let err () = raise (Parsing.Parse_error) in
  let i = ref 0 in
  let l = String.length s in
  let eos () = !i = l in
  let test c = not (eos ()) && s.[!i] = c in
  let accept c = let r = test c in if r then incr i; r in
  let num = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4
    | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
    | _ -> err () in
  let hex = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4
    | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
    | 'A' | 'a' -> 10 | 'B' | 'b' -> 11 | 'C' | 'c' -> 12
    | 'D' | 'd' -> 13 | 'E' |'e' -> 14 | 'F' | 'f' -> 15
    | _ -> err () in
  let get () =
    let do_get () = let r = s.[!i] in incr i; r in
      if accept '\\' then
        match do_get () with
          | '^' -> '^'
          | '-' -> '-'
          | 'b' -> '\008'
          | 'n' -> '\010'
          | 'r' -> '\013'
          | 't' -> '\009'
          | '\\' -> '\\'
          | c   ->
          if c = '0' && accept 'x' then
            let h1 = hex (do_get ()) in
            let h2 = hex (do_get ()) in
            Char.chr (16 * h1 + h2)
          else
            let n1 = num c in
            let n2 = num (do_get ()) in
            let n3 = num (do_get ()) in
            Char.chr (100 * n1 + 10 * n2 + n3)
      else (do_get ()) in
  let next () = if eos () then err () else get () in
  let rec go acc =
    if eos () then Safelist.rev acc
    else
      let acc' =
        if accept '-' then err ()
        else
          let c1 = next () in
            if accept '-' then
              (c1,next ())::acc
            else (c1,c1)::acc in
        go acc' in
    go []

(* helper for parsing qids *)
let parse_qid i qstr =
  let err () = raise (Parsing.Parse_error) in
  let j = ref 0 in
  let l = String.length qstr in
  let eos () = !j = l in
  let get () = let r = qstr.[!j] in incr j; r in
  let next () = if eos () then err () else get () in
  let rec go (acc,x) =
    if eos () then (Safelist.rev acc,(i,x))
    else
      let c1 = next () in
      if c1 = '.' then go ((i,x)::acc,"")
      else go (acc,x ^ (String.make 1 c1)) in
  go ([],"")

(* helper for building functions *)
let build_fun i param_alts body sort =
  let f,_,f_sort =
    Safelist.fold_right
      (fun pa (f,so,s) -> match pa with
         | Misc.Left(p) ->
             let f' = EFun(i,p,so,f) in
             let s' = SFunction(Id.wild,sort_of_param p,s) in
             (f',None,s')
         | Misc.Right(a) ->
             let f' = ETyFun(i,a,f) in
             let s' = SForall(a,s) in
             (f',None,s'))
      param_alts (body,Some sort,sort) in
  (f,f_sort)

(* helper for building un-sorted functions *)
let build_bare_fun i param_alts body =
  Safelist.fold_right
    (fun pa f ->
       match pa with
         | Misc.Left(p) ->
             EFun(i,p,None,f)
         | Misc.Right(a) ->
             ETyFun(i,a,f))
    param_alts body

let rec mk_tree op l = match l with
  | [] -> syntax_error (Info.M "mk_tree") "empty tree"
  | [e1] -> e1
  | _ ->
      let n = Safelist.length l in
      let rec take i l acc = match i,l with
        | 0,_ | _,[] -> (Safelist.rev acc,l)
        | _,h::t -> take (pred i) t (h::acc) in
      let l1,l2 = take (n/2) l [] in
      let e1 = mk_tree op l1 in
      let e2 = mk_tree op l2 in
      let res = mk_over (me e1 e2) op [e1;e2] in
      res

let rec fixup_pat i p0 = match p0 with
  | PVnt(i,x,Some pti) -> PVnt(i,x,Some (fixup_pat i pti)) (*syntax_error i "illegal pattern"*)
  | PVnt(i,x,None)   -> PVar(i,Qid.id_of_t x,None)
  | PPar(i,p1,p2)    -> PPar(i,fixup_pat i p1,fixup_pat i p2)
  | _ -> p0

(* helpers for building grammars *)
let rec info_of_nonempty_list f l = match l with
  | [] -> syntax_error (Info.M "info_of_nonempty_list") "list was empty"
  | [h] -> f h
  | h::t -> m (f h) (info_of_nonempty_list f t)

let info_of_rules =
  info_of_nonempty_list info_of_rule

let add_aexp ei (i,ys) = (me1 ei i,ei::ys)

let add_atom ai (i,xs,bs) = match ai with
  | Misc.Left e1 -> (me1 e1 i,e1::xs,bs)
  | Misc.Right (l1,e1) ->
      let i1 = Id.info_of_t l1 in
      (m i1 i, EVar(i,Qid.t_of_id l1)::xs,(l1,e1)::bs)

%}

%token <Info.t> EOF
%token <Info.t> MODULE OPEN OF TYPE
%token <Info.t> UNIT BOOL INT CHAR STRING REGEXP AREGEXP SKELETONS RESOURCES LENS CANONIZER FORALL WHERE
%token <Info.t> BOOLPREFS INTPREFS STRINGPREFS STRINGLISTPREFS
%token <Bident.Id.t> STR UIDENT LIDENT QUALIDENT TYVARIDENT CSET NSET
%token <Info.t * char> CHARACTER
%token <Info.t * int> INTEGER
%token <Info.t * bool> BOOLEAN
%token <Info.t> CEX
%token <Info.t * float> FLOAT
%token <Info.t> HASH LBRACE RBRACE LLIST LBRACK RBRACK LPAREN RPAREN LANGLE RANGLE
%token <Info.t> ARROW DARROW DEQARROW EQARROW
%token <Info.t> BEGIN END GRAMMAR AND FUN LET IN TEST MATCH WITH
%token <Info.t> SEMI COMMA DOT EQUAL COLON COLONCOLON BACKSLASH SLASH
%token <Info.t> STAR RLUS BANG BAR BARBAR DOLLAR PLUS MINUS UNDERLINE HAT TILDE AMPERSAND QMARK
%token <Info.t> LT GT LEQ GEQ
%token <Info.t> STYPE VTYPE ASTYPE AVTYPE BIJ GET PUT CREATE CANONIZE CHOOSE INTO
%token <Info.t> ERROR
%token <Info.t> PERM PROJECT SYNTH USING SQUASH


%start modl uid qid
%type <Bsyntax.modl> modl
%type <Bident.Qid.t> uid
%type <Bident.Qid.t> qid

%%

modl:
  | MODULE UIDENT EQUAL opens decls EOF
      { Mod(m $1 $6,$2,$4,$5) }

opens:
  | OPEN qid opens
      { $2::$3 }
  | { [] }

/* --------- DECLARATIONS ---------- */
decls:
  | MODULE UIDENT EQUAL decls END decls
      { let i = m $1 $5 in
        DMod(i,$2,$4)::$6 }

  | TYPE tyvar_list LIDENT EQUAL dtsort_list decls
      { let i = m $1 $4 in
        DType(i,$2,Qid.t_of_id $3,$5)::$6 }

  | LET id param_list COLON sort EQUAL exp decls
      { let i = me2 $1 $7 in
        let f,f_sort = build_fun i $3 $7 $5 in
        let i2,_ = $2 in
        let b = Bind(i,PVar(i2,$2,None),None,f) in
        DLet(i,b)::$8 }

  | LET id param_list EQUAL exp decls
      { let i = me2 $1 $5 in
        let f = build_bare_fun i $3 $5 in
        let i2,_ = $2 in
        let b =  Bind(i,PVar(i2,$2,None),None,f) in
        DLet(i,b)::$6 }

  | LET letpat COLON sort EQUAL exp decls
      { let i = me2 $1 $6 in
        let b = Bind(i,fixup_pat i $2,Some $4,$6) in
        DLet(i,b)::$7 }

  | LET letpat EQUAL exp decls
      { let i = me2 $1 $4 in
        let b =  Bind(i,fixup_pat i $2,None,$4) in
        DLet(i,b)::$5 }

  | TEST infixexp EQUAL test_res_exp decls
      { let i4,tr = $4 in
        let i = m $1 i4 in
        DTest(i,$2,tr)::$5 }

  | TEST infixexp COLON test_res_sort decls
      { let i = m $1 $3 in
        DTest(i,$2,$4)::$5 }

  | TEST infixexp COLON ERROR decls
      { let i = m $1 $4 in
        DTest(i,$2,TestError)::$5 }

  | { [] }

/* --------- TEST RESULTS --------- */
test_res_exp:
  | QMARK
      { ($1,TestPrint) }
  | ERROR
      { ($1,TestError) }
  | appexp
      { (info_of_exp $1, TestEqual $1) }

test_res_sort:
  | QMARK
      { TestSortPrint None }

  | sort
      { TestSortEqual(None,$1) }

/* --------- EXPRESSIONS ---------- */
exp:
  | LET id param_list COLON sort EQUAL exp IN exp
      { let i = me2 $1 $9 in
        let f,f_sort = build_fun i $3 $7 $5 in
        let i2,_ = $2 in
        let b = Bind(i,PVar(i2,$2,None),None,f) in
        ELet(i,b,$9) }

  | LET id param_list EQUAL exp IN exp
      { let i = me2 $1 $7 in
        let f = build_bare_fun i $3 $5 in
        let i2,_ = $2 in
        let b = Bind(i,PVar(i2,$2,None),None,f) in
        ELet(i,b,$7) }

  | LET letpat COLON sort EQUAL exp IN exp
      { let i = me2 $1 $8 in
        let b = Bind(i,fixup_pat i $2,Some $4,$6) in
        ELet(i,b,$8) }

  | LET letpat EQUAL exp IN exp
      { let i = me2 $1 $6 in
        let b = Bind(i,fixup_pat i $2,None,$4) in
        ELet(i,b,$6) }

  | funexp
      { $1 }

/* anonymous functions and application operator */
funexp:
  | FUN param_list ARROW exp
      { let i = me2 $1 $4 in
        build_bare_fun i $2 $4 }

  | FUN param_list COLON asort ARROW exp
      { let i = me2 $1 $6 in
        let f,_ = build_fun i $2 $6 $4 in
        f }

  | cexp DOLLAR funexp
      { mk_app (me $1 $3) $1 $3 }

  | cexp
      { $1 }

/* case expressions */
cexp:
  | MATCH composeexp WITH branch_list
       { let i4,pl = $4 in
         ECase(m $1 i4,$2,pl,None) }

  | MATCH composeexp WITH branch_list COLON sort
      { let i4,pl = $4 in
        ECase(m $1 i4,$2,pl,Some $6) }

  | composeexp
      { $1 }

/* compose expressions */
composeexp:
  | composeexp SEMI commaexp
      { mk_compose (me $1 $3) $1 $3 }

  | commaexp
      { $1 }

/* comma expression */
commaexp:
  | commaexp COMMA barexp
      { EPair(me $1 $3, $1, $3) }
  | barexp
      { $1 }

/* bar expressions */
barexp:
  | obarexp
      { mk_tree OBar (Safelist.rev $1) }

  | dbarexp
      { mk_tree OBarBar (Safelist.rev $1) }

  | equalexp
      { $1 }

obarexp:
  | obarexp BAR equalexp
      { $3 :: $1 }
  | equalexp BAR equalexp
      { [$3; $1 ] }

dbarexp:
  | dbarexp BARBAR equalexp
      { $3 :: $1 }
  | equalexp BARBAR equalexp
      { [$3; $1] }

equalexp:
  | appexp EQUAL appexp
      { mk_over (me $1 $3) OEqual [$1; $3] }
  | infixexp
      { $1 }

infixexp:
  | dotexp
      { $1 }
  | tildeexp
      { $1 }
  | rewriteexp
      { $1 }
  | ampexp
      { $1 }
  | ampampexp
      { $1 }
  | lenscomponentexp
      { $1 }
  | minusexp
      { $1 }
  | ltexp
      { $1 }
  | leqexp
      { $1 }
  | gtexp
      { $1 }
  | geqexp
      { $1 }
  | appexp
      { $1 }

dotexp:
  | dotexp DOT appexp
      { mk_over (me $1 $3) ODot [$1; $3] }
  | dotexp DOT rewriteexp
      { mk_over (me $1 $3) ODot [$1; $3] }
  | appexp DOT appexp
      { mk_over (me $1 $3) ODot [$1; $3] }
  | rewriteexp DOT rewriteexp
      { mk_over (me $1 $3) ODot [$1; $3] }
  | rewriteexp DOT appexp
      { mk_over (me $1 $3) ODot [$1; $3] }
  | appexp DOT rewriteexp
      { mk_over (me $1 $3) ODot [$1; $3] }

tildeexp:
  | tildeexp TILDE appexp
      { mk_over (me $1 $3) OTilde [$1; $3] }
  | appexp TILDE appexp
      { mk_over (me $1 $3) OTilde [$1; $3] }

ampexp:
  | ampexp AMPERSAND appexp
      { mk_over (me $1 $3) OAmp [$1; $3] }
  | appexp AMPERSAND appexp
      { mk_over (me $1 $3) OAmp [$1; $3] }

ampampexp:
  | appexp AMPERSAND AMPERSAND ampampexp
      { mk_over (me $1 $4) OAmpAmp [$1; $4] }
  | appexp AMPERSAND AMPERSAND appexp
      { mk_over (me $1 $4) OAmpAmp [$1; $4] }

rewriteexp:
  | appexp DARROW appexp
      { mk_over (me $1 $3) ODarrow [$1; $3] }

  | appexp DEQARROW appexp
      { mk_over (me $1 $3) ODeqarrow [$1; $3] }

lenscomponentexp:
  | appexp GET appexp
      { let i = me $1 $3 in
        mk_bin_op i (mk_core_var i "get") $1 $3 }
  | appexp PUT appexp INTO appexp
      { let i = me $1 $3 in
        mk_tern_op i (mk_core_var i "put") $1 $3 $5 }
  | appexp CREATE appexp
      { let i = me $1 $3 in
        mk_bin_op i (mk_core_var i "create") $1 $3 }
  | appexp CANONIZE appexp
      { let i = me $1 $3 in
        mk_bin_op i (mk_core_var i "canonize") $1 $3 }
  | appexp CHOOSE appexp
      { let i = me $1 $3 in
        mk_bin_op i (mk_core_var i "choose") $1 $3 }

minusexp:
  | infixexp MINUS appexp
      { mk_over (me $1 $3) OMinus [$1; $3] }
  | MINUS appexp
      { mk_over (me2 $1 $2) OMinus [EInteger($1,0); $2] }

ltexp:
  | appexp LT appexp
      { mk_over (me $1 $3) OLt [$1; $3] }

leqexp:
  | appexp LEQ appexp
      { mk_over (me $1 $3) OLeq [$1; $3] }

gtexp:
  | appexp GT appexp
      { mk_over (me $1 $3) OGt [$1; $3] }

geqexp:
  | appexp GEQ appexp
      { mk_over (me $1 $3) OGeq [$1; $3] }


/* application expressions */
appexp:
  | appexp repexp
      { mk_app (me $1 $2) $1 $2 }

    | PERM LPAREN listexp1 WITH repexp
            { let i = me2 $1 $5 in
        EPerm(i,$3,$5) }

    | PROJECT appexp ARROW repexp
      { let i = me2 $1 $4 in
        EProject(i,$2,$4) }

    | SYNTH appexp DEQARROW repexp USING LBRACE listexp2
         { let i = if List.length $7 = 0 then
					me2 $1 $4 else
					me2 $1 (List.hd (List.rev $7)) in
        ESynth (i,$2,$4,Some $7) }

    | SYNTH appexp DEQARROW repexp
         { let i = me2 $1 $4 in
        ESynth (i,$2,$4,None) }

		| SQUASH appexp ARROW repexp USING repexp
		  { let i = me2 $1 $6 in
        ESquash(i,$2,$4, $6) }

  | repexp
      { $1 }

listexp1:
  | RPAREN
        {[]}

    | appexp RPAREN
        {[$1]}

    | appexp COMMA listexp1
        {$1 :: $3}

listexp2:
  | RBRACE
		{[]}

	| appexp RBRACE
		{[$1]}

	| appexp COMMA listexp2
		{$1 :: $3}


/* repeated expressions */
repexp:
  | tyexp rep
      { let i2,(min,max) = $2 in
        let i = me1 $1 i2 in
	  mk_iter i min max $1 }

  | tyexp
      { $1 }

tyexp:
  | tyexp LBRACE sort RBRACE
      { let i = me1 $1 $4 in
        ETyApp(i,$1,$3) }

  | aexp
      { $1 }


/* atomic expressions */
aexp:

  | LPAREN exp RPAREN
      { $2 }

  | BEGIN exp END
      { $2 }

  | qid
      { mk_qid_var $1 }

  | matchexp
      { $1 }

  | HASH LBRACE sort LLIST list
      { let i6,mk = $5 in
        let i = m $1 i6 in
        let l = mk i $3 in
        l }

  | CHARACTER
      { let i,c = $1 in
        EChar(i,c) }

  | INTEGER
      { let i,n = $1 in
        EInteger(i,n) }

  | BOOLEAN
      { let i,b = $1 in
        EBoolean(i,if b then None else Some (EString(i,""))) }

  | CEX LPAREN exp RPAREN
      { EBoolean($1,Some $3) }

  | CSET
      { let i1,s1 = $1 in
        ECSet(i1,true,parse_cset s1) }

  | NSET
      { let i1,s1 = $1 in
        ECSet(i1,false,parse_cset s1) }

  | STR
      { let i,s = $1 in
        EString(i,s) }

  | LPAREN RPAREN
      { EUnit(m $1 $2) }

  | GRAMMAR productions END
      { EGrammar(m $1 $3, $2) }

  | aexp STYPE
      { let i = me1 $1 $2 in
        mk_app i (mk_core_var i "stype") $1 }
  | aexp VTYPE
      { let i = me1 $1 $2 in
        mk_app i (mk_core_var i "vtype") $1 }
  | aexp ASTYPE
      { let i = me1 $1 $2 in
        mk_app i (mk_core_var i "astype") $1 }
  | aexp AVTYPE
      { let i = me1 $1 $2 in
        mk_app i (mk_core_var i "avtype") $1 }
  | aexp BIJ
      { let i = me1 $1 $2 in
        mk_app i (mk_core_var i "bij") $1 }

matchexp:
  | LANGLE exp RANGLE
      { let i = $1 in
        let tag = mk_default_tag i in
        mk_over (m $1 $3) OMatch [tag; $2] }
  | LANGLE appexp COLON exp RANGLE
      { mk_over (m $1 $5) OMatch [$2; $4] }

/* --------- BRANCHES ---------- */
branch:
  | pat ARROW equalexp
      { let i = m (info_of_pat $1) (info_of_exp $3) in
        (i,$1,$3) }

branch_list:
  | branch branch_list2
      { let (i1,p,e) = $1 in
        let (i2,l) = $2 i1 in
        (m i1 i2, (p,e)::l) }

  | BAR branch branch_list2
      { let (i1,p,e) = $2 in
        let (i2,l) = $3 i1 in
        (m $1 i2, (p,e)::l) }

branch_list2:
  |
      { (fun i -> (i,[])) }

  | BAR branch branch_list2
      { let (i1,p,e) = $2 in
        let (i2,l) = $3 i1 in
        (fun _ -> (m $1 i2, (p,e)::l)) }

/* --------- REPETITIONS ---------- */
rep:
  | STAR
      { ($1, (0,-1)) }

  | PLUS
      { ($1, (1,-1)) }

  | QMARK
      { ($1, (0,1)) }

  | LBRACE INTEGER RBRACE
      { let i = m $1 $3 in let _,n = $2 in (i, (n,n)) }

  | LBRACE INTEGER COMMA RBRACE
      { let i = m $1 $3 in let _,n = $2 in (i, (n,-1)) }

  | LBRACE INTEGER COMMA INTEGER RBRACE
      { let i = m $1 $5 in let _,n2 = $2 in let _,n4 = $4 in (i, (n2, n4)) }

/* --------- LISTS ------------ */
list:
  | RBRACK
      { $1, (fun i s -> ETyApp(i,mk_list_var i "Nil",s)) }

  | commaexp RBRACK
      { ($2,
         (fun i s ->
            mk_app i
              (ETyApp(i,mk_list_var i "Cons",s))
              (EPair(i,$1,ETyApp(i,mk_list_var i "Nil",s))))) }

  | commaexp SEMI list
    { let i3,mk = $3 in
      (i3,
       (fun i s ->
          mk_app i
            (ETyApp(i,mk_list_var i "Cons",s))
            (EPair(i,$1, mk i s)))) }


/* --------- GRAMMARS ----------- */
atom:
  | aexp
      { Misc.Left $1 }
  | LIDENT COLON aexp
      { Misc.Right ($1,$3) }

atoms:
  | atom atoms2
      { add_atom $1 $2 }

atoms2:
  |
     { (Info.M "atoms",[],[]) }
  | atom atoms2
     { add_atom $1 $2 }

aexps:
  | aexp aexps2
      { add_aexp $1 $2 }

aexps2:
  |
      { (Info.M "aexps", []) }
  | aexp aexps2
      { add_aexp $1 $2 }

rule:
  | atoms DARROW aexps
     { let i1,xs,bs = $1 in
       let i3,ys = $3 in
       Rule(m i1 i3,xs,ys,bs) }

rules:
  | rule rules2
    { $1 :: $2 }
  | BAR rule rules2
    { $2 :: $3 }

rules2:
  |
    { [] }
  | BAR rule rules2
    { $2 :: $3 }

production:
  | LIDENT COLONCOLON EQUAL rules
    { let i1,_ = $1 in
      let i5 = info_of_rules $4 in
      Prod(m i1 i5,$1,$4) }

productions:
  | production
    { [$1] }
  | production AND productions
    { $1 :: $3 }

/* --------- IDENTIFIERS ---------- */
id:
  | LIDENT
      { $1 }
  | UIDENT
      { $1 }

qid:
  | LIDENT
      { Qid.t_of_id $1 }
  | UIDENT
      { Qid.t_of_id $1 }
  | QUALIDENT
      { let (i,qs) = $1 in parse_qid i qs }

qvar:
  | LIDENT
      { Qid.t_of_id $1 }

  | QUALIDENT
      { let (i,qs) = $1 in parse_qid i qs }

/* --------- PARAMETERS ---------- */
param_list:
  | param param_list2
      { $1 :: $2 }
param_list2:
  | param param_list2
      { $1 :: $2 }
  |
      { [] }

param:
  | LPAREN id COLON sort RPAREN
      { let i = m $1 $5 in
        Misc.Left (Param(i,$2,$4)) }

  | LPAREN id COLON LENS IN QMARK DARROW appexp RPAREN
      { let i = m $1 $9 in
        let p = mk_bin_op i (mk_core_var i "equiv")
	                    (mk_app i (mk_core_var i "vtype") (mk_var $2))
                            $8 in
        let s = SRefine($2,false,SLens,p,None) in
        Misc.Left (Param(i,$2,s)) }

  | LPAREN id COLON LENS IN QMARK DEQARROW appexp RPAREN
      { let i = m $1 $9 in
        let p = mk_bin_op i (mk_core_var i "land")
	                    (mk_bin_op i (mk_core_var i "equiv")
	                                 (mk_app i (mk_core_var i "vtype") (mk_var $2))
                                         $8)
			    (mk_app i (mk_core_var i "bij") (mk_var $2)) in
        let s = SRefine($2,false,SLens,p,None) in
        Misc.Left (Param(i,$2,s)) }

  | LPAREN id COLON LENS IN appexp DARROW QMARK RPAREN
      { let i = m $1 $9 in
        let p = mk_bin_op i (mk_core_var i "equiv")
	                    (mk_app i (mk_core_var i "stype") (mk_var $2))
                            $6 in
        let s = SRefine($2,false,SLens,p,None) in
        Misc.Left (Param(i,$2,s)) }

  | LPAREN id COLON LENS IN appexp DEQARROW QMARK RPAREN
      { let i = m $1 $9 in
        let p = mk_bin_op i (mk_core_var i "land")
	                    (mk_bin_op i (mk_core_var i "equiv")
	                                 (mk_app i (mk_core_var i "stype") (mk_var $2))
                                         $6)
			    (mk_app i (mk_core_var i "bij") (mk_var $2)) in
        let s = SRefine($2,false,SLens,p,None) in
        Misc.Left (Param(i,$2,s)) }

  | LPAREN id COLON LENS IN appexp DARROW appexp RPAREN
      { let i = m $1 $9 in
        let p = mk_tern_op i (mk_core_var i "in_lens_type") (mk_var $2) $6 $8 in
        let s = SRefine($2,false,SLens,p,None) in
        Misc.Left (Param(i,$2,s)) }

  | LPAREN id COLON LENS IN appexp DEQARROW appexp RPAREN
      { let i = m $1 $9 in
        let p = mk_tern_op i (mk_core_var i "in_bij_lens_type") (mk_var $2) $6 $8 in
        let s = SRefine($2,false,SLens,p,Some($6, $8)) in
        Misc.Left (Param(i,$2,s)) }

  | LPAREN id COLON STRING IN exp RPAREN
      { let i = m $1 $7 in
        let p = mk_bin_op i (mk_core_var i "matches") $6 (mk_var $2) in
        let s = SRefine($2,false,SString,p,None) in
        Misc.Left (Param(i,$2,s)) }

  | LPAREN id COLON sort WHERE BANG exp RPAREN
      { let i,_ = $2 in
        let s = SRefine($2,true,$4,$7,None) in
        Misc.Left (Param(i,$2,s)) }

  | LPAREN id COLON sort WHERE exp RPAREN
      { let i,_ = $2 in
        let s = SRefine($2,false,$4,$6,None) in
        Misc.Left (Param(i,$2,s)) }

  | LPAREN TYVARIDENT RPAREN
      { Misc.Right ($2) }

  | TYVARIDENT
      { Misc.Right ($1) }

  /* hack: "(unit)" cannot be an identifier */
  | LPAREN RPAREN
      { let i = m $1 $2 in
        Misc.Left (Param(i,(i,"(unit)"),SUnit)) }

/* --------- SORTS ---------- */
sort:
  | FORALL TYVARIDENT EQARROW sort
      { SForall($2,$4) }

  | arrowsort
      { $1 }

/* arrow sorts */
arrowsort:
  | productsort ARROW arrowsort
      { SFunction(Id.wild,$1,$3) }

  | LPAREN id COLON productsort ARROW arrowsort RPAREN
      { SFunction($2,$4,$6) }

  | productsort
      { $1 }

/* product sorts */
productsort:
  | productsort STAR datatypesort
      { SProduct($1,$3) }

  | datatypesort
      { $1 }

/* data type sorts */
datatypesort:
  | bsort qvar
      { SData([$1],$2) }

  | LPAREN sort COMMA sort_list RPAREN qvar
      { SData($2::$4, $6) }

  | bsort
      { $1 }

bsort:
  | LPAREN sort RPAREN
      { $2 }

  | LPAREN sort WHERE BANG exp RPAREN
      { SRefine(Id.wild,false,$2,$5,None) }

  | LPAREN sort WHERE exp RPAREN
      { SRefine(Id.wild,true,$2,$4,None) }

  | LPAREN id COLON sort WHERE BANG exp RPAREN
      { SRefine($2,true,$4,$7,None) }

  | LPAREN id COLON sort WHERE exp RPAREN
      { SRefine($2,false,$4,$6,None) }

  | LPAREN LENS IN QMARK DARROW appexp RPAREN
      { let i = m $1 $7 in
        let l = Id.mk i "_l" in
        let p = mk_bin_op i (mk_core_var i "equiv")
	                    (mk_app i (mk_core_var i "vtype") (mk_var l))
                            $6 in
        SRefine(l,false,SLens,p,None) }

  | LPAREN LENS IN QMARK DEQARROW appexp RPAREN
      { let i = m $1 $7 in
        let l = Id.mk i "_l" in
        let p = mk_bin_op i (mk_core_var i "land")
	                    (mk_bin_op i (mk_core_var i "equiv")
	                                 (mk_app i (mk_core_var i "vtype") (mk_var l))
                                         $6)
			    (mk_app i (mk_core_var i "bij") (mk_var l)) in
        SRefine(l,false,SLens,p,None) }

  | LPAREN LENS IN appexp DARROW QMARK RPAREN
      { let i = m $1 $7 in
	let l = Id.mk i "_l" in
        let p = mk_bin_op i (mk_core_var i "equiv")
	                    (mk_app i (mk_core_var i "stype") (mk_var l))
                            $4 in
        SRefine(l,false,SLens,p,None) }

  | LPAREN LENS IN appexp DEQARROW QMARK RPAREN
      { let i = m $1 $7 in
	let l = Id.mk i "_l" in
        let p = mk_bin_op i (mk_core_var i "land")
	                    (mk_bin_op i (mk_core_var i "equiv")
	                                 (mk_app i (mk_core_var i "stype") (mk_var l))
                                         $4)
			    (mk_app i (mk_core_var i "bij") (mk_var l)) in
        SRefine(l,false,SLens,p,None) }

  | LPAREN LENS IN appexp DARROW appexp RPAREN
      { let i = m $1 $7 in
        let l = Id.mk i "_l" in
        let chk c a = mk_tern_op i (mk_core_var i "in_lens_type") (mk_var l) c a in
        SRefine(l,false,SLens,chk $4 $6,None) }

  | LPAREN LENS IN appexp DEQARROW appexp RPAREN
      { let i = m $1 $7 in
        let l = Id.mk i "_l" in
        let chk c a = mk_tern_op i (mk_core_var i "in_bij_lens_type") (mk_var l) c a in
        SRefine(l,false,SLens,chk $4 $6,Some($4,$6)) }

  | LPAREN STRING IN exp RPAREN
      { let i = m $1 $5 in
        let s = Id.mk i "_s" in
        let p = mk_bin_op i (mk_core_var i "matches") $4 (mk_var s) in
        SRefine(s,false,SString,p,None) }

  | asort
      { $1 }

/* atomic sorts */
asort:
  | qvar
      { SData([], $1) }

  | CHAR
      { SChar }

  | STRING
      { SString }

  | REGEXP
      { SRegexp }

  | AREGEXP
      { SAregexp }

  | SKELETONS
      { SSkeletons }

  | RESOURCES
      { SResources }

  | LENS
      { SLens }

  | INT
      { SInteger }

  | BOOL
      { SBool }

  | CANONIZER
      { SCanonizer }

  | BOOLPREFS
      { SPrefs PrBool }

  | INTPREFS
      { SPrefs PrInt }

  | STRINGPREFS
      { SPrefs PrString }

  | STRINGLISTPREFS
      { SPrefs PrStringList }

  | UNIT
      { SUnit }

  | tyvar
      { SVar $1 }

tyvar:
  | TYVARIDENT
      { $1 }

tyvar_list:
  |
      { [] }

  | tyvar
      { [$1] }

  | LPAREN tyvar_list2 RPAREN
      { $2 }

tyvar_list2:
  | tyvar
      { [$1] }

  | tyvar COMMA tyvar_list2
      { $1::$3 }

sort_list:
  | sort
      { [$1] }

  | sort COMMA sort_list
      { $1 :: $3 }

/* data type sorts */
dtsort:
  | UIDENT
      { ($1,None) }

  | UIDENT OF sort
      { ($1,Some $3) }

dtsort_list:
  | dtsort dtsort_list2
      { $1 :: $2 }

dtsort_list2:
  |
      { [] }

  | BAR dtsort dtsort_list2
      { $2 :: $3 }

/* --------- PATTERNS ---------- */
lcpat:
  | listpat { $1 }
  | conpat { $1 }

pat:
  | pat COMMA lcpat
      { let i = mp $1 $3 in
        PPar(i,$1,$3) }

  | lcpat { $1 }

letpat:
  | letpat COMMA lcpat
      { let i = mp $1 $3 in
        PPar(i,$1,$3) }

  | QUALIDENT apat
      { let i,qs = $1 in
        PVnt(i,parse_qid i qs, Some $2) }
      /* COMMA lcapat
      { let i1, qs = $1 in
        let i = mp $2 $4 in
        PPar(i, PVnt(i1, parse_qid i1 qs, Some $2) ,$4) }*/

  | apat { $1 }

conpat:
  | UIDENT apat
      { let i1,_ = $1 in
        let i = mp2 i1 $2 in
         PVnt(i,Qid.t_of_id $1,Some $2) }

  | QUALIDENT apat
      { let (i,qs) = $1 in
        PVnt(i,parse_qid i qs,Some $2) }

  | apat { $1 }

apat:
  | aapat
      { $1 }

  | LPAREN RPAREN
      { PUnt(m $1 $2) }

  | INTEGER
    { let i,n = $1 in
       PInt(i,n) }

  | BOOLEAN
      { let i,b = $1 in
        PBol(i,b) }

  | CEX LPAREN pat RPAREN
      { PCex($1,$3) }

  | STR
      { let i,s = $1 in
        PStr(i,s) }

  | UIDENT
      { let i,_ = $1 in
        PVnt(i,Qid.t_of_id $1,None) }

  | QUALIDENT
      { let (i,qs) = $1 in
        PVnt(i,parse_qid i qs,None) }

  | LPAREN pat RPAREN
      { $2 }

aapat:
  | UNDERLINE
      { PWld $1 }

  | LIDENT
      { let i, _ = $1 in
        PVar (i, $1, None) }

listpat:
  | CSET
      { let i, cset = $1 in
        if cset = ""
        then PVnt (i, Qid.mk_list_t i "Nil", None)
        else syntax_error i (Printf.sprintf "\nDid you meant %s::[] instead of [%s]?" cset cset) }

  | conpat COLONCOLON aapat
      { let i = $2 in
        let p = PPar (i, $1, $3) in
        PVnt ($2, Qid.mk_list_t i "Cons", Some p) }

  | conpat COLONCOLON listpat
      { let i = $2 in
        let p = PPar (i, $1, $3) in
        PVnt ($2, Qid.mk_list_t i "Cons", Some p) }

/* --------- MISC SYMBOLS ---------- */
uid:
  | UIDENT
      { Qid.t_of_id $1 }
