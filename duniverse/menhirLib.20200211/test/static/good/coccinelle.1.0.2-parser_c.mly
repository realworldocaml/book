(* Original file: coccinelle.1.0.2/coccinelle-1.0.2/parsing_c/parser_c.mly *)
%{
(* Yoann Padioleau
 *
 * Copyright (C) 2002, 2006, 2007, 2008, 2009 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

open Ast_c

module LP = Lexer_parser
open Lexer_parser (* for the fields *)

open Semantic_c (* Semantic exn *)
module T = Token_c

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let warning s v =
  if !Flag_parsing_c.verbose_parsing
  then Common.warning ("PARSING: " ^ s) v
  else v

let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_c.verbose_parsing

(*****************************************************************************)
(* Parse helpers functions *)
(*****************************************************************************)

(*-------------------------------------------------------------------------- *)
(* Type related *)
(*-------------------------------------------------------------------------- *)

type shortLong      = Short  | Long | LongLong

type decl = {
  storageD: storagebis wrap;
  typeD: ((sign option) * (shortLong option) * (typeCbis option)) wrap;
  qualifD: typeQualifierbis wrap;
  inlineD: bool             wrap;
  (* note: have a full_info: parse_info list; to remember ordering
   * between storage, qualifier, type ? well this info is already in
   * the Ast_c.info, just have to sort them to get good order *)
}

let nullDecl = {
  storageD = NoSto, [];
  typeD = (None, None, None), [];
  qualifD = nullQualif;
  inlineD = false, [];
}
let fake_pi = Common.fake_parse_info

let addStorageD  = function
  | ((x,ii), ({storageD = (NoSto,[])} as v)) -> { v with storageD = (x, [ii]) }
  | ((x,ii), ({storageD = (y, ii2)} as v)) ->
      if x = y then warning "duplicate storage classes" v
      else raise (Semantic ("multiple storage classes", fake_pi))

let addInlineD  = function
  | ((true,ii), ({inlineD = (false,[])} as v)) -> { v with inlineD=(true,[ii])}
  | ((true,ii), ({inlineD = (true, ii2)} as v)) -> warning "duplicate inline" v
  | _ -> raise (Impossible 86)


let addTypeD     = function
  | ((Left3 Signed,ii)   ,({typeD = ((Some Signed,  b,c),ii2)} as v)) ->
      warning "duplicate 'signed'"   v
  | ((Left3 UnSigned,ii) ,({typeD = ((Some UnSigned,b,c),ii2)} as v)) ->
      warning "duplicate 'unsigned'" v
  | ((Left3 _,ii),        ({typeD = ((Some _,b,c),ii2)} as _v)) ->
      raise (Semantic ("both signed and unsigned specified", fake_pi))
  | ((Left3 x,ii),        ({typeD = ((None,b,c),ii2)} as v))   ->
      {v with typeD = (Some x,b,c),ii @ ii2}

  | ((Middle3 Short,ii),  ({typeD = ((a,Some Short,c),ii2)} as v)) ->
      warning "duplicate 'short'" v


  (* gccext: long long allowed *)
  | ((Middle3 Long,ii),   ({typeD = ((a,Some Long ,c),ii2)} as v)) ->
      { v with typeD = (a, Some LongLong, c),ii @ ii2 }
  | ((Middle3 Long,ii),   ({typeD = ((a,Some LongLong ,c),ii2)} as v)) ->
      warning "triplicate 'long'" v


  | ((Middle3 _,ii),      ({typeD = ((a,Some _,c),ii2)} as _v)) ->
      raise (Semantic ("both long and short specified", fake_pi))
  | ((Middle3 x,ii),      ({typeD = ((a,None,c),ii2)} as v))  ->
      {v with typeD = (a, Some x,c),ii @ ii2}

  | ((Right3 t,ii),       ({typeD = ((a,b,Some x),ii2)} as _v)) ->
      raise (Semantic ((Printf.sprintf "two or more data types: t %s ii %s
typeD %s ii2 %s
" (Dumper.dump t) (Dumper.dump ii) (Dumper.dump x) (Dumper.dump ii2)), fake_pi))
  | ((Right3 t,ii),       ({typeD = ((a,b,None),ii2)} as v))   ->
      {v with typeD = (a,b, Some t),ii @ ii2}


let addQualif = function
  | ({const=true},   ({const=true} as x)) ->   warning "duplicate 'const'" x
  | ({volatile=true},({volatile=true} as x))-> warning "duplicate 'volatile'" x
  | ({const=true},    v) -> {v with const=true}
  | ({volatile=true}, v) -> {v with volatile=true}
  | _ ->
      internal_error "there is no noconst or novolatile keyword"

let addQualifD ((qu,ii), ({qualifD = (v,ii2)} as x)) =
  { x with qualifD = (addQualif (qu, v),ii::ii2) }


(*-------------------------------------------------------------------------- *)
(* Declaration/Function related *)
(*-------------------------------------------------------------------------- *)


(* stdC: type section, basic integer types (and ritchie)
 * To understand the code, just look at the result (right part of the PM)
 * and go back.
 *)
let (fixDeclSpecForDecl: decl -> (fullType * (storage wrap)))  = function
 {storageD = (st,iist);
  qualifD = (qu,iiq);
  typeD = (ty,iit);
  inlineD = (inline,iinl);
  } ->
   let ty',iit' =
   (match ty with
 | (None,None,None)       ->
     (* generate fake_info, otherwise type_annotater can crash in
      * offset.
      *)
     warning "type defaults to 'int'" (defaultInt, [fakeInfo fake_pi])
 | (None, None, Some t)   -> (t, iit)

 | (Some sign,   None, (None| Some (BaseType (IntType (Si (_,CInt))))))  ->
     BaseType(IntType (Si (sign, CInt))), iit
 | ((None|Some Signed),Some x,(None|Some(BaseType(IntType (Si (_,CInt)))))) ->
     BaseType(IntType (Si (Signed, [Short,CShort; Long, CLong; LongLong, CLongLong] +> List.assoc x))), iit
 | (Some UnSigned, Some x, (None| Some (BaseType (IntType (Si (_,CInt))))))->
     BaseType(IntType (Si (UnSigned, [Short,CShort; Long, CLong; LongLong, CLongLong] +> List.assoc x))), iit
 | (Some sign,   None, (Some (BaseType (IntType CChar))))   ->
     BaseType(IntType (Si (sign, CChar2))), iit
 | (None, Some Long,(Some(BaseType(FloatType CDouble))))    ->
     BaseType (FloatType (CLongDouble)), iit

 | (Some _,_, Some _) ->
     (*mine*)
     raise (Semantic ("signed, unsigned valid only for char and int", fake_pi))
 | (_,Some _,(Some(BaseType(FloatType (CFloat|CLongDouble))))) ->
     raise (Semantic ("long or short specified with floating type", fake_pi))
 | (_,Some Short,(Some(BaseType(FloatType CDouble)))) ->
     raise (Semantic ("the only valid combination is long double", fake_pi))

 | (_, Some _, Some _) ->
     (* mine *)
     raise (Semantic ("long, short valid only for int or float", fake_pi))

     (* if do short uint i, then gcc say parse error, strange ? it is
      * not a parse error, it is just that we dont allow with typedef
      * either short/long or signed/unsigned. In fact, with
      * parse_typedef_fix2 (with et() and dt()) now I say too parse
      * error so this code is executed only when do short struct
      * {....} and never with a typedef cos now we parse short uint i
      * as short ident ident => parse error (cos after first short i
      * pass in dt() mode) *)

   )
   in
   ((qu, iiq),
   (ty', iit'))
     ,((st, inline),iist @ iinl)


let fixDeclSpecForParam = function ({storageD = (st,iist)} as r) ->
  let ((qu,ty) as v,_st) = fixDeclSpecForDecl r in
  match st with
  | (Sto Register) -> (v, true), iist
  | NoSto -> (v, false), iist
  | _ ->
      raise
        (Semantic ("storage class specified for parameter of function",
                  fake_pi))

let fixDeclSpecForMacro = function ({storageD = (st,iist)} as r) ->
  let ((qu,ty) as v,_st) = fixDeclSpecForDecl r in
  match st with
  | NoSto -> v
  | _ ->
      raise
        (Semantic ("storage class specified for macro type decl",
                  fake_pi))


let fixDeclSpecForFuncDef x =
  let (returnType,storage) = fixDeclSpecForDecl x in
  (match fst (unwrap storage) with
  | StoTypedef ->
      raise (Semantic ("function definition declared 'typedef'", fake_pi))
  | _ -> (returnType, storage)
  )


(* parameter: (this is the context where we give parameter only when
 * in func DEFINITION not in funct DECLARATION) We must have a name.
 * This function ensure that we give only parameterTypeDecl with well
 * formed Classic constructor todo?: do we accept other declaration
 * in ? so I must add them to the compound of the deffunc. I dont
 * have to handle typedef pb here cos C forbid to do VF f { ... }
 * with VF a typedef of func cos here we dont see the name of the
 * argument (in the typedef)
 *)
let (fixOldCDecl: fullType -> fullType) = fun ty ->
  match Ast_c.unwrap_typeC ty with
  | FunctionType (fullt, (params, (b, iib))) ->

      (* stdC: If the prototype declaration declares a parameter for a
       * function that you are defining (it is part of a function
       * definition), then you must write a name within the declarator.
       * Otherwise, you can omit the name. *)
      (match params with
      | [{p_namei = None; p_type = ty2},_] ->
          (match Ast_c.unwrap_typeC ty2 with
          | BaseType Void ->
              ty
          | _ ->
              pr2_once ("SEMANTIC:parameter name omitted, but I continue");
              ty
          )

      | params ->
          (params +> List.iter (fun (param,_) ->
            match param with
            | {p_namei = None} ->
              (* if majuscule, then certainly macro-parameter *)
                pr2_once ("SEMANTIC:parameter name omitted, but I continue");
	    | _ -> ()
          ));
          ty
      )

        (* todo? can we declare prototype in the decl or structdef,
           ... => length <> but good kan meme *)
  | _ ->
      (* gcc say parse error but dont see why *)
      raise (Semantic ("seems this is not a function", fake_pi))


let fixFunc (typ, compound, old_style_opt) =
  let (cp,iicp) = compound in

  let (name, ty,   (st,iist),  attrs) = typ in

  let (qu, tybis) = ty in

  match Ast_c.unwrap_typeC ty with
  | FunctionType (fullt, (params,abool)) ->
      let iifunc = Ast_c.get_ii_typeC_take_care tybis in

      let iistart = Ast_c.fakeInfo () in
      assert (qu = nullQualif);

      (match params with
      | [{p_namei= None; p_type = ty2}, _] ->
          (match Ast_c.unwrap_typeC ty2 with
          | BaseType Void ->  ()
          | _ ->
                (* failwith "internal errror: fixOldCDecl not good" *)
              ()
          )
      | params ->
          params +> List.iter (function
          | ({p_namei = Some s}, _) -> ()
	  | _ -> ()
                (* failwith "internal errror: fixOldCDecl not good" *)
          )
      );
      (* bugfix: cf tests_c/function_pointer4.c.
       * Apparemment en C on peut syntaxiquement ecrire ca:
       *
       *   void a(int)(int x);
       * mais apres gcc gueule au niveau semantique avec:
       *   xxx.c:1: error: 'a' declared as function returning a function
       * Je ne faisais pas cette verif. Sur du code comme
       *   void METH(foo)(int x) { ...} , le parser croit (a tort) que foo
       * est un typedef, et donc c'est parsé comme l'exemple precedent,
       * ce qui ensuite confuse l'unparser qui n'est pas habitué
       * a avoir dans le returnType un FunctionType et qui donc
       * pr_elem les ii dans le mauvais sens ce qui genere au final
       * une exception. Hence this fix to at least detect the error
       * at parsing time (not unparsing time).
       *)
      (match Ast_c.unwrap_typeC fullt with
      | FunctionType _ ->
          let s = Ast_c.str_of_name name in
          let iis = Ast_c.info_of_name name in
          pr2 (spf "WEIRD: %s declared as function returning a function." s);
          pr2 (spf "This is probably because of a macro. Extend standard.h");
          raise (Semantic (spf "error: %s " s, Ast_c.parse_info_of_info iis))
      | _ -> ()
      );

      (* it must be nullQualif,cos parser construct only this*)
      {f_name = name;
       f_type = (fullt, (params, abool));
       f_storage = st;
       f_body =
	if !Flag_parsing_c.parsing_header_for_types
	then []
	else cp;
       f_attr = attrs;
       f_old_c_style = old_style_opt;
      },
      (iifunc @ iicp @ [iistart] @ iist)
  | _ ->
      raise
        (Semantic
            ("you are trying to do a function definition but you dont give " ^
             "any parameter", fake_pi))


(*-------------------------------------------------------------------------- *)
(* parse_typedef_fix2 *)
(*-------------------------------------------------------------------------- *)

let dt s () =
  if !Flag_parsing_c.debug_etdt then pr2 ("<" ^ s);
  LP.disable_typedef ()

let et s () =
  if !Flag_parsing_c.debug_etdt then pr2 (">" ^ s);
  LP.enable_typedef ()


let fix_add_params_ident x =
  let (s, ty, st, _attrs) = x in
  match Ast_c.unwrap_typeC ty with
  | FunctionType (fullt, (params, bool)) ->

      (match params with
      | [{p_namei=None; p_type=ty2}, _] ->
          (match Ast_c.unwrap_typeC ty2 with
          | BaseType Void -> ()
          | _ ->
              (* failwith "internal errror: fixOldCDecl not good" *)
              ()
          )
      | params ->
          params +> List.iter (function
          | ({p_namei= Some name}, _) ->
              LP.add_ident (Ast_c.str_of_name s)
	  | _ ->
              ()
                (* failwith "internal errror: fixOldCDecl not good" *)
          )
      )
  | _ -> ()

(*-------------------------------------------------------------------------- *)
(* shortcuts *)
(*-------------------------------------------------------------------------- *)

let mk_e e ii = Ast_c.mk_e e ii

let mk_string_wrap (s,info) = (s, [info])

(*-------------------------------------------------------------------------- *)
(* support for functions with no return type *)
(*-------------------------------------------------------------------------- *)

let args_are_params l =
  match l with
    [Right (ArgAction(ActMisc [x])), ii] when Ast_c.is_fake x -> true
  | _ -> List.for_all (function Right (ArgType x), ii -> true | _ -> false) l
let args_to_params l pb =
  let pi =
    match pb with Some pb -> Ast_c.parse_info_of_info pb | None -> fake_pi in
  match l with
    [(Right (ArgAction(ActMisc [x])), ii)] when Ast_c.is_fake x -> []
  | l ->
      List.map
	(function
	    Right (ArgType x), ii -> x, ii
	  | x ->
	      raise
		(Semantic
		   ("function with no return type must have types in param list",
		    pi)))
	l

%}

/*(*****************************************************************************)*/
/*(* Tokens *)*/
/*(*************************************************************************)*/

/*
(*
 * Some tokens are not even used in this file because they are filtered
 * in some intermediate phase. But they still must be declared because
 * ocamllex may generate them, or some intermediate phase may also
 * generate them (like some functions in parsing_hacks.ml)
 *)
*/

%token <Ast_c.info> TUnknown /*(* unrecognized token *)*/

/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Ast_c.info> TCommentSpace TCommentNewline   TComment

/*(*-----------------------------------------*)*/
/*(* the normal tokens *)*/
/*(*-----------------------------------------*)*/

%token <(string * (Ast_c.sign * Ast_c.base)) * Ast_c.info> TInt
%token <(string * Ast_c.floatType) * Ast_c.info> TFloat
%token <(string * Ast_c.isWchar) * Ast_c.info>   TChar
%token <(string * Ast_c.isWchar) * Ast_c.info>   TString
%token <(string * Ast_c.isWchar) * Ast_c.info>   TQuote
%token <Ast_c.info> TPct
%token <string * Ast_c.info> TFormat TSubString
%token <(string * string (*n*) * string (*p*)) * Ast_c.info> TDecimal

%token <string * Ast_c.info> TIdent
%token <string * Ast_c.info> TKRParam
%token <string * Ast_c.info> Tconstructorname /* parsing_hack for C++ */
/*(* appears mostly after some fix_xxx in parsing_hack *)*/
%token <string * Ast_c.info> TypedefIdent

/*
(* Some tokens like TOPar and TCPar are used as synchronisation stuff,
 * in parsing_hack.ml. So if define special tokens like TOParDefine and
 * TCParEOL, then take care to also modify in Token_helpers.
 *)
*/

%token <Ast_c.info> TOPar TCPar TOBrace TCBrace TOCro TCCro
%token <Ast_c.info> TDot TComma TPtrOp
%token <Ast_c.info> TInc TDec
%token <Ast_c.assignOp> TAssign
%token <Ast_c.info> TEq
%token <Ast_c.info> TWhy  TTilde TBang
%token <Ast_c.info> TEllipsis
%token <Ast_c.info> TDotDot

%token <Ast_c.info> TPtVirg
%token <Ast_c.info>
       TOrLog TAndLog TOr TXor TAnd  TEqEq TNotEq TInf TSup TInfEq TSupEq
       TShl TShr
       TPlus TMinus TMul TDiv TMod  TMax TMin

%token <Ast_c.info>
       Tchar Tshort Tint Tdouble Tfloat Tlong Tunsigned Tsigned Tvoid
       Tsize_t Tssize_t Tptrdiff_t
       Tauto Tregister Textern Tstatic
       Ttypedef
       Tconst Tvolatile
       Tstruct Tunion Tenum Tdecimal Texec
       Tbreak Telse Tswitch Tcase Tcontinue Tfor Tdo Tif  Twhile Treturn
       Tgoto Tdefault
       Tsizeof Tnew Tdelete Tdefined TOParCplusplusInit Tnamespace

/*(* C99 *)*/
%token <Ast_c.info>
       Trestrict

/*(*-----------------------------------------*)*/
/*(* gccext: extra tokens *)*/
/*(*-----------------------------------------*)*/
%token <Ast_c.info> Tasm
%token <Ast_c.info> Tattribute
%token <Ast_c.info> TattributeNoarg
%token <Ast_c.info> Tinline
%token <Ast_c.info> Ttypeof

/*(*-----------------------------------------*)*/
/*(* cppext: extra tokens *)*/
/*(*-----------------------------------------*)*/
/*(* coupling with Token_helpers.is_cpp_token *)*/


/*(*---------------*)*/
/*(* define        *)*/
/*(*---------------*)*/

%token <Ast_c.info> TDefine
%token <(string * Ast_c.info)> TDefParamVariadic

/*(* disappear after fix_tokens_define *)*/
%token <Ast_c.info> TCppEscapedNewline

%token <Ast_c.info> TCppConcatOp

/*(* appear    after fix_tokens_define *)*/
%token <Ast_c.info> TOParDefine
%token <Ast_c.info> TOBraceDefineInit

%token <(string * Ast_c.info)> TIdentDefine /*(* same *)*/
%token <Ast_c.info>            TDefEOL      /*(* same *)*/


/*(*---------------*)*/
/*(* include       *)*/
/*(*---------------*)*/


/*(* used only in lexer_c, then transformed in comment or splitted in tokens *)*/
%token <(string * string * bool ref * Ast_c.info)> TInclude

/*(* tokens coming from above, generated in parse_c from TInclude, etc *)*/
%token <(Ast_c.info * bool ref)> TIncludeStart
%token <(string * Ast_c.info)> TIncludeFilename


/*(*---------------*)*/
/*(* ifdef         *)*/
/*(*---------------*)*/

/*(* coupling: Token_helpers.is_cpp_instruction *)*/
%token <(Ast_c.ifdef_guard * (int * int) option ref * Ast_c.info)>
  TIfdef TIfdefelif
%token <((int * int) option ref * Ast_c.info)>
  TIfdefelse TEndif
%token <(bool * (int * int) option ref * Ast_c.info)>
  TIfdefBool TIfdefMisc TIfdefVersion

/* Note [Nasty Undisciplined Cpp]
 *
 * These tokens replace regular Cpp-ifdef tokens for nasty undisciplined
 * variability patterns.
 *
 * Note that these tokens do not have matching_tag.
 * (TU stands for Token-Undisciplined.)
 *
 * /Iago
 */
%token <Ast_c.info>
  TUifdef TUelseif TUendif

/*(*---------------*)*/
/*(* other         *)*/
/*(*---------------*)*/

%token <Ast_c.info> TUndef
%token <Ast_c.info> TPragma

%token <Ast_c.info> TCppDirectiveOther

/*(*---------------*)*/
/*(* macro use     *)*/
/*(*---------------*)*/

/*(* appear  after fix_tokens_cpp, cf also parsing_hacks#hint *)*/

%token <(string * Ast_c.info)>            TMacroAttr
%token <(string * Ast_c.info)>            TMacroStmt
%token <(string * Ast_c.info)> TMacroIdentBuilder
/*(* no need value for the moment *)*/
%token <(string * Ast_c.info)>            TMacroString
%token <(string * Ast_c.info)> TMacroDecl
%token <Ast_c.info>            TMacroDeclConst

%token <(string * Ast_c.info)> TMacroIterator
/*(*
%token <(string * Ast_c.info)> TMacroTop
%token <(string * Ast_c.info)> TMacroStructDecl
*)*/

%token <(string * Ast_c.info)>            TMacroAttrStorage


/*(*---------------*)*/
/*(* other         *)*/
/*(*---------------*)*/


/*(* should disappear after parsing_hack *)*/
%token <Ast_c.info> TCommentSkipTagStart TCommentSkipTagEnd


/*(* appear after parsing_hack *)*/
%token <Ast_c.info> TCParEOL

%token <Ast_c.info> TAction


/*(* TCommentMisc still useful ? obsolete ? *)*/
%token <Ast_c.info> TCommentMisc
%token <(Token_c.cppcommentkind * Ast_c.info)> TCommentCpp


/*(*-----------------------------------------*)*/
%token <Ast_c.info> EOF

/*(*-----------------------------------------*)*/

/*(* must be at the top so that it has the lowest priority *)*/
%nonassoc SHIFTHERE

%nonassoc Telse

%left TOrLog
%left TAndLog
%left TOr
%left TXor
%left TAnd
%left TEqEq TNotEq
%left TInf TSup TInfEq TSupEq
%left TShl TShr
%left TPlus TMinus
%left TMul TDiv TMod TMin TMax

/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start main celem statement expr type_name
%type <Ast_c.program> main
%type <Ast_c.toplevel> celem

%type <Ast_c.statement> statement
%type <Ast_c.expression> expr
%type <Ast_c.fullType> type_name

%%
/*(*************************************************************************)*/
/*
(* TOC:
 * toplevel (obsolete)
 *
 * ident
 * expression
 * statement
 * types with
 *   - left part (type_spec, qualif),
 *   - right part (declarator, abstract declarator)
 *   - aux part (parameters)
 * declaration, storage, initializers
 * struct
 * enum
 * cpp directives
 * celem (=~ main)
 *
 * generic workarounds (obrace, cbrace for context setting)
 * xxx_list, xxx_opt
 *)
*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* toplevel *)*/
/*(*************************************************************************)*/
/*(* no more used; now that use error recovery *)*/

main:
 translation_unit EOF     { $1 }

translation_unit:
 |
     { [] }
 | translation_unit external_declaration
     { !LP._lexer_hint.context_stack <- [LP.InTopLevel]; $1 @ [$2] }
 | translation_unit Tnamespace TIdent TOBrace translation_unit TCBrace
     { !LP._lexer_hint.context_stack <- [LP.InTopLevel];
       $1 @ [Namespace ($5, [$2; snd $3; $4; $6])] }


/*(*************************************************************************)*/
/*(* ident *)*/
/*(*************************************************************************)*/

/*(* Why this ? Why not s/ident/TIdent ? cos there is multiple namespaces in C,
   * so a label can have the same name that a typedef, same for field and tags
   * hence sometimes the use of ident  instead of TIdent.
   *)*/
ident:
 | TIdent       { $1 }
 | TypedefIdent { $1 }


identifier:
 | TIdent       { $1 }

/*
(* cppext: string concatenation of idents
 * also cppext: gccext: ##args for variadic macro
 *)
*/
identifier_cpp:
 | TIdent
     { RegularName (mk_string_wrap $1) }
 | ident_extra_cpp { $1 }

ident_cpp:
 | TIdent
     { RegularName (mk_string_wrap $1) }
 | TypedefIdent
     { RegularName (mk_string_wrap $1) }
 | ident_extra_cpp { $1 }

ident_extra_cpp:
 | TIdent TCppConcatOp identifier_cpp_list
     {
       CppConcatenatedName (
         match $3 with
         | [] -> raise (Impossible 87)
         | (x,concatnull)::xs ->
             assert (concatnull = []);
             (mk_string_wrap $1, [])::(x,[$2])::xs
       )
   }
 | TCppConcatOp TIdent
     { CppVariadicName (fst $2, [$1; snd $2]) }
 | TMacroIdentBuilder TOPar param_define_list TCPar
     { CppIdentBuilder ((fst $1, [snd $1;$2;$4]), $3) }

identifier_cpp_list:
 | TIdent { [mk_string_wrap $1, []] }
 | identifier_cpp_list TCppConcatOp TIdent { $1 @ [mk_string_wrap $3, [$2]] }

/*(*************************************************************************)*/
/*(* expr *)*/
/*(*************************************************************************)*/

expr:
 | assign_expr             { $1 }
 | expr TComma assign_expr { mk_e (Sequence ($1,$3)) [$2] }

/*(* bugfix: in C grammar they put unary_expr, but in fact it must be
   * cast_expr, otherwise (int * ) xxx = &yy; is not allowed
   *)*/
assign_expr:
 | cond_expr                     { $1 }
 | cast_expr TAssign assign_expr { mk_e(Assignment ($1, $2, $3)) []}
 | cast_expr TEq     assign_expr { mk_e (Assignment ($1, (SimpleAssign, [$2]),$3)) []}

/*(* gccext: allow optional then part hence gcc_opt_expr
   * bugfix: in C grammar they put TDotDot cond_expr, but in fact it must be
   * assign_expr, otherwise   pnp ? x : x = 0x388  is not allowed
   *)*/
cond_expr:
 | arith_expr
     { $1 }
 | arith_expr TWhy gcc_opt_expr TDotDot assign_expr
     { mk_e (CondExpr ($1,$3,$5)) [$2;$4] }


arith_expr:
 | cast_expr                     { $1 }
 | arith_expr TMul    arith_expr { mk_e(Binary ($1, (Arith Mul,[$2]), $3)) [] }
 | arith_expr TDiv    arith_expr { mk_e(Binary ($1, (Arith Div, [$2]), $3)) [] }
 | arith_expr TMin    arith_expr { mk_e(Binary ($1, (Arith Min, [$2]), $3)) [] }
 | arith_expr TMax    arith_expr { mk_e(Binary ($1, (Arith Max, [$2]), $3)) [] }
 | arith_expr TMod    arith_expr { mk_e(Binary ($1, (Arith Mod, [$2]), $3)) [] }
 | arith_expr TPlus   arith_expr { mk_e(Binary ($1, (Arith Plus, [$2]), $3)) [] }
 | arith_expr TMinus  arith_expr { mk_e(Binary ($1, (Arith Minus, [$2]), $3)) [] }
 | arith_expr TShl    arith_expr { mk_e(Binary ($1, (Arith DecLeft, [$2]), $3)) [] }
 | arith_expr TShr    arith_expr { mk_e(Binary ($1, (Arith DecRight, [$2]), $3)) [] }
 | arith_expr TInf    arith_expr { mk_e(Binary ($1, (Logical Inf, [$2]), $3)) [] }
 | arith_expr TSup    arith_expr { mk_e(Binary ($1, (Logical Sup, [$2]), $3)) [] }
 | arith_expr TInfEq  arith_expr { mk_e(Binary ($1, (Logical InfEq, [$2]), $3)) [] }
 | arith_expr TSupEq  arith_expr { mk_e(Binary ($1, (Logical SupEq, [$2]), $3)) [] }
 | arith_expr TEqEq   arith_expr { mk_e(Binary ($1, (Logical Eq, [$2]), $3)) [] }
 | arith_expr TNotEq  arith_expr { mk_e(Binary ($1, (Logical NotEq, [$2]), $3)) [] }
 | arith_expr TAnd    arith_expr { mk_e(Binary ($1, (Arith And, [$2]), $3)) [] }
 | arith_expr TOr     arith_expr { mk_e(Binary ($1, (Arith Or, [$2]), $3)) [] }
 | arith_expr TXor    arith_expr { mk_e(Binary ($1, (Arith Xor, [$2]), $3)) [] }
 | arith_expr TAndLog arith_expr { mk_e(Binary ($1, (Logical AndLog, [$2]), $3)) [] }
 | arith_expr TOrLog  arith_expr { mk_e(Binary ($1, (Logical OrLog, [$2]), $3)) [] }

cast_expr:
 | unary_expr                        { $1 }
 | topar2 type_name tcpar2 cast_expr { mk_e(Cast ($2, $4)) [$1;$3] }

unary_expr:
 | postfix_expr                    { $1 }
 | TInc unary_expr                 { mk_e(Infix ($2, Inc))    [$1] }
 | TDec unary_expr                 { mk_e(Infix ($2, Dec))    [$1] }
 | unary_op cast_expr              { mk_e(Unary ($2, fst $1)) [snd $1] }
 | Tsizeof unary_expr              { mk_e(SizeOfExpr ($2))    [$1] }
 | Tsizeof topar2 type_name tcpar2 { mk_e(SizeOfType ($3))    [$1;$2;$4] }
 | Tnew new_argument               { mk_e(New (None, $2))     [$1] }
 | Tnew TOPar argument_list_ne TCPar new_argument { mk_e(New (Some $3, $5))             [$1; $2; $4] }
 | Tdelete cast_expr               { mk_e(Delete $2)          [$1] }
 | Tdefined identifier_cpp         { mk_e(Defined $2)         [$1] }
 | Tdefined TOPar identifier_cpp TCPar
 { mk_e(Defined $3) [$1;$2;$4] }

new_argument:
 | TIdent TOPar argument_list_ne TCPar
     { let fn = mk_e(Ident (RegularName (mk_string_wrap $1))) [] in
       Left (mk_e(FunCall (fn, $3)) [$2;$4]) }
 | TIdent TOPar TCPar
     { let fn = mk_e(Ident (RegularName (mk_string_wrap $1))) [] in
       Left(mk_e(FunCall (fn, [])) [$2;$3]) }
 | TypedefIdent TOPar argument_list_ne TCPar
     { let fn = mk_e(Ident (RegularName (mk_string_wrap $1))) [] in
       Left (mk_e(FunCall (fn, $3)) [$2;$4]) }
 | TypedefIdent TOPar TCPar
     { let fn = mk_e(Ident (RegularName (mk_string_wrap $1))) [] in
       Left (mk_e(FunCall (fn, [])) [$2;$3]) }
 | type_spec
     { let ty = addTypeD ($1,nullDecl) in
       let ((returnType,hasreg), iihasreg) = fixDeclSpecForParam ty in
       Right (ArgType { p_namei = None; p_type = returnType;
			p_register = hasreg, iihasreg;
		      } )
     }
 | new_argument TOCro expr TCCro
     {
       match $1 with
	 Left(e) -> Left(mk_e(ArrayAccess (e, $3)) [$2;$4])
       | Right(ArgType(ty)) -> (* lots of hacks to make the right type *)
	   let fty = mk_ty (Array (Some $3, ty.Ast_c.p_type)) [$2;$4] in
	   let pty = { ty with p_type = fty } in
	   Right(ArgType pty)
       | _ -> raise (Impossible 88)
     }

unary_op:
 | TAnd   { GetRef,     $1 }
 | TMul   { DeRef,      $1 }
 | TPlus  { UnPlus,     $1 }
 | TMinus { UnMinus,    $1 }
 | TTilde { Tilde,      $1 }
 | TBang  { Not,        $1 }
 /*(* gccext: have that a lot in old kernel to get address of local label.
    * cf gcc manual "local labels as values".
    *)*/
 | TAndLog { GetRefLabel, $1 }

postfix_expr:
 | primary_expr               { $1 }
 | postfix_expr TOCro expr TCCro
     { mk_e(ArrayAccess ($1, $3)) [$2;$4] }
 | postfix_expr TOPar argument_list_ne TCPar
     { mk_e(FunCall ($1, $3)) [$2;$4] }
 | postfix_expr TOPar  TCPar  { mk_e(FunCall ($1, [])) [$2;$3] }
 | postfix_expr TDot   ident_cpp { mk_e(RecordAccess   ($1,$3)) [$2] }
 | postfix_expr TPtrOp ident_cpp { mk_e(RecordPtAccess ($1,$3)) [$2] }
 | postfix_expr TInc          { mk_e(Postfix ($1, Inc)) [$2] }
 | postfix_expr TDec          { mk_e(Postfix ($1, Dec)) [$2] }

 /*(* gccext: also called compound literals *)*/
 | topar2 type_name tcpar2 TOBrace TCBrace
     { mk_e(Constructor ($2, (InitList [], [$4;$5]))) [$1;$3] }
 | topar2 type_name tcpar2 TOBrace initialize_list gcc_comma_opt_struct TCBrace
     { mk_e(Constructor ($2, (InitList (List.rev $5),[$4;$7] @ $6))) [$1;$3] }


primary_expr:
 | identifier_cpp  { mk_e(Ident  ($1)) [] }
 | TInt
    { let (str,(sign,base)) = fst $1 in
      mk_e(Constant (Int (str,Si(sign,base)))) [snd $1] }
 | TFloat  { mk_e(Constant (Float  (fst $1))) [snd $1] }
 | TString { mk_e(Constant (String (fst $1))) [snd $1] }
 | TQuote string_fragments TQuote
     { let ((fullstring,isW),lqinfo) = $1 in
       let (_,rqinfo) = $3 in
       mk_e (Ast_c.StringConstant($2, fullstring, isW)) [lqinfo;rqinfo] }
 | TChar   { mk_e(Constant (Char   (fst $1))) [snd $1] }
 | TDecimal { let (a,b,c) = fst $1 in
              mk_e(Constant (DecimalConst (a,b,c))) [snd $1] }
 | TOPar expr TCPar { mk_e(ParenExpr ($2)) [$1;$3] }  /*(* forunparser: *)*/

 /*(* gccext: cppext: TODO better ast ? *)*/
 | TMacroString { mk_e(Constant (MultiString [fst $1])) [snd $1] }
 | string_elem string_list
     { mk_e(Constant (MultiString ["TODO: MultiString"])) ($1 @ $2) }

 /*(* gccext: allow statement as expressions via ({ statement }) *)*/
 | TOPar compound TCPar  { mk_e(StatementExpr ($2)) [$1;$3] }

string_fragments:
 | /* empty */ { [] }
 | string_fragment string_fragments { $1 :: $2 }

string_fragment:
 | TPct string_format { Ast_c.FormatFragment($2), [$1] }
 | TSubString { Ast_c.ConstantFragment(fst $1), [snd $1] }

string_format:
 | TFormat { Ast_c.ConstantFormat(fst $1), [snd $1] }

/*(*----------------------------*)*/
/*(* cppext: *)*/
/*(*----------------------------*)*/

/*(* cppext: *)*/
/*(* to avoid conflicts have to introduce a _not_empty (ne) version *)*/
argument_ne:
 | assign_expr { Left $1 }
 | parameter_decl { Right (ArgType $1)  }
 | action_higherordermacro_ne { Right (ArgAction $1) }


argument:
 | assign_expr { Left $1 }
 | parameter_decl { Right (ArgType $1)  }
 /*(* had conflicts before, but julia fixed them *)*/
 | action_higherordermacro { Right (ArgAction $1) }

action_higherordermacro_ne:
 | taction_list_ne
     { if $1=[]
       then ActMisc [Ast_c.fakeInfo()]
       else ActMisc $1
     }


action_higherordermacro:
 | taction_list
     { if $1=[]
       then ActMisc [Ast_c.fakeInfo()]
       else ActMisc $1
     }


/*(*----------------------------*)*/
/*(* workarounds *)*/
/*(*----------------------------*)*/

/*(* would like evalInt $1 but require too much info *)*/
const_expr: cond_expr { $1  }


topar2: TOPar { et "topar2" (); $1  }
tcpar2: TCPar { et "tcpar2" (); $1 (*TODO? et ? sure ? c pas dt plutot ? *) }



/*(*************************************************************************)*/
/*(* statement *)*/
/*(*************************************************************************)*/

statement: statement2 { mk_st (fst $1) (snd $1) }

statement2:
 | labeled         { Labeled      (fst $1), snd $1 }
 | compound        { Compound     (fst $1), snd $1 }
 | expr_statement  { ExprStatement(fst $1), snd $1 }
 | selection       { Selection    (fst $1), snd $1 @ [fakeInfo()] }
 | iteration       { Iteration    (fst $1), snd $1 @ [fakeInfo()] }
 | jump TPtVirg    { Jump         (fst $1), snd $1 @ [$2] }

 /*(* gccext: *)*/
 | Tasm TOPar asmbody TCPar TPtVirg             { Asm $3, [$1;$2;$4;$5] }
 | Tasm Tvolatile TOPar asmbody TCPar TPtVirg   { Asm $4, [$1;$2;$3;$5;$6] }

 /*(* cppext: *)*/
 | TMacroStmt { MacroStmt, [snd $1] }

 | Texec identifier exec_list TPtVirg { Exec($3), [$1;snd $2;$4] }


/*(* note that case 1: case 2: i++;    would be correctly parsed, but with
   * a Case  (1, (Case (2, i++)))  :(
   *)*/
labeled:
 | ident_cpp        TDotDot sw_stat_or_decl   { Label ($1, $3),  [$2] }
 | Tcase const_expr TDotDot sw_stat_or_decl   { Case ($2, $4),       [$1; $3] }
 | Tcase const_expr TEllipsis const_expr TDotDot sw_stat_or_decl
     { CaseRange ($2, $4, $6), [$1;$3;$5] } /*(* gccext: allow range *)*/
 | Tdefault    TDotDot sw_stat_or_decl   { Default $3,             [$1; $2] }

sw_stat_or_decl:
 | decl      { mk_st (Decl ($1 Ast_c.LocalDecl)) Ast_c.noii }
 | statement { $1 }


end_labeled:
 /*(* gccext:  allow toto: }
    * was generating each 30 shift/Reduce conflicts,
    * mais ca va, ca fait ce qu'il faut.
    * update: julia fixed the problem by introducing end_labeled
    * and modifying below stat_or_decl_list
    *)*/
 | ident_cpp            TDotDot
     { Label ($1, (mk_st (ExprStatement None) Ast_c.noii)), [$2] }
 | Tcase const_expr TDotDot
     { Case ($2, (mk_st (ExprStatement None) Ast_c.noii)), [$1;$3] }
 | Tdefault         TDotDot
     { Default (mk_st (ExprStatement None) Ast_c.noii),    [$1; $2] }





compound: tobrace compound2 tcbrace { $2, [$1; $3]  }


/*
(* cppext: because of cpp, some stuff looks like declaration but are in
 * fact statement but too hard to figure out, and if parse them as
 * expression, then we force to have first decls and then exprs, then
 * will have a parse error. So easier to let mix decl/statement.
 * Moreover it helps to not make such a difference between decl and
 * statement for further coccinelle phases to factorize code.
*)*/
compound2:
 |                   { ([]) }
 | stat_or_decl_list { $1 }


stat_or_decl_list:
 | stat_or_decl                   { [$1] }
 /*(* gccext: to avoid conflicts, cf end_labeled above *)*/
 | end_labeled  { [StmtElem (mk_st (Labeled  (fst $1)) (snd $1))] }
 /*(* old: conflicts | stat_or_decl_list stat_or_decl { $1 @ [$2] } *)*/
 | stat_or_decl stat_or_decl_list { $1 :: $2 }

stat_or_decl:
 | decl      { StmtElem (mk_st (Decl ($1 Ast_c.LocalDecl)) Ast_c.noii) }
 | statement { StmtElem $1 }

  /*(* gccext: *)*/
 | function_definition { StmtElem (mk_st (NestedFunc $1) Ast_c.noii) }

 /* (* cppext: *)*/
 | cpp_directive
     { CppDirectiveStmt $1 }
 | cpp_ifdef_directive/*(* stat_or_decl_list ...*)*/
     { IfdefStmt $1 }


expr_statement:
 | TPtVirg      { None,    [$1] }
 | expr TPtVirg { Some $1, [$2] }

selection:
 | Tif TOPar expr TCPar statement              %prec SHIFTHERE
     { If ($3, $5, (mk_st (ExprStatement None) Ast_c.noii)),   [$1;$2;$4] }
 | Tif TOPar expr TCPar statement Telse statement
     { If ($3, $5, $7),  [$1;$2;$4;$6] }
 | Tswitch TOPar expr TCPar statement
     { Switch ($3,$5),   [$1;$2;$4]  }
 /* [Nasty Undisciplined Cpp] #ifdef A if e S1 else #endif S2 */
 | TUifdef Tif TOPar expr TCPar statement Telse TUendif statement
     { Ifdef_Ite ($4,$6,$9), [$1;$2;$3;$5;$7;$8] }
 /* [Nasty Undisciplined Cpp] #ifdef A if e S1 else #else S2 #endif S3 */
 | TUifdef Tif TOPar expr TCPar statement Telse TUelseif statement TUendif statement
     { Ifdef_Ite2 ($4,$6,$9,$11), [$1;$2;$3;$5;$7;$8;$10] }

iteration:
 | Twhile TOPar expr TCPar statement
     { While ($3,$5),                [$1;$2;$4] }
 | Tdo statement Twhile TOPar expr TCPar TPtVirg
     { DoWhile ($2,$5),              [$1;$3;$4;$6;$7] }
 | Tfor TOPar expr_statement expr_statement TCPar statement
     { For (ForExp $3,$4,(None, []),$6),    [$1;$2;$5]}
 | Tfor TOPar expr_statement expr_statement expr TCPar statement
     { For (ForExp $3,$4,(Some $5, []),$7), [$1;$2;$6] }
 /*(* C++ext: for(int i = 0; i < n; i++)*)*/
 | Tfor TOPar decl expr_statement TCPar statement
     { For (ForDecl ($3 Ast_c.LocalDecl),$4,(None, []),$6),    [$1;$2;$5]}
 | Tfor TOPar decl expr_statement expr TCPar statement
     { For (ForDecl ($3 Ast_c.LocalDecl),$4,(Some $5, []),$7), [$1;$2;$6] }
 /*(* cppext: *)*/
 | TMacroIterator TOPar argument_list_ne TCPar statement
     { MacroIteration (fst $1, $3, $5), [snd $1;$2;$4] }
 | TMacroIterator TOPar TCPar statement
     { MacroIteration (fst $1, [], $4), [snd $1;$2;$3] }

/*(* the ';' in the caller grammar rule will be appended to the infos *)*/
jump:
 | Tgoto ident_cpp  { Goto ($2),  [$1] }
 | Tcontinue    { Continue,       [$1] }
 | Tbreak       { Break,          [$1] }
 | Treturn      { Return,         [$1] }
 | Treturn expr { ReturnExpr $2,  [$1] }
 | Tgoto TMul expr { GotoComputed $3, [$1;$2] }



/*(*----------------------------*)*/
/*(* gccext: *)*/
/*(*----------------------------*)*/
string_elem:
 | TString { [snd $1] }
 /*(* cppext:  ex= printk (KERN_INFO "xxx" UTS_RELEASE)  *)*/
 | TMacroString { [snd $1] }


asmbody:
 | string_list colon_asm_list  { $1, $2 }
 | string_list { $1, [] } /*(* in old kernel *)*/


colon_asm: TDotDot colon_option_list { Colon $2, [$1]   }

colon_option:
 | TString                      { ColonMisc, [snd $1] }
 | TString TOPar asm_expr TCPar { ColonExpr $3, [snd $1; $2;$4] }
 /*(* cppext: certainly a macro *)*/
 | TOCro identifier TCCro TString TOPar asm_expr TCPar
     { ColonExpr $6, [$1;snd $2;$3;snd $4; $5; $7 ] }
 | identifier                       { ColonMisc, [snd $1] }
 | /*(* empty *)*/                  { ColonMisc, [] }

// IBM C only
exec_list:
    /* empty */ { [] }
  | TDotDot identifier_cpp exec_ident exec_list
      { (ExecEval ($3 (mk_e (Ident $2) [])), [$1]) :: $4 }
  | TIdent exec_ident2 exec_list
      { (ExecToken, [snd $1]) :: $2 @ $3 }
  | token exec_list { (ExecToken, [$1]) :: $2 }

exec_ident:
    { function prev -> prev }
 | TDot   TIdent exec_ident
     { function prev ->
       let fld = RegularName (mk_string_wrap $2) in
       $3 (mk_e(RecordAccess   (prev,fld)) [$1]) }
 | TPtrOp TIdent exec_ident
     { function prev ->
       let fld = RegularName (mk_string_wrap $2) in
       $3 (mk_e(RecordPtAccess   (prev,fld)) [$1]) }
 | TOCro expr TCCro exec_ident
     { function prev ->
       $4 (mk_e(ArrayAccess   (prev,$2)) [$1;$3]) }

exec_ident2:
    { [] }
 | TDot   TIdent exec_ident2
    { (ExecToken, [$1]) :: (ExecToken, [snd $2]) :: $3 }

asm_expr: assign_expr { $1 }

token:
    TPlus   { $1 }
  | TMinus  { $1 }
  | TMul    { $1 }
  | TDiv    { $1 }
  | TMod    { $1 }
  | TMin    { $1 }
  | TMax    { $1 }
  | TInc    { $1 }
  | TDec    { $1 }
  | TEq     { $1 }
  | TAssign { List.hd (snd $1) }

  | TEqEq   { $1 }
  | TNotEq  { $1 }
  | TSupEq  { $1 }
  | TInfEq  { $1 }
  | TSup    { $1 }
  | TInf    { $1 }

  | TAndLog { $1 }
  | TOrLog  { $1 }
  | TShr    { $1 }
  | TShl    { $1 }
  | TAnd    { $1 }
  | TOr     { $1 }
  | TXor    { $1 }

  | TOBrace { $1 }
  | TCBrace { $1 }
/*  | TOCro   { $1 }
  | TCCro   { $1 }*/
  | TOPar   { $1 }
  | TCPar   { $1 }

/*| TPtrOp  { $1 }
  | TDot    { $1 }*/
  | TWhy    { $1 }
  | TBang   { $1 }
  | TComma  { $1 }
/*  | TIdent  { snd $1 }*/
  | TypedefIdent { snd $1 }

  | Tif     { $1 }
  | Telse   { $1 }
  | TInt    { snd $1 }
  | TFloat  { snd $1 }
  | TString { snd $1 }
  | TChar   { snd $1 } /* other constants needed? */

/*(*************************************************************************)*/
/*(* types *)*/
/*(*************************************************************************)*/


/*(*-----------------------------------------------------------------------*)*/
/*(* Type spec, left part of a type *)*/
/*(*-----------------------------------------------------------------------*)*/
type_spec2:
 | Tvoid                { Right3 (BaseType Void),            [$1] }
 | Tchar                { Right3 (BaseType (IntType CChar)), [$1]}
 | Tint                 { Right3 (BaseType (IntType (Si (Signed,CInt)))), [$1]}
 | Tfloat               { Right3 (BaseType (FloatType CFloat)),  [$1]}
 | Tdouble              { Right3 (BaseType (FloatType CDouble)), [$1] }
 | Tsize_t              { Right3 (BaseType SizeType),            [$1] }
 | Tssize_t             { Right3 (BaseType SSizeType),           [$1] }
 | Tptrdiff_t           { Right3 (BaseType PtrDiffType),         [$1] }
 | Tshort               { Middle3 Short,  [$1]}
 | Tlong                { Middle3 Long,   [$1]}
 | Tsigned              { Left3 Signed,   [$1]}
 | Tunsigned            { Left3 UnSigned, [$1]}
 | struct_or_union_spec { Right3 (fst $1), snd $1 }
 | enum_spec            { Right3 (fst $1), snd $1 }
 | Tdecimal TOPar const_expr TComma const_expr TCPar
     { Right3 (Decimal($3,Some $5)), [$1;$2;$4;$6] }
 | Tdecimal TOPar const_expr TCPar
     { Right3 (Decimal($3,None)), [$1;$2;$4] }

 /*
 (* parse_typedef_fix1: cant put: TIdent {} cos it make the grammar
  * ambiguous, generates lots of conflicts => we must
  * use some tricks: we make the lexer and parser cooperate, cf lexerParser.ml.
  *
  * parse_typedef_fix2: this is not enough, and you must use
  * parse_typedef_fix2 to fully manage typedef problems in grammar.
  *
  * parse_typedef_fix3:
  *
  * parse_typedef_fix4: try also to do now some consistency checking in
  * Parse_c
  *)*/
 | TypedefIdent
     { let name = RegularName (mk_string_wrap $1) in
       Right3 (TypeName (name, Ast_c.noTypedefDef())),[] }

 | Ttypeof TOPar assign_expr TCPar { Right3 (TypeOfExpr ($3)), [$1;$2;$4] }
 | Ttypeof TOPar type_name   TCPar { Right3 (TypeOfType ($3)), [$1;$2;$4] }

/*(*----------------------------*)*/
/*(* workarounds *)*/
/*(*----------------------------*)*/

type_spec: type_spec2    { dt "type" (); $1   }

/*(*-----------------------------------------------------------------------*)*/
/*(* Qualifiers *)*/
/*(*-----------------------------------------------------------------------*)*/

type_qualif:
 | Tconst    { {const=true  ; volatile=false}, $1 }
 | Tvolatile { {const=false ; volatile=true},  $1 }
 /*(* C99 *)*/
 | Trestrict { (* TODO *) {const=false ; volatile=false},  $1 }


/*(*-----------------------------------------------------------------------*)*/
/*(* gccext: attributes  *)*/
/*(*-----------------------------------------------------------------------*)*/

attribute:
 | Tattribute TOPar /*stuff*/ TCPar { raise Todo }
 /*(* cppext: *)*/
 | TMacroAttr { Attribute (fst $1), [snd $1] }

attribute_storage:
 | TMacroAttrStorage { $1 }

type_qualif_attr:
 | type_qualif { $1 }
/*(*TODO !!!!! *)*/
 | TMacroAttr { {const=true  ; volatile=false}, snd $1   }

/*(*-----------------------------------------------------------------------*)*/
/*(* Declarator, right part of a type + second part of decl (the ident)  *)*/
/*(*-----------------------------------------------------------------------*)*/

/*
(* declarator return a couple:
 *  (name, partial type (a function to be applied to return type))
 *
 * when int* f(int) we must return Func(Pointer int,int) and not
 * Pointer (Func(int,int)
 *)*/

declarator:
 | pointer direct_d { (fst $2, fun x -> x +> $1 +> (snd $2)  ) }
 | direct_d         { $1  }

/*(* so must do  int * const p; if the pointer is constant, not the pointee *)*/
pointer:
 | tmul                   { fun x -> mk_ty (Pointer x) [$1] }
 | tmul pointer           { fun x -> mk_ty (Pointer ($2 x)) [$1] }
 | tmul type_qualif_list
     { fun x -> ($2.qualifD, mk_tybis (Pointer x) [$1])}
 | tmul type_qualif_list pointer
     { fun x -> ($2.qualifD, mk_tybis (Pointer ($3 x)) [$1]) }

tmul:
   TMul { $1 }
 | TAnd
     { if !Flag.c_plus_plus
     then $1
     else
       let i = Ast_c.parse_info_of_info $1 in
       raise (Semantic("& not allowed in C types, try -c++ option", i)) }


direct_d:
 | identifier_cpp
     { ($1, fun x -> x) }
 | TOPar declarator TCPar      /*(* forunparser: old: $2 *)*/
     { (fst $2, fun x -> mk_ty (ParenType ((snd $2) x)) [$1;$3]) }
 | direct_d tocro            tccro
     { (fst $1,fun x->(snd $1) (mk_ty (Array (None,x)) [$2;$3])) }
 | direct_d tocro const_expr tccro
     { (fst $1,fun x->(snd $1) (mk_ty (Array (Some $3,x)) [$2;$4])) }
 | direct_d topar            tcpar
     { (fst $1,
       fun x->(snd $1)
         (mk_ty (FunctionType (x,(([],(false, []))))) [$2;$3]))
     }
 | direct_d topar parameter_type_list tcpar
     { (fst $1,fun x->(snd $1)
       (mk_ty (FunctionType (x, $3)) [$2;$4]))
     }


/*(*----------------------------*)*/
/*(* workarounds *)*/
/*(*----------------------------*)*/

tocro: TOCro { et "tocro" ();$1 }
tccro: TCCro { dt "tccro" ();$1 }

/*(*-----------------------------------------------------------------------*)*/
abstract_declarator:
 | pointer                            { $1 }
 |         direct_abstract_declarator { $1 }
 | pointer direct_abstract_declarator { fun x -> x +> $2 +> $1 }

direct_abstract_declarator:
 | TOPar abstract_declarator TCPar /*(* forunparser: old: $2 *)*/
     { fun x -> mk_ty (ParenType ($2 x)) [$1;$3] }

 | TOCro            TCCro
     { fun x -> mk_ty (Array (None, x)) [$1;$2] }
 | TOCro const_expr TCCro
     { fun x -> mk_ty (Array (Some $2, x)) [$1;$3] }
 | direct_abstract_declarator TOCro            TCCro
     { fun x -> $1 (mk_ty (Array (None, x))  [$2;$3]) }
 | direct_abstract_declarator TOCro const_expr TCCro
     { fun x -> $1 (mk_ty (Array (Some $3,x))  [$2;$4]) }
 | TOPar TCPar
     { fun x -> mk_ty (FunctionType (x, ([], (false,  [])))) [$1;$2] }
 | topar parameter_type_list tcpar
     { fun x -> mk_ty (FunctionType (x, $2))  [$1;$3] }
/*(* subtle: here must also use topar, not TOPar, otherwise if have for
   * instance   (xxx ( * )(xxx)) cast, then the second xxx may still be a Tident
   * but we want to reduce topar, to set the InParameter so that
   * parsing_hack can get a chance to change the type of xxx into a typedef.
   * That's an example where parsing_hack and the lookahead of ocamlyacc does
   * not go very well together ... we got the info too late. We got
   * a similar pb with xxx xxx; declaration, cf parsing_hack.ml and the
   * "disable typedef cos special case ..." message.
*)*/
 | direct_abstract_declarator topar tcpar
     { fun x -> $1 (mk_ty (FunctionType (x, (([], (false, []))))) [$2;$3]) }
 | direct_abstract_declarator topar parameter_type_list tcpar
     { fun x -> $1 (mk_ty (FunctionType (x, $3)) [$2;$4]) }

/*(*-----------------------------------------------------------------------*)*/
/*(* Parameters (use decl_spec not type_spec just for 'register') *)*/
/*(*-----------------------------------------------------------------------*)*/
parameter_type_list:
 | parameter_list                  { ($1, (false, []))}
 | parameter_list TComma TEllipsis { ($1, (true,  [$2;$3])) }


parameter_decl2:
   TKRParam {
     let name = RegularName (mk_string_wrap $1) in
     LP.add_ident (str_of_name name);
     { p_namei = Some name;
       p_type = mk_ty NoType [];
       p_register = (false, []);
     }
   }
 | decl_spec declaratorp
     { let ((returnType,hasreg),iihasreg) = fixDeclSpecForParam $1 in
       let (name, ftyp) = $2 in
       { p_namei = Some (name);
         p_type = ftyp returnType;
         p_register = (hasreg, iihasreg);
       }
     }
 | decl_spec abstract_declaratorp
     { let ((returnType,hasreg), iihasreg) = fixDeclSpecForParam $1 in
       { p_namei = None;
         p_type = $2 returnType;
         p_register = hasreg, iihasreg;
       }
     }
 | decl_spec
     { let ((returnType,hasreg), iihasreg) = fixDeclSpecForParam $1 in
       { p_namei = None;
         p_type = returnType;
         p_register = hasreg, iihasreg;
       }
     }


/*(*----------------------------*)*/
/*(* workarounds *)*/
/*(*----------------------------*)*/

parameter_decl: parameter_decl2 { et "param" ();  $1 }
 | attributes parameter_decl2 { et "param" (); $2 }

declaratorp:
 | declarator  { LP.add_ident (str_of_name (fst $1)); $1 }
 /*(* gccext: *)*/
 | attributes declarator   { LP.add_ident (str_of_name (fst $2)); $2 }
 | declarator attributes   { LP.add_ident (str_of_name (fst $1)); $1 }

abstract_declaratorp:
 | abstract_declarator { $1 }
 /*(* gccext: *)*/
 | attributes abstract_declarator { $2 }

/*(*-----------------------------------------------------------------------*)*/
/*(* helper type rules *)*/
/*(*-----------------------------------------------------------------------*)*/

/*(* for struct and also typename *)*/
/*(* cant put decl_spec cos no storage is allowed for field struct *)*/
spec_qualif_list2:
 | type_spec                    { addTypeD ($1, nullDecl) }
 | type_qualif                  { {nullDecl with qualifD = (fst $1,[snd $1])}}
 | type_spec   spec_qualif_list { addTypeD ($1,$2)   }
 | type_qualif spec_qualif_list { addQualifD ($1,$2) }

spec_qualif_list: spec_qualif_list2            {  dt "spec_qualif" (); $1 }


/*(* for pointers in direct_declarator and abstract_declarator *)*/
type_qualif_list:
 | type_qualif_attr                  { {nullDecl with qualifD = (fst $1,[snd $1])} }
 | type_qualif_list type_qualif_attr { addQualifD ($2,$1) }






/*(*-----------------------------------------------------------------------*)*/
/*(* xxx_type_id *)*/
/*(*-----------------------------------------------------------------------*)*/

type_name:
 | spec_qualif_list
     { let (returnType, _) = fixDeclSpecForDecl $1 in  returnType }
 | spec_qualif_list abstract_declaratort
     { let (returnType, _) = fixDeclSpecForDecl $1 in $2 returnType }



abstract_declaratort:
 | abstract_declarator { $1 }
 /*(* gccext: *)*/
 | attributes abstract_declarator { $2 }


/*(*************************************************************************)*/
/*(* declaration and initializers *)*/
/*(*************************************************************************)*/

decl2:
 | decl_spec TPtVirg
     { function local ->
       let (returnType,storage) = fixDeclSpecForDecl $1 in
       let iistart = Ast_c.fakeInfo () in
       DeclList ([{v_namei = None; v_type = returnType;
                   v_storage = unwrap storage; v_local = local;
                   v_attr = Ast_c.noattr;
                   v_type_bis = ref None;
                },[]],
                ($2::iistart::snd storage))
     }
 | decl_spec init_declarator_list TPtVirg
     { function local ->
       let (returnType,storage) = fixDeclSpecForDecl $1 in
       let iistart = Ast_c.fakeInfo () in
       DeclList (
         ($2 +> List.map (fun ((((name,f),attrs), ini), iivirg) ->
           let s = str_of_name name in
	   if fst (unwrap storage) = StoTypedef
	   then LP.add_typedef s;
           {v_namei = Some (name, ini);
            v_type = f returnType;
            v_storage = unwrap storage;
            v_local = local;
            v_attr = attrs;
            v_type_bis = ref None;
           },
           iivirg
         )
         ),  ($3::iistart::snd storage))
     }
 /*(* cppext: *)*/

 | storage_const_opt TMacroDecl TOPar argument_list TCPar TPtVirg
     { function _ ->
       match $1 with
	 Some (sto,stoii) ->
	   MacroDecl
	     ((sto, fst $2, $4, true), (snd $2::$3::$5::$6::fakeInfo()::stoii))
       | None ->
	   MacroDecl
	     ((NoSto, fst $2, $4, true), [snd $2;$3;$5;$6;fakeInfo()]) }

 | storage_const_opt
     TMacroDecl TOPar argument_list TCPar teq initialize TPtVirg
     { function _ ->
       match $1 with
	 Some (sto,stoii) ->
	   MacroDeclInit
	     ((sto, fst $2, $4, $7),
	      (snd $2::$3::$5::$6::$8::fakeInfo()::stoii))
       | None ->
	   MacroDeclInit
	     ((NoSto, fst $2, $4, $7), [snd $2;$3;$5;$6;$8;fakeInfo()]) }

storage_const_opt:
   storage_class_spec_nt TMacroDeclConst { Some (fst $1,[snd $1; $2]) }
 | storage_class_spec_nt { Some (fst $1,[snd $1]) }
 |                       { None }

/*(*-----------------------------------------------------------------------*)*/
decl_spec2:
 | storage_class_spec      { {nullDecl with storageD = (fst $1, [snd $1]) } }
 | type_spec               { addTypeD ($1,nullDecl) }
 | type_qualif             { {nullDecl with qualifD  = (fst $1, [snd $1]) } }
 | Tinline                 { {nullDecl with inlineD = (true, [$1]) } }
 | storage_class_spec decl_spec2 { addStorageD ($1, $2) }
 | type_spec          decl_spec2 { addTypeD    ($1, $2) }
 | type_qualif        decl_spec2 { addQualifD  ($1, $2) }
 | Tinline            decl_spec2 { addInlineD ((true, $1), $2) }

/*(* can simplify by putting all in _opt ? must have at least one otherwise
   *  decl_list is ambiguous ? (no cos have ';' between decl)
   *)*/


storage_class_spec_nt:
 | Tstatic      { Sto Static,  $1 }
 | Textern      { Sto Extern,  $1 }
 | Tauto        { Sto Auto,    $1 }
 | Tregister    { Sto Register,$1 }

storage_class_spec2:
 | storage_class_spec_nt { $1 }
 | Ttypedef     { StoTypedef,  $1 }

storage_class_spec:
 /*(* gccext: *)*/
 | storage_class_spec2 { $1 }
 | storage_class_spec2 attribute_storage_list { $1 (* TODO *) }



/*(*----------------------------*)*/
/*(* workarounds *)*/
/*(*----------------------------*)*/

decl:      decl2         { et "decl" (); $1 }
decl_spec: decl_spec2    { dt "declspec" (); $1  }

/*(*-----------------------------------------------------------------------*)*/
/*(* declarators (right part of type and variable) *)*/
/*(*-----------------------------------------------------------------------*)*/
init_declarator2:
 | declaratori                  { ($1, NoInit) }
 | declaratori teq initialize   { ($1, ValInit($2, $3)) }
 /* C++ only */
 | declaratori TOParCplusplusInit argument_list TCPar
     { ($1, ConstrInit($3,[$2;$4])) }


/*(*----------------------------*)*/
/*(* workarounds *)*/
/*(*----------------------------*)*/
teq: TEq  { et "teq" (); $1 }

init_declarator: init_declarator2  { dt "init" (); $1 }


/*(*----------------------------*)*/
/*(* gccext: *)*/
/*(*----------------------------*)*/

declaratori:
 | declarator              { LP.add_ident (str_of_name (fst $1)); $1, Ast_c.noattr }
 /*(* gccext: *)*/
 | declarator gcc_asm_decl { LP.add_ident (str_of_name (fst $1)); $1, Ast_c.noattr }
 /*(* gccext: *)*/
 | attributes declarator   { LP.add_ident (str_of_name (fst $2)); $2, $1 }
 | declarator attributes   { LP.add_ident (str_of_name (fst $1)); $1, Ast_c.noattr (* TODO *) }



gcc_asm_decl:
 | Tasm TOPar asmbody TCPar              {  }
 | Tasm Tvolatile TOPar asmbody TCPar   {  }


/*(*-----------------------------------------------------------------------*)*/
initialize:
 | assign_expr
     { InitExpr $1,                [] }
 | tobrace_ini initialize_list gcc_comma_opt_struct  tcbrace_ini
     { InitList (List.rev $2),     [$1;$4] @ $3 }
 | tobrace_ini tcbrace_ini
     { InitList [],       [$1;$2] } /*(* gccext: *)*/


/*
(* opti: This time we use the weird order of non-terminal which requires in
 * the "caller" to do a List.rev cos quite critical. With this weird order it
 * allows yacc to use a constant stack space instead of exploding if we would
 * do a  'initialize2 Tcomma initialize_list'.
 *)
*/
initialize_list:
 | initialize2                        { [$1,   []] }
 | initialize_list TComma initialize2 { ($3,  [$2])::$1 }


/*(* gccext: condexpr and no assign_expr cos can have ambiguity with comma *)*/
initialize2:
 | cond_expr
     { InitExpr $1,   [] }
 | tobrace_ini initialize_list gcc_comma_opt_struct tcbrace_ini
     { InitList (List.rev $2),   [$1;$4] @ $3 }
 | tobrace_ini tcbrace_ini
     { InitList [],  [$1;$2]  }

 /*(* gccext: labeled elements, a.k.a designators *)*/
 | designator_list TEq initialize2
     { InitDesignators ($1, $3), [$2] }

 /*(* gccext: old format *)*/
 | ident TDotDot initialize2
     { InitFieldOld (fst $1, $3),     [snd $1; $2] } /*(* in old kernel *)*/
/* conflict
 | TOCro const_expr TCCro initialize2
     { InitIndexOld ($2, $4),    [$1;$3] }
*/



/*(* they can be nested, can have a .x[3].y *)*/
designator:
 | TDot ident
     { DesignatorField (fst $2), [$1;snd $2] }
 | TOCro const_expr TCCro
     { DesignatorIndex ($2),  [$1;$3] }
 | TOCro const_expr TEllipsis const_expr TCCro
     { DesignatorRange ($2, $4),  [$1;$3;$5] }


/*(*----------------------------*)*/
/*(* workarounds *)*/
/*(*----------------------------*)*/

gcc_comma_opt_struct:
 | TComma {  [$1] }
 | /*(* empty *)*/  {  [Ast_c.fakeInfo() +> Ast_c.rewrap_str ","]  }








/*(*************************************************************************)*/
/*(* struct *)*/
/*(*************************************************************************)*/

s_or_u_spec2:
 | struct_or_union ident tobrace_struct struct_decl_list_gcc tcbrace_struct
     { StructUnion (fst $1, Some (fst $2), $4),  [snd $1;snd $2;$3;$5]  }
 | struct_or_union       tobrace_struct struct_decl_list_gcc tcbrace_struct
     { StructUnion (fst $1, None, $3), [snd $1;$2;$4] }
 | struct_or_union ident
     { StructUnionName (fst $1, fst $2), [snd $1;snd $2] }

struct_or_union2:
 | Tstruct   { Struct, $1 }
 | Tunion    { Union, $1 }
 /*(* gccext: *)*/
 | Tstruct attributes   { Struct, $1 (* TODO *) }
 | Tunion  attributes   { Union, $1  (* TODO *) }



struct_decl2:
 | field_declaration { DeclarationField $1 }
 | TPtVirg { EmptyField $1  }

 /*(* no conflict ? no need for a TMacroStruct ? apparently not as at struct
    * the rule are slightly different.
    *)*/
 | identifier TOPar argument_list TCPar TPtVirg
     { MacroDeclField ((fst $1, $3), [snd $1;$2;$4;$5;fakeInfo()]) }

 /*(* cppext: *)*/
 | cpp_directive
     { CppDirectiveStruct $1 }
 | cpp_ifdef_directive/*(* struct_decl_list ... *)*/
     { IfdefStruct $1 }


field_declaration:
 | spec_qualif_list struct_declarator_list TPtVirg
     {
       let (returnType,storage) = fixDeclSpecForDecl $1 in
       if fst (unwrap storage) <> NoSto
       then internal_error "parsing dont allow this";

       let iistart = Ast_c.fakeInfo () in (* for parallelism with DeclList *)
       FieldDeclList ($2 +> (List.map (fun (f, iivirg) ->
         f returnType, iivirg))
                         ,[$3;iistart])
         (* dont need to check if typedef or func initialised cos
          * grammar dont allow typedef nor initialiser in struct
          *)
     }

 | spec_qualif_list TPtVirg
     {
       (* gccext: allow empty elements if it is a structdef or enumdef *)
       let (returnType,storage) = fixDeclSpecForDecl $1 in
       if fst (unwrap storage) <> NoSto
       then internal_error "parsing dont allow this";

       let iistart = Ast_c.fakeInfo () in (* for parallelism with DeclList *)
       FieldDeclList ([(Simple (None, returnType)) , []], [$2;iistart])
     }





struct_declarator:
 | declaratorsd
     { (fun x -> Simple   (Some (fst $1), (snd $1) x)) }
 | dotdot const_expr2
     { (fun x -> BitField (None, x, $1, $2)) }
 | declaratorsd dotdot const_expr2
     { (fun x -> BitField (Some (fst $1), ((snd $1) x), $2, $3)) }


/*(*----------------------------*)*/
/*(* workarounds *)*/
/*(*----------------------------*)*/
declaratorsd:
 | declarator { (*also ? LP.add_ident (fst (fst $1)); *) $1 }
 /*(* gccext: *)*/
 | attributes declarator   { $2 }
 | declarator attributes   { $1 }




struct_or_union_spec: s_or_u_spec2 { dt "su" (); $1 }
struct_or_union: struct_or_union2 { et "su" (); $1 }
struct_decl: struct_decl2  { et "struct" (); $1 }

dotdot: TDotDot  { et "dotdot" (); $1 }
const_expr2: const_expr { dt "const_expr2" (); $1 }

struct_decl_list_gcc:
 | struct_decl_list  { $1 }
 | /*(* empty *)*/       { [] } /*(* gccext: allow empty struct *)*/


/*(*************************************************************************)*/
/*(* enum *)*/
/*(*************************************************************************)*/
enum_spec:
 | Tenum        tobrace_enum enumerator_list gcc_comma_opt_struct tcbrace_enum
     { Enum (None,    $3),           [$1;$2;$5] @ $4 }
 | Tenum ident  tobrace_enum enumerator_list gcc_comma_opt_struct tcbrace_enum
     { Enum (Some (fst $2), $4),     [$1; snd $2; $3;$6] @ $5 }
 | Tenum ident
     { EnumName (fst $2),       [$1; snd $2] }

enumerator:
 | idente                 { $1, None     }
 | idente  TEq const_expr { $1, Some ($2, $3) }


/*(*----------------------------*)*/
/*(* workarounds *)*/
/*(*----------------------------*)*/

idente: ident_cpp { LP.add_ident (str_of_name $1); $1 }



/*(*************************************************************************)*/
/*(* function *)*/
/*(*************************************************************************)*/
function_definition: function_def    { fixFunc $1 }

decl_list:
 | decl           { [$1 Ast_c.LocalDecl]   }
 | decl_list decl { $1 @ [$2 Ast_c.LocalDecl] }

/* hack : to drop when a better solution is found */
cpp_directive_list:
 | cpp_directive                    { }
 | cpp_directive_list cpp_directive { }

function_def:
 | start_fun compound      { LP.del_scope(); ($1, $2, None) }
 | start_fun cpp_directive_list compound { LP.del_scope(); ($1, $3, None) }
 | start_fun decl_list compound      {
     (* TODO: undo the typedef added ? *)
     LP.del_scope();
     ($1, $3, Some $2)
   }

start_fun: start_fun2
  { LP.new_scope();
    fix_add_params_ident $1;
    (* toreput? !LP._lexer_hint.toplevel <- false;  *)
    $1
  }

start_fun2: decl_spec declaratorfd
     { let (returnType,storage) = fixDeclSpecForFuncDef $1 in
       let (id, attrs) = $2 in
       (fst id, fixOldCDecl ((snd id) returnType) , storage, attrs)
     }
   | ctor_dtor { $1 }

ctor_dtor:
 | Tconstructorname topar tcpar {
     let id = RegularName (mk_string_wrap $1) in
     let ret = mk_ty NoType [] in
     let ty = mk_ty (FunctionType (ret, (([], (false, []))))) [$2;$3] in
     let storage = ((NoSto,false),[]) in
     let attrs = [] in
     (id, ty, storage, attrs) }
 | Tconstructorname topar parameter_type_list tcpar {
     let id = RegularName (mk_string_wrap $1) in
     let ret = mk_ty NoType [] in
     let ty = mk_ty (FunctionType (ret, $3)) [$2;$4] in
     let storage = ((NoSto,false),[]) in
     let attrs = [] in
     (id, ty, storage, attrs) }

/*(*----------------------------*)*/
/*(* workarounds *)*/
/*(*----------------------------*)*/

/* It would be very nice if we could make declarator aware that this is
coming from a function definition.  Then on the ( and ) cases, it could
set the state to something other than InParameter.  Then the case
(TIdent (s, i1)::(TComma _|TCPar _)::_ , (TComma _ |TOPar _)::_ )
in parsing_hacks.ml would not have to consider K&R variable declarations
as typedefs.  Unfortunately, doing something about this problem seems to
introduce conflicts in the parser. */

declaratorfd:
 | declarator { et "declaratorfd" (); $1, Ast_c.noattr }
 /*(* gccext: *)*/
 | attributes declarator   { et "declaratorfd" (); $2, $1 }
 | declarator attributes   { et "declaratorfd" (); $1, Ast_c.noattr }



/*(*************************************************************************)*/
/*(* cpp directives *)*/
/*(*************************************************************************)*/

cpp_directive:
 | TIncludeStart TIncludeFilename
     {
       let (i1, in_ifdef) = $1 in
       let (s, i2) = $2 in

       (* redo some lexing work :( *)
       let inc_file =
         match () with
         | _ when s =~ "^\"\(.*\)\"$" ->
             Local (Common.split "/" (matched1 s))
         | _ when s =~ "^\<\(.*\)\>$" ->
             NonLocal (Common.split "/" (matched1 s))
         | _ ->
             Weird s
       in
       Include { i_include = (inc_file, [i1;i2]);
                 i_rel_pos = Ast_c.noRelPos();
                 i_is_in_ifdef = !in_ifdef;
                 i_content = Ast_c.noi_content;
       }
     }

 | TDefine TIdentDefine define_val TDefEOL
     { Define ((fst $2, [$1; snd $2;$4]), (DefineVar, $3)) }

 /*
 (* The TOParDefine is introduced to avoid ambiguity with previous rules.
  * A TOParDefine is a TOPar that was just next to the ident.
  *)*/
 | TDefine TIdentDefine TOParDefine param_define_list TCPar define_val TDefEOL
     { Define
         ((fst $2, [$1; snd $2; $7]),
           (DefineFunc ($4, [$3;$5]), $6))
     }

 | TUndef TIdentDefine TDefEOL
     { Define((fst $2, [$1; snd $2; $3]), (Undef,DefineEmpty)) }

 | TPragma TIdentDefine pragmainfo TDefEOL
     { Pragma((fst $2, [$1; snd $2; $4]), $3) }

 | TCppDirectiveOther { OtherDirective ([$1]) }

pragmainfo:
   TOPar argument_list_ne TCPar { (PragmaTuple ($2, [$1;$3])) }
 | TOPar TCPar { PragmaTuple ([], [$1;$2]) }
 | ident_define_list_ne { PragmaIdList $1 }

/*(* perhaps better to use assign_expr ? but in that case need
   * do a assign_expr_of_string in parse_c
   *)*/
define_val:
 | expr      { DefineExpr $1 }
 | statement { DefineStmt $1 }
 | decl      { DefineStmt (mk_st (Decl ($1 Ast_c.NotLocalDecl)) Ast_c.noii) }

/*(*old:
  * | TypedefIdent { DefineType (nQ,(TypeName(fst $1,noTypedefDef()),[snd $1]))}
  * get conflicts:
  * | spec_qualif_list TMul
  *   { let (returnType, _) = fixDeclSpecForDecl $1 in  DefineType returnType }
  *)
*/
 | decl_spec
     { let returnType = fixDeclSpecForMacro $1 in
       DefineType returnType
     }
 | decl_spec abstract_declarator
     { let returnType = fixDeclSpecForMacro $1 in
       let typ = $2 returnType in
       DefineType typ
     }

/*(* can be in conflict with decl_spec, maybe change fixDeclSpecForMacro
 * to also allow storage ?
 | storage_class_spec { DefineTodo }
 | Tinline { DefineTodo }
*)*/

 | stat_or_decl stat_or_decl_list
     { DefineMulti
	 (List.map
	    (function
		StmtElem e -> e
	      | _ -> failwith "unexpected statement for DefineMulti")
	    ($1 :: $2)) }
/*(*
 | statement statement { DefineTodo }
 | decl function_definition { DefineTodo }
*)*/




 | function_definition { DefineFunction $1 }

 | TOBraceDefineInit initialize_list gcc_comma_opt_struct TCBrace comma_opt
    { DefineInit (InitList (List.rev $2), [$1;$4] @ $3 @ $5)  }

 /*(* note: had a conflict before when were putting TInt instead of expr *)*/
 | Tdo statement Twhile TOPar expr TCPar
     {
       (* TOREPUT
       if fst $5 <> "0"
       then pr2 "WEIRD: in macro and have not a while(0)";
       *)
       DefineDoWhileZero (($2,$5),   [$1;$3;$4;$6])
     }

 | Tasm TOPar asmbody TCPar              { DefineTodo }
 | Tasm Tvolatile TOPar asmbody TCPar    { DefineTodo }

 /*(* aliases macro *)*/
 | TMacroAttr { DefineTodo }

 | /*(* empty *)*/ { DefineEmpty }




param_define:
 | TIdent               { mk_string_wrap $1 }
 | TypedefIdent         { mk_string_wrap $1 }
 | TDefParamVariadic    { mk_string_wrap $1 }
 | TEllipsis            { "...", [$1] }
 /*(* they reuse keywords :(  *)*/
 | Tregister            { "register", [$1] }




cpp_ifdef_directive:
 | TIfdef
     { let (cond,tag,ii) = $1 in
       IfdefDirective ((Ifdef cond, IfdefTag (Common.some !tag)),  [ii]) }
 | TIfdefelse
     { let (tag,ii) = $1 in
       IfdefDirective ((IfdefElse, IfdefTag (Common.some !tag)), [ii]) }
 | TIfdefelif
     { let (cond,tag,ii) = $1 in
       IfdefDirective ((IfdefElseif cond, IfdefTag (Common.some !tag)), [ii]) }
 | TEndif
     { let (tag,ii) = $1 in
       IfdefDirective ((IfdefEndif, IfdefTag (Common.some !tag)), [ii]) }

 | TIfdefBool
     { let (_b, tag,ii) = $1 in
       IfdefDirective ((Ifdef Gnone, IfdefTag (Common.some !tag)), [ii]) }
 | TIfdefMisc
     { let (_b, tag,ii) = $1 in
       IfdefDirective ((Ifdef Gnone, IfdefTag (Common.some !tag)), [ii]) }
 | TIfdefVersion
     { let (_b, tag,ii) = $1 in
       IfdefDirective ((Ifdef Gnone, IfdefTag (Common.some !tag)), [ii]) }


/*(* cppext: *)*/
cpp_other:
 /*(* no conflict ? no need for a TMacroTop ? apparently not as at toplevel
    * the rule are slightly different, they cant be statement and so expr
    * at the top, only decl or function definition.
    *)*/
 | identifier TOPar argument_list TCPar TPtVirg
     {
       if args_are_params $3
       then
	 (* if all args are params, assume it is a prototype of a function
	    with no return type *)
	 let parameters = args_to_params $3 None in
	 let paramlist = (parameters, (false, [])) in (* no varargs *)
	 let id = RegularName (mk_string_wrap $1) in
	 let ret =
	   warning "type defaults to 'int'"
	     (mk_ty defaultInt [fakeInfo fake_pi]) in
	 let ty =
	   fixOldCDecl (mk_ty (FunctionType (ret, paramlist)) [$2;$4]) in
	 let attrs = Ast_c.noattr in
	 let sto = (NoSto, false), [] in
	 let iistart = Ast_c.fakeInfo () in
	 Declaration(
	 DeclList ([{v_namei = Some (id,NoInit); v_type = ty;
                      v_storage = unwrap sto; v_local = NotLocalDecl;
                      v_attr = attrs; v_type_bis = ref None;
                    },[]],
                   ($5::iistart::snd sto)))
       else
	 Declaration
	   (MacroDecl((NoSto, fst $1, $3, true), [snd $1;$2;$4;$5;fakeInfo()]))
           (* old: MacroTop (fst $1, $3,    [snd $1;$2;$4;$5])  *)
     }

 /* cheap solution for functions with no return type.  Not really a
       cpp_other, but avoids conflicts */
 | identifier TOPar argument_list TCPar compound {
   let parameters = args_to_params $3 (Some (snd $1)) in
   let paramlist = (parameters, (false, [])) in (* no varargs *)
   let fninfo =
     let id = RegularName (mk_string_wrap $1) in
     let ret =
       warning "type defaults to 'int'"
	 (mk_ty defaultInt [fakeInfo fake_pi]) in
     let ty = mk_ty (FunctionType (ret, paramlist)) [$2;$4] in
     let attrs = Ast_c.noattr in
     let sto = (NoSto, false), [] in
     (id, fixOldCDecl ty, sto, attrs) in
   let fundef = fixFunc (fninfo, $5, None) in
   Definition fundef
 }

 /*(* TCParEOL to fix the end-of-stream bug of ocamlyacc *)*/
 | identifier TOPar argument_list TCParEOL
     { Declaration
	 (MacroDecl ((NoSto, fst $1, $3, false), [snd $1;$2;$4;fakeInfo()])) }

  /*(* ex: EXPORT_NO_SYMBOLS; *)*/
 | identifier TPtVirg { EmptyDef [snd $1;$2] }



/*(*************************************************************************)*/
/*(* celem *)*/
/*(*************************************************************************)*/

external_declaration:
 | function_definition               { Definition $1 }
 | decl                              { Declaration ($1 Ast_c.NotLocalDecl) }


celem:
 | Tnamespace TIdent TOBrace translation_unit TCBrace
     { !LP._lexer_hint.context_stack <- [LP.InTopLevel];
       Namespace ($4, [$1; snd $2; $3; $5]) }

 | external_declaration                         { $1 }

 /*(* cppext: *)*/
 | cpp_directive
     { CppTop $1 }
 | cpp_other
     { $1 }
 | cpp_ifdef_directive /* (*external_declaration_list ...*)*/
     { IfdefTop $1 }

 /*(* can have asm declaration at toplevel *)*/
 | Tasm TOPar asmbody TCPar TPtVirg             { EmptyDef [$1;$2;$4;$5] }

 /*
 (* in ~/kernels/src/linux-2.5.2/drivers/isdn/hisax/isdnl3.c sometimes
  * the function ends with }; instead of just }
  * can also remove this rule and report "parse error" pb to morton
  *)*/
 | TPtVirg    { EmptyDef [$1] }


 | EOF        { FinalDef $1 }




/*(*************************************************************************)*/
/*(* some generic workarounds *)*/
/*(*************************************************************************)*/

tobrace: TOBrace  { LP.push_context LP.InFunction; LP.new_scope (); $1 }
tcbrace: TCBrace  { LP.pop_context();              LP.del_scope (); $1 }

tobrace_enum: TOBrace { LP.push_context LP.InEnum; $1 }
tcbrace_enum: TCBrace { LP.pop_context (); $1 }

tobrace_ini: TOBrace { LP.push_context LP.InInitializer; $1 }
tcbrace_ini: TCBrace { LP.pop_context (); $1 }

tobrace_struct: TOBrace { LP.push_context LP.InStruct; $1}
tcbrace_struct: TCBrace { LP.pop_context (); $1 }




topar: TOPar
     { LP.new_scope ();et "topar" ();
       LP.push_context LP.InParameter;
       $1
     }
tcpar: TCPar
     { LP.del_scope ();dt "tcpar" ();
       LP.pop_context ();
       $1
     }




/*(*************************************************************************)*/
/*(* xxx_list, xxx_opt *)*/
/*(*************************************************************************)*/


/*(* old:
compound2:
 |                            { ([],[]) }
 |  statement_list            { ([], $1) }
 |  decl_list                 { ($1, []) }
 |  decl_list statement_list  { ($1,$2) }

statement_list: stat_or_decl_list { $1 }
*)*/


/*(*
decl_list:
 | decl           { [$1]   }
 | decl_list decl { $1 @ [$2] }

statement_list:
 | statement { [$1] }
 | statement_list statement { $1 @ [$2] }
*)*/





string_list:
 | string_elem { $1 }
 | string_list string_elem { $1 @ $2 }

colon_asm_list:
 | colon_asm { [$1] }
 | colon_asm_list colon_asm  { $1 @ [$2] }

colon_option_list:
 | colon_option { [$1, []] }
 | colon_option_list TComma colon_option { $1 @ [$3, [$2]] }


argument_list_ne:
 | argument_ne                           { [$1, []] }
 | argument_list_ne TComma argument { $1 @ [$3,    [$2]] }

argument_list:
 | argument                           { [$1, []] }
 | argument_list TComma argument { $1 @ [$3,    [$2]] }

/*(*
expression_list:
 | assign_expr { [$1, []] }
 | expression_list TComma assign_expr { $1 @ [$3,   [$2]] }
*)*/


ident_define_list_ne:
 | TIdentDefine              { [RegularName (mk_string_wrap $1), []] }
 | ident_define_list_ne TIdentDefine
     { $1 @ [RegularName (mk_string_wrap $2), []] }


struct_decl_list:
 | struct_decl                   { [$1] }
 | struct_decl_list struct_decl  { $1 @ [$2] }


struct_declarator_list:
 | struct_declarator                               { [$1,           []] }
 | struct_declarator_list TComma struct_declarator { $1 @ [$3,     [$2]] }


enumerator_list:
 | enumerator                        { [$1,          []]   }
 | enumerator_list TComma enumerator { $1 @ [$3,    [$2]] }


init_declarator_list:
 | init_declarator                             { [$1,   []] }
 | init_declarator_list TComma init_declarator { $1 @ [$3,     [$2]] }


parameter_list:
 | parameter_decl                       { [$1, []] }
 | parameter_list TComma parameter_decl { $1 @ [$3,  [$2]] }

taction_list_ne:
 | TAction                 { [$1] }
 | TAction taction_list_ne { $1 :: $2 }

taction_list:
/*old: was generating conflict, hence now taction_list_ne
    | (* empty *) { [] }
 | TAction { [$1] }
 | taction_list TAction { $1 @ [$2] }
*/
 |                      { [] }
 | TAction taction_list { $1 :: $2 }

param_define_list:
 | /*(* empty *)*/ { [] }
 | param_define                           { [$1, []] }
 | param_define_list TComma param_define  { $1 @ [$3, [$2]] }

designator_list:
 | designator { [$1] }
 | designator_list designator { $1 @ [$2] }

attribute_list:
 | attribute { [$1] }
 | attribute_list attribute { $1 @ [$2] }

attribute_storage_list:
 | attribute_storage { [$1] }
 | attribute_storage_list attribute_storage { $1 @ [$2] }


attributes: attribute_list { $1 }

comma_opt:
 | TComma {  [$1] }
 | /*(* empty *)*/  {  []  }

/*(*
gcc_opt_virg:
 | TPtVirg { }
 |  { }
*)*/

gcc_opt_expr:
 | expr        { Some $1 }
 | /*(* empty *)*/ { None  }

/*(*
opt_ptvirg:
 | TPtVirg { [$1] }
 | { [] }
*)*/
