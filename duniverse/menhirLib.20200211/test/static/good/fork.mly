%token <string * Lexing.position * Lexing.position> IDENTIFIER
%token KIND TYPE STAR ARROW LPAR RPAR EOF EQ LATER DOT COLON DEFAULT LAMBDA
%token COMMA LEMMA FORALL BACKQUOTE EXISTS BIGLAMBDA TERM LBRACKET RBRACKET
%token PACK IN UNPACK AS LET WILDCARD ASSERT
%token <string> INCLUDE

%start<Syntax.declarations> program

%{

  open Syntax

  let mkid sort (name, startp, endp) =
    Identifier.make name sort startp endp

  (* A sordid incantation for turning multiple binding forms into single binding
     forms while generating reasonable location information. *)

  let bind
      ((ids : (Error.location * 'id) list), (ko : 'annotation))
      (form : Lexing.position -> 'id -> 'annotation -> Error.location * 'a -> Error.location * 'a)
      (body : Error.location * 'a) : 'a =
    let _, body =
      List.fold_right (fun ((startp, _), id) body ->
	form startp id ko body
      ) ids body
    in
    body

  let mktyabs ((_, endp), _) startp id ko ty =
    (startp, endp), TyAbs (id, ko, ty)

  let mktyquantified q ((_, endp), _) startp id ko ty =
    (startp, endp), TyQuantified (q, id, ko, ty)

  let mksforall _ id ko (_, s) =
    Error.dummy, SForall (id, ko, s)

  let mkteabs ((_, endp), _) startp id ty t =
    (startp, endp), TeAbs (id, ty, t)

  let mktetyabs ((_, endp), _) startp id ko t =
    (startp, endp), TeTyAbs (id, ko, t)

%}

%%

(* ------------------------------------------------------------------------- *)

(* Recording locations. *)

%inline loc(X):
| x = X
    { (($startpos, $endpos), x) }

(* ------------------------------------------------------------------------- *)

(* Identifiers. *)

(* Each identifier is tagged with its sort, which is known thanks
   to the context where the identifier appears. *)

kind_variable:
| id = IDENTIFIER
    { mkid kind_sort id }

type_variable:
| id = IDENTIFIER
    { mkid type_sort id }

term_variable:
| id = IDENTIFIER
    { mkid term_sort id }

(* ------------------------------------------------------------------------- *)

(* Kinds. *)

kind0:
| STAR
    { KStar }
| id = kind_variable
    { KDefined id }
| LATER kind = kind0
    { KLater kind }
| LPAR kind = kind RPAR
    { kind }

kind1:
| kind = kind0
    { kind }
| kind1 = kind0 ARROW kind2 = kind1
    { KArrow (kind1, kind2) }

%inline kind:
| kind = kind1
    { kind }

(* ------------------------------------------------------------------------- *)

(* A list of type variables with an optional kind annotation. *)

type_binder:
| ids = loc(type_variable)+
    { ids, None }
| ids = loc(type_variable)+ COLON kind = kind
    { ids, Some kind }

(* A quantifier. *)

quantifier:
| FORALL
    { QForall }
| EXISTS
    { QExists }

(* Types. *)

ty0:
| id = type_variable
    { TyVar id }
| LPAR RPAR
    { TyUnit }
| LPAR ty = ty RPAR
    { ty }
| LPAR ty = loc(ty) COLON kind = kind RPAR
    { TyConstraint (ty, kind) }
| LPAR ty1 = loc(ty) delimiter = COMMA ty2 = loc(ty) RPAR
    { let loc1 = ($startpos, $endpos(delimiter))
      and () = delimiter in (* avoid a warning *)
      TyPair (loc1, ty1, ty2) }
(* TEMPORARY is this useful/necessary?
| q = quantifier LBRACKET kind = kind RBRACKET
    { TyRawQuantifier (q, kind) }
*)

ty1:
| ty = ty0
    { ty }
| ty1 = loc(ty1) ty2 = loc(ty0)
    { TyApp (ty1, ty2) }

ty2:
| ty = ty1
    { ty }
| ty1 = loc(ty2) BACKQUOTE ty = loc(ty1) delimiter = BACKQUOTE ty2 = loc(ty1)
    { let loc = ($startpos, $endpos(delimiter)) in
      let () = delimiter in (* avoid a warning *)
      TyApp ((loc, TyApp (ty, ty1)), ty2) }

ty3:
| ty = ty2
    { ty }
| ty1 = loc(ty2) delimiter = ARROW ty2 = loc(ty3)
    { let loc1 = ($startpos, $endpos(delimiter))
      and () = delimiter in (* avoid a warning *)
      TyArrow (loc1, ty1, ty2) }
| LAMBDA b = type_binder DOT ty = loc(ty3)
    { bind b (mktyabs ty) ty }
| q = quantifier b = type_binder DOT ty = loc(ty3)
    { bind b (mktyquantified q ty) ty }

%inline ty:
| ty = ty3
    { ty }

(* ------------------------------------------------------------------------- *)

(* Statements. *)

statement:
| ty1 = loc(ty2) EQ ty2 = loc(ty2)
    { SEquation (ty1, ty2) }
| FORALL b = type_binder DOT s = statement
    { bind b mksforall (Error.dummy, s) }

(* ------------------------------------------------------------------------- *)

(* A term variable or a wildcard. *)

term_variable_or_wildcard:
| id = term_variable
    { Some id }
| WILDCARD
    { None }

(* A list of term variables with a type annotation. *)

term_binder:
| ids = loc(term_variable)+ COLON ty = loc(ty)
    { ids, ty }

(* Terms. *)

term0:
| id = term_variable
    { TeVar id }
| LPAR RPAR
    { TeUnit }
| LPAR t = term RPAR
    { t }
| LPAR t = loc(term) COLON ty = loc(ty) RPAR
    { TeConstraint (t, ty) }
| LPAR t1 = loc(term) COMMA t2 = loc(term) RPAR
    { TePair (t1, t2) }

term1:
| t = term0
    { t }
| t1 = loc(term1) t2 = loc(term0)
    { TeApp (t1, t2) }
| t = loc(term1) LBRACKET ty = loc(ty) RBRACKET
    { TeTyApp (t, ty) }

term2:
| t = term1
    { t }
| t1 = loc(term2) BACKQUOTE t = loc(term1) delimiter = BACKQUOTE t2 = loc(term1)
    { let loc = ($startpos, $endpos(delimiter)) in
      let () = delimiter in (* avoid a warning *)
      TeApp ((loc, TeApp (t, t1)), t2) }

term3:
| t = term2
    { t }
| LAMBDA b = term_binder DOT t = loc(term3)
    { bind b (mkteabs t) t }
| BIGLAMBDA b = type_binder DOT t = loc(term3)
    { bind b (mktetyabs t) t }
| PACK ty2 = loc(ty) COMMA t = loc(term) AS ty1 = loc(ty)
    { TePack (t, ty1, ty2) }
| UNPACK alpha = type_variable COMMA x = term_variable EQ t1 = loc(term) IN t2 = loc(term)
    { TeUnpack (alpha, x, t1, t2) }
| LET id = term_variable EQ t1 = loc(term) IN t2 = loc(term)
    { TeLet (id, t1, t2) }
| LET LPAR id1 = term_variable_or_wildcard COMMA id2 = term_variable_or_wildcard RPAR
  EQ t1 = loc(term) IN t2 = loc(term)
    { TeLetPair (id1, id2, t1, t2) }
| ASSERT id = term_variable COLON ty = loc(ty) IN t = loc(term)
    { let var = ($startpos(id), $endpos(id)), TeVar id in
      let con = ($startpos(id), $endpos(ty)), TeConstraint (var, ty) in
      TeLet (id, con, t) }
    (* [assert x : ty in t] is sugar for [let x = (x : ty) in t] *)
| LET id = term_variable COLON ty = loc(ty) EQ t1 = loc(term) IN t2 = loc(term)
    { let con = ($startpos(id), $endpos(ty)), TeConstraint (t1, ty) in
      TeLet (id, con, t2) }
    (* [let x : ty = t1 in t2] is sugar for [let x = (t1 : ty) in t2] *)
| TYPE id = type_variable EQ ty = loc(ty) IN t = loc(term)
    { TeType (id, ty, t) }

%inline term:
| t = term3
    { t }

(* ------------------------------------------------------------------------- *)

(* Declarations. *)

declaration:
| KIND id = kind_variable EQ kind = kind
    { [ DKind (id, kind) ] }
| TYPE id = type_variable COLON kind = kind EQ ty = loc(ty)
    { [ DType (id, kind, ty) ] }
| DEFAULT KIND id = type_variable COLON kind = kind
    { [ DDefaultKind (id, kind) ] }
| LEMMA IDENTIFIER COLON s = statement
    { [ DLemma s ] }
| TERM id = term_variable COLON ty = loc(ty) EQ t = loc(term)
    { [ DTerm (id, ty, t) ] }
| filename = INCLUDE
    { !ParserBootstrap.load_and_parse filename }

program:
  ds = declaration* EOF
    { List.flatten ds }

