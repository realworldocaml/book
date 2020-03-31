(* Original file: osdp.0.6.0/osdp-0.6.0/src/pa_parser.mly *)
%{
(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014  P. Roux and P.L. Garoche
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Camlp4.PreCast

let loc () =
  let start_p = Parsing.symbol_start_pos () in
  let end_p = Parsing.symbol_end_pos () in
  Loc.of_tuple
    (start_p.Lexing.pos_fname,
     start_p.Lexing.pos_lnum, start_p.Lexing.pos_bol, start_p.Lexing.pos_cnum,
     end_p.Lexing.pos_lnum, end_p.Lexing.pos_bol, end_p.Lexing.pos_cnum,
     false)

let id_of_list loc l =
  let id_of_str s =
    if 'A' <= s.[0] && s.[0] <= 'Z'
       || s = "::" || s = "[]" then Ast.IdUid (loc, s)
    else Ast.IdLid (loc, s) in
  Ast.ExId (loc, Ast.idAcc_of_list (List.map id_of_str l))

let olf loc idl el =
  List.fold_left
    (fun f e -> Ast.ExApp (loc, f, e))
    (id_of_list loc (["Osdp"; "Lmi"; "Float"] @ idl)) el

let olfl idl el = olf (loc ()) idl el

let osf loc idl el =
  List.fold_left
    (fun f e -> Ast.ExApp (loc, f, e))
    (id_of_list loc (["Osdp"; "Sos"; "Float"] @ idl)) el

let osfl idl el = osf (loc ()) idl el

let opf loc idl el =
  List.fold_left
    (fun f e -> Ast.ExApp (loc, f, e))
    (id_of_list loc (["Osdp"; "Polynomial"; "Float"] @ idl)) el

let opfl idl el = opf (loc ()) idl el

let cons l e1 e2 =
  Ast.ExApp (l, Ast.ExApp (l, Ast.ExId (l, Ast.IdUid (l, "::")), e1), e2)

let empty_list l = Ast.ExId (l, Ast.IdUid (l, "[]"))

let slist l e = cons l e (empty_list l)

let pair l e1 e2 = Ast.ExTup (l, Ast.ExCom (l, e1, e2))

let simplify_mat_float e =
  let is_id l = function
    | Ast.ExId (_, i) ->
       let l' = Ast.list_of_ident i [] in
       begin
         try
           List.for_all
             (function
               | s, Ast.IdUid (_, s') | s, Ast.IdLid (_, s') -> s = s'
               | _ -> false)
             (List.combine l l')
         with Invalid_argument _ -> false
       end
    | _ -> false in

  let is_1x1_float e =
    let is_slist_slist e =
      let is_slist = function
        | Ast.ExApp (_, Ast.ExApp (_, ic, h), ie) ->
           if is_id ["::"] ic && is_id ["[]"] ie then Some h else None
        | _ -> None in
      match is_slist e with None -> None | Some e -> is_slist e in
    let aux = function
      | Ast.ExApp (_, c, Ast.ExApp (_, oll, llf)) ->
         if is_id ["Osdp"; "Lmi"; "Float"; "Const"] c
            && is_id ["Osdp"; "Lmi"; "Float"; "Mat"; "of_list_list"] oll then
           match is_slist_slist llf with
           | Some (Ast.ExFlo (l, f))
           | Some (Ast.ExInt (l, f)) ->
              Some (Ast.ExFlo (l, f))
           | _ -> None
         else None
      | _ -> None in
    match e with
    | Ast.ExApp (_, mi, m) when is_id ["Osdp"; "Lmi"; "Float"; "Minus"] mi ->
       begin
         match aux m with
         | Some (Ast.ExFlo (l, f)) -> Some (Ast.ExFlo (l, "-" ^ f))
         | _ -> None
       end
    | _ -> aux e in

  let rec map_ExSem f = function
    | Ast.ExSem (l, e1, e2) ->
       begin
         match f e1, map_ExSem f e2 with
         | Some e1, Some e2 -> Some (cons l e1 e2)
         | _ -> None
       end
    | e ->
       match f e with
       | None -> None
       | Some e -> Some (slist (Ast.loc_of_expr e) e) in

  let loc = loc () in
  let simplify_mat_line = function
    | Ast.ExArr (l, e) -> map_ExSem is_1x1_float e
    | _ -> None in
  match map_ExSem simplify_mat_line e with
  | Some llf ->
     olf loc ["Const"] [olf loc ["Mat"; "of_list_list"] [llf]]
  | None -> olf loc ["Block"] [Ast.ExArr (loc, e)]

let monom loc v d =
  let rec aux n =
    if n <= 0 then slist loc d
    else cons loc (Ast.ExInt (loc, "0")) (aux (n - 1)) in
  Ast.ExApp (loc, id_of_list loc ["Osdp"; "Monomial"; "of_list"], aux v)
%}

%token <string> FLOAT
%token <string> INT
%token <int> MID
%token <string> ID
%token <string> AQ
%token INT0 SQUOTE HAT PLUS MINUS TIMES
%token ZEROS EYE KRON KRSYM LIFT
%token DERIV
%token LPAR RPAR LBRA RBRA COMMA SEMICOL LEQ GEQ
%token UMINUS PVM FINT0 EOF

%left PLUS MINUS
%right TIMES
%nonassoc UMINUS LPAR
%nonassoc SQUOTE

%nonassoc PVM
%left HAT

%nonassoc FINT0
%nonassoc EOF

%type <Camlp4.PreCast.Ast.expr> lmi
%type <Camlp4.PreCast.Ast.expr> sos
%type <Camlp4.PreCast.Ast.expr> pol
%start lmi
%start sos
%start pol

%%

f:
| INT0 %prec FINT0 { Ast.ExFlo (loc (), "0.") }
| INT { Ast.ExFlo (loc (), $1) }
| FLOAT { Ast.ExFlo (loc (), $1) }

id:
| MID { "x" ^ string_of_int $1 }
| ID { $1 }

ncid:
| INT0 { Ast.ExInt (loc (), "0") }
| INT { Ast.ExInt (loc (), $1) }
| id { id_of_list (loc ()) [$1] }
| AQ { Camlp4.PreCast.Syntax.AntiquotSyntax.parse_expr (loc ()) $1 }

exprl:
| id { id_of_list (loc ()) [$1] }
| ZEROS LPAR ncid COMMA ncid RPAR { olfl ["Zeros"] [$3; $5] }
| EYE LPAR ncid RPAR { olfl ["Eye"] [$3] }
| KRON LPAR ncid COMMA ncid COMMA ncid RPAR { olfl ["Kron"]
                                                    [$3; $5; $7] }
| KRSYM LPAR ncid COMMA ncid COMMA ncid RPAR { olfl ["Kron_sym"]
                                                    [$3; $5; $7] }
| LBRA b RBRA { simplify_mat_float $2 }
| LIFT LPAR exprl COMMA ncid COMMA ncid COMMA ncid COMMA ncid RPAR { olfl ["Lift_block"] [$3; $5; $7; $9; $11] }
| exprl SQUOTE { olfl ["Transpose"] [$1] }
| MINUS exprl %prec UMINUS { olfl ["Minus"] [$2] }
| exprl PLUS exprl { olfl ["Add"] [$1; $3] }
| exprl MINUS exprl { olfl ["Sub"] [$1; $3] }
| exprl TIMES exprl { olfl ["Mult"] [$1; $3] }
| LPAR exprl RPAR { $2 }
| f { let l = loc () in
      olf l ["Const"] [olf l ["Mat"; "of_list_list"] [slist l (slist l $1)]] }

b:
| li { Ast.ExArr (loc (), $1) }
| li SEMICOL b { Ast.ExSem (loc (), Ast.ExArr (Ast.loc_of_expr $1, $1), $3) }

li:
| exprl { $1 }
| exprl COMMA li { Ast.ExSem (loc (), $1, $3) }

lmi:
| exprl EOF { $1 }
| exprl LEQ INT0 EOF { olfl ["Minus"] [$1] }
| exprl GEQ INT0 EOF { $1 }
| exprl LEQ exprl EOF { olfl ["Sub"] [$3; $1] }
| exprl GEQ exprl EOF { olfl ["Sub"] [$1; $3] }

vm:
| MID %prec PVM { let l = loc () in monom l $1 (Ast.ExInt (l, "1")) }
| MID HAT ncid { monom (loc ()) $1 $3 }

monom:
| vm { $1 }
| monom vm  { let l = loc () in
              Ast.ExApp (l,
                         Ast.ExApp (l,
                                    id_of_list l ["Osdp"; "Monomial"; "mult"],
                                    $1),
                         $2) }

exprs:
| ID { id_of_list (loc ()) [$1] }
| monom { let l = loc () in
          osf l ["Const"] [osf l ["Poly"; "of_list"]
                                 [slist l (pair l $1 (Ast.ExFlo (l, "1.")))]] }
| f monom { let l = loc () in
            osf l ["Const"] [osf l ["Poly"; "of_list"]
                                   [slist l (pair l $2 $1)]] }
| exprs PLUS exprs { osfl ["Add"] [$1; $3] }
| exprs MINUS exprs { osfl ["Sub"] [$1; $3] }
| MINUS exprs %prec UMINUS { let l = loc () in
                             osf l ["Sub"]
                                 [osf l ["Const"] [osf l ["Poly"; "zero"] []];
                                  $2] }
| exprs TIMES exprs { osfl ["Mult"] [$1; $3] }
| exprs HAT ncid { osfl ["Power"] [$1; $3] }
| exprs LPAR l RPAR { osfl ["Compose"] [$1; $3] }
| DERIV MID LPAR exprs RPAR { osfl ["Derive"] [$4; Ast.ExInt (loc (), string_of_int $2)] }
| LPAR exprs RPAR { $2 }
| f { let l = loc () in
      osf l ["Const"]
          [osf l ["Poly"; "of_list"]
               [slist l (pair l (Ast.ExApp (l, id_of_list l ["Osdp"; "Monomial"; "of_list"], empty_list l)) $1)]] }

l:
| le { $1 }
| le COMMA l { let l = loc () in
                Ast.ExApp (l, Ast.ExApp (l, id_of_list l ["@"], $1), $3) }

le:
| exprs { slist (loc ()) $1 }
| AQ { Camlp4.PreCast.Syntax.AntiquotSyntax.parse_expr (loc ()) $1 }

sos:
| exprs EOF { $1 }
| exprs LEQ exprs EOF { osfl ["Sub"] [$3; $1] }
| exprs GEQ exprs EOF { osfl ["Sub"] [$1; $3] }

exprp:
| ID { id_of_list (loc ()) [$1] }
| monom { let l = loc () in
          opf l ["of_list"] [slist l (pair l $1 (Ast.ExFlo (l, "1.")))] }
| f monom { let l = loc () in opf l ["of_list"] [slist l (pair l $2 $1)] }
| exprp PLUS exprp { opfl ["add"] [$1; $3] }
| exprp MINUS exprp { opfl ["sub"] [$1; $3] }
| MINUS exprp %prec UMINUS { let l = loc () in
                             opf l ["sub"] [opf l ["zero"] []; $2] }
| exprp TIMES exprp { opfl ["mult"] [$1; $3] }
| exprp HAT ncid { opfl ["power"] [$1; $3] }
| exprp LPAR lp RPAR { opfl ["compose"] [$1; $3] }
| DERIV MID LPAR exprs RPAR { osfl ["Derive"] [$4; Ast.ExInt (loc (), string_of_int $2)] }
| LPAR exprp RPAR { $2 }
| f { let l = loc () in
      opf l ["of_list"]
          [slist l (pair l (Ast.ExApp (l, id_of_list l ["Osdp"; "Monomial"; "of_list"], empty_list l)) $1)] }

lp:
| lep { $1 }
| lep COMMA lp { let l = loc () in
                 Ast.ExApp (l, Ast.ExApp (l, id_of_list l ["@"], $1), $3) }

lep:
| exprp { slist (loc ()) $1 }
| AQ { Camlp4.PreCast.Syntax.AntiquotSyntax.parse_expr (loc ()) $1 }

pol:
| exprp EOF { $1 }
