(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* A pretty-printer for [IL]. *)

open IL

module PreliminaryMake (X : sig

  (* We assume that the following types and functions are given. This
     allows us to work both with buffers of type [Buffer.t] and with
     output channels of type [out_channel]. *)

  type channel
  val fprintf: channel -> ('a, channel, unit) format -> 'a
  val output_substring: channel -> string -> int -> int -> unit

  (* This is the channel that is being written to. *)

  val f: channel

  (* [locate_stretches] controls the way we print OCaml stretches (types and
     semantic actions). If it is [Some dstfilename], where [dstfilename] is
     the name of the file that is being written, then we surround stretches
     with OCaml line number directives of the form # <line number> <filename>.
     If it is [None], then we don't. *)

  (* Providing line number directives allows the OCaml typechecker to report
     type errors in the .mly file, instead of in the generated .ml / .mli
     files. Line number directives also affect the dynamic semantics of any
     [assert] statements contained in semantic actions: when they are provided,
     the [Assert_failure] exception carries a location in the .mly file. As a
     general rule of thumb, line number directives should always be provided,
     except perhaps where we think that they decrease readability (e.g., in a
     generated .mli file). *)

  val locate_stretches: string option

end) = struct
open X

let output_char f c =
  fprintf f "%c" c

let output_string f s =
  fprintf f "%s" s

let flush f =
  fprintf f "%!"

(* ------------------------------------------------------------------------- *)
(* Dealing with newlines and indentation. *)

let maxindent =
  120

let whitespace =
  String.make maxindent ' '

let indentation =
  ref 0

let line =
  ref 1

(* [rawnl] is, in principle, the only place where writing a newline
   character to the output channel is permitted. This ensures that the
   line counter remains correct. But see also [stretch] and [typ0]. *)

let rawnl f =
  incr line;
  output_char f '\n'

let nl f =
  rawnl f;
  output_substring f whitespace 0 !indentation

let indent ofs producer f x =
  let old_indentation = !indentation in
  let new_indentation = old_indentation + ofs in
  if new_indentation <= maxindent then
    indentation := new_indentation;
  nl f;
  producer f x;
  indentation := old_indentation

(* This produces a line number directive. *)

let sharp f line file =
  fprintf f "%t# %d \"%s\"%t" rawnl line file rawnl

(* ------------------------------------------------------------------------- *)
(* Printers of atomic elements. *)

let nothing _ =
  ()

let space f =
  output_char f ' '

let comma f =
  output_string f ", "

let semi f =
  output_char f ';'

let seminl f =
  semi f;
  nl f

let times f =
  output_string f " * "

let letrec f =
  output_string f "let rec "

let letnonrec f =
  output_string f "let "

let keytyp f =
  output_string f "type "

let exc f =
  output_string f "exception "

let et f =
  output_string f "and "

let var f x =
  output_string f x

let bar f =
  output_string f " | "

(* ------------------------------------------------------------------------- *)
(* List printers. *)

(* A list with a separator in front of every element. *)

let rec list elem sep f = function
  | [] ->
      ()
  | e :: es ->
      fprintf f "%t%a%a" sep elem e (list elem sep) es

(* A list with a separator between elements. *)

let seplist elem sep f = function
  | [] ->
      ()
  | e :: es ->
      fprintf f "%a%a" elem e (list elem sep) es

(* OCaml type parameters. *)

let typeparams p0 p1 f = function
  | [] ->
      ()
  | [ param ] ->
      fprintf f "%a " p0 param
  | _ :: _ as params ->
      fprintf f "(%a) " (seplist p1 comma) params

(* ------------------------------------------------------------------------- *)
(* Expression printer. *)

(* We use symbolic constants that stand for subsets of the
   expression constructors. We do not use numeric levels
   to stand for subsets, because our subsets do not form
   a linear inclusion chain. *)

type subset =
  | All
  | AllButSeq
  | AllButFunTryMatch
  | AllButFunTryMatchSeq
  | AllButLetFunTryMatch
  | AllButLetFunTryMatchSeq
  | AllButIfThenSeq
  | OnlyAppOrAtom
  | OnlyAtom

(* This computes the intersection of a subset with the
   constraint "should not be a sequence". *)

let andNotSeq = function
  | All
  | AllButSeq ->
      AllButSeq
  | AllButFunTryMatch
  | AllButFunTryMatchSeq ->
      AllButFunTryMatchSeq
  | AllButLetFunTryMatch
  | AllButLetFunTryMatchSeq ->
      AllButLetFunTryMatchSeq
  | AllButIfThenSeq ->
      AllButIfThenSeq
  | OnlyAppOrAtom ->
      OnlyAppOrAtom
  | OnlyAtom ->
      OnlyAtom

(* This defines the semantics of subsets by relating
   expressions with subsets. *)

let rec member e k =
  match e with
  | EComment _
  | EPatComment _ ->
      true
  | EFun _
  | ETry _
  | EMatch _ ->
      begin
        match k with
        | AllButFunTryMatch
        | AllButFunTryMatchSeq
        | AllButLetFunTryMatch
        | AllButLetFunTryMatchSeq
        | OnlyAppOrAtom
        | OnlyAtom ->
            false
        | _ ->
            true
      end
  | ELet ([], e) ->
      member e k
  | ELet ((PUnit, _) :: _, _) ->
      begin
        match k with
        | AllButSeq
        | AllButFunTryMatchSeq
        | AllButLetFunTryMatchSeq
        | AllButIfThenSeq
        | OnlyAppOrAtom
        | OnlyAtom ->
            false
        | _ ->
            true
      end
  | ELet (_ :: _, _) ->
      begin
        match k with
        | AllButLetFunTryMatch
        | AllButLetFunTryMatchSeq
        | OnlyAppOrAtom
        | OnlyAtom ->
            false
        | _ ->
            true
      end
  | EIfThen _ ->
      begin
        match k with
        | AllButIfThenSeq
        | OnlyAppOrAtom
        | OnlyAtom ->
            false
        | _ ->
            true
      end
  | EApp (_, _ :: _)
  | EData (_, _ :: _)
  | EMagic _
  | ERepr _
  | ERaise _ ->
      begin
        match k with
        | OnlyAtom ->
            false
        | _ ->
            true
      end
  | ERecordWrite _
  | EIfThenElse _ ->
      begin
        match k with
        | OnlyAppOrAtom
        | OnlyAtom ->
            false
        | _ ->
            true
      end
  | EVar _
  | ETextual _
  | EApp (_, [])
  | EData (_, [])
  | ETuple _
  | EAnnot _
  | ERecord _
  | ERecordAccess (_, _)
  | EIntConst _
  | EStringConst _
  | EUnit
  | EArray _
  | EArrayAccess (_, _) ->
      true


let rec exprlet k pes f e2 =
  match pes with
  | [] ->
      exprk k f e2
  | (PUnit, e1) :: pes ->
      fprintf f "%a%t%a" (exprk AllButLetFunTryMatch) e1 seminl (exprlet k pes) e2
  | (PVar id1, EAnnot (e1, ts1)) :: pes ->
      (* TEMPORARY current ocaml does not support type schemes here; drop quantifiers, if any *)
      fprintf f "let %s : %a = %a in%t%a" id1 typ ts1.body (* scheme ts1 *) expr e1 nl (exprlet k pes) e2
  | (PVar id1, EFun (ps1, e1)) :: pes ->
      fprintf f "let %s%a = %a in%t%t%a"
        id1 (list pat0 space) ps1 (indent 2 expr) e1 nl nl (exprlet k pes) e2
  | (p1, (ELet _ as e1)) :: pes ->
      fprintf f "let %a =%a%tin%t%a" pat p1 (indent 2 expr) e1 nl nl (exprlet k pes) e2
  | (p1, e1) :: pes ->
      fprintf f "let %a = %a in%t%a" pat p1 expr e1 nl (exprlet k pes) e2

and atom f e =
  exprk OnlyAtom f e

and app f e =
  exprk OnlyAppOrAtom f e

and expr f e =
  exprk All f e

and exprk k f e =
  if member e k then
    match e with
    | EComment (c, e) ->
        if Settings.comment then
          fprintf f "(* %s *)%t%a" c nl (exprk k) e
        else
          exprk k f e
    | EPatComment (s, p, e) ->
        if Settings.comment then
          fprintf f "(* %s%a *)%t%a" s pat p nl (exprk k) e
        else
          exprk k f e
    | ELet (pes, e2) ->
        exprlet k pes f e2
    | ERecordWrite (e1, field, e2) ->
        fprintf f "%a.%s <- %a" atom e1 field (exprk (andNotSeq k)) e2
    | EMatch (_, []) ->
        assert false
    | EMatch (e, brs) ->
        fprintf f "match %a with%a" expr e (branches k) brs
    | ETry (_, []) ->
        assert false
    | ETry (e, brs) ->
        fprintf f "try%a%twith%a" (indent 2 expr) e nl (branches k) brs
    | EIfThen (e1, e2) ->
        fprintf f "if %a then%a" expr e1 (indent 2 (exprk (andNotSeq k))) e2
    | EIfThenElse (e0, e1, e2) ->
        fprintf f "if %a then%a%telse%a"
          expr e0 (indent 2 (exprk AllButIfThenSeq)) e1 nl (indent 2 (exprk (andNotSeq k))) e2
    | EFun (ps, e) ->
        fprintf f "fun%a ->%a" (list pat0 space) ps (indent 2 (exprk k)) e
    | EApp (EVar op, [ e1; e2 ])
      when op.[0] = '(' && op.[String.length op - 1] = ')' ->
        let op = String.sub op 1 (String.length op - 2) in
        fprintf f "%a %s %a" app e1 op app e2
    | EApp (e, args) ->
        fprintf f "%a%a" app e (list atom space) args
    | ERaise e ->
        fprintf f "raise %a" atom e
    | EMagic e ->
        fprintf f "Obj.magic %a" atom e
    | ERepr e ->
        fprintf f "Obj.repr %a" atom e
    | EData (d, []) ->
        var f d
    | EData (d, [ arg ]) ->
        fprintf f "%s %a" d atom arg
    | EData ("::", [ arg1; arg2 ]) ->
        (* Special case for infix cons. *)
        fprintf f "%a :: %a" atom arg1 atom arg2
    | EData (d, (_ :: _ :: _ as args)) ->
        fprintf f "%s (%a)" d (seplist app comma) args
    | EVar v ->
        var f v
    | ETextual action ->
        stretch false f action
    | EUnit ->
        fprintf f "()"
    | EIntConst k ->
        if k >= 0 then
          fprintf f "%d" k
        else
          fprintf f "(%d)" k
    | EStringConst s ->
        fprintf f "\"%s\"" (Compatibility.String.escaped s)
    | ETuple [] ->
        assert false
    | ETuple [ e ] ->
        atom f e
    | ETuple (_ :: _ :: _ as es) ->
        fprintf f "(%a)" (seplist app comma) es
    | EAnnot (e, s) ->
        (* TEMPORARY current ocaml does not support type schemes here; drop quantifiers, if any *)
        fprintf f "(%a : %a)" app e typ s.body (* should be scheme s *)
    | ERecordAccess (e, field) ->
        fprintf f "%a.%s" atom e field
    | ERecord fs ->
        fprintf f "{%a%t}" (indent 2 (seplist field nl)) fs nl
    | EArray fs ->
        fprintf f "[|%a%t|]" (indent 2 (seplist array_field nl)) fs nl
    | EArrayAccess (e, i) ->
        fprintf f "%a.(%a)" atom e expr i
  else
    fprintf f "(%a)" expr e

(* When printing a stretch, the string [content] includes padding (that is,
   whitespace) to as to preserve the column numbers of the source file,
   whereas the string [raw_content] does not. When [X.locate_stretches]
   is [None], the parameter [raw] controls the choice between them. When
   [X.locate_stretches] is [Some _], we ignore [raw] and force the use of
   the [content] string, so as to have correct column numbers. *)

and stretch raw f stretch =
  let content = stretch.Stretch.stretch_content
  and raw_content = stretch.Stretch.stretch_raw_content in
  match X.locate_stretches with
  | Some basename ->
      sharp f stretch.Stretch.stretch_linenum stretch.Stretch.stretch_filename;
      output_string f content;
      line := !line + stretch.Stretch.stretch_linecount;
      (* The addition [_ + 2] anticipates the effect on the line
         counter of the directive that we are just about to print. *)
      sharp f (!line + 2) basename;
      output_substring f whitespace 0 !indentation
  | None ->
      output_string f (if raw then raw_content else content)

and branches k f = function
  | [] ->
      ()
  | [ br ] ->
      fprintf f "%t| %a" nl (branch k) br
  | br :: brs ->
      fprintf f "%t| %a%a" nl (branch AllButFunTryMatch) br (branches k) brs

and branch k f br =
  fprintf f "%a ->%a" pat br.branchpat (indent 4 (exprk k)) br.branchbody

and field f (label, e) =
  fprintf f "%s = %a%t" label app e semi

and fpat f (label, p) =
  fprintf f "%s = %a%t" label pat p semi

and array_field f e =
  fprintf f "%a%t" app e semi

and pat0 f = function
  | PUnit ->
      fprintf f "()"
  | PWildcard ->
      fprintf f "_"
  | PVar x ->
      var f x
  | PVarLocated x ->
      (* Turn [x] on the fly into a stretch and print that. *)
      stretch true f (Lexer.stretch_of_id x)
  | PData (d, []) ->
      var f d
  | PTuple [] ->
      assert false
  | PTuple [ p ] ->
      pat0 f p
  | PTuple (_ :: _ :: _ as ps) ->
      fprintf f "(%a)" (seplist pat1 comma) ps
  | PAnnot (p, t) ->
      fprintf f "(%a : %a)" pat p typ t
  | PRecord fps ->
      (* 2018/10/19. In a record pattern, we used to omit bindings of the form
         [field = _]. However, this triggers OCaml's warning 9. We now print all
         bindings. *)
      fprintf f "{%a%t}" (indent 2 (seplist fpat nl)) fps nl
  | p ->
      fprintf f "(%a)" pat p

and pat1 f = function
  | PData (d, [ arg ]) ->
      fprintf f "%s %a" d pat0 arg
  | PData (d, (_ :: _ :: _ as args)) ->
      fprintf f "%s (%a)" d (seplist pat1 comma) args
  | PTuple [ p ] ->
      pat1 f p
  | p ->
      pat0 f p

and pat2 f = function
  | POr [] ->
      assert false
  | POr (_ :: _ as ps) ->
      seplist pat2 bar f ps
  | PTuple [ p ] ->
      pat2 f p
  | p ->
      pat1 f p

and pat f p =
  pat2 f p

and typevar f = function
  | "_" ->
      fprintf f "_"
  | v ->
      fprintf f "'%s" v

and typ0 f = function
  | TypTextual (Stretch.Declared ocamltype) ->
      (* Parentheses are necessary to avoid confusion between 1 - ary
         data constructor with n arguments and n - ary data constructor. *)
      fprintf f "(%a)" (stretch true) ocamltype
  | TypTextual (Stretch.Inferred t) ->
      line := !line + LineCount.count 0 (Lexing.from_string t);
      fprintf f "(%s)" t
  | TypVar v ->
      typevar f v
  | TypApp (t, params) ->
      fprintf f "%a%s" (typeparams typ0 typ) params t
  | t ->
      fprintf f "(%a)" typ t

and typ1 f = function
  | TypTuple [] ->
      assert false
  | TypTuple (_ :: _ as ts) ->
      seplist typ0 times f ts
  | t ->
      typ0 f t

and typ2 f = function
  | TypArrow (t1, t2) ->
      fprintf f "%a -> %a" typ1 t1 typ2 t2
  | t ->
      typ1 f t

and typ f =
  typ2 f

and scheme f scheme =
  match scheme.quantifiers with
  | [] ->
      typ f scheme.body
  | qs ->
      fprintf f "%a. %a" (list typevar space) qs typ scheme.body

(* ------------------------------------------------------------------------- *)
(* Toplevel definition printer. *)

(* The tuple of the arguments of a data constructor. *)

let datavalparams f params =
  (* [typ1] because [type t = A of  int -> int ] is not allowed by OCaml *)
  (*                [type t = A of (int -> int)] is allowed *)
  seplist typ1 times f params

(* A data constructor definition. *)

let datadef typename f def =
  fprintf f "  | %s" def.dataname;
  match def.datavalparams, def.datatypeparams with
  | [], None ->
      (* | A *)
      ()
  | _ :: _, None ->
      (* | A of t * u *)
      fprintf f " of %a"
        datavalparams def.datavalparams
  | [], Some indices ->
      (* | A : (v, w) ty *)
      fprintf f " : %a%s"
        (typeparams typ0 typ) indices typename
  | _ :: _, Some indices ->
      (* | A : t * u -> (v, w) ty *)
      fprintf f " : %a -> %a%s"
        datavalparams def.datavalparams
        (typeparams typ0 typ) indices typename

let fielddef f def =
  fprintf f "  %s%s: %a"
    (if def.modifiable then "mutable " else "")
    def.fieldname
    scheme def.fieldtype

let typerhs typename f = function
  | TDefRecord [] ->
      assert false
  | TDefRecord (_ :: _ as fields) ->
      fprintf f " = {%t%a%t}" nl (seplist fielddef seminl) fields nl
  | TDefSum [] ->
      ()
  | TDefSum defs ->
      fprintf f " = %a" (list (datadef typename) nl) defs
  | TAbbrev t ->
      fprintf f " = %a" typ t

let typeconstraint f = function
  | None ->
      ()
  | Some (t1, t2) ->
      fprintf f "%tconstraint %a = %a" nl typ t1 typ t2

let typedef f def =
  fprintf f "%a%s%a%a"
    (typeparams typevar typevar) def.typeparams
    def.typename
    (typerhs def.typename) def.typerhs
    typeconstraint def.typeconstraint

let rec pdefs pdef sep1 sep2 f = function
  | [] ->
      ()
  | [ def ] ->
      fprintf f "%t%a" sep1 pdef def
  | def :: defs ->
      fprintf f "%t%a%t%t%a"
        sep1 pdef def
        (* Separate two successive items with two newlines. *)
        nl nl
        (pdefs pdef sep2 sep2) defs

let valdef f = function
  | { valpat = PVar id; valval = EAnnot (e, ts) } ->
      (* TEMPORARY current ocaml does not support type schemes here; drop quantifiers, if any *)
      fprintf f "%s : %a =%a" id typ ts.body (* scheme ts *) (indent 2 expr) e
  | { valpat = p; valval = e } ->
      fprintf f "%a =%a" pat p (indent 2 expr) e

let valdefs recursive =
  pdefs valdef (if recursive then letrec else letnonrec) et

let typedefs =
  pdefs typedef keytyp et

let excdef in_intf f def =
  match in_intf, def.exceq with
  | _, None
  | true, Some _ ->
      fprintf f "%s" def.excname
  | false, Some s ->
      fprintf f "%s = %s" def.excname s

let excdefs in_intf =
  pdefs (excdef in_intf) exc exc

let block format body f b =
  fprintf f format (fun f b ->
    indent 2 body f b;
    nl f
  ) b

(* Convention: each structure (or interface) item prints a newline before and
   after itself. *)

let rec structure_item f item =
  match item with
  | SIFunctor ([], s) ->
      structure f s
  | SIStretch stretches ->
      List.iter (stretch false f) stretches
  | _ ->
    nl f;
    begin match item with
    | SIFunctor (params, s) ->
        fprintf f "module Make%a%t= %a"
          (list (stretch false) nl) params
          nl
          structend s
    | SIExcDefs defs ->
        excdefs false f defs
    | SITypeDefs defs ->
        typedefs f defs
    | SIValDefs (recursive, defs) ->
        valdefs recursive f defs
    | SIStretch _ ->
        assert false (* already handled above *)
    | SIModuleDef (name, rhs) ->
        fprintf f "module %s = %a" name modexpr rhs
    | SIInclude e ->
        fprintf f "include %a" modexpr e
    | SIComment comment ->
        fprintf f "(* %s *)" comment
    end;
    nl f

and structend f s =
  block "struct%aend" structure f s

and structure f s =
  list structure_item nothing f s

and modexpr f = function
  | MVar x ->
      fprintf f "%s" x
  | MStruct s ->
      structend f s
  | MApp (e1, e2) ->
      fprintf f "%a (%a)" modexpr e1 modexpr e2

let valdecl f (x, ts) =
  fprintf f "val %s: %a" x typ ts.body

let with_kind f = function
  | WKNonDestructive ->
      output_string f "="
  | WKDestructive ->
      output_string f ":="

let rec module_type f = function
  | MTNamedModuleType s ->
      output_string f s
  | MTWithType (mt, params, name, wk, t) ->
      fprintf f "%a%a"
        module_type mt
        (indent 2 with_type) (params, name, wk, t)
  | MTSigEnd i ->
      sigend f i

and with_type f (params, name, wk, t) =
  fprintf f "with type %a %a %a"
    typ (TypApp (name, List.map (fun v -> TypVar v) params))
    with_kind wk
    typ t

and interface_item f item =
  match item with
  | IIFunctor ([], i) ->
      interface f i
  | _ ->
    nl f;
    begin match item with
    | IIFunctor (params, i) ->
        fprintf f "module Make%a%t: %a"
          (list (stretch false) nl) params nl
          sigend i
    | IIExcDecls defs ->
        excdefs true f defs
    | IITypeDecls defs ->
        typedefs f defs
    | IIValDecls decls ->
        pdefs valdecl nothing nothing f decls
    | IIInclude mt ->
        fprintf f "include %a" module_type mt
    | IIModule (name, mt) ->
        fprintf f "module %s : %a" name module_type mt
    | IIComment comment ->
        fprintf f "(* %s *)" comment
    end;
    nl f

and sigend f i =
  block "sig%aend" interface f i

and interface f i =
  list interface_item nothing f i

let program s =
  structure X.f s;
  flush X.f

let interface i =
  interface X.f i;
  flush X.f

let expr e =
  expr X.f e;
  flush X.f

end

(* ------------------------------------------------------------------------- *)
(* Instantiation with output channels. *)

module Make (X : sig
  val f: out_channel
  val locate_stretches: string option
end) = struct
  include PreliminaryMake(struct
    type channel = out_channel
    include X
    let fprintf = Printf.fprintf
    let output_substring = output_substring
  end)
end

(* ------------------------------------------------------------------------- *)
(* Instantiation with buffers. *)

module MakeBuffered (X : sig
  val f: Buffer.t
  val locate_stretches: string option
end) = struct
  include PreliminaryMake(struct
    type channel = Buffer.t
    include X
    let fprintf = Printf.bprintf
    let output_substring = Buffer.add_substring
  end)
end

(* ------------------------------------------------------------------------- *)
(* Common instantiations. *)

let print_expr f e =
  let module P =
    Make (struct
      let f = f
      let locate_stretches = None
    end)
  in
  P.expr e

let string_of_expr e =
  let b = Buffer.create 512 in
  let module P =
    MakeBuffered (struct
      let f = b
      let locate_stretches = None
    end)
  in
  P.expr e;
  Buffer.contents b
