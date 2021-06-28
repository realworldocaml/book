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

open IL
open CodeBits

(* In the following, we only inline global functions. In order to
   avoid unintended capture, as we traverse terms, we keep track of
   local identifiers that hide global ones. The following little class
   helps do that. (The pathological case where a local binding hides a
   global one probably does not arise very often. Fortunately,
   checking against it in this way is quite cheap, and lets me sleep
   safely.) *)

class locals table = object

  method pvar (locals : StringSet.t) (id : string) =
    if Hashtbl.mem table id then StringSet.add id locals else locals

end

(* Here is the inliner. *)

let inline_valdefs (defs : valdef list) : valdef list =

  (* Create a table of all global definitions. *)

  let before, table = Traverse.tabulate_defs defs in

  (* Prepare to count how many times each function is used, including
     inside its own definition. The public functions serve as starting
     points for this discovery phase. *)

  let queue : valdef Queue.t =
    Queue.create()
  and usage : int StringMap.t ref =
    ref StringMap.empty
  in

  (* [visit] is called at every identifier occurrence. *)

  let visit locals id =
    if StringSet.mem id locals then
      (* This is a local identifier. Do nothing. *)
      ()
    else
      try
        let _, def = Hashtbl.find table id in

        (* This is a globally defined identifier. Increment its usage
           count. If it was never visited, enqueue its definition for
           exploration. *)

        let n =
          try
            StringMap.find id !usage
          with Not_found ->
            Queue.add def queue;
            0
        in
        usage := StringMap.add id (n + 1) !usage

      with Not_found ->
        (* This identifier is not global. It is either local or a
           reference to some external library, e.g. ocaml's standard
           library. *)
        ()
  in

  (* Look for occurrences of identifiers inside expressions. *)

  let o =
    object
        inherit [ StringSet.t, unit ] Traverse.fold
        inherit locals table
        method! evar locals () id =
          visit locals id
    end
  in

  (* Initialize the queue with all public definitions, and work from
     there. We assume that the left-hand side of every definition is
     a variable. *)

  List.iter (fun { valpublic = public; valpat = p } ->
    if public then
      visit StringSet.empty (pat2var p)
  ) defs;
  Misc.qfold (o#valdef StringSet.empty) () queue;
  let usage = !usage in

  (* Now, inline every function that is called at most once. At the
     same time, every function that is never called is dropped. The
     public functions again serve as starting points for the
     traversal. *)

  let queue : valdef Queue.t =
    Queue.create()
  and emitted =
    ref StringSet.empty
  in

  let enqueue def =
    let id = pat2var def.valpat in
    if not (StringSet.mem id !emitted) then begin
      emitted := StringSet.add id !emitted;
      Queue.add def queue
    end
  in

  (* A simple application is an application of a variable to a number
     of variables, constants, or record accesses out of variables. *)

  let rec is_simple_arg = function
    | EVar _
    | EData (_, [])
    | ERecordAccess (EVar _, _) ->
        true
    | EMagic e ->
        is_simple_arg e
    | _ ->
        false
  in

  let is_simple_app = function
    | EApp (EVar _, actuals) ->
        List.for_all is_simple_arg actuals
    | _ ->
        false
  in

  (* Taking a fresh instance of a type scheme. Ugly. *)

  let instance =
    let count = ref 0 in
    let fresh tv =
      incr count;
      tv, Printf.sprintf "freshtv%d" !count
    in
    fun scheme ->
      let mapping = List.map fresh scheme.quantifiers in
      let rec sub typ =
        match typ with
        | TypTextual _ ->
            typ
        | TypVar v ->
            begin try
              TypVar (List.assoc v mapping)
            with Not_found ->
              typ
            end
        | TypApp (f, typs) ->
            TypApp (f, List.map sub typs)
        | TypTuple typs ->
            TypTuple (List.map sub typs)
        | TypArrow (typ1, typ2) ->
            TypArrow (sub typ1, sub typ2)
      in
      sub scheme.body
  in

  (* Destructuring a type annotation. *)

  let rec annotate formals body typ =
    match formals, typ with
    | [], _ ->
        [], CodeBits.annotate body typ
    | formal :: formals, TypArrow (targ, tres) ->
        let formals, body = annotate formals body tres in
        PAnnot (formal, targ) :: formals, body
    | _ :: _, _ ->
        (* Type annotation has insufficient arity. *)
        assert false
  in

  (* The heart of the inliner: rewriting a function call to a [let]
     expression.

     If there was a type annotation at the function definition site, it is
     dropped, provided the semantic actions have been type-checked. Otherwise,
     it is kept, because, due to the presence of [EMagic] expressions in the
     code, dropping a type annotation could cause an ill-typed program to
     become apparently well-typed. Keeping a type annotation requires taking a
     fresh instance of the type scheme, because OCaml doesn't have support for
     locally and existentially bound type variables. Yuck. *)

  let inline formals actuals body oscheme =
    assert (List.length actuals = List.length formals);
    match oscheme with
    | Some scheme
      when not Front.ocaml_types_have_been_checked ->

        let formals, body = annotate formals body (instance scheme) in
        mlet formals actuals body

    | _ ->
        mlet formals actuals body
  in

  (* Look for occurrences of identifiers inside expressions, branches,
     etc. and replace them with their definitions if they have only
     one use site or if their definitions are sufficiently simple. *)

  let o =
    object (self)
      inherit [ StringSet.t ] Traverse.map as super
      inherit locals table
      method! eapp locals e actuals =
        match e with
        | EVar id when
            (Hashtbl.mem table id) &&       (* a global identifier *)
            (not (StringSet.mem id locals)) (* not hidden by a local identifier *)
          ->

            let _, def = Hashtbl.find table id in (* cannot fail, thanks to the above check *)

            let formals, body, oscheme =
              match def with
              | { valval = EFun (formals, body) } ->
                  formals, body, None
              | { valval = EAnnot (EFun (formals, body), scheme) } ->
                  formals, body, Some scheme
              | { valval = _ } ->
                  (* The definition is not a function definition. This should not
                     happen in the kind of code that we generate. *)
                  assert false
            in

            assert (StringMap.mem id usage);
            if StringMap.find id usage = 1 || is_simple_app body then

              (* The definition can be inlined, with beta reduction. *)

              inline formals (self#exprs locals actuals) (EComment (id, self#expr locals body)) oscheme

            else begin

              (* The definition cannot be inlined. *)

              enqueue def;
              super#eapp locals e actuals

            end

        | _ ->
            (* The thing in function position is not a reference to a global. *)
            super#eapp locals e actuals

    end
  in

  (* Initialize the queue with all public definitions, and work from
     there. *)

  List.iter (function { valpublic = public } as def ->
    if public then
      enqueue def
  ) defs;

  let valdefs =
    Misc.qfold (fun defs def ->
      o#valdef StringSet.empty def :: defs
    ) [] queue
  in

  Error.logC 1 (fun f ->
    Printf.fprintf f "%d functions before inlining, %d functions after inlining.\n"
       before (List.length valdefs));

  Time.tick "Inlining";

  valdefs

(* Dumb recursive traversal. *)

let rec inline_structure_item item =
  match item with
  | SIValDefs (true, defs) ->
      (* A nest of recursive definitions. Act on it. *)
      SIValDefs (true, inline_valdefs defs)
  | SIFunctor (params, s) ->
      SIFunctor (params, inline_structure s)
  | SIModuleDef (name, e) ->
      SIModuleDef (name, inline_modexpr e)
  | SIInclude e ->
      SIInclude (inline_modexpr e)
  | SIExcDefs _
  | SITypeDefs _
  | SIValDefs (false, _)
  | SIStretch _
  | SIComment _ ->
      item

and inline_structure s =
  List.map inline_structure_item s

and inline_modexpr = function
  | MVar x ->
      MVar x
  | MStruct s ->
      MStruct (inline_structure s)
  | MApp (e1, e2) ->
      MApp (inline_modexpr e1, inline_modexpr e2)

(* The external entry point. *)

let inline (p : program) : program =
  if Settings.code_inlining then
    inline_structure p
  else
    p
