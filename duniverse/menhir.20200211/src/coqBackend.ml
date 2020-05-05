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

open Printf
open Grammar

module Run (T: sig end) = struct
  let from_menhirlib f =
    match Settings.coq_lib_path with
    | None ->
        ()
    | Some path ->
        fprintf f "From %s " path

  let menhirlib_path =
    match Settings.coq_lib_path with
    | None -> ""
    | Some path -> path ^ "."

  let print_term t =
    assert (not (Terminal.pseudo t));
    sprintf "%s't" (Terminal.print t)

  let print_nterm nt =
    sprintf "%s'nt" (Nonterminal.print true nt)

  let print_symbol = function
    | Symbol.N nt -> sprintf "NT %s" (print_nterm nt)
    | Symbol.T t -> sprintf "T %s" (print_term t)

  let print_type ty =
    if Settings.coq_no_actions then "unit"
    else match ty with
         | None -> "unit"
         | Some t -> match t with
                     | Stretch.Declared s -> s.Stretch.stretch_content
                     | Stretch.Inferred _ -> assert false (* We cannot infer coq types *)

  let is_final_state node =
    match Default.has_default_reduction node with
      | Some (prod, _) -> Production.is_start prod
      | None -> false

  let lr1_iter_nonfinal f =
    Lr1.iter (fun node -> if not (is_final_state node) then f node)

  let lr1_iterx_nonfinal f =
    Lr1.iterx (fun node -> if not (is_final_state node) then f node)

  let lr1_foldx_nonfinal f =
    Lr1.foldx (fun accu node -> if not (is_final_state node) then f accu node else accu)

  let print_nis nis =
    sprintf "Nis'%d" (Lr1.number nis)

  let print_init init =
    sprintf "Init'%d" (Lr1.number init)

  let print_st st =
    match Lr1.incoming_symbol st with
      | Some _ -> sprintf "Ninit %s" (print_nis st)
      | None -> sprintf "Init %s" (print_init st)

  let (prod_ids, _) =
    Production.foldx (fun p (prod_ids, counters) ->
      let lhs = Production.nt p in
      let id = try SymbolMap.find (Symbol.N lhs) counters with Not_found -> 0 in
      (ProductionMap.add p id prod_ids, SymbolMap.add (Symbol.N lhs) (id+1) counters))
      (ProductionMap.empty, SymbolMap.empty)

  let print_prod p =
    sprintf "Prod'%s'%d" (Nonterminal.print true (Production.nt p)) (ProductionMap.find p prod_ids)

  let () =
    if not Settings.coq_no_actions then
      begin
        Nonterminal.iterx (fun nonterminal ->
          match Nonterminal.ocamltype nonterminal with
            | None -> Error.error [] "I don't know the type of the nonterminal symbol %s."
                                     (Nonterminal.print false nonterminal)
            | Some _ -> ());
        Production.iterx (fun prod ->
          if not (Keyword.KeywordSet.is_empty (Action.keywords (Production.action prod))) then
            Error.error [] "the Coq back-end supports none of the $ keywords."
        )
      end;

    Production.iterx (fun prod ->
      Array.iter (fun symb ->
        match symb with
          | Symbol.T t ->
              if t = Terminal.error then
                Error.error [] "the Coq back-end does not support the error token."
          | _ -> ())
        (Production.rhs prod));

    if Front.grammar.BasicSyntax.parameters <> [] then
      Error.error [] "the Coq back-end does not support %%parameter."

  let write_tokens f =
    fprintf f "Inductive token : Type :=";
    Terminal.iter_real (fun term ->
        fprintf f "\n| %s : %s%%type -> token"
                (Terminal.print term) (print_type (Terminal.ocamltype term))
    );
    fprintf f ".\n\n"

  let write_inductive_alphabet f name constrs =
    fprintf f "Inductive %s' : Set :=" name;
    List.iter (fprintf f "\n| %s") constrs;
    fprintf f ".\n";
    fprintf f "Definition %s := %s'.\n\n" name name;
    if List.length constrs > 0 then
      begin
        let iteri f = ignore (List.fold_left (fun k x -> f k x; succ k) 1 constrs) in
        fprintf f "Program Instance %sNum : %sAlphabet.Numbered %s :=\n" name menhirlib_path name;
        fprintf f "  { inj := fun x => match x return _ with";
        iteri (fun k constr -> fprintf f "\n    | %s => %d%%positive" constr k);
        fprintf f "\n    end;\n";
        fprintf f "    surj := (fun n => match n return _ with";
        iteri (fprintf f "\n    | %d%%positive => %s");
        fprintf f "\n    | _ => %s\n    end)%%Z;\n" (List.hd constrs);
        fprintf f "    inj_bound := %d%%positive }.\n" (List.length constrs);
      end
    else
      begin
        fprintf f "Program Instance %sAlph : %sAlphabet.Alphabet %s :=\n" name menhirlib_path name;
        fprintf f "  { AlphabetComparable := {| compare := fun x y =>\n";
        fprintf f "      match x, y return comparison with end |};\n";
        fprintf f "    AlphabetEnumerable := {| all_list := []%%list |} }.";
      end

  let write_terminals f =
    write_inductive_alphabet f "terminal" (
      Terminal.fold (fun t l -> if Terminal.pseudo t then l else print_term t::l)
        []);
    fprintf f "Instance TerminalAlph : %sAlphabet.Alphabet terminal := _.\n\n" menhirlib_path

  let write_nonterminals f =
    write_inductive_alphabet f "nonterminal" (
      Nonterminal.foldx (fun nt l -> (print_nterm nt)::l) []);
    fprintf f "Instance NonTerminalAlph : %sAlphabet.Alphabet nonterminal := _.\n\n" menhirlib_path

  let write_symbol_semantic_type f =
    fprintf f "Definition terminal_semantic_type (t:terminal) : Type:=\n";
    fprintf f "  match t with\n";
    Terminal.iter_real (fun terminal ->
        fprintf f "  | %s => %s%%type\n"
          (print_term terminal) (print_type (Terminal.ocamltype terminal))
    );
    fprintf f "  end.\n\n";

    fprintf f "Definition nonterminal_semantic_type (nt:nonterminal) : Type:=\n";
    fprintf f "  match nt with\n";
    Nonterminal.iterx (fun nonterminal ->
                         fprintf f "  | %s => %s%%type\n"
                           (print_nterm nonterminal)
                           (print_type (Nonterminal.ocamltype nonterminal)));
    fprintf f "  end.\n\n";

    fprintf f "Definition symbol_semantic_type (s:symbol) : Type:=\n";
    fprintf f "  match s with\n";
    fprintf f "  | T t => terminal_semantic_type t\n";
    fprintf f "  | NT nt => nonterminal_semantic_type nt\n";
    fprintf f "  end.\n\n"

  let write_token_term f =
    fprintf f "Definition token_term (tok : token) : terminal :=\n";
    fprintf f "  match tok with\n";
    Terminal.iter_real (fun terminal ->
        fprintf f "  | %s _ => %s\n" (Terminal.print terminal) (print_term terminal));
    fprintf f "  end.\n\n"

  let write_token_sem f =
    fprintf f "Definition token_sem (tok : token) : symbol_semantic_type (T (token_term tok)) :=\n";
    fprintf f "  match tok with\n";
    Terminal.iter_real (fun terminal ->
        fprintf f "  | %s x => x\n" (Terminal.print terminal));
    fprintf f "  end.\n\n"

  let write_productions f =
    write_inductive_alphabet f "production" (
      Production.foldx (fun prod l -> (print_prod prod)::l) []);
    fprintf f "Instance ProductionAlph : %sAlphabet.Alphabet production := _.\n\n" menhirlib_path

  let write_productions_contents f =
    fprintf f "Definition prod_contents (p:production) :\n";
    fprintf f "  { p:nonterminal * list symbol &\n";
    fprintf f "    %sGrammar.arrows_right\n" menhirlib_path;
    fprintf f "      (symbol_semantic_type (NT (fst p)))\n";
    fprintf f "      (List.map symbol_semantic_type (snd p)) }\n";
    fprintf f " :=\n";
    fprintf f "  let box := existT (fun p =>\n";
    fprintf f "    %sGrammar.arrows_right\n" menhirlib_path;
    fprintf f "      (symbol_semantic_type (NT (fst p)))\n";
    fprintf f "      (List.map symbol_semantic_type (snd p)) )\n";
    fprintf f "  in\n";
    fprintf f "  match p with\n";
    Production.iterx (fun prod ->
      fprintf f "  | %s => box\n" (print_prod prod);
      fprintf f "    (%s, [%s]%%list)\n"
        (print_nterm (Production.nt prod))
        (String.concat "; "
           (List.map print_symbol (List.rev (Array.to_list (Production.rhs prod)))));
      if Production.length prod = 0 then
        fprintf f "    (\n"
      else
        fprintf f "    (fun %s =>\n"
          (String.concat " " (List.rev (Array.to_list (Production.identifiers prod))));
      if Settings.coq_no_actions then
        fprintf f "tt"
      else
        Printer.print_expr f (Action.to_il_expr (Production.action prod));
      fprintf f "\n)\n");
    fprintf f "  end.\n\n";

    fprintf f "Definition prod_lhs (p:production) :=\n";
    fprintf f "  fst (projT1 (prod_contents p)).\n";
    fprintf f "Definition prod_rhs_rev (p:production) :=\n";
    fprintf f "  snd (projT1 (prod_contents p)).\n";
    fprintf f "Definition prod_action (p:production) :=\n";
    fprintf f "  projT2 (prod_contents p).\n\n"

  let write_nullable_first f =
    fprintf f "Definition nullable_nterm (nt:nonterminal) : bool :=\n";
    fprintf f "  match nt with\n";
    Nonterminal.iterx (fun nt ->
      fprintf f "  | %s => %b\n"
        (print_nterm nt)
        (Analysis.nullable nt));
    fprintf f "  end.\n\n";

    fprintf f "Definition first_nterm (nt:nonterminal) : list terminal :=\n";
    fprintf f "  match nt with\n";
    Nonterminal.iterx (fun nt ->
      let firstSet = Analysis.first nt in
      fprintf f "  | %s => [" (print_nterm nt);
      let first = ref true in
      TerminalSet.iter (fun t ->
        if !first then first := false else fprintf f "; ";
        fprintf f "%s" (print_term t)
        ) firstSet;
      fprintf f "]%%list\n");
    fprintf f "  end.\n\n"

  let write_grammar f =
    fprintf f "Module Import Gram <: %sGrammar.T.\n\n" menhirlib_path;
    fprintf f "Local Obligation Tactic := let x := fresh in intro x; case x; reflexivity.\n\n";
    write_terminals f;
    write_nonterminals f;
    fprintf f "Include %sGrammar.Symbol.\n\n" menhirlib_path;
    write_symbol_semantic_type f;
    fprintf f "Definition token := token.\n\n";
    write_token_term f;
    write_token_sem f;
    write_productions f;
    write_productions_contents f;
    fprintf f "Include %sGrammar.Defs.\n\n" menhirlib_path;
    fprintf f "End Gram.\n\n"

  let write_nis f =
    write_inductive_alphabet f "noninitstate" (
      lr1_foldx_nonfinal (fun l node -> (print_nis node)::l) []);
    fprintf f "Instance NonInitStateAlph : %sAlphabet.Alphabet noninitstate := _.\n\n" menhirlib_path

  let write_init f =
    write_inductive_alphabet f "initstate" (
      ProductionMap.fold (fun _prod node l ->
        (print_init node)::l) Lr1.entry []);
    fprintf f "Instance InitStateAlph : %sAlphabet.Alphabet initstate := _.\n\n" menhirlib_path

  let write_start_nt f =
    fprintf f "Definition start_nt (init:initstate) : nonterminal :=\n";
    fprintf f "  match init with\n";
    Lr1.fold_entry (fun _prod node startnt _t () ->
      fprintf f "  | %s => %s\n" (print_init node) (print_nterm startnt)
    ) ();
    fprintf f "  end.\n\n"

  let write_actions f =
    fprintf f "Definition action_table (state:state) : action :=\n";
    fprintf f "  match state with\n";
    lr1_iter_nonfinal (fun node ->
      fprintf f "  | %s => " (print_st node);
      match Default.has_default_reduction node with
        | Some (prod, _) ->
          fprintf f "Default_reduce_act %s\n" (print_prod prod)
        | None ->
          fprintf f "Lookahead_act (fun terminal:terminal =>\n";
          fprintf f "    match terminal return lookahead_action terminal with\n";
          let has_fail = ref false in
          Terminal.iter_real (fun t ->
            try
              let target = SymbolMap.find (Symbol.T t) (Lr1.transitions node) in
              fprintf f "    | %s => Shift_act %s (eq_refl _)\n" (print_term t) (print_nis target)
            with Not_found ->
              try
                let prod = Misc.single (TerminalMap.find t (Lr1.reductions node)) in
                fprintf f "    | %s => Reduce_act %s\n" (print_term t) (print_prod prod)
              with Not_found -> has_fail := true);
          if !has_fail then
            fprintf f "    | _ => Fail_act\n";
          fprintf f "    end)\n"
    );
    fprintf f "  end.\n\n"

  let write_gotos f =
    fprintf f "Definition goto_table (state:state) (nt:nonterminal) :=\n";
    fprintf f "  match state, nt return option { s:noninitstate | NT nt = last_symb_of_non_init_state s } with\n";
    let has_none = ref false in
    lr1_iter_nonfinal (fun node ->
      Nonterminal.iterx (fun nt ->
        try
          let target = SymbolMap.find (Symbol.N nt) (Lr1.transitions node) in
          fprintf f "  | %s, %s => " (print_st node) (print_nterm nt);
          if is_final_state target then fprintf f "None"
          else fprintf f "Some (exist _ %s (eq_refl _))\n" (print_nis target)
        with Not_found -> has_none := true));
    if !has_none then fprintf f "  | _, _ => None\n";
    fprintf f "  end.\n\n"

  let write_last_symb f =
    fprintf f "Definition last_symb_of_non_init_state (noninitstate:noninitstate) : symbol :=\n";
    fprintf f "  match noninitstate with\n";
    lr1_iterx_nonfinal (fun node ->
      match Lr1.incoming_symbol node with
        | Some s -> fprintf f "  | %s => %s\n" (print_nis node) (print_symbol s)
        | None -> assert false);
    fprintf f "  end.\n\n"

  let write_past_symb f =
    fprintf f "Definition past_symb_of_non_init_state (noninitstate:noninitstate) : list symbol :=\n";
    fprintf f "  match noninitstate with\n";
    lr1_iterx_nonfinal (fun node ->
      let s =
        String.concat "; " (List.tl
          (Invariant.fold (fun l _ symb _ -> print_symbol symb::l)
             [] (Invariant.stack node)))
      in
      fprintf f "  | %s => [%s]%%list\n" (print_nis node) s);
    fprintf f "  end.\n";
    fprintf f "Extract Constant past_symb_of_non_init_state => \"fun _ -> assert false\".\n\n"

  module NodeSetMap = Map.Make(Lr1.NodeSet)
  let write_past_states f =
    let get_stateset_id =
      let memo = ref NodeSetMap.empty in
      let next_id = ref 1 in
      fun stateset ->
        try NodeSetMap.find stateset !memo
        with
        | Not_found ->
           let id = sprintf "state_set_%d" !next_id in
           memo := NodeSetMap.add stateset id !memo;
           incr next_id;
           fprintf f "Definition %s (s:state) : bool :=\n" id;
           fprintf f "  match s with\n";
           fprintf f "  ";
           Lr1.NodeSet.iter (fun st -> fprintf f "| %s " (print_st st)) stateset;
           fprintf f "=> true\n";
           fprintf f "  | _ => false\n";
           fprintf f "  end.\n";
           fprintf f "Extract Inlined Constant %s => \"assert false\".\n\n" id;
           id
    in
    let b = Buffer.create 256 in
    bprintf b "Definition past_state_of_non_init_state (s:noninitstate) : list (state -> bool) :=\n";
    bprintf b "  match s with\n";
    lr1_iterx_nonfinal (fun node ->
      let s =
        String.concat "; "
          (Invariant.fold (fun accu _ _ states -> get_stateset_id states::accu)
            [] (Invariant.stack node))
      in
      bprintf b "  | %s => [ %s ]%%list\n" (print_nis node) s);
    bprintf b "  end.\n";
    Buffer.output_buffer f b;
    fprintf f "Extract Constant past_state_of_non_init_state => \"fun _ -> assert false\".\n\n"

  module TerminalSetMap = Map.Make(TerminalSet)
  let write_items f =
    if not Settings.coq_no_complete then
      begin
        let get_lookaheadset_id =
          let memo = ref TerminalSetMap.empty in
          let next_id = ref 1 in
          fun lookaheadset ->
            let lookaheadset =
              if TerminalSet.mem Terminal.sharp lookaheadset then TerminalSet.universe
              else lookaheadset
            in
            try TerminalSetMap.find lookaheadset !memo
            with Not_found ->
              let id = sprintf "lookahead_set_%d" !next_id in
              memo := TerminalSetMap.add lookaheadset id !memo;
              incr next_id;
              fprintf f "Definition %s : list terminal :=\n  [" id;
              let first = ref true in
              TerminalSet.iter (fun lookahead ->
                if !first then first := false
                else fprintf f "; ";
                fprintf f "%s" (print_term lookahead)
              ) lookaheadset;
              fprintf f "]%%list.\nExtract Inlined Constant %s => \"assert false\".\n\n" id;
              id
        in
        let b = Buffer.create 256 in
        lr1_iter_nonfinal (fun node ->
          bprintf b "Definition items_of_state_%d : list item :=\n" (Lr1.number node);
          bprintf b "  [ ";
          let first = ref true in
          Item.Map.iter (fun item lookaheads ->
            let prod, pos = Item.export item in
            if not (Production.is_start prod) then begin
                if !first then first := false
                else bprintf b ";\n    ";
                bprintf b "{| prod_item := %s; dot_pos_item := %d; lookaheads_item := %s |}"
                        (print_prod prod) pos (get_lookaheadset_id lookaheads);
            end
          )  (Lr0.closure (Lr0.export (Lr1.state node)));
          bprintf b " ]%%list.\n";
          bprintf b "Extract Inlined Constant items_of_state_%d => \"assert false\".\n\n" (Lr1.number node)
        );
        Buffer.output_buffer f b;

        fprintf f "Definition items_of_state (s:state) : list item :=\n";
        fprintf f "  match s with\n";
        lr1_iter_nonfinal (fun node ->
          fprintf f "  | %s => items_of_state_%d\n" (print_st node) (Lr1.number node));
        fprintf f "  end.\n";
      end
    else
      fprintf f "Definition items_of_state (s:state): list item := []%%list.\n";
    fprintf f "Extract Constant items_of_state => \"fun _ -> assert false\".\n\n"

  let write_automaton f =
    fprintf f "Module Aut <: %sAutomaton.T.\n\n" menhirlib_path;
    fprintf f "Local Obligation Tactic := let x := fresh in intro x; case x; reflexivity.\n\n";
    fprintf f "Module Gram := Gram.\n";
    fprintf f "Module GramDefs := Gram.\n\n";
    write_nullable_first f;
    write_nis f;
    write_last_symb f;
    write_init f;
    fprintf f "Include %sAutomaton.Types.\n\n" menhirlib_path;
    write_start_nt f;
    write_actions f;
    write_gotos f;
    write_past_symb f;
    write_past_states f;
    write_items f;
    fprintf f "End Aut.\n\n"

  let write_theorems f =
    fprintf f "Module MenhirLibParser := %sMain.Make Aut.\n" menhirlib_path;

    fprintf f "Theorem safe:\n";
    fprintf f "  MenhirLibParser.safe_validator tt = true.\n";
    fprintf f "Proof eq_refl true<:MenhirLibParser.safe_validator tt = true.\n\n";

    if not Settings.coq_no_complete then
      begin
        fprintf f "Theorem complete:\n";
        fprintf f "  MenhirLibParser.complete_validator tt = true.\n";
        fprintf f "Proof eq_refl true<:MenhirLibParser.complete_validator tt = true.\n\n";
      end;

    Lr1.fold_entry (fun _prod node startnt _t () ->
          let funName = Nonterminal.print true startnt in
          fprintf f "Definition %s : nat -> MenhirLibParser.Inter.buffer -> MenhirLibParser.Inter.parse_result %s := MenhirLibParser.parse safe Aut.%s.\n\n"
            funName (print_type (Nonterminal.ocamltype startnt)) (print_init node);

          fprintf f "Theorem %s_correct (log_fuel : nat) (buffer : MenhirLibParser.Inter.buffer):\n" funName;
          fprintf f "  match %s log_fuel buffer with\n" funName;
          fprintf f "  | MenhirLibParser.Inter.Parsed_pr sem buffer_new =>\n";
          fprintf f "      exists word (tree : Gram.parse_tree (%s) word),\n"
            (print_symbol (Symbol.N startnt));
          fprintf f "        buffer = MenhirLibParser.Inter.app_buf word buffer_new /\\\n";
          fprintf f "        Gram.pt_sem tree = sem\n";
          fprintf f "  | _ => True\n";
          fprintf f "  end.\n";
          fprintf f "Proof. apply MenhirLibParser.parse_correct with (init:=Aut.%s). Qed.\n\n" (print_init node);

          if not Settings.coq_no_complete then
            begin
              fprintf f "Theorem %s_complete (log_fuel : nat) (word : list token) (buffer_end : MenhirLibParser.Inter.buffer) :\n" funName;
              fprintf f "  forall tree : Gram.parse_tree (%s) word,\n" (print_symbol (Symbol.N startnt));
              fprintf f "  match %s log_fuel (MenhirLibParser.Inter.app_buf word buffer_end) with\n" funName;
              fprintf f "  | MenhirLibParser.Inter.Fail_pr => False\n";
              fprintf f "  | MenhirLibParser.Inter.Parsed_pr output_res buffer_end_res =>\n";
              fprintf f "      output_res = Gram.pt_sem tree /\\\n";
              fprintf f "      buffer_end_res = buffer_end /\\ (Gram.pt_size tree <= PeanoNat.Nat.pow 2 log_fuel)%%nat\n";
              fprintf f "  | MenhirLibParser.Inter.Timeout_pr => (PeanoNat.Nat.pow 2 log_fuel < Gram.pt_size tree)%%nat\n";
              fprintf f "  end.\n";
              fprintf f "Proof. apply MenhirLibParser.parse_complete with (init:=Aut.%s); exact complete. Qed.\n" (print_init node);
            end
    ) ()

  let write_all f =
    if not Settings.coq_no_actions then
      List.iter (fun s -> fprintf f "%s\n\n" s.Stretch.stretch_content)
        Front.grammar.BasicSyntax.preludes;

    fprintf f "From Coq.Lists Require List.\n";
    fprintf f "From Coq.PArith Require Import BinPos.\n";
    from_menhirlib f; fprintf f "Require Main.\n";
    if not Settings.coq_no_version_check then
      begin from_menhirlib f; fprintf f "Require Version.\n" end;
    fprintf f "Import List.ListNotations.\n\n";

    if not Settings.coq_no_version_check then
      fprintf f "Definition version_check : unit := %sVersion.require_%s.\n\n" menhirlib_path Version.version;

    fprintf f "Unset Elimination Schemes.\n\n";

    write_tokens f;
    write_grammar f;
    write_automaton f;
    write_theorems f;

    if not Settings.coq_no_actions then
      List.iter (fun stretch -> fprintf f "\n\n%s" stretch.Stretch.stretch_raw_content)
        Front.grammar.BasicSyntax.postludes
end
