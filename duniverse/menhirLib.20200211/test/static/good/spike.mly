/*
* Project: Spike ver 0.1
  * File: parser.mly

* Content: Parser of specification files
    */

%{

open Values
open Context
open Critical_context_set
open Diverse
open Io
open Dicos
open Symbols
open Terms
open Terms_parser
open Order
open Literals
open Clauses
open Dummies
open Strategies
open Test_sets
open Shell
open Extract

let introduce_var_exist c =
  let rec fn_term lv t =
    match t#content with
	Var_exist _ -> t
      | Var_univ (i, s) -> if List.mem (i, s, true) lv then t else new term (Var_exist (i, s))
      | Term (i, l, s) -> new term (Term (i, List.map (fn_term lv) l, s))
  in
  let fn_lit lv lit =
    match lit#content with
	Lit_rule (t1, t2) -> new literal (Lit_rule ((fn_term lv t1), (fn_term lv t2)))
      | Lit_equal (t1, t2) -> new literal (Lit_equal ((fn_term lv t1), (fn_term lv t2)))
      |	Lit_diff (t1, t2) -> new literal (Lit_diff ((fn_term lv t1), (fn_term lv t2)))
  in
  let (x, y, c') = c in
  let (n, p) = c'#content in
  let var_lhs = (c'#lefthand_side)#variables in
  let n' = List.map (fn_lit var_lhs) n in
  let p' = List.map (fn_lit var_lhs) p in
  let new_c' = new clause (n', p') [] ("",0,([],[])) in
  (x, y, new_c')

(* If no ordering is specified in the specification file, we use a total ordering based on symbol codes *)
let default_fill_order_dico () =
  let fn c =
    let ldef_symb = all_nonvariable_symbols c in
    let lhs,rhs = c#both_sides in
    let lhs_head_symbol =
      try
	(match lhs#content with
	    Term (f, _, _) -> f
	  | Var_exist _| Var_univ _ -> failwith "default_fill_order_dico"
	)
      with Not_Horn -> failwith "default_fill_order_dico"
    in
    let r_cond_symb = try
      remove_el ( = ) lhs_head_symbol ldef_symb
    with Failure "remove_el" -> failwith "default_fill_order_dico"
    in
    let () = if !debug_mode then
      let () = buffered_output c#string in
      let () = print_string "\n" in
      let () = print_int lhs_head_symbol in
      let () = print_string "\n" in
      let () = print_list ", " print_int r_cond_symb in
      let () = print_string "\n" in
      let () = flush stdout in
	()
    in

    let is_orientable =
      try
	let rhs_head_symbol =
	  try
	    (match rhs#content with
		 Term (f, _, _) -> f
	       | Var_exist _| Var_univ _ -> failwith "variable"
	    )
	  with Not_Horn -> failwith "default_fill_order_dico"
	in
	  not (List.mem lhs_head_symbol (try dico_order#find rhs_head_symbol with Not_found -> []))
      with Failure "variable" -> true
    in
      if is_orientable then
	List.iter (dico_order#add_couple lhs_head_symbol) r_cond_symb
  in
  let () = buffered_output "Setting default greater order for symbols" in
  let () = flush stdout in
  let axioms = List.map (fun (_, _, x) -> x) !yy_axioms in
  let () = if !debug_mode then
    let () = print_string "\n Current axioms :" in
    let () = print_clause_list axioms in
    let () = print_dico_const_string () in
      ()
  in
  let _ = List.iter fn axioms in
  let () =
    try
      dico_order#merge_equivalence_relation dico_equivalence ;
    with (Failure "rehash") ->
      parse_failwith "there are incompatibilities between the order and equivalence relations"
  in
  if !debug_mode then
    let () = print_dico_order () in
    let () = print_dico_equivalence () in
    ()

let share_variables s s' =
  let rec fn s =
    match s with
	Def_sort _ -> []
      | Abstr_sort0 str -> [str]
      | Abstr_sort1 (_, sort) -> fn sort
      | Abstr_sort2 (_, s1, s2) -> (fn s1) @ (fn s2)
  in
  let lvar_s = fn s in
  let lvar_s' = fn s' in
  List.exists (fun x -> List.mem x lvar_s) lvar_s'


  (* to be continued  *)



(* Parse a positive integer *)
let parse_positive_int s =
  let i =
    try int_of_string s
    with (Failure "int_of_string") -> parse_failwith "not a positive integer"
  in if i < 0
  then parse_failwith "not a positive integer"
  else i

(* Get sort id from string *)
let find_sort_id s =
  try dico_sort_string#find_key s
  with Failure "find_key" ->
    if not (* !specif_paramete
rized  *) true
    then parse_failwith ("unknown sort \"" ^ s ^ "\"")
    else
      let () = if !debug_mode then print_string ("\nWARNING: the sort " ^ s ^ " is parameterized") in
      Abstr_sort0 s


(* Get symbol id from string *)
let find_symbol_id s =
  try dico_const_string#find_key s
  with Failure "find_key" -> parse_failwith ("undefined symbol \"" ^ s ^ "\"")

  (* Provided an integer reference i, a value, and a (int, _) dictionary, we add the couple
     (i, v) if v is not already t here and increment i (decrement if negative), do nothing otherwise.
     Returns key *)
let selective_add d i v =
  try d#find_key v with
    Failure "find_key" -> let n = !i in d#add n v; if n >= 0 then incr i else decr i; n
;;

let selective_add_sort d i v =
  try d#find_key v with
    Failure "find_key" -> let n = !i in d#add (Def_sort n) v; if n >= 0 then incr i else decr i; Def_sort n
;;


  (* tests if there is a parameterized sort in the clause content c  *)
let test_well_founded_cl c =
  let fn_sort s =
    match s with
	Def_sort _ -> true
      | Abstr_sort0 _| Abstr_sort1 _ | Abstr_sort2 _ -> let () = buffered_output ("\nThe sort " ^ (sprint_sort s) ^ " is parameterized") in false
  in
  let fn_term t =
    match t#content with
	Var_exist (_, s) | Var_univ (_, s) -> fn_sort s
      | Term (_, _, s) -> fn_sort s
  in
  let fn_lit l =
    match l#content with
	Lit_rule (t1, t2) -> (fn_term t1) && (fn_term t2)
      | Lit_equal (t1, t2) -> (fn_term t1) && (fn_term t2)
      | Lit_diff (t1, t2) -> (fn_term t1) && (fn_term t2)
  in
  let (negs, poss) = c in
  List.fold_right (fun x y -> fn_lit x && y) (negs @ poss) true

(*
   * Given:
   * - a counter (either variables or constants)
   * - a string
   * - a list of sorts (might be empty)
   * - a sort
   * updates the dictionaries to create a symbol with the given profile
*)
let process_specif_symbol counter s (l, rs) =
  let sym, l_ar, r_ar = process_underscores s
  in begin
    try
      let _ = dico_const_string#find_key sym
      in parse_failwith ("symbol \"" ^ sym ^ "\" already defined")
    with Failure "find_key" -> ()
  end ;
  if (l_ar + r_ar) <> List.length l
  then parse_failwith ("mismatch between declared arities and profile")
  else () ;
  let v = selective_add dico_const_string counter sym in
  let new_profile = update_profile (rs::l) in
  let sort_v = List.hd new_profile in
  dico_const_profile#add v new_profile ;
  dico_const_sort#add v sort_v ;
  dico_arities#add v (l_ar, r_ar)

let process_function_props list_symb prop =
  let fn id =
      try
	let sym = dico_const_string#find_key id in
    	let l_ar, r_ar = dico_arities#find sym in
    	if ((l_ar = 0 && r_ar = 2) || (l_ar = 1 && r_ar = 1))
    	then dico_properties#add sym prop
    	else parse_failwith ("symbol \"" ^ id ^ "\" has a profile incompatible with its " ^
    	(match prop with Prop_ac -> "AC" | Prop_assoc -> "ASSOC" | Prop_peano -> "") ^ " properties")
      with Failure "find_key" -> parse_failwith ("symbol \"" ^ id ^ "\" is not defined")
	| Not_found -> failwith "raising Not_found in process_function_props"
  in
  let () =  assert (  prop =  Prop_ac or prop = Prop_assoc) in
  let _ = List.map fn list_symb in
  ()



(* Given a string and a status, update the status dictionary accordingly *)
let add_to_status_dico c st =
  try
    let old_st = dico_id_status#find c
    in if old_st = st
    then ()
    else parse_failwith ("attempt to define different statuses to symbol \""
                         ^ (dico_const_string#find c)
                         ^ "\"")
  with Not_found -> dico_id_status#add c st




(* In the case where an explicit type is given to a variable, check that it is compatible with the remaining equation *)
let check_explicit_type v s =
  (* Checks that it is not a functional symbol *)
  let () =
    try
      let _ = dico_const_string#find_key v
      in parse_failwith ("attempting to redefine sort of functional symbol " ^ v)
    with Failure "find_key" -> ()
  and id = code_of_var v
  and sort_id = find_sort_id s in
  try
    let sort_id2 = List.assoc id !yy_tmp_sorts in
    if sort_id <> sort_id2
    then parse_failwith ("conflicting sorts "
                         ^ s
                         ^ " (declared) and "
                         ^ (dico_sort_string#find sort_id2)
                         ^ " (infered) for symbol "
                         ^ v)
    else ()
  with Not_found ->
    yy_tmp_sorts := generic_insert_sorted (id, sort_id) !yy_tmp_sorts

(*
   * The core function: add a new incomplete tree at the first undefined node in "yy_incomplete_tree".
   * This new tree can be either a Variable node, or a new node with at the root the id of the given symbol,
   * and as arguments, as many trees as the left arity picked from the stack yy_terms_stack, and as many
   * Undefined nodes as the right arity
   * If no place can be found (the tree is complete), the whole tree is pushed into the stack, and yy_incomplete_tree
   * becomes the produced tree
   * When the whole term has been parsed, we should have an empty stack and a complete tree
*)
let add_to_incomplete_tree tk =
  let fn1 tk =
    let () = if !debug_mode then (print_string "\n incomplete tree >>>>>  "; print_term_token tk) else () in
    match tk with
	TT_ident s ->
          begin
            try
	      let id = dico_const_string#find_key s in
	      let l_ar, r_ar = dico_arities#find id in
	(* let () = buffered_output ("Popping " ^ (string_of_int l_ar) ^ " elements from stack") ; flush stdout in *)
	      let l_args = pop_n_times l_ar yy_terms_stack in
	      let r_args = list_init r_ar (Undefined: incomplete_tree pointer)
	      in
	      Defined (Iterm (id, l_args @ r_args))
            with Failure "find_key"
	      | Not_found -> let id = code_of_var s in Defined (Ivar id)
          end
      | TT_tree t -> Defined t
  in
  let rec fn h t tk =
    match h with
      Undefined -> (fn1 tk) :: t
    | Defined (Ivar _) -> h :: (fn2 tk t)
    | Defined (Iterm (s', l)) ->
        try
          let l' = fn2 tk l in
          (Defined (Iterm (s', l'))) :: t
        with Failure "fn2" ->
          h :: (fn2 tk t)
  and fn2 tk = function
      [] -> failwith "fn2"
    | h :: t -> fn h t tk
  in
  if incomplete_tree_is_complete !yy_incomplete_tree
  then
    begin
      if !debug_mode then
	buffered_output ("Pushing " ^ (sprint_incomplete_tree_pointer !yy_incomplete_tree) ^ " into stack")
      else () ;
      Stack.push !yy_incomplete_tree yy_terms_stack ;
      yy_incomplete_tree := fn1 tk
    end
  else
    yy_incomplete_tree := List.hd (fn !yy_incomplete_tree [] tk)


let sprint_bool flag = match flag with true -> "True" | false -> "False"

(* We now process a list of identifiers (tokens) *)
let process_term_tokens =
  let rec fn = function
      [] ->
	let empty_stack = Stack.length yy_terms_stack = 0 in
	let complete_tree = incomplete_tree_is_complete !yy_incomplete_tree in
        if  (not empty_stack) or not complete_tree
        then parse_failwith "badly formed term"
        else
          let t = !yy_incomplete_tree in
          let () = yy_incomplete_tree := Undefined in
          t
    | h::t ->
        let () = add_to_incomplete_tree h in
        fn t
  in fn

let is_param_defined_sort s =
  match s with
      Def_sort  _ -> true
    | Abstr_sort0 _| Abstr_sort1 _ | Abstr_sort2 _ -> false


let is_param_sort1 s =
  match s with
      Abstr_sort1 _ -> true
    | Def_sort _| Abstr_sort0 _ | Abstr_sort2 _ -> false

let is_param_sort0 s =
  match s with
      Abstr_sort0 _ -> true
    | Def_sort _| Abstr_sort1 _ | Abstr_sort2 _  -> false

let is_param_sort2 s =
  match s with
      Abstr_sort2 _ -> true
    | Abstr_sort1 _ | Abstr_sort0 _ | Def_sort _  -> false

let get_id_param_sort s =
  match s with
      Abstr_sort1 (i, _) -> i
    | Abstr_sort2 (i, _, _) -> i
    | Def_sort _| Abstr_sort0 _ -> failwith "get_id_param_sort"
	(*
   * Typecheck an incomplete tree, infer type of variables.
   * In one case, we need to delay typechecking: when a literal of the form x = y is present.
   * It means that the actual terms creation must be delayed to the end of the parsing of an axiom / clause.
*)

let rec typecheck_incomplete_tree ps t =
  let () = if !debug_mode then buffered_output ("\nenter typecheck_incomplete_tree: the parameters ps and t are: " ^ ((sprint_param_sort ps) ^ "  " ^ (sprint_incomplete_tree_pointer t)))  in
  match t with
      Undefined -> invalid_arg "typecheck_incomplete_tree"
    | Defined (Ivar x) -> (
        try
          let s' = List.assoc x !yy_tmp_sorts in
          let new_s' =
	    match ps with
		Actual_sort s'' ->
		  (try
		    unify_sorts ps s'
		  with Failure "unify_sorts" ->  parse_failwith ("\nConflicting types: " ^ (sprint_sort s') ^ " and " ^ (sprint_sort s''))
		  )
            | Variable_sort x' -> (* We have a sort for y in x = y ; we update the sort of x_{E} *)
		let l = yy_tmp_equiv_dico#find x' in
		 let () = List.iter (fun v -> yy_tmp_sorts := generic_insert_sorted (v, s') !yy_tmp_sorts) l in
		 let () = yy_tmp_equiv_dico#remove x' in
		 s'
	  in
	  if x < 0 then new term (Var_exist (x, new_s')) else new term (Var_univ (x, new_s'))
        with Not_found ->
          let new_s' = match ps with
	      Actual_sort s'' ->
		let new_s'' = expand_sorts s'' in
		let () = yy_tmp_sorts := generic_insert_sorted (x, new_s'') !yy_tmp_sorts in
		new_s''
            | Variable_sort x' ->
		let () = yy_tmp_equiv_dico#add_couple x x' in (* x has a fresh sort *)
		let () = param_sort_counter := !param_sort_counter + 1 in
		let str = ("'Undefined" ^ (string_of_int !param_sort_counter)) in
		let () = yy_tmp_sorts := generic_insert_sorted (x, Abstr_sort0 str) !yy_tmp_sorts in
		Abstr_sort0 str
	  in
	  if x < 0 then new term (Var_exist (x, new_s')) else new term (Var_univ (x, new_s'))
      )
    | Defined (Iterm (x, l)) ->
        let p =
	  try
	    dico_const_profile#find x
	  with Not_found -> parse_failwith ("constant " ^ (string_of_int x) ^ "not found in dico_const_profile")
        in
	let p' =  update_profile p  in
	let s' = List.hd p'
        and a' = List.tl p' in
        let () = match ps with
            Actual_sort s'' ->
              (try let _ = unify_sorts ps s' in () with Failure "unify_sorts" ->
(* 		let () = if !debug_mode then print_string ("\ncall of unify_sorts in parser.mly:  the list yy_tmp_param_sorts before application is : " ^ *)
(* 		(List.fold_right (fun (x, s) y -> (x ^ " has associated the sort " ^ (sprint_sort s) ^ ", " ^ y)) !yy_tmp_param_sorts "")) else () in  *)
		parse_failwith ("\n Error: sort " ^ (sprint_sort s'') ^ " is not unifiable with " ^ (sprint_sort s')) )
          | Variable_sort x' ->
              try
                let new_x' = yy_tmp_equiv_dico#find x' in
                List.iter (fun v -> yy_tmp_sorts := generic_insert_sorted (v, s') !yy_tmp_sorts) new_x';
                yy_tmp_equiv_dico#remove x'
              with Not_found ->
                yy_tmp_sorts := generic_insert_sorted (x', s') !yy_tmp_sorts
        in
	let new_s' = unify_sorts ps s' in
        let a'' = List.map (fun v -> Actual_sort v) a' in
        let terms_l = List.map2 typecheck_incomplete_tree a'' l in
	new term (Term (x, terms_l, new_s'))

let term_with_new_sort t s =
  match t#content with
      Var_exist (i, _) -> new term (Var_exist (i, s))
    | Var_univ (i, _) -> new term (Var_univ (i, s))
    | Term (i, l, _) -> new term (Term (i, l, s))

let literal_of_incomplete_terms lit =
  let x, x', tlit = lit in
  let new_tx = x#expand_sorts in
  let new_tx' = x'#expand_sorts in
  let s = try unify_sorts (Actual_sort new_tx#sort) new_tx'#sort with Failure "unify_sorts" -> failwith "literal_of_incomplete_terms" in
  let t = term_with_new_sort new_tx s in
  let t' = term_with_new_sort new_tx' s in
  let () = if !debug_mode then ((print_detailed_term t); (print_detailed_term t')) in
  match tlit with
      Lit_equal (_, _) -> new literal (Lit_equal (t, t'))
    | Lit_rule (_, _) -> new literal (Lit_rule (t, t'))
    | Lit_diff (_, _) -> new literal (Lit_diff (t, t'))

(* Table of oracles (string, boolean reference) *)
let oracles_table = Hashtbl.create 13

let _ = List.iter (fun (kwd, tok) -> Hashtbl.add oracles_table kwd tok)
    [ ("system_is_sufficiently_complete",               system_is_sufficiently_complete) ;
      ("system_is_strongly_sufficiently_complete",      system_is_strongly_sufficiently_complete) ;
      ("system_is_ground_convergent",                   system_is_ground_convergent) ]

(* Table of tests (string, () -> ()) *)
let tests_table = Hashtbl.create 13
let _ = List.iter (fun (kwd, tok) -> Hashtbl.add tests_table kwd tok)
    [ ("do_sufficient_completeness_test",               sufficient_completeness_test) ;
      ("do_strongly_sufficient_completeness_test",      strongly_sufficient_completeness_test) ;
      ("do_ground_convergence_test",                    ground_convergence_test) ]
;;

(* returns a list of clauses by deleting the min and max symbols from c *)

let del_minmax c =
  let rec delt_minmax t =
    match t#content with
      | Var_univ _ | Var_exist _ -> [([], t)]
      | Term (f, l, s) ->
	  let  megamix12 =
	    megamix (List.fold_right (fun t lres -> (delt_minmax t) :: lres) l [])
	  in
	  let res = List.fold_right (
	    fun l1'  lres ->
	      if f == id_symbol_min || f == id_symbol_max then
		(* let _ = buffered_output ("Here delt_minmax is " ^ (dico_const_string#find f) ^ " and value is " ^ (string_of_int f)) in *)
		let (l1, t1) = List.hd l1' in
		let (l2, t2) = List.hd (List.tl l1') in
		let tless = new term (Term (id_symbol_less, [t1;t2], id_sort_bool)) in
		let tge = new term (Term (id_symbol_geq, [t1;t2], id_sort_bool)) in
		let litless = new literal (Lit_equal (tless, new term (Term (id_symbol_true, [], id_sort_bool)))) in
		let litge = new literal (Lit_equal (tge, new term (Term (id_symbol_true, [], id_sort_bool)))) in
		if f == id_symbol_min then (litless :: (l1@l2), t1) :: ((litge:: (l1@l2), t2) :: lres)
		else if f == id_symbol_max then (litless:: (l1@l2), t2) :: ((litge:: (l1@l2), t1) :: lres)
		else ((l1@l2), new term (Term (f, [t1;t2], s))) :: lres
		else
		  let nl, l' =  (List.fold_right (fun (l1,t) (ll, lt) -> (l1 @ ll, t::lt)) l1' ([],[])) in
		  (nl, new term (Term (f, l',s))) :: lres
	  )  megamix12 []
	  in
	  if res == [] then [([], t)] else res

  in
  let dellit_minmax lit =
       match lit#content with
	 | Lit_equal (tl, tr) ->
	   let tl' = delt_minmax tl in
	   let tr' = delt_minmax tr in
	   let megamix12 = megamix [tl'; tr'] in
	   List.fold_right (fun l lres ->
	     let (l1, t1) = List.hd l in
	     let (l2, t2) = List.hd (List.tl l) in
	     [(l1@l2), new literal (Lit_equal (t1, t2))] @ lres) megamix12 []
	 | Lit_rule (tl, tr) ->
	   let tl' = delt_minmax tl in
	   let tr' = delt_minmax tr in
	   let megamix12 = megamix [tl'; tr'] in
	   List.fold_right (fun l lres ->
	     let (l1, t1) = List.hd l in
	     let (l2, t2) = List.hd (List.tl l) in
	     [(l1@l2), new literal (Lit_rule (t1, t2))] @ lres) megamix12 []
	 | Lit_diff (tl, tr) ->
	   let tl' = delt_minmax tl in
	   let tr' = delt_minmax tr in
	   let megamix12 = megamix [tl'; tr'] in
	   List.fold_right (fun l lres ->
	     let (l1, t1) = List.hd l in
	     let (l2, t2) = List.hd (List.tl l) in
	     [(l1@l2), new literal (Lit_diff (t1, t2))] @ lres) megamix12 []
  in
  let rec split_f l l' len =
    if len == 0 then (l, l')
    else
      try
	let l1 = List.hd l' in
	split_f (l1::l) (List.tl l') (len - 1)
      with Failure "hd" ->
	failwith "split_f"
  in
  let lnegs = c#negative_lits in
  let len_nlits = List.length lnegs in
  let lpos = c#positive_lits in
  let nlits_mm = List.map (fun l -> (dellit_minmax l)) lnegs in
  let npos_mm = List.map (fun l -> (dellit_minmax l)) lpos in
  let mm = megamix (nlits_mm @ npos_mm) in
    (* if nlits_mm == [] then npos_mm  *)
    (* else if npos_mm == [] then nlits_mm  *)
    (* else megamix (nlits_mm @ npos_mm) in *)
  List.map (fun ll ->
    let (ln', lp') = split_f [] ll len_nlits in
    let (ln1, ln) = List.fold_right (fun (lnegs, lit) (lln, llits) -> (lnegs @ lln, lit :: llits)) ln' ([],[])  in
    let (lp1, lp) = List.fold_right (fun (lposs, lit) (llp, llits) -> (lposs @ llp, lit :: llits)) lp' ([],[])  in
    let nlneg = lp1 @ ln1 @ ln in
    let nlpos = lp in
    let nlneg' = expand_sorts_list nlneg in
    let nlpos' = expand_sorts_list nlpos in
    let () = if not !specif_parameterized && not (test_well_founded_cl (nlneg', nlpos')) then
      failwith "clause3: undefined types"
    in
    new clause (nlneg', nlpos') [] ("",0,([],[]))
    (* c#build nlneg' nlpos' *)
  ) mm
    %}

/* End of file. Specification file may also end with the "end" keyword */
%token TOK_EOF

/* Punctuation */
%token TOK_COLUMN
       TOK_COMA
       TOK_ARROW
       TOK_SEMICOLUMN
       TOK_LPAR
       TOK_RPAR
       TOK_IMPLIES
       TOK_EQUAL
       TOK_DIFF
       TOK_AND
       TOK_OR
       TOK_QUESTION_MARK
       TOK_AROBAT
       TOK_LBRACKET
       TOK_RBRACKET

/* Field declarators */
%token TOK_SPECIF
       TOK_SORTS
       TOK_CONSTRUCTORS
       TOK_FUNCTIONS
       TOK_FUNCTION_PROPS
       TOK_EQUIV
       TOK_STATUS
       TOK_AXIOMS
       TOK_OBS_SORTS
       TOK_CONJECTURES
       TOK_COMPLETE_TERMS
       TOK_LEMMAS
       TOK_STRATEGIES
       TOK_USE
       TOK_PROPERTIES
       TOK_PRIORITIES
       TOK_IND_PRIORITIES
       TOK_TEST_SETS
       TOK_NULLARY_SORTS
       TOK_NORM
       TOK_RPOCOMPARE
       TOK_COMPARE
       TOK_MAX_COMPARE
       TOK_STOP_ON_CLAUSE
       TOK_EXTRACT
       TOK_MATCH
       TOK_AC_SUBSUMES
       TOK_CRITICAL_CONTEXT_SETS
       TOK_CRITIC
       TOK_HYPOTHESES

/* Keywords for order-status */
%token TOK_LEFTRIGHT
       TOK_MULTISET
       TOK_RIGHTLEFT

/* AC symbols */
%token TOK_AC
       TOK_ASSOC

/* Strategy keywords */
%token TOK_TRY
       TOK_SATURATE
       TOK_ON
       TOK_REDUCTION
       TOK_REPEAT
       TOK_REPEAT_PLUS
       TOK_TAUTOLOGY
       TOK_GENERATE
       TOK_GENERATE_EQ
       TOK_GENERATE_OBS
       TOK_PARTIAL_CASE_REWRITING
       TOK_TOTAL_CASE_REWRITING
       TOK_CONTEXTUAL_REWRITING
       TOK_CONGRUENCE_CLOSURE
       TOK_EQUATIONAL_REWRITING
       TOK_REWRITING
       TOK_NEGATIVE_DECOMPOSITION
       TOK_POSITIVE_DECOMPOSITION
       TOK_POSITIVE_CLASH
       TOK_ELIMINATE_TRIVIAL_LITERAL
       TOK_ELIMINATE_REDUNDANT_LITERAL
       TOK_AUTO_SIMPLIFICATION
       TOK_COMPLEMENT
       TOK_SUBSUMPTION
       TOK_AUGMENTATION
       TOK_NEGATIVE_CLASH
       TOK_START_WITH
       TOK_AUGMENTATION_STRATEGY
       TOK_GOTO
       TOK_ADDPREMISE
       TOK_SIMPLIFY
       TOK_GREATER
       TOK_DELETE
       TOK_ID
       TOK_PRINT_GOALS
       TOK_PRINT_CAML
       TOK_PRINT_GOALS_HISTORY

/* Keywords for substitutions */

       TOK_OPEN_SUBSTITUTION
       TOK_CLOSE_SUBSTITUTION


/* Basically any string except the previous */
%token <string> TOK_IDENT
%token <string> TOK_STRING
/* %token <string list> TOK_IDENT_LIST */

%start specif
%type < Strategies.problem_token Queue.t > specif

%start strategy_term
%type < Strategies.strategy > strategy_term

%start reasoning_module
%type < Strategies.reasoning_module > reasoning_module

%start list_of_systems
%type < Clauses.which_system list > list_of_systems

%start specif_clausal_position
%type < Dummies.position_argument > specif_clausal_position

%start specif_literal_position_in_clause
%type < Dummies.position_argument > specif_literal_position_in_clause

%start specif_substitution
%type < (Symbols.var * Terms.term) list > specif_substitution

%start specif_positive_int
%type < int > specif_positive_int

%start get_term
%type < Terms.term > get_term

%start specif_term
%type < Terms_parser.term_token list > specif_term

%start specif_clause2
%type < Clauses.peano_context Clauses.clause > specif_clause2

%start specif_shell_command
%type < Dummies.shell_commands > specif_shell_command

%%

specif:
  spec_fields  spec_ordering spec_prop spec_problem TOK_EOF
  { yy_queue }
| spec_fields spec_ordering spec_prop TOK_EOF
  {
    let q = Queue.create ()
    in let () = Queue.add (Message_token "Correct specification") q
    in q
  }

/* Three kinds of specifications. To be put in three different files */

spec_fields:
  opt_specif_name
  opt_specif_use
  opt_specif_sorts
  opt_specif_obs_sorts
  opt_specif_constructors
  opt_specif_functions
  opt_specif_axioms
  {
  update_dico_free_constructors () ;
    if !free_constructors
    then buffered_output "All constructors are free"
    else () ;
    all_defined_functions := List.map (fun x -> - x) (List.tl (list_create (- !function_counter))) ;
    all_constructors := list_create !constructor_counter
(*     default_fill_order_dico_cc (); *)
  }

spec_ordering:
  opt_specif_greater
  opt_specif_equivs
  opt_specif_status
  opt_specif_test_sets
  opt_specif_nullary_sorts
  opt_specif_function_props
  {
    let () = determine_ac_category () in
    (* Orient axioms *)
    let rec fn = function
      	[] -> []
      | (f, l, c)::t ->
          try
            let c' = c#orient in
            let () = buffered_output ("\t" ^ c'#string) in
            (f, l, c')::fn t
          with (Failure "orient") ->
            parsed_gfile := f ;
            linenumber := l ;
	    let concl = List.hd ((fun (_, p) -> p) c#content) in
	    match concl#content with
		Lit_equal _
	      | Lit_rule _ ->
      		  let c' = c#force_orientation in
		  let () = buffered_output ("\t" ^ c'#string) in
		  (* let () = broken_order := true in  *)
		  let () = buffered_output ("\nWARNING: the axiom [" ^ (string_of_int c#number) ^ "] is not orientable in a rewrite rule using the current order") in
		  (f, l, c')::fn t

	      | Lit_diff _ -> parse_failwith ("The axiom [" ^ (string_of_int c#number) ^ "] is not orientable")
    in
    buffered_output "Orienting axioms" ;
(*     if !use_order *)
(*     then *)
    let l = fn !yy_axioms
    in let () = yy_axioms := l in
(*     else *)
(*       () ; *)
    rewrite_system#init (List.map (fun (_, _, x) -> x) !yy_axioms) ;

(*    print_clause_list rewrite_system#content ;*)
(*     buffered_output "\nThe current order is :"; *)
    print_dico_order ();
    print_dico_equivalence ();
    buffered_output ("Computing nullary sorts") ;
    flush stdout ;
    update_dico_sort_nullarity () ;

    buffered_output ("Computing nullary individuals") ;
    flush stdout ;
    update_dico_nullary_individuals () ;

    if !observational_proof then
      begin
	buffered_output ("Using test-sets version " ^ (string_of_int !test_set_version)) ;
	buffered_output "Computing test sets" ;
	if List.length !yy_axioms > 0
	then !compute_test_set () ;
	!print_dico_test_set () ;

	compute_critical_context_set ();
      end;

(*     buffered_output ("Using test-sets version " ^ (string_of_int !test_set_version)) ; *)
(*     buffered_output "Computing test sets" ; *)
(*     if (List.length !yy_axioms > 0) && (not !int_specif_defined) (* the int sort cannot be computed with the test set version 0 in "int" theory *) *)
(*     then !compute_test_set ()  *)
(*     else if (!int_specif_defined) then *)
(*       rewrite_system#compute_induction_positions_v0        *)
(*     else (); *)
    if !boolean_specification
    then buffered_output "We have a boolean specification"
    else buffered_output "We do not have a boolean specification" ;
}

spec_prop:
  opt_specif_properties
  opt_specif_priorities
  opt_specif_critical_context_sets
  { }

spec_problem:
  spec_problem_field
  { [$1] }
| spec_problem spec_problem_field
  { $1 @ [$2] }

spec_problem_field:
  specif_lemmas
  { }
| specif_conjectures
  { }
| specif_hypotheses
  { }
| specif_complete_terms
  { }
| specif_ind_priorities
  { }
| specif_strategies
  { }
| specif_startpoint
  { }
| specif_augmentation
  { }
| specif_norm
  { }
| specif_rpocompare
  { }
| specif_compare
  { }
| specif_max_compare
  { }
| specif_stop_on_clause
  { }
| specif_extract
  { }
| specif_match
  { }
| specif_ac_subsumes
  { }
| print_caml
  { }

/* System specifications */

opt_specif_name:
  TOK_SPECIF TOK_COLUMN TOK_IDENT
  { spec_name := $3 }
| TOK_SPECIF TOK_COLUMN
  { }
|
  { }

opt_specif_use:
  TOK_USE TOK_COLUMN list_of_idents TOK_SEMICOLUMN
  {
    let rec fn = function
      [] -> ()
    | h::_ ->
        try
          add_predefined_specif h
        with (Failure "add_predefined_specif") ->
          parse_failwith ("\"" ^ h ^ "\" is not a valid predefined specification") in
    fn $3 }
| TOK_USE TOK_COLUMN
  { }
|
  { }

list_of_idents:
  TOK_IDENT
  { [ $1 ] }
| list_of_idents TOK_IDENT
  { $1 @ [ $2 ] }

opt_specif_sorts:
  TOK_SORTS TOK_COLUMN get_list_of_sorts TOK_SEMICOLUMN
  { buffered_output "\nSuccessfully parsed sorts" ;
    flush stdout }
| TOK_SORTS TOK_COLUMN
  { }
|
  { }

get_list_of_sorts:
  TOK_IDENT
  { selective_add_sort dico_sort_string sort_counter $1 }
| get_list_of_sorts TOK_IDENT
  { selective_add_sort dico_sort_string sort_counter $2 }

opt_specif_constructors:
  TOK_CONSTRUCTORS TOK_COLUMN list_of_constructors
  { buffered_output "\nSuccessfully parsed constructors" ;
    flush stdout }
| TOK_CONSTRUCTORS TOK_COLUMN
  { }
|
  { }

opt_specif_obs_sorts:
  TOK_OBS_SORTS TOK_COLUMN get_list_of_obs_sorts TOK_SEMICOLUMN
  { buffered_output "Successfully parsed observable sorts" ;
    flush stdout }
| TOK_OBS_SORTS TOK_COLUMN
  { }
|
  { }

get_list_of_obs_sorts:
  TOK_IDENT
  { try
     let () = observational_proof := true in
     let k = (try (dico_sort_string#find_key $1) with Failure "find_key" -> failwith "get_list_of_obs_sorts") in
     match k with
	 Def_sort i ->
	   let ref_i = ref i in
	   selective_add_sort dico_obs_sort ref_i (*obs_sort_counter*) $1
       | Abstr_sort0 _| Abstr_sort1 _ | Abstr_sort2 _  -> failwith "get_list_of_obs_sorts"
     with Not_found ->
       selective_add_sort dico_sort_string sort_counter $1
     }
| get_list_of_obs_sorts TOK_IDENT
  {
    try
     let k = (try (dico_sort_string#find_key $2) with Failure "find_key" -> failwith "get_list_of_obs_sorts") in
     match k with
	 Def_sort i ->
	   let ref_i = ref i in
	   selective_add_sort dico_obs_sort ref_i (*obs_sort_counter*) $2
       |  Abstr_sort0 _| Abstr_sort1 _| Abstr_sort2 _  -> failwith "get_list_of_obs_sorts"

    with Not_found ->
     selective_add_sort dico_sort_string sort_counter $2

(*    let a = selective_add dico_sort_string sort_counter $2 in
    selective_add dico_obs_sort obs_sort_counter $2*)}


list_of_constructors:
  specif_constructor
  { }
| list_of_constructors specif_constructor
  { }

specif_constructor:
  TOK_IDENT TOK_COLUMN specif_profile
  {process_specif_symbol constructor_counter $1 $3  }

opt_specif_functions:
  TOK_FUNCTIONS TOK_COLUMN list_of_functions
  { buffered_output "\nSuccessfully parsed functions" ;
    flush stdout }
| TOK_FUNCTIONS TOK_FUNCTIONS TOK_COLUMN list_of_functions
  { buffered_output "\nSuccessfully parsed functions" ;
    flush stdout }
| TOK_FUNCTIONS TOK_COLUMN
  { }
| TOK_FUNCTIONS TOK_FUNCTIONS TOK_COLUMN
  { }
|
  { }

list_of_functions:
  specif_function
  { }
| list_of_functions specif_function
  { }

specif_function:
  TOK_IDENT TOK_COLUMN specif_profile
  { process_specif_symbol function_counter $1 $3 }

specif_profile:
  list_of_sorts TOK_ARROW list_of_sorts TOK_SEMICOLUMN
  { if List.length $3 > 1 then parse_failwith "The function should return only one value" else ($1, List.hd $3) }
| list_of_sorts TOK_SEMICOLUMN
  { ([], List.hd $1) }
| TOK_ARROW list_of_sorts TOK_SEMICOLUMN
  {  if List.length $2 > 1 then parse_failwith "The function should return only one value" else  ([], List.hd $2) }


list_of_sorts:
  ident_sort
  {$1}
| list_of_sorts ident_sort
      {$1 @ $2}

ident_sort:
  TOK_IDENT
  { let s =
      find_sort_id $1
  in [ s ] }
| TOK_LPAR TOK_IDENT ident_sort end_of_sorts
  {let arg = List.hd $3 in
   let s = find_sort_id $2 in
   if $4 = [] then [ Abstr_sort1 ((def_sort_id s), arg)]
   else
     let arg' = List.hd $4 in
     [ Abstr_sort2 ((def_sort_id s), arg, arg')]
 }


end_of_sorts:
  TOK_RPAR
  {[]}
| ident_sort TOK_RPAR
  {$1}


opt_specif_axioms:
  TOK_AXIOMS TOK_COLUMN pos_codes_true list_of_horn_clauses
  { buffered_output "\nSuccessfully parsed axioms" ; flush stdout ;
    yy_axioms := List.map introduce_var_exist $4 ;
    if !debug_mode then print_clause_list (List.map (fun (_, _, x) -> x) !yy_axioms) }
| TOK_AXIOMS TOK_COLUMN
  { }
|
  { }

/* Properties specification for defined symbols, e.g., AC or C */

opt_specif_function_props:
  TOK_FUNCTION_PROPS TOK_COLUMN  list_of_raw_symbols TOK_COLUMN TOK_AC TOK_SEMICOLUMN
  {process_function_props $3 Prop_ac }
|  TOK_FUNCTION_PROPS TOK_COLUMN  list_of_raw_symbols TOK_COLUMN TOK_ASSOC TOK_SEMICOLUMN
  {process_function_props $3 Prop_assoc }
| TOK_FUNCTION_PROPS TOK_COLUMN
  { }
|
  {}

list_of_raw_symbols:
  TOK_IDENT
  { [ $1 ] }
| list_of_raw_symbols TOK_IDENT
  { $1 @ [ $2 ] }


list_of_symbols:
  TOK_IDENT
  { [ find_symbol_id $1 ] }
| list_of_symbols TOK_IDENT
  { let s = find_symbol_id $2 in
    if not (List.mem s $1) then $1 @ [ find_symbol_id $2 ] else parse_failwith ($2 ^ " is duplicated") }

/* Ordering specifications */

opt_specif_greater:
  TOK_GREATER TOK_COLUMN init_order_dico list_of_greater
  {     (* print_dico_order () *)}
| TOK_GREATER TOK_COLUMN
  { }
| init_equiv_dico
  { let () = buffered_output "No order provided" in default_fill_order_dico () }

init_order_dico:
  {
    print_dico_const_string ();
    dico_order#init (!all_defined_functions @ !all_constructors) ;
    flush stdout }

list_of_greater:
  specif_greater
  { }
| list_of_greater specif_greater
  { }

specif_greater:
  TOK_IDENT TOK_COLUMN list_of_symbols TOK_SEMICOLUMN
  {  let v = find_symbol_id $1 in
     List.iter (fun x -> dico_order#add_couple v x) $3 }


opt_specif_equivs:
  TOK_EQUIV TOK_COLUMN init_equiv_dico list_of_equivs
  {
    if dico_order#empty
    then
      let () = buffered_output "Order dico is empty" in default_fill_order_dico ()
    else
      try
(* 	print_dico_equivalence (); *)
        dico_order#merge_equivalence_relation dico_equivalence ;
        buffered_output "\nSuccessfully parsed equivalence relation" ; flush stdout
      with (Failure "rehash") ->
        parse_failwith "t here are incompatibilities between the order and equivalence relations"
  }
| TOK_EQUIV TOK_COLUMN
  { if dico_order#empty
    then
      let () = buffered_output "Order dico is empty" in default_fill_order_dico ()
    else
      ()
  }
|
  { if dico_order#empty
    then
      let () = buffered_output "Order dico is empty" in default_fill_order_dico ()
    else
      ()
  }


init_equiv_dico:
  {

(*     List.iter (fun x -> (buffered_output ("init_order_dico : x = " ^ (string_of_int x)))) (!all_defined_functions @ !all_constructors); *)
    dico_equivalence#init dico_order#keys; (* (!all_defined_functions @ !all_constructors) ; *)
    flush stdout }

list_of_equivs:
  specif_equiv
  { }
| list_of_equivs specif_equiv
  { }

specif_equiv:
  list_of_symbols TOK_SEMICOLUMN
  { match $1 with
      [] -> failwith "I'm bewildered"
      | _::_ ->  dico_equivalence#fill (fun _ _ -> true) $1;  }

opt_specif_status:
  TOK_STATUS TOK_COLUMN list_of_statuses
  { buffered_output "\nSuccessfully parsed statuses" ; flush stdout ;
    print_dico_id_status () ;
    (try complete_status_dico ()
    with (Failure s) -> parse_failwith ("Symbol \"" ^ s ^ "\" is ac and must have multiset status") );
    try check_status_equivalent_symbols ()
    with (Failure "check_status_equivalent_symbols") -> parse_failwith "equivalent symbols must have the same status"
  }
| TOK_STATUS TOK_COLUMN
  { buffered_output "\nSuccessfully parsed statuses" ; flush stdout ;
    print_dico_id_status () ;
    (try complete_status_dico ()
    with (Failure s) -> parse_failwith ("Symbol \"" ^ s ^ "\" is ac and must have multiset status") );
    try check_status_equivalent_symbols ()
    with (Failure "check_status_equivalent_symbols") -> parse_failwith "equivalent symbols must have the same status"
  }
|
  { buffered_output "\nSuccessfully parsed statuses" ; flush stdout ;
    print_dico_id_status () ;
    (try complete_status_dico ()
    with (Failure s) -> parse_failwith ("Symbol \"" ^ s ^ "\" is ac and must have multiset status") );
    try check_status_equivalent_symbols ()
    with (Failure "check_status_equivalent_symbols") -> parse_failwith "equivalent symbols must have the same status"
  }

list_of_statuses:
  specif_status
  { }
| list_of_statuses specif_status
  { }

specif_status:
  list_of_symbols TOK_LEFTRIGHT TOK_SEMICOLUMN
  {
    try
      let () = List.iter (fun x -> if symbol_is_ac x then failwith (dico_const_string#find x) else ()) $1
      in List.iter (fun x -> add_to_status_dico x Left) $1
    with (Failure s) -> parse_failwith ("Symbol \"" ^ s ^ "\" is ac and must have multiset status")
      | Not_found -> failwith "raising Not_found in specif_status"}
| list_of_symbols TOK_RIGHTLEFT TOK_SEMICOLUMN
  { try
      let () = List.iter (fun x -> if symbol_is_ac x then failwith (dico_const_string#find x) else ()) $1
      in List.iter (fun x -> add_to_status_dico x Right) $1
    with (Failure s) -> parse_failwith ("Symbol \"" ^ s ^ "\" is ac and must have multiset status")
      | Not_found -> failwith "raising Not_found in specif_status"}
| list_of_symbols TOK_MULTISET TOK_SEMICOLUMN
      { List.iter (fun x -> add_to_status_dico x Multiset) $1 }

opt_specif_properties:
  TOK_PROPERTIES TOK_COLUMN list_of_properties
  { buffered_output "\nSuccessfully parsed properties" ; flush stdout }
| TOK_PROPERTIES TOK_COLUMN
  { }
|
  { }

list_of_properties:
  TOK_IDENT TOK_SEMICOLUMN
  { try
      let p = Hashtbl.find oracles_table $1
      in p := true
    with Not_found ->
      try
        let p = Hashtbl.find tests_table $1
        in p ()
      with Not_found ->
        parse_failwith ("property \"" ^ $1 ^ "\" is not defined") }
| list_of_properties TOK_IDENT TOK_SEMICOLUMN
  { try
      let p = Hashtbl.find oracles_table $2
      in p := true
    with Not_found ->
      try
        let p = Hashtbl.find tests_table $2
        in p ()
      with Not_found -> parse_failwith ("property \"" ^ $2 ^ "\" is not defined") }

opt_specif_priorities:
  TOK_PRIORITIES TOK_COLUMN list_of_priorities
  { buffered_output "\nSuccessfully parsed priorities" ; flush stdout ;
    !fill_default_induction_positions $3 ;
    buffered_output "Generate will be attempted on the following positions:" ;
    print_induction_symbols_priorities () }
| TOK_PRIORITIES TOK_COLUMN
  { !fill_default_induction_positions [] ;
    buffered_output "Generate will be attempted on the following positions:" ;
    print_induction_symbols_priorities () }
|
  { !fill_default_induction_positions [] ;
     buffered_output "Generate will be attempted on the following positions:" ;
     print_induction_symbols_priorities ()
  }

list_of_priorities:
  list_of_function_symbols TOK_SEMICOLUMN
  { [ $1 ] }
| list_of_priorities list_of_function_symbols TOK_SEMICOLUMN
  { $1 @ [ $2 ] }

list_of_function_symbols:
  specif_fun_with_positions
  { $1 }
| list_of_function_symbols specif_fun_with_positions
  { merge_induction_positions $1 $2 }

specif_fun_with_positions:
  TOK_IDENT
  {
    let n =
      try
	let n = dico_const_string#find_key $1
	in if is_defined n
	then n
	else parse_failwith ("symbol " ^ $1 ^ " is not a defined function")
      with Failure "find_key" -> parse_failwith ("symbol " ^ $1 ^ " is not a defined function")
    in
    try
      let l = (dico_ind_positions_v0#find n) in
      let all_ind_pos = Sort.list (<=) (List.map (fun p -> n, p) (list_remove_doubles (=) (List.flatten l))) in
      Ind_pos_position all_ind_pos
    with Not_found -> parse_failwith ("symbol \"" ^ $1 ^ "\" has no induction positions") }
| TOK_IDENT TOK_LPAR list_of_positions TOK_RPAR
  {
    let n =
      try
        let n = dico_const_string#find_key $1
        in if is_defined n
        then n
        else parse_failwith ("symbol " ^ $1 ^ " is not a defined function")
      with Failure "find_key" -> parse_failwith ("symbol " ^ $1 ^ " is not a defined function")
    in try
      let l = dico_ind_positions_v0#find n in
      let all_ind_pos = list_remove_doubles (=) (List.flatten l) in
      let _ = generic_setminus all_ind_pos $3
      in Ind_pos_position ((Sort.list (<=) (List.map (fun p -> n, p) $3)))
    with Not_found -> parse_failwith ("symbol \"" ^ $1 ^ "\" has no induction positions")
      | (Failure "setminus") -> parse_failwith ("provided induction positions of symbol \"" ^ $1 ^
        "\" are not a subset of actual positions") }
| specif_path
  {
    Ind_pos_void
  }

specif_path:
  TOK_LBRACKET list_of_sym_int_couples TOK_RBRACKET
  { $2 }

list_of_paths:
  specif_path
  { [$1] }
| list_of_paths specif_path
  { $1 @ [$2] }
list_of_sym_int_couples:
  sym_int_couple
  { [ $1 ] }
| list_of_sym_int_couples TOK_SEMICOLUMN sym_int_couple
  { $1 @ [ $3 ] }

sym_int_couple:
  TOK_IDENT TOK_AROBAT TOK_IDENT
  { let f = find_symbol_id $1
    and i = parse_positive_int $3
    in f, i }



opt_specif_test_sets:
  TOK_TEST_SETS TOK_COLUMN list_of_test_sets
  {
(*     buffered_output "\nSuccessfully parsed test sets" ;  *)
(*     flush stdout; *)
(*     !print_dico_test_set () *)
  }
| TOK_TEST_SETS TOK_COLUMN
  { }
|
  {}

list_of_test_sets:
  specif_test_set
  { }
| list_of_test_sets specif_test_set
  { }

specif_test_set:
  TOK_IDENT TOK_COLUMN list_of_terms TOK_SEMICOLUMN
  {
  }
| list_of_paths TOK_COLUMN list_of_terms TOK_SEMICOLUMN
  { }

opt_specif_nullary_sorts:
  TOK_NULLARY_SORTS TOK_COLUMN list_of_nullary_sorts TOK_SEMICOLUMN
 {
   let fn string =
     let s = find_sort_id string in
     try
       let _ = dico_sort_nullarity#find s in
       buffered_output ("The sort \"" ^ string ^ "\" is already in the dictionary of nullary sorts"); flush stdout
     with Not_found -> dico_sort_nullarity#add s true
   in List.iter fn $3;

    buffered_output "\nSuccessfully parsed nullary sorts" ;
    flush stdout;
     buffered_output "WARNING: The user introduced the following nullary sorts !" ; flush stdout;
    print_dico_sort_nullarity ()
  }
| TOK_NULLARY_SORTS TOK_COLUMN
  { }
|
  { }

list_of_nullary_sorts:
 specif_nullary_sort
    {[$1] }
| list_of_nullary_sorts specif_nullary_sort
    { $1 @ [$2]}

specif_nullary_sort:
 TOK_IDENT
{ $1
}

/* ********************************************************************************** */
list_of_contexts:
  reset_tmp_vars context
  { [ $2 ] }
| list_of_contexts reset_tmp_vars context
  { $1 @ [ $3 ] }

context:
  TOK_LBRACKET get_term TOK_COMA TOK_IDENT TOK_RBRACKET
  { let t = $2
    and x =
      try List.assoc $4 !yy_tmp_vars
      with Not_found -> parse_failwith "The contextual variable is not in the context"
    in let var_sort = try List.assoc x (List.map (fun (x,y,_) -> (x, y)) t#variables) with Not_found -> failwith "raising Not_found in context"
    in let c = new context t#content  x
    in let () = Critical_context_set.critical_context_set_by_var#add var_sort c
    in c }

context_specif:
   TOK_IDENT TOK_COLUMN list_of_contexts TOK_SEMICOLUMN
   { let declared_sort = find_sort_id $1
     and contexts = $3
     in if not (List.for_all (fun c -> c#sort = declared_sort)contexts)
            then
               parse_failwith "A context is not of the declared sort"
            else
               (*Critical_context_set.critical_context_set#add declared_sort contexts*)
               critical_context_set#replace declared_sort (( let old = ref [] in let () = try old := critical_context_set#find declared_sort with _ -> () in !old ) @ contexts)

   }

list_of_context_specif:
  context_specif
  { [ $1 ] }
| list_of_context_specif context_specif
  { $1 @ [ $2 ] }

opt_specif_critical_context_sets:
  TOK_CRITICAL_CONTEXT_SETS TOK_COLUMN list_of_context_specif
  { buffered_output "Successfully parsed critical context sets" ;print_critical_context_set ();flush stdout }
| TOK_CRITICAL_CONTEXT_SETS TOK_COLUMN
  { }
|
  { }

/* Problem specification */

specif_ind_priorities:
  TOK_IND_PRIORITIES TOK_COLUMN list_of_infs
  { }
| TOK_IND_PRIORITIES TOK_COLUMN
  { }

list_of_infs:
  specif_infs
  { }
| list_of_infs specif_infs
  { }

specif_infs:
  list_of_symbols TOK_SEMICOLUMN
  {
(*     let l = list_2_list_of_couples $1 *)
(*     in List.iter (fun (x, y) -> dico_infs#add_couple y x) l  *)
    let () = dico_infs#clear in
    let () = dico_infs_flag := true in
    let rec fn l =
      match l with
	  [] -> ()
	| h :: t -> let () = List.iter (fun x -> dico_infs#add h x) t in
	  fn t
    in
    let lst = if $1 <> [] then let () = list_ind_priorities := $1 in $1 @ [last_el $1] else [] in
    fn (List.rev lst)
  }

specif_complete_terms :
  TOK_COMPLETE_TERMS TOK_COLUMN list_of_terms TOK_SEMICOLUMN
  {Queue.add (Cterm_token $3) yy_queue }
| TOK_COMPLETE_TERMS TOK_COLUMN
  { }




specif_lemmas:
  TOK_LEMMAS TOK_COLUMN pos_codes_false list_of_clauses
  { buffered_output "\nSuccessfully parsed lemmas" ; flush stdout ;
    print_clause_list $4 ;
    Queue.add (Lemmas_token $4) yy_queue }
| TOK_LEMMAS TOK_COLUMN
  { }

specif_conjectures:
  TOK_CONJECTURES TOK_COLUMN pos_codes_false list_of_clauses_history
  { buffered_output "\nSuccessfully parsed conjectures" ;
    print_clause_list $4 ;
    let lc =
      if !specif_LA_defined && not !specif_Rmaxs0_defined && not !specif_Rmins0_defined && not !specif_Rzmm_defined then  let res = List.fold_right (fun c l -> (del_minmax c) @ l) $4 [] in res
      else $4
    in
    Queue.add (Conjectures_token lc) yy_queue
  }
| TOK_CONJECTURES TOK_COLUMN
  { }

specif_hypotheses:
  TOK_HYPOTHESES TOK_COLUMN pos_codes_false list_of_clauses
  { buffered_output "\nSuccessfully parsed hypotheses" ;
    print_clause_list $4 ;
    Queue.add (Hypotheses_token $4) yy_queue }
| TOK_HYPOTHESES TOK_COLUMN
  { }

list_of_clauses:
  reset_and_clause
  { [$1] }
| list_of_clauses reset_and_clause
  { $1 @ [$2] }

list_of_clauses_history:
  reset_and_clause
  { [$1] }
|  reset_and_clause history_clause
  { let () = $1#set_history $2 in [$1]}
| list_of_clauses_history reset_and_clause
  { $1 @ [$2] }

history_clause:
 one_history
{[$1]}
|  history_clause one_history
{$1 @ [$2]}

one_history:
TOK_OPEN_SUBSTITUTION specif_substitution2 TOK_CLOSE_SUBSTITUTION  garbage_history specif_clause TOK_SEMICOLUMN
{($2, $5)}

garbage_history:
TOK_ON  TOK_LBRACKET TOK_IDENT TOK_RBRACKET
{}

reset_and_clause:
  reset_tmp_vars specif_clause TOK_SEMICOLUMN
  { $2 }

list_of_horn_clauses:
  reset_and_horn_clause
  { [$1] }
| list_of_horn_clauses reset_and_horn_clause
  { $1 @ [$2] }

reset_and_horn_clause:
  reset_tmp_vars specif_horn_clause TOK_SEMICOLUMN
  { (!parsed_gfile, !linenumber, $2) }

reset_tmp_vars:
  { yy_tmp_vars := [] ;
    yy_tmp_sorts := [] ;
(*     if !debug_mode then print_string "\nReset of yy_tmp_param_sorts"; *)
    yy_tmp_param_sorts := [] ;
    yy_tmp_equiv_dico#clear }

specif_clause2: specif_clause TOK_EOF
  { $1 }

specif_clause:
  list_of_literals
  {
    let l' = List.map literal_of_incomplete_terms $1 in
    let new_l' = expand_sorts_list l' in
    let () = if not !specif_parameterized  && not (test_well_founded_cl ([], new_l')) then
      failwith "clause1: undefined types"
    in
    let res = new clause ([], new_l') [] ("",0,([],[])) in
(*     let () = print_detailed_clause res in *)
    res
  }
| list_of_literals TOK_IMPLIES
  {
    let l = List.map literal_of_incomplete_terms $1 in
    let new_l = expand_sorts_list l in
    let () = if not !specif_parameterized && not (test_well_founded_cl (new_l, [])) then
      failwith "clause2: undefined types"
    in
    let res = new clause (new_l, []) [] ("",0,([],[])) in
(*     let () = print_detailed_clause res in *)
    res
  }
| list_of_literals TOK_IMPLIES list_of_literals
  {
    let l = List.map literal_of_incomplete_terms $1 in
    let l' = List.map literal_of_incomplete_terms $3 in
    let new_l' = expand_sorts_list l' in
    let new_l = expand_sorts_list l in
    let () = if not !specif_parameterized && not (test_well_founded_cl (new_l, new_l')) then
      failwith "clause3: undefined types"
    in
    let res = new clause (new_l, new_l') [] ("",0,([],[])) in
(*     let () = print_detailed_clause res in *)
    res
  }

specif_horn_clause:
  specif_literal
  {
    let l' = [ literal_of_incomplete_terms $1 ] in
    let new_l' = expand_sorts_list l' in
    let lhs, _ = (List.hd new_l')#both_sides in
    let arg_lhs = lhs#sons in
    let () = if List.exists (fun t -> not (t#is_constructor_term)) arg_lhs then failwith ("one of the arguments is not a constructor term" ) in
    let () = if not !specif_parameterized && not (test_well_founded_cl ([], new_l')) then
      failwith "clause4: undefined types"
    in
    let res = new clause ([], new_l') [] ("",0,([],[])) in
(*     let () = print_detailed_clause res in *)
    res
  }
| list_of_literals TOK_IMPLIES specif_literal
  {
    let l = List.map literal_of_incomplete_terms $1
    and l' = [ literal_of_incomplete_terms $3 ] in
    let new_l' = expand_sorts_list l' in
    let new_l = expand_sorts_list l in
    let lhs, _ = (List.hd new_l')#both_sides in
    let arg_lhs = lhs#sons in
    let () = if List.exists (fun t -> not (t#is_constructor_term)) arg_lhs then failwith ("one of the arguments is not a constructor term" ) in
    let () = if not !specif_parameterized && not (test_well_founded_cl (new_l, new_l')) then
      failwith "clause5: undefined types"
    in
    let res = new clause (new_l, new_l') [] ("",0,([],[])) in
(*     let () = print_detailed_clause res in *)
    res
  }

list_of_literals:
  specif_literal
  { [ $1 ] }
| list_of_literals TOK_COMA specif_literal
  { $1 @ [ $3 ] }

specif_literal:
  literal_get_sides
  {
  let lhs, rhs, type_lit = $1 in
    let t = process_term_tokens lhs
    and t' = process_term_tokens rhs in
    let content = try (defined_content t) with
	(Failure "defined_content") -> failwith "defined_content"
    in
    let term_t, term_t' =
      match content with
	  Ivar x -> (* t is a variable *)
	    begin (* dicards PM on exceptions *)
              try
		let s = List.assoc x !yy_tmp_sorts
		in
		if x < 0 then ((new term (Var_exist (x, s))), typecheck_incomplete_tree (Actual_sort s) t')
		else ((new term (Var_univ (x, s))), typecheck_incomplete_tree (Actual_sort s) t')
              with Not_found -> (* t has a fresh unknown sort *)
		let () = param_sort_counter := !param_sort_counter + 1 in
		let str = ("Undefined" ^ (string_of_int !param_sort_counter)) in
		let s' = Abstr_sort0 str in
		let new_t' = typecheck_incomplete_tree (Actual_sort s') t' in
		let () = yy_tmp_sorts := generic_insert_sorted (x, new_t'#sort) !yy_tmp_sorts in
		if x < 0 then ((new term (Var_exist (x, new_t'#sort))), new_t')
		else ((new term (Var_univ (x, new_t'#sort))), new_t')
            end
	| Iterm (x, _) -> (* t is a term *)
            let s = try dico_const_sort#find x with Not_found -> failwith "raising Not_found in specif_literal" in
	    let s' = List.hd (update_profile [s]) in (* the abstract sorts are renamed *)
            ((typecheck_incomplete_tree (Actual_sort s') t), typecheck_incomplete_tree (Actual_sort s')  t')
    in
    term_t, term_t', type_lit }

literal_get_sides:
  specif_term TOK_EQUAL specif_term
  { ($1,$3, Lit_equal (term_true, term_true)) }
| specif_term TOK_ARROW specif_term
  { ($1,$3, Lit_rule (term_true, term_true))}
| specif_term TOK_DIFF specif_term
  { ($1,$3, Lit_diff (term_true, term_true)) }


specif_term:
  list_of_tokens
  {$1 }

list_of_tokens:
  one_token
  { $1 }
| list_of_tokens one_token
  { $1 @ $2 }

one_token:
  TOK_IDENT
  { [ TT_ident $1 ] }
/* | TOK_IDENT_LIST */
/*   { List.map (fun x -> TT_ident x) $1 } */
| TOK_LPAR TOK_IDENT TOK_COLUMN TOK_IDENT TOK_RPAR
  { let () = check_explicit_type $2 $4 in
    [ TT_ident $2 ] }
| TOK_LPAR list_of_term_tokens TOK_RPAR
  { let () = if !debug_mode then (print_string"\n token list <<<<< " ; List.iter (fun x -> print_term_token x) $2) else ()
  in $2}

list_of_term_tokens:
  list_of_tokens
  { let content = (try defined_content (process_term_tokens $1) with
      (Failure "defined_content") -> failwith "defined_content")
  in [ TT_tree content ] }
| list_of_term_tokens TOK_COMA list_of_tokens
  { let content = (try (defined_content (process_term_tokens $3)) with
      (Failure "defined_content") -> failwith "defined_content")
  in $1 @ [ TT_tree content ] }

specif_strategies:
  TOK_STRATEGIES TOK_COLUMN list_of_strategies
  { buffered_output "\nSuccessfully parsed strategies" ;
    Queue.add (Strat_token $3) yy_queue }
| TOK_STRATEGIES TOK_COLUMN
  { }

list_of_strategies:
  specif_strategy
  { [$1] }
| list_of_strategies specif_strategy
  { $1 @ [$2] }

specif_strategy:
  TOK_IDENT TOK_EQUAL strategy_term TOK_SEMICOLUMN
  { ($1, $3) }

strategy_term:
| TOK_ADDPREMISE TOK_LPAR reasoning_module TOK_COMA TOK_LBRACKET list_of_reasoning_modules TOK_RBRACKET TOK_RPAR
    {new strategy (Inference_rule (AddPremise ($3, new strategy (Try_ $6))))}
| TOK_SIMPLIFY TOK_LPAR reasoning_module TOK_COMA TOK_LBRACKET list_of_reasoning_modules TOK_RBRACKET TOK_RPAR
    {new strategy (Inference_rule (Simplify ($3, new strategy (Try_ $6))))}
| TOK_DELETE TOK_LPAR reasoning_module TOK_COMA TOK_LBRACKET list_of_reasoning_modules TOK_RBRACKET TOK_RPAR
    {new strategy (Inference_rule (Delete ($3, new strategy (Try_ $6))))}
| TOK_GOTO TOK_IDENT
  { match $2 with
      "smallest" -> new strategy (Inference_rule (Goto Goto_smallest))
    | "greatest" -> new strategy (Inference_rule (Goto Goto_greatest))
    | _ ->
        let i = parse_positive_int $2
        in new strategy (Inference_rule (Goto (Goto_number i))) }
| TOK_LPAR list_of_strategy_terms TOK_RPAR
  { new strategy (Series $2) }
| TOK_TRY TOK_LPAR list_of_strategy_terms TOK_RPAR
  { new strategy (Try_ $3) }
| TOK_SATURATE TOK_LPAR list_of_strategy_terms TOK_RPAR
  { new strategy (Saturate $3) }
| TOK_REPEAT strategy_term
  { new strategy (Repeat $2) }
| TOK_REPEAT_PLUS strategy_term
  { new strategy (Repeat_plus $2) }
| TOK_IDENT
  { new strategy (Named_strategy $1) }
| TOK_QUESTION_MARK
  { new strategy Query }
| TOK_PRINT_GOALS
  { new strategy (Print_goals (false, false)) }
| TOK_PRINT_GOALS_HISTORY
  { new strategy (Print_goals (false, true)) }
| TOK_PRINT_GOALS TOK_LPAR TOK_IDENT TOK_RPAR
  { match String.lowercase $3 with
      "t" | "true" -> new strategy (Print_goals (true, false))
    | "f" | "false" -> new strategy (Print_goals (false, false))
    | _ -> parse_failwith "Bad argument for strategy \"print_goals\"" }

| TOK_PRINT_GOALS_HISTORY TOK_LPAR TOK_IDENT TOK_RPAR
  { match String.lowercase $3 with
      "t" | "true" -> new strategy (Print_goals (true, true))
    | "f" | "false" -> new strategy (Print_goals (false, true))
    | _ -> parse_failwith "Bad argument for strategy \"print_goals_history\"" }

list_of_strategy_terms:
  strategy_term
  { [ $1 ] }
| list_of_strategy_terms TOK_COMA strategy_term
  { $1 @ [ $3 ] }

reasoning_module:
  TOK_CONTEXTUAL_REWRITING TOK_LPAR strategy_term TOK_COMA specif_list_of_systems TOK_COMA specif_clausal_position TOK_RPAR
  { Contextual_rewriting ($3, $5, $7) }
| TOK_EQUATIONAL_REWRITING TOK_LPAR specif_literal_position_in_clause TOK_RPAR
  { (Equational_rewriting $3) }
| TOK_REWRITING TOK_LPAR TOK_IDENT TOK_COMA specif_list_of_systems TOK_COMA specif_literal_position_in_clause TOK_RPAR
  { match $3 with
      "rewrite" -> (Rewriting (false, $5, $7))
    | "normalize" -> (Rewriting (true, $5, $7))
    | _ -> parse_failwith "argument of rewriting must be either \"rewrite\" or \"normalize\"" }
| TOK_PARTIAL_CASE_REWRITING TOK_LPAR specif_list_of_systems TOK_COMA specif_literal_position_in_clause TOK_RPAR
  { Partial_case_rewriting ($3, $5) }
| TOK_TOTAL_CASE_REWRITING TOK_LPAR strategy_term TOK_COMA specif_list_of_systems TOK_COMA specif_literal_position_in_clause TOK_RPAR
  { Total_case_rewriting ($3, $5, $7) }
| TOK_GENERATE
  { Generate (true, []) }
| TOK_GENERATE TOK_LPAR TOK_QUESTION_MARK TOK_RPAR
  {Generate (false, []) }
| TOK_GENERATE TOK_LPAR list_of_priorities TOK_RPAR
  { Generate (true, $3) }
| TOK_GENERATE_EQ
  { Generate_eq (true, []) }
| TOK_GENERATE_EQ TOK_LPAR TOK_QUESTION_MARK TOK_RPAR
  {Generate_eq (false, []) }
| TOK_GENERATE_EQ TOK_LPAR list_of_priorities TOK_RPAR
  { Generate_eq (true, $3) }
| TOK_GENERATE_OBS
  { ((Generate_obs (true, []))) }
| TOK_GENERATE_OBS TOK_LPAR TOK_QUESTION_MARK TOK_RPAR
  { ((Generate_obs (false, []))) }
| TOK_GENERATE_OBS TOK_LPAR list_of_priorities TOK_RPAR
  { ( (Generate_obs (true, $3))) }
| TOK_POSITIVE_DECOMPOSITION
  { Positive_decomposition }
| TOK_CONGRUENCE_CLOSURE
  { Congruence_closure }
| TOK_NEGATIVE_DECOMPOSITION
  { Negative_decomposition }
| TOK_POSITIVE_CLASH
  { Positive_clash }
| TOK_TAUTOLOGY
  { Tautology }
| TOK_SUBSUMPTION TOK_LPAR specif_list_of_systems TOK_RPAR
  { Subsumption ($3)}
| TOK_AUGMENTATION TOK_LPAR specif_list_of_systems TOK_RPAR
  { Augmentation ($3)}
| TOK_NEGATIVE_CLASH
  { Negative_clash }
| TOK_ELIMINATE_REDUNDANT_LITERAL
  { Eliminate_redundant_literal }
| TOK_ELIMINATE_TRIVIAL_LITERAL
  { Eliminate_trivial_literal }
| TOK_AUTO_SIMPLIFICATION
  { Auto_simplification }
| TOK_COMPLEMENT
  { Complement }
| TOK_ID
  { Id }


list_of_reasoning_modules:
  reasoning_module
  { [ new strategy (Inference_rule (Id_st $1)) ] }
| list_of_reasoning_modules TOK_COMA reasoning_module
  { $1 @ [ new strategy (Inference_rule (Id_st $3)) ] }



specif_list_of_systems:
  TOK_QUESTION_MARK
  { LOS_query }
| list_of_systems
  { LOS_defined $1 }

list_of_systems:
  specif_system
  { [ $1 ] }
| list_of_systems TOK_OR specif_system
  { $1 @ [ $3 ] }

specif_system:
  TOK_IDENT
  { match $1 with
    | "r"| "R" -> R
    | "c"| "C" -> C
    | "l"| "L" -> L
    | _ -> parse_failwith "bad systems specification" }

/*
specif_list_of_rw_systems:
  TOK_QUESTION_MARK
  { LOS_query }
| list_of_rw_systems
  { LOS_defined $1 }

list_of_rw_systems:
  specif_rw_system
  { [ $1 ] }
| list_of_rw_systems TOK_AND specif_rw_system
  { $1 @ [ $3 ] }

specif_rw_system:
  TOK_IDENT
  { match $1 with
      "r"| "R" -> R
    | "l"| "L" -> L
    | _ -> parse_failwith "bad rw systems specification" }
*/
specif_startpoint:
  TOK_START_WITH TOK_COLUMN strategy_term
  { buffered_output "\nSuccessfully parsed startpoint" ;
    Queue.add (Startpoint_token $3) yy_queue }
| TOK_START_WITH TOK_COLUMN
  { }

specif_augmentation:
  TOK_AUGMENTATION_STRATEGY TOK_COLUMN strategy_term
  { buffered_output "\nSuccessfully parsed the augmentation strategy" ;
    Queue.add (Augmentation_token $3) yy_queue }
| TOK_AUGMENTATION_STRATEGY TOK_COLUMN
  { }

specif_norm:
  TOK_NORM TOK_COLUMN list_of_terms TOK_SEMICOLUMN
  { Queue.add (Norm_token $3) yy_queue }
| TOK_NORM TOK_COLUMN
  { }

specif_rpocompare:
  TOK_RPOCOMPARE TOK_COLUMN two_terms
  {
     let t, t' = $3 in
    Queue.add (Rpo_token (t, t')) yy_queue }
| TOK_RPOCOMPARE TOK_COLUMN
  { }

specif_compare:
  TOK_COMPARE TOK_COLUMN two_clauses
  { let c, c' = $3 in
    Queue.add (Compare_token (c, c')) yy_queue }
| TOK_COMPARE TOK_COLUMN
  { }

specif_max_compare:
  TOK_MAX_COMPARE TOK_COLUMN two_clauses
  { let c, c' = $3 in
    Queue.add (Compare_max_token (c, c')) yy_queue }
| TOK_MAX_COMPARE TOK_COLUMN
  { }


specif_stop_on_clause:
  TOK_STOP_ON_CLAUSE TOK_COLUMN TOK_IDENT TOK_SEMICOLUMN
  {
    stop_clause := int_of_string $3}
| TOK_STOP_ON_CLAUSE TOK_COLUMN
  {}

specif_extract:
  TOK_EXTRACT TOK_COLUMN list_of_symbols
  { extract_specification $3}
| TOK_EXTRACT TOK_COLUMN
  { }

specif_match:
  TOK_MATCH TOK_COLUMN two_terms TOK_SEMICOLUMN
  { let t, t' = $3 in
    Queue.add (Match_token (t, t')) yy_queue }
| TOK_MATCH TOK_COLUMN
  { }

specif_ac_subsumes:
  TOK_AC_SUBSUMES reset_tmp_vars TOK_COLUMN set_of_terms reset_tmp_vars TOK_SEMICOLUMN set_of_terms TOK_SEMICOLUMN
  { Queue.add (Ac_token ($4, $7)) yy_queue }
| TOK_AC_SUBSUMES reset_tmp_vars TOK_COLUMN
  { }

set_of_terms:
  get_term
  { [ $1 ] }
| set_of_terms TOK_COMA get_term
  { $1 @ [ $3 ] }

two_terms:
  reset_tmp_vars get_term TOK_QUESTION_MARK get_term
  { ($2, $4) }

two_clauses:
  pos_codes_false reset_tmp_vars specif_clause TOK_QUESTION_MARK specif_clause
  { ($3, $5) }

specif_literal_position_in_clause:
  TOK_IDENT
  { match $1 with
      "*" -> Pos_all
    | _ -> Pos_litdefined (true, parse_positive_int $1) }
| TOK_IDENT bracket_enclosed_list_of_positive_ints
  { Pos_defined (true, parse_positive_int $1, $2) }
| TOK_QUESTION_MARK
  { Pos_query }

specif_clausal_position:
  TOK_IDENT TOK_IDENT bracket_enclosed_list_of_positive_ints
  { let i = parse_positive_int $1
    in let b =
      match i with
        0 -> false
      | 1 -> true
      | _ -> parse_failwith "clausal position must start with 0 or 1"
    in Pos_defined (b, parse_positive_int $2, $3) }
| TOK_IDENT
  { match $1 with
      "*" -> Pos_all
    | _ -> parse_failwith "clausal position is either \"*\" or a real position" }
| TOK_QUESTION_MARK
  { Pos_query }

bracket_enclosed_list_of_positive_ints:
  TOK_LBRACKET list_of_positive_ints TOK_RBRACKET
  { $2 }
| TOK_LBRACKET TOK_RBRACKET
  { [] }

list_of_positions:
  bracket_enclosed_list_of_positive_ints
  { [List.map (fun x -> (0,x)) $1 ] } /* to be modified */
| list_of_positions bracket_enclosed_list_of_positive_ints
  { $1 @ [List.map (fun x -> (0,x))  $2 ] }

list_of_positive_ints:
  TOK_IDENT
  { [(parse_positive_int $1) - 1] }
| list_of_positive_ints TOK_IDENT
  { $1 @ [(parse_positive_int $2) - 1] }

specif_substitution: specif_substitution2 TOK_EOF
  { $1 }

specif_substitution2:
  specif_var_term
  { [$1] }
| specif_substitution2 TOK_SEMICOLUMN specif_var_term
  { insert_sorted (fun (x, _) (x', _) -> x = x') (fun (x, _) (x', _) -> x < x') $3 $1 }

specif_var_term:
  TOK_IDENT TOK_COMA specif_term
  {
    let v =
      try List.assoc $1 !yy_tmp_vars
      with Not_found -> let tmp_v = newvar () in let () = yy_tmp_vars := ($1, tmp_v) :: !yy_tmp_vars in tmp_v
      in

(*     let s = try List.assoc v !yy_tmp_sorts2 with Not_found -> failwith "raising Not_found in specif_var_term" in *)
    let t = process_term_tokens $3 in
    let term = typecheck_incomplete_tree (Variable_sort 0) t in
    (v,  term)
  }

specif_positive_int:
  TOK_IDENT
  { parse_positive_int $1 }


get_term:
  specif_term
  { let t = process_term_tokens $1 in
    let term_t =
      match t with
          Defined (Iterm (f, _)) ->
	    begin
	      let s =
		try
		  dico_const_sort#find f
		with Not_found -> parse_failwith "get_term"
	      in
	      typecheck_incomplete_tree (Actual_sort s) t
	    end
	| Defined (Ivar x) ->
            begin
              let s =
		try
		  List.assoc x !yy_tmp_sorts
		with Not_found -> parse_failwith "unbound types"
	      in
	      typecheck_incomplete_tree (Actual_sort s) t
            end
	| Undefined -> parse_failwith "unbound types"
    in term_t }

list_of_terms:
  reset_tmp_vars get_term
  { [$2] }
| list_of_terms TOK_COMA reset_tmp_vars get_term
  { $1 @ [$4] }

pos_codes_true:
  { pick_pos_codes := true }

pos_codes_false:
  { pick_pos_codes := false }

/* Command line parsing */

specif_shell_command:
  TOK_STRATEGIES strategy_term
  { Command_strategy $2 }
| TOK_IDENT
  { match $1 with
      "p" -> Command_previous
    | "n" -> Command_next
    | "r" -> Command_run
    | "d" -> Command_display
    | "exit" -> Command_exit
    | _ -> Command_error }

print_caml:
  TOK_PRINT_CAML  TOK_COLUMN pos_codes_false list_of_clauses
  {
    buffered_output (sprint_list "\n\nThe CAML version :" compute_string_clause_caml $4);
  }

| TOK_PRINT_CAML TOK_COLUMN
  { }


