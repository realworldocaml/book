(* Parsing of S-expression. The parsing is written as an automaton for which
   we provide different implementations of actions.
*)

open! Base
open! Import

(* Sharing of transitions *)
module Sharing = struct
  let create_assign_id () =
    let cache = Hashtbl.Poly.create () in
    ( cache
    , fun x ->
      if not (Hashtbl.mem cache x)
      then Hashtbl.add_exn cache ~key:x ~data:(Hashtbl.length cache) )
  ;;

  let share (table : Automaton.Table.t) =
    let transitions, assign_transition_id = create_assign_id () in
    let transitions_eoi, assign_transition_eoi_id = create_assign_id () in
    Array.iter table.transitions ~f:assign_transition_id;
    Array.iter table.transitions_eoi ~f:assign_transition_eoi_id;
    transitions, transitions_eoi
  ;;
end

let pr fmt = Printf.ksprintf Stdio.print_endline fmt

let ordered_ids tbl =
  Hashtbl.fold tbl ~init:[] ~f:(fun ~key:x ~data:id acc -> (id, x) :: acc)
  |> List.sort ~compare:(fun (id1, _) (id2, _) -> compare id1 id2)
;;

let print_named_transition (id, tr) =
  match (tr : Automaton.Table.transition Automaton.Table.or_error) with
  | Error error ->
    pr "let tr_%02d _state _char _stack =" id;
    pr "  raise _state ~at_eof:false %s" (Automaton.Error.to_string error)
  | Ok { action = eps_actions, action; goto; advance } ->
    let eps_actions =
      List.filter_map ~f:Automaton.Epsilon_action.to_runtime_function eps_actions
    in
    let action = Automaton.Action.to_runtime_function action in
    pr
      "let tr_%02d state %schar stack ="
      id
      (if Option.is_none action
       && not ([%compare.equal: Automaton.Table.goto_state] goto End_block_comment)
       then "_"
       else "");
    List.iter eps_actions ~f:(pr "  let stack = %s state stack in");
    (match action with
     | None -> ()
     | Some s -> pr "  let stack = %s state char stack in" s);
    (match goto with
     | State n -> pr "  set_automaton_state state %d;" n
     | End_block_comment ->
       pr "  let stack = end_block_comment state char stack in";
       pr
         "  set_automaton_state state (if block_comment_depth state <> 0 then %d else %d);"
         (Automaton.State.to_int (Block_comment Normal))
         (Automaton.State.to_int Whitespace));
    pr
      "  %s state;"
      (match advance with
       | Advance -> "advance"
       | Advance_eol -> "advance_eol");
    pr "  stack"
;;

let print_named_transition_eoi (id, tr) =
  match (tr : Automaton.Epsilon_action.t list Automaton.Table.or_error) with
  | Error error ->
    pr "let tr_eoi_%02d state _stack =" id;
    pr "  raise state ~at_eof:true %s" (Automaton.Error.to_string error)
  | Ok eps_actions ->
    pr "let tr_eoi_%02d state stack =" id;
    let eps_actions =
      List.filter_map eps_actions ~f:Automaton.Epsilon_action.to_runtime_function
    in
    List.iter eps_actions ~f:(pr "  let stack = %s state stack in");
    pr "  eps_eoi_check state stack"
;;

let print_table suffix tbl ids =
  Array.map tbl ~f:(fun tr ->
    Printf.sprintf "tr%s_%02d" suffix (Hashtbl.find_exn ids tr))
  |> Array.to_list
  |> String.concat ~sep:";"
  |> Stdio.printf "let transitions%s = [| %s |]" suffix
;;

let print_old_parser_approx_cont_states () =
  List.map Automaton.State.all ~f:Automaton.State.old_parser_approx_cont_state
  |> String.concat ~sep:";"
  |> Stdio.printf
       "let old_parser_approx_cont_states : Old_parser_cont_state.t array = [| %s |]"
;;

let print_code () =
  let named_transitions, named_transitions_eoi = Sharing.share Automaton.table in
  List.iter (ordered_ids named_transitions) ~f:print_named_transition;
  List.iter (ordered_ids named_transitions_eoi) ~f:print_named_transition_eoi;
  print_table "" Automaton.table.transitions named_transitions;
  print_table "_eoi" Automaton.table.transitions_eoi named_transitions_eoi;
  print_old_parser_approx_cont_states ()
;;
