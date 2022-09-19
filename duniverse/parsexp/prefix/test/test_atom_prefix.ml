open! Core
open! Import

(** High-level testing strategy:

    We generate parser inputs that parse to single sexp atoms.

    For each parser input, we check every prefix.

    For every prefix s.t. [Atom_prefix.create] returns [Some atom_prefix], we check that

    1. The parser input parses to the same atom if we replace the prefix with
    [Atom_prefix.get_signifier atom_prefix]

    2. [Atom_prefix.get_signifier] does not include any comments.

    Across all the prefix, we check that [Atom_prefix.get_signified] has returned every
    prefix of the atom.

    Across all parser inputs, we check that we have covered every [state.automaton_state].
*)

let get_signified signifier =
  match Parsexp.Single.parse_string_exn signifier with
  | List _ -> assert false
  | Atom signified -> signified
;;

let eager_cst_consume_string s on_parsed_value =
  let state = Parsexp.Eager_cst.State.create on_parsed_value in
  let stack = Parsexp.Eager_cst.Stack.empty in
  let stack = Parsexp.Eager_cst.feed_string state s stack in
  ignore (stack : Parsexp.Eager_cst.Stack.t)
;;

let parse_a_comment_eagerly signifier =
  Option.try_with_join (fun () ->
    with_return_option (fun { return } ->
      eager_cst_consume_string signifier (fun _ -> function
        | Sexp _ -> ()
        | Comment comment -> return comment)))
;;

let assert_signifier_has_no_comments of_atom ~len ~parser_input =
  let prefix_signifier = Atom_prefix.get_signifier of_atom ~parser_input in
  Option.iter (parse_a_comment_eagerly prefix_signifier) ~f:(fun comment ->
    raise_s
      [%message
        "Signifier included a comment."
          (comment : Parsexp.Cst.comment)
          (prefix_signifier : string)
          (len : int)
          (parser_input : string)])
;;

let round_trip_prefix of_atom ~len ~parser_input ~verbose =
  let prefix_signifier = Atom_prefix.get_signifier of_atom ~parser_input in
  let suffix = String.drop_prefix parser_input len in
  if verbose then print_s [%message (prefix_signifier : string) (of_atom : Atom_prefix.t)];
  prefix_signifier ^ suffix
;;

let show_state state len =
  let of_atom = Atom_prefix.create_opt state in
  let positions = Positions.Builder.contents state.user_state |> Positions.to_list in
  let state = Parsexp_symbolic_automaton.Automaton.State.of_int state.automaton_state in
  [%sexp
    { len : int
    ; of_atom : Atom_prefix.t option
    ; state : Parsexp_symbolic_automaton.Automaton.State.t
    ; positions : Positions.pos list
    }]
;;

let assert_prefix_can_round_trip of_atom ~len ~parser_input ~verbose =
  let observed = get_signified (round_trip_prefix of_atom ~len ~parser_input ~verbose) in
  let expected = get_signified parser_input in
  require_equal [%here] (module String) observed expected
;;

let test_every_prefix (saw_state : Coverage.Saw_state.t) ~parser_input ~verbose =
  let%bind.Tilde_f saw_prefix =
    Coverage.with_prefix_coverage (get_signified parser_input)
  in
  let%bind.Tilde_f len, state, _ =
    List.iter (state_after_every_prefix Single parser_input)
  in
  saw_state.f state;
  if verbose then print_s (show_state state len);
  let%bind.Tilde_f of_atom = Option.iter (Atom_prefix.create_opt state) in
  match Atom_prefix.get_signified of_atom with
  | Incomplete { prefix_of_prefix = prefix } | Complete { prefix } ->
    saw_prefix.f ~prefix;
    assert_signifier_has_no_comments of_atom ~len ~parser_input;
    assert_prefix_can_round_trip of_atom ~len ~parser_input ~verbose;
    Tilde_f.return ()
;;

let%expect_test _ =
  Tilde_f.run
    (let%bind.Tilde_f () = Tilde_f.of_unlabeled (require_does_not_raise [%here]) in
     let%bind.Tilde_f state_coverage = Coverage.with_state_coverage in
     let%bind.Tilde_f parser_input = Base_quickcheck.Test.run_exn (module Atom_string) in
     test_every_prefix state_coverage ~parser_input ~verbose:false);
  [%expect {| |}]
;;
