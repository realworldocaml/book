(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
module Trie = Markup__Trie

let singleton w value =
  Trie.create () |> Trie.add w value

let advance w trie =
  let rec loop index trie =
    if index >= String.length w then trie
    else loop (index + 1) (Trie.advance (Char.code w.[index]) trie)
  in
  loop 0 trie

let assert_matches trie w status =
  assert_equal (Trie.matches (advance w trie)) status

let tests = [
  ("trie.empty" >:: fun _ ->
    let trie = Trie.create () in
    assert_equal (Trie.matches trie) Trie.No);

  ("trie.one-character" >:: fun _ ->
    let trie = singleton "a" 0 in
    assert_matches trie "" Trie.Prefix;
    assert_matches trie "a" (Trie.Yes 0);
    assert_matches trie "b" Trie.No);

  ("trie.simple-word" >:: fun _ ->
    let trie = singleton "ab" 0 in
    assert_matches trie "" Trie.Prefix;
    assert_matches trie "a" Trie.Prefix;
    assert_matches trie "b" Trie.No;
    assert_matches trie "ab" (Trie.Yes 0));

  ("trie.prefix-match" >:: fun _ ->
    let trie = Trie.create () in
    let trie = Trie.add "a" 0 trie in
    let trie = Trie.add "ab" 1 trie in
    assert_matches trie "" Trie.Prefix;
    assert_matches trie "a" (Trie.Multiple 0);
    assert_matches trie "ab" (Trie.Yes 1));

  ("trie.branching" >:: fun _ ->
    let trie = Trie.create () in
    let trie = Trie.add "aa" 0 trie in
    let trie = Trie.add "ab" 1 trie in
    assert_matches trie "" Trie.Prefix;
    assert_matches trie "a" Trie.Prefix;
    assert_matches trie "aa" (Trie.Yes 0);
    assert_matches trie "ab" (Trie.Yes 1));

  ("trie.unsupported-character" >:: fun _ ->
    let trie = singleton "a" 0 in
    assert_matches trie " " Trie.No);

  ("trie.advance-no-match" >:: fun _ ->
    let trie = singleton "a" 0 in
    assert_matches trie "a" (Trie.Yes 0);
    assert_matches trie "aa" Trie.No;
    assert_matches trie "aaa" Trie.No);

  ("trie.empty-string" >:: fun _ ->
    let trie = singleton "" 0 in
    assert_matches trie "" (Trie.Yes 0));

  ("trie.replace-leaf" >:: fun _ ->
    let trie = singleton "a" 0 in
    assert_matches trie "a" (Trie.Yes 0);
    let trie = Trie.add "a" 1 trie in
    assert_matches trie "a" (Trie.Yes 1));

  ("trie.replace-node" >:: fun _ ->
    let trie = singleton "ab" 0 in
    assert_matches trie "a" Trie.Prefix;
    let trie = Trie.add "a" 1 trie in
    assert_matches trie "a" (Trie.Multiple 1);
    let trie = Trie.add "a" 2 trie in
    assert_matches trie "a" (Trie.Multiple 2));

  ("trie.memory_usage" >:: fun _ ->
    let trie = singleton "ab" 0 in
    let expected_nodes = 2 in
    let expected_leaves = 1 in
    let expected_empty_leaves =
      expected_nodes * Trie.array_size
      - expected_leaves
      - (expected_nodes - 1)
    in
    let expected_machine_word_count =
      expected_nodes * (4 + Trie.array_size)
      + expected_leaves * 2
      + expected_empty_leaves * 1
    in
    assert_equal (Trie.guess_memory_usage trie) expected_machine_word_count);
]
