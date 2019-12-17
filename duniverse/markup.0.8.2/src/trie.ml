(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(* Tries. These aren't fully functional nor fully mutable. To accumulate a trie,
   it is necessary to retain the latest result of [add]. However, previous tries
   become invalid after [add]. *)

type 'a trie =
  | Empty
  | Leaf of 'a
  | Node of 'a option * 'a trie array

let lower_limit = Char.code '0'
let upper_limit = Char.code 'z'
let array_size = upper_limit - lower_limit + 1

let create () =
  Empty

let edge_index c =
  Char.code c - lower_limit

let add key value trie =
  let rec traverse index trie =
    if index >= String.length key then
      match trie with
      | Empty | Leaf _ -> Leaf value
      | Node (_, children) -> Node (Some value, children)

    else
      let edge_index = edge_index key.[index] in
      let value', children, current_child =
        match trie with
        | Empty -> None, None, Empty
        | Leaf v -> Some v, None, Empty
        | Node (v, children) -> v, Some children, children.(edge_index)
      in
      let child = traverse (index + 1) current_child in
      let children =
        match children with
        | None ->
          Array.init array_size (fun i ->
            if i = edge_index then child else Empty)
        | Some children ->
          children.(edge_index) <- child;
          children
      in
      Node (value', children)
  in

  traverse 0 trie

type 'a match_ =
  | No
  | Yes of 'a
  | Prefix
  | Multiple of 'a

let matches = function
  | Empty -> No
  | Leaf v -> Yes v
  | Node (None, _) -> Prefix
  | Node (Some v, _) -> Multiple v

let advance c = function
  | Empty | Leaf _ -> Empty
  | Node (_, children) ->
    if c < lower_limit || c > upper_limit then Empty
    else children.(c - lower_limit)

let guess_memory_usage trie =
  let rec accumulate words = function
    | Empty -> words + 1
    | Leaf _ -> words + 2
    | Node (_, children) ->
      let words = words + 4 + Array.length children in
      Array.fold_left accumulate words children
  in
  accumulate 0 trie
