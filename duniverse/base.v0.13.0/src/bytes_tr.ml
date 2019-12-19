open! Import0.Int_replace_polymorphic_compare
module Bytes = Bytes0
module String = String0

(* Construct a byte string of length 256, mapping every input character code to
   its corresponding output character.

   Benchmarks indicate that this is faster than the lambda (including cost of
   this function), even if target/replacement are just 2 characters each.

   Return None if the translation map is equivalent to just the identity. *)
let tr_create_map ~target ~replacement =
  let tr_map = Bytes.create 256 in
  for i = 0 to 255 do
    Bytes.unsafe_set tr_map i (Char.of_int_exn i)
  done;
  for i = 0 to min (String.length target) (String.length replacement) - 1 do
    let index = Char.to_int (String.unsafe_get target i) in
    Bytes.unsafe_set tr_map index (String.unsafe_get replacement i)
  done;
  let last_replacement = String.unsafe_get replacement (String.length replacement - 1) in
  for
    i = min (String.length target) (String.length replacement)
    to String.length target - 1
  do
    let index = Char.to_int (String.unsafe_get target i) in
    Bytes.unsafe_set tr_map index last_replacement
  done;
  let rec have_any_different tr_map i =
    if i = 256
    then false
    else if Char.( <> ) (Bytes0.unsafe_get tr_map i) (Char.of_int_exn i)
    then true
    else have_any_different tr_map (i + 1)
  in
  (* quick check on the first target character which will 99% be true *)
  let first_target = target.[0] in
  if Char.( <> ) (Bytes0.unsafe_get tr_map (Char.to_int first_target)) first_target
  || have_any_different tr_map 0
  then Some (Bytes0.unsafe_to_string ~no_mutation_while_string_reachable:tr_map)
  else None
;;
