(* $MDX part-begin=1 *)
type value =
  [ `Assoc of (string * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `Null
  | `String of string ]
(* $MDX part-end *)

open Core

let rec output_value outc = function
  | `Assoc obj -> print_assoc outc obj
  | `List l -> print_list outc l
  | `String s -> printf "\"%s\"" s
  | `Int i -> printf "%d" i
  | `Float x -> printf "%f" x
  | `Bool true -> Out_channel.output_string outc "true"
  | `Bool false -> Out_channel.output_string outc "false"
  | `Null -> Out_channel.output_string outc "null"

and print_assoc outc obj =
  Out_channel.output_string outc "{ ";
  let sep = ref "" in
  List.iter
    ~f:(fun (key, value) ->
      printf "%s\"%s\": %a" !sep key output_value value;
      sep := ",\n  ")
    obj;
  Out_channel.output_string outc " }"

and print_list outc arr =
  Out_channel.output_string outc "[";
  List.iteri
    ~f:(fun i v ->
      if i > 0 then Out_channel.output_string outc ", ";
      Out_channel.output_value outc v)
    arr;
  Out_channel.output_string outc "]"
