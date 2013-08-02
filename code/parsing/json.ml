type value = [
  | `Object of (string * value) list
  | `Array of value array
  | `String of string
  | `Int of int
  | `Float of float
  | `True
  | `False
  | `Null ]

(* part 1 *)
open Printf
let rec output_value outc = function
  | `Object obj -> print_object outc obj
  | `Array arr -> print_array outc arr
  | `String s -> printf "\"%s\"" s
  | `Int i -> printf "%d" i
  | `Float x -> printf "%f" x
  | `True -> output_string outc "true"
  | `False -> output_string outc "false"
  | `Null -> output_string outc "null"

and print_object outc obj =
  output_string outc "{ ";
  let sep = ref "" in
  List.iter (fun (key, value) ->
      printf "%s%s: %a" !sep key output_value value;
      sep := ", ") obj;
  output_string outc " }"

and print_array outc arr =
  output_string outc "[";
  Array.iteri (fun i v ->
      if i > 0 then
        output_string outc ", ";
      output_value outc v) arr;
  output_string outc "]"
