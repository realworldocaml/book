open Core
open Poly

let num_char n = Char.of_int_exn (Char.to_int '0' + n)

let data_line percentage =
  List.init (percentage + 1) ~f:(fun i ->
    if i mod 10 = 0
    then (
      let tens = i / 10 in
      if tens = 0 || tens = 10 then '|' else num_char tens)
    else if i mod 5 = 0
    then '+'
    else '-')
  |> String.of_char_list
;;

let%test _ = data_line 27 = "|----+----1----+----2----+--"
let%test _ = data_line 0 = "|"

let%test _ =
  data_line 100
  = "|----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----|"
;;

let narrow_data_line percentage =
  let div = 2 in
  List.init
    ((percentage / div) + 1)
    ~f:(fun i ->
      let parts_per_10 = 10 / div in
      if i mod parts_per_10 = 0
      then (
        let tens = i / parts_per_10 in
        if tens = 0 || tens = 10 then '|' else num_char tens)
      else '-')
  |> String.of_char_list
;;

let%test _ = narrow_data_line 27 = "|----1----2---"
let%test _ = narrow_data_line 0 = "|"
let%test _ = narrow_data_line 100 = "|----1----2----3----4----5----6----7----8----9----|"

let line ~narrow ~label ~value ~norm =
  let percentage = Option.value (Float.iround_towards_zero (value *. norm)) ~default:0 in
  assert (0 <= percentage && percentage <= 100);
  let pre =
    if norm > 1.0
    then sprintf "%10s %5.2f " label value
    else sprintf "%10s %8.0f " label value
  in
  if narrow then pre ^ narrow_data_line percentage else pre ^ data_line percentage
;;

let%test _ =
  line ~narrow:false ~label:"bar1" ~value:1.05062 ~norm:10.0
  = "      bar1  1.05 |----+----1"
;;

let%test _ =
  line ~narrow:false ~label:"bar2" ~value:499.6 ~norm:0.004 = "      bar2      500 |-"
;;

let render ?(narrow = false) labels_and_values =
  if List.is_empty labels_and_values
  || List.exists ~f:(fun (_, x) -> x < 0.0) labels_and_values
  then
    failwiths
      ~here:[%here]
      "Text_graph.render: Labels and values should be non-empty and values must be \
       positive"
      labels_and_values
      [%sexp_of: (string * float) list];
  let largest =
    match labels_and_values with
    | [] -> assert false (* checked above *)
    | (_, v) :: tl -> List.fold tl ~init:v ~f:(fun max (_, v) -> Float.max max v)
  in
  let norm = 100.0 /. largest in
  let lines =
    List.map labels_and_values ~f:(fun (label, value) -> line ~narrow ~label ~value ~norm)
  in
  let last_line =
    sprintf
      "(each \'-\' is approximately %.3f units.)"
      (if narrow then 2.0 /. norm else 1.0 /. norm)
  in
  let concat l = String.concat l ~sep:"\n" in
  concat [ concat lines; last_line; "" ]
;;
