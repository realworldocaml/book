open QCheck

let (|>) x f = f x

let quoted_str = Printf.sprintf "%S"

let run lst =
  OUnit2.run_test_tt_main (QCheck_runner.to_ounit2_test lst)

let _ = run (
  let f1 s = Stringext.split_trim_left ~on:"," ~trim:" " s in
  let f2 s = Stringext.split ~on:',' s |> List.map Stringext.trim_left in
  let pp str =
    (quoted_str str) ^ " -> " ^
    (Print.list quoted_str (f1 str)) ^ " != " ^
    (Print.list quoted_str (f2 str))
  in
  Test.make ~name:"stringext.split_trim_left == split |> trim_left" ~small:String.length
    ((pair (oneofl [",";" ,";", ";" , "]) (list_of_size (Gen.int_bound 3) printable_string)) |>
     map (fun (sep,sub) -> String.concat sep sub) |>
     set_shrink Shrink.string |>
     set_print pp)
    (fun s -> f1 s = f2 s)
)
