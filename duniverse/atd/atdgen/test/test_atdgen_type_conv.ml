open Sexplib.Std

let my_record = Test_type_conv_t.({ fst=123; snd="testing" })

let cmrs : (float Test_type_conv_t.contains_my_record) list =
  let open Test_type_conv_t in
  [ `C1 123
  ; `C2 123.0
  ; `C3 my_record ]

let sexps =
  [my_record |> Test_type_conv_t.sexp_of_my_record] @
  (List.map (Test_type_conv_t.sexp_of_contains_my_record sexp_of_float) cmrs)

let () =
  sexps
  |> sexp_of_list (fun x -> x)
  |> Sexplib.Sexp.to_string
  |> print_endline
