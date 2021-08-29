open Tyxml

let to_string = Format.asprintf "%a" (Svg.pp_elt ())

let tyxml_tests l =
  let f (name, (ty : Svg_types.text Svg.elt), s) =
    name, `Quick, fun () -> Alcotest.(check string) name (to_string ty) s
  in
  List.map f l

let svg_attributes = "svg attributes", tyxml_tests Svg.[

  "text data-foo",
  text ~a:[ a_user_data "foo" "valfoo" ] [],
  "<text data-foo=\"valfoo\"></text>" ;

]

let tests = [
  svg_attributes ;
]

let () = Alcotest.run "tyxml-svg" tests
