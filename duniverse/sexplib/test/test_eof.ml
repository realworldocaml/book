open Sexplib.Std

let with_temp_file sexp_of_t ~contents ~f =
  let fname, oc = Filename.open_temp_file "test" ".sexp" in
  let result =
    match
      output_string oc contents;
      close_out oc;
      f fname
    with
    | x -> `Ok x
    | exception e -> `Error e
  in
  Sys.remove fname;
  Printf.printf !"%{sexp:[ `Ok of t | `Error of exn ]}" result

let%expect_test "file ending with an atom" =
  with_temp_file [%sexp_of: int] ~contents:"5" ~f:(fun fname ->
    Sexplib.Sexp.load_sexp_conv_exn fname [%of_sexp: int]);
  [%expect {|
     (Ok 5)
  |}]

let%expect_test "file ending with an atom" =
  with_temp_file [%sexp_of: Sexplib.Sexp.t list] ~contents:"5" ~f:(fun fname ->
    let ic = open_in fname in
    (match Sexplib.Sexp.input_sexps ic with
     | x -> close_in ic; x
     | exception e -> close_in ic; raise e));
  [%expect {|
     (Ok (5))
  |}]
