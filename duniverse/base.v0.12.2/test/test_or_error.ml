open! Import
open! Or_error

let%test _ = [%compare.equal: string t] (errorf "foo %d" 13) (error_string "foo 13")

let%test_unit _ =
  for i = 0 to 10; do
    assert ([%compare.equal: unit list t]
              (combine_errors (List.init i ~f:(fun _ -> Ok ())))
              (Ok (List.init i ~f:(fun _ -> ()))));
  done
let%test _ = Result.is_error (combine_errors [ error_string "" ])
let%test _ = Result.is_error (combine_errors [ Ok (); error_string "" ])

let (=) = [%compare.equal: unit t]
let%test _ = combine_errors_unit [Ok (); Ok ()] = Ok ()
let%test _ = combine_errors_unit [] = Ok ()
let%test _ =
  let a = Error.of_string "a" and b = Error.of_string "b" in
  match combine_errors_unit [Ok (); Error a; Ok (); Error b] with
  | Ok _ -> false
  | Error e -> String.equal
                 (Error.to_string_hum e)
                 (Error.to_string_hum (Error.of_list [a;b]))
;;

