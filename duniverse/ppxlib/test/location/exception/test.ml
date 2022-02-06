open Ppxlib.Location

let catch_as_compiler_exception =
  try raise_errorf ~loc:none "foo" with
  | Ocaml_common.Location.Error _ -> "caught"
  | _ -> "uncaught"
[%%expect{|
val catch_as_compiler_exception : string = "caught"
|}]

let catch_as_ppxlib_exception =
  try raise_errorf ~loc:none "foo" with
  | Error _ -> "caught"
  | _ -> "uncaught"
[%%expect{|
val catch_as_ppxlib_exception : string = "caught"
|}]
