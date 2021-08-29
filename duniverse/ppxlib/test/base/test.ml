#require "base";;
#require "stdio";;

let () = Printexc.record_backtrace false

open Base
open Stdio
open Ppxlib

module N = Ppxlib_private.Name
[%%expect{|
module N = Ppxlib.Ppxlib_private.Name
|}]


let dot_suffixes name =
  Caml.Printf.sprintf "%s"
    (Sexp.to_string_hum
       (List.sexp_of_t String.sexp_of_t (N.dot_suffixes name)))
[%%expect{|
val dot_suffixes : string -> string = <fun>
|}]

let _ = dot_suffixes "foo.bar.baz"
[%%expect{|
- : string = "(baz bar.baz foo.bar.baz)"
|}]

let _ = dot_suffixes "foo.@bar.baz"
[%%expect{|
- : string = "(bar.baz foo.bar.baz)"
|}]


let split_path name =
    let a, b = N.split_path name in
    Caml.Printf.sprintf "%s"
      (Sexp.to_string_hum
         (List [sexp_of_string a; Option.sexp_of_t sexp_of_string b]))
[%%expect{|
val split_path : string -> string = <fun>
|}]

let _ = split_path "a.b.c"
[%%expect{|
- : string = "(a.b.c ())"
|}]

let _ = split_path "a.b.c.D"
[%%expect{|
- : string = "(a.b.c (D))"
|}]

let _ = split_path ".D"
[%%expect{|
- : string = "(\"\" (D))"
|}]

let convert_longident string =
  let lident = Longident.parse string in
  let name = Longident.name lident in
  (name, lident)
[%%expect{|
val convert_longident : string -> string * longident = <fun>
|}]

let _ = convert_longident "x"
[%%expect{|
- : string * longident = ("x", Ppxlib.Longident.Lident "x")
|}]

let _ = convert_longident "(+)"
[%%expect{|
- : string * longident = ("( + )", Ppxlib.Longident.Lident "+")
|}]

let _ = convert_longident "( + )"
[%%expect{|
- : string * longident = ("( + )", Ppxlib.Longident.Lident "+")
|}]

let _ = convert_longident "Base.x"
[%%expect{|
- : string * longident =
("Base.x", Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Base", "x"))
|}]

let _ = convert_longident "Base.(+)"
[%%expect{|
- : string * longident =
("Base.( + )", Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Base", "+"))
|}]

let _ = convert_longident "Base.( + )"
[%%expect{|
- : string * longident =
("Base.( + )", Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Base", "+"))
|}]

let _ = convert_longident "Base.( land )"
[%%expect{|
- : string * longident =
("Base.( land )",
 Ppxlib.Longident.Ldot (Ppxlib.Longident.Lident "Base", "land"))
|}]

let _ = convert_longident "A(B)"
[%%expect{|
Exception: (Invalid_argument "Ppxlib.Longident.parse: \"A(B)\"")
|}]

let _ = convert_longident "A.B(C)"
[%%expect{|
Exception: (Invalid_argument "Ppxlib.Longident.parse: \"A.B(C)\"")
|}]

let _ = convert_longident ")"
[%%expect{|
Exception: (Invalid_argument "Ppxlib.Longident.parse: \")\"")
|}]

let _ = Ppxlib.Code_path.(file_path @@ top_level ~file_path:"dir/main.ml")
[%%expect{|
- : string = "dir/main.ml"
|}]

let _ = Ppxlib.Code_path.(fully_qualified_path @@ top_level ~file_path:"dir/main.ml")
[%%expect{|
- : string = "Main"
|}]

let complex_path =
  let open Ppxlib.Code_path in
  let loc = Ppxlib.Location.none in
  top_level ~file_path:"dir/main.ml"
  |> enter_module ~loc "Sub"
  |> enter_module ~loc "Sub_sub"
  |> enter_value ~loc "some_val"
[%%expect{|
val complex_path : Code_path.t = <abstr>
|}]

let _ = Ppxlib.Code_path.fully_qualified_path complex_path
[%%expect{|
- : string = "Main.Sub.Sub_sub.some_val"
|}]

let _ = Ppxlib.Code_path.to_string_path complex_path
[%%expect{|
- : string = "dir/main.ml.Sub.Sub_sub"
|}]
