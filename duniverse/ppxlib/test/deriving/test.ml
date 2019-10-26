#use "topfind";;
#require "base";;
#load "ppxlib_metaquot_lifters.cmo";;
#load "ppxlib_metaquot.cmo";;

open Ppxlib


let foo =
  Deriving.add "foo"
    ~str_type_decl:(Deriving.Generator.make_noarg
                      (fun ~loc ~path:_ _ -> [%str let x = 42]))
[%%expect{|
val foo : Deriving.t = <abstr>
|}]

let bar =
  Deriving.add "bar"
    ~str_type_decl:(Deriving.Generator.make_noarg
                      ~deps:[foo]
                      (fun ~loc ~path:_ _ -> [%str let () = Printf.printf "x = %d\n" x]))
[%%expect{|
val bar : Deriving.t = <abstr>
|}]

type t = int [@@deriving bar]
[%%expect{|
Line _, characters 25-28:
Error: Deriver foo is needed for bar, you need to add it before in the list
|}]

type t = int [@@deriving bar, foo]
[%%expect{|
Line _, characters 25-33:
Error: Deriver foo is needed for bar, you need to add it before in the list
|}]

type t = int [@@deriving foo, bar]
[%%expect{|
type t = int
val x : int = 42
|}]
