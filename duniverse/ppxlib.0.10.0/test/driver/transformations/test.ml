#use "topfind";;
#require "base";;
#require "ocaml-migrate-parsetree";;

open Base
open Ppxlib


(* Linters *)

let lint = object
  inherit [Driver.Lint_error.t list] Ast_traverse.fold as super

  method! type_declaration td acc =
    let acc = super#type_declaration td acc in
    match td.ptype_kind with
    | Ptype_record lds ->
      if Poly.(<>)
           (List.sort lds ~compare:(fun a b -> String.compare a.pld_name.txt b.pld_name.txt))
           lds
      then
        Driver.Lint_error.of_string td.ptype_loc
          "Fields are not sorted!"
        :: acc
      else
        acc
    | _ -> acc
end
let () =
  Driver.register_transformation "lint" ~lint_impl:(fun st -> lint#structure st [])
[%%expect{|
val lint : Driver.Lint_error.t list Ast_traverse.fold = <obj>
|}]

type t =
  { b : int
  ; a : int
  }
[%%expect{|
Line _, characters 0-36:
Error (warning 22): Fields are not sorted!
|}]


(* Extension with a path argument *)

let () =
  Driver.register_transformation "plop"
    ~rules:[Context_free.Rule.extension
              (Extension.declare_with_path_arg "plop"
                 Expression
                 Ast_pattern.(pstr nil)
                 (fun ~loc ~path:_ ~arg ->
                    let open Ast_builder.Default in
                    match arg with
                    | None -> estring ~loc "-"
                    | Some { loc; txt } -> estring ~loc (Longident.name txt)))]
[%%expect{|
|}]

let _ = Caml.Printf.sprintf "%s\n" [%plop]
[%%expect{|
- : string = "-\n"
|}]

let _ = Caml.Printf.sprintf "%s\n" [%plop.Truc]
[%%expect{|
- : string = "Truc\n"
|}]

let _ = Caml.Printf.sprintf "%s\n" [%plop.Truc.Bidule]
[%%expect{|
- : string = "Truc.Bidule\n"
|}]
