module Bisect_visit___deriving___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\014\000\000\000\005\000\000\000\017\000\000\000\017\192\160I@\160MA\160vC\160\000DB" in
      let `Staged cb =
        Bisect.Runtime.register_file "deriving.ml" ~len:4
          ~data:point_definitions in
      cb
  end
open Bisect_visit___deriving___ml
let () = ___bisect_visit___ 0; ()
type a =
  | Foo [@@deriving show]
let rec pp_a : Format.formatter -> a -> Ppx_deriving_runtime.unit =
  ___bisect_visit___ 1;
  (((let open! Ppx_deriving_runtime in
       fun fmt ->
         ___bisect_visit___ 1;
         (function
          | Foo ->
              (___bisect_visit___ 1;
               Format.pp_print_string fmt "Deriving.Foo"))))
  [@ocaml.warning "-A"])
and show_a : a -> Ppx_deriving_runtime.string =
  ___bisect_visit___ 1;
  (fun x -> ___bisect_visit___ 1; Format.asprintf "%a" pp_a x)
let () =
  ___bisect_visit___ 3;
  (show_a Foo) |> ((___bisect_visit___ 2; print_endline))
