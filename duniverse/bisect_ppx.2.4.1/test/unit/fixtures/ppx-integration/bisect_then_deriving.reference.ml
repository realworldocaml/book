module Bisect_visit___deriving___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\011\000\000\000\004\000\000\000\r\000\000\000\r\176\160I@\160vB\160\000DA" in
      let `Staged cb =
        Bisect.Runtime.register_file "deriving.ml" ~len:3
          ~data:point_definitions in
      cb
  end
open Bisect_visit___deriving___ml
let () = ___bisect_visit___ 0; ()
type a =
  | Foo [@@deriving show]
let rec (pp_a : Format.formatter -> a -> Ppx_deriving_runtime.unit) =
  ((let open! Ppx_deriving_runtime in
      fun fmt -> function | Foo -> Format.pp_print_string fmt "Deriving.Foo")
  [@ocaml.warning "-A"])
and show_a : a -> Ppx_deriving_runtime.string =
  fun x -> Format.asprintf "%a" pp_a x
let () =
  ___bisect_visit___ 2;
  (show_a Foo) |> ((___bisect_visit___ 1; print_endline))
