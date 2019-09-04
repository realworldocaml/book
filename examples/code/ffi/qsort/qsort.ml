open Core
open Ctypes
open PosixTypes
open Foreign

let compare_t = ptr void @-> ptr void @-> returning int

let qsort = foreign "qsort"
    (ptr void @-> size_t @-> size_t @-> funptr compare_t @->
       returning void)

let qsort' cmp arr =
  let open Unsigned.Size_t in
  let ty = CArray.element_type arr in
  let len = of_int (CArray.length arr) in
  let elsize = of_int (sizeof ty) in
  let start = to_voidp (CArray.start arr) in
  let compare l r = cmp (!@ (from_voidp ty l)) (!@ (from_voidp ty r)) in
  qsort start len elsize compare;
  arr

let sort_stdin () =
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:int_of_string
  |> CArray.of_list int
  |> qsort' Int.compare
  |> CArray.to_list
  |> List.iter ~f:(fun a -> printf "%d\n" a)

let () =
  Command.basic_spec ~summary:"Sort integers on standard input"
    Command.Spec.empty sort_stdin
  |> Command.run
