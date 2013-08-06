open Core.Std
open Core_bench.Std

type t = | Alice | Bob | Charlie | David

let polymorphic_pattern () =
  let test v =
    match v with
    | `Alice   -> 100
    | `Bob     -> 101
    | `Charlie -> 102
    | `David   -> 103
  in
  List.iter ~f:(fun v -> ignore(test v))
    [`Alice; `Bob; `Charlie; `David]

let monomorphic_pattern_exhaustive () =
  let test v =
    match v with
    | Alice   -> 100
    | Bob     -> 101
    | Charlie -> 102
    | David   -> 103 in
  List.iter ~f:(fun v -> ignore(test v))
    [ Alice; Bob; Charlie; David ]

let monomorphic_pattern_incomplete () =
  let test v =
    match v with
    | Alice   -> 100
    | Bob     -> 101
    | _       -> 102
  in
  List.iter ~f:(fun v -> ignore(test v))
    [ Alice; Bob; Charlie; David ]

let tests = [
  "Polymorphic pattern", polymorphic_pattern;
  "Monomorphic incomplete pattern", monomorphic_pattern_incomplete;
  "Monomorphic exhaustive pattern", monomorphic_pattern_exhaustive
]

let () =
  List.map tests ~f:(fun (name,test) -> Bench.Test.create ~name test)
  |> Bench.make_command
  |> Command.run
