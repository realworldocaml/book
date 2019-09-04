open Core
open Core_bench

type t = | Alice | Bob
type s = | A | B | C | D | E

let polymorphic_pattern () =
  let test v =
    match v with
    | `Alice   -> 100
    | `Bob     -> 101
    | `Charlie -> 102
    | `David   -> 103
    | `Eve     -> 104
  in
  List.iter ~f:(fun v -> ignore(test v))
    [`Alice; `Bob; `Charlie; `David]

let monomorphic_pattern_small () =
  let test v =
    match v with
    | Alice   -> 100
    | Bob     -> 101 in
  List.iter ~f:(fun v -> ignore(test v))
    [ Alice; Bob ]

let monomorphic_pattern_large () =
  let test v =
    match v with
    | A       -> 100
    | B       -> 101
    | C       -> 102
    | D       -> 103
    | E       -> 104
  in
  List.iter ~f:(fun v -> ignore(test v))
    [ A; B; C; D ]

let tests = [
  "Polymorphic pattern", polymorphic_pattern;
  "Monomorphic larger pattern", monomorphic_pattern_large;
  "Monomorphic small pattern", monomorphic_pattern_small;
]

let () =
  List.map tests ~f:(fun (name,test) -> Bench.Test.create ~name test)
  |> Bench.make_command
  |> Command.run
