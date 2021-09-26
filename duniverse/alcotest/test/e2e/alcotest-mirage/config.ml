open Mirage

let main =
  foreign
    ~packages:[ package "alcotest-mirage" ]
    "Unikernel.Main" (mclock @-> job)

let () = register "alcotest" [ main $ default_monotonic_clock ]
