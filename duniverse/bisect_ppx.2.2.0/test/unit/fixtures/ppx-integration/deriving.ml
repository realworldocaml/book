let () = ()

type a = Foo [@@deriving show]

let () = show_a Foo |> print_endline
