open Core

let add_days ~base_date ~num_days () =
  Date.add_days base_date num_days
  |> Date.to_string
  |> print_endline

let add =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    [%map_open
      let base_date = anon ("base" %: date) 
      and num_days = anon ("days" %: int) in
      add_days ~base_date ~num_days
    ]

let () = Command.run add
