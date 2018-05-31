open Core

let common =
  let open Command.Let_syntax in
  [%map_open
    let debug = flag "-d" (optional_with_default false bool) ~doc:" Debug mode"
    and verbose = flag "-v" (optional_with_default false bool) ~doc:" Verbose output" in
    debug, verbose
  ]

let add =
  let open Command.Let_syntax in
  Command.basic ~summary:"Add [days] to the [base] date"
    [%map_open
      let base = anon ("base" %: date)
      and span = anon ("days" %: int)
      and debug, verbose = common in
      (fun () ->
         Date.add_days base span
         |> Date.to_string
         |> print_endline
      )
    ]

let diff =
  let open Command.Let_syntax in
  Command.basic ~summary:"Show days between [date2] and [date1]"
    [%map_open
      let date1 = anon ("date1" %: date)
      and date2 = anon ("date2" %: date)
      and debug, verbose = common in
      (fun () ->
         Date.diff date1 date2
         |> printf "%d days\n"
      )
    ]


[@@@part "1"];;
let () =
  Command.group ~summary:"Manipulate dates" [ "add", add; "diff", diff ]
  |> Command.run
