open Core.Std

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    Command.Spec.( 
      step (fun m base days -> m ~base ~days)
      +> anon ("base" %: date)
      +> anon ("days" %: int)
    )
  (fun ~base ~days () ->
    Date.add_days base days
    |> Date.to_string
    |> print_endline
  )

let () = Command.run add
