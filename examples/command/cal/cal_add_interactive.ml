open Core.Std

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    Command.Spec.( 
      step (fun m base days ->
        match days with
        | Some days -> m base days
        | None ->
            print_endline "enter days: ";
            m base (read_int ())
      )
      +> anon ("base" %: date)
      +> anon (maybe ("days" %: int))
    )
  (fun base span () ->
    Date.add_days base span
    |> Date.to_string
    |> print_endline
  )

let () = Command.run add
