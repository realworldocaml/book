open Core.Std

let add_days base span () =
  Date.add_days base span
  |> Date.to_string
  |> print_endline

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    Command.Spec.( 
      step 
        (fun m base days ->
           match days with
           | Some days ->
             m base days
           | None ->
             print_endline "enter days: ";
             read_int ()
             |> m base
        )
      +> anon ("base" %: date)
      +> anon (maybe ("days" %: int))
    )
    add_days

let () = Command.run add
