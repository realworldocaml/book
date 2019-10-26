open Core

let add_days base days =
  Date.add_days base days
  |> Date.to_string
  |> print_endline

let prompt_for_string name of_string =
  printf "enter %s: %!" name;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> of_string line

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    Command.Let_syntax.(
      let%map_open
        base = anon ("base" %: date)
      and days = anon (maybe ("days" %: int))
      in
      let days =
        match days with
        | Some x -> x
        | None -> prompt_for_string "days" Int.of_string
      in
      fun () ->
        add_days base days)

let () = Command.run add
