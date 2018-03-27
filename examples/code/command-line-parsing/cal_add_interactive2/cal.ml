open Core

let add_days base days =
  Date.add_days base days
  |> Date.to_string
  |> print_endline

let prompt_for_string name of_string =
  printf "enter %s: %!" name;
  match In_channel.(input_line stdin) with
  | None -> failwith "no value entered. aborting."
  | Some line -> of_string line

[@@@part "1"];;
let anon_prompt name of_string =
  let arg = Command.Arg_type.create of_string in
  Command.Let_syntax.(
    let%map_open value = anon (maybe (name %: arg)) in
    match value with
    | Some v -> v
    | None -> prompt_for_string name of_string)

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    Command.Let_syntax.(
      let%map_open
        base = anon ("base" %: date)
      and days = anon_prompt "days" Int.of_string
      in
      fun () ->
        add_days base days)

[@@@part "2"];;
let () = Command.run add
